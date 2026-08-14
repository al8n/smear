//! Draft §6 execution, as a Sans-I/O state machine.
//!
//! The links here are bare, which is the split's doing. While this was `smear::proto`, a module
//! with an outer doc comment on its declaration, every link had to be crate-absolute: rustdoc
//! resolves the MERGED fragments of a module's documentation in the parent's scope, so a relative
//! `[`Executor`]` reported "no item named `Executor` in module `smear`" under
//! `RUSTDOCFLAGS="-D warnings"`. A crate root has no parent to be merged into, and rustdoc
//! resolves an inlined re-export's links in the crate they were written in — so `crate::Executor`
//! became a *redundant explicit link target*, which is also denied. `smear`'s `validator/mod.rs`
//! header records the trap this one just escaped.
//!
//! # What this module is
//!
//! [`Executor`] runs a validated query against a built [`Schema`](smear_schema::Schema) and
//! produces a draft §7.1 response. It performs no I/O, holds no runtime, spawns nothing, and never
//! calls a resolver: it says which field it needs next and waits to be told the answer. That is
//! what makes it usable from a thread pool, an async runtime, a wasm host or a C caller without
//! any of them appearing in its types.
//!
//! ```text
//!            ┌──────────────────────────────────────────────┐
//!  start ───▶│                                              │
//!            │   poll_resolve()   ──▶  FieldRequest          │
//!            │   poll_abandoned() ──▶  ReqId (cancel this)   │
//!            │   poll_response()  ──▶  Response              │
//!            │                                              │
//!            │◀── handle_resolved(id, value)                 │
//!            │◀── handle_field_error(id, message)            │
//!            └──────────────────────────────────────────────┘
//! ```
//!
//! # The driver owns the values
//!
//! There is no `smear::proto::Value`. The driver keeps values in whatever representation it
//! already has and answers structural questions about them through [`Values`] — is this null, is
//! it a list, which object type is it, serialise this leaf. That trait's own documentation explains why at length; the
//! short version is that an owned enum would force an allocation per leaf to answer questions the
//! service can already answer in place, and would make a wasm or FFI handle second-class.
//!
//! The consequence worth stating up front: **`proto` owns structure and control flow, the driver
//! owns representation.** Which fields are collected, in what order they complete, where a null
//! propagates to and which path an error carries are all `proto`'s. What an `Int` is, is not.
//!
//! # All three of §7.1's result kinds are here, and one of them is not a map
//!
//! §7.1 *Response Format* defines a *response* as "either an *execution result*, a *response
//! stream*, or a *request error result*". [`Response`] is the first, [`RequestErrorResult`] is the
//! third, and [`ResponseStream`] is the second.
//!
//! The second is shaped differently from the other two and the type says so. §7.1.1 and §7.1.3 each
//! specify a **map** with a named set of entries, so each is a type holding those entries. §7.1.2
//! specifies neither: "A response stream must be a stream of *execution result*" — a sequence over
//! time, which a crate owning no stream cannot hold. So [`ResponseStream`] is the stream's *state*,
//! the sequence is successive [`Response`]s out of
//! [`poll_response`](Executor::poll_response), and the three things §7.1.2 says are each
//! structural rather than documented. `subscribe.rs`'s header has that argument in full.
//!
//! # The execution result is a map with three entries, and this crate holds all three
//!
//! Draft §7.1.8 *Additional Entries* closes it: an execution result "must not contain any entries
//! other than those described above", so [`Response`] is the whole shape rather than a part of it.
//!
//! - **`data`** (§7.1.5 *Data*) — [`Response::data`], the tree the executor built.
//! - **`errors`** (§7.1.6 *Errors*) — [`Response::errors`], present when it is non-empty, which
//!   [`Response::is_ok`] answers.
//! - **`extensions`** (§7.1.7 *Extensions*) — [`Response::extensions`], present when the driver
//!   attached an [`Extensions`] map with [`Executor::set_extensions`].
//!
//! The third is split at the one place §7.1.7 draws a line. It requires the entry's value to "be a
//! map" if set and then stops — "there are no additional restrictions on its contents" — so
//! [`Extensions`] owns the map and each value in it is one `V::Value` the driver hands over and
//! nothing here reads. That makes `"extensions": null`, a scalar or a list a type error rather than
//! a rule a driver is asked to keep.
//!
//! §7.1.7 has a **second** response-level site, and it is [`RequestErrorResult`]'s: the entry is
//! admitted by "an *execution result* or *request error result*", and [`Executor::start`] can
//! refuse a **valid** document — ambiguity between two named operations is a §6.1 `GetOperation`
//! failure, not a validation one. `start` still returns [`StartError`] and builds no response, so
//! the result is a value the driver constructs from the refusal it was handed, under the ceilings
//! of the executor that refused. That type's header has the four things §7.1.3 demands and how
//! each one is made structural.
//!
//! **The numbers moved, so the titles are given with them.** The draft renumbered §7.1 when it
//! separated the result kinds and added *Response Position*: `Data` and `Errors` were §7.1.1 and
//! §7.1.2 as recently as the October 2021 specification and are §7.1.5 and §7.1.6 now, and
//! `Extensions` — which October 2021 stated inside §7.1 *Response Format* without a subsection of
//! its own — is §7.1.7. Citations in the rest of this crate still read against October 2021, so a
//! `§7.1.2` met in `error.rs` is today's §7.1.6.
//!
//! What this crate does **not** do is write any of the three out. §7.2's serialization format is
//! the driver's, for the reason a leaf is.
//!
//! # Scope
//!
//! Draft §6.2's three operations, as three values of one at-most-one phase inside one
//! [`Executor`] — not three types. Draft §6.2.2's serial rule for a mutation's top-level fields is expressed
//! by *withholding*: [`poll_resolve`](crate::Executor::poll_resolve) offers one of them and
//! keeps the next off the ready chain until that one's whole subtree is **complete or cancelled**,
//! so the ordering is structural rather than a contract a driver could forget to honour. Everything
//! below the top level is draft §6.3's ordinary collection, which is where the specification draws
//! the line too.
//!
//! "Or cancelled" is the edge and it is load-bearing rather than a hedge. When draft §6.4.4 nulls a
//! mutation field because something under it failed, the requests still outstanding beneath it are
//! *abandoned* — [`poll_abandoned`](crate::Executor::poll_abandoned) is how the driver hears
//! so — and the next mutation field is released over them rather than behind them. `graphql-js`
//! 16.11.0 was measured doing the same, and waiting instead would put that field behind work the
//! driver has just been told to stop doing: retiring those entries on that channel, or answering
//! them, which are the only two things that clear the count.
//!
//! Releasing over them is not free, and the price is stated exactly. An abandoned request keeps
//! its in-flight slot until the driver retires *or answers* it, so a driver that does neither runs
//! under a ceiling narrowed by however many it is holding — never by all of them, because
//! [`max_in_flight`](crate::Limits::max_in_flight) bounds them at one below itself. The
//! cost is concurrency, down to a floor of one request at a time, and never progress.
//!
//! Draft §6.2.3's subscription is the third value of that phase, and the same surface serves it.
//! [`Executor::start`] runs §6.2.3.1 `CreateSourceEventStream` and begins no execution;
//! [`Executor::source_field`] is what the driver calls `ResolveFieldEventStream` with, because
//! resolving is the driver's; and each source event handed to
//! [`Executor::handle_source_event`] is one whole execution whose result comes out of
//! [`poll_response`](Executor::poll_response) like any other. §6.2.3.2's ordering — one execution
//! result per event, in source order — is owned by the machine rather than published as a rule: the
//! intake refuses the next event while the previous result is undelivered, which is draft §6.2.2's
//! withholding applied one level up, and a source stream that ends mid-event has its ending
//! *recorded* so that event's result is still emitted. The two obligations that stay the driver's
//! are made unskippable the same way: [`Executor::start`] refuses while a response stream is still
//! open, because ending it through [`Executor::unsubscribe`] is what publishes §6.2.3.3's
//! cancellation. `subscribe.rs`'s header has the division of labour, the `graphql-js` measurement
//! behind the serial intake, and what a subscription retains between events, which is nothing that
//! holds a driver value.
//!
//! Draft §6.1 `CoerceVariableValues` is the driver's: values reaching [`Values::variable`] are
//! already coerced against their declared types. It is one of the §6 preamble's six *request*
//! members, and the section below has why two of them are the driver's rather than one.
//!
//! Of draft §4.4's three meta-fields, one is executed and two are not. `__typename` is answered by
//! the executor from the object type it resolved and arrives as [`Node::TypeName`];
//! the driver is never asked, because the executor has already spent that object type deciding
//! which fragments applied and must not let the answer disagree with them. `__schema` and `__type`
//! are left to the driver as ordinary fields of the query root — draft §4.5 introspection is a
//! later phase, and until it lands this executor has nothing better to say about them than the
//! service does.
//!
//! ## Draft §6's request has six members, and this crate carries the five `ExecuteRequest` takes
//!
//! The preamble to §6 makes *request* a defined term and enumerates it — `schema`, `document`,
//! `operationName`, `variableValues`, `initialValue`, and, added since the October 2021
//! specification, `extensions`, "a map reserved for implementation-specific additional
//! information". It then hands **five** of the six to the algorithm: "the result of
//! `ExecuteRequest(schema, document, operationName, variableValues, initialValue)` produces the
//! response".
//!
//! `extensions` is the one member the specification withholds from execution, and it withholds it
//! everywhere. The word occurs three times in the whole of §6 and all three are in that preamble;
//! no numbered step reads it, and §6.1's `ExecuteRequest` cannot pass on what it was not handed.
//! What the preamble asks of an implementation is the reverse of a step to run — "implementations
//! should not add additional properties to a *request* […] instead, `extensions` provides a
//! reserved location" — so the entry is a place to put things, not a channel with a consumer.
//!
//! The five are here. `schema` and `document` are [`Executor::new`]'s, `operationName` and
//! `initialValue` are [`Executor::start`]'s two arguments, and `variableValues` is
//! [`Values::variable`]'s. **The sixth is not, and the reason is the fifth's.** `variableValues`
//! is a member `ExecuteRequest` *does* take and this crate still does not hold it, because its only
//! reader is the driver. A member the algorithm is never given has the weaker claim of the two.
//!
//! ### An `Executor` member would satisfy the lifetime table's rule 2, and that is the point
//!
//! [`Limits`]' lifetime table asks every held member to name its reader and calls an empty reader
//! set a defect by inspection. That rule does **not** decide this, and reading it as though it did
//! would condemn the map this crate already carries: `proto` reads not one value of a draft §7.1.7
//! [`Extensions`] either — nothing in it is ever interpreted — and it satisfies the rule because a
//! *reader* there means an accessor or a lend, which is what every entry in that column is. An
//! `Executor::request_extensions` would be exactly such an accessor, so rule 2 is satisfiable here
//! and settles nothing.
//!
//! What separates the two maps is a **destination**. §7.1.7's has one this crate owns — the
//! execution result — so handing the map over buys the driver something it cannot do for itself.
//! §6's has none: the specification names no consumer for it anywhere, that being the entry's whole
//! purpose, so the accessor would return exactly what the caller moved in.
//!
//! It would also return it *less* usefully than a field on the driver's own type. [`Values`] is the
//! surface a driver's value logic runs on, and its seven methods see the driver's type and never
//! the executor — so a map parked on an [`Executor`] is unreadable at the two steps an
//! implementation-specific request hint exists for, [`Values::variable`] (§6.4.1 steps 5.c–5.f and
//! §6.3's directive conditions) and [`Values::coerce_leaf`] (§6.4.3 step 4). Between
//! [`Executor::poll_resolve`] and [`Executor::handle_resolved`] the driver does hold the executor,
//! and even there it would have to drop the [`FieldRequest`] before it could ask.
//!
//! ### It does not survive into the response, and that was checked rather than assumed
//!
//! §7.1.8 *Additional Entries* closes the response's entry set: an execution result and a request
//! error result "must not contain any entries other than those described above", and "clients must
//! ignore any entries other than those described above". So there is no response entry a request
//! extension could arrive in. The only map left is the response's own §7.1.7 one — a *different*
//! map, "reserved for implementers to extend the protocol however they see fit" — and §7 draws no
//! line between the two.
//!
//! A driver that wants one echoed therefore says so, by putting it in an [`Extensions`] and calling
//! [`Executor::set_extensions`]. Which keys to copy is policy, and policy is the driver's for the
//! same reason a leaf's representation is. Echoing is also a move or a copy the *driver* makes and
//! never a clone this crate performs: no `V` here carries a [`Clone`] bound, so the entries come
//! from the driver's own value or out of its map with [`Extensions::remove`].
//!
//! ### So the map goes on the driver's [`Values`] implementation
//!
//! Which is the one place both of its readers reach it: the trait's methods during execution, and
//! the driver's own loop between them.
//!
//! ```
//! use graphql_proto::{Executor, Extensions, Leaf, Node, Values};
//! # use smear_parser::{
//! #   graphql::{
//! #     GraphQL,
//! #     ast::{ExecutableDocument, TypeSystemDocument},
//! #     error::GraphqlErrors,
//! #     syntactic::{GraphqlLexer, executable_document, type_system_document},
//! #   },
//! #   lexer::tokora::{Parse as _, Parser},
//! # };
//! # use smear_schema::Schema;
//!
//! // Draft §6's `extensions`, held where every reader can reach it. §6 asks only that it be a map
//! // with unique-prefixed keys; nothing reads it but this driver, so nothing else constrains it.
//! struct Space {
//!   request_extensions: Vec<(String, String)>,
//! }
//!
//! impl Space {
//!   fn request_extension(&self, key: &str) -> Option<&str> {
//!     self
//!       .request_extensions
//!       .iter()
//!       .find(|(name, _)| name == key)
//!       .map(|(_, value)| value.as_str())
//!   }
//! }
//!
//! impl Values for Space {
//!   type Value = String;
//!
//!   fn coerce_leaf(&mut self, value: String, _: Leaf<'_>) -> Option<String> {
//!     // Readable *here*, which is the argument in one line: a `Values` method is handed the
//!     // driver's type and never the executor, so a map parked on an `Executor` could not be read
//!     // at this step at all.
//!     Some(match self.request_extension("com.example.locale") {
//!       Some(locale) => format!("{value} [{locale}]"),
//!       None => value,
//!     })
//!   }
//! #   fn is_null(&self, _: &String) -> bool { false }
//! #   fn as_bool(&self, _: &String) -> Option<bool> { None }
//! #   fn list_len(&self, _: &String) -> Option<usize> { None }
//! #   fn list_item(&mut self, _: &String, _: usize) -> String { String::new() }
//! #   fn type_name<'a>(&'a self, _: &'a String) -> Option<&'a str> { None }
//! #   fn variable(&mut self, _: &str) -> Option<String> { None }
//! }
//!
//! # let schema = Schema::build(
//! #   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
//! #     type_system_document,
//! #   )
//! #   .parse_str("type Query { greeting: String }")
//! #   .expect("the SDL parses"),
//! # )
//! # .expect("the SDL is a schema");
//! # let document = Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
//! #   executable_document,
//! # )
//! # .parse_str("{ greeting }")
//! # .expect("the query parses");
//! let mut space = Space {
//!   request_extensions: vec![("com.example.locale".to_owned(), "en-GB".to_owned())],
//! };
//! let mut executor = Executor::new(&schema, &document);
//! executor
//!   .start(&mut space, None, String::new())
//!   .expect("the operation resolves");
//!
//! while let Some(request) = executor.poll_resolve(&mut space) {
//!   let id = request.id();
//!   executor.handle_resolved(&mut space, id, "hello".to_owned());
//! }
//!
//! // §7.1.8 admits no other channel, so echoing one into the response is a call the driver makes
//! // into draft §7.1.7's map — which is a different map, and the copy is the driver's.
//! let locale = space
//!   .request_extension("com.example.locale")
//!   .expect("the request carried it")
//!   .to_owned();
//! let mut echoed = Extensions::new(executor.limits());
//! echoed
//!   .insert("com.example.locale", locale)
//!   .expect("well under `max_extension_entries`");
//! executor
//!   .set_extensions(echoed)
//!   .expect("an operation is running and the response is not delivered");
//!
//! let response = executor.poll_response().expect("nothing is outstanding");
//! let Node::Object(mut fields) = response.data() else {
//!   panic!("the root is an object")
//! };
//! let (_, value) = fields.next().expect("one field was selected");
//! // The driver read the request extension during execution ...
//! assert!(matches!(value, Node::Leaf(text) if text == "hello [en-GB]"));
//! // ... and separately chose to echo it, which is the only way it reaches a response.
//! assert_eq!(
//!   response
//!     .extensions()
//!     .and_then(|map| map.get("com.example.locale"))
//!     .map(String::as_str),
//!   Some("en-GB"),
//! );
//! ```
//!
//! # Worked example
//!
//! ```
//! use graphql_proto::{Executor, Extensions, Leaf, Node, Values};
//! use smear_parser::{
//!   graphql::{
//!     GraphQL,
//!     ast::{ExecutableDocument, TypeSystemDocument},
//!     error::GraphqlErrors,
//!     syntactic::{GraphqlLexer, executable_document, type_system_document},
//!   },
//!   lexer::tokora::{Parse as _, Parser},
//! };
//! use smear_schema::Schema;
//!
//! // The driver's value representation, and the space that interprets it.
//! #[derive(Clone, Debug)]
//! enum Json {
//!   Null,
//!   Str(String),
//!   Obj(Vec<(String, Json)>),
//! }
//!
//! struct Space;
//!
//! impl Values for Space {
//!   type Value = Json;
//!
//!   fn is_null(&self, value: &Json) -> bool {
//!     matches!(value, Json::Null)
//!   }
//!   fn as_bool(&self, _: &Json) -> Option<bool> {
//!     None
//!   }
//!   fn list_len(&self, _: &Json) -> Option<usize> {
//!     None
//!   }
//!   fn list_item(&mut self, _: &Json, _: usize) -> Json {
//!     Json::Null
//!   }
//!   fn type_name<'a>(&'a self, _: &'a Json) -> Option<&'a str> {
//!     None
//!   }
//!   fn coerce_leaf(&mut self, value: Json, _: Leaf<'_>) -> Option<Json> {
//!     Some(value)
//!   }
//!   fn variable(&mut self, _: &str) -> Option<Json> {
//!     None
//!   }
//! }
//!
//! let schema = Schema::build(
//!   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
//!     type_system_document,
//!   )
//!   .parse_str("type Query { greeting: String }")
//!   .expect("the SDL parses"),
//! )
//! .expect("the SDL is a schema");
//!
//! let document = Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
//!   executable_document,
//! )
//! .parse_str("{ greeting }")
//! .expect("the query parses");
//!
//! let mut space = Space;
//! let mut executor = Executor::new(&schema, &document);
//! executor
//!   .start(&mut space, None, Json::Obj(Vec::new()))
//!   .expect("the operation resolves");
//!
//! // The driver's loop: take work, answer it, take the response.
//! while let Some(request) = executor.poll_resolve(&mut space) {
//!   let id = request.id();
//!   executor.handle_resolved(&mut space, id, Json::Str("hello".to_owned()));
//! }
//!
//! // Draft §7.1.7's entry: the service fills it in, and `proto` reads none of the values. The map
//! // is created under the executor's own ceilings, so `set_extensions` cannot refuse it as too
//! // large — and both `insert` and the attach hand the value back rather than dropping it.
//! let mut extensions = Extensions::new(executor.limits());
//! extensions
//!   .insert("tookMs", Json::Str("3".to_owned()))
//!   .expect("well under `max_extension_entries`");
//! executor
//!   .set_extensions(extensions)
//!   .expect("an operation is running and the response is not delivered");
//!
//! let response = executor.poll_response().expect("nothing is outstanding");
//! assert_eq!(response.error_count(), 0);
//! let carried = response.extensions().expect("the service attached a map");
//! assert_eq!(carried.len(), 1);
//! assert!(matches!(carried.get("tookMs"), Some(Json::Str(text)) if text == "3"));
//! let Node::Object(mut fields) = response.data() else {
//!   panic!("the root is an object")
//! };
//! let (key, value) = fields.next().expect("one field was selected");
//! assert_eq!(key.to_string(), "greeting");
//! assert!(matches!(value, Node::Leaf(Json::Str(text)) if text == "hello"));
//! ```

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]

// The alloc-as-std alias: the crate's files spell their allocations `std::boxed::Box`, which
// resolves to `alloc` in a `no_std` build and to the real `std` otherwise.
//
// `#[allow(unused_extern_crates)]`, ON EVERY MEMBER AND NOT ONLY THE ONE THAT TRIPS TODAY. The
// alias is a crate-wide compatibility shim whose USERS are feature-gated, and
// `unused_extern_crates` is a lint about the item: it fires in any configuration that happens to
// compile none of them. On this tree that is `smear-parser --no-default-features`, where every
// `std::` use is inside a dialect or `rowan` module — but the construct is identical in all five
// members and which one trips is an accident of where the uses sit. Measured: `smear-lexer` 41,
// `smear-parser` 70, `smear-schema` 15, `smear-compiler` 8, `graphql-proto` 82.
//
// Not a narrower `#[cfg]`: the honest predicate would be "any feature whose module names `std::`",
// which is a restatement of the module list and drifts the first time one is added. And not
// deletion, because the gated modules need it.
//
// `unused_extern_crates` is DENIED in `[workspace.lints.rust]` so that this allow is the recorded
// exception rather than the lint being off — a stray `extern crate` anywhere else is a hard error
// locally, with no `RUSTFLAGS` needed. That deny is the repair for the gate, not for the code:
// this construct reddened CI twice, and both times the local run that cleared it was narrower
// than CI's.
#[cfg(not(feature = "std"))]
#[allow(unused_extern_crates)]
extern crate alloc as std;

#[cfg(feature = "std")]
#[allow(unused_extern_crates)]
extern crate std;

mod collect;
mod error;
mod execute;
mod extensions;
mod request;
mod request_error;
mod response;
mod subscribe;
mod values;

pub use collect::variable_key;
pub use error::{Error, Kind};
pub use execute::{Executor, Limits, Response, SetExtensionsError, StartError};
pub use extensions::{Ceiling, Extensions, Full};
pub use request::{Argument, ArgumentSource, FieldRequest, ReqId, SourceField};
pub use request_error::{RequestErrorResult, TooLarge};
pub use response::{Ancestors, Children, INLINE_SEGMENTS, Node, Path, PathIter, Segment};
pub use subscribe::{ResponseStream, SourceEventError};
pub use values::{Leaf, Values};

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` re-exports this crate whole, so every `#[cfg(feature = …)]` inside
/// it is gated by THIS crate's features — and cargo unifies a package's features across the entire
/// graph, so a second dependency naming `graphql-proto` directly could switch a capability on behind a
/// `smear` consumer who never asked for it. Observed, not argued: with
/// `smear = { default-features = false, features = ["std"] }` plus a direct `graphql-proto` dependency,
/// a path the consumer had not enabled resolved.
///
/// `smear` reads these constants and refuses to compile when one disagrees with its own matching
/// feature, which is what makes "the umbrella's feature is the gate" true rather than advertised.
/// The alternative — a facade module per gated path — cannot reach trait impls, which are not
/// namespaced, and would have to mirror every nested `#[cfg]` besides.
///
/// `ci/feature_reachability.py` derives this list from `cargo metadata` and fails when a feature
/// this crate declares has no constant here or no assertion in `smear`.
#[doc(hidden)]
pub mod __features {
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
}
