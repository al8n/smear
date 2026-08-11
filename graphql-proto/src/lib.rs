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
//! # Scope
//!
//! Queries and mutations. Draft §6.2.2's serial rule for a mutation's top-level fields is expressed
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
//! A `subscription` is refused by [`Executor::start`] with
//! [`StartError::NotAQueryOrMutation`]: draft §6.2.3
//! delivers a *stream* of responses over a source event stream, and this surface delivers one.
//!
//! Draft §6.1 `CoerceVariableValues` is the driver's: values reaching [`Values::variable`] are
//! already coerced against their declared types.
//!
//! Of draft §4.4's three meta-fields, one is executed and two are not. `__typename` is answered by
//! the executor from the object type it resolved and arrives as [`Node::TypeName`];
//! the driver is never asked, because the executor has already spent that object type deciding
//! which fragments applied and must not let the answer disagree with them. `__schema` and `__type`
//! are left to the driver as ordinary fields of the query root — draft §4.5 introspection is a
//! later phase, and until it lands this executor has nothing better to say about them than the
//! service does.
//!
//! # Worked example
//!
//! ```
//! use graphql_proto::{Executor, Leaf, Node, Values};
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
//! let response = executor.poll_response().expect("nothing is outstanding");
//! assert_eq!(response.error_count(), 0);
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

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

mod collect;
mod error;
mod execute;
mod request;
mod response;
mod values;

pub use error::{Error, Kind};
pub use execute::{Executor, Limits, Response, StartError};
pub use request::{Argument, ArgumentSource, FieldRequest, ReqId};
pub use response::{Children, Node, Path, PathIter, Segment};
pub use values::{Leaf, Values};
