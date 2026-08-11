//! GraphQL document validation.
//!
//! Standard GraphQL only. The `graphqlx` dialect's generics, `where` constraints, map and set
//! types and namespaced paths have no specification semantics to validate against, so nothing in
//! this module knows about them.
//!
//! # What is here
//!
//! - [`schema`] — the built-once
//!   [`Schema`] every rule reads, and the draft §3 "Type
//!   Validation" pass that runs while building it. A server rejects a malformed SDL exactly once,
//!   at startup, and never rediscovers it per request.
#![cfg_attr(
  feature = "introspection",
  doc = "- [`schema::introspection`] — the second \
         construction door, building that same schema out of a server's draft §4 introspection \
         response. It renders the response as SDL and hands it to the same builder, so draft §3 \
         runs there too and the two doors cannot drift apart."
)]
//! - [`validate_executable`] — the draft §5 rules over a
//!   parsed executable document, reported into the caller's
//!   [`Sink`] and worked out in the caller's
//!   [`Scratch`].
#![cfg_attr(
  feature = "rowan",
  doc = "- [`validate_executable_lossless`] — the \
         same rules over a lossless rowan CST, by projecting it to that same AST rather than by \
         being a second validator. The IDE door: it recovers per definition, so a document \
         somebody is still typing is validated as far as it is well-formed, and the \
         [`Recovery`] it returns says how far that was.\n\
         - [`validate_schema_lossless`] — the same \
         again for SDL, and the reason an editor validating a schema through the CST gets the \
         draft §3 refusals rather than nothing. It projects and calls \
         [`Schema::build`], so §3 has one implementation reached \
         three ways, and it recovers and reports a \
         [`Recovery`] for the same reason its executable twin does."
)]
//!
//! The links here are bare, and they were crate-absolute until the split. While this was
//! `smear::validator`, a module whose declaration carried an outer doc comment, they had to be:
//! rustdoc resolves the MERGED fragments of a module's documentation in the parent's scope, so a
//! `schema`-relative link reported "no item named `schema` in module `smear`" under
//! `RUSTDOCFLAGS="-D warnings"`. A crate root has no parent to be merged into, and the absolute
//! spelling then became a *redundant explicit link target*, which the same flag denies just as
//! firmly. `graphql-proto`'s header records the identical reversal.
//!
//! # Validating a request
//!
//! ```
//! use smear_compiler::{Budget, First, Rule, Schema, Scratch, validate_executable};
//! use smear_parser::{
//!   graphql::{
//!     GraphQL,
//!     ast::{ExecutableDocument, TypeSystemDocument},
//!     error::GraphqlErrors,
//!     syntactic::{GraphqlLexer, executable_document, type_system_document},
//!   },
//!   lexer::tokora::{Parse as _, Parser},
//! };
//!
//! let schema = Schema::build(
//!   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
//!     type_system_document,
//!   )
//!   .parse_str("type Query { hero: Character } interface Character { name: String! }")
//!   .expect("the SDL parses"),
//! )
//! .expect("the SDL is a schema");
//!
//! // The two the caller owns, created once and reused for every request.
//! let mut scratch = Scratch::new();
//! let budget = Budget::default();
//!
//! let request = Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
//!   executable_document,
//! )
//! .parse_str("{ hero { title } }")
//! .expect("the query parses");
//!
//! let mut sink = First::new();
//! let invalid = validate_executable(&schema, &request, &mut scratch, &budget, &mut sink)
//!   .expect_err("`title` is not a field of `Character`");
//! assert_eq!(invalid.emitted(), 1);
//!
//! let diagnostic = sink.get().expect("a diagnostic");
//! assert_eq!(diagnostic.rule(), Rule::FieldSelections);
//! assert_eq!(diagnostic.subject_source(), Some(&"title"));
//! ```
//!
//! # The three axes
//!
//! - **The schema** is built once and read as `&Schema` by an unbounded number of concurrent
//!   validations.
//! - **The report** is one seam: [`Sink::diagnostic`] takes a
//!   value and returns whether to keep going. [`First`],
//!   [`Collect`], [`Count`] and
//!   [`Ignore`] mirror `tokora`'s emitter-bundle semantics.
//! - **The working set** is the caller's [`Scratch`], reused across
//!   requests, and what may be *done* is the caller's [`Budget`]. The
//!   steady state allocates nothing at all, which `tests/validator_allocation.rs` measures with a
//!   counting allocator rather than asserting.
//!
//! # The shape, and why
//!
//! Three properties of [`Schema`] decide everything
//! downstream:
//!
//! - **It is not generic over the document's source type.** The builder consumes a
//!   `TypeSystemDocument<S>` for any `S: AsRef<[u8]>` — the bound `tokora`'s entire source lattice
//!   satisfies — and produces one owned, source-independent value. So the SDL can be dropped the
//!   moment the schema exists, and the schema can be `'static`, sent across threads, or handed to
//!   a foreign caller.
//! - **It holds no `Arc`, no atomics and no interior mutability.** Validation takes `&Schema`, so
//!   there is no shared-ownership primitive to choose and no target tier to gate: the caller wraps
//!   it in whatever pointer their platform has. That also forbids lazy interning and per-query
//!   memoisation inside it, which is a constraint, not an oversight — it is what keeps a rule a
//!   pure function of `(schema, document)`.
//! - **Every cost a rule would otherwise pay per query is paid once, here.** Names are `u32`
//!   symbols, field lookup is a binary search over a sym-sorted group, directive locations are a
//!   `u32` mask, and an interface's implementors are a bitset — so draft 5.5.2.3, all four of its
//!   subsections, is one word-`AND`.

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

/// The built-once schema, and the draft §3 "Type Validation" pass that runs while building it.
///
/// The layer is the `smear-schema` crate; this is the name it has always had inside `smear` and
/// the path every consumer already writes. Its `build` feature — which is what carries
/// [`Schema::build`](smear_schema::Schema::build) — is a hard dependency of this crate rather
/// than a feature of it: a validator with no builder could not produce the `Schema` its rules
/// read.
#[doc(inline)]
pub use smear_schema as schema;

mod diagnostic;
mod executable;
#[cfg(feature = "rowan")]
mod lossless;
mod rule;
mod scratch;
mod sink;

pub use diagnostic::{Context, Diagnostic, DiagnosticDisplay, MergeConflict};
pub use executable::{Invalid, validate_executable, validate_executable_with};

#[cfg(feature = "rowan")]
#[cfg_attr(docsrs, doc(cfg(feature = "rowan")))]
pub use lossless::{
  LosslessInvalid, LosslessSchemaErrors, Recovery, validate_executable_lossless,
  validate_executable_lossless_with, validate_schema_lossless,
};
pub use rule::{Rule, RuleSet};
pub use schema::{Schema, SchemaBuilder, SchemaError, SchemaErrorKind, SchemaErrors};

#[cfg(feature = "introspection")]
#[cfg_attr(docsrs, doc(cfg(feature = "introspection")))]
pub use schema::IntrospectionError;
pub use scratch::{Budget, Scratch};
pub use sink::{Collect, Count, First, Ignore, Sink};

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` re-exports this crate whole, so every `#[cfg(feature = …)]` inside
/// it is gated by THIS crate's features — and cargo unifies a package's features across the entire
/// graph, so a second dependency naming `smear-compiler` directly could switch a capability on behind a
/// `smear` consumer who never asked for it. Observed, not argued: with
/// `smear = { default-features = false, features = ["std"] }` plus a direct `smear-compiler` dependency,
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
  /// `introspection`, as this crate resolved it.
  pub const INTROSPECTION: bool = cfg!(feature = "introspection");
  /// `rowan`, as this crate resolved it.
  pub const ROWAN: bool = cfg!(feature = "rowan");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
}
