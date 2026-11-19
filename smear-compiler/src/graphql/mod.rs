//! GraphQL document validation.
//!
//! Standard GraphQL only. The `graphqlx` dialect's generics, `where` constraints, set and map
//! literals and namespaced paths have no specification semantics to validate against, so nothing in
//! this module knows about them.
//!
//! # What is here
//!
//! - [`schema`] — the built-once
//!   [`Schema`] every rule reads, and the draft §3 "Type
//!   Validation" pass that runs while building it. A server rejects a malformed SDL exactly once,
//!   at startup, and never rediscovers it per request.
#![cfg_attr(
  all(any(feature = "std", feature = "alloc"), feature = "introspection"),
  doc = "- [`schema::introspection`] — the second \
         construction door, building that same schema out of a server's draft §4 introspection \
         response. It renders the response as SDL and hands it to the same builder, so draft §3 \
         runs there too and the two doors cannot drift apart."
)]
//! - [`validate_executable`] — the draft §5 rules over a
//!   parsed executable document, reported into the caller's
//!   [`Sink`] and worked out in the caller's
//!   [`Scratch`]. The document is the one either of `smear-parser`'s parser layers answered, or one
//!   the caller built, and a syntax error never reaches the sink: it leaves by the layer's own.
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
//! use smear_compiler::graphql::{Budget, First, Rule, Schema, Scratch, validate_executable};
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

// The crate's `schema` module, re-exported so that `smear::validator::schema` resolves. No outer
// doc comment: the module's own header is its one doc fragment, so its links resolve where they
// are written.
#[doc(inline)]
pub use crate::schema;

#[cfg(any(feature = "std", feature = "alloc"))]
mod diagnostic;
#[cfg(any(feature = "std", feature = "alloc"))]
mod executable;
#[cfg(any(feature = "std", feature = "alloc"))]
mod rule;
#[cfg(any(feature = "std", feature = "alloc"))]
mod scratch;
#[cfg(any(feature = "std", feature = "alloc"))]
mod sink;

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub use diagnostic::{
  Context, Diagnostic, DiagnosticDisplay, MemberContext, MergeConflict, UsageContext,
};
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub use executable::{Invalid, Refusal, validate_executable, validate_executable_with};
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub use rule::{Rule, RuleSet};
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub use schema::{Schema, SchemaBuilder, SchemaError, SchemaErrorKind, SchemaErrors};

#[cfg(all(any(feature = "std", feature = "alloc"), feature = "introspection"))]
#[cfg_attr(
  docsrs,
  doc(cfg(all(any(feature = "std", feature = "alloc"), feature = "introspection")))
)]
pub use schema::IntrospectionError;
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub use scratch::{Budget, Scratch};
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub use sink::{Collect, Count, First, Ignore, Sink};
