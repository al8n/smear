//! GraphQL document validation.
//!
//! Standard GraphQL only. The `graphqlx` dialect's generics, `where` constraints, map and set
//! types and namespaced paths have no specification semantics to validate against, so nothing in
//! this module knows about them.
//!
//! # What is here
//!
//! - [`schema`](crate::validator::schema) — the built-once
//!   [`Schema`](crate::validator::schema::Schema) every rule reads, and the draft §3 "Type
//!   Validation" pass that runs while building it. A server rejects a malformed SDL exactly once,
//!   at startup, and never rediscovers it per request.
#![cfg_attr(
  feature = "introspection",
  doc = "- [`schema::introspection`](crate::validator::schema::introspection) — the second \
         construction door, building that same schema out of a server's draft §4 introspection \
         response. It renders the response as SDL and hands it to the same builder, so draft §3 \
         runs there too and the two doors cannot drift apart."
)]
//! - [`validate_executable`](crate::validator::validate_executable) — the draft §5 rules over a
//!   parsed executable document, reported into the caller's
//!   [`Sink`](crate::validator::Sink) and worked out in the caller's
//!   [`Scratch`](crate::validator::Scratch).
#![cfg_attr(
  feature = "rowan",
  doc = "- [`validate_executable_lossless`](crate::validator::validate_executable_lossless) — the \
         same rules over a lossless rowan CST, by projecting it to that same AST rather than by \
         being a second validator. The IDE door: it recovers per definition, so a document \
         somebody is still typing is validated as far as it is well-formed, and the \
         [`Recovery`](crate::validator::Recovery) it returns says how far that was.\n\
         - [`validate_schema_lossless`](crate::validator::validate_schema_lossless) — the same \
         again for SDL, and the reason an editor validating a schema through the CST gets the \
         draft §3 refusals rather than nothing. It projects and calls \
         [`Schema::build`](crate::validator::Schema::build), so §3 has one implementation reached \
         three ways, and it recovers and reports a \
         [`Recovery`](crate::validator::Recovery) for the same reason its executable twin does."
)]
//!
//! Every link in this module's docs is crate-absolute on purpose. `pub mod validator;` in
//! `lib.rs` carries an outer doc comment as well, and rustdoc resolves the merged fragments in
//! the **parent** module's scope — so a `schema`-relative link here reports "no item named
//! `schema` in module `smear`" under `RUSTDOCFLAGS="-D warnings"`.
//!
//! # Validating a request
//!
//! ```
//! use smear::{
//!   lexer::tokora::{Parse as _, Parser},
//!   parser::graphql::{
//!     GraphQL,
//!     ast::{ExecutableDocument, TypeSystemDocument},
//!     error::GraphqlErrors,
//!     syntactic::{GraphqlLexer, executable_document, type_system_document},
//!   },
//!   validator::{Budget, First, Rule, Schema, Scratch, validate_executable},
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
//! - **The report** is one seam: [`Sink::diagnostic`](crate::validator::Sink::diagnostic) takes a
//!   value and returns whether to keep going. [`First`](crate::validator::First),
//!   [`Collect`](crate::validator::Collect), [`Count`](crate::validator::Count) and
//!   [`Ignore`](crate::validator::Ignore) mirror `tokora`'s emitter-bundle semantics.
//! - **The working set** is the caller's [`Scratch`](crate::validator::Scratch), reused across
//!   requests, and what may be *done* is the caller's [`Budget`](crate::validator::Budget). The
//!   steady state allocates nothing at all, which `tests/validator_allocation.rs` measures with a
//!   counting allocator rather than asserting.
//!
//! # The shape, and why
//!
//! Three properties of [`Schema`](crate::validator::schema::Schema) decide everything
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

pub mod schema;

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
