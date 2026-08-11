#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

// Deliberately no outer doc comment, unlike the three modules below. Rustdoc resolves the MERGED
// fragments of a module's documentation in the scope of whichever attribute came from outside, so
// an outer comment here would reinterpret every link in `diagnostic/mod.rs` as one rooted in
// `smear` — sixteen `unresolved link` errors under `RUSTDOCFLAGS="-D warnings"`, all of them for
// items that are right there in the module. `validator/mod.rs` pays for its outer comment by
// spelling every link crate-absolute; this module does not, because it is written to be lifted
// into `tokora` unchanged and a `crate::`-rooted link is the one thing in it that would not
// survive the move. The module's own header supplies the summary line the crate index shows.
pub mod diagnostic;

/// Lexers for GraphQL and GraphQL-like DSLs.
///
/// Turns source text into zero-copy tokens over `&str`, `&[u8]`, `bytes::Bytes`,
/// `hipstr::{HipStr, HipByt}` and friends.
///
/// Each dialect exposes two token streams:
///
/// - `syntactic` — skips trivia (whitespace, commas, comments). For servers and
///   query execution, where speed matters and formatting does not.
/// - `lossless` — preserves every byte of trivia. For formatters, linters, IDEs and
///   syntax highlighters, where the source must round-trip exactly.
///
/// A dialect is a module only in a build that enables its feature, so each link below is gated
/// to the build that has something to link to. Ungated, they are `unresolved link` errors under
/// `RUSTDOCFLAGS="-D warnings"` in every single-dialect configuration — the failure the `docs`
/// CI job's dialect-alone legs exist to catch.
///
/// This module is the crate's irreducible base and carries no feature of its own: the parser
/// cannot exist without it, and a build with both switched off would be an empty crate. The
/// cross-reference below is gated for the same reason the dialect links are — `parser` is a
/// module only in a build that enables its feature, and an ungated link to it is an
/// `unresolved link` error under `RUSTDOCFLAGS="-D warnings"` in every lexer-alone build.
///
#[cfg_attr(
  feature = "parser",
  doc = "See [`parser`] for the layer built on top of it."
)]
/// Available dialects:
///
#[cfg_attr(feature = "graphql", doc = "- [`graphql`](lexer::graphql)")]
#[cfg_attr(feature = "graphqlx", doc = "- [`graphqlx`](lexer::graphqlx)")]
// Scoped here rather than crate-wide: it was `#![allow(clippy::result_large_err)]` on the
// `smear-lexer` crate root, and the merge is the moment it stops covering the parser too.
#[allow(clippy::result_large_err)]
pub mod lexer;

/// Parsers for GraphQL and GraphQL-like DSLs.
///
/// Atomic parser combinators that build AST nodes from the token streams produced by [`lexer`],
/// composed with the `tokora` combinator library, plus the lossless CST tower behind the `rowan`
/// feature.
///
/// Available dialects, each link gated on its feature for the reason recorded on [`lexer`]:
///
#[cfg_attr(feature = "graphql", doc = "- [`graphql`](parser::graphql)")]
#[cfg_attr(
  feature = "graphqlx",
  doc = "- [`graphqlx`](parser::graphqlx) — adds imports, generics, where-clauses, map and set \
         types, and namespaced paths on top of GraphQL."
)]
#[cfg(feature = "parser")]
#[cfg_attr(docsrs, doc(cfg(feature = "parser")))]
// Scoped here rather than crate-wide: it was `#![allow(clippy::type_complexity)]` on the
// `smear-parser` crate root, and the merge is the moment it stops covering the lexer too.
#[allow(clippy::type_complexity)]
pub mod parser;

/// GraphQL document validation, and the built-once schema every rule reads.
///
/// Draft §3 "Type Validation" runs inside [`Schema::build`](validator::Schema::build), so a
/// malformed SDL is refused once at startup rather than rediscovered per request.
///
#[cfg_attr(
  feature = "introspection",
  doc = "A schema can also be built from a server's draft §4 introspection response — \
         [`Schema::from_introspection`](validator::Schema::from_introspection), behind the \
         `introspection` feature. It is a second entrance to the same value: it renders the \
         response as SDL and hands it to the same builder, so draft §3 runs there too and the \
         two doors cannot drift apart."
)]
#[cfg_attr(
  not(feature = "introspection"),
  doc = "The `introspection` feature adds a second construction door, building the same schema \
         out of a server's draft §4 introspection response instead of out of SDL."
)]
///
/// Standard GraphQL only —
#[cfg_attr(
  feature = "graphqlx",
  doc = "[`graphqlx`](parser::graphqlx) has no validator, deliberately: its generics, \
         `where` constraints, map and set types and namespaced paths have no specification \
         semantics, so validating them would be language design rather than conformance."
)]
#[cfg_attr(
  not(feature = "graphqlx"),
  doc = "the GraphQLx dialect has no validator, deliberately: its extensions have no \
         specification semantics to validate against."
)]
#[cfg(feature = "validator")]
#[cfg_attr(docsrs, doc(cfg(feature = "validator")))]
pub mod validator;

/// Draft §6 execution, as a Sans-I/O state machine.
///
/// Runs a validated query against a built [`Schema`](validator::Schema) and produces a draft §7.1
/// response, performing no I/O and calling no resolver: it says which field it needs and waits to
/// be told the answer.
///
/// It defines no value type. The driver keeps values in its own representation and answers
/// structural questions about them through [`Values`](proto::Values) — the same choice [`lexer`]
/// makes about its source, made for the same reason: an owned enum would force an allocation per
/// leaf and would make a wasm or FFI handle second-class.
///
/// Requires `validator`, because execution is entered with a document the draft §5 rules have
/// already accepted.
#[cfg(feature = "proto")]
#[cfg_attr(docsrs, doc(cfg(feature = "proto")))]
pub mod proto;

#[doc(hidden)]
pub mod __private {
  pub use tokora;
}
