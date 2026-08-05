#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]

/// Lexers for GraphQL and GraphQL-like DSLs.
///
/// Re-export of the [`smear-lexer`](smear_lexer) crate, which turns source text into
/// zero-copy tokens over `&str`, `&[u8]`, `bytes::Bytes`, `hipstr::{HipStr, HipByt}`
/// and friends.
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
/// Available dialects:
///
#[cfg_attr(feature = "graphql", doc = "- [`graphql`](lexer::graphql)")]
#[cfg_attr(feature = "graphqlx", doc = "- [`graphqlx`](lexer::graphqlx)")]
pub use smear_lexer as lexer;

/// Parsers for GraphQL and GraphQL-like DSLs.
///
/// Re-export of the [`smear-parser`](smear_parser) crate: atomic parser combinators
/// that build AST nodes from the token streams produced by [`lexer`], composed with
/// the `tokora` combinator library.
///
/// Available dialects, each link gated on its feature for the reason recorded on [`lexer`]:
///
#[cfg_attr(feature = "graphql", doc = "- [`graphql`](parser::graphql)")]
#[cfg_attr(
  feature = "graphqlx",
  doc = "- [`graphqlx`](parser::graphqlx) — adds imports, generics, where-clauses, map and set \
         types, and namespaced paths on top of GraphQL."
)]
pub use smear_parser as parser;

#[doc(hidden)]
pub mod __private {
  pub use tokora;
}
