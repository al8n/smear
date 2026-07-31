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
/// Available dialects are [`graphql`](lexer::graphql) and, under the `graphqlx`
/// feature, [`graphqlx`](lexer::graphqlx).
pub use smear_lexer as lexer;

/// Parsers for GraphQL and GraphQL-like DSLs.
///
/// Re-export of the [`smear-parser`](smear_parser) crate: atomic parser combinators
/// that build AST nodes from the token streams produced by [`lexer`], composed with
/// the `tokora` combinator library.
///
/// Available dialects are [`graphql`](parser::graphql) and, under the `graphqlx`
/// feature, [`graphqlx`](parser::graphqlx) — the latter adding imports, generics,
/// where-clauses, map and set types, and namespaced paths on top of GraphQL.
pub use smear_parser as parser;

#[doc(hidden)]
pub mod __private {
  pub use tokora;
}
