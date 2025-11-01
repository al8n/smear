#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![allow(clippy::double_parens)]
#![deny(warnings, missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

pub use string_lexer::*;

pub use logosky;

/// The error module contains error types and traits for lexer errors.
pub mod error;
/// The hints module contains types and traits for expected token hints.
pub mod hints;
/// Keyword tokens for GraphQL and GraphQLx.
pub mod keywords;
/// Punctuation tokens used in GraphQL and GraphQLx.
pub mod punctuator;

/// Lexers for standard GraphQL (draft specification).
///
/// This module provides zero-copy tokenization for GraphQL source text. All tokens
/// reference spans in the original source, avoiding unnecessary allocations.
///
/// # Token Streams
///
/// GraphQL lexing offers two complementary token types:
///
/// - **[`SyntacticToken`](graphql::syntactic::SyntacticToken)** - Fast token stream that skips trivia (whitespace, comments, commas)
///   - Use for: GraphQL servers, query execution, performance-critical parsing
///   - Benefits: Minimal memory, maximum speed
///
/// - **[`LosslessToken`](graphql::lossless::LosslessToken)** - Complete token stream that preserves all source information
///   - Use for: Code formatters, linters, IDEs, syntax highlighters
///   - Benefits: Perfect source reconstruction, access to comments and formatting
///
/// # Recognized Tokens
///
/// The GraphQL lexer recognizes:
/// - **Identifiers**: Names for types, fields, arguments, etc.
/// - **Literals**: Integers, floats, strings (inline and block), booleans, null
/// - **Punctuators**: `(`, `)`, `{`, `}`, `[`, `]`, `:`, `=`, `@`, `$`, `!`, `|`, `&`, `,`
/// - **Keywords**: `query`, `mutation`, `subscription`, `fragment`, `type`, `interface`, etc.
/// - **Trivia** (LosslessToken only): Whitespace, comments, commas
///
/// # Source Types
///
/// The lexer is generic over source type `S`:
/// - `&str`: Most common, UTF-8 validated
/// - `&[u8]`: For binary sources, can be converted to `&str` when needed
/// - `bytes::Bytes`: For shared ownership (requires `bytes` feature)
///
/// # Modules
///
/// - [`syntactic`](graphql::syntactic): Syntactic tokens (fast, skips trivia)
/// - [`lossless`](graphql::lossless): Lossless tokens (complete, preserves all formatting)
/// - [`error`](graphql::error): Lexer-specific error types
///
/// # Example
///
/// ```rust,ignore
/// use smear_lexer::graphql::syntactic::Lexer;
///
/// let source = "query { user { id } }";
/// let tokens = Lexer::new(source);
/// for token in tokens {
///   // Only syntactically significant tokens (whitespace automatically skipped)
/// }
/// ```
#[cfg(feature = "graphql")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphql")))]
pub mod graphql;

/// Lexers for GraphQLx (extended GraphQL).
///
/// This module extends the standard GraphQL lexer with additional tokens for GraphQLx
/// features like generics, imports, map types, and namespacing.
///
/// # Token Streams
///
/// GraphQLx lexing offers two complementary token types:
///
/// - **[`SyntacticToken`](graphqlx::syntactic::SyntacticToken)** - Fast token stream that skips trivia (whitespace, comments, commas)
///   - Use for: GraphQLx servers, query execution, performance-critical parsing
///   - Benefits: Minimal memory, maximum speed
///
/// - **[`LosslessToken`](graphqlx::lossless::LosslessToken)** - Complete token stream that preserves all source information
///   - Use for: Code formatters, linters, IDEs, syntax highlighters
///   - Benefits: Perfect source reconstruction, access to comments and formatting
///
/// # Additional Tokens (Beyond GraphQL)
///
/// GraphQLx adds these tokens for advanced features:
/// - **Path separator**: `::` for namespaced types (`namespace::Type`)
/// - **Angle brackets**: `<`, `>` for generics (`Container<T>`)
/// - **Fat arrow**: `=>` for map types (`<Key => Value>`)
/// - **Arithmetic operators**: `+`, `-` for extended type operations
/// - **Asterisk**: `*` for wildcard imports
///
/// # Source Types
///
/// Like the GraphQL lexer, GraphQLx is generic over source type `S`:
/// - `&str`: UTF-8 validated strings
/// - `&[u8]`: Byte slices
/// - `bytes::Bytes`: Shared ownership (requires `bytes` feature)
///
/// # Modules
///
/// - [`syntactic`](graphqlx::syntactic): Syntactic tokens (fast, skips trivia)
/// - [`lossless`](graphqlx::lossless): Lossless tokens (complete, preserves all formatting)
/// - [`error`](graphqlx::error): Lexer-specific error types
///
/// # Example
///
/// ```rust,ignore
/// use smear_lexer::graphqlx::syntactic::Lexer;
///
/// let source = "import { User } from \"./types.graphqlx\"";
/// let tokens = Lexer::new(source);
/// // Fast tokenization with trivia automatically skipped
/// ```
///
/// # Note
///
/// GraphQLx requires the `unstable` and `graphqlx` feature flag.
#[cfg(feature = "graphqlx")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphqlx")))]
pub mod graphqlx;

mod string_lexer;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod handlers;

#[doc(hidden)]
pub mod __private {
  pub use logosky;
}
