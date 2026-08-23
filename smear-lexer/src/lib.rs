#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]
// Was `#[allow(clippy::result_large_err)]` on `pub mod lexer;` in `smear`'s crate root, which is
// where it landed when #84 merged the two crates. Splitting them again puts it back on the crate
// root it started on, and it covers the lexer alone once more rather than reaching the parser.
#![allow(clippy::result_large_err)]

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

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub use lossless_lexer::LosslessLexer;

pub use string_lexer::*;

pub use tokora;

/// Implementation detail of the macros this crate exports. Not public API.
///
/// `keyword!` is `#[macro_export]`ed, so its expansion is compiled in whatever crate invokes it
/// and cannot name `tokora` by a path that crate happens to have. `$crate::__private::tokora`
/// is the path that works from anywhere, including from a crate that reaches this one only
/// through `smear`.
#[doc(hidden)]
pub mod __private {
  pub use tokora;
}

/// The error module contains error types and traits for lexer errors.
pub mod error;
/// The hints module contains types and traits for expected token hints.
pub mod hints;
/// Keyword tokens for GraphQL and GraphQLx.
pub mod keywords;
// NO `///` HERE, and that is a fix rather than an omission. A module with doc fragments from
// *both* places — a `///` on the `mod` item and a `//!` header in the file — has the intra-doc
// links of ALL of them resolved against the scope of the FIRST fragment, which is this file's.
// `limits.rs`'s header links `MAX_NESTING_DEPTH`, which is in scope there and not here, so the
// pair made a `-D warnings` rustdoc build fail with "no item named `MAX_NESTING_DEPTH` in scope"
// and no source span to find it by. One fragment, one scope. The same ruling
// `smear-parser/src/lossless/mod.rs` records for the same reason.
pub mod limits;
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

#[cfg(all(test, feature = "graphql", feature = "graphqlx"))]
mod keyword_prefix;

mod string_lexer;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod handlers;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod lossless_lexer;

/// Dialect-agnostic SIMD-lexer primitives shared by the GraphQL and GraphQLx
/// SIMD lexers.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) mod simd;

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` re-exports this crate whole, so every `#[cfg(feature = …)]` inside
/// it is gated by THIS crate's features — and cargo unifies a package's features across the entire
/// graph, so a second dependency naming `smear-lexer` directly could switch a capability on behind a
/// `smear` consumer who never asked for it. Observed, not argued: with
/// `smear = { default-features = false, features = ["std"] }` plus a direct `smear-lexer` dependency,
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
  /// `bstr`, as this crate resolved it.
  pub const BSTR: bool = cfg!(feature = "bstr");
  /// `bytes`, as this crate resolved it.
  pub const BYTES: bool = cfg!(feature = "bytes");
  /// `graphql`, as this crate resolved it.
  pub const GRAPHQL: bool = cfg!(feature = "graphql");
  /// `graphqlx`, as this crate resolved it.
  pub const GRAPHQLX: bool = cfg!(feature = "graphqlx");
  /// `hipstr`, as this crate resolved it.
  pub const HIPSTR: bool = cfg!(feature = "hipstr");
  /// `smallvec`, as this crate resolved it.
  pub const SMALLVEC: bool = cfg!(feature = "smallvec");
  /// `smol-bytes`, as this crate resolved it.
  pub const SMOL_BYTES: bool = cfg!(feature = "smol-bytes");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
}
