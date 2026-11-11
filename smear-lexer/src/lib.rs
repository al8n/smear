#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]
// Was `#[allow(clippy::result_large_err)]` on `pub mod lexer;` in `smear`'s crate root, which is
// where it landed when #84 merged the two crates. Splitting them again puts it back on the crate
// root it started on, and it covers the lexer alone once more rather than reaching the parser.
#![allow(clippy::result_large_err)]

// Every allocating item is gated on `alloc` alone, so a `std` that stopped implying `alloc` would
// compile them all out and still build green.
#[cfg(all(feature = "std", not(feature = "alloc")))]
compile_error!("the `std` feature must enable `alloc`: check this crate's [features] table");

// The alloc-as-std alias: the crate's files spell their allocations `std::boxed::Box`, which
// resolves to `alloc` in a `no_std` build with `alloc` and to the real `std` in a `std` build.
//
// `#[allow(unused_extern_crates)]`, ON EVERY MEMBER AND NOT ONLY THE ONE THAT TRIPS TODAY. The
// alias is a crate-wide compatibility shim whose USERS are feature-gated, and
// `unused_extern_crates` is a lint about the item: it fires in any configuration that happens to
// compile none of them. On this tree that is `smear-parser` with `alloc` alone, where every
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
#[cfg(all(feature = "alloc", not(feature = "std")))]
#[allow(unused_extern_crates)]
extern crate alloc as std;

#[cfg(feature = "std")]
#[allow(unused_extern_crates)]
extern crate std;

#[cfg(all(feature = "alloc", any(feature = "graphql", feature = "graphqlx")))]
#[cfg_attr(
  docsrs,
  doc(cfg(all(feature = "alloc", any(feature = "graphql", feature = "graphqlx"))))
)]
pub use lossless_lexer::LosslessLexer;

#[cfg(feature = "alloc")]
#[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
pub use string_lexer::*;

#[cfg(feature = "alloc")]
#[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
pub use error::SyntaxErrorKind;

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
#[cfg(feature = "alloc")]
#[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
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
/// - `&str`: Most common; UTF-8 by its type, so nothing is checked again
/// - `&[u8]`: For binary sources. Not validated as UTF-8 wholesale, but its string literals and
///   comments must be well-formed UTF-8: a string literal that is not is refused with
///   [`StringError::UnsupportedCharacter`](crate::error::StringError::UnsupportedCharacter), a
///   comment with
///   [`LexerErrorData::UnknownLexeme`](graphql::error::LexerErrorData::UnknownLexeme), one per
///   ill-formed sequence
/// - `bytes::Bytes`: For shared ownership (requires the `bytes_1` feature); a byte source like `&[u8]`
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
#[cfg(all(feature = "alloc", feature = "graphql"))]
#[cfg_attr(docsrs, doc(cfg(all(feature = "alloc", feature = "graphql"))))]
pub mod graphql;

/// Lexers for GraphQLx (extended GraphQL).
///
/// This module extends the standard GraphQL lexer with additional tokens for GraphQLx
/// features like generics, imports, set and map literals, and namespacing.
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
/// - **Path roots**: `~`, `^` and `..`, each before a `::`, root a path at the package, at a
///   dependency or at the parent module (`~::geo::Point`, `^::geo::Point`, `..::Point`)
/// - **Angle brackets**: `<`, `>` for generics (`Container<T>`)
/// - **Fat arrow**: `=>` for map entries (`map!{ key => value }`)
/// - **Arithmetic operators**: `+`, `-` for extended type operations
/// - **Asterisk**: `*` for wildcard imports
///
/// # Source Types
///
/// Like the GraphQL lexer, GraphQLx is generic over source type `S`:
/// - `&str`: UTF-8 by its type
/// - `&[u8]`: Byte slices, not validated as UTF-8 wholesale; their string literals and comments
///   must be well-formed UTF-8, exactly as in the GraphQL lexer
/// - `bytes::Bytes`: Shared ownership (requires the `bytes_1` feature); a byte source like `&[u8]`
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
#[cfg(all(feature = "alloc", feature = "graphqlx"))]
#[cfg_attr(docsrs, doc(cfg(all(feature = "alloc", feature = "graphqlx"))))]
pub mod graphqlx;

#[cfg(feature = "alloc")]
mod string_lexer;

// Gated with `string_lexer` and not with the dialects: a byte source's block string literals are
// walked through `LitStr::try_from(&[u8])` as well as through the dialects.
#[cfg(feature = "alloc")]
pub(crate) mod utf8;

#[cfg(all(feature = "alloc", any(feature = "graphql", feature = "graphqlx")))]
mod handlers;

#[cfg(all(feature = "alloc", any(feature = "graphql", feature = "graphqlx")))]
mod lossless_lexer;

/// Dialect-agnostic SIMD-lexer primitives shared by the GraphQL and GraphQLx
/// SIMD lexers.
#[cfg(all(feature = "alloc", any(feature = "graphql", feature = "graphqlx")))]
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
  /// `alloc`, as this crate resolved it.
  pub const ALLOC: bool = cfg!(feature = "alloc");
  /// `bstr_1`, as this crate resolved it.
  pub const BSTR_1: bool = cfg!(feature = "bstr_1");
  /// `bytes_1`, as this crate resolved it.
  pub const BYTES_1: bool = cfg!(feature = "bytes_1");
  /// `graphql`, as this crate resolved it.
  pub const GRAPHQL: bool = cfg!(feature = "graphql");
  /// `graphqlx`, as this crate resolved it.
  pub const GRAPHQLX: bool = cfg!(feature = "graphqlx");
  /// `hipstr_0_8`, as this crate resolved it.
  pub const HIPSTR_0_8: bool = cfg!(feature = "hipstr_0_8");
  /// `smallvec_1`, as this crate resolved it.
  pub const SMALLVEC_1: bool = cfg!(feature = "smallvec_1");
  /// `smol-bytes_0_2`, as this crate resolved it.
  pub const SMOL_BYTES_0_2: bool = cfg!(feature = "smol-bytes_0_2");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
}
