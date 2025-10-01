#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// The error types and traits
pub mod error;

/// The hints about expection of GraphQL definitions
pub mod hints;

/// Punctuation tokens.
pub mod punctuator;

/// Keyword tokens.
pub mod keywords;

/// The scaffold structures for GraphQL definitions
pub mod scaffold;

/// The lexers for GraphQL or GraphQL-like DSLs.
pub mod lexer;

/// The parsers for GraphQL or GraphQL-like DSLs.
pub mod parser;

#[doc(hidden)]
pub mod __private {
  pub use logosky::{self, chumsky};
}
