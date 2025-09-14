#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Standards-compliant GraphQL parser combinators.
pub mod cst;

/// Error types.
pub mod error;

/// Lexer for Standard GraphQL
pub mod lexer;

/// Parser for Standard GraphQL.
pub mod parser;

pub use smear_parser::{parse, source::WithSource};
