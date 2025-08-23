#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// The GraphQL parser combinators.
pub use smear_parser as parser;

/// The concrete GraphQL parser implementation based on the parser combinators.
pub use smear_graphql as graphql;
