#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
// #![deny(missing_docs, warnings)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// The GraphQL parsers
pub mod graphql;

/// The parser
pub mod parser;
mod utils;

#[cfg(all(feature = "std", test))]
mod tests;

#[test]
fn t() {}
