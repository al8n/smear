#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// GraphQL argument parsers
pub mod arguments;
/// GraphQL directive parsers
pub mod directives;
/// GraphQL field parsers
pub mod field;
/// GraphQL value parsers
pub mod value;

#[test]
fn t() {}
