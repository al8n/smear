#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Standards-compliant GraphQL parser conbinators.
pub mod ast;

pub use smear_parser::{parse, source::WithSource};

#[test]
fn t() {}
