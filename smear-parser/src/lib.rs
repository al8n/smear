#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
// #![deny(missing_docs, warnings)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// The parser
pub mod parser;
mod utils;

#[doc(hidden)]
pub mod __private {
  pub use chumsky;

  pub use crate::parser::{Char, Spanned};
}

#[cfg(all(feature = "std", test))]
mod tests;
