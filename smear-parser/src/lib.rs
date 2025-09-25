#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_docs)]
#![allow(clippy::type_complexity)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

pub use logosky::chumsky;

/// Language related parsers.
///
/// This module implements all content belongs to the GraphQL language specification.
///
/// Spec: [Language spec](https://spec.graphql.org/draft/#sec-Language)
pub mod lang;

/// Definations parsers
pub mod definitions;

/// Error traits and types
pub mod error;

#[doc(hidden)]
pub mod __private {
  pub use logosky::{self, chumsky};
}

/// Utility types and traits
pub mod utils;

#[cfg(all(feature = "std", test))]
mod tests;
