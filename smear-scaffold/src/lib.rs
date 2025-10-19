#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![deny(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Parser error traits
pub mod error;

/// Hint types for parser errors
pub mod hints;

/// The scaffold AST nodes
pub mod ast;

/// The scaffold CST nodes
#[cfg(feature = "cst")]
#[cfg_attr(docsrs, doc(cfg(feature = "cst")))]
pub mod cst;