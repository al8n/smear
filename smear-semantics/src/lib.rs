//! The algorithms smear's two semantic layers share: the GraphQL schema builder and the GraphQLx
//! analyzer.
//!
//! [`interner`] turns byte spellings into dense symbols, [`duplicates`] finds the earlier position
//! of a name list that wrote a given name, and [`scc`] finds which nodes of a directed graph lie on
//! a cycle and which strongly connected component each node is in. Each is generic over its
//! caller's symbol type, and all three need `std` or `alloc`.
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![forbid(unsafe_code)]
#![deny(missing_docs)]

// The alloc-as-std alias: the modules spell their allocations `std::vec::Vec`, which resolves to
// `alloc` in a `no_std` build with `alloc` and to the real `std` in a `std` build. The `#[allow]`
// is the exception the workspace's `unused_extern_crates = "deny"` records: the alias's users are
// gated, and the lint fires in any configuration that compiles none of them.
#[cfg(all(not(feature = "std"), feature = "alloc"))]
#[allow(unused_extern_crates)]
extern crate alloc as std;

#[cfg(feature = "std")]
#[allow(unused_extern_crates)]
extern crate std;

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub mod duplicates;
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub mod interner;
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub mod scc;

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` mounts nothing of this crate, but layers it does mount are built on
/// it, so it asserts these against its own features as it does every member's: a second dependency
/// naming this crate directly cannot raise its tier behind a `smear` consumer who never asked.
///
/// `ci/feature_reachability.py` derives this list from `cargo metadata` and fails when a feature
/// this crate declares has no constant here or no assertion in `smear`.
#[doc(hidden)]
pub mod __features {
  /// `alloc`, as this crate resolved it.
  pub const ALLOC: bool = cfg!(feature = "alloc");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
}
