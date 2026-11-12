//! The GraphQLx schema analyzer.
//!
//! Its `graphqlx` module takes GraphQLx packages in and gives a resolved model of them, and the
//! diagnostics on them, back. It needs `alloc`, and without it the module is absent.
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![forbid(unsafe_code)]
#![deny(missing_docs)]

// The alloc-as-std alias: the crate's files spell their allocations `std::vec::Vec`, which resolves
// to `alloc` in a `no_std` build with `alloc` and to the real `std` in a `std` build. The `#[allow]`
// is the exception the workspace's `unused_extern_crates = "deny"` records: the alias's users are
// gated, and the lint fires in any configuration that compiles none of them.
#[cfg(all(not(feature = "std"), feature = "alloc"))]
#[allow(unused_extern_crates)]
extern crate alloc as std;

#[cfg(feature = "std")]
#[allow(unused_extern_crates)]
extern crate std;

// No outer doc comment: the module's own header is its one doc fragment, so a link in it resolves
// in the module where it is written.
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub mod graphqlx;

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` re-exports this crate whole, so every `#[cfg(feature = …)]` inside
/// it is gated by THIS crate's features — and cargo unifies a package's features across the entire
/// graph, so a second dependency naming `smear-analysis` directly could switch a capability on
/// behind a `smear` consumer who never asked for it.
///
/// `smear` reads these constants and refuses to compile when one disagrees with its own matching
/// feature, which is what makes "the umbrella's feature is the gate" true rather than advertised.
///
/// `ci/feature_reachability.py` derives this list from `cargo metadata` and fails when a feature
/// this crate declares has no constant here or no assertion in `smear`.
#[doc(hidden)]
pub mod __features {
  /// `alloc`, as this crate resolved it.
  pub const ALLOC: bool = cfg!(feature = "alloc");
  /// `bstr_1`, as this crate resolved it.
  pub const BSTR_1: bool = cfg!(feature = "bstr_1");
  /// `bytes_1`, as this crate resolved it.
  pub const BYTES_1: bool = cfg!(feature = "bytes_1");
  /// `hipstr_0_8`, as this crate resolved it.
  pub const HIPSTR_0_8: bool = cfg!(feature = "hipstr_0_8");
  /// `smol-bytes_0_2`, as this crate resolved it.
  pub const SMOL_BYTES_0_2: bool = cfg!(feature = "smol-bytes_0_2");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
  /// `test-support`, as this crate resolved it.
  pub const TEST_SUPPORT: bool = cfg!(feature = "test-support");
}
