//! What a parsed document is compiled into: the built-once GraphQL schema, with the draft §3
//! builder and the draft §5 validator that read it, and the GraphQLx schema analyzer.
//!
//! # The layers
//!
//! - [`schema`] is the runtime layer and is always compiled: the representation a built schema
//!   has, the diagnostic vocabulary and the specified definitions. A crate that only reads a
//!   schema, as `graphql-proto` does, names a tier and nothing else, and gets no parser.
#![cfg_attr(
  feature = "graphql",
  doc = "- [`graphql`] is GraphQL's compiler: draft §3 inside `Schema::build`, the introspection \
         door behind `introspection`, and draft §5 document validation."
)]
#![cfg_attr(
  not(feature = "graphql"),
  doc = "- `graphql`, behind the feature of that name, is GraphQL's compiler: draft §3 inside \
         `Schema::build`, the introspection door behind `introspection`, and draft §5 document \
         validation."
)]
#![cfg_attr(
  all(any(feature = "std", feature = "alloc"), feature = "graphqlx"),
  doc = "- [`graphqlx`] is GraphQLx's: packages in, a resolved model and its diagnostics out."
)]
#![cfg_attr(
  not(all(any(feature = "std", feature = "alloc"), feature = "graphqlx")),
  doc = "- `graphqlx`, behind the feature of that name and a tier, is GraphQLx's: packages in, a \
         resolved model and its diagnostics out."
)]
//!
//! The two compilers run on one private kernel: a byte-spelling interner, duplicate-name detection
//! and a graph-cycle kernel.
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![forbid(unsafe_code)]
#![deny(missing_docs)]

// The alloc-as-std alias: the crate's files spell their allocations `std::boxed::Box`, which
// resolves to `alloc` in a `no_std` build with `alloc` and to the real `std` in a `std` build. The
// `#[allow]` is the exception the workspace's `unused_extern_crates = "deny"` records: the alias's
// users are gated, and the lint fires in any configuration that compiles none of them.
#[cfg(all(not(feature = "std"), feature = "alloc"))]
#[allow(unused_extern_crates)]
extern crate alloc as std;

#[cfg(feature = "std")]
#[allow(unused_extern_crates)]
extern crate std;

// No outer doc comment on any of the three layers: each module's own header is its one doc
// fragment, so a link in it resolves in the module where it is written.
pub mod schema;

// On the feature alone, not on a tier: with no tier the module still holds `schema`, which is
// what `smear::validator` has always been in that build.
#[cfg(feature = "graphql")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphql")))]
pub mod graphql;

#[cfg(all(any(feature = "std", feature = "alloc"), feature = "graphqlx"))]
#[cfg_attr(
  docsrs,
  doc(cfg(all(any(feature = "std", feature = "alloc"), feature = "graphqlx")))
)]
pub mod graphqlx;

// Private: the schema builder and the analyzer are its two callers, so it is compiled with a tier
// and either compiler.
#[cfg(all(
  any(feature = "std", feature = "alloc"),
  any(feature = "graphql", feature = "graphqlx")
))]
mod kernel;

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` mounts this crate's modules, so every `#[cfg(feature = …)]` inside
/// them is gated by THIS crate's features — and cargo unifies a package's features across the
/// entire graph, so a second dependency naming `smear-compiler` directly could switch a capability
/// on behind a `smear` consumer who never asked for it. Observed, not argued: with
/// `smear = { default-features = false, features = ["std"] }` plus a direct dependency on the
/// crate, a path the consumer had not enabled resolved.
///
/// `smear` reads these constants and refuses to compile when one disagrees with its own matching
/// feature, which is what makes "the umbrella's feature is the gate" true rather than advertised.
/// The alternative — a facade module per gated path — cannot reach trait impls, which are not
/// namespaced, and would have to mirror every nested `#[cfg]` besides.
///
/// Two of the umbrella's twins are not namesakes: `graphql` is asserted against `smear/validator`
/// and `graphqlx` against `smear/analysis`, because the umbrella's own `graphql` and `graphqlx` are
/// the dialects, which turn on a front end and no compiler.
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
  /// `graphql`, as this crate resolved it.
  pub const GRAPHQL: bool = cfg!(feature = "graphql");
  /// `graphqlx`, as this crate resolved it.
  pub const GRAPHQLX: bool = cfg!(feature = "graphqlx");
  /// `hipstr_0_8`, as this crate resolved it.
  pub const HIPSTR_0_8: bool = cfg!(feature = "hipstr_0_8");
  /// `introspection`, as this crate resolved it.
  pub const INTROSPECTION: bool = cfg!(feature = "introspection");
  /// `smol-bytes_0_2`, as this crate resolved it.
  pub const SMOL_BYTES_0_2: bool = cfg!(feature = "smol-bytes_0_2");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
  /// `test-support`, as this crate resolved it.
  pub const TEST_SUPPORT: bool = cfg!(feature = "test-support");
}
