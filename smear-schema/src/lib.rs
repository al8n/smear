//! The built-once schema: representation, construction, and draft §3 refusal.
//!
//! [`Schema`] is the substrate every validation rule sits on. It is built once from a
//! `TypeSystemDocument`, owns everything it needs, and is then read — never written — by an
//! unbounded number of concurrent validations holding `&Schema`.
//!
//! # The `build` feature, and what the default tier is for
//!
//! Three consumers, and they do not want the same thing. `smear-compiler` BUILDS a schema, so it
//! needs the parser underneath it; `graphql-proto` READS one at execution time and `smear-derive`
//! will generate from one, and neither has any use for a front end. So the builder and the
//! introspection door sit behind `build`, which is what pulls `smear-parser` — and a consumer who
//! only reads a schema depends on this crate with `default-features = false` and gets `core` +
//! `alloc` with `tokora` as the one dependency.
//!
//! That is a FEATURE boundary and not a crate boundary, and the distinction is the whole reason
//! this crate is shaped this way: [`Schema::build`] and `Schema::from_introspection` are
//! **inherent** `impl Schema` blocks, and an inherent impl cannot cross a crate boundary. Splitting
//! them out would have cost an extension trait per body and a `use` line at every call site, for a
//! tier the feature buys outright.
//!
#![cfg_attr(
  feature = "build",
  doc = "```
use smear_parser::{
  graphql::{
    GraphQL, ast::TypeSystemDocument, error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  lexer::tokora::{Parse as _, Parser},
};
use smear_schema::{RootOperation, Schema};

let sdl = \"type Query { hero: Character } interface Character { name: String! }\";
let document = Parser::with_parser::<
  GraphqlLexer<'_, str>,
  TypeSystemDocument<&str>,
  GraphqlErrors<&str>,
  _,
  GraphQL,
>(type_system_document)
.parse_str(sdl)
.expect(\"the SDL parses\");

let schema = Schema::build(&document).expect(\"the SDL is a schema\");
let (query, _) = schema.type_by_name(b\"Query\").expect(\"Query is defined\");
assert_eq!(schema.root(RootOperation::Query), Some(query));

// Introspection is part of every schema, so an introspection query has something to
// validate against.
assert!(schema.type_by_name(b\"__Schema\").is_some());
```"
)]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![forbid(unsafe_code)]
#![deny(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

// The diagnostic vocabulary, and the one place in the layout the design's §2 did not settle.
//
// `error.rs` and `introspection/error.rs` implement `Diagnose`, so `diagnostic` has to sit at or
// BELOW this crate — and §2 puts it in `smear-compiler`, which is above. It ships here because
// this is the lowest crate that needs it, and it is ungated because `smear::diagnostic` is ungated
// today and moving it behind a feature would be an API change. Two consequences, both recorded:
// this crate's `[dependencies]` table is no longer empty (`tokora`, for `SimpleSpan`), and
// `smear` depends on it unconditionally rather than behind `validator`.
//
// It is written to be lifted into `tokora` unchanged, which is why no link in it is `crate::`
// rooted and why this declaration carries no outer doc comment: rustdoc resolves the MERGED
// fragments of a module's documentation in the scope of whichever attribute came from outside, so
// an outer comment here would reinterpret every link inside it as one rooted in this crate.
pub mod diagnostic;

pub mod builtin;

// Deliberately no outer doc comment, for the reason recorded on `diagnostic` above: an outer
// comment here would silently reinterpret every `super::` link in `introspection/mod.rs` as one
// rooted in this crate's root, and they would all stop resolving under `RUSTDOCFLAGS="-D warnings"`.
#[cfg(feature = "introspection")]
#[cfg_attr(docsrs, doc(cfg(feature = "introspection")))]
pub mod introspection;

#[cfg(feature = "build")]
mod builder;
#[cfg(feature = "build")]
mod error;
mod repr;

/// The literal-shape coercion table, shared with the executable rules.
///
/// Crate-private on purpose: it is a seam between two callers inside this crate, not a promise to
/// anyone outside it, and `repr` — the part of this crate that is published widest — stays free of
/// it. It followed the builder down from `smear-compiler` because the builder is its only caller;
/// re-confirmed on this tree by restricting its visibility and rebuilding.
#[cfg(feature = "build")]
pub(crate) mod literal;

#[cfg(feature = "introspection")]
#[cfg_attr(docsrs, doc(cfg(feature = "introspection")))]
pub use introspection::IntrospectionError;

/// Draft §6.3's collection indexes the *document's* names the way [`NameIndex`] indexes the
/// schema's, and one hash for both is one hash to reason about. See [`NameIndex`] for why the two
/// callers owe different arguments about it being unkeyed.
///
/// `#[doc(hidden)]`: a seam between this crate and `graphql-proto`, not a promise to anyone outside
/// the workspace. It was `pub(crate)` behind `#[cfg(feature = "proto")]` while both lived in one
/// crate; a crate boundary has no narrower spelling to offer, and two functions is the whole of the
/// widening.
#[doc(hidden)]
pub use repr::name::{bucket, hash_bytes};

#[cfg(feature = "build")]
#[cfg_attr(docsrs, doc(cfg(feature = "build")))]
pub use builder::SchemaBuilder;
#[cfg(feature = "build")]
#[cfg_attr(docsrs, doc(cfg(feature = "build")))]
pub use error::{SchemaError, SchemaErrorKind, SchemaErrors};
pub use repr::{
  DefaultKind, DirectiveDef, DirectiveLocation, DirectiveLocations, FieldDef, InputValueDef,
  MAX_SYMBOLS, MAX_WRAPPERS, NameIndex, PackedType, Range32, RootOperation, Schema, Sym, TypeDef,
  TypeFlags, TypeId, TypeKind, is_name, is_reserved,
};

/// The features this crate was compiled with, as constants the umbrella asserts against.
///
/// **Not public API.** `smear` re-exports this crate whole, so every `#[cfg(feature = …)]` inside
/// it is gated by THIS crate's features — and cargo unifies a package's features across the entire
/// graph, so a second dependency naming `smear-schema` directly could switch a capability on behind a
/// `smear` consumer who never asked for it. Observed, not argued: with
/// `smear = { default-features = false, features = ["std"] }` plus a direct `smear-schema` dependency,
/// a path the consumer had not enabled resolved.
///
/// `smear` reads these constants and refuses to compile when one disagrees with its own matching
/// feature, which is what makes "the umbrella's feature is the gate" true rather than advertised.
/// The alternative — a facade module per gated path — cannot reach trait impls, which are not
/// namespaced, and would have to mirror every nested `#[cfg]` besides.
///
/// `ci/feature_reachability.py` derives this list from `cargo metadata` and fails when a feature
/// this crate declares has no constant here or no assertion in `smear`.
#[doc(hidden)]
pub mod __features {
  /// `build`, as this crate resolved it.
  pub const BUILD: bool = cfg!(feature = "build");
  /// `introspection`, as this crate resolved it.
  pub const INTROSPECTION: bool = cfg!(feature = "introspection");
  /// `std`, as this crate resolved it.
  pub const STD: bool = cfg!(feature = "std");
}
