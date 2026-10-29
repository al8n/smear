//! `smear`'s schema representation, compiled for a core without atomics.
//!
//! # What this member is for
//!
//! `smear-schema` claims three things about [`repr::Schema`] that a reader
//! has to take on trust unless something checks them:
//!
//! 1. it depends on nothing but `core` and `alloc`;
//! 2. it contains no `Arc`, no atomics and no interior mutability, so it is `Send + Sync` by
//!    construction and needs no shared-ownership primitive of its own;
//! 3. it is therefore reachable from an embedded target — including one with no native
//!    compare-and-swap.
//!
//! This crate turns all three into a build. It is `#![no_std]` over `alloc`, it has an empty
//! `[dependencies]` table, and CI builds it for `thumbv6m-none-eabi` (Cortex-M0+), where
//! `alloc::sync` does not exist and `core::sync::atomic`'s pointer-width types are absent. An
//! `Arc`, a `Cell` or a stray `use` anywhere in the representation fails that build.
//!
//! # It compiles the originals, not a copy
//!
//! The module below is `#[path]`-included straight out of `smear-schema/src/repr/`.
//! Nothing is duplicated, so nothing can drift: this is the same source text `smear` ships, read
//! through a second crate root that supplies a different `std`.
//!
//! # The claim is about `repr/`, not about `smear-schema` as a whole
//!
//! `smear-schema`'s own `[dependencies]` table is not empty — the diagnostic vocabulary moved down
//! with it and needs `tokora`, and its `build` feature pulls `smear-parser`. None of that reaches
//! the directory below, which is the only thing this crate compiles and the only thing the claim
//! was ever about: `repr/` still has exactly one outbound path, the alloc-as-std alias, and a
//! `use` anywhere in it reaching past `core` and `alloc` fails this build.
//!
//! # Why `smear` itself is not built for the target
//!
//! It cannot be. `smear-parser`'s `graphql::ast` offers an `Arc`-backed spelling of recursive list
//! types (`ArcType`), which needs `alloc::sync` and therefore native atomics, and the crate's
//! `memspan` dependency is pulled in with `std`. The claim under test was never that the parser
//! reaches an embedded core — it is that the *schema* does, because a build-once, read-many value
//! is shared as `&Schema` and lets the caller pick the pointer: `alloc::sync::Arc` where there
//! are atomics, `portable_atomic_util::Arc` where there are not.

#![no_std]
#![forbid(unsafe_code)]
#![deny(missing_docs)]

// The same aliasing `smear` uses when its `std` feature is off, which is what lets the included
// files spell their allocations `std::boxed::Box` in both crates.
extern crate alloc as std;

/// The schema representation, compiled from `smear`'s own files.
#[path = "../../smear-schema/src/repr/mod.rs"]
pub mod repr;

use repr::{
  Deprecation, DirectiveDef, DirectiveLocations, DirectiveMeta, EnumValueMeta, FieldDef, FieldMeta,
  InputValueDef, InputValueMeta, NameIndex, PackedType, Range32, Schema, Sym, Text, TypeDef,
  TypeId, TypeMeta,
};

/// The claims, as compile-time assertions.
///
/// `Send + Sync` is the one the whole design rests on: validation takes `&Schema`, so a schema
/// that were not both could not be shared, and the absence of interior mutability is what makes
/// it both. The row types are asserted separately so a regression names the table it is in.
const _: () = {
  const fn send_sync<T: Send + Sync>() {}
  let _ = send_sync::<Schema>;
  let _ = send_sync::<NameIndex>;
  let _ = send_sync::<TypeDef>;
  let _ = send_sync::<FieldDef>;
  let _ = send_sync::<InputValueDef>;
  let _ = send_sync::<DirectiveDef>;
  // The side tables draft §4's metadata lives in, and the handle into their text arena.
  let _ = send_sync::<Text>;
  let _ = send_sync::<Deprecation>;
  let _ = send_sync::<TypeMeta>;
  let _ = send_sync::<FieldMeta>;
  let _ = send_sync::<InputValueMeta>;
  let _ = send_sync::<EnumValueMeta>;
  let _ = send_sync::<DirectiveMeta>;

  // Every row is a plain integer record, so the tables are `Box<[T]>` of `Copy` values with no
  // interior pointers. `Copy` is the property that says so.
  const fn plain<T: Copy>() {}
  let _ = plain::<Sym>;
  let _ = plain::<TypeId>;
  let _ = plain::<Range32>;
  let _ = plain::<PackedType>;
  let _ = plain::<TypeDef>;
  let _ = plain::<FieldDef>;
  let _ = plain::<InputValueDef>;
  let _ = plain::<DirectiveDef>;
  let _ = plain::<DirectiveLocations>;
  let _ = plain::<Text>;
  let _ = plain::<Deprecation>;
  let _ = plain::<TypeMeta>;
  let _ = plain::<FieldMeta>;
  let _ = plain::<InputValueMeta>;
  let _ = plain::<EnumValueMeta>;
  let _ = plain::<DirectiveMeta>;
};

/// The text handle's two states, exercised without an arena to issue one.
///
/// Absent is not empty: [`Text::NONE`] is the one handle no arena issues, and the empty string at
/// offset zero is a string. A `const` evaluation says it on the target, for the reason the wrapper
/// word's block below gives.
const _: () = {
  assert!(Text::NONE.is_none());
  assert!(Text::new(0, 0).is_some());
  assert!(Text::new(7, 3).is_some());
  assert!(!Deprecation::NotDeprecated.is_deprecated());
  assert!(Deprecation::Deprecated(Text::NONE).is_deprecated());
  assert!(Deprecation::Deprecated(Text::NONE).reason().is_none());
  assert!(Deprecation::NotDeprecated.reason().is_none());
  assert!(FieldMeta::EMPTY.description().is_none());
  assert!(InputValueMeta::EMPTY.default_value().is_none());
};

/// The wrapper word, exercised without a schema to build it from.
///
/// A `const` evaluation is the strongest form this can take: it runs at compile time on the
/// target, needs no allocator and no test harness, and would stop the build rather than a test
/// run. `[[Int!]!]!` is the shape that uses every code and both nesting directions.
const _: () = {
  let int = PackedType::named(Sym::new(0), TypeId::new(0));
  assert!(int.depth() == 0);
  assert!(!int.is_non_null());

  let Some(non_null) = int.push_non_null() else {
    panic!("one wrapper fits")
  };
  let Some(list) = non_null.push_list() else {
    panic!("two wrappers fit")
  };
  let Some(list) = list.push_non_null() else {
    panic!("three wrappers fit")
  };
  let Some(outer) = list.push_list() else {
    panic!("four wrappers fit")
  };
  let Some(outer) = outer.push_non_null() else {
    panic!("five wrappers fit")
  };

  assert!(outer.depth() == 5);
  assert!(outer.is_non_null());
  assert!(!outer.is_list());
  assert!(outer.nullable().is_list());
};
