#![cfg(all(feature = "rowan", feature = "graphql"))]

use rowan::Language;
use smear::parser::graphql::{kinds::SyntaxKind as K, lossless::GraphQLLang};

#[test]
fn every_kind_round_trips_through_raw() {
  // Declaration order is the contract: `raw` is a cast, so `from_raw` must be its inverse
  // across the whole space. A mis-ordered enum silently mismaps every kind.
  for k in K::ALL {
    let raw = k.raw();
    assert_eq!(
      GraphQLLang::kind_from_raw(rowan::SyntaxKind(raw)),
      *k,
      "kind {k:?} did not round-trip through raw {raw}"
    );
    assert_eq!(GraphQLLang::kind_to_raw(*k), rowan::SyntaxKind(raw));
  }
}

#[test]
fn a_raw_value_outside_the_space_is_rejected_by_the_fallible_door() {
  // `rowan::kind_from_raw` has no fallible form, so the crate needs its own. This is the
  // door the sink's validator uses; the panicking one is only reached for in-space values.
  assert!(K::from_raw(u16::MAX).is_none());
  assert!(K::from_raw(K::ALL.len() as u16).is_none());
  // Positive control: an in-space value must be accepted, or the two asserts above prove nothing.
  assert_eq!(K::from_raw(0), Some(K::Name));
}

#[test]
fn the_declaration_order_array_is_complete() {
  // If a variant is added to the enum but not to ALL, `from_raw` silently loses it.
  assert_eq!(K::ALL.len(), 87, "ALL must list every SyntaxKind variant");
  assert_eq!(K::ALL[K::ALL.len() - 1], K::Root, "Root is declared last");
}
