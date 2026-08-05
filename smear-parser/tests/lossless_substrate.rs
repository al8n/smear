#![cfg(feature = "rowan")]

//! The dialect-generic lossless substrate's own properties.
//!
//! Everything here is asserted through `smear_parser::lossless`, never through a dialect module,
//! because the substrate's whole claim is that it is nameable and usable without one. A test that
//! reached for `graphql::lossless` to prove a substrate property would prove the opposite.

use smear_parser::{
  graphql::kinds::SyntaxKind as GK,
  lossless::{KindSpace, test_support::assert_kind_space_is_well_formed},
};

/// The GraphQL space satisfies the contract every dialect's space must satisfy.
///
/// This is the same three properties `graphql/kinds.rs`'s own `mod tests` asserted, moved to a
/// generic helper so the GraphQLx space (Task 8) inherits them by declaration rather than by
/// somebody remembering to copy three tests.
#[test]
fn the_graphql_kind_space_is_well_formed() {
  assert_kind_space_is_well_formed::<GK>();
}

/// The contract's constants agree with the enum's own spelling.
///
/// Without this, an impl could satisfy `assert_kind_space_is_well_formed` while pointing `ERROR` at
/// `Gap` — the helper only checks the *positions* of the last three, not which name sits in each.
#[test]
fn the_graphql_bookkeeping_constants_name_the_right_kinds() {
  assert_eq!(<GK as KindSpace>::ERROR, GK::Error);
  assert_eq!(<GK as KindSpace>::GAP, GK::Gap);
  assert_eq!(<GK as KindSpace>::ROOT, GK::Root);
  assert_eq!(<GK as KindSpace>::NAME, "graphql");
}

/// The space is not empty and `from_raw` is not a constant function.
///
/// The positive control for the test above: a `from_raw` that answered `None` for everything, or an
/// `ALL` that was empty, would pass a loop over `ALL` vacuously.
#[test]
fn the_graphql_kind_space_answers_in_both_directions() {
  let all = <GK as KindSpace>::ALL;
  assert!(all.len() >= 80, "the space shrank to {}", all.len());
  assert_eq!(
    <GK as KindSpace>::from_raw(<GK as KindSpace>::raw(GK::Name)),
    Some(GK::Name)
  );
  assert_eq!(<GK as KindSpace>::from_raw(all.len() as u16), None);
  assert_eq!(<GK as KindSpace>::from_raw(u16::MAX), None);
}
