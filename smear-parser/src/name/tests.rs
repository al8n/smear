use std::string::String;

use tokora::{
  SimpleSpan,
  error::ErrorNode,
  span::AsSpan,
  types::{
    Ident, Status,
    recovery::{Components, FromComponents, RecoveryState},
  },
  utils::IntoComponents,
};

use super::Name;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CustomSpan(u8);

/// A span of the test's own, taking the obligation in the one line a consumer would
/// write. A `Span` sits in a leaf position on every value carrier, so it answers the
/// same question the source representation does (`al8n/smear#176`).
impl crate::value::Leaf for CustomSpan {}

trait OtherLanguage {}

#[test]
fn carrier_preserves_an_arbitrary_unsized_language_marker() {
  let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(1), "field");
  assert_eq!(name.as_span(), &CustomSpan(1));
  assert_eq!(name.source(), &"field");

  let ident: Ident<_, CustomSpan, dyn OtherLanguage> = name.into();
  assert_eq!(ident.as_span(), &CustomSpan(1));

  let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(2), "field");
  let Components {
    span,
    payload,
    status,
  } = name.into_components();
  assert_eq!(span, CustomSpan(2));
  assert_eq!(payload, "field");
  assert_eq!(status, Status::Valid);
}

#[test]
fn decomposing_a_placeholder_and_rebuilding_it_keeps_the_placeholder() {
  // The pair this used to decompose into had no room for the status, so the only rebuild
  // available was `Name::new`, which declares its result valid. That turned a recovery
  // placeholder into ordinary syntax on the way through.
  for (name, expected) in [
    (Name::<&str>::error(SimpleSpan::new(0, 5)), Status::Error),
    (
      Name::<&str>::missing(SimpleSpan::new(5, 5)),
      Status::Missing,
    ),
    (
      Name::<&str>::new(SimpleSpan::new(0, 5), "field"),
      Status::Valid,
    ),
  ] {
    assert_eq!(name.status(), expected);
    assert_eq!(Name::from_components(name.into_components()), name);
  }
}

#[test]
fn source_borrows_non_copy_source() {
  let name = Name::<_, CustomSpan, dyn OtherLanguage>::new(CustomSpan(1), String::from("field"));
  let source: &String = name.source();
  assert_eq!(source, "field");
}
