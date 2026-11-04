use std::string::ToString;

use super::{ExponentHint, FloatHint, HexExponentHint, HexFloatHint};

/// An exponent hint displays the exponent hint it carries.
///
/// Both read `#[display("_0")]`, which derive_more takes as the literal text `_0` — a field is
/// addressed as `{_0}` — so every exponent hint rendered as `_0`.
#[test]
fn an_exponent_hint_displays_its_payload() {
  for (hint, expected) in [
    (ExponentHint::Digit, "digit"),
    (ExponentHint::SignOrDigit, "'+', '-' or digit"),
    (ExponentHint::Identifier, "'e' or 'E'"),
  ] {
    assert_eq!(FloatHint::Exponent(hint).to_string(), expected);
  }
  for (hint, expected) in [
    (HexExponentHint::Digit, "digit"),
    (HexExponentHint::SignOrDigit, "'+', '-' or digit"),
    (HexExponentHint::Identifier, "'p' or 'P'"),
  ] {
    assert_eq!(HexFloatHint::Exponent(hint).to_string(), expected);
  }
}
