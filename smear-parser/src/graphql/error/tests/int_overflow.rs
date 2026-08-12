//! The checked door onto [`IntOverflow`], which is the only way an out-of-crate caller can name
//! an [`IntWidth`].
//!
//! The property under test is not "the constructor works". It is that **the pair it builds is
//! consistent**: a payload's width has to be one the literal beside it genuinely overflows. A
//! width-less overflow was already unrepresentable before this door existed — every constructor
//! demanded a width — and that turned out to be the easy half. A *wrong*-width one was
//! constructible, because the constructor took the discriminant as an argument and believed it,
//! so `IntOverflow::new("2147483648", IntWidth::I64)` compiled and a renderer trusting the payload
//! reported a 64-bit refusal of a value that is a perfectly good `i64`.
//!
//! What the crate's own paths can produce is held elsewhere and deliberately: the marker widths by
//! `each_marker_names_its_own_width`, the end-to-end widths by
//! `the_two_widths_disagree_on_the_literal_between_them`, and the agreement between *this* door
//! and those productions by `the_checked_constructor_admits_exactly_what_the_productions_refuse`
//! — a promise about another function's behaviour is worth what a test comparing them is worth.
//! These are the door's own boundaries.

use crate::graphql::error::{ErrorData, GraphqlError, IntOverflow, IntWidth};
use tokora::SimpleSpan as Span;

/// `i32::MAX + 1`: out of range at the specified width, and a value at the permissive one. The
/// literal the two readings disagree about, and therefore the one a wrong width lies about.
const PAST_I32: &str = "2147483648";
/// `i64::MAX + 1`: out of range at both, so either width is a true statement about it.
const PAST_I64: &str = "9223372036854775808";

/// The finding, refused. Each half of the pair is a fact the other has to be consistent with.
#[test]
fn a_width_the_literal_does_not_overflow_is_refused() {
  // The exact forgery: an `I64` overflow for a literal that only overflows `i32`.
  assert_eq!(
    IntOverflow::checked(PAST_I32, IntWidth::I64),
    Err(PAST_I32),
    "`2147483648` is a value at 64 bits, so no 64-bit conversion ever refused it",
  );

  // …and the same pair at the width that did refuse it is accepted, so the refusal above is not
  // the constructor declining everything.
  let overflow = IntOverflow::checked(PAST_I32, IntWidth::I32).expect("outside `i32`");
  assert_eq!(*overflow.value(), PAST_I32);
  assert_eq!(overflow.width(), IntWidth::I32);
}

/// A literal past both widths may name either, because both readers refuse it — and the crate's
/// own 32-bit production does exactly that, so a check that insisted on "the narrowest width that
/// refuses it" would reject a payload the parser produces.
#[test]
fn a_literal_past_both_widths_may_name_either() {
  for width in [IntWidth::I32, IntWidth::I64] {
    let overflow = IntOverflow::checked(PAST_I64, width).expect("outside both widths");
    assert_eq!(overflow.width(), width);
  }
}

/// An in-range literal is not an overflow at the width that holds it, at either boundary or in
/// the middle.
#[test]
fn a_literal_that_fits_is_not_an_overflow() {
  for (literal, width) in [
    ("0", IntWidth::I32),
    ("0", IntWidth::I64),
    ("-0", IntWidth::I32),
    ("7", IntWidth::I32),
    ("7", IntWidth::I64),
    ("2147483647", IntWidth::I32),
    ("-2147483648", IntWidth::I32),
    ("9223372036854775807", IntWidth::I64),
    ("-9223372036854775808", IntWidth::I64),
  ] {
    assert_eq!(
      IntOverflow::checked(literal, width),
      Err(literal),
      "{literal:?} fits {width} and is not an overflow at it",
    );
  }
}

/// Not an integer literal at all is not an integer overflow either — at **either** width, which
/// is the case a check written as "the reader said no" would get wrong in both directions.
///
/// The reader behind the check answers `None` to two different questions — "outside the width"
/// and "not an integer at all" — and only the first is an overflow. Without the digit-shape
/// conjunct, `checked("hello", I64)` would build an error saying `hello` overflowed 64 bits.
#[test]
fn a_non_literal_is_not_an_overflow_at_any_width() {
  for literal in ["", "-", "hello", "1.0", "1e400", "1x", "+7", " 7", "7 "] {
    for width in [IntWidth::I32, IntWidth::I64] {
      assert_eq!(
        IntOverflow::checked(literal, width),
        Err(literal),
        "{literal:?} is not an integer literal, so it overflows nothing at {width}",
      );
    }
  }
}

/// The refusal hands the literal back rather than dropping it — a caller told "this is not an
/// overflow" still has a document to report something else about.
#[test]
fn a_refusal_returns_the_literal_unconsumed() {
  let owned = std::string::String::from(PAST_I32);
  let returned = IntOverflow::checked(owned, IntWidth::I64).expect_err("a value at 64 bits");
  assert_eq!(returned, PAST_I32);

  // And the accepting direction keeps it too, so nothing is lost on either arm.
  let overflow = IntOverflow::checked(returned, IntWidth::I32).expect("outside `i32`");
  assert_eq!(overflow.into_value(), PAST_I32);
}

/// A byte-slice source reaches the same decision as a `str` one — the bound is `AsRef<[u8]>` and
/// the reader behind it walks bytes, so a caller on the slice tier is not a second answer.
#[test]
fn the_decision_is_the_same_on_a_byte_slice_source() {
  assert!(IntOverflow::checked(PAST_I32.as_bytes(), IntWidth::I32).is_ok());
  assert_eq!(
    IntOverflow::checked(PAST_I32.as_bytes(), IntWidth::I64),
    Err(PAST_I32.as_bytes()),
  );
}

/// The producer takes the payload, so the width a report can name is one the payload justified.
///
/// The assertion is the *call*: `int_overflow` no longer has a parameter a caller could put a
/// freely chosen [`IntWidth`] into, and the only thing that fits the slot is a value the checked
/// door or this crate's own conversions produced.
#[test]
fn the_producer_carries_the_checked_payload_into_the_error() {
  let overflow = IntOverflow::checked(PAST_I32, IntWidth::I32).expect("outside `i32`");
  let error = GraphqlError::<&str>::int_overflow(overflow, Span::new(0, PAST_I32.len()));

  assert_eq!(error.span(), Span::new(0, PAST_I32.len()));
  match error.data() {
    ErrorData::IntOverflow(overflow) => {
      assert_eq!(*overflow.value(), PAST_I32);
      assert_eq!(overflow.width(), IntWidth::I32);
      assert_eq!(overflow.width().bits(), 32);
    }
    other => panic!("expected IntOverflow, got {other:?}"),
  }
}
