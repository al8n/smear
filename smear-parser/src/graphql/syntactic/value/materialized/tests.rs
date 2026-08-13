//! Materialised-value production tests, at every width the module parses.
//!
//! Driven end to end over the real GraphQL syntactic lexer under a `Fatal<GraphqlErrors>`
//! context, the same way the slice suite next door drives its productions, and over both a `str`
//! and a `[u8]` source so the `AsRef<[u8]>` path is exercised on a non-UTF-8-typed backing too.
//!
//! # One suite, because there is one module
//!
//! Two suites shipped before this: `materialized::tests` at `i64` and `materialized32::tests` at
//! `i32`, most of each being the other with a type substituted. They are one file now, and the
//! question a test asks decides its shape rather than the file it is in:
//!
//! * a question with a width in it — what a leaf converts to, what a refusal names — is asked
//!   **at both**, through a generic body called twice, so a width cannot be silently exercised at
//!   one and not the other;
//! * a question with no width in it — a string keeping its slice, a float refusal, a syntax error
//!   that is not a conversion — is asked once, of the production that takes no `I`.
//!
//! **Two tests did not survive the collapse, and their absence is deliberate.**
//! `only_the_int_leaf_differs_between_the_two_widths` compared the two trees' `Debug` output over
//! an integer-free document, and `out_of_range_float_is_the_same_error_at_both_widths` compared
//! two float refusals. Both were cross-checks between two *implementations*; there is one
//! implementation now — one `enum` whose only `I`-typed slot is `Int`, and one `numbers::float`
//! that takes no width — so each comparison was between a thing and itself. A parity gate whose
//! two sides have been unified cannot fail, and nothing announces that on its own, so it is said
//! here.

use tokora::{FatalContext, Parse, Parser, span::AsSpan};

use super::{
  MaterialisedInt, const_value, float_value, int_value, list_value, object_value, try_int_value,
  value,
};
use crate::graphql::{
  GraphQL,
  ast::materialized::{ConstInputValue, InputValue},
  error::{ErrorData, GraphqlErrors, IntOverflow, IntWidth},
  syntactic::{GraphqlInput, GraphqlLexer},
};
use tokora::try_parse_input::ParseAttempt;

type StrCtx<'inp> = FatalContext<'inp, GraphqlLexer<'inp, str>, GraphqlErrors<&'inp str>, GraphQL>;
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>, GraphQL>;

fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(f)
    .parse_str(input)
}

fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, [u8]>, O, GraphqlErrors<&'inp [u8]>, _, GraphQL>(f)
    .parse_slice(input)
}

/// What a width-carrying test needs of `I` beyond parsing: to compare a payload against a literal
/// written once, and to print one when the comparison fails.
///
/// `Into<i64>` rather than a per-width expected value, so a shared body states its expectation in
/// one type and the widths differ only where they are *meant* to.
trait TestInt: MaterialisedInt + Copy + core::fmt::Debug + Into<i64> {}
impl<I> TestInt for I where I: MaterialisedInt + Copy + core::fmt::Debug + Into<i64> {}

/// The `IntWidth` an `IntOverflow` names, or a description of whatever else came back.
fn overflow_width(errors: &GraphqlErrors<&str>) -> Result<IntWidth, String> {
  match errors[0].data() {
    ErrorData::IntOverflow(overflow) => Ok(overflow.width()),
    other => Err(std::format!("{other:?}")),
  }
}

#[test]
fn int_leaf_materializes_on_both_backings() {
  fn at<I: TestInt>() {
    let width = I::WIDTH;

    let parsed = drive_str(int_value::<_, _, I>, "42").expect("str");
    assert_eq!(Into::<i64>::into(*parsed.source()), 42, "at {width}");
    assert_eq!(parsed.as_span().start(), 0);

    let parsed = drive_slice(int_value::<_, _, I>, b"-42").expect("slice");
    assert_eq!(Into::<i64>::into(*parsed.source()), -42, "at {width}");
  }

  at::<i32>();
  at::<i64>();
}

/// A width's most negative value has a magnitude one past its most positive, so these are the
/// inputs that separate a conversion accumulating in the literal's own sign from one that parses
/// unsigned and negates. `i32` passes for the same reason `i64` does — the digits are read at
/// `i64` in the literal's sign and *then* range-checked down.
#[test]
fn each_widths_extremes_parse() {
  let parsed = drive_str(int_value::<_, _, i64>, "-9223372036854775808").expect("str");
  assert_eq!(*parsed.source(), i64::MIN);
  let parsed = drive_str(int_value::<_, _, i64>, "9223372036854775807").expect("str");
  assert_eq!(*parsed.source(), i64::MAX);

  let parsed = drive_str(int_value::<_, _, i32>, "-2147483648").expect("str");
  assert_eq!(*parsed.source(), i32::MIN);
  let parsed = drive_str(int_value::<_, _, i32>, "2147483647").expect("str");
  assert_eq!(*parsed.source(), i32::MAX);
}

/// [`float_value`] takes no width, so this is asked once. That it *cannot* be asked per width is
/// the property; see the module header for the test that was deleted rather than reworded.
#[test]
fn float_leaf_materializes() {
  let parsed = drive_str(float_value, "1.5e3").expect("str");
  assert_eq!(*parsed.source(), 1500.0_f64);

  let parsed = drive_slice(float_value, b"-0.25").expect("slice");
  assert_eq!(*parsed.source(), -0.25_f64);
}

/// The `try_` form is the design's `ParseAttempt::and_then`: it declines without consuming on a
/// head that is not an integer, and converts when it accepts.
#[test]
fn try_int_declines_without_consuming_and_converts_when_it_accepts() {
  fn at<I: TestInt>() {
    match drive_str(try_int_value::<_, _, I>, "7").expect("accept") {
      ParseAttempt::Accept(node) => {
        assert_eq!(Into::<i64>::into(*node.source()), 7, "at {}", I::WIDTH);
      }
      ParseAttempt::Decline => panic!("declined an integer head at {}", I::WIDTH),
    }

    match drive_str(try_int_value::<_, _, I>, "\"s\"") {
      Ok(ParseAttempt::Decline) => {}
      other => panic!("expected a decline at {}, got {other:?}", I::WIDTH),
    }
  }

  at::<i32>();
  at::<i64>();
}

/// The decision the design records: strings are **not** materialised. This is the test that
/// fails if someone later adds unescaping here.
#[test]
fn strings_keep_their_slice() {
  fn at<I: TestInt>() {
    let parsed = drive_str(value::<_, _, I>, r#""a\nb""#).expect("str");
    match parsed {
      InputValue::String(s) => assert_eq!(*s.source(), r#""a\nb""#),
      other => panic!("expected a string at {}, got {other:?}", I::WIDTH),
    }
  }

  at::<i32>();
  at::<i64>();
}

#[test]
fn nested_containers_materialize_every_leaf() {
  fn at<I: TestInt>() {
    let width = I::WIDTH;

    let parsed = drive_str(list_value::<_, _, I>, "[1, 2.5, [3]]").expect("str");
    let items = parsed.values();
    assert_eq!(items.len(), 3, "at {width}");
    assert!(matches!(&items[0], InputValue::Int(v) if Into::<i64>::into(*v.source()) == 1));
    assert!(matches!(&items[1], InputValue::Float(v) if *v.source() == 2.5));
    match &items[2] {
      InputValue::List(inner) => {
        assert!(
          matches!(&inner.values()[0], InputValue::Int(v) if Into::<i64>::into(*v.source()) == 3)
        );
      }
      other => panic!("expected a nested list at {width}, got {other:?}"),
    }

    let parsed = drive_str(object_value::<_, _, I>, "{a: 1, b: {c: -2}}").expect("str");
    let fields = parsed.fields();
    assert_eq!(fields.len(), 2, "at {width}");
    assert!(matches!(fields[0].value(), InputValue::Int(v) if Into::<i64>::into(*v.source()) == 1));
  }

  at::<i32>();
  at::<i64>();
}

#[test]
fn const_values_materialize_too() {
  fn at<I: TestInt>() {
    let width = I::WIDTH;
    let parsed = drive_str(const_value::<_, _, I>, "{n: 5, f: 0.5}").expect("str");
    match parsed {
      ConstInputValue::Object(object) => {
        assert!(
          matches!(object.fields()[0].value(), ConstInputValue::Int(v) if Into::<i64>::into(*v.source()) == 5)
        );
        assert!(
          matches!(object.fields()[1].value(), ConstInputValue::Float(v) if *v.source() == 0.5)
        );
      }
      other => panic!("expected an object at {width}, got {other:?}"),
    }
  }

  at::<i32>();
  at::<i64>();
}

/// **The round trip the second width exists for**, both directions, on one literal each.
///
/// `2147483648` is `i32::MAX + 1`: a well-formed `IntValue` under draft §2.9.1 and not a value
/// draft §3.5.1's `Int` can hold. It must be a refusal at `i32` and a value at `i64` — and if that
/// ever inverts, the two widths have swapped meanings, which is the mistake this whole layer is
/// easy to make.
///
/// `9223372036854775808` is `i64::MAX + 1`: out of range at both widths, and *the width it names
/// is different at each*. Without the width on the error, this literal and the one above are the
/// same report, and a consumer cannot tell "outside the specification" from "outside anything this
/// crate reads".
///
/// **This is what `I::WIDTH` buys, so this is what fails when it is wrong.** The width on each
/// refusal below is read off the payload type by `Materialized<I>` and by nothing else; a plant
/// that makes one impl answer the other's constant moves exactly the rows for that width.
#[test]
fn the_two_widths_disagree_on_the_literal_between_them() {
  const PAST_I32: &str = "2147483648";
  const PAST_I64: &str = "9223372036854775808";

  // i32::MAX + 1 — refused at the specified width, naming 32.
  let errors =
    drive_str(int_value::<_, _, i32>, PAST_I32).expect_err("i32::MAX + 1 must not fit i32");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I32));
  assert_eq!(errors[0].span().start(), 0);
  assert_eq!(errors[0].span().end(), PAST_I32.len());

  // …and a value at the permissive width.
  let parsed = drive_str(int_value::<_, _, i64>, PAST_I32).expect("i32::MAX + 1 fits i64");
  assert_eq!(*parsed.source(), 2_147_483_648_i64);

  // i64::MAX + 1 — refused at both, and each names its own width.
  let errors =
    drive_str(int_value::<_, _, i32>, PAST_I64).expect_err("i64::MAX + 1 must not fit i32");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I32));

  let errors =
    drive_str(int_value::<_, _, i64>, PAST_I64).expect_err("i64::MAX + 1 must not fit i64");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I64));
}

/// A leading-zeroed literal never reaches a conversion at either width, so no width is a fact
/// about it — the other half of the R3 finding, held end to end rather than at the predicate.
///
/// `IntOverflow::checked("02147483648", I32)` used to answer `Ok`, and this is the claim that made
/// it wrong: the lexer refuses the spelling before either production sees an `Int` leaf, so the
/// `IntOverflow` a caller could mint described a refusal that never happened. The assertion is
/// therefore about *which* refusal, not that there was one — `overflow_width` returns `Err` with
/// the report's own text whenever the failure is not an integer overflow.
///
/// `007` is the row that separates shape from range: it is `7`, a value at both widths, so a
/// production that reported an overflow for it could not be blamed on the range check.
#[test]
fn a_leading_zeroed_literal_is_refused_before_any_conversion_at_either_width() {
  for literal in ["007", "02147483648", "09223372036854775808"] {
    let at_i32 = drive_str(int_value::<_, _, i32>, literal).map(|_| ());
    let at_i64 = drive_str(int_value::<_, _, i64>, literal).map(|_| ());

    for (width, outcome) in [(IntWidth::I32, at_i32), (IntWidth::I64, at_i64)] {
      let errors = outcome.expect_err("the lexer refuses a leading zero");
      assert!(
        overflow_width(&errors).is_err(),
        "{literal:?} at {width}: the production reported an integer overflow for a spelling the \
         lexer never turns into an `Int`",
      );

      // …and the public door now says the same thing about the same bytes.
      assert_eq!(IntOverflow::checked(literal, width), Err(literal));
    }
  }
}

/// **The public checked door and the productions agree, literal for literal.**
///
/// `IntOverflow::checked` is the only way a caller outside this crate can name an [`IntWidth`],
/// and its promise is a claim about *these* productions: it accepts a `(literal, width)` pair
/// exactly when the production at that width would have refused the literal. A promise about
/// another function's behaviour is worth what a test comparing them is worth, and this module is
/// where both widths are already in view.
///
/// It is deliberately asked at both widths over one corpus rather than at the boundary the round
/// was about. A door that answered `I32` correctly and `I64` by always refusing would satisfy the
/// single interesting literal and fail here.
#[test]
fn the_checked_constructor_admits_exactly_what_the_productions_refuse() {
  let (mut refused_somewhere, mut accepted_somewhere) = (0usize, 0usize);

  for literal in [
    "0",
    "-0",
    "7",
    "2147483647",
    "2147483648",
    "-2147483648",
    "-2147483649",
    "9223372036854775807",
    "9223372036854775808",
    "-9223372036854775808",
    "-9223372036854775809",
    "99999999999999999999999999",
  ] {
    for (width, refused) in [
      (
        IntWidth::I32,
        drive_str(int_value::<_, _, i32>, literal).is_err(),
      ),
      (
        IntWidth::I64,
        drive_str(int_value::<_, _, i64>, literal).is_err(),
      ),
    ] {
      assert_eq!(
        IntOverflow::checked(literal, width).is_ok(),
        refused,
        "{literal:?}: the production at {width} refused={refused}, the checked door disagreed",
      );
      if refused {
        refused_somewhere += 1;
      } else {
        accepted_somewhere += 1;
      }
    }
  }

  // Non-vacuity: both verdicts occur, so the equality above is not two constants agreeing.
  assert!(refused_somewhere >= 6, "only {refused_somewhere} refusals");
  assert!(accepted_somewhere >= 6, "only {accepted_somewhere} values");
}

/// The accepted bound, asserted where the source documents it: a literal the specification calls
/// syntactically valid becomes a **parse** error in this view, and it names the spelling, the
/// span and the width so a caller can report it like any other.
#[test]
fn out_of_range_integer_is_a_parse_error_naming_the_literal_and_the_width() {
  fn at<I: TestInt>(literal: &'static str, bits: u32) {
    let errors = drive_str(int_value::<_, _, I>, literal).expect_err("must reject");
    let error = &errors[0];
    match error.data() {
      ErrorData::IntOverflow(overflow) => {
        assert_eq!(*overflow.value(), literal);
        assert_eq!(overflow.width(), I::WIDTH);
        assert_eq!(overflow.width().bits(), bits);
      }
      other => panic!("expected IntOverflow at {}, got {other:?}", I::WIDTH),
    }
    assert_eq!(error.span().start(), 0);
    assert_eq!(error.span().end(), literal.len());
  }

  at::<i32>("2147483648", 32);
  at::<i64>("99999999999999999999999999", 64);
}

#[test]
fn out_of_range_float_is_a_parse_error_naming_the_literal() {
  let errors = drive_str(float_value, "1e400").expect_err("must reject");
  assert!(
    matches!(errors[0].data(), ErrorData::FloatOverflow("1e400")),
    "expected FloatOverflow, got {:?}",
    errors[0].data()
  );
}

/// The same literal inside a container still reports, so the conversion is not skipped on the
/// fused dispatch path that `value` takes for a head token.
#[test]
fn out_of_range_reports_from_inside_a_container() {
  let errors = drive_str(value::<_, _, i32>, "[1, 2147483648]").expect_err("must reject");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I32));

  let errors =
    drive_str(value::<_, _, i64>, "[1, 99999999999999999999999999]").expect_err("must reject");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I64));

  let errors = drive_str(const_value::<_, _, i32>, "{a: 1e400}").expect_err("must reject");
  assert!(
    matches!(errors[0].data(), ErrorData::FloatOverflow(_)),
    "expected FloatOverflow, got {:?}",
    errors[0].data()
  );
}

/// The slice parser and this one are the same parser at two payloads, so a failure that is not a
/// conversion must be reported identically. If the materialising module ever grows its own copy
/// of a composite, this is what notices the copy diverging.
#[test]
fn non_numeric_failures_are_identical_to_the_slice_parser() {
  fn at<I: TestInt>() {
    for source in ["[1,", "{a:}", "{a 1}", "$", "["] {
      let mine = drive_str(value::<_, _, I>, source)
        .map(|_| ())
        .map_err(|e| std::format!("{:?}", e[0].data()));
      let theirs = drive_str(super::super::value, source)
        .map(|_| ())
        .map_err(|e| std::format!("{:?}", e[0].data()));
      assert_eq!(mine, theirs, "diverged on {source:?} at {}", I::WIDTH);
    }
  }

  at::<i32>();
  at::<i64>();
}
