//! Spec-width materialised-value production tests.
//!
//! Driven the way the `i64` suite next door drives its own — over the real GraphQL syntactic
//! lexer under a `Fatal<GraphqlErrors>` context, over both a `str` and a `[u8]` source so the
//! `AsRef<[u8]>` path is exercised on a non-UTF-8-typed backing too.
//!
//! The tests that matter here are the ones that need **both** widths in view. A suite that only
//! read this module could pass with the two widths silently identical, which is the one failure a
//! second width has that the first did not.

use tokora::{FatalContext, Parse, Parser, span::AsSpan};

use super::{const_value, float_value, int_value, list_value, object_value, try_int_value, value};
use crate::graphql::{
  GraphQL,
  ast::materialized32::{ConstInputValue, InputValue},
  error::{ErrorData, GraphqlErrors, IntOverflow, IntWidth},
  syntactic::{GraphqlInput, GraphqlLexer, value::materialized},
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

/// The `IntWidth` an `IntOverflow` names, or a description of whatever else came back.
fn overflow_width(errors: &GraphqlErrors<&str>) -> Result<IntWidth, String> {
  match errors[0].data() {
    ErrorData::IntOverflow(overflow) => Ok(overflow.width()),
    other => Err(std::format!("{other:?}")),
  }
}

#[test]
fn int_leaf_materializes_at_the_specified_width_on_both_backings() {
  let parsed = drive_str(int_value, "42").expect("str");
  assert_eq!(*parsed.source(), 42_i32);
  assert_eq!(parsed.as_span().start(), 0);

  let parsed = drive_slice(int_value, b"-42").expect("slice");
  assert_eq!(*parsed.source(), -42_i32);
}

/// `i32::MIN`'s magnitude is one past `i32::MAX`, so this is the input that separates a
/// conversion accumulating in the literal's sign from one that parses unsigned and negates. It is
/// the `i64` suite's `int_min_parses` at this width, and it passes for the same reason: the
/// digits are read at `i64` in the literal's own sign and *then* range-checked into `i32`.
#[test]
fn int_min_parses() {
  let parsed = drive_str(int_value, "-2147483648").expect("str");
  assert_eq!(*parsed.source(), i32::MIN);

  let parsed = drive_str(int_value, "2147483647").expect("str");
  assert_eq!(*parsed.source(), i32::MAX);
}

#[test]
fn float_leaf_materializes() {
  let parsed = drive_str(float_value, "1.5e3").expect("str");
  assert_eq!(*parsed.source(), 1500.0_f64);

  let parsed = drive_slice(float_value, b"-0.25").expect("slice");
  assert_eq!(*parsed.source(), -0.25_f64);
}

#[test]
fn try_int_declines_without_consuming_and_converts_when_it_accepts() {
  match drive_str(try_int_value, "7").expect("accept") {
    ParseAttempt::Accept(node) => assert_eq!(*node.source(), 7_i32),
    ParseAttempt::Decline => panic!("declined an integer head"),
  }

  match drive_str(try_int_value, "\"s\"") {
    Ok(ParseAttempt::Decline) => {}
    other => panic!("expected a decline, got {other:?}"),
  }
}

#[test]
fn strings_keep_their_slice() {
  let parsed = drive_str(value, r#""a\nb""#).expect("str");
  match parsed {
    InputValue::String(s) => assert_eq!(*s.source(), r#""a\nb""#),
    other => panic!("expected a string, got {other:?}"),
  }
}

#[test]
fn nested_containers_materialize_every_leaf() {
  let parsed = drive_str(list_value, "[1, 2.5, [3]]").expect("str");
  let items = parsed.values();
  assert_eq!(items.len(), 3);
  assert!(matches!(&items[0], InputValue::Int(v) if *v.source() == 1_i32));
  assert!(matches!(&items[1], InputValue::Float(v) if *v.source() == 2.5));
  match &items[2] {
    InputValue::List(inner) => {
      assert!(matches!(&inner.values()[0], InputValue::Int(v) if *v.source() == 3_i32));
    }
    other => panic!("expected a nested list, got {other:?}"),
  }

  let parsed = drive_str(object_value, "{a: 1, b: {c: -2}}").expect("str");
  let fields = parsed.fields();
  assert_eq!(fields.len(), 2);
  assert!(matches!(fields[0].value(), InputValue::Int(v) if *v.source() == 1_i32));
}

#[test]
fn const_values_materialize_too() {
  let parsed = drive_str(const_value, "{n: 5, f: 0.5}").expect("str");
  match parsed {
    ConstInputValue::Object(object) => {
      assert!(
        matches!(object.fields()[0].value(), ConstInputValue::Int(v) if *v.source() == 5_i32)
      );
      assert!(
        matches!(object.fields()[1].value(), ConstInputValue::Float(v) if *v.source() == 0.5)
      );
    }
    other => panic!("expected an object, got {other:?}"),
  }
}

/// **The round trip the second width exists for**, both directions, on one literal each.
///
/// `2147483648` is `i32::MAX + 1`: a well-formed `IntValue` under draft §2.9.1 and not a value
/// draft §3.5.1's `Int` can hold. It must be a refusal here and a value next door — and if that
/// ever inverts, the two modules have swapped meanings, which is the mistake this whole layer is
/// easy to make.
///
/// `9223372036854775808` is `i64::MAX + 1`: out of range at both widths, and *the width it names
/// is different at each*. Without the width on the error, this literal and the one above are the
/// same report from this module, and a consumer cannot tell "outside the specification" from
/// "outside anything this crate reads".
#[test]
fn the_two_widths_disagree_on_the_literal_between_them() {
  const PAST_I32: &str = "2147483648";
  const PAST_I64: &str = "9223372036854775808";

  // i32::MAX + 1 — refused here, naming 32.
  let errors = drive_str(int_value, PAST_I32).expect_err("i32::MAX + 1 must not fit i32");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I32));
  assert_eq!(errors[0].span().start(), 0);
  assert_eq!(errors[0].span().end(), PAST_I32.len());

  // …and a value at the permissive width.
  let parsed = drive_str(materialized::int_value, PAST_I32).expect("i32::MAX + 1 fits i64");
  assert_eq!(*parsed.source(), 2_147_483_648_i64);

  // i64::MAX + 1 — refused at both, and each names its own width.
  let errors = drive_str(int_value, PAST_I64).expect_err("i64::MAX + 1 must not fit i32");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I32));

  let errors =
    drive_str(materialized::int_value, PAST_I64).expect_err("i64::MAX + 1 must not fit i64");
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
    let at_i32 = drive_str(int_value, literal).map(|_| ());
    let at_i64 = drive_str(materialized::int_value, literal).map(|_| ());

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

/// **The public checked door and the two productions agree, literal for literal.**
///
/// `IntOverflow::checked` is the only way a caller outside this crate can name an [`IntWidth`],
/// and its promise is a claim about *these* productions: it accepts a `(literal, width)` pair
/// exactly when the production at that width would have refused the literal. A promise about
/// another function's behaviour is worth what a test comparing them is worth, and this module is
/// where both productions are already in view.
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
      (IntWidth::I32, drive_str(int_value, literal).is_err()),
      (
        IntWidth::I64,
        drive_str(materialized::int_value, literal).is_err(),
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

/// The literal's spelling survives beside the width — a report needs both, and the width alone
/// would not let a consumer quote the document.
#[test]
fn out_of_range_integer_names_the_literal_as_well_as_the_width() {
  let errors = drive_str(int_value, "2147483648").expect_err("must reject");
  match errors[0].data() {
    ErrorData::IntOverflow(overflow) => {
      assert_eq!(*overflow.value(), "2147483648");
      assert_eq!(overflow.width(), IntWidth::I32);
      assert_eq!(overflow.width().bits(), 32);
    }
    other => panic!("expected IntOverflow, got {other:?}"),
  }
}

/// `Float` is `f64` at both widths, so a float refusal must be *indistinguishable* between the
/// two modules. A `FloatOverflow` that started naming a width would be naming a distinction that
/// does not exist.
#[test]
fn out_of_range_float_is_the_same_error_at_both_widths() {
  let mine = drive_str(float_value, "1e400").expect_err("must reject");
  let theirs = drive_str(materialized::float_value, "1e400").expect_err("must reject");

  assert!(
    matches!(mine[0].data(), ErrorData::FloatOverflow("1e400")),
    "expected FloatOverflow, got {:?}",
    mine[0].data()
  );
  assert_eq!(
    std::format!("{:?}", mine[0].data()),
    std::format!("{:?}", theirs[0].data()),
  );
}

/// The same literal inside a container still reports, so the conversion is not skipped on the
/// fused dispatch path that `value` takes for a head token.
#[test]
fn out_of_range_reports_from_inside_a_container() {
  let errors = drive_str(value, "[1, 2147483648]").expect_err("must reject");
  assert_eq!(overflow_width(&errors), Ok(IntWidth::I32));

  let errors = drive_str(const_value, "{a: 1e400}").expect_err("must reject");
  assert!(
    matches!(errors[0].data(), ErrorData::FloatOverflow(_)),
    "expected FloatOverflow, got {:?}",
    errors[0].data()
  );
}

/// The slice parser and this one are the same parser at two payloads, so a failure that is not a
/// conversion must be reported identically. If this module ever grows its own copy of a
/// composite, this is what notices the copy diverging.
#[test]
fn non_numeric_failures_are_identical_to_the_slice_parser() {
  for source in ["[1,", "{a:}", "{a 1}", "$", "["] {
    let mine = drive_str(value, source)
      .map(|_| ())
      .map_err(|e| std::format!("{:?}", e[0].data()));
    let theirs = drive_str(super::super::value, source)
      .map(|_| ())
      .map_err(|e| std::format!("{:?}", e[0].data()));
    assert_eq!(mine, theirs, "diverged on {source:?}");
  }
}

/// Everything that is not an `Int` must render identically at the two widths, because one leaf
/// type is the only difference between the modules. A `Debug` comparison over a document with no
/// integer in it is the cheapest instrument that would notice a divergence anywhere else — a
/// float rounded differently, a string unescaped on one side, a span shifted.
///
/// **One substitution is permitted and it is named.** The shared container carriers hold a
/// `PhantomData` over the value type, so `Debug` prints each tree's own module path inside a
/// document that contains no number at all. `at_the_other_width` rewrites exactly that path and
/// nothing else; a difference anywhere but there survives it.
///
/// The altered document is the control, and it is needed because the comparison is an *equality*:
/// a `Debug` that had stopped printing payloads would satisfy it trivially. It differs by one
/// float, and is deliberately not an integer — an `i32` `7` and an `i64` `7` render as the same
/// character, so a rendering cannot see the width at all. That is why the round trip above reads
/// the `IntWidth` off the error rather than off a `Debug` string.
#[test]
fn only_the_int_leaf_differs_between_the_two_widths() {
  const NO_INT: &str = r#"{s: "a\nb", f: [1.5, -2.25e3], b: true, n: null, e: SOME_ENUM}"#;
  const NO_INT_ALTERED: &str = r#"{s: "a\nb", f: [1.5, -2.25e4], b: true, n: null, e: SOME_ENUM}"#;

  fn at_the_other_width(rendered: &str) -> std::string::String {
    rendered.replace("ast::materialized::", "ast::materialized32::")
  }

  let mine = std::format!("{:?}", drive_str(value, NO_INT).expect("i32 tree"));
  let theirs = std::format!(
    "{:?}",
    drive_str(materialized::value, NO_INT).expect("i64 tree")
  );
  assert_eq!(
    mine,
    at_the_other_width(&theirs),
    "the two widths diverged on a document with no integer in it, somewhere other than the tree's \
     own type path",
  );

  let altered = std::format!("{:?}", drive_str(value, NO_INT_ALTERED).expect("i32 tree"));
  assert_ne!(
    mine, altered,
    "two documents differing in a float rendered identically — the comparison above is blind and \
     proves nothing",
  );
}
