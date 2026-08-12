//! Materialised-value production tests.
//!
//! Driven end to end over the real GraphQL syntactic lexer under a `Fatal<GraphqlErrors>`
//! context, the same way the slice suite next door drives its productions, and over both a `str`
//! and a `[u8]` source so the `AsRef<[u8]>` path is exercised on a non-UTF-8-typed backing too.

use tokora::{FatalContext, Parse, Parser, span::AsSpan};

use super::{const_value, float_value, int_value, list_value, object_value, try_int_value, value};
use crate::graphql::{
  GraphQL,
  ast::materialized::{ConstInputValue, InputValue},
  error::{ErrorData, GraphqlErrors},
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

#[test]
fn int_leaf_materializes_on_both_backings() {
  let parsed = drive_str(int_value, "42").expect("str");
  assert_eq!(*parsed.source(), 42_i64);
  assert_eq!(parsed.as_span().start(), 0);

  let parsed = drive_slice(int_value, b"-42").expect("slice");
  assert_eq!(*parsed.source(), -42_i64);
}

/// `i64::MIN`'s magnitude is one past `i64::MAX`, so this input is what separates a conversion
/// that accumulates in the literal's sign from one that parses unsigned and negates.
#[test]
fn int_min_parses() {
  let parsed = drive_str(int_value, "-9223372036854775808").expect("str");
  assert_eq!(*parsed.source(), i64::MIN);
}

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
  match drive_str(try_int_value, "7").expect("accept") {
    ParseAttempt::Accept(node) => assert_eq!(*node.source(), 7_i64),
    ParseAttempt::Decline => panic!("declined an integer head"),
  }

  match drive_str(try_int_value, "\"s\"") {
    Ok(ParseAttempt::Decline) => {}
    other => panic!("expected a decline, got {other:?}"),
  }
}

/// The decision the design records: strings are **not** materialised. This is the test that
/// fails if someone later adds unescaping here.
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
  assert!(matches!(&items[0], InputValue::Int(v) if *v.source() == 1));
  assert!(matches!(&items[1], InputValue::Float(v) if *v.source() == 2.5));
  match &items[2] {
    InputValue::List(inner) => {
      assert!(matches!(&inner.values()[0], InputValue::Int(v) if *v.source() == 3));
    }
    other => panic!("expected a nested list, got {other:?}"),
  }

  let parsed = drive_str(object_value, "{a: 1, b: {c: -2}}").expect("str");
  let fields = parsed.fields();
  assert_eq!(fields.len(), 2);
  assert!(matches!(fields[0].value(), InputValue::Int(v) if *v.source() == 1));
}

#[test]
fn const_values_materialize_too() {
  let parsed = drive_str(const_value, "{n: 5, f: 0.5}").expect("str");
  match parsed {
    ConstInputValue::Object(object) => {
      assert!(matches!(object.fields()[0].value(), ConstInputValue::Int(v) if *v.source() == 5));
      assert!(
        matches!(object.fields()[1].value(), ConstInputValue::Float(v) if *v.source() == 0.5)
      );
    }
    other => panic!("expected an object, got {other:?}"),
  }
}

/// The accepted bound, asserted where the source documents it: a literal the specification calls
/// syntactically valid becomes a **parse** error in this view, and it names the spelling and the
/// span so a caller can report it like any other.
#[test]
fn out_of_range_integer_is_a_parse_error_naming_the_literal() {
  let errors = drive_str(int_value, "99999999999999999999999999").expect_err("must reject");
  let error = &errors[0];
  assert!(
    matches!(
      error.data(),
      ErrorData::IntOverflow("99999999999999999999999999")
    ),
    "expected IntOverflow, got {:?}",
    error.data()
  );
  assert_eq!(error.span().start(), 0);
  assert_eq!(error.span().end(), 26);
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
  let errors = drive_str(value, "[1, 99999999999999999999999999]").expect_err("must reject");
  assert!(
    matches!(errors[0].data(), ErrorData::IntOverflow(_)),
    "expected IntOverflow, got {:?}",
    errors[0].data()
  );

  let errors = drive_str(const_value, "{a: 1e400}").expect_err("must reject");
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
  for source in ["[1,", "{a:}", "{a 1}", "$", "["] {
    let mine = drive_str(value, source)
      .map(|_| ())
      .map_err(|e| format!("{:?}", e[0].data()));
    let theirs = drive_str(super::super::value, source)
      .map(|_| ())
      .map_err(|e| format!("{:?}", e[0].data()));
    assert_eq!(mine, theirs, "diverged on {source:?}");
  }
}
