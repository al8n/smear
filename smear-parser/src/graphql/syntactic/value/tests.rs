//! Value-production tests.
//!
//! Every production is driven end to end over the real GraphQL syntactic lexer under
//! a `Fatal<GraphqlErrors>` context. Each accept case runs the full source matrix
//! (`str`, `[u8]`, and `Bytes` behind the feature), asserting equal ASTs modulo the
//! slice type; reject cases assert the error family; and a table-driven oracle pins
//! the accept/reject verdicts the frozen `smear-parser` crate produces for the same
//! inputs (the true/false/null-before-enum ordering included).
//!
//! The productions are fixed to the concrete GraphQL syntactic lexer and `GraphQL`
//! marker, so the drivers use `Parser::with_parser` with that marker explicitly.

use smear_lexer::graphql::syntactic::SyntacticTokenKind;
use tokora::{FatalContext, Lexer, Parse, Parser, SimpleSpan, Source, utils::cmp::Equivalent};

use super::{const_object_field, default_value, object_field, try_default_value};
use crate::graphql::{
  GraphQL,
  ast::{
    BooleanValue, BooleanValue as BooleanValueParser, ConstInputValue,
    ConstInputValue as ConstInputValueParser, DefaultInputValue, EnumValue as EnumValueParser,
    FloatValue as FloatValueParser, InputValue, InputValue as InputValueParser, IntValue,
    IntValue as IntValueParser, NullValue as NullValueParser, StringValue as StringValueParser,
    VariableValue as VariableValueParser,
  },
  error::{ErrorData, Expectation, GraphqlErrors, ObjectFieldValueHint, Unclosed},
  syntactic::{GraphqlError, GraphqlInput, GraphqlLexer, GraphqlToken},
};
use tokora::try_parse_input::ParseAttempt;

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, GraphqlLexer<'inp, str>, GraphqlErrors<&'inp str>, GraphQL>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> =
  FatalContext<'inp, GraphqlLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>, GraphQL>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, str, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(f)
    .parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, [u8]>, O, GraphqlErrors<&'inp [u8]>, _, GraphQL>(f)
    .parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, [u8], SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, [u8]>, O, GraphqlErrors<&'inp [u8]>, _, GraphQL>(f)
    .parse_bytes(input)
}

/// Runs `parser` over `src` as `str`, `[u8]`, and (behind the feature) `Bytes`,
/// applying the generic `check` fn to each accepted AST — the source matrix.
macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

/// Asserts `parser` rejects `src` over both `str` and `[u8]`.
macro_rules! reject_all {
  ($parser:expr, $src:expr) => {{
    assert!(
      drive_str(|inp| $parser(inp).map(|_| ()), $src).is_err(),
      "str should reject: {:?}",
      $src
    );
    assert!(
      drive_slice(|inp| $parser(inp).map(|_| ()), $src.as_bytes()).is_err(),
      "slice should reject: {:?}",
      $src
    );
  }};
}

fn assert_unclosed_list<S>(result: Result<(), GraphqlErrors<S>>) {
  let error = result
    .expect_err("unterminated list should fail")
    .into_iter()
    .next()
    .expect("unterminated list should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::List)
  ));
}

fn assert_unclosed_object<S>(result: Result<(), GraphqlErrors<S>>) {
  let error = result
    .expect_err("unterminated object should fail")
    .into_iter()
    .next()
    .expect("unterminated object should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::Unclosed(Unclosed::Object)
  ));
}

// ─── Leaf builders (driven standalone) ───────────────────────────────────────

#[test]
fn int_value_accepts() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::IntValue<S>) {
    assert!("42".equivalent(v.source()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 2));
  }
  accept_all!(IntValueParser::graphql, "42", check);
}

#[test]
fn int_value_rejects_non_int() {
  reject_all!(IntValueParser::graphql, "\"s\"");
  reject_all!(IntValueParser::graphql, "");
}

#[test]
fn int_value_graphql_does_not_require_equivalent() {
  // `NumericSlice` intentionally has no `Equivalent<str>` bound. This generic
  // witness type-checks only while the direct integer API stays independent of
  // identifier spelling classification.
  #[allow(dead_code)]
  fn parse_numeric_slice<'inp, Src, NumericSlice, Ctx>(
    inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  ) -> Result<IntValue<NumericSlice>, GraphqlError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = NumericSlice> + ?Sized,
    NumericSlice: tokora::Slice<'inp> + Clone + 'inp,
    GraphqlLexer<'inp, Src>:
      Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: crate::combinator::ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  {
    IntValueParser::graphql(inp)
  }
}

#[test]
fn float_value_accepts() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::FloatValue<S>) {
    assert!("3.14".equivalent(v.source()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(FloatValueParser::graphql, "3.14", check);
}

#[test]
fn float_value_rejects_int() {
  reject_all!(FloatValueParser::graphql, "42");
}

#[test]
fn string_value_accepts_inline() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::StringValue<S>) {
    assert!("\"hi\"".equivalent(v.source()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(StringValueParser::graphql, "\"hi\"", check);
}

#[test]
fn string_value_accepts_block() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::StringValue<S>) {
    assert!("\"\"\"hi\"\"\"".equivalent(v.source()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 8));
  }
  accept_all!(StringValueParser::graphql, "\"\"\"hi\"\"\"", check);
}

#[test]
fn string_value_rejects_int() {
  reject_all!(StringValueParser::graphql, "42");
}

// ─── Variable ────────────────────────────────────────────────────────────────

#[test]
fn variable_value_accepts() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::VariableValue<S>) {
    assert!("userId".equivalent(v.name().source()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 7));
  }
  accept_all!(VariableValueParser::graphql, "$userId", check);
}

#[test]
fn variable_value_rejects_missing_name() {
  reject_all!(VariableValueParser::graphql, "$");
}

#[test]
fn variable_value_rejects_without_dollar() {
  reject_all!(VariableValueParser::graphql, "userId");
}

#[test]
fn try_variable_value_accepts_and_declines() {
  // Accept on `$name`.
  let accepted = drive_str(VariableValueParser::try_graphql, "$x").unwrap();
  assert!(matches!(accepted, ParseAttempt::Accept(_)));
  // Decline (nothing consumed) on a non-`$` token.
  let declined = drive_str(
    |inp| {
      let attempt = VariableValueParser::try_graphql(inp)?;
      // The declined identifier is untouched, so a plain value pulls it back.
      let leftover = InputValueParser::graphql(inp)?;
      Ok::<_, GraphqlErrors<&str>>((attempt.is_decline(), leftover.is_enum()))
    },
    "x",
  )
  .unwrap();
  assert_eq!(declined, (true, true));
}

// ─── value dispatcher: one row per alternative ───────────────────────────────

#[test]
fn value_int_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let i = v.unwrap_int_ref();
    assert!("42".equivalent(i.source()));
    assert_eq!(*i.span(), SimpleSpan::new(0, 2));
  }
  accept_all!(InputValueParser::graphql, "42", check);
}

#[test]
fn value_float_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!("1.5e3".equivalent(v.unwrap_float_ref().source()));
  }
  accept_all!(InputValueParser::graphql, "1.5e3", check);
}

#[test]
fn value_string_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!("\"hi\"".equivalent(v.unwrap_string_ref().source()));
  }
  accept_all!(InputValueParser::graphql, "\"hi\"", check);
}

#[test]
fn value_block_string_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_string());
  }
  accept_all!(InputValueParser::graphql, "\"\"\"block\"\"\"", check);
}

#[test]
fn value_true_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let b = v.unwrap_boolean_ref();
    assert!(b.value());
    assert_eq!(*b.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(InputValueParser::graphql, "true", check);
}

#[test]
fn value_false_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(!v.unwrap_boolean_ref().value());
  }
  accept_all!(InputValueParser::graphql, "false", check);
}

#[test]
fn value_null_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let n = v.unwrap_null_ref();
    assert!("null".equivalent(n.source()));
    assert_eq!(*n.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(InputValueParser::graphql, "null", check);
}

#[test]
fn value_enum_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let e = v.unwrap_enum_ref();
    assert!("ACTIVE".equivalent(e.source()));
    assert_eq!(*e.span(), SimpleSpan::new(0, 6));
  }
  accept_all!(InputValueParser::graphql, "ACTIVE", check);
}

#[test]
fn value_enum_arm_accepts_soft_keywords() {
  // `enum`/`type` are soft keywords: only true/false/null are excluded from enum.
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_enum());
  }
  accept_all!(InputValueParser::graphql, "enum", check);
  accept_all!(InputValueParser::graphql, "type", check);
}

#[test]
fn value_variable_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!("x".equivalent(v.unwrap_variable_ref().name().source()));
  }
  accept_all!(InputValueParser::graphql, "$x", check);
}

#[test]
fn value_list_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let list = v.unwrap_list_ref();
    assert_eq!(list.values().len(), 2);
    assert_eq!(*list.span(), SimpleSpan::new(0, 6));
    assert!(list.values()[0].is_int());
    assert!(list.values()[1].is_int());
  }
  accept_all!(InputValueParser::graphql, "[1, 2]", check);
}

#[test]
fn value_empty_list_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.unwrap_list_ref().values().is_empty());
  }
  accept_all!(InputValueParser::graphql, "[]", check);
}

#[test]
fn value_object_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let obj = v.unwrap_object_ref();
    assert_eq!(obj.fields().len(), 1);
    let field = &obj.fields()[0];
    assert!("a".equivalent(field.name().source()));
    assert!(field.value().is_int());
  }
  accept_all!(InputValueParser::graphql, "{ a: 1 }", check);
}

#[test]
fn value_empty_object_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.unwrap_object_ref().fields().is_empty());
  }
  accept_all!(InputValueParser::graphql, "{}", check);
}

#[test]
fn value_nested_list_and_object() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let outer = v.unwrap_list_ref();
    assert_eq!(outer.values().len(), 2);
    // [ [1], { k: $v } ]
    assert_eq!(outer.values()[0].unwrap_list_ref().values().len(), 1);
    let obj = outer.values()[1].unwrap_object_ref();
    assert!(obj.fields()[0].value().is_variable());
  }
  accept_all!(InputValueParser::graphql, "[[1], { k: $v }]", check);
}

// ─── value: enum exclusion ordering (Ruling 2) ───────────────────────────────

#[test]
fn value_resolves_true_false_null_before_enum() {
  // The three reserved spellings never fall through to the enum arm.
  assert!(
    drive_str(InputValueParser::graphql, "true")
      .unwrap()
      .is_boolean()
  );
  assert!(
    drive_str(InputValueParser::graphql, "false")
      .unwrap()
      .is_boolean()
  );
  assert!(
    drive_str(InputValueParser::graphql, "null")
      .unwrap()
      .is_null()
  );
  // Anything else on the identifier arm is an enum.
  assert!(
    drive_str(InputValueParser::graphql, "trueish")
      .unwrap()
      .is_enum()
  );
  assert!(
    drive_str(InputValueParser::graphql, "NULLABLE")
      .unwrap()
      .is_enum()
  );
}

// ─── value: reject rows + error families ─────────────────────────────────────

#[test]
fn value_rejects_non_value_heads() {
  reject_all!(InputValueParser::graphql, "}");
  reject_all!(InputValueParser::graphql, ")");
  reject_all!(InputValueParser::graphql, "]");
  reject_all!(InputValueParser::graphql, ":");
  reject_all!(InputValueParser::graphql, "@");
  reject_all!(InputValueParser::graphql, "=");
}

#[test]
fn value_invalid_head_expects_input_value() {
  for (src, kind) in [
    ("}", SyntacticTokenKind::RBrace),
    ("@", SyntacticTokenKind::At),
  ] {
    let error = drive_str(|inp| InputValueParser::graphql(inp).map(|_| ()), src)
      .expect_err("a non-value head should fail")
      .into_iter()
      .next()
      .expect("a non-value head should emit an error");
    assert!(matches!(
      error.into_data(),
      ErrorData::UnexpectedToken(unexpected)
        if unexpected.expected() == &Expectation::InputValue
          && unexpected.found() == Some(&kind)
    ));
  }
}

#[test]
fn value_end_of_input_error_family() {
  let is_eot = match drive_str(|inp| InputValueParser::graphql(inp).map(|_| ()), "") {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_end_of_input()),
    Ok(()) => false,
  };
  assert!(is_eot);
}

#[test]
fn value_unterminated_list_is_unclosed_list() {
  assert_unclosed_list(drive_str(
    |inp| InputValueParser::graphql(inp).map(|_| ()),
    "[1, 2",
  ));
  assert_unclosed_list(drive_slice(
    |inp| InputValueParser::graphql(inp).map(|_| ()),
    b"[1, 2",
  ));
}

#[test]
fn value_unterminated_object_is_unclosed_object() {
  assert_unclosed_object(drive_str(
    |inp| InputValueParser::graphql(inp).map(|_| ()),
    "{ a: 1",
  ));
  assert_unclosed_object(drive_slice(
    |inp| InputValueParser::graphql(inp).map(|_| ()),
    b"{ a: 1",
  ));
}

#[test]
fn value_object_field_missing_colon_is_error() {
  reject_all!(InputValueParser::graphql, "{ a 1 }");
}

// ─── const_value ─────────────────────────────────────────────────────────────

#[test]
fn const_value_int_arm() {
  fn check<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!("7".equivalent(v.unwrap_int_ref().source()));
  }
  accept_all!(ConstInputValueParser::graphql, "7", check);
}

#[test]
fn const_value_enum_and_scalars() {
  fn check_enum<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!(v.is_enum());
  }
  accept_all!(ConstInputValueParser::graphql, "ACTIVE", check_enum);
  fn check_bool<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!(v.unwrap_boolean_ref().value());
  }
  accept_all!(ConstInputValueParser::graphql, "true", check_bool);
  fn check_null<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!(v.is_null());
  }
  accept_all!(ConstInputValueParser::graphql, "null", check_null);
}

#[test]
fn const_value_list_and_object() {
  fn check<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    let obj = v.unwrap_object_ref();
    assert_eq!(obj.fields().len(), 1);
    assert!(obj.fields()[0].value().is_list());
  }
  accept_all!(ConstInputValueParser::graphql, "{ xs: [1, 2] }", check);
}

#[test]
fn const_value_rejects_variable() {
  reject_all!(ConstInputValueParser::graphql, "$x");
  let error = drive_str(|inp| ConstInputValueParser::graphql(inp).map(|_| ()), "$x")
    .expect_err("variables are not const input values")
    .into_iter()
    .next()
    .expect("a variable head should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::ConstInputValue
        && unexpected.found() == Some(&SyntacticTokenKind::Dollar)
  ));
}

#[test]
fn const_value_rejects_nested_variable() {
  reject_all!(ConstInputValueParser::graphql, "[$x]");
  reject_all!(ConstInputValueParser::graphql, "{ a: $x }");
}

#[test]
fn const_value_unterminated_list_is_unclosed_list() {
  assert_unclosed_list(drive_str(
    |inp| ConstInputValueParser::graphql(inp).map(|_| ()),
    "[1, 2",
  ));
  assert_unclosed_list(drive_slice(
    |inp| ConstInputValueParser::graphql(inp).map(|_| ()),
    b"[1, 2",
  ));
}

#[test]
fn const_value_unterminated_object_is_unclosed_object() {
  assert_unclosed_object(drive_str(
    |inp| ConstInputValueParser::graphql(inp).map(|_| ()),
    "{ a: 1",
  ));
  assert_unclosed_object(drive_slice(
    |inp| ConstInputValueParser::graphql(inp).map(|_| ()),
    b"{ a: 1",
  ));
}

// ─── object fields ───────────────────────────────────────────────────────────

#[test]
fn object_field_accepts() {
  fn check<S: AsRef<[u8]>>(f: crate::graphql::ast::ObjectField<S>) {
    assert!("name".equivalent(f.name().source()));
    assert!(f.value().is_string());
    assert_eq!(f.span().start(), 0);
  }
  accept_all!(object_field, "name: \"v\"", check);
}

#[test]
fn object_field_name_may_be_reserved_word() {
  // Object field names are `Name`s; `true` is a legal field name (only the enum
  // *value* position excludes it).
  fn check<S: AsRef<[u8]>>(f: crate::graphql::ast::ObjectField<S>) {
    assert!("true".equivalent(f.name().source()));
    assert!(f.value().is_int());
  }
  accept_all!(object_field, "true: 1", check);
}

#[test]
fn const_object_field_accepts_and_rejects_variable() {
  fn check<S: AsRef<[u8]>>(f: crate::graphql::ast::ConstObjectField<S>) {
    assert!("k".equivalent(f.name().source()));
    assert!(f.value().is_int());
  }
  accept_all!(const_object_field, "k: 1", check);
  reject_all!(const_object_field, "k: $v");
}

#[test]
fn object_fields_missing_colon_leave_value_token() {
  let (expects_colon, value_remains) = drive_str(
    |inp| {
      let expects_colon = match object_field(inp) {
        Err(errors) => errors.into_iter().next().is_some_and(|error| {
          matches!(
            error.into_data(),
            ErrorData::UnexpectedToken(unexpected)
              if unexpected.expected() == &Expectation::Colon
                && matches!(unexpected.found(), Some(SyntacticTokenKind::Int))
          )
        }),
        Ok(_) => false,
      };
      let value_remains = matches!(
        inp
          .next()?
          .expect("the token in place of the colon should remain")
          .into_data(),
        GraphqlToken::<'_, str>::LitInt(_)
      );
      Ok::<_, GraphqlErrors<&str>>((expects_colon, value_remains))
    },
    "name 1",
  )
  .unwrap();
  assert!(expects_colon);
  assert!(value_remains);

  let (expects_colon, value_remains) = drive_str(
    |inp| {
      let expects_colon = match const_object_field(inp) {
        Err(errors) => errors.into_iter().next().is_some_and(|error| {
          matches!(
            error.into_data(),
            ErrorData::UnexpectedToken(unexpected)
              if unexpected.expected() == &Expectation::Colon
                && matches!(unexpected.found(), Some(SyntacticTokenKind::Int))
          )
        }),
        Ok(_) => false,
      };
      let value_remains = matches!(
        inp
          .next()?
          .expect("the token in place of the colon should remain")
          .into_data(),
        GraphqlToken::<'_, str>::LitInt(_)
      );
      Ok::<_, GraphqlErrors<&str>>((expects_colon, value_remains))
    },
    "name 1",
  )
  .unwrap();
  assert!(expects_colon);
  assert!(value_remains);
}

#[test]
fn object_fields_eot_after_name_expect_colon() {
  for is_expected in [
    drive_str(
      |inp| {
        Ok::<_, GraphqlErrors<&str>>(match object_field(inp) {
          Err(errors) => errors.into_iter().next().is_some_and(|error| {
            matches!(
              error.into_data(),
              ErrorData::UnexpectedEndOfObjectFieldValue(end)
                if end.hint() == &ObjectFieldValueHint::Colon
            )
          }),
          Ok(_) => false,
        })
      },
      "key ",
    )
    .unwrap(),
    drive_str(
      |inp| {
        Ok::<_, GraphqlErrors<&str>>(match const_object_field(inp) {
          Err(errors) => errors.into_iter().next().is_some_and(|error| {
            matches!(
              error.into_data(),
              ErrorData::UnexpectedEndOfObjectFieldValue(end)
                if end.hint() == &ObjectFieldValueHint::Colon
            )
          }),
          Ok(_) => false,
        })
      },
      "key ",
    )
    .unwrap(),
  ] {
    assert!(is_expected);
  }
}

#[test]
fn object_fields_eot_after_colon_expect_value() {
  for is_expected in [
    drive_str(
      |inp| {
        Ok::<_, GraphqlErrors<&str>>(match object_field(inp) {
          Err(errors) => errors.into_iter().next().is_some_and(|error| {
            matches!(
              error.into_data(),
              ErrorData::UnexpectedEndOfObjectFieldValue(end)
                if end.hint() == &ObjectFieldValueHint::Value
            )
          }),
          Ok(_) => false,
        })
      },
      "key: ",
    )
    .unwrap(),
    drive_str(
      |inp| {
        Ok::<_, GraphqlErrors<&str>>(match const_object_field(inp) {
          Err(errors) => errors.into_iter().next().is_some_and(|error| {
            matches!(
              error.into_data(),
              ErrorData::UnexpectedEndOfObjectFieldValue(end)
                if end.hint() == &ObjectFieldValueHint::Value
            )
          }),
          Ok(_) => false,
        })
      },
      "key: ",
    )
    .unwrap(),
  ] {
    assert!(is_expected);
  }
}

#[test]
fn object_fields_wrong_value_leave_closing_brace() {
  let (expects_value, brace_remains) = drive_str(
    |inp| {
      let expects_value = match object_field(inp) {
        Err(errors) => errors.into_iter().next().is_some_and(|error| {
          matches!(
            error.into_data(),
            ErrorData::UnexpectedToken(unexpected)
              if unexpected.expected() == &Expectation::InputValue
                && matches!(unexpected.found(), Some(SyntacticTokenKind::RBrace))
          )
        }),
        Ok(_) => false,
      };
      let brace_remains = matches!(
        inp
          .next()?
          .expect("the closing brace should remain for the object parser")
          .into_data(),
        GraphqlToken::<'_, str>::RBrace
      );
      Ok::<_, GraphqlErrors<&str>>((expects_value, brace_remains))
    },
    "key:}",
  )
  .unwrap();
  assert!(expects_value);
  assert!(brace_remains);

  let (expects_value, brace_remains) = drive_str(
    |inp| {
      let expects_value = match const_object_field(inp) {
        Err(errors) => errors.into_iter().next().is_some_and(|error| {
          matches!(
            error.into_data(),
            ErrorData::UnexpectedToken(unexpected)
              if unexpected.expected() == &Expectation::ConstInputValue
                && matches!(unexpected.found(), Some(SyntacticTokenKind::RBrace))
          )
        }),
        Ok(_) => false,
      };
      let brace_remains = matches!(
        inp
          .next()?
          .expect("the closing brace should remain for the object parser")
          .into_data(),
        GraphqlToken::<'_, str>::RBrace
      );
      Ok::<_, GraphqlErrors<&str>>((expects_value, brace_remains))
    },
    "key:}",
  )
  .unwrap();
  assert!(expects_value);
  assert!(brace_remains);
}

#[test]
fn object_non_terminator_commits_and_expects_name() {
  let error = drive_str(|inp| InputValueParser::graphql(inp).map(|_| ()), "{ : 1 }")
    .expect_err("a non-terminator commits the object-field parser")
    .into_iter()
    .next()
    .expect("the invalid field name should emit an error");
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::Name
        && matches!(unexpected.found(), Some(SyntacticTokenKind::Colon))
  ));
}

// ─── default value ───────────────────────────────────────────────────────────

#[test]
fn default_value_present() {
  fn check<S: AsRef<[u8]>>(v: Option<DefaultInputValue<S>>) {
    let default = v.expect("present");
    assert!(default.value().is_int());
    assert_eq!(*default.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(default_value, "= 42", check);
}

#[test]
fn default_value_absent_on_empty_input() {
  assert!(drive_str(default_value, "").unwrap().is_none());
  assert!(drive_slice(default_value, b"").unwrap().is_none());
}

#[test]
fn default_value_absent_without_equal() {
  // No `=`, so it declines to `None` and leaves the token in place.
  let (absent, leftover_is_int) = drive_str(
    |inp| {
      let default = default_value(inp)?;
      let leftover = InputValueParser::graphql(inp)?;
      Ok::<_, GraphqlErrors<&str>>((default.is_none(), leftover.is_int()))
    },
    "42",
  )
  .unwrap();
  assert!(absent);
  assert!(leftover_is_int);
}

fn assert_try_default_tail_error(src: &'static str, kind: SyntacticTokenKind) {
  let (diagnostic_matches, leftover_kind) = drive_str(
    |inp| {
      let diagnostic_matches = match try_default_value(inp) {
        Err(errors) => errors.into_iter().next().is_some_and(|error| {
          matches!(
            error.into_data(),
            ErrorData::UnexpectedToken(unexpected)
              if unexpected.expected() == &Expectation::ConstInputValue
                && unexpected.found() == Some(&kind)
          )
        }),
        Ok(_) => false,
      };
      let leftover_kind = inp.next()?.map(|token| token.into_data().kind());
      Ok::<_, GraphqlErrors<&str>>((diagnostic_matches, leftover_kind))
    },
    src,
  )
  .unwrap();
  assert!(diagnostic_matches);
  assert_eq!(leftover_kind, Some(kind));
  assert!(drive_str(default_value, src).is_err());
}

#[test]
fn try_default_value_equal_without_tail_expects_const_value() {
  let error = drive_str(|inp| try_default_value(inp).map(|_| ()), "=")
    .expect_err("`=` commits even at end of input")
    .into_iter()
    .next()
    .expect("a missing default value should emit an error");
  assert_eq!(error.span(), SimpleSpan::new(1, 1));
  assert!(matches!(
    error.into_data(),
    ErrorData::UnexpectedToken(unexpected)
      if unexpected.expected() == &Expectation::ConstInputValue
        && unexpected.found().is_none()
  ));
  assert!(drive_str(default_value, "=").is_err());
}

#[test]
fn try_default_value_malformed_tail_commits_and_leaves_token() {
  assert_try_default_tail_error("= }", SyntacticTokenKind::RBrace);
  assert_try_default_tail_error("= $v", SyntacticTokenKind::Dollar);
}

// ─── try_ twins: the committed/attempt pairs ─────────────────────────────────

/// After a declining `try_` twin, the whole input is still available — a plain
/// `value` parse recovers the token the twin left untouched (the position law).
macro_rules! try_declines_leaving {
  ($try_fn:path, $src:literal) => {{
    let (declined, recovered) = drive_str(
      |inp| {
        let attempt = $try_fn(inp)?;
        let recovered = InputValueParser::graphql(inp).is_ok();
        Ok::<_, GraphqlErrors<&str>>((attempt.is_decline(), recovered))
      },
      $src,
    )
    .unwrap();
    assert!(
      declined,
      concat!(stringify!($try_fn), " should decline on ", $src)
    );
    assert!(
      recovered,
      concat!(stringify!($try_fn), " should leave the input on ", $src)
    );
  }};
}

#[test]
fn try_leaf_twins_decline_and_leave() {
  try_declines_leaving!(IntValueParser::try_graphql, "true");
  try_declines_leaving!(FloatValueParser::try_graphql, "42");
  try_declines_leaving!(StringValueParser::try_graphql, "42");
  try_declines_leaving!(BooleanValueParser::<&str>::try_graphql, "ACTIVE");
  try_declines_leaving!(NullValueParser::try_graphql, "ACTIVE");
  try_declines_leaving!(EnumValueParser::try_graphql, "true"); // reserved spelling → not an enum
}

#[test]
fn try_leaf_twins_accept() {
  assert!(matches!(
    drive_str(IntValueParser::try_graphql, "42").unwrap(),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(FloatValueParser::try_graphql, "3.14").unwrap(),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(StringValueParser::try_graphql, "\"hi\"").unwrap(),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(BooleanValueParser::<&str>::try_graphql, "true").unwrap(),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(NullValueParser::try_graphql, "null").unwrap(),
    ParseAttempt::Accept(_)
  ));
  assert!(matches!(
    drive_str(EnumValueParser::try_graphql, "ACTIVE").unwrap(),
    ParseAttempt::Accept(_)
  ));
}

#[test]
fn boolean_value_committed_accepts_and_rejects() {
  let true_value: BooleanValue<&str> =
    drive_str(BooleanValueParser::<&str>::graphql, "true").unwrap();
  assert!(true_value.value());

  let false_value: BooleanValue<&[u8]> =
    drive_slice(BooleanValueParser::<&[u8]>::graphql, b"false").unwrap();
  assert!(!false_value.value());

  for rejected in ["null", "ACTIVE", "42"] {
    assert!(drive_str(BooleanValueParser::<&str>::graphql, rejected).is_err());
    assert!(drive_slice(BooleanValueParser::<&[u8]>::graphql, rejected.as_bytes()).is_err());
  }
}

#[test]
fn null_value_committed_accepts_and_rejects() {
  assert!(
    "null".equivalent(
      drive_str(NullValueParser::graphql, "null")
        .unwrap()
        .source()
    )
  );
  reject_all!(NullValueParser::graphql, "true");
  reject_all!(NullValueParser::graphql, "ACTIVE");
}

#[test]
fn enum_value_committed_excludes_reserved() {
  assert!(
    "ACTIVE".equivalent(
      drive_str(EnumValueParser::graphql, "ACTIVE")
        .unwrap()
        .source()
    )
  );
  // Soft keywords are enums; the three reserved spellings are excluded.
  assert!(drive_str(EnumValueParser::graphql, "type").is_ok());
  reject_all!(EnumValueParser::graphql, "true");
  reject_all!(EnumValueParser::graphql, "false");
  reject_all!(EnumValueParser::graphql, "null");
}

// ─── variable commitment regressions ─────────────────────────────────────────

#[test]
fn variable_dollar_commits_and_reports_missing_name() {
  // The try parser's U1 head is `$`: once it sees that token, a missing name is a
  // committed variable error rather than a decline.
  let (is_eot, at_eot) = drive_str(
    |inp| {
      let is_eot = match VariableValueParser::try_graphql(inp) {
        Err(errs) => errs
          .into_iter()
          .next()
          .is_some_and(|error| error.data().is_end_of_input()),
        Ok(_) => false,
      };
      let at_eot = inp.next()?.is_none();
      Ok::<_, GraphqlErrors<&str>>((is_eot, at_eot))
    },
    "$",
  )
  .unwrap();
  assert!(is_eot);
  assert!(at_eot);
}

#[test]
fn variable_wrong_name_token_is_left_for_its_parent() {
  let (errored, colon_remains) = drive_str(
    |inp| {
      let errored = VariableValueParser::try_graphql(inp).is_err();
      let colon_remains = matches!(
        inp
          .next()?
          .expect("the wrong name token should remain")
          .into_data(),
        GraphqlToken::<'_, str>::Colon
      );
      Ok::<_, GraphqlErrors<&str>>((errored, colon_remains))
    },
    "$:name",
  )
  .unwrap();
  assert!(errored);
  assert!(colon_remains);
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts the frozen `smear-parser` `parse_input_value` produces for
/// the same inputs. `smear-parser` is not a dependency here, so the verdicts are
/// pinned as a table (the spec/frozen behaviour is the arbiter). Deviations from
/// frozen would appear only via the Deviations Register — there are none for values.
const VALUE_ORACLE: &[(&str, bool)] = &[
  ("42", true),
  ("-7", true),
  ("3.14", true),
  ("1.0e10", true),
  ("\"text\"", true),
  ("\"\"\"block\"\"\"", true),
  ("true", true),
  ("false", true),
  ("null", true),
  ("ENUM_VALUE", true),
  ("$variable", true),
  ("[]", true),
  ("[1, 2, 3]", true),
  ("{}", true),
  ("{ a: 1, b: $v }", true),
  ("[{ nested: [true] }]", true),
  ("}", false),
  (")", false),
  ("]", false),
  (":", false),
  ("@", false),
  ("=", false),
  ("", false),
  ("$", false),
];

#[test]
fn value_matches_frozen_verdicts() {
  for (src, accept) in VALUE_ORACLE {
    assert_eq!(
      drive_str(|inp| InputValueParser::graphql(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str value({src:?})"
    );
    assert_eq!(
      drive_slice(
        |inp| InputValueParser::graphql(inp).map(|_| ()),
        src.as_bytes(),
      )
      .is_ok(),
      *accept,
      "slice value({src:?})"
    );
  }
}

/// The const oracle mirrors the value oracle minus the variable acceptances: a
/// variable in any position is a rejection in a constant context.
const CONST_VALUE_ORACLE: &[(&str, bool)] = &[
  ("42", true),
  ("3.14", true),
  ("\"text\"", true),
  ("true", true),
  ("null", true),
  ("ENUM_VALUE", true),
  ("[1, 2]", true),
  ("{ a: 1 }", true),
  ("[{ nested: [true] }]", true),
  ("$variable", false),
  ("[$v]", false),
  ("{ a: $v }", false),
  ("}", false),
  ("", false),
];

#[test]
fn const_value_matches_frozen_verdicts() {
  for (src, accept) in CONST_VALUE_ORACLE {
    assert_eq!(
      drive_str(|inp| ConstInputValueParser::graphql(inp).map(|_| ()), src,).is_ok(),
      *accept,
      "str const_value({src:?})"
    );
    assert_eq!(
      drive_slice(
        |inp| ConstInputValueParser::graphql(inp).map(|_| ()),
        src.as_bytes()
      )
      .is_ok(),
      *accept,
      "slice const_value({src:?})"
    );
  }
}
