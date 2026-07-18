//! Value-production tests.
//!
//! Every production is driven end to end over the real GraphQL syntactic lexer under
//! a `Fatal<GraphqlErrors>` context — a no-op [`CstEmitter`](tokora::emitter::CstEmitter),
//! so these also prove the `node(kind)` brackets compile and run tree-lessly at zero
//! cost (no rowan anywhere). Each accept case runs the full source matrix (`str`,
//! `[u8]`, and `Bytes` behind the feature), asserting equal ASTs modulo the slice
//! type; reject cases assert the error family; and a table-driven oracle pins the
//! accept/reject verdicts the frozen `smear-parser` crate produces for the same
//! inputs (the true/false/null-before-enum ordering included).
//!
//! The productions are `Lang`-generic, so — exactly as the frozen crate's
//! `run_parse_str` does — they are driven here through `Parser::with_parser` with the
//! marker left at its `()` default; the dialect is carried by the AST, kinds, and
//! error, not by the marker.

use smear_lexer::graphql::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser, SimpleSpan, utils::cmp::Equivalent};

use super::{
  const_object_field, const_value, default_value, float_value, int_value, object_field,
  string_value, try_variable_value, value, variable_value,
};
use crate::graphql::{
  ast::{ConstInputValue, DefaultInputValue, InputValue},
  error::GraphqlErrors,
};
use tokora::try_parse_input::ParseAttempt;

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_bytes(input)
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

// ─── Leaf builders (driven standalone) ───────────────────────────────────────

#[test]
fn int_value_accepts() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::IntValue<S>) {
    assert!("42".equivalent(v.source_ref()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 2));
  }
  accept_all!(int_value, "42", check);
}

#[test]
fn int_value_rejects_non_int() {
  reject_all!(int_value, "\"s\"");
  reject_all!(int_value, "");
}

#[test]
fn float_value_accepts() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::FloatValue<S>) {
    assert!("3.14".equivalent(v.source_ref()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(float_value, "3.14", check);
}

#[test]
fn float_value_rejects_int() {
  reject_all!(float_value, "42");
}

#[test]
fn string_value_accepts_inline() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::StringValue<S>) {
    assert!("\"hi\"".equivalent(v.source_ref()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(string_value, "\"hi\"", check);
}

#[test]
fn string_value_accepts_block() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::StringValue<S>) {
    assert!("\"\"\"hi\"\"\"".equivalent(v.source_ref()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 8));
  }
  accept_all!(string_value, "\"\"\"hi\"\"\"", check);
}

#[test]
fn string_value_rejects_int() {
  reject_all!(string_value, "42");
}

// ─── Variable ────────────────────────────────────────────────────────────────

#[test]
fn variable_value_accepts() {
  fn check<S: AsRef<[u8]>>(v: crate::graphql::ast::VariableValue<S>) {
    assert!("userId".equivalent(v.name().source_ref()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 7));
  }
  accept_all!(variable_value, "$userId", check);
}

#[test]
fn variable_value_rejects_missing_name() {
  reject_all!(variable_value, "$");
}

#[test]
fn variable_value_rejects_without_dollar() {
  reject_all!(variable_value, "userId");
}

#[test]
fn try_variable_value_accepts_and_declines() {
  // Accept on `$name`.
  let accepted = drive_str(try_variable_value, "$x").unwrap();
  assert!(matches!(accepted, ParseAttempt::Accept(_)));
  // Decline (nothing consumed) on a non-`$` token.
  let declined = drive_str(
    |inp| {
      let attempt = try_variable_value(inp)?;
      // The declined identifier is untouched, so a plain value pulls it back.
      let leftover = value(inp)?;
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
    let i = v.unwrap_int();
    assert!("42".equivalent(i.source_ref()));
    assert_eq!(*i.span(), SimpleSpan::new(0, 2));
  }
  accept_all!(value, "42", check);
}

#[test]
fn value_float_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!("1.5e3".equivalent(v.unwrap_float().source_ref()));
  }
  accept_all!(value, "1.5e3", check);
}

#[test]
fn value_string_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!("\"hi\"".equivalent(v.unwrap_string().source_ref()));
  }
  accept_all!(value, "\"hi\"", check);
}

#[test]
fn value_block_string_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_string());
  }
  accept_all!(value, "\"\"\"block\"\"\"", check);
}

#[test]
fn value_true_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let b = v.unwrap_boolean();
    assert!(b.value());
    assert_eq!(*b.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(value, "true", check);
}

#[test]
fn value_false_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(!v.unwrap_boolean().value());
  }
  accept_all!(value, "false", check);
}

#[test]
fn value_null_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let n = v.unwrap_null();
    assert!("null".equivalent(n.source_ref()));
    assert_eq!(*n.span(), SimpleSpan::new(0, 4));
  }
  accept_all!(value, "null", check);
}

#[test]
fn value_enum_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let e = v.unwrap_enum();
    assert!("ACTIVE".equivalent(e.source_ref()));
    assert_eq!(*e.span(), SimpleSpan::new(0, 6));
  }
  accept_all!(value, "ACTIVE", check);
}

#[test]
fn value_enum_arm_accepts_soft_keywords() {
  // `enum`/`type` are soft keywords: only true/false/null are excluded from enum.
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_enum());
  }
  accept_all!(value, "enum", check);
  accept_all!(value, "type", check);
}

#[test]
fn value_variable_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!("x".equivalent(v.unwrap_variable().name().source_ref()));
  }
  accept_all!(value, "$x", check);
}

#[test]
fn value_list_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let list = v.unwrap_list();
    assert_eq!(list.values().len(), 2);
    assert_eq!(*list.span(), SimpleSpan::new(0, 6));
    assert!(list.values()[0].is_int());
    assert!(list.values()[1].is_int());
  }
  accept_all!(value, "[1, 2]", check);
}

#[test]
fn value_empty_list_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.unwrap_list().values().is_empty());
  }
  accept_all!(value, "[]", check);
}

#[test]
fn value_object_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let obj = v.unwrap_object();
    assert_eq!(obj.fields().len(), 1);
    let field = &obj.fields()[0];
    assert!("a".equivalent(field.name().source_ref()));
    assert!(field.value().is_int());
  }
  accept_all!(value, "{ a: 1 }", check);
}

#[test]
fn value_empty_object_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.unwrap_object().fields().is_empty());
  }
  accept_all!(value, "{}", check);
}

#[test]
fn value_nested_list_and_object() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let outer = v.unwrap_list();
    assert_eq!(outer.values().len(), 2);
    // [ [1], { k: $v } ]
    assert_eq!(outer.values()[0].unwrap_list_ref().values().len(), 1);
    let obj = outer.values()[1].unwrap_object_ref();
    assert!(obj.fields()[0].value().is_variable());
  }
  accept_all!(value, "[[1], { k: $v }]", check);
}

// ─── value: enum exclusion ordering (Ruling 2) ───────────────────────────────

#[test]
fn value_resolves_true_false_null_before_enum() {
  // The three reserved spellings never fall through to the enum arm.
  assert!(drive_str(value, "true").unwrap().is_boolean());
  assert!(drive_str(value, "false").unwrap().is_boolean());
  assert!(drive_str(value, "null").unwrap().is_null());
  // Anything else on the identifier arm is an enum.
  assert!(drive_str(value, "trueish").unwrap().is_enum());
  assert!(drive_str(value, "NULLABLE").unwrap().is_enum());
}

// ─── value: reject rows + error families ─────────────────────────────────────

#[test]
fn value_rejects_non_value_heads() {
  reject_all!(value, "}");
  reject_all!(value, ")");
  reject_all!(value, "]");
  reject_all!(value, ":");
  reject_all!(value, "@");
  reject_all!(value, "=");
}

#[test]
fn value_unexpected_token_error_family() {
  let is_unexpected = |src: &str| match drive_str(|inp| value(inp).map(|_| ()), src) {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_unexpected_token()),
    Ok(()) => false,
  };
  assert!(is_unexpected("}"));
  assert!(is_unexpected("@"));
}

#[test]
fn value_end_of_input_error_family() {
  let is_eot = match drive_str(|inp| value(inp).map(|_| ()), "") {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_end_of_input()),
    Ok(()) => false,
  };
  assert!(is_eot);
}

#[test]
fn value_unterminated_list_is_error() {
  reject_all!(value, "[1, 2");
}

#[test]
fn value_object_field_missing_colon_is_error() {
  reject_all!(value, "{ a 1 }");
}

// ─── const_value ─────────────────────────────────────────────────────────────

#[test]
fn const_value_int_arm() {
  fn check<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!("7".equivalent(v.unwrap_int().source_ref()));
  }
  accept_all!(const_value, "7", check);
}

#[test]
fn const_value_enum_and_scalars() {
  fn check_enum<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!(v.is_enum());
  }
  accept_all!(const_value, "ACTIVE", check_enum);
  fn check_bool<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!(v.unwrap_boolean().value());
  }
  accept_all!(const_value, "true", check_bool);
  fn check_null<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!(v.is_null());
  }
  accept_all!(const_value, "null", check_null);
}

#[test]
fn const_value_list_and_object() {
  fn check<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    let obj = v.unwrap_object();
    assert_eq!(obj.fields().len(), 1);
    assert!(obj.fields()[0].value().is_list());
  }
  accept_all!(const_value, "{ xs: [1, 2] }", check);
}

#[test]
fn const_value_rejects_variable() {
  reject_all!(const_value, "$x");
  // The rejection is an unexpected-token, not an end-of-input.
  let family = match drive_str(|inp| const_value(inp).map(|_| ()), "$x") {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_unexpected_token()),
    Ok(()) => false,
  };
  assert!(family);
}

#[test]
fn const_value_rejects_nested_variable() {
  reject_all!(const_value, "[$x]");
  reject_all!(const_value, "{ a: $x }");
}

// ─── object fields ───────────────────────────────────────────────────────────

#[test]
fn object_field_accepts() {
  fn check<S: AsRef<[u8]>>(f: crate::graphql::ast::ObjectField<S>) {
    assert!("name".equivalent(f.name().source_ref()));
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
    assert!("true".equivalent(f.name().source_ref()));
    assert!(f.value().is_int());
  }
  accept_all!(object_field, "true: 1", check);
}

#[test]
fn const_object_field_accepts_and_rejects_variable() {
  fn check<S: AsRef<[u8]>>(f: crate::graphql::ast::ConstObjectField<S>) {
    assert!("k".equivalent(f.name().source_ref()));
    assert!(f.value().is_int());
  }
  accept_all!(const_object_field, "k: 1", check);
  reject_all!(const_object_field, "k: $v");
}

// ─── default value ───────────────────────────────────────────────────────────

#[test]
fn default_value_present() {
  fn check<S: AsRef<[u8]>>(v: Option<DefaultInputValue<S>>) {
    let default = v.expect("present");
    assert!(default.value().is_int());
    assert_eq!(default.span().start(), 0);
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
      let leftover = value(inp)?;
      Ok::<_, GraphqlErrors<&str>>((default.is_none(), leftover.is_int()))
    },
    "42",
  )
  .unwrap();
  assert!(absent);
  assert!(leftover_is_int);
}

#[test]
fn default_value_rejects_bad_const_value() {
  // `=` commits; a following non-const-value is an error, not a decline.
  reject_all!(default_value, "= }");
  reject_all!(default_value, "= $v");
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
      drive_str(|inp| value(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str value({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| value(inp).map(|_| ()), src.as_bytes()).is_ok(),
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
      drive_str(|inp| const_value(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str const_value({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| const_value(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice const_value({src:?})"
    );
  }
}
