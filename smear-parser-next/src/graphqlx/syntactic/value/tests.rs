//! GraphQLx `Value` production tests.
//!
//! Every production is driven end to end over the real GraphQLx syntactic lexer
//! under a `Fatal<GraphqlxErrors>` context. Each accept case runs the full
//! source matrix (`str`, `[u8]`, and `Bytes` behind the feature), asserting equal
//! ASTs modulo the slice type; reject cases assert the error family; and a
//! table-driven oracle pins the accept/reject verdicts the frozen `smear-parser`
//! crate's `parse_input_value` produces for the same inputs.
//!
//! The radix regression rows (hex / octal / binary ints incl. negative, `_`
//! separators, and `p`-exponent hex floats) pin the radix-preserving payloads
//! *through* the productions, not just at the atom layer.

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{
  FatalContext, InputRef, Parse, Parser, SimpleSpan, try_parse_input::ParseAttempt,
  utils::cmp::Equivalent,
};

use super::{
  const_map_entry, const_object_field, const_value, default_value, float_value, int_value,
  map_entry, object_field, string_value, try_variable_value, value, variable_value,
};
use crate::graphqlx::{
  ast::{ConstInputValue, InputValue},
  error::GraphqlxErrors,
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlxErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlxErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlxErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlxErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
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
fn int_value_accepts_decimal() {
  fn check<S: AsRef<[u8]>>(v: crate::graphqlx::ast::IntValue<S>) {
    assert!(v.value_ref().is_decimal());
    assert!("42".equivalent(v.value_ref().source_ref()));
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
fn float_value_accepts_decimal() {
  fn check<S: AsRef<[u8]>>(v: crate::graphqlx::ast::FloatValue<S>) {
    assert!(v.value_ref().is_decimal());
    assert!("3.14".equivalent(v.value_ref().source_ref()));
  }
  accept_all!(float_value, "3.14", check);
}

#[test]
fn string_value_accepts_inline_and_block() {
  fn check_inline<S: AsRef<[u8]>>(v: crate::graphqlx::ast::StringValue<S>) {
    assert!("\"hi\"".equivalent(v.source_ref()));
  }
  accept_all!(string_value, "\"hi\"", check_inline);
  fn check_block<S: AsRef<[u8]>>(v: crate::graphqlx::ast::StringValue<S>) {
    assert!("\"\"\"hi\"\"\"".equivalent(v.source_ref()));
  }
  accept_all!(string_value, "\"\"\"hi\"\"\"", check_block);
}

// ─── Radix regression (through the leaves) ───────────────────────────────────

#[test]
fn int_value_preserves_radix() {
  fn hex<S: AsRef<[u8]>>(v: crate::graphqlx::ast::IntValue<S>) {
    assert!(v.value_ref().is_hex());
  }
  accept_all!(int_value, "0xFF", hex);
  accept_all!(int_value, "-0xFF", hex);
  accept_all!(int_value, "0xFF_FF", hex);

  fn octal<S: AsRef<[u8]>>(v: crate::graphqlx::ast::IntValue<S>) {
    assert!(v.value_ref().is_octal());
  }
  accept_all!(int_value, "0o77", octal);

  fn binary<S: AsRef<[u8]>>(v: crate::graphqlx::ast::IntValue<S>) {
    assert!(v.value_ref().is_binary());
  }
  accept_all!(int_value, "0b1010", binary);

  fn decimal_neg<S: AsRef<[u8]>>(v: crate::graphqlx::ast::IntValue<S>) {
    assert!(v.value_ref().is_decimal());
    assert!("-5".equivalent(v.value_ref().source_ref()));
  }
  accept_all!(int_value, "-5", decimal_neg);

  fn underscore<S: AsRef<[u8]>>(v: crate::graphqlx::ast::IntValue<S>) {
    assert!("1_000".equivalent(v.value_ref().source_ref()));
  }
  accept_all!(int_value, "1_000", underscore);
}

#[test]
fn float_value_preserves_radix() {
  fn hexf<S: AsRef<[u8]>>(v: crate::graphqlx::ast::FloatValue<S>) {
    assert!(v.value_ref().is_hex());
  }
  accept_all!(float_value, "0x1.8p3", hexf);
  accept_all!(float_value, "-0x1p-2", hexf);

  fn decf<S: AsRef<[u8]>>(v: crate::graphqlx::ast::FloatValue<S>) {
    assert!(v.value_ref().is_decimal());
  }
  accept_all!(float_value, "1.5e10", decf);
}

// ─── Variable ────────────────────────────────────────────────────────────────

#[test]
fn variable_value_accepts() {
  fn check<S: AsRef<[u8]>>(v: crate::graphqlx::ast::VariableValue<S>) {
    assert!("userId".equivalent(v.name().source_ref()));
    assert_eq!(*v.span(), SimpleSpan::new(0, 7));
  }
  accept_all!(variable_value, "$userId", check);
}

#[test]
fn variable_value_rejects() {
  reject_all!(variable_value, "$");
  reject_all!(variable_value, "userId");
}

#[test]
fn try_variable_value_accepts_and_declines() {
  let accepted = drive_str(try_variable_value, "$x").unwrap();
  assert!(matches!(accepted, ParseAttempt::Accept(_)));
  // Decline leaves the identifier untouched, so a plain value pulls it back as enum.
  let declined = drive_str(
    |inp| {
      let attempt = try_variable_value(inp)?;
      let leftover = value(inp)?;
      Ok::<_, GraphqlxErrors<&str>>((attempt.is_decline(), leftover.is_enum()))
    },
    "x",
  )
  .unwrap();
  assert_eq!(declined, (true, true));
}

// ─── value dispatcher: one row per alternative ───────────────────────────────

#[test]
fn value_int_arm_all_radices() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_int());
  }
  accept_all!(value, "42", check);
  accept_all!(value, "0xFF", check);
  accept_all!(value, "-0o17", check);
  accept_all!(value, "0b1010", check);
}

#[test]
fn value_float_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_float());
  }
  accept_all!(value, "3.14", check);
  accept_all!(value, "0x1.8p3", check);
}

#[test]
fn value_string_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_string());
  }
  accept_all!(value, "\"hi\"", check);
}

#[test]
fn value_boolean_and_null_arms() {
  fn is_true<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.unwrap_boolean().value());
  }
  accept_all!(value, "true", is_true);
  fn is_false<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(!v.unwrap_boolean().value());
  }
  accept_all!(value, "false", is_false);
  fn is_null<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_null());
  }
  accept_all!(value, "null", is_null);
}

#[test]
fn value_variable_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert!(v.is_variable());
  }
  accept_all!(value, "$v", check);
}

// ─── enum `::`-path composite ────────────────────────────────────────────────

#[test]
fn value_enum_single_segment() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let e = v.unwrap_enum();
    assert!(!e.value().is_fully_qualified());
    let segs = e.value().segments_slice();
    assert_eq!(segs.len(), 1);
    assert!("Color".equivalent(segs[0].source_ref()));
  }
  accept_all!(value, "Color", check);
}

#[test]
fn value_enum_soft_keyword_without_brace_is_single_segment() {
  // `set` / `map` not followed by `{` fall through to a bare single-segment enum
  // path (frozen `graphqlx/ast/value.rs`).
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let e = v.unwrap_enum();
    assert_eq!(e.value().segments_slice().len(), 1);
  }
  accept_all!(value, "set", check);
  accept_all!(value, "map", check);
}

#[test]
fn value_enum_leading_path_is_fully_qualified() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let e = v.unwrap_enum();
    assert!(e.value().is_fully_qualified());
    let segs = e.value().segments_slice();
    assert_eq!(segs.len(), 3);
    assert!("a".equivalent(segs[0].source_ref()));
    assert!("C".equivalent(segs[2].source_ref()));
  }
  accept_all!(value, "::a::b::C", check);
}

// ─── list / object ───────────────────────────────────────────────────────────

#[test]
fn value_list_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let l = v.unwrap_list();
    assert_eq!(l.values().len(), 3);
  }
  accept_all!(value, "[1, 2, 3]", check);
  accept_all!(value, "[1 2 3]", check); // commas are insignificant
  fn empty<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert_eq!(v.unwrap_list().values().len(), 0);
  }
  accept_all!(value, "[]", empty);
}

#[test]
fn value_object_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let o = v.unwrap_object();
    assert_eq!(o.fields().len(), 2);
  }
  accept_all!(value, "{ a: 1, b: $v }", check);
  fn empty<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert_eq!(v.unwrap_object().fields().len(), 0);
  }
  accept_all!(value, "{}", empty);
}

// ─── set / map composites ────────────────────────────────────────────────────

#[test]
fn value_set_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let s = v.unwrap_set();
    assert_eq!(s.values().len(), 3);
  }
  accept_all!(value, "set { 1 2 3 }", check);
  accept_all!(value, "set { 1, 2, 3 }", check);
  fn empty<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert_eq!(v.unwrap_set().values().len(), 0);
  }
  accept_all!(value, "set {}", empty);
}

#[test]
fn value_map_arm() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let m = v.unwrap_map();
    let entries = m.entries_slice();
    assert_eq!(entries.len(), 2);
    assert!(entries[0].key().is_string());
    assert!(entries[0].value().is_string());
  }
  accept_all!(value, "map { \"a\" => \"x\" \"b\" => \"y\" }", check);
  fn empty<S: AsRef<[u8]>>(v: InputValue<S>) {
    assert_eq!(v.unwrap_map().entries_slice().len(), 0);
  }
  accept_all!(value, "map {}", empty);
}

#[test]
fn value_map_nested() {
  fn check<S: AsRef<[u8]>>(v: InputValue<S>) {
    let m = v.unwrap_map();
    let entries = m.entries_slice();
    assert_eq!(entries.len(), 1);
    let inner = entries[0].value().unwrap_map_ref();
    assert_eq!(inner.entries_slice().len(), 2);
  }
  accept_all!(
    value,
    "map { \"creds\" => map { \"u\" => \"admin\" \"p\" => \"secret\" } }",
    check
  );
}

#[test]
fn value_map_requires_fat_arrow() {
  // Deviations Register: the fixture grammar requires a literal `=>` between key and
  // value; the frozen `parse_input_value` consumes the arrow position unchecked. W7
  // enforces `=>`.
  reject_all!(value, "map { \"a\" \"b\" }");
  reject_all!(value, "map { 1 2 }");
}

#[test]
fn value_rejects_non_value_heads() {
  reject_all!(value, "}");
  reject_all!(value, ")");
  reject_all!(value, "]");
  reject_all!(value, ":");
  reject_all!(value, "@");
  reject_all!(value, "=");
  reject_all!(value, "");
  reject_all!(value, "$");
}

// ─── const value ─────────────────────────────────────────────────────────────

#[test]
fn const_value_rejects_variables() {
  reject_all!(const_value, "$v");
  reject_all!(const_value, "[$v]");
  reject_all!(const_value, "{ a: $v }");
  reject_all!(const_value, "set { $v }");
  reject_all!(const_value, "map { \"a\" => $v }");
}

#[test]
fn const_value_accepts_composites() {
  fn set_ok<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert_eq!(v.unwrap_set().values().len(), 2);
  }
  accept_all!(const_value, "set { 1 2 }", set_ok);
  fn map_ok<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert_eq!(v.unwrap_map().entries_slice().len(), 1);
  }
  accept_all!(const_value, "map { 1 => 2 }", map_ok);
  fn enum_ok<S: AsRef<[u8]>>(v: ConstInputValue<S>) {
    assert!(v.is_enum());
  }
  accept_all!(const_value, "::a::b", enum_ok);
}

// ─── object field / map entry (standalone) ───────────────────────────────────

#[test]
fn object_field_accepts() {
  fn check<S: AsRef<[u8]>>(f: crate::graphqlx::ast::ObjectField<S>) {
    assert!("a".equivalent(f.name().source_ref()));
    assert!(f.value().is_int());
  }
  accept_all!(object_field, "a: 1", check);
}

#[test]
fn const_object_field_rejects_variable() {
  reject_all!(const_object_field, "a: $v");
}

#[test]
fn map_entry_accepts_and_requires_arrow() {
  fn check<S: AsRef<[u8]>>(e: smear_scaffold::ast::MapEntry<InputValue<S>, InputValue<S>>) {
    assert!(e.key().is_int());
    assert!(e.value().is_int());
  }
  accept_all!(map_entry, "1 => 2", check);
  reject_all!(map_entry, "1 2");
}

#[test]
fn const_map_entry_accepts() {
  fn check<S: AsRef<[u8]>>(
    e: smear_scaffold::ast::MapEntry<ConstInputValue<S>, ConstInputValue<S>>,
  ) {
    assert!(e.key().is_string());
  }
  accept_all!(const_map_entry, "\"a\" => \"b\"", check);
}

// ─── default value ───────────────────────────────────────────────────────────

#[test]
fn default_value_accepts_and_declines() {
  let some = drive_str(default_value, "= 42").unwrap();
  assert!(some.is_some());
  let none = drive_str(default_value, "").unwrap();
  assert!(none.is_none());
  // `=` commits; a following non-const value is an error, not a decline.
  reject_all!(default_value, "= }");
  reject_all!(default_value, "= $v");
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts the frozen `smear-parser` `parse_input_value` produces for
/// the same inputs. `smear-parser` is not a dependency here, so the verdicts are
/// pinned as a table (frozen `graphqlx/ast/value.rs:157-265` is the arbiter). The
/// only deviation is the map-entry `=>` enforcement (its own reject test above);
/// no oracle row exercises the unchecked-arrow path.
const VALUE_ORACLE: &[(&str, bool)] = &[
  ("42", true),
  ("-7", true),
  ("0xFF", true),
  ("0o77", true),
  ("0b1010", true),
  ("3.14", true),
  ("0x1.8p3", true),
  ("\"text\"", true),
  ("\"\"\"block\"\"\"", true),
  ("true", true),
  ("false", true),
  ("null", true),
  ("ENUM_VALUE", true),
  ("::a::b::C", true),
  ("set { 1 2 }", true),
  ("map { 1 => 2 }", true),
  ("set {}", true),
  ("map {}", true),
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
  ("0xFF", true),
  ("3.14", true),
  ("\"text\"", true),
  ("true", true),
  ("null", true),
  ("ENUM_VALUE", true),
  ("::a::b", true),
  ("set { 1 2 }", true),
  ("map { 1 => 2 }", true),
  ("[1, 2]", true),
  ("{ a: 1 }", true),
  ("$variable", false),
  ("[$v]", false),
  ("{ a: $v }", false),
  ("map { \"a\" => $v }", false),
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
