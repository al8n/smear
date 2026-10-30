//! The canonical form, checked against what graphql-js's `print` writes for the same literals.
//!
//! # Where the expectations come from
//!
//! graphql-js's `printer` and `printString`: `[a, b]`, `{a: 1}`, and the escape table that writes
//! `\b` `\t` `\n` `\f` `\r` `\"` `\\` short and every other C0 and C1 control as an upper-case
//! `\u00XX`. Every backslash a GraphQL spelling below needs is written `\u{5c}`, so that no escape
//! is read by Rust before the decoder reads it.

use std::{format, string::String, vec};

use smear_parser::graphql::ast::{
  ConstInputValue, ConstList, InputObjectTypeDefinition, Nested, TypeDefinition,
  TypeSystemDefinition, TypeSystemDefinitionOrExtension, TypeSystemDocument,
};
use tokora::{Parse as _, Parser, SimpleSpan};

use super::{
  super::{GraphQL, GraphqlErrors, GraphqlLexer, MAX_CONST_VALUE_DEPTH, type_system_document},
  Measure, Unprinted, canonical, escape,
};

/// The default of `input I { f: T = <literal> }`, printed, after checking the measure agrees.
fn printed(literal: &str) -> String {
  let sdl = format!("input I {{ f: T = {literal} }}");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(&sdl)
  .unwrap_or_else(|errors| panic!("`{sdl}` parses: {errors:?}"));
  let default = default_of(document.definitions()).expect("the document declares a default");

  let mut measure = Measure::default();
  canonical(default, &mut measure).expect("the literal prints");
  let mut out = String::new();
  canonical(default, &mut out).expect("the literal prints");
  assert_eq!(
    measure.0,
    out.len(),
    "the measure of `{literal}` is not what was written"
  );
  out
}

/// The one input field's default, out of a document holding one input object.
fn default_of<S>(
  definitions: &[TypeSystemDefinitionOrExtension<S>],
) -> Option<&ConstInputValue<S>> {
  let [TypeSystemDefinitionOrExtension::Definition(described)] = definitions else {
    return None;
  };
  let TypeSystemDefinition::Type(TypeDefinition::InputObject(input)) = described.node() else {
    return None;
  };
  input_default(input)
}

fn input_default<S>(input: &InputObjectTypeDefinition<S>) -> Option<&ConstInputValue<S>> {
  let [field] = input.fields_definition()?.input_value_definitions() else {
    return None;
  };
  Some(field.default_value()?.value())
}

#[test]
fn a_leaf_prints_as_written() {
  for (literal, expected) in [
    ("null", "null"),
    ("true", "true"),
    ("false", "false"),
    ("10", "10"),
    ("-3", "-3"),
    ("0", "0"),
    // A float keeps its lexeme: graphql-js would coerce `1.0` to `1` first, and this prints the
    // literal rather than a coerced value.
    ("1.0", "1.0"),
    ("1.5e3", "1.5e3"),
    ("-0.25E-2", "-0.25E-2"),
    ("ENUM_MEMBER", "ENUM_MEMBER"),
  ] {
    assert_eq!(printed(literal), expected, "`{literal}`");
  }
}

#[test]
fn a_string_prints_quoted_with_its_escapes_normalised() {
  for (literal, expected) in [
    ("\"plain\"", "\"plain\""),
    ("\"\"", "\"\""),
    // The short forms graphql-js writes, whatever escape the source used.
    (
      "\"\u{5c}b\u{5c}f\u{5c}n\u{5c}r\u{5c}t\"",
      "\"\\b\\f\\n\\r\\t\"",
    ),
    ("\"a\u{5c}\"b\u{5c}\u{5c}c\"", "\"a\\\"b\\\\c\""),
    // `/` is left alone, so `\/` prints as the character it names.
    ("\"a\u{5c}/b\"", "\"a/b\""),
    // A fixed-width or braced escape of an ordinary character prints the character.
    ("\"\u{5c}u0041\u{5c}u{e9}\"", "\"A\u{e9}\""),
    ("\"\u{5c}uD83D\u{5c}uDE00\"", "\"\u{1f600}\""),
    // The controls graphql-js spells as `\u00XX`, upper case, and the first character it does not.
    (
      "\"\u{5c}u0001\u{5c}u001F\u{5c}u007f\u{5c}u0080\u{5c}u009F\u{5c}u00A0\"",
      "\"\\u0001\\u001F\\u007F\\u0080\\u009F\u{a0}\"",
    ),
  ] {
    assert_eq!(printed(literal), expected, "`{literal}`");
  }
}

/// A block string prints as the quoted string its value is, which is what graphql-js prints too.
#[test]
fn a_block_string_prints_as_a_quoted_one() {
  assert_eq!(
    printed("\"\"\"\n    line one\n      \"quoted\"\n    \"\"\""),
    "\"line one\\n  \\\"quoted\\\"\""
  );
  assert_eq!(printed("\"\"\"\"\"\""), "\"\"");
  assert_eq!(
    printed("\"\"\"a \u{5c}\"\"\" b\"\"\""),
    "\"a \\\"\\\"\\\" b\""
  );
}

#[test]
fn a_container_prints_in_print_form() {
  for (literal, expected) in [
    ("[]", "[]"),
    ("[1,2 , 3]", "[1, 2, 3]"),
    ("[[1], [], [\"a\"]]", "[[1], [], [\"a\"]]"),
    ("{}", "{}"),
    ("{a:1,b:{c:[true, null]}}", "{a: 1, b: {c: [true, null]}}"),
    ("[{x: RED}, {y: \"\u{5c}n\"}]", "[{x: RED}, {y: \"\\n\"}]"),
  ] {
    assert_eq!(printed(literal), expected, "`{literal}`");
  }
}

/// Printing a printed value gives it back: the form is a fixed point, which is what lets the
/// introspection door hand a server's `defaultValue` to the builder and get the same text out.
#[test]
fn printing_is_idempotent() {
  for literal in [
    "{a:1,b:{c:[true, null]}}",
    "\"\"\"\n  x\n  y\n\"\"\"",
    "\"\u{5c}u0001 \u{5c}t \u{5c}/ \u{5c}u{1F600}\"",
    "[ENUM, -1.5e3, \"\"]",
  ] {
    let once = printed(literal);
    assert_eq!(printed(&once), once, "`{literal}` printed as `{once}`");
  }
}

/// `escape` between two quotes is `printString`, character for character.
#[test]
fn escaping_is_print_string() {
  fn quote(text: &str, out: &mut String) {
    out.push('"');
    escape(text, out);
    out.push('"');
  }

  let mut out = String::new();
  quote("a\"b\\c\u{1}\u{7f}\u{9f}\u{a0}/\u{1f600}\n", &mut out);
  assert_eq!(
    out,
    "\"a\\\"b\\\\c\\u0001\\u007F\\u009F\u{a0}/\u{1f600}\\n\""
  );

  let mut out = String::new();
  quote("", &mut out);
  assert_eq!(out, "\"\"");
}

/// The walk stops where `const_value` stops, and prints nothing past it.
#[test]
fn a_literal_past_the_ceiling_has_no_canonical_text() {
  fn nested(depth: usize) -> ConstInputValue<&'static str> {
    let span = SimpleSpan::const_new(0, 0);
    let mut value = ConstInputValue::List(ConstList::new(span, Nested::new(vec![])));
    for _ in 1..depth {
      value = ConstInputValue::List(ConstList::new(span, Nested::new(vec![value])));
    }
    value
  }

  let mut out = String::new();
  canonical(&nested(MAX_CONST_VALUE_DEPTH), &mut out).expect("the ceiling itself prints");
  assert_eq!(out.len(), 2 * MAX_CONST_VALUE_DEPTH);
  assert!(out.starts_with("[[") && out.ends_with("]]"));

  let mut measure = Measure::default();
  assert!(matches!(
    canonical(&nested(MAX_CONST_VALUE_DEPTH + 1), &mut measure),
    Err(Unprinted::TooDeep)
  ));
}

/// A string that is no text has no canonical text either, and says which literal it was.
#[test]
fn a_string_that_is_not_utf8_is_unreadable() {
  let sdl: &[u8] = b"input I { f: T = [\"ok\", \"\xff\"] }";
  let document = Parser::with_parser::<
    GraphqlLexer<'_, [u8]>,
    TypeSystemDocument<&[u8]>,
    GraphqlErrors<&[u8]>,
    _,
    GraphQL,
  >(type_system_document)
  .parse(sdl)
  .expect("the byte lexer passes the content through unread");
  let default = default_of(document.definitions()).expect("the document declares a default");

  let mut measure = Measure::default();
  match canonical(default, &mut measure) {
    Err(Unprinted::Unreadable(span)) => assert_eq!(
      &sdl[span.start()..span.end()],
      b"\"\xff\"",
      "the refusal points at the literal that is not text"
    ),
    other => panic!("expected an unreadable string, got {other:?}"),
  }
}
