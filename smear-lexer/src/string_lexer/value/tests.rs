//! Draft §2.9's static semantics, read off a spelling's bytes.
//!
//! # Where the expectations come from
//!
//! Two places, and neither is this file's own reading of the specification. The block-string cases
//! are draft §2.9.4's worked example and graphql-js's `blockString-test` cases, which an
//! independent Python port of `BlockStringValue()` reproduced byte for byte before any of this was
//! written. And every literal the lexer accepts is also cooked by the `Cow` conversions in the
//! modules beside this one — the ones `string_lexer/tests.rs` holds to `apollo-parser` — so
//! [`the_byte_decoder_agrees_with_the_cow_conversions`] makes those two answer the same question
//! over the same spellings, and a drift in either is caught by the other.

use std::{borrow::Cow, string::String, vec::Vec};

use super::{
  super::{LitBlockStr, LitInlineStr, LitStr},
  StringValueError, decode_string_value, string_value_len, string_value_pieces,
};

/// Decodes a spelling, or panics with the refusal.
fn decoded(spelling: &str) -> String {
  let mut out = String::new();
  decode_string_value(spelling.as_bytes(), &mut out)
    .unwrap_or_else(|error| panic!("`{spelling}` denotes a value: {error}"));
  out
}

/// The refusal a spelling produces.
fn refused(spelling: &[u8]) -> StringValueError {
  let mut out = String::from("kept");
  let error = decode_string_value(spelling, &mut out).expect_err("the spelling denotes no value");
  assert_eq!(out, "kept", "a refusal appended something");
  assert_eq!(string_value_len(spelling), Err(error));
  error
}

/// The cooked value of a literal through the lexer and its `Cow` conversion.
fn cooked(literal: &str) -> Cow<'_, str> {
  match LitStr::try_from(literal) {
    Ok(LitStr::Inline(inline)) => inline.into(),
    Ok(LitStr::Block(block)) => block.into(),
    Err(errors) => panic!("`{literal}` is a literal this lexer accepts: {errors:?}"),
  }
}

#[test]
fn every_inline_escape_is_its_character() {
  assert_eq!(decoded(r#""""#), "");
  assert_eq!(decoded(r#""plain""#), "plain");
  assert_eq!(
    decoded(r#""\" \\ \/ \b \f \n \r \t""#),
    "\" \\ / \u{8} \u{c} \n \r \t"
  );
  // `\u{5c}` is the backslash, so these are the fixed-width `\u` escapes the decoder reads rather
  // than characters the Rust lexer already read.
  assert_eq!(
    decoded("\"\u{5c}u0041\u{5c}u00e9\u{5c}u20AC\""),
    "A\u{e9}\u{20ac}"
  );
  // A surrogate pair is two escapes naming one character.
  assert_eq!(decoded("\"\u{5c}uD83D\u{5c}uDE00\""), "\u{1f600}");
  assert_eq!(decoded("\"\u{5c}ud83d\u{5c}ude00\""), "\u{1f600}");
  // The braced form, at both ends of its range.
  assert_eq!(decoded(r#""\u{1F600}""#), "\u{1f600}");
  assert_eq!(decoded(r#""\u{41}\u{0}""#), "A\u{0}");
  assert_eq!(decoded(r#""\u{10FFFF}""#), "\u{10ffff}");
  // Characters the spelling carries as themselves pass through untouched.
  assert_eq!(decoded("\"caf\u{e9} \u{2603}\""), "caf\u{e9} \u{2603}");
}

/// Draft §2.9.4's worked example, and graphql-js's dedent cases.
#[test]
fn a_block_string_is_block_string_value() {
  const CASES: &[(&str, &str)] = &[
    (
      "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n  \"\"\"",
      "Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    (
      "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\"\"\"",
      "Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    // Trailing spaces on a line are content.
    (
      "\"\"\"\n    Hello,     \n      World!   \n\n    Yours,     \n      GraphQL.   \n\"\"\"",
      "Hello,     \n  World!   \n\nYours,     \n  GraphQL.   ",
    ),
    // Leading and trailing blank lines go, however many.
    (
      "\"\"\"\n\n\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\n\n\"\"\"",
      "Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    // The first line of the raw split keeps its indentation and sets none.
    (
      "\"\"\"    Hello,\n      World!\n\n    Yours,\n      GraphQL.\"\"\"",
      "    Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    ("\"\"\"\"\"\"", ""),
    ("\"\"\"hello\"\"\"", "hello"),
    // Nothing but blank lines is the empty string.
    ("\"\"\"   \n\t\n  \"\"\"", ""),
    // A tab is one indentation character, as a space is.
    ("\"\"\"\n\tline1\n\t\tline2\n\"\"\"", "line1\n\tline2"),
    // A blank line shorter than the common indent loses all of it; a longer one keeps the excess.
    ("\"\"\"\n    a\n  \n    b\n\"\"\"", "a\n\nb"),
    ("\"\"\"\n  a\n      \n  b\n\"\"\"", "a\n    \nb"),
    // Every terminator becomes one line feed.
    ("\"\"\"a\r\nb\rc\nd\"\"\"", "a\nb\nc\nd"),
    // `\"""` is a block string's only escape, and a backslash before anything else is a backslash.
    ("\"\"\"x \\\"\"\" y\"\"\"", "x \"\"\" y"),
    (
      "\"\"\"\n  \\\"\"\"quoted\\\"\"\"\n\"\"\"",
      "\"\"\"quoted\"\"\"",
    ),
    ("\"\"\"a \\n b\"\"\"", "a \\n b"),
  ];

  for (spelling, value) in CASES {
    assert_eq!(decoded(spelling), *value, "`{spelling}`");
    assert_eq!(
      string_value_len(spelling.as_bytes()),
      Ok(value.len()),
      "`{spelling}`"
    );
  }
}

/// Every literal the lexer accepts decodes the way its `Cow` conversion cooks it.
///
/// The conversions read the line facts the lexer measured, and this decoder reads nothing but the
/// spelling, so the two reach the value by different routes and agree only if both are right.
#[test]
fn the_byte_decoder_agrees_with_the_cow_conversions() {
  const CORPUS: &[&str] = &[
    r#""""#,
    r#""A""#,
    r#""a\"b""#,
    r#""a\\b""#,
    r#""a\/b""#,
    r#""\b\f\n\r\t""#,
    // The fixed-width `\u` escapes, with `\u{5c}` spelling their backslash.
    "\"\u{5c}u0041\"",
    "\"\u{5c}u00e9\"",
    "\"\u{5c}ud83d\u{5c}ude00\"",
    "\"\u{e9}\u{1f600}\"",
    r#""\u{1F600}""#,
    r#""caf\u{e9} \u{1f600}""#,
    "\"caf\u{e9}\"",
    r#""""""""#,
    r#""""plain""""#,
    "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n    \"\"\"",
    "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\"\"\"",
    "\"\"\"\n  a\n    \n  b\n\"\"\"",
    "\"\"\"\n    a\n  \n    b\n\"\"\"",
    "\"\"\"a\n\"\"\"",
    "\"\"\"\na\n\"\"\"",
    "\"\"\"\ncontent\n\n\"\"\"",
    "\"\"\"a\n  b\n    c\n\"\"\"",
    "\"\"\"\n\t\ta\n\t\t\tb\n\"\"\"",
    "\"\"\"  keeps first line indent\n  and dedents this\n\"\"\"",
    "\"\"\"\r\n  a\r\n  b\r\n\"\"\"",
    "\"\"\"a\r\nb\rc\n\"\"\"",
    "\"\"\"\n   \n  \n\"\"\"",
    r#""""a\"""b""""#,
    r#""""a\nb""""#,
  ];

  for literal in CORPUS {
    assert_eq!(decoded(literal), cooked(literal), "`{literal}`");
  }
}

/// The pieces concatenate to the value, and none of them splits a character.
#[test]
fn a_piece_is_whole_text() {
  for spelling in [
    "\"caf\u{e9}\\n\u{1f600}\\u{1F600}x\"",
    "\"\"\"\n  \u{e9}\u{e9}\n  \\\"\"\"\u{2603}\n\"\"\"",
  ] {
    let mut pieces: Vec<String> = Vec::new();
    string_value_pieces(spelling.as_bytes(), |piece| pieces.push(piece.into()))
      .expect("the spelling denotes a value");
    assert!(pieces.len() > 1, "`{spelling}` came back as one piece");
    assert_eq!(pieces.concat(), decoded(spelling));
  }
}

/// A refusal says which of the three it is, and where an escape went wrong.
#[test]
fn a_spelling_with_no_value_is_refused_and_appends_nothing() {
  // `\x5c` is the backslash, spelled so that no escape in the table is read before the decoder
  // reads it.
  const INVALID_ESCAPES: &[(&[u8], usize)] = &[
    (b"\"\x5cx\"", 1),
    (b"\"ab\x5cq\"", 3),
    // Lone surrogates, either half, in either spelling.
    (b"\"\x5cuD800\"", 1),
    (b"\"\x5cuDC00\"", 1),
    (b"\"\x5cuD800\x5cu0041\"", 1),
    (b"\"\x5cu{D800}\"", 1),
    // Past U+10FFFF, no digits, too many digits, too few, and no closing brace.
    (b"\"\x5cu{110000}\"", 1),
    (b"\"\x5cu{}\"", 1),
    (b"\"\x5cu{1234567}\"", 1),
    (b"\"\x5cu12\"", 1),
    (b"\"\x5cu{12\"", 1),
    // A backslash with nothing after it.
    (b"\"\x5c\"", 1),
  ];

  for &(spelling, offset) in INVALID_ESCAPES {
    assert_eq!(
      refused(spelling),
      StringValueError::InvalidEscape(offset),
      "{spelling:?}"
    );
  }

  for spelling in [&b"abc"[..], b"\"", b"\"\"\"", b"\"\"\"abc\"", b""] {
    assert_eq!(
      refused(spelling),
      StringValueError::Unquoted,
      "{spelling:?}"
    );
  }
}

/// Bytes that are no text are refused, which is the one refusal a lexed spelling can reach.
///
/// The byte lexer passes a literal's content through unread — `LitStr::try_from(&[u8])` accepts
/// both spellings below — so the decoder is where a non-UTF-8 value is first noticed.
#[test]
fn a_value_that_is_not_utf8_is_refused() {
  for spelling in [&b"\"\xff\""[..], b"\"\"\"\n  \xc3\x28\n\"\"\""] {
    assert!(
      matches!(
        LitStr::try_from(spelling),
        Ok(LitStr::Inline(LitInlineStr::Plain(_)) | LitStr::Block(LitBlockStr::Plain(_)))
          | Ok(LitStr::Block(LitBlockStr::Complex(_)))
      ),
      "the byte lexer is expected to accept {spelling:?}"
    );
    assert_eq!(refused(spelling), StringValueError::NotUtf8, "{spelling:?}");
  }
}

/// `decode_string_value` appends, and reserves the decoded length rather than the spelling's.
#[test]
fn decoding_appends_to_what_is_there() {
  let mut out = String::from("> ");
  decode_string_value(br#""a\u{1F600}b""#, &mut out).expect("a value");
  decode_string_value(b"\"\"\"\n  c\n\"\"\"", &mut out).expect("a value");
  assert_eq!(out, "> a\u{1f600}bc");
}
