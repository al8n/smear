//! A lexical refusal answers the diagnostic contract: its kind's `smear::syntax` code, a sentence
//! with no position in it, and its own span as the primary location.

use std::string::{String, ToString};

use tokora::{
  Lexer as _,
  diagnostic::{Diagnose, Location},
};

use crate::{SyntaxErrorKind, graphql::syntactic::SyntacticLexer, limits::SyntacticLimits};

/// Whether `text` names a source position: a `[0-9]+..[0-9]+` range, or a word one is spelled
/// with.
fn names_a_position(text: &str) -> bool {
  let bytes = text.as_bytes();
  let range = bytes.windows(2).enumerate().any(|(i, pair)| {
    pair == b".."
      && i > 0
      && bytes[i - 1].is_ascii_digit()
      && bytes.get(i + 2).is_some_and(u8::is_ascii_digit)
  });
  range
    || ["byte", "offset", "position", " at "]
      .iter()
      .any(|word| text.contains(word))
}

/// The first error a lexer meets, taken out of its batch.
macro_rules! first_error {
  ($lexer:expr, $src:expr) => {{
    let mut lexer = $lexer;
    let src = $src;
    loop {
      match lexer.lex() {
        Some(Err(mut errors)) => break errors.remove(0),
        Some(Ok(_)) => continue,
        None => panic!("{src:?} lexed without an error"),
      }
    }
  }};
}

/// Asserts everything the contract reads off one lexical refusal.
macro_rules! assert_refusal {
  ($error:expr, $src:expr, $kind:expr, $message:expr) => {{
    let error = $error;
    let src = $src;
    let kind: SyntaxErrorKind = $kind;
    let message: &str = $message;
    let rendered: String = error.to_string();
    assert_eq!(error.kind(), kind, "{src:?}");
    assert_eq!(error.code(), kind.code(), "{src:?}");
    assert_eq!(rendered, message, "{src:?}");
    assert!(!names_a_position(&rendered), "{src:?}: {rendered:?}");
    assert_eq!(error.severity(), kind.severity());
    assert_eq!(error.primary(), Location::new(0, error.span()), "{src:?}");
    assert_eq!(error.primary_label(), Some(kind.message()));
    assert_eq!(error.help(), kind.help());
    assert_eq!(error.labels(), 0);
    assert_eq!(error.path_segments(), 0);
  }};
}

#[test]
fn every_lexical_refusal_renders_its_own_sentence_under_its_own_code() {
  const CASES: &[(&str, SyntaxErrorKind, &str)] = &[
    (
      "+1",
      SyntaxErrorKind::UnexpectedCharacter,
      "unexpected character",
    ),
    ("?", SyntaxErrorKind::UnknownCharacter, "unknown character"),
    (
      "..x",
      SyntaxErrorKind::UnterminatedSpread,
      "unterminated spread operator, expected `...`",
    ),
    (
      "007",
      SyntaxErrorKind::LeadingZero,
      "leading zero in an integer",
    ),
    (
      "00.5",
      SyntaxErrorKind::LeadingZero,
      "leading zero in a float",
    ),
    (
      "1e",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected `+`, `-` or an exponent digit",
    ),
    (
      "1.",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a fractional digit",
    ),
    (
      ".5",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, a float must have an integer part",
    ),
    (
      "123abc",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, unexpected character after an integer",
    ),
    (
      "1.5x",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, unexpected character after a float",
    ),
    (
      "\"unterminated",
      SyntaxErrorKind::UnterminatedString,
      "unterminated string, expected a closing `\"`",
    ),
    (
      "\"\"\"oops",
      SyntaxErrorKind::UnterminatedString,
      "unterminated block string, expected a closing `\"\"\"`",
    ),
    (
      "\"\nhello\n\"",
      SyntaxErrorKind::LineTerminatorInString,
      "line feed in a single-line string",
    ),
    (
      r#""a\qb""#,
      SyntaxErrorKind::UnknownEscape,
      "unknown escape sequence",
    ),
    (
      "\"abc\\",
      SyntaxErrorKind::IncompleteEscape,
      "incomplete escape sequence, nothing follows the `\\`",
    ),
    (
      r#""\u12zz""#,
      SyntaxErrorKind::InvalidUnicodeEscape,
      "invalid Unicode escape, expected four hex digits after `\\u`",
    ),
    (
      r#""\u222"#,
      SyntaxErrorKind::InvalidUnicodeEscape,
      "incomplete Unicode escape, expected four hex digits after `\\u`",
    ),
    (
      r#""\uD83D""#,
      SyntaxErrorKind::InvalidUnicodeScalar,
      "unpaired high surrogate in a Unicode escape",
    ),
    (
      r#""\u{D800}""#,
      SyntaxErrorKind::InvalidUnicodeScalar,
      "escape names a surrogate, not a Unicode scalar value",
    ),
    (
      r#""\u{110000}""#,
      SyntaxErrorKind::InvalidUnicodeScalar,
      "escape names a value past `10FFFF`, not a Unicode scalar value",
    ),
  ];

  for (src, kind, message) in CASES {
    let error = first_error!(SyntacticLexer::<str>::new(src), src);
    assert_refusal!(error, src, *kind, *message);
  }
}

#[test]
fn a_byte_source_refuses_an_ill_formed_string_as_invalid_utf8() {
  let src: &[u8] = b"\"a\xffb\"";
  let error = first_error!(SyntacticLexer::<[u8]>::new(src), src);
  assert_refusal!(
    error,
    src,
    SyntaxErrorKind::InvalidUtf8,
    "invalid UTF-8 in a string"
  );
}

/// A tripped nesting ceiling is a `State` refusal, and it reports the resource-limit kind: the
/// lexer stopped, the document did not end.
#[test]
fn a_tripped_ceiling_reports_a_resource_limit() {
  let src = "(x)";
  let error = first_error!(
    SyntacticLexer::<str>::with_state(src, SyntacticLimits::with_max_nesting_depth(0)),
    src
  );
  assert_refusal!(
    error,
    src,
    SyntaxErrorKind::ResourceLimit,
    "a resource limit stopped the parse"
  );
}
