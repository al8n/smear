//! A lexical refusal answers the diagnostic contract in GraphQLx as it does in GraphQL: its kind's
//! `smear::syntax` code, a sentence with no position in it, and its own span as the primary
//! location. GraphQL's twin carries the full table; this one holds the arms GraphQLx lexes on its
//! own — the radix literals — beside a sample of the shared ones.

use std::string::{String, ToString};

use tokora::{
  Lexer as _,
  diagnostic::{Diagnose, Location},
};

use crate::{SyntaxErrorKind, graphqlx::syntactic::SyntacticLexer, limits::SyntacticLimits};

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
    ("?", SyntaxErrorKind::UnknownCharacter, "unknown character"),
    (
      "..x",
      SyntaxErrorKind::UnterminatedSpread,
      "unterminated spread operator, expected `...`",
    ),
    (
      "1e",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected `+`, `-` or an exponent digit",
    ),
    (
      "0xZZ",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, unexpected character after a hexadecimal integer",
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
      r#""a\qb""#,
      SyntaxErrorKind::UnknownEscape,
      "unknown escape sequence",
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

/// The nesting ceiling counts `<` as well as the three pairs GraphQL has, and its trip reports the
/// resource-limit kind.
#[test]
fn a_tripped_ceiling_reports_a_resource_limit() {
  let src = "<a::b>";
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

/// The one error a whole-slice door refuses `src` with.
macro_rules! door_refusal {
  ($door:expr, $src:expr) => {{
    let src = $src;
    let mut errors = $door(src).expect_err("the slice is not one token of the door's kind");
    assert_eq!(errors.len(), 1, "{src:?}");
    errors.remove(0)
  }};
}

/// GraphQLx's whole-slice doors — `identifier` and the `TryFrom<&str>` of `LitInt` and `LitFloat` —
/// refuse a slice that is not exactly one token of the kind they asked for as `NotOneToken`, under
/// `smear::syntax::unexpected-token` and with no help line, as GraphQL's do.
#[test]
fn a_whole_slice_door_refuses_what_is_not_exactly_one_token_of_its_kind() {
  use tokora::SimpleSpan;

  use crate::graphqlx::{LitFloat, LitInt, error::LexerErrorData, identifier};

  for (src, error, message) in [
    ("1", door_refusal!(identifier, "1"), "not exactly one name"),
    (
      "a b",
      door_refusal!(identifier, "a b"),
      "not exactly one name",
    ),
    (
      " a",
      door_refusal!(identifier, " a"),
      "not exactly one name",
    ),
    (
      "1.0",
      door_refusal!(LitInt::try_from, "1.0"),
      "not exactly one integer",
    ),
    (
      "1",
      door_refusal!(LitFloat::try_from, "1"),
      "not exactly one float",
    ),
  ] {
    assert!(
      matches!(error.data(), LexerErrorData::NotOneToken(_)),
      "{src:?}: {:?}",
      error.data()
    );
    assert_eq!(error.span(), SimpleSpan::new(0, src.len()), "{src:?}");
    assert_eq!(error.help(), None, "{src:?}");
    assert_refusal!(error, src, SyntaxErrorKind::UnexpectedToken, message);
  }
}
