//! A lexical refusal answers the diagnostic contract in GraphQLx as it does in GraphQL: its kind's
//! `smear::syntax` code, a sentence with no position in it, and its own span as the primary
//! location. GraphQL's twin carries the full table; this one holds the arms GraphQLx lexes on its
//! own — its number errors, the radix literals and hexadecimal floats among them — beside a sample
//! of the shared ones.

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
    assert!(error.label(0).is_none());
    assert_eq!(error.path_segments(), 0);
    assert!(error.path_segment(0).is_none());
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
      "0x",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a hex digit",
    ),
    (
      "0o8",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, unexpected character after an octal integer",
    ),
    (
      "0o",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected an octal digit",
    ),
    (
      "0b2",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, unexpected character after a binary integer",
    ),
    (
      "0b",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a binary digit",
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
      ".5",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, a float must have an integer part",
    ),
    (
      "1.)",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, expected a fractional digit",
    ),
    (
      "1e+",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected an exponent digit",
    ),
    (
      "0x1p1z",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, unexpected character after a hexadecimal float",
    ),
    (
      "0x1.)",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, expected a fractional hex digit",
    ),
    (
      "0x1.",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a fractional hex digit",
    ),
    (
      "0x1p",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected `+`, `-` or an exponent digit",
    ),
    (
      "0x1p+",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected an exponent digit",
    ),
    (
      ".8p1",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, a hexadecimal float must have an integer part",
    ),
    (
      "0x1.8",
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, a hexadecimal float must have a `p` exponent",
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

/// Every token kind renders as a diagnostic names what it found, as GraphQL's twin pins it, over
/// this dialect's own vocabulary: the angle pair, `=>`, `*`, `+`, `-` and `::` beside GraphQL's
/// punctuation. Both tables are copied from the impls row for row.
#[test]
fn every_token_kind_renders_as_a_diagnostic_names_it() {
  use crate::graphqlx::{
    lossless::LosslessTokenKind as Lossless, syntactic::SyntacticTokenKind as Syntactic,
  };

  for (kind, text) in [
    (Syntactic::Identifier, "name"),
    (Syntactic::Int, "integer"),
    (Syntactic::Float, "float"),
    (Syntactic::InlineString, "string"),
    (Syntactic::BlockString, "block string"),
    (Syntactic::Dollar, "`$`"),
    (Syntactic::FatArrow, "`=>`"),
    (Syntactic::LAngle, "`<`"),
    (Syntactic::RAngle, "`>`"),
    (Syntactic::LParen, "`(`"),
    (Syntactic::RParen, "`)`"),
    (Syntactic::Spread, "`...`"),
    (Syntactic::Colon, "`:`"),
    (Syntactic::Equal, "`=`"),
    (Syntactic::Asterisk, "`*`"),
    (Syntactic::At, "`@`"),
    (Syntactic::LBracket, "`[`"),
    (Syntactic::RBracket, "`]`"),
    (Syntactic::LBrace, "`{`"),
    (Syntactic::RBrace, "`}`"),
    (Syntactic::Pipe, "`|`"),
    (Syntactic::Bang, "`!`"),
    (Syntactic::Ampersand, "`&`"),
    (Syntactic::Plus, "`+`"),
    (Syntactic::Minus, "`-`"),
    (Syntactic::PathSeparator, "`::`"),
  ] {
    assert_eq!(kind.to_string(), text, "{kind:?}");
  }

  for (kind, text) in [
    (Lossless::Asterisk, "`*`"),
    (Lossless::At, "`@`"),
    (Lossless::Bom, "BOM"),
    (Lossless::Dollar, "`$`"),
    (Lossless::FatArrow, "`=>`"),
    (Lossless::LAngle, "`<`"),
    (Lossless::RAngle, "`>`"),
    (Lossless::LParen, "`(`"),
    (Lossless::RParen, "`)`"),
    (Lossless::Spread, "`...`"),
    (Lossless::Colon, "`:`"),
    (Lossless::Equal, "`=`"),
    (Lossless::LBracket, "`[`"),
    (Lossless::RBracket, "`]`"),
    (Lossless::LBrace, "`{`"),
    (Lossless::RBrace, "`}`"),
    (Lossless::Pipe, "`|`"),
    (Lossless::Bang, "`!`"),
    (Lossless::Ampersand, "`&`"),
    (Lossless::Plus, "`+`"),
    (Lossless::Minus, "`-`"),
    (Lossless::PathSeparator, "`::`"),
    (Lossless::Comma, "`,`"),
    (Lossless::Space, "space"),
    (Lossless::Tab, "tab"),
    (Lossless::Newline, "line feed"),
    (Lossless::CarriageReturn, "carriage return"),
    (Lossless::CarriageReturnAndNewline, "line break"),
    (Lossless::Comment, "comment"),
    (Lossless::Identifier, "name"),
    (Lossless::Float, "float"),
    (Lossless::Int, "integer"),
    (Lossless::InlineString, "string"),
    (Lossless::BlockString, "block string"),
  ] {
    assert_eq!(kind.to_string(), text, "{kind:?}");
  }
}

/// The refusals no lexing door in this dialect reaches, read at the value: the two
/// hexadecimal-float hints no scanner raises, an integer the input ends inside, the sign character
/// this scanner lexes as an operator instead, an end of input it never raises, a source that is not
/// UTF-8, a free-form note and an empty string batch. Each answers the contract under the kind its
/// data names.
#[test]
fn the_refusals_no_lexing_door_reaches_render_their_sentences() {
  use tokora::{SimpleSpan, error::UnexpectedEnd};

  use crate::{
    error::StringErrors,
    graphqlx::error::{DecimalError, HexFloatError, LexerError, LexerErrorData},
    hints::{DecimalHint, HexExponentHint, HexFloatHint},
  };

  /// A refusal built from its data, at a span no lexer chose.
  fn built(data: LexerErrorData<char, ()>) -> LexerError<char, ()> {
    LexerError::new(SimpleSpan::new(0, 1), data)
  }

  let not_utf8 = core::str::from_utf8(&[u8::MAX]).expect_err("0xFF begins no UTF-8 sequence");

  for (what, error, kind, message) in [
    (
      "an integer the input ends inside",
      built(LexerErrorData::Decimal(DecimalError::UnexpectedEnd(
        UnexpectedEnd::new(0, DecimalHint::Digit),
      ))),
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a digit",
    ),
    (
      "a hexadecimal float the input ends inside before a digit",
      built(LexerErrorData::HexFloat(HexFloatError::UnexpectedEnd(
        UnexpectedEnd::new(0, HexFloatHint::Digit),
      ))),
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a hex digit",
    ),
    (
      "a hexadecimal float the input ends inside before its exponent marker",
      built(LexerErrorData::HexFloat(HexFloatError::UnexpectedEnd(
        UnexpectedEnd::new(0, HexFloatHint::Exponent(HexExponentHint::Identifier)),
      ))),
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected `p` or `P`",
    ),
    (
      "a sign that begins no token",
      built(LexerErrorData::unexpected_char('+', 0)),
      SyntaxErrorKind::UnexpectedCharacter,
      "unexpected character",
    ),
    (
      "an end of input the scanner never raises",
      built(LexerErrorData::UnexpectedEndOfInput),
      SyntaxErrorKind::UnexpectedEndOfInput,
      "unexpected end of input",
    ),
    (
      "a source that is not UTF-8",
      built(LexerErrorData::InvalidUtf8(not_utf8)),
      SyntaxErrorKind::InvalidUtf8,
      "invalid UTF-8",
    ),
    (
      "a free-form note",
      built(LexerErrorData::other("a free-form note")),
      SyntaxErrorKind::InvalidSyntax,
      "a free-form note",
    ),
    (
      "an empty string batch",
      built(LexerErrorData::String(StringErrors::default())),
      SyntaxErrorKind::InvalidSyntax,
      "invalid syntax",
    ),
  ] {
    assert_refusal!(error, what, kind, message);
  }
}
