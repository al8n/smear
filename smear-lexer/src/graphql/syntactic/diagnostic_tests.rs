//! A lexical refusal answers the diagnostic contract: its kind's `smear::syntax` code, a sentence
//! with no position in it, and its own span as the primary location.

use std::string::{String, ToString};

use tokora::{
  Lexer as _, SimpleSpan,
  diagnostic::{Diagnose, Location},
  state::RecursionLimitExceeded,
};

use crate::{
  SyntaxErrorKind,
  error::StringError,
  graphql::{
    error::{LexerError, LexerErrorData},
    syntactic::SyntacticLexer,
  },
  limits::SyntacticLimits,
};

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
      "1.)",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, expected a fractional digit",
    ),
    (
      "1e+)",
      SyntaxErrorKind::InvalidNumber,
      "invalid number, expected an exponent digit",
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
    // A single-line string ends at its first line terminator, unterminated there.
    (
      "\"\nhello\n\"",
      SyntaxErrorKind::UnterminatedString,
      "unterminated string, expected a closing `\"`",
    ),
    (
      "\"a\rb\"",
      SyntaxErrorKind::UnterminatedString,
      "unterminated string, expected a closing `\"`",
    ),
    (
      "\"a\r\nb\"",
      SyntaxErrorKind::UnterminatedString,
      "unterminated string, expected a closing `\"`",
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
      r#""\uDE00""#,
      SyntaxErrorKind::InvalidUnicodeScalar,
      "unpaired low surrogate in a Unicode escape",
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
    (
      r#""\u{12""#,
      SyntaxErrorKind::InvalidUnicodeEscape,
      "unclosed Unicode escape, expected `}`",
    ),
    (
      r#""\u{G}""#,
      SyntaxErrorKind::InvalidUnicodeEscape,
      "invalid Unicode escape, expected hex digits between `{` and `}`",
    ),
    (
      r#""\u{}""#,
      SyntaxErrorKind::InvalidUnicodeEscape,
      "empty Unicode escape, expected a hex digit between `{` and `}`",
    ),
    (
      r#""\u{1234567}""#,
      SyntaxErrorKind::InvalidUnicodeEscape,
      "invalid Unicode escape, more than 6 hex digits",
    ),
  ];

  for (src, kind, message) in CASES {
    let error = first_error!(SyntacticLexer::<str>::new(src), src);
    assert_refusal!(error, src, *kind, *message);
  }
}

/// The lexer reports no line terminator inside a string, which ends a single-line string instead,
/// so the sentences of an error a caller builds with one are read at the value.
#[test]
fn a_line_terminator_in_a_string_renders_its_sentence_at_the_value() {
  for (error, message) in [
    (
      StringError::unexpected_new_line('\n', 2),
      "line feed in a single-line string",
    ),
    (
      StringError::unexpected_carriage_return('\r', 2),
      "carriage return in a single-line string",
    ),
    (
      StringError::unexpected_carriage_return_new_line(SimpleSpan::new(2, 4)),
      "line break in a single-line string",
    ),
  ] {
    let error = LexerError::<char, RecursionLimitExceeded>::new(
      SimpleSpan::new(0, 4),
      LexerErrorData::String(error.into()),
    );
    assert_refusal!(
      error,
      message,
      SyntaxErrorKind::LineTerminatorInString,
      message
    );
  }
}

/// The entry that counts what a string literal's record did not keep closes the record and is never
/// its first, so its sentence is read at the value, in the singular and in the plural.
#[test]
fn the_count_of_what_a_record_did_not_keep_renders_its_sentence_at_the_value() {
  for (count, message) in [
    (
      1,
      "1 more error in this string literal was found and not kept",
    ),
    (
      984,
      "984 more errors in this string literal were found and not kept",
    ),
  ] {
    let error = LexerError::<char, RecursionLimitExceeded>::new(
      SimpleSpan::new(0, 4),
      LexerErrorData::String(StringError::Omitted(count).into()),
    );
    assert_refusal!(error, message, SyntaxErrorKind::InvalidSyntax, message);
  }
}

/// The error that counts what a comment's refusal did not keep is never its first either, so its
/// sentence is read at the value too.
#[test]
fn the_count_of_what_a_comments_refusal_did_not_keep_renders_its_sentence_at_the_value() {
  for (count, message) in [
    (1, "1 more error in this comment was found and not kept"),
    (
      984,
      "984 more errors in this comment were found and not kept",
    ),
  ] {
    let error = LexerError::<u8, RecursionLimitExceeded>::new(
      SimpleSpan::new(17, 1001),
      LexerErrorData::Omitted(count),
    );
    assert_refusal!(error, message, SyntaxErrorKind::InvalidSyntax, message);
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

/// A byte source refuses a braced escape whose content is not hex as a text source does, but by a
/// route of its own, which is what this case pins. A text source's `"\u{G}"`, in the table above,
/// is taken by the rule for `\u{` and one character. A byte that begins no UTF-8 scalar matches
/// neither that rule nor the hex one, so the bare `\u{` rule takes the escape, and its search for
/// the closing brace goes past that byte.
#[test]
fn a_byte_source_refuses_a_braced_escape_that_is_not_hex() {
  let src: &[u8] = b"\"\\u{\xff}\"";
  let error = first_error!(SyntacticLexer::<[u8]>::new(src), src);
  assert_refusal!(
    error,
    src,
    SyntaxErrorKind::InvalidUnicodeEscape,
    "invalid Unicode escape, expected hex digits between `{` and `}`"
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

/// The one error a whole-slice door refuses `src` with.
macro_rules! door_refusal {
  ($door:expr, $src:expr) => {{
    let src = $src;
    let mut errors = $door(src).expect_err("the slice is not one token of the door's kind");
    assert_eq!(errors.len(), 1, "{src:?}");
    errors.remove(0)
  }};
}

/// A whole-slice door refuses a slice that is not exactly one token of the kind it asked for — a
/// token of another kind, a second token, or trivia around it — as `NotOneToken`, under
/// `smear::syntax::unexpected-token` and with no help line. The scanner's `UnexpectedLexeme`, whose
/// code and help are about a sign character, is not what a door answers.
#[test]
fn a_whole_slice_door_refuses_what_is_not_exactly_one_token_of_its_kind() {
  use tokora::SimpleSpan;

  use crate::graphql::{error::LexerErrorData, float_literal, identifier, int_literal};

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
      door_refusal!(int_literal, "1.0"),
      "not exactly one integer",
    ),
    (
      "1",
      door_refusal!(float_literal, "1"),
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

/// Every token kind renders as a diagnostic names what it found: punctuation in backticks, and
/// every class, trivia included, as a bare noun. Both tables are copied from the impls row for row,
/// so rewording a kind is a change this test has to be shown.
#[test]
fn every_token_kind_renders_as_a_diagnostic_names_it() {
  use crate::graphql::{
    lossless::LosslessTokenKind as Lossless, syntactic::SyntacticTokenKind as Syntactic,
  };

  for (kind, text) in [
    (Syntactic::Identifier, "name"),
    (Syntactic::Int, "integer"),
    (Syntactic::Float, "float"),
    (Syntactic::InlineString, "string"),
    (Syntactic::BlockString, "block string"),
    (Syntactic::Dollar, "`$`"),
    (Syntactic::LParen, "`(`"),
    (Syntactic::RParen, "`)`"),
    (Syntactic::Spread, "`...`"),
    (Syntactic::Colon, "`:`"),
    (Syntactic::Equal, "`=`"),
    (Syntactic::At, "`@`"),
    (Syntactic::LBracket, "`[`"),
    (Syntactic::RBracket, "`]`"),
    (Syntactic::LBrace, "`{`"),
    (Syntactic::RBrace, "`}`"),
    (Syntactic::Pipe, "`|`"),
    (Syntactic::Bang, "`!`"),
    (Syntactic::Ampersand, "`&`"),
  ] {
    assert_eq!(kind.to_string(), text, "{kind:?}");
  }

  for (kind, text) in [
    (Lossless::Ampersand, "`&`"),
    (Lossless::At, "`@`"),
    (Lossless::Bom, "BOM"),
    (Lossless::RBrace, "`}`"),
    (Lossless::LBrace, "`{`"),
    (Lossless::RBracket, "`]`"),
    (Lossless::LBracket, "`[`"),
    (Lossless::RParen, "`)`"),
    (Lossless::LParen, "`(`"),
    (Lossless::Bang, "`!`"),
    (Lossless::Colon, "`:`"),
    (Lossless::Dollar, "`$`"),
    (Lossless::Equal, "`=`"),
    (Lossless::Space, "space"),
    (Lossless::Tab, "tab"),
    (Lossless::CarriageReturn, "carriage return"),
    (Lossless::Newline, "line feed"),
    (Lossless::CarriageReturnAndNewline, "line break"),
    (Lossless::Comma, "`,`"),
    (Lossless::Pipe, "`|`"),
    (Lossless::Spread, "`...`"),
    (Lossless::Float, "float"),
    (Lossless::Identifier, "name"),
    (Lossless::Int, "integer"),
    (Lossless::InlineString, "string"),
    (Lossless::BlockString, "block string"),
    (Lossless::Comment, "comment"),
  ] {
    assert_eq!(kind.to_string(), text, "{kind:?}");
  }
}

/// The refusals no lexing door reaches, read at the value: the two hints no scanner raises, an
/// integer the input ends inside (no scanner builds that one either), a source that is not UTF-8,
/// the free-form notes, an empty string batch, and the unterminated delimiter no lexed string
/// leaves undecided. The unopened spelling is the one built by a real door, `LitStr::try_from`,
/// which re-reads a caller's spelling rather than lexing a document. Each answers the contract
/// under the kind its data names.
#[test]
fn the_refusals_no_lexing_door_reaches_render_their_sentences() {
  use std::borrow::Cow;

  use tokora::{SimpleSpan, error::UnexpectedEnd};

  use crate::{
    LitStr,
    error::{StringError, StringErrors},
    graphql::error::{DecimalError, FloatError, LexerError, LexerErrorData},
    hints::{DecimalHint, ExponentHint, FloatHint, LitStrDelimiterHint},
  };

  /// A refusal built from its data, at a span no lexer chose.
  fn built(data: LexerErrorData<char, ()>) -> LexerError<char, ()> {
    LexerError::new(SimpleSpan::new(0, 1), data)
  }

  let not_utf8 = core::str::from_utf8(&[u8::MAX]).expect_err("0xFF begins no UTF-8 sequence");
  let unopened = LitStr::<&str>::try_from("value").expect_err("the spelling opens with no quote");
  let note: StringError = StringError::Other(Cow::Borrowed("a free-form string note"));
  let undecided: StringError = StringError::Unterminated(UnexpectedEnd::new(
    0,
    LitStrDelimiterHint::QuoteOrTripleQuote,
  ));

  for (what, error, kind, message) in [
    (
      "an integer the input ends inside",
      built(LexerErrorData::Int(DecimalError::UnexpectedEnd(
        UnexpectedEnd::new(0, DecimalHint::Digit),
      ))),
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a digit",
    ),
    (
      "a float the input ends inside before a digit",
      built(LexerErrorData::Float(FloatError::UnexpectedEnd(
        UnexpectedEnd::new(0, FloatHint::Digit),
      ))),
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected a digit",
    ),
    (
      "a float the input ends inside before its exponent marker",
      built(LexerErrorData::Float(FloatError::UnexpectedEnd(
        UnexpectedEnd::new(0, FloatHint::Exponent(ExponentHint::Identifier)),
      ))),
      SyntaxErrorKind::IncompleteNumber,
      "incomplete number, expected `e` or `E`",
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
    (
      "a free-form string note",
      built(LexerErrorData::String(note.into())),
      SyntaxErrorKind::InvalidSyntax,
      "a free-form string note",
    ),
    (
      "a string the input ends inside, delimiter undecided",
      built(LexerErrorData::String(undecided.into())),
      SyntaxErrorKind::UnterminatedString,
      "unterminated string",
    ),
    (
      "a spelling with no opening quote",
      built(LexerErrorData::String(unopened)),
      SyntaxErrorKind::UnopenedString,
      "a string value must open with `\"` or `\"\"\"`",
    ),
  ] {
    assert_refusal!(error, what, kind, message);
  }
}
