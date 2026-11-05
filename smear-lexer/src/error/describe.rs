//! The phrases a lexer error's message is assembled from, shared by both dialects.
//!
//! Every phrase follows the diagnostic house style: lowercase, no trailing period, spellings in
//! backticks, and never a position — the position travels in the diagnostic's primary location.
//! The hint enums carry a `Display` of their own; it predates that style (`'+', '-' or digit`), so
//! these mappings are what a message reads rather than that.

use core::fmt;

use super::{
  BracedUnicodeEscapeError, FixedUnicodeEscapeError, InvalidUnicodeScalarKind, StringError,
  SyntaxErrorKind, UnicodeError,
};
use crate::hints::{
  ExponentHint, FloatHint, LineTerminatorHint, LitStrDelimiterHint, UnpairedSurrogateHint,
};
#[cfg(feature = "graphqlx")]
use crate::hints::{HexExponentHint, HexFloatHint};

/// What a float was expected to continue with, as the object of "expected …".
pub(crate) const fn float_hint(hint: &FloatHint) -> &'static str {
  match hint {
    FloatHint::Fractional => "a fractional digit",
    FloatHint::Exponent(exponent) => exponent_hint(exponent),
    FloatHint::Digit => "a digit",
  }
}

/// What a decimal exponent was expected to continue with.
const fn exponent_hint(hint: &ExponentHint) -> &'static str {
  match hint {
    ExponentHint::Digit => "an exponent digit",
    ExponentHint::SignOrDigit => "`+`, `-` or an exponent digit",
    ExponentHint::Identifier => "`e` or `E`",
  }
}

/// What a hexadecimal float was expected to continue with.
#[cfg(feature = "graphqlx")]
pub(crate) const fn hex_float_hint(hint: &HexFloatHint) -> &'static str {
  match hint {
    HexFloatHint::Fractional => "a fractional hex digit",
    HexFloatHint::Exponent(exponent) => hex_exponent_hint(exponent),
    HexFloatHint::Digit => "a hex digit",
  }
}

/// What a hexadecimal float's binary exponent was expected to continue with.
#[cfg(feature = "graphqlx")]
const fn hex_exponent_hint(hint: &HexExponentHint) -> &'static str {
  match hint {
    HexExponentHint::Digit => "an exponent digit",
    HexExponentHint::SignOrDigit => "`+`, `-` or an exponent digit",
    HexExponentHint::Identifier => "`p` or `P`",
  }
}

/// A string literal's refusal, as the sentence its lexer error renders.
pub(crate) fn string_error<Char>(
  error: &StringError<Char>,
  f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
  match error {
    StringError::UnsupportedCharacter(_) => f.write_str("invalid UTF-8 in a string"),
    StringError::UnexpectedLineTerminator(lexeme) => f.write_str(match lexeme.hint() {
      LineTerminatorHint::NewLine => "line feed in a single-line string",
      LineTerminatorHint::CarriageReturn => "carriage return in a single-line string",
      LineTerminatorHint::CarriageReturnNewLine => "line break in a single-line string",
    }),
    StringError::UnexpectedEscapedCharacter(_) => {
      f.write_str(SyntaxErrorKind::UnknownEscape.message())
    }
    StringError::IncompleteEscape(_) => {
      f.write_str("incomplete escape sequence, nothing follows the `\\`")
    }
    StringError::Unopened(_) => f.write_str(SyntaxErrorKind::UnopenedString.message()),
    StringError::Unterminated(end) => f.write_str(match end.hint() {
      LitStrDelimiterHint::Quote => "unterminated string, expected a closing `\"`",
      LitStrDelimiterHint::TripleQuote => "unterminated block string, expected a closing `\"\"\"`",
      LitStrDelimiterHint::QuoteOrTripleQuote => SyntaxErrorKind::UnterminatedString.message(),
    }),
    StringError::Unicode(error) => f.write_str(unicode_error(error)),
    StringError::Other(message) => f.write_str(message),
  }
}

/// A Unicode escape's refusal, as the sentence its lexer error renders.
fn unicode_error<Char>(error: &UnicodeError<Char>) -> &'static str {
  match error {
    UnicodeError::Fixed(FixedUnicodeEscapeError::Incomplete(_)) => {
      "incomplete Unicode escape, expected four hex digits after `\\u`"
    }
    UnicodeError::Fixed(FixedUnicodeEscapeError::InvalidSequence(_)) => {
      "invalid Unicode escape, expected four hex digits after `\\u`"
    }
    UnicodeError::Fixed(FixedUnicodeEscapeError::UnpairedSurrogate(lexeme)) => {
      match lexeme.hint() {
        UnpairedSurrogateHint::High => "unpaired high surrogate in a Unicode escape",
        UnpairedSurrogateHint::Low => "unpaired low surrogate in a Unicode escape",
      }
    }
    UnicodeError::Braced(BracedUnicodeEscapeError::Unclosed(_)) => {
      "unclosed Unicode escape, expected `}`"
    }
    UnicodeError::Braced(BracedUnicodeEscapeError::Empty(_)) => {
      "empty Unicode escape, expected a hex digit between `{` and `}`"
    }
    UnicodeError::Braced(BracedUnicodeEscapeError::TooManyDigits { .. }) => {
      "invalid Unicode escape, more than 6 hex digits"
    }
    UnicodeError::Braced(BracedUnicodeEscapeError::InvalidSequence(_)) => {
      "invalid Unicode escape, expected hex digits between `{` and `}`"
    }
    UnicodeError::Braced(BracedUnicodeEscapeError::InvalidScalar(value)) => match value.kind() {
      InvalidUnicodeScalarKind::Surrogate => "escape names a surrogate, not a Unicode scalar value",
      InvalidUnicodeScalarKind::Overflow => {
        "escape names a value past `10FFFF`, not a Unicode scalar value"
      }
    },
  }
}
