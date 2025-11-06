use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  error::{
    UnexpectedEnd, UnexpectedLexeme, UnexpectedLineTerminator, UnicodeEscapeError, UnknownLexeme,
  },
  utils::{Message, PositionedChar, SingleCharEscape, Span, knowledge::Characters},
};

use crate::hints::LitStrDelimiterHint;

/// An error encountered during lexing for string literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum StringError<Char = char> {
  /// An unsupported character in a string literal.
  UnsupportedCharacter(UnknownLexeme<Char, Characters>),
  /// An unexpected line terminator in a string literal.
  UnexpectedLineTerminator(UnexpectedLineTerminator<Char>),
  /// An unexpected escaped character in a string literal.
  UnexpectedEscapedCharacter(SingleCharEscape<Char>),
  /// An unterminated string literal.
  Unterminated(UnexpectedEnd<LitStrDelimiterHint>),
  /// A unicode error in a string literal.
  Unicode(UnicodeEscapeError<Char>),
}

impl<Char> StringError<Char> {
  /// Returns the span of the string error.
  #[inline]
  pub fn span(&self) -> Span
  where
    Char: logosky::utils::CharLen,
  {
    match self {
      Self::UnsupportedCharacter(e) => e.span(),
      Self::UnexpectedLineTerminator(e) => e.span(),
      Self::UnexpectedEscapedCharacter(e) => e.span(),
      Self::Unterminated(e) => e.span(),
      Self::Unicode(e) => e.span(),
    }
  }

  /// Creates an unterminated string error.
  #[inline]
  pub const fn unterminated_inline_string(span: Span) -> Self {
    Self::Unterminated(UnexpectedEnd::with_name(
      span,
      Message::from_static("string value"),
      LitStrDelimiterHint::Quote,
    ))
  }

  /// Creates an unterminated block string error.
  #[inline]
  pub const fn unterminated_block_string(span: Span) -> Self {
    Self::Unterminated(UnexpectedEnd::with_name(
      span,
      Message::from_static("string value"),
      LitStrDelimiterHint::TripleQuote,
    ))
  }

  /// Creates an unexpected new line error.
  #[inline]
  pub const fn unexpected_new_line(ch: Char, position: usize) -> Self {
    Self::UnexpectedLineTerminator(UnexpectedLexeme::new_line(position, ch))
  }

  /// Creates an unexpected carriage return error.
  #[inline]
  pub const fn unexpected_carriage_return(ch: Char, position: usize) -> Self {
    Self::UnexpectedLineTerminator(UnexpectedLexeme::carriage_return(position, ch))
  }

  /// Creates an unexpected carriage return + new line error.
  #[inline]
  pub const fn unexpected_carriage_return_new_line(span: Span) -> Self {
    Self::UnexpectedLineTerminator(UnexpectedLexeme::carriage_return_new_line(span))
  }

  /// Creates an unexpected escaped character error.
  #[inline]
  pub const fn unexpected_escaped_character(span: Span, ch: Char, position: usize) -> Self {
    Self::UnexpectedEscapedCharacter(SingleCharEscape::from_positioned_char(
      span,
      PositionedChar::with_position(ch, position),
    ))
  }

  /// Bumps the span or position of the error by `n`.
  #[inline]
  pub(crate) fn bump(&mut self, n: usize) -> &mut Self {
    match self {
      Self::UnexpectedLineTerminator(lexeme) => {
        lexeme.bump(n);
      }
      Self::UnexpectedEscapedCharacter(esc_char) => {
        esc_char.bump(n);
      }
      Self::UnsupportedCharacter(ch) => {
        ch.bump(n);
      }
      Self::Unicode(unicode) => {
        unicode.bump(n);
      }
      Self::Unterminated(err) => {
        err.bump(n);
      }
    };

    self
  }
}
