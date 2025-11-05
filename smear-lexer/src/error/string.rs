use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  error::{UnexpectedEnd, UnexpectedLexeme, UnexpectedLineTerminator, UnicodeEscapeError},
  utils::{Lexeme, Message, PositionedChar, SingleCharEscape, Span},
};

use crate::hints::LitStrDelimiterHint;

use super::LengthError;

/// An error encountered during lexing for string literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum StringError<Char = char> {
  /// An unsupported character in a string literal.
  #[from(skip)]
  UnsupportedCharacter(Lexeme<Char>),
  /// An unexpected line terminator in a string literal.
  UnexpectedLineTerminator(UnexpectedLineTerminator<Char>),
  /// An unexpected escaped character in a string literal.
  UnexpectedEscapedCharacter(SingleCharEscape<Char>),
  /// A unopened string literal. e.g. a string literal that starts with a non-quote character.
  Unopened(UnexpectedLexeme<Option<Char>, LitStrDelimiterHint>),
  /// An unterminated string literal.
  Unterminated(UnexpectedEnd<LitStrDelimiterHint>),
  /// A unicode error in a string literal.
  Unicode(UnicodeEscapeError<Char>),
  /// Any other error in a string literal.
  Other(Message),
}

impl<Char> Default for StringError<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::Other(Message::from_static("unknown"))
  }
}

impl<Char> StringError<Char> {
  /// Creates an unterminated string error.
  #[inline]
  pub const fn unterminated_inline_string() -> Self {
    Self::Unterminated(UnexpectedEnd::with_name(
      Message::from_static("string value"),
      LitStrDelimiterHint::Quote,
    ))
  }

  /// Creates an unterminated block string error.
  #[inline]
  pub const fn unterminated_block_string() -> Self {
    Self::Unterminated(UnexpectedEnd::with_name(
      Message::from_static("string value"),
      LitStrDelimiterHint::TripleQuote,
    ))
  }

  /// Creates an expected open delimiter error.
  #[inline]
  pub const fn unopened_string(ch: Option<Char>, position: usize) -> Self {
    Self::Unopened(UnexpectedLexeme::from_char(
      position,
      ch,
      LitStrDelimiterHint::QuoteOrTripleQuote,
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
      Self::Unopened(lexeme) => {
        lexeme.bump(n);
      }
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
      Self::Other(_) | Self::Unterminated(_) => {}
    };

    self
  }
}

#[cfg(feature = "smallvec")]
type DefaultStringErrorsContainer<Char = char> = smallvec::SmallVec<[StringError<Char>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultStringErrorsContainer<Char = char> = std::vec::Vec<StringError<Char>>;

/// A container for storing multiple string errors.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct StringErrors<Char = char>(DefaultStringErrorsContainer<Char>);

impl<Char> From<StringError<Char>> for StringErrors<Char> {
  #[inline]
  fn from(error: StringError<Char>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<Char> StringErrors<Char> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultStringErrorsContainer::with_capacity(capacity))
  }
}

impl<Char> IntoIterator for StringErrors<Char> {
  type Item = StringError<Char>;

  type IntoIter = <DefaultStringErrorsContainer<Char> as IntoIterator>::IntoIter;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<Char> TryFrom<StringErrors<Char>> for StringError<Char> {
  type Error = LengthError;

  #[inline]
  fn try_from(value: StringErrors<Char>) -> Result<Self, Self::Error> {
    match value.len() {
      0 => Err(LengthError::Empty),
      1 => Ok(value.into_iter().next().unwrap()),
      _ => Err(LengthError::TooManyErrors),
    }
  }
}
