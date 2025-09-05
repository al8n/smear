use core::{error, fmt};
use std::borrow::Cow;

use derive_more::{From, IsVariant, Unwrap, TryUnwrap};
use either::Either;

use crate::lexer::number::{ExponentError, ExponentHint, FractionalError, FractionalHint, UnexpectedEndOfExponent};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap)]
pub enum FloatHint {
  Fractional(FractionalHint),
  Exponent(ExponentHint),
}

impl fmt::Display for FloatHint {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fractional(hint) => write!(f, "{hint}"),
      Self::Exponent(hint) => write!(f, "{hint}"),
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UnexpectedEndOfFloat<C> {
  hint: FloatHint,
  found: Option<C>,
}

impl<C> UnexpectedEndOfFloat<C> {
  #[inline]
  const fn new(hint: FloatHint) -> Self {
    Self::maybe_found(None, hint)
  }

  #[inline]
  const fn maybe_found(found: Option<C>, hint: FloatHint) -> Self {
    Self {
      found,
      hint,
    }
  }

  #[inline]
  pub const fn found(&self) -> Option<&C> {
    self.found.as_ref()
  }

  #[inline]
  pub const fn hint(&self) -> FloatHint {
    self.hint
  }
}

impl<C> From<UnexpectedEndOfExponent<C>> for UnexpectedEndOfFloat<C> {
  #[inline]
  fn from(e: UnexpectedEndOfExponent<C>) -> Self {
    let (found, hint) = e.into_components();
    Self::maybe_found(found, FloatHint::Exponent(hint))
  }
}


/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum FloatError<Char> {
  // #[error("unexpected character '{0}' as float suffix")]
  UnexpectedSuffix(Char),
  UnexpectedEof(UnexpectedEndOfFloat<Char>),
  // #[error("float must not have non-significant leading zeroes")]
  LeadingZero,
}

impl<Char> fmt::Display for FloatError<Char>
where
  Char: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UnexpectedSuffix(c) => {
        write!(f, "unexpected character '{}' as float suffix", c)
      }
      Self::LeadingZero => {
        write!(f, "float must not have non-significant leading zeroes")
      },
      Self::UnexpectedEof(e) => {
        write!(f, "unexpected end of input in float, expected {}", e.hint())
      }
    }
  }
}

impl<Char> error::Error for FloatError<Char> where Char: fmt::Debug + fmt::Display {}

impl<Char> From<ExponentError<Char>> for FloatError<Char> {
  #[inline]
  fn from(e: ExponentError<Char>) -> Self {
    match e {
      ExponentError::UnexpectedSuffix(c) => Self::UnexpectedSuffix(c),
      ExponentError::UnexpectedEof(e) => Self::UnexpectedEof(e.into()),
    }
  }
}

impl<Char> From<FractionalError<Char>> for FloatError<Char> {
  #[inline]
  fn from(e: FractionalError<Char>) -> Self {
    let (found, hint) = e.into_components();
    Self::UnexpectedEof(UnexpectedEndOfFloat::maybe_found(found, FloatHint::Fractional(hint)))
  }
}

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum IntError<Char> {
  // #[error("unexpected character '{0}' as integer suffix")]
  UnexpectedSuffix(Char),
  // #[error("integer must not have non-significant leading zeroes")]
  LeadingZero,
}

impl<Char> fmt::Display for IntError<Char>
where
  Char: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IntError::UnexpectedSuffix(c) => {
        write!(f, "unexpected character '{}' as integer suffix", c)
      }
      IntError::LeadingZero => {
        write!(f, "integer must not have non-significant leading zeroes")
      }
    }
  }
}

impl<Char> error::Error for IntError<Char> where Char: fmt::Debug + fmt::Display {}

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, thiserror::Error)]
// #[unwrap(ref)]
// #[try_unwrap(ref)]
pub enum StringError {
  #[error("unterminated string")]
  UnterminatedString,
  #[error("unsupported string character")]
  UnsupportedStringCharacter,
  #[error("unterminated block string")]
  UnterminatedBlockString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnknownCharacter<Char> {
  found: Char,
  pos: usize,
}

impl<Char> UnknownCharacter<Char> {
  /// Creates a new `UnknownCharacter` error.
  #[inline]
  pub const fn new(found: Char, pos: usize) -> Self {
    Self { found, pos }
  }

  /// Returns the unknown character.
  #[inline]
  pub const fn char(&self) -> &Char {
    &self.found
  }

  /// Returns the position of the unknown character.
  #[inline]
  pub const fn pos(&self) -> usize {
    self.pos
  }

  /// Consumes the error, returning the unknown character and its position.
  #[inline]
  pub fn into_components(self) -> (Char, usize) {
    (self.found, self.pos)
  }
}

impl<Char> fmt::Display for UnknownCharacter<Char>
where
  Char: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "unknown character '{}'", self.found)
  }
}

impl<Char> error::Error for UnknownCharacter<Char> where Char: fmt::Debug + fmt::Display {}

/// An error encountered during lexing tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum Error<Char> {
  /// An error encountered during lexing for float literals.
  Float(FloatError<Char>),
  /// An error encountered during lexing for integer literals.
  Int(IntError<Char>),
  /// Unknown token character.
  UnknownCharacter(UnknownCharacter<Char>),
  /// Unexpected end of input.
  UnexpectedEndOfInput,
  /// Unterminated spread operator.
  UnterminatedSpreadOperator,
  /// Other error.
  Other(Cow<'static, str>),
}

impl<Char> Default for Error<Char> {
  #[inline(always)]
  fn default() -> Self {
    Self::Other(Cow::Borrowed("unknown"))
  }
}

impl<Char> fmt::Display for Error<Char>
where
  Char: fmt::Display,
{
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Float(e) => write!(f, "{e}"),
      Self::Int(e) => write!(f, "{e}"),
      Self::UnknownCharacter(e) => write!(f, "{e}"),
      Self::UnexpectedEndOfInput => write!(f, "unexpected end of input"),
      Self::UnterminatedSpreadOperator => write!(f, "unterminated spread operator"),
      Self::Other(s) => write!(f, "{s}"),
    }
  }
}

impl<Char> error::Error for Error<Char> where Char: fmt::Debug + fmt::Display {}

impl<Char> From<ExponentError<Char>> for Error<Char> {
  #[inline]
  fn from(e: ExponentError<Char>) -> Self {
    Self::Float(e.into())
  }
}
