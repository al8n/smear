use core::{error, fmt};
use std::borrow::Cow;

use derive_more::{From, IsVariant, Unwrap, TryUnwrap};
use either::Either;

use crate::lexer::number::{ExponentError, ExponentHint, FractionalError, FractionalHint, UnexpectedExponentCharacter, UnexpectedFractionalCharacter};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap)]
pub enum FloatHint {
  Fractional(FractionalHint),
  Exponent(ExponentHint),
  Digit,
}

impl fmt::Display for FloatHint {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fractional(hint) => write!(f, "{hint}"),
      Self::Exponent(hint) => write!(f, "{hint}"),
      Self::Digit => write!(f, "digit"),
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UnexpectedFloatCharacter<C> {
  hint: FloatHint,
  found: C,
}

impl<C> UnexpectedFloatCharacter<C> {
  #[inline]
  pub(super) const fn new(found: C, hint: FloatHint) -> Self {
    Self {
      found,
      hint,
    }
  }

  #[inline]
  pub const fn found(&self) -> &C {
    &self.found
  }

  #[inline]
  pub const fn hint(&self) -> FloatHint {
    self.hint
  }
}

impl<C> From<UnexpectedExponentCharacter<C>> for UnexpectedFloatCharacter<C> {
  #[inline]
  fn from(e: UnexpectedExponentCharacter<C>) -> Self {
    let (found, hint) = e.into_components();
    Self::new(found, FloatHint::Exponent(hint))
  }
}

impl<C> From<UnexpectedFractionalCharacter<C>> for UnexpectedFloatCharacter<C> {
  #[inline]
  fn from(e: UnexpectedFractionalCharacter<C>) -> Self {
    let (found, hint) = e.into_components();
    Self::new(found, FloatHint::Fractional(hint))
  }
}

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum FloatError<Char> {
  /// The float has an unexpected suffix, e.g. `1.0x`, `1.e+1y`
  UnexpectedSuffix(Char),
  /// Unexpected character in float literal, e.g. `1.x`, `1.ex`, `1.e+x`
  UnexpectedCharacter(UnexpectedFloatCharacter<Char>),
  /// Unexpected end of input in float literal.
  UnexpectedEof(FloatHint),
  /// Float must not have non-significant leading zeroes.
  LeadingZero,
}

impl<Char> fmt::Display for FloatError<Char>
where
  Char: fmt::Display,
{
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UnexpectedSuffix(c) => {
        write!(f, "unexpected character '{c}' as float suffix")
      }
      Self::LeadingZero => {
        write!(f, "float must not have non-significant leading zeroes")
      },
      Self::UnexpectedCharacter(e) => {
        write!(f, "unexpected character '{}' in float, expected {}", e.found(), e.hint())
      }
      Self::UnexpectedEof(e) => {
        write!(f, "unexpected end of input in float, expected {e}")
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
      ExponentError::UnexpectedCharacter(e) => Self::UnexpectedCharacter(e.into()),
    }
  }
}

impl<Char> From<FractionalError<Char>> for FloatError<Char> {
  #[inline]
  fn from(e: FractionalError<Char>) -> Self {
    match e {
      FractionalError::UnexpectedCharacter(e) => Self::UnexpectedCharacter(e.into()),
      FractionalError::UnexpectedEof(e) => Self::UnexpectedEof(e.into()),
    }
  }
}

impl<Char> From<UnexpectedFractionalCharacter<Char>> for FloatError<Char> {
  #[inline]
  fn from(e: UnexpectedFractionalCharacter<Char>) -> Self {
    Self::UnexpectedCharacter(e.into())
  }
}

impl<Char> From<FractionalHint> for FloatError<Char> {
  #[inline]
  fn from(e: FractionalHint) -> Self {
    Self::UnexpectedEof(e.into())
  }
}

impl<Char> From<UnexpectedExponentCharacter<Char>> for FloatError<Char> {
  #[inline]
  fn from(e: UnexpectedExponentCharacter<Char>) -> Self {
    Self::UnexpectedCharacter(e.into())
  }
}

impl<Char> From<ExponentHint> for FloatError<Char> {
  #[inline]
  fn from(e: ExponentHint) -> Self {
    Self::UnexpectedEof(e.into())
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UnexpectedIntCharacter<C> {
  found: C,
}

impl<C> UnexpectedIntCharacter<C> {
  #[inline]
  pub(super) const fn new(found: C) -> Self {
    Self {
      found,
    }
  }

  #[inline]
  pub const fn found(&self) -> &C {
    &self.found
  }

  #[inline]
  pub fn into_found(self) -> C {
    self.found
  }
}


impl<C: core::fmt::Display> core::fmt::Display for UnexpectedIntCharacter<C> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(
      f,
      "unexpected character '{}' in integer, expected digit",
      self.found
    )
  }
}

impl<C: core::fmt::Display + core::fmt::Debug> core::error::Error for UnexpectedIntCharacter<C> {}


/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum IntError<Char> {
  /// Unexpected character in integer literal suffix, e.g. `123a`
  UnexpectedSuffix(Char),
  /// Unexpected character in integer literal, e.g. `-A`
  UnexpectedCharacter(UnexpectedIntCharacter<Char>),
  // #[error("integer must not have non-significant leading zeroes")]
  LeadingZero,
}

impl<Char> fmt::Display for IntError<Char>
where
  Char: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UnexpectedSuffix(c) => {
        write!(f, "unexpected character '{c}' as integer suffix")
      }
      Self::LeadingZero => {
        write!(f, "integer must not have non-significant leading zeroes")
      }
      Self::UnexpectedCharacter(e) => {
        write!(f, "unexpected character '{}' in integer, expected digit", e.found())
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
  pub const fn position(&self) -> usize {
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


// ---
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnexpectedCharacter<Char> {
  found: Char,
  pos: usize,
}

impl<Char> UnexpectedCharacter<Char> {
  /// Creates a new `UnexpectedCharacter` error.
  #[inline]
  pub const fn new(found: Char, pos: usize) -> Self {
    Self { found, pos }
  }

  /// Returns the unexpected character.
  #[inline]
  pub const fn char(&self) -> &Char {
    &self.found
  }

  /// Returns the position of the unexpected character.
  #[inline]
  pub const fn position(&self) -> usize {
    self.pos
  }

  /// Consumes the error, returning the unknown character and its position.
  #[inline]
  pub fn into_components(self) -> (Char, usize) {
    (self.found, self.pos)
  }
}

impl<Char> fmt::Display for UnexpectedCharacter<Char>
where
  Char: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "unexpected character '{}'", self.found)
  }
}

impl<Char> error::Error for UnexpectedCharacter<Char> where Char: fmt::Debug + fmt::Display {}

/// An error encountered during lexing tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum Error<Char> {
  /// An error encountered during lexing for float literals.
  Float(FloatError<Char>),
  /// An error encountered during lexing for integer literals.
  Int(IntError<Char>),
  /// Unexpected token character.
  UnexpectedCharacter(UnexpectedCharacter<Char>),
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
      Self::UnexpectedCharacter(c) => write!(f, "unexpected character '{c}'"),
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

impl<Char> From<UnexpectedIntCharacter<Char>> for Error<Char> {
  #[inline]
  fn from(e: UnexpectedIntCharacter<Char>) -> Self {
    Self::Int(IntError::UnexpectedCharacter(e))
  }
}

impl<Char> From<FractionalError<Char>> for Error<Char> {
  #[inline]
  fn from(e: FractionalError<Char>) -> Self {
    Self::Float(e.into())
  }
}

impl<Char> From<UnexpectedFloatCharacter<Char>> for Error<Char> {
  #[inline]
  fn from(e: UnexpectedFloatCharacter<Char>) -> Self {
    Self::Float(FloatError::UnexpectedCharacter(e))
  }
}
