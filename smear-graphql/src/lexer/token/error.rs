use core::{error, fmt};
use std::borrow::Cow;

use derive_more::{From, IsVariant, Unwrap, TryUnwrap, Display};
use logosky::utils::{Lexeme, PositionedChar, UnexpectedEnd, UnexpectedLexeme};

/// The hint about what is expected for the next character
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum ExponentHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
  /// Expect the next character to be a sign or a digit.
  #[display("'+', '-' or digit")]
  SignOrDigit,
  /// Expect the next character to be an exponent identifier 'e' or 'E'.
  #[display("'e' or 'E'")]
  Identifier,
}

#[derive(Copy, Clone, Debug, Display, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap)]
pub enum IntHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap)]
pub enum FloatHint {
  Fractional,
  Exponent(ExponentHint),
  Digit,
}

impl fmt::Display for FloatHint {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fractional => write!(f, "fractional digits"),
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

// impl<C> From<UnexpectedExponentCharacter<C>> for UnexpectedFloatCharacter<C> {
//   #[inline]
//   fn from(e: UnexpectedExponentCharacter<C>) -> Self {
//     let (found, hint) = e.into_components();
//     Self::new(found, FloatHint::Exponent(hint))
//   }
// }

// impl<C> From<UnexpectedFractionalCharacter<C>> for UnexpectedFloatCharacter<C> {
//   #[inline]
//   fn from(e: UnexpectedFractionalCharacter<C>) -> Self {
//     let (found, hint) = e.into_components();
//     Self::new(found, FloatHint::Fractional)
//   }
// }

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum FloatError<Char> {
  /// The float has an unexpected suffix, e.g. `1.0x`, `1.e+1y`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected lexeme in float literal, e.g. `1.x`, `1.ex`, `1.e+x`
  UnexpectedLexeme(UnexpectedLexeme<Char, FloatHint>),
  /// Unexpected end of input in float literal.
  UnexpectedEof(UnexpectedEnd<FloatHint>),
  /// Float must not have non-significant leading zeroes.
  #[from(skip)]
  LeadingZeros(Lexeme<Char>),
  /// Float literals must have an integer part, e.g. `.1` is invalid.
  #[from(skip)]
  MissingIntegerPart,
}

// impl<Char> fmt::Display for FloatError<Char>
// where
//   Char: fmt::Display,
// {
//   #[inline]
//   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//     match self {
//       Self::UnexpectedSuffix(c) => {
//         write!(f, "unexpected character '{c}' as float suffix")
//       }
//       Self::LeadingZero => {
//         write!(f, "float must not have non-significant leading zeroes")
//       },
//       Self::UnexpectedCharacter(e) => {
//         write!(f, "unexpected character '{}' in float, expected {}", e.found(), e.hint())
//       }
//       Self::UnexpectedEof(e) => {
//         write!(f, "unexpected end of input in float, expected {e}")
//       }
//     }
//   }
// }

// impl<Char> error::Error for FloatError<Char> where Char: fmt::Debug + fmt::Display {}

// impl<Char> From<ExponentError<Char>> for FloatError<Char> {
//   #[inline]
//   fn from(e: ExponentError<Char>) -> Self {
//     match e {
//       ExponentError::UnexpectedSuffix(c) => Self::UnexpectedSuffix(c),
//       ExponentError::UnexpectedEof(e) => Self::UnexpectedEof(e.into()),
//       ExponentError::UnexpectedLexeme(e) => Self::UnexpectedCharacter(e.into()),
//     }
//   }
// }

// impl<Char> From<FractionalError<Char>> for FloatError<Char> {
//   #[inline]
//   fn from(e: FractionalError<Char>) -> Self {
//     match e {
//       FractionalError::UnexpectedCharacter(e) => Self::UnexpectedCharacter(e.into()),
//       FractionalError::UnexpectedEof(e) => Self::UnexpectedEof(e.into()),
//     }
//   }
// }

// impl<Char> From<UnexpectedFractionalCharacter<Char>> for FloatError<Char> {
//   #[inline]
//   fn from(e: UnexpectedFractionalCharacter<Char>) -> Self {
//     Self::UnexpectedCharacter(e.into())
//   }
// }

// impl<Char> From<FractionalHint> for FloatError<Char> {
//   #[inline]
//   fn from(e: FractionalHint) -> Self {
//     Self::UnexpectedEof(e.into())
//   }
// }

// impl<Char> From<UnexpectedExponentCharacter<Char>> for FloatError<Char> {
//   #[inline]
//   fn from(e: UnexpectedExponentCharacter<Char>) -> Self {
//     Self::UnexpectedCharacter(e.into())
//   }
// }

// impl<Char> From<ExponentHint> for FloatError<Char> {
//   #[inline]
//   fn from(e: ExponentHint) -> Self {
//     Self::UnexpectedEof(e.into())
//   }
// }

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum IntError<Char> {
  /// Unexpected character in integer literal suffix, e.g. `123abc`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in integer literal, e.g. `-A`
  UnexpectedEof(UnexpectedEnd<IntHint>),
  // #[error("integer must not have non-significant leading zeroes")]
  #[from(skip)]
  LeadingZeros(Lexeme<Char>),
}

// impl<Char> fmt::Display for IntError<Char>
// where
//   Char: fmt::Display,
// {
//   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//     match self {
//       Self::UnexpectedSuffix(c) => {
//         write!(f, "unexpected character '{c}' as integer suffix")
//       }
//       Self::LeadingZero => {
//         write!(f, "integer must not have non-significant leading zeroes")
//       }
//       Self::UnexpectedCharacter(e) => {
//         write!(f, "unexpected character '{}' in integer, expected digit", e.found())
//       }
//     }
//   }
// }

// impl<Char> error::Error for IntError<Char> where Char: fmt::Debug + fmt::Display {}

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
  #[from(skip)]
  UnexpectedCharacter(PositionedChar<Char>),
  /// Unknown token character.
  #[from(skip)]
  UnknownCharacter(PositionedChar<Char>),
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

