use core::{fmt, ops::Range};

use std::borrow::Cow;

use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
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

#[derive(
  Copy,
  Clone,
  Debug,
  Display,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Hash,
  From,
  IsVariant,
  Unwrap,
  TryUnwrap,
)]
pub enum IntHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
}

#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap,
)]
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
pub enum ErrorData<Char> {
  /// An error encountered during lexing for float literals.
  Float(FloatError<Char>),
  /// An error encountered during lexing for integer literals.
  Int(IntError<Char>),
  /// Unexpected token character.
  #[from(skip)]
  UnexpectedLexeme(Lexeme<Char>),
  /// Unknown token character.
  #[from(skip)]
  UnknownLexeme(Lexeme<Char>),
  /// Unexpected end of input.
  UnexpectedEndOfInput,
  /// Unterminated spread operator.
  UnterminatedSpreadOperator,
  /// Other error.
  Other(Cow<'static, str>),
}

impl<Char> Default for ErrorData<Char> {
  #[inline(always)]
  fn default() -> Self {
    Self::Other(Cow::Borrowed("unknown"))
  }
}

impl<Char> ErrorData<Char> {
  /// Create a new error data with the given message.
  #[inline]
  pub fn other(message: impl Into<Cow<'static, str>>) -> Self {
    Self::Other(message.into())
  }

  /// Creates new float error data.
  #[inline]
  pub const fn float(error: FloatError<Char>) -> Self {
    Self::Float(error)
  }

  /// Creates new int error data.
  #[inline]
  pub const fn int(error: IntError<Char>) -> Self {
    Self::Int(error)
  }

  /// Creates new unexpected lexeme error data.
  #[inline]
  pub const fn unexpected_lexeme(lexeme: Lexeme<Char>) -> Self {
    Self::UnexpectedLexeme(lexeme)
  }

  /// Creates new unknown lexeme error data.
  #[inline]
  pub const fn unknown_lexeme(lexeme: Lexeme<Char>) -> Self {
    Self::UnknownLexeme(lexeme)
  }

  /// Creates new unexpected lexeme error data from a positioned character.
  #[inline]
  pub const fn unexpected_char(char: Char, position: usize) -> Self {
    Self::UnexpectedLexeme(Lexeme::Char(PositionedChar::with_position(char, position)))
  }

  /// Creates new unknown lexeme error data from a positioned character.
  #[inline]
  pub const fn unknown_char(char: Char, position: usize) -> Self {
    Self::UnknownLexeme(Lexeme::Char(PositionedChar::with_position(char, position)))
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Error<Char> {
  span: Range<usize>,
  data: ErrorData<Char>,
}

impl<Char> Default for Error<Char> {
  #[inline(always)]
  fn default() -> Self {
    Self::unexpected_eoi(0..0)
  }
}

impl<Char> Error<Char> {
  /// Create a new error with the given span and data.
  #[inline]
  pub const fn new(span: Range<usize>, data: ErrorData<Char>) -> Self {
    Self { span, data }
  }

  /// Creates a new float error.
  #[inline]
  pub const fn float(span: Range<usize>, error: FloatError<Char>) -> Self {
    Self::new(span, ErrorData::Float(error))
  }

  /// Creates a new int error.
  #[inline]
  pub const fn int(span: Range<usize>, error: IntError<Char>) -> Self {
    Self::new(span, ErrorData::Int(error))
  }

  /// Creates a new unexpected lexeme error.
  #[inline]
  pub const fn unexpected_lexeme(span: Range<usize>, lexeme: Lexeme<Char>) -> Self {
    Self::new(span, ErrorData::UnexpectedLexeme(lexeme))
  }

  /// Creates a new unknown lexeme error.
  #[inline]
  pub const fn unknown_lexeme(span: Range<usize>, lexeme: Lexeme<Char>) -> Self {
    Self::new(span, ErrorData::UnknownLexeme(lexeme))
  }

  /// Creates a new unexpected lexeme error from a positioned character.
  #[inline]
  pub const fn unexpected_char(span: Range<usize>, char: Char, position: usize) -> Self {
    Self::new(span, ErrorData::unexpected_char(char, position))
  }

  /// Creates a new unknown lexeme error from a positioned character.
  #[inline]
  pub const fn unknown_char(span: Range<usize>, char: Char, position: usize) -> Self {
    Self::new(span, ErrorData::unknown_char(char, position))
  }

  /// Creates an End of Input error.
  #[inline]
  pub const fn unexpected_eoi(span: Range<usize>) -> Self {
    Self::new(span, ErrorData::UnexpectedEndOfInput)
  }

  /// Get the span of the error. The span contains the start and end byte indices in the source,
  /// where the error occurred, the data may also contain a span for more precise information.
  #[inline]
  pub const fn span(&self) -> &Range<usize> {
    &self.span
  }

  /// Get the data of the error.
  #[inline]
  pub const fn data(&self) -> &ErrorData<Char> {
    &self.data
  }

  /// Consume the error and return the error data.
  #[inline]
  pub fn into_data(self) -> ErrorData<Char> {
    self.data
  }

  /// Consumes the error and returns its components.
  #[inline]
  pub fn into_components(self) -> (Range<usize>, ErrorData<Char>) {
    (self.span, self.data)
  }
}

impl<Char> From<Error<Char>> for ErrorData<Char> {
  #[inline]
  fn from(error: Error<Char>) -> Self {
    error.into_data()
  }
}

#[cfg(feature = "smallvec")]
type DefaultErrorsContainer<Char> = smallvec::SmallVec<[Error<Char>; 2]>;

#[cfg(not(feature = "smallvec"))]
type DefaultErrorsContainer<Char> = std::vec::Vec<Error<Char>>;

#[derive(
  Debug,
  Default,
  Clone,
  PartialEq,
  Eq,
  Hash,
  derive_more::Deref,
  derive_more::DerefMut,
  derive_more::AsMut,
  derive_more::AsRef,
)]
pub struct Errors<Char>(DefaultErrorsContainer<Char>);

impl<Char> From<Error<Char>> for Errors<Char> {
  #[inline]
  fn from(error: Error<Char>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<Char> From<DefaultErrorsContainer<Char>> for Errors<Char> {
  #[inline]
  fn from(errors: DefaultErrorsContainer<Char>) -> Self {
    Self(errors)
  }
}

impl<Char> From<Errors<Char>> for DefaultErrorsContainer<Char> {
  #[inline]
  fn from(errors: Errors<Char>) -> Self {
    errors.0
  }
}

impl<Char> Errors<Char> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
pub enum LengthError {
  /// The error list contains more than one error.
  #[error("too many errors")]
  TooManyErrors,
  /// The error list is empty.
  #[error("no errors")]
  Empty,
}

impl<Char> TryFrom<Errors<Char>> for Error<Char> {
  type Error = LengthError;

  #[inline]
  fn try_from(value: Errors<Char>) -> Result<Self, Self::Error> {
    match value.len() {
      0 => Err(LengthError::Empty),
      1 => Ok(value.0.into_iter().next().unwrap()),
      _ => Err(LengthError::TooManyErrors),
    }
  }
}
