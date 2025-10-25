use std::borrow::Cow;

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme};

use crate::{error::*, hints::*};

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum FloatError<Char = char> {
  /// The float has an unexpected suffix, e.g. `1.0x`, `1.e+1y`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected lexeme in float literal, e.g. `1.x`, `1.ex`, `1.e+x`
  UnexpectedLexeme(UnexpectedLexeme<Char, FloatHint>),
  /// Unexpected end of input in float literal.
  UnexpectedEnd(UnexpectedEnd<FloatHint>),
  /// Float must not have non-significant leading zeroes.
  #[from(skip)]
  LeadingZeros(Lexeme<Char>),
  /// Float literals must have an integer part, e.g. `.1` is invalid.
  #[from(skip)]
  MissingIntegerPart,
}

/// An error encountered during lexing for decimal literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum DecimalError<Char = char> {
  /// Unexpected character in decimal literal suffix, e.g. `123abc`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in decimal literal, e.g. `-A`
  UnexpectedEnd(UnexpectedEnd<DecimalHint>),
  /// Decimal literals must not have non-significant leading zeroes, e.g. `0123`
  #[from(skip)]
  LeadingZeros(Lexeme<Char>),
}

/// The data of the lexer error.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LexerErrorData<Char = char, StateError = ()> {
  /// An error encountered during lexing for float literals.
  Float(FloatError<Char>),
  /// An error encountered during lexing for integer literals.
  Int(DecimalError<Char>),
  /// An error encountered during lexing for string literals.
  String(StringErrors<Char>),
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
  /// The lexer state related error.
  #[from(skip)]
  State(StateError),
  /// Not a valid UTF-8 source.
  InvalidUtf8(core::str::Utf8Error),
  /// Other error.
  Other(Cow<'static, str>),
}

impl<Char, StateError> Default for LexerErrorData<Char, StateError> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::Other(Cow::Borrowed("unknown"))
  }
}

impl<Char, StateError> LexerErrorData<Char, StateError> {
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
  pub const fn int(error: DecimalError<Char>) -> Self {
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

/// A lexer error with span and data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexerError<Char = char, StateError = ()> {
  span: Span,
  data: LexerErrorData<Char, StateError>,
}

impl<Char, StateError> Default for LexerError<Char, StateError> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::unexpected_eoi(Span::from(0..0))
  }
}

impl<Char, StateError> LexerError<Char, StateError> {
  /// Create a new error with the given span and data.
  #[inline]
  pub const fn const_new(span: Span, data: LexerErrorData<Char, StateError>) -> Self {
    Self { span, data }
  }

  /// Create a new error with the given span and data.
  #[inline]
  pub fn new(span: impl Into<Span>, data: LexerErrorData<Char, StateError>) -> Self {
    Self {
      span: span.into(),
      data,
    }
  }

  /// Creates a new float error.
  #[inline]
  pub const fn float(span: Span, error: FloatError<Char>) -> Self {
    Self::const_new(span, LexerErrorData::Float(error))
  }

  /// Creates a new int error.
  #[inline]
  pub const fn int(span: Span, error: DecimalError<Char>) -> Self {
    Self::const_new(span, LexerErrorData::Int(error))
  }

  /// Creates a new unexpected lexeme error.
  #[inline]
  pub const fn unexpected_lexeme(span: Span, lexeme: Lexeme<Char>) -> Self {
    Self::const_new(span, LexerErrorData::UnexpectedLexeme(lexeme))
  }

  /// Creates a new unknown lexeme error.
  #[inline]
  pub const fn unknown_lexeme(span: Span, lexeme: Lexeme<Char>) -> Self {
    Self::const_new(span, LexerErrorData::UnknownLexeme(lexeme))
  }

  /// Creates a new unexpected lexeme error from a positioned character.
  #[inline]
  pub const fn unexpected_char(span: Span, char: Char, position: usize) -> Self {
    Self::const_new(span, LexerErrorData::unexpected_char(char, position))
  }

  /// Creates a new unknown lexeme error from a positioned character.
  #[inline]
  pub const fn unknown_char(span: Span, char: Char, position: usize) -> Self {
    Self::const_new(span, LexerErrorData::unknown_char(char, position))
  }

  /// Creates an End of Input error.
  #[inline]
  pub const fn unexpected_eoi(span: Span) -> Self {
    Self::const_new(span, LexerErrorData::UnexpectedEndOfInput)
  }

  /// Get the span of the error. The span contains the start and end byte indices in the source,
  /// where the error occurred, the data may also contain a span for more precise information.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the data of the error.
  #[inline]
  pub const fn data(&self) -> &LexerErrorData<Char, StateError> {
    &self.data
  }

  /// Get the mutable data of the error.
  #[inline]
  pub fn data_mut(&mut self) -> &mut LexerErrorData<Char, StateError> {
    &mut self.data
  }

  /// Consume the error and return the error data.
  #[inline]
  pub fn into_data(self) -> LexerErrorData<Char, StateError> {
    self.data
  }

  /// Consumes the error and returns its components.
  #[inline]
  pub fn into_components(self) -> (Span, LexerErrorData<Char, StateError>) {
    (self.span, self.data)
  }
}

impl<Char, StateError> From<LexerError<Char, StateError>> for LexerErrorData<Char, StateError> {
  #[inline]
  fn from(error: LexerError<Char, StateError>) -> Self {
    error.into_data()
  }
}

#[cfg(feature = "smallvec")]
type DefaultErrorsContainer<Char = char, StateError = ()> =
  smallvec::SmallVec<[LexerError<Char, StateError>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultErrorsContainer<Char = char, StateError = ()> =
  std::vec::Vec<LexerError<Char, StateError>>;

/// A container for storing multiple lexer errors.
#[derive(Debug, Clone, PartialEq, Eq, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct LexerErrors<Char = char, StateError = ()>(DefaultErrorsContainer<Char, StateError>);

impl<Char, StateError> Default for LexerErrors<Char, StateError> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<Char, StateError> From<LexerError<Char, StateError>> for LexerErrors<Char, StateError> {
  #[inline]
  fn from(error: LexerError<Char, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<Char, StateError> LexerErrors<Char, StateError> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<Char, StateError> UnterminatedSpreadOperatorError for LexerError<Char, StateError> {
  #[inline]
  fn unterminated_spread_operator(span: Span) -> Self {
    Self::const_new(span, LexerErrorData::UnterminatedSpreadOperator)
  }
}

impl<Char, StateError> UnterminatedSpreadOperatorError for LexerErrors<Char, StateError> {
  #[inline]
  fn unterminated_spread_operator(span: Span) -> Self {
    LexerError::const_new(span, LexerErrorData::UnterminatedSpreadOperator).into()
  }
}

impl<Char, StateError> BadStateError for LexerError<Char, StateError> {
  type StateError = StateError;
  #[inline]
  fn bad_state(span: Span, error: Self::StateError) -> Self {
    Self::const_new(span, LexerErrorData::State(error))
  }
}

impl<Char, StateError> BadStateError for LexerErrors<Char, StateError> {
  type StateError = StateError;
  #[inline]
  fn bad_state(span: Span, error: Self::StateError) -> Self {
    LexerError::const_new(span, LexerErrorData::State(error)).into()
  }
}
