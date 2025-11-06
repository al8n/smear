use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  error::{
    DefaultContainer, Errors, UnexpectedEnd, UnexpectedEot, UnexpectedLexeme, UnexpectedSuffix,
    UnknownLexeme, Unterminated,
  },
  utils::{
    CharLen, Lexeme, Span, Spanned,
    knowledge::{FloatLiteral, HexFloatLiteral, IntLiteral},
  },
};

use crate::{error::*, graphqlx::GraphQLx, hints::*};

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum FloatError<Char = char> {
  /// The float has an unexpected suffix, e.g. `1.0x`, `1.e+1y`
  UnexpectedSuffix(UnexpectedSuffix<Char, FloatLiteral>),
  /// Unexpected lexeme in float literal, e.g. `1.x`, `1.ex`, `1.e+x`
  UnexpectedLexeme(UnexpectedLexeme<Char, FloatHint>),
  /// Unexpected end of input in float literal.
  UnexpectedEnd(UnexpectedEnd<FloatHint>),
  /// Float literals must have an integer part, e.g. `.1` is invalid.
  #[from(skip)]
  MissingIntegerPart(Span),
}

impl<Char> FloatError<Char> {
  /// Creates a new unexpected suffix float error.
  #[inline]
  pub fn unexpected_suffix(token: Span, suffix: Lexeme<Char>) -> Self
  where
    Char: CharLen,
  {
    Self::UnexpectedSuffix(UnexpectedSuffix::new(token, suffix))
  }
}

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum HexFloatError<Char = char> {
  /// The float has an unexpected suffix, e.g. `1.0x`, `1.e+1y`
  UnexpectedSuffix(UnexpectedSuffix<Char, HexFloatLiteral>),
  /// Unexpected lexeme in float literal, e.g. `1.x`, `1.ex`, `1.e+x`
  UnexpectedLexeme(UnexpectedLexeme<Char, HexFloatHint>),
  /// Unexpected end of input in float literal.
  UnexpectedEnd(UnexpectedEnd<HexFloatHint>),
  /// Hex float literals must have an integer part, e.g. `.1` is invalid.
  #[from(skip)]
  MissingIntegerPart(Span),
  /// Hex float literals must have an exponent part, e.g. `0x1.0` is invalid.
  #[from(skip)]
  MissingExponent(Span),
}

impl<Char> HexFloatError<Char> {
  /// Creates a new unexpected suffix hex float error.
  #[inline]
  pub fn unexpected_suffix(token: Span, suffix: Lexeme<Char>) -> Self
  where
    Char: CharLen,
  {
    Self::UnexpectedSuffix(UnexpectedSuffix::new(token, suffix))
  }
}

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum DecimalError<Char = char> {
  /// Unexpected character in decimal literal suffix, e.g. `123abc`
  UnexpectedSuffix(UnexpectedSuffix<Char, IntLiteral>),
  /// Unexpected character in decimal literal, e.g. `-A`
  UnexpectedEnd(UnexpectedEnd<DecimalHint>),
}

impl<Char> DecimalError<Char> {
  /// Creates a new unexpected suffix decimal error.
  #[inline]
  pub fn unexpected_suffix(token: Span, suffix: Lexeme<Char>) -> Self
  where
    Char: CharLen,
  {
    Self::UnexpectedSuffix(UnexpectedSuffix::new(token, suffix))
  }
}

/// An error encountered during lexing for hex literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum HexError<Char = char> {
  /// Unexpected character in hex literal suffix, e.g. `0x123abc`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in hex literal, e.g. `0x-1`
  UnexpectedEnd(UnexpectedEnd<HexHint>),
}

/// An error encountered during lexing for octal literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum OctalError<Char = char> {
  /// Unexpected character in octal literal suffix, e.g. `0o123abc`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in octal literal, e.g. `0o-1`
  UnexpectedEnd(UnexpectedEnd<OctalHint>),
}

/// An error encountered during lexing for binary literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum BinaryError<Char = char> {
  /// Unexpected character in binary literal suffix, e.g. `0b10102`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in binary literal, e.g. `0b-1`
  UnexpectedEnd(UnexpectedEnd<BinaryHint>),
}

/// The data of the lexer error.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum LexerError<Char = char, StateError = ()> {
  /// An error encountered during lexing for float literals.
  Float(FloatError<Char>),
  /// An error encountered during lexing for hex float literals.
  HexFloat(HexFloatError<Char>),
  /// An error encountered during lexing for decimal literals.
  Decimal(DecimalError<Char>),
  /// An error encountered during lexing for hex literals.
  Hex(HexError<Char>),
  /// An error encountered during lexing for octal literals.
  Octal(OctalError<Char>),
  /// An error encountered during lexing for binary literals.
  Binary(BinaryError<Char>),
  /// An error encountered during lexing for string literals.
  String(StringError<Char>),
  /// Unexpected token character.
  UnexpectedLexeme(UnexpectedLexeme<Char, GraphQLx>),
  /// Unknown token character.
  UnknownLexeme(UnknownLexeme<Char, GraphQLx>),
  /// Unexpected end of input.
  UnexpectedEndOfInput(UnexpectedEot),
  /// Unterminated spread operator.
  UnterminatedSpreadOperator(Unterminated<SpreadOperator>),
  /// The lexer state related error.
  State(Spanned<StateError>),
}

impl<Char, StateError> LexerError<Char, StateError> {
  /// Creates new float error.
  #[inline]
  pub const fn float(error: FloatError<Char>) -> Self {
    Self::Float(error)
  }

  /// Creates new float error.
  #[inline]
  pub const fn hex_float(error: HexFloatError<Char>) -> Self {
    Self::HexFloat(error)
  }

  /// Creates new decimal error.
  #[inline]
  pub const fn decimal(error: DecimalError<Char>) -> Self {
    Self::Decimal(error)
  }

  /// Creates new hex error.
  #[inline]
  pub const fn hex(error: HexError<Char>) -> Self {
    Self::Hex(error)
  }

  /// Creates new octal error.
  #[inline]
  pub const fn octal(error: OctalError<Char>) -> Self {
    Self::Octal(error)
  }

  /// Creates new binary error.
  #[inline]
  pub const fn binary(error: BinaryError<Char>) -> Self {
    Self::Binary(error)
  }

  /// Creates a unexpected end of input error.
  #[inline]
  pub const fn unexpected_eoi(span: Span) -> Self {
    Self::UnexpectedEndOfInput(UnexpectedEot::eot(span))
  }

  /// Creates new unknown char from a positioned character.
  #[inline]
  pub const fn unknown_char(pos: usize, ch: Char) -> Self {
    Self::UnknownLexeme(UnknownLexeme::from_char(pos, ch, GraphQLx(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub fn unknown_chars(range: impl Into<Span>) -> Self {
    Self::UnknownLexeme(UnknownLexeme::from_range(range, GraphQLx(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub const fn unknown_chars_const(start: usize, end: usize) -> Self {
    Self::UnknownLexeme(UnknownLexeme::from_range_const(
      Span::new(start, end),
      GraphQLx(()),
    ))
  }

  /// Creates new unknown char from a positioned character.
  #[inline]
  pub const fn unexpected_char(pos: usize, ch: Char) -> Self {
    Self::UnexpectedLexeme(UnexpectedLexeme::from_char(pos, ch, GraphQLx(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub fn unexpected_chars(range: impl Into<Span>) -> Self {
    Self::UnexpectedLexeme(UnexpectedLexeme::from_range(range, GraphQLx(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub const fn unexpected_chars_const(start: usize, end: usize) -> Self {
    Self::UnexpectedLexeme(UnexpectedLexeme::from_range_const(
      Span::new(start, end),
      GraphQLx(()),
    ))
  }

  /// Creates a new unterminated spread operator error.
  #[inline]
  pub const fn unterminated_spread_operator(span: Span) -> Self {
    Self::UnterminatedSpreadOperator(Unterminated::new(span, SpreadOperator))
  }

  /// Creates a state error.
  #[inline]
  pub fn state(span: Span, error: StateError) -> Self {
    Self::State(Spanned::new(span, error))
  }
}

/// A container for storing multiple lexer errors.
pub type LexerErrors<
  Char = char,
  StateError = (),
  Container = DefaultContainer<LexerError<Char, StateError>>,
> = Errors<LexerError<Char, StateError>, Container>;

impl<Char, StateError, Container> crate::error::Wrapper
  for LexerErrors<Char, StateError, Container>
{
  type Underlying = Container;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from_underlying(underlying: Self::Underlying) -> Self {
    Self::from_container(underlying)
  }
}

impl<Char, StateError, Container> FromIterator<StringError<Char>>
  for LexerErrors<Char, StateError, Container>
where
  Container: FromIterator<LexerError<Char, StateError>>,
{
  #[inline]
  fn from_iter<T: IntoIterator<Item = StringError<Char>>>(iter: T) -> Self {
    iter.into_iter().map(LexerError::String).collect()
  }
}

impl<Char, StateError> UnterminatedSpreadOperatorError for LexerError<Char, StateError> {
  #[inline]
  fn unterminated_spread_operator(span: Span) -> Self {
    Self::unterminated_spread_operator(span)
  }
}

impl<Char, StateError, Container> UnterminatedSpreadOperatorError
  for LexerErrors<Char, StateError, Container>
where
  Container: FromIterator<LexerError<Char, StateError>>,
{
  #[inline]
  fn unterminated_spread_operator(span: Span) -> Self {
    LexerError::unterminated_spread_operator(span).into()
  }
}

impl<Char, StateError> BadStateError for LexerError<Char, StateError> {
  type StateError = StateError;
  #[inline]
  fn bad_state(span: Span, error: Self::StateError) -> Self {
    Self::state(span, error)
  }
}

impl<Char, StateError, Container> BadStateError for LexerErrors<Char, StateError, Container>
where
  Container: FromIterator<LexerError<Char, StateError>>,
{
  type StateError = StateError;
  #[inline]
  fn bad_state(span: Span, error: Self::StateError) -> Self {
    LexerError::state(span, error).into()
  }
}
