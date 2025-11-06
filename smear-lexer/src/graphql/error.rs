use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  error::{
    DefaultContainer, Errors, UnexpectedEnd, UnexpectedEot, UnexpectedLexeme, UnexpectedPrefix,
    UnexpectedSuffix, UnknownLexeme, Unterminated,
  },
  utils::{
    CharLen, Lexeme, Span, Spanned,
    knowledge::{FloatLiteral, IntLiteral},
  },
};

use crate::{error::*, graphql::GraphQL, hints::*};

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
  /// Float must not have non-significant leading zeroes.
  LeadingZeros(UnexpectedPrefix<Char, FloatLiteral>),
  /// Float literals must have an integer part, e.g. `.1` is invalid.
  #[from(skip)]
  MissingIntegerPart(Span),
}

impl<Char> FloatError<Char> {
  /// Creates a new leading zeros float error.
  #[inline]
  pub fn leading_zeros(token: Span, prefix: Lexeme<Char>) -> Self
  where
    Char: CharLen,
  {
    Self::LeadingZeros(UnexpectedPrefix::new(token, prefix))
  }

  /// Creates a new unexpected suffix float error.
  #[inline]
  pub fn unexpected_suffix(token: Span, suffix: Lexeme<Char>) -> Self
  where
    Char: CharLen,
  {
    Self::UnexpectedSuffix(UnexpectedSuffix::new(token, suffix))
  }

  /// Returns the span of the float error.
  #[inline]
  pub fn span(&self) -> Span
  where
    Char: CharLen,
  {
    match self {
      Self::UnexpectedSuffix(e) => e.span(),
      Self::UnexpectedLexeme(e) => e.span(),
      Self::UnexpectedEnd(e) => e.span(),
      Self::LeadingZeros(e) => e.span(),
      Self::MissingIntegerPart(span) => *span,
    }
  }
}

/// An error encountered during lexing for decimal literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum IntError<Char = char> {
  /// Unexpected character in decimal literal suffix, e.g. `123abc`
  UnexpectedSuffix(UnexpectedSuffix<Char, IntLiteral>),
  /// Decimal literals must not have non-significant leading zeroes, e.g. `0123`
  LeadingZeros(UnexpectedPrefix<Char, IntLiteral>),
}

impl<Char> IntError<Char> {
  /// Creates a new leading zeros float error.
  #[inline]
  pub fn leading_zeros(token: Span, prefix: Lexeme<Char>) -> Self
  where
    Char: CharLen,
  {
    Self::LeadingZeros(UnexpectedPrefix::new(token, prefix))
  }

  /// Creates a new unexpected suffix decimal error.
  #[inline]
  pub fn unexpected_suffix(token: Span, suffix: Lexeme<Char>) -> Self
  where
    Char: CharLen,
  {
    Self::UnexpectedSuffix(UnexpectedSuffix::new(token, suffix))
  }

  /// Returns the span of the int error.
  #[inline]
  pub fn span(&self) -> Span
  where
    Char: CharLen,
  {
    match self {
      Self::UnexpectedSuffix(e) => e.span(),
      Self::LeadingZeros(e) => e.span(),
    }
  }
}

/// The data of the lexer error.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum LexerError<Char = char, StateError = ()> {
  /// An error encountered during lexing for float literals.
  Float(FloatError<Char>),
  /// An error encountered during lexing for integer literals.
  Int(IntError<Char>),
  /// An error encountered during lexing for string literals.
  String(StringError<Char>),
  /// Unexpected token character.
  UnexpectedLexeme(UnexpectedLexeme<Char, GraphQL>),
  /// Unknown token character.
  UnknownLexeme(UnknownLexeme<Char, GraphQL>),
  /// Unexpected end of input.
  UnexpectedEndOfInput(UnexpectedEot),
  /// Unterminated spread operator.
  UnterminatedSpreadOperator(Unterminated<SpreadOperator>),
  /// The lexer state related error.
  State(Spanned<StateError>),
}

impl<Char, StateError> LexerError<Char, StateError> {
  /// Returns the span of the lexer error.
  #[inline]
  pub fn span(&self) -> Span
  where
    Char: CharLen,
  {
    match self {
      Self::Float(e) => e.span(),
      Self::Int(e) => e.span(),
      Self::String(e) => e.span(),
      Self::UnexpectedLexeme(e) => e.span(),
      Self::UnknownLexeme(e) => e.span(),
      Self::UnexpectedEndOfInput(e) => e.span(),
      Self::UnterminatedSpreadOperator(e) => e.span(),
      Self::State(e) => *e.span(),
    }
  }

  /// Creates new string error.
  #[inline]
  pub const fn string(error: StringError<Char>) -> Self {
    Self::String(error)
  }

  /// Creates new float error.
  #[inline]
  pub const fn float(error: FloatError<Char>) -> Self {
    Self::Float(error)
  }

  /// Creates new int error.
  #[inline]
  pub const fn int(error: IntError<Char>) -> Self {
    Self::Int(error)
  }

  /// Creates a unexpected end of input error.
  #[inline]
  pub const fn unexpected_eoi(span: Span) -> Self {
    Self::UnexpectedEndOfInput(UnexpectedEot::eot(span))
  }

  /// Creates new unknown char from a positioned character.
  #[inline]
  pub const fn unknown_char(pos: usize, ch: Char) -> Self {
    Self::UnknownLexeme(UnknownLexeme::from_char(pos, ch, GraphQL(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub fn unknown_chars(range: impl Into<Span>) -> Self {
    Self::UnknownLexeme(UnknownLexeme::from_range(range, GraphQL(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub const fn unknown_chars_const(start: usize, end: usize) -> Self {
    Self::UnknownLexeme(UnknownLexeme::from_range_const(
      Span::new(start, end),
      GraphQL(()),
    ))
  }

  /// Creates new unknown char from a positioned character.
  #[inline]
  pub const fn unexpected_char(pos: usize, ch: Char) -> Self {
    Self::UnexpectedLexeme(UnexpectedLexeme::from_char(pos, ch, GraphQL(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub fn unexpected_chars(range: impl Into<Span>) -> Self {
    Self::UnexpectedLexeme(UnexpectedLexeme::from_range(range, GraphQL(())))
  }

  /// Creates new unknown lexeme error from a range.
  #[inline]
  pub const fn unexpected_chars_const(start: usize, end: usize) -> Self {
    Self::UnexpectedLexeme(UnexpectedLexeme::from_range_const(
      Span::new(start, end),
      GraphQL(()),
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
pub type LexerErrors<Char, StateError, Container = DefaultContainer<LexerError<Char, StateError>>> =
  Errors<LexerError<Char, StateError>, Container>;

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
