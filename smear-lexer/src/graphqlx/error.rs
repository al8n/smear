use std::borrow::Cow;

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  diagnostic::{Code, Diagnose, Label, Location, PathSegment, Severity},
  error::{UnexpectedEnd, UnexpectedLexeme},
  utils::{Lexeme, PositionedChar},
};

type Span = SimpleSpan;

use crate::{
  error::{describe, *},
  hints::*,
};

/// The one document a lex reads, as a [`Location`] numbers it.
const SOURCE: u32 = 0;

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum FloatError<Char = char> {
  /// The float has an unexpected suffix, e.g. `1.0x`, `1.e+1y`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected lexeme in float literal, e.g. `1.x`, `1.ex`, `1.e+x`
  UnexpectedLexeme(UnexpectedLexeme<Char, FloatHint>),
  /// Unexpected end of input in float literal.
  UnexpectedEnd(UnexpectedEnd<FloatHint>),
  /// Float literals must have an integer part, e.g. `.1` is invalid.
  #[from(skip)]
  MissingIntegerPart(Span),
}

impl<Char> FloatError<Char> {
  /// Returns the syntax-error kind this refusal reports as.
  #[inline]
  pub const fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::UnexpectedSuffix(_) | Self::UnexpectedLexeme(_) | Self::MissingIntegerPart(_) => {
        SyntaxErrorKind::InvalidNumber
      }
      Self::UnexpectedEnd(_) => SyntaxErrorKind::IncompleteNumber,
    }
  }

  /// Writes the sentence a float refusal renders as.
  fn describe(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(_) => {
        f.write_str("invalid number, unexpected character after a float")
      }
      Self::UnexpectedLexeme(lexeme) => {
        f.write_str("invalid number, expected ")?;
        f.write_str(describe::float_hint(lexeme.hint()))
      }
      Self::UnexpectedEnd(end) => {
        f.write_str("incomplete number, expected ")?;
        f.write_str(describe::float_hint(end.hint()))
      }
      Self::MissingIntegerPart(_) => {
        f.write_str("invalid number, a float must have an integer part")
      }
    }
  }
}

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum HexFloatError<Char = char> {
  /// The float has an unexpected suffix, e.g. `1.0x`, `1.e+1y`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
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
  /// Returns the syntax-error kind this refusal reports as.
  #[inline]
  pub const fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::UnexpectedSuffix(_) | Self::UnexpectedLexeme(_) | Self::MissingIntegerPart(_) => {
        SyntaxErrorKind::InvalidNumber
      }
      Self::UnexpectedEnd(_) | Self::MissingExponent(_) => SyntaxErrorKind::IncompleteNumber,
    }
  }

  /// Writes the sentence a hexadecimal float refusal renders as.
  fn describe(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(_) => {
        f.write_str("invalid number, unexpected character after a hexadecimal float")
      }
      Self::UnexpectedLexeme(lexeme) => {
        f.write_str("invalid number, expected ")?;
        f.write_str(describe::hex_float_hint(lexeme.hint()))
      }
      Self::UnexpectedEnd(end) => {
        f.write_str("incomplete number, expected ")?;
        f.write_str(describe::hex_float_hint(end.hint()))
      }
      Self::MissingIntegerPart(_) => {
        f.write_str("invalid number, a hexadecimal float must have an integer part")
      }
      Self::MissingExponent(_) => {
        f.write_str("incomplete number, a hexadecimal float must have a `p` exponent")
      }
    }
  }
}

/// An error encountered during lexing for float literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum DecimalError<Char = char> {
  /// Unexpected character in decimal literal suffix, e.g. `123abc`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in decimal literal, e.g. `-A`
  UnexpectedEnd(UnexpectedEnd<DecimalHint>),
}

impl<Char> DecimalError<Char> {
  /// Returns the syntax-error kind this refusal reports as.
  #[inline]
  pub const fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::UnexpectedSuffix(_) => SyntaxErrorKind::InvalidNumber,
      Self::UnexpectedEnd(_) => SyntaxErrorKind::IncompleteNumber,
    }
  }

  /// Writes the sentence a decimal integer refusal renders as.
  fn describe(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(_) => {
        f.write_str("invalid number, unexpected character after an integer")
      }
      Self::UnexpectedEnd(_) => f.write_str("incomplete number, expected a digit"),
    }
  }
}

/// An error encountered during lexing for hex literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum HexError<Char = char> {
  /// Unexpected character in hex literal suffix, e.g. `0x123abc`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in hex literal, e.g. `0x-1`
  UnexpectedEnd(UnexpectedEnd<HexHint>),
}

impl<Char> HexError<Char> {
  /// Returns the syntax-error kind this refusal reports as.
  #[inline]
  pub const fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::UnexpectedSuffix(_) => SyntaxErrorKind::InvalidNumber,
      Self::UnexpectedEnd(_) => SyntaxErrorKind::IncompleteNumber,
    }
  }

  /// Writes the sentence a hexadecimal integer refusal renders as.
  fn describe(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(_) => {
        f.write_str("invalid number, unexpected character after a hexadecimal integer")
      }
      Self::UnexpectedEnd(_) => f.write_str("incomplete number, expected a hex digit"),
    }
  }
}

/// An error encountered during lexing for octal literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum OctalError<Char = char> {
  /// Unexpected character in octal literal suffix, e.g. `0o123abc`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in octal literal, e.g. `0o-1`
  UnexpectedEnd(UnexpectedEnd<OctalHint>),
}

impl<Char> OctalError<Char> {
  /// Returns the syntax-error kind this refusal reports as.
  #[inline]
  pub const fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::UnexpectedSuffix(_) => SyntaxErrorKind::InvalidNumber,
      Self::UnexpectedEnd(_) => SyntaxErrorKind::IncompleteNumber,
    }
  }

  /// Writes the sentence an octal integer refusal renders as.
  fn describe(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(_) => {
        f.write_str("invalid number, unexpected character after an octal integer")
      }
      Self::UnexpectedEnd(_) => f.write_str("incomplete number, expected an octal digit"),
    }
  }
}

/// An error encountered during lexing for binary literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum BinaryError<Char = char> {
  /// Unexpected character in binary literal suffix, e.g. `0b10102`
  #[from(skip)]
  UnexpectedSuffix(Lexeme<Char>),
  /// Unexpected character in binary literal, e.g. `0b-1`
  UnexpectedEnd(UnexpectedEnd<BinaryHint>),
}

impl<Char> BinaryError<Char> {
  /// Returns the syntax-error kind this refusal reports as.
  #[inline]
  pub const fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::UnexpectedSuffix(_) => SyntaxErrorKind::InvalidNumber,
      Self::UnexpectedEnd(_) => SyntaxErrorKind::IncompleteNumber,
    }
  }

  /// Writes the sentence a binary integer refusal renders as.
  fn describe(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(_) => {
        f.write_str("invalid number, unexpected character after a binary integer")
      }
      Self::UnexpectedEnd(_) => f.write_str("incomplete number, expected a binary digit"),
    }
  }
}

/// The data of the lexer error.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum LexerErrorData<Char = char, StateError = ()> {
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
  String(StringErrors<Char>),
  /// A character that can begin a token, where it begins none.
  ///
  /// This dialect's scanner lexes `+` and `-` as operators and raises none today. The variant
  /// mirrors GraphQL's, where only the scanner raises it and always over the one character it
  /// refused, which is what [`UnexpectedCharacter`](SyntaxErrorKind::UnexpectedCharacter) reports.
  /// A slice a whole-slice door refuses is [`NotOneToken`](Self::NotOneToken) instead.
  #[from(skip)]
  UnexpectedLexeme(Lexeme<Char>),
  /// The slice handed to a whole-slice door — [`identifier`](super::identifier), or the
  /// `TryFrom<&str>` of [`LitInt`](super::LitInt) or [`LitFloat`](super::LitFloat) — is not
  /// exactly one token of the kind the door asked for: a token of another kind, trivia around it,
  /// or a second token.
  ///
  /// The payload names that kind as a noun, `"name"`, `"integer"` or `"float"`, and the error
  /// renders "not exactly one" followed by it. The scanner never raises it. It reports
  /// [`UnexpectedToken`](SyntaxErrorKind::UnexpectedToken), and no character in the slice was
  /// refused.
  #[from(skip)]
  NotOneToken(&'static str),
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
  #[inline(always)]
  fn default() -> Self {
    Self::Other(Cow::Borrowed("unknown"))
  }
}

impl<Char, StateError> LexerErrorData<Char, StateError> {
  /// Returns the syntax-error kind this refusal reports as.
  ///
  /// A `String` batch reports its first string error's kind, and an empty one reports
  /// [`InvalidSyntax`](SyntaxErrorKind::InvalidSyntax). A `State` refusal is a tripped
  /// `smear_lexer::limits` ceiling — the nesting depth, or the lossless lexer's token budget —
  /// and reports [`ResourceLimit`](SyntaxErrorKind::ResourceLimit) whichever one tripped, since
  /// nothing here bounds `StateError` to say which.
  pub fn kind(&self) -> SyntaxErrorKind {
    match self {
      Self::Float(error) => error.kind(),
      Self::HexFloat(error) => error.kind(),
      Self::Decimal(error) => error.kind(),
      Self::Hex(error) => error.kind(),
      Self::Octal(error) => error.kind(),
      Self::Binary(error) => error.kind(),
      Self::String(errors) => errors
        .iter()
        .next()
        .map_or(SyntaxErrorKind::InvalidSyntax, StringError::kind),
      Self::UnexpectedLexeme(_) => SyntaxErrorKind::UnexpectedCharacter,
      Self::NotOneToken(_) => SyntaxErrorKind::UnexpectedToken,
      Self::UnknownLexeme(_) => SyntaxErrorKind::UnknownCharacter,
      Self::UnexpectedEndOfInput => SyntaxErrorKind::UnexpectedEndOfInput,
      Self::UnterminatedSpreadOperator => SyntaxErrorKind::UnterminatedSpread,
      Self::State(_) => SyntaxErrorKind::ResourceLimit,
      Self::InvalidUtf8(_) => SyntaxErrorKind::InvalidUtf8,
      Self::Other(_) => SyntaxErrorKind::InvalidSyntax,
    }
  }

  /// Writes the sentence this refusal renders as: the kind's message, with a more specific clause
  /// where the data has one, and no position.
  fn describe(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Float(error) => error.describe(f),
      Self::HexFloat(error) => error.describe(f),
      Self::Decimal(error) => error.describe(f),
      Self::Hex(error) => error.describe(f),
      Self::Octal(error) => error.describe(f),
      Self::Binary(error) => error.describe(f),
      Self::String(errors) => match errors.iter().next() {
        Some(error) => describe::string_error(error, f),
        None => f.write_str(SyntaxErrorKind::InvalidSyntax.message()),
      },
      Self::UnterminatedSpreadOperator => {
        f.write_str("unterminated spread operator, expected `...`")
      }
      Self::NotOneToken(what) => {
        f.write_str("not exactly one ")?;
        f.write_str(what)
      }
      Self::Other(message) => f.write_str(message),
      Self::UnexpectedLexeme(_)
      | Self::UnknownLexeme(_)
      | Self::UnexpectedEndOfInput
      | Self::State(_)
      | Self::InvalidUtf8(_) => f.write_str(self.kind().message()),
    }
  }

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

  /// Creates new decimal error data.
  #[inline]
  pub const fn decimal(error: DecimalError<Char>) -> Self {
    Self::Decimal(error)
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
  #[inline(always)]
  fn default() -> Self {
    Self::unexpected_eoi(Span::new(0, 0))
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

  /// Creates a new hex float error.
  #[inline]
  pub const fn hex_float(span: Span, error: HexFloatError<Char>) -> Self {
    Self::const_new(span, LexerErrorData::HexFloat(error))
  }

  /// Creates a new hex error.
  #[inline]
  pub const fn hex(span: Span, error: HexError<Char>) -> Self {
    Self::const_new(span, LexerErrorData::Hex(error))
  }

  /// Creates a new octal error.
  #[inline]
  pub const fn octal(span: Span, error: OctalError<Char>) -> Self {
    Self::const_new(span, LexerErrorData::Octal(error))
  }

  /// Creates a new binary error.
  #[inline]
  pub const fn binary(span: Span, error: BinaryError<Char>) -> Self {
    Self::const_new(span, LexerErrorData::Binary(error))
  }

  /// Creates a new decimal error.
  #[inline]
  pub const fn decimal(span: Span, error: DecimalError<Char>) -> Self {
    Self::const_new(span, LexerErrorData::Decimal(error))
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

  /// Returns the syntax-error kind this refusal reports as, which is its data's.
  #[inline]
  pub fn kind(&self) -> SyntaxErrorKind {
    self.data.kind()
  }
}

/// A lexical refusal, as a sentence: the kind's message, with a more specific clause where the
/// data has one — "unknown escape sequence", "incomplete number, expected a hex digit".
///
/// **Never a position**, and nothing quoted from the source, for the reasons GraphQL's twin
/// records — which is also why this needs no bound on `Char`.
impl<Char, StateError> core::fmt::Display for LexerError<Char, StateError> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.data.describe(f)
  }
}

impl<Char, StateError> core::error::Error for LexerError<Char, StateError>
where
  Char: core::fmt::Debug,
  StateError: core::fmt::Debug,
{
}

/// A lexical refusal, read as structure: its [`SyntaxErrorKind`]'s code, severity and help, and
/// its own span in the one document a lex reads. No labels and no path segments.
impl<Char, StateError> Diagnose for LexerError<Char, StateError> {
  #[inline]
  fn code(&self) -> Code {
    self.kind().code()
  }

  #[inline]
  fn severity(&self) -> Severity {
    self.kind().severity()
  }

  #[inline]
  fn primary(&self) -> Location {
    Location::new(SOURCE, self.span)
  }

  #[inline]
  fn primary_label(&self) -> Option<&'static str> {
    Some(self.kind().message())
  }

  #[inline]
  fn labels(&self) -> usize {
    0
  }

  #[inline]
  fn label(&self, _: usize) -> Option<Label> {
    None
  }

  #[inline]
  fn path_segments(&self) -> usize {
    0
  }

  #[inline]
  fn path_segment(&self, _: usize) -> Option<PathSegment<'_>> {
    None
  }

  #[inline]
  fn help(&self) -> Option<&'static str> {
    self.kind().help()
  }
}

impl<Char, StateError> From<LexerError<Char, StateError>> for LexerErrorData<Char, StateError> {
  #[inline]
  fn from(error: LexerError<Char, StateError>) -> Self {
    error.into_data()
  }
}

type DefaultErrorsContainer<Char = char, StateError = ()> =
  std::vec::Vec<LexerError<Char, StateError>>;

/// A container for storing multiple lexer errors.
#[derive(Debug, Clone, PartialEq, Eq, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct LexerErrors<Char = char, StateError = ()>(DefaultErrorsContainer<Char, StateError>);

impl<Char, StateError> Default for LexerErrors<Char, StateError> {
  #[inline(always)]
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

impl<Char, StateError> From<StateError> for LexerErrors<Char, StateError> {
  #[inline]
  fn from(error: StateError) -> Self {
    LexerError::const_new(Span::new(0, 0), LexerErrorData::State(error)).into()
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
