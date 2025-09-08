use core::fmt;

use std::borrow::Cow;

use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme};

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
  UnexpectedEnd(UnexpectedEnd<FloatHint>),
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
//       Self::UnexpectedEnd(e) => {
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
//       ExponentError::UnexpectedEnd(e) => Self::UnexpectedEnd(e.into()),
//       ExponentError::UnexpectedLexeme(e) => Self::UnexpectedCharacter(e.into()),
//     }
//   }
// }

// impl<Char> From<FractionalError<Char>> for FloatError<Char> {
//   #[inline]
//   fn from(e: FractionalError<Char>) -> Self {
//     match e {
//       FractionalError::UnexpectedCharacter(e) => Self::UnexpectedCharacter(e.into()),
//       FractionalError::UnexpectedEnd(e) => Self::UnexpectedEnd(e.into()),
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
//     Self::UnexpectedEnd(e.into())
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
//     Self::UnexpectedEnd(e.into())
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
  UnexpectedEnd(UnexpectedEnd<IntHint>),
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

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
enum InvalidUnicodeHexDigitsRepr<Char> {
  #[default]
  Zero,
  One(PositionedChar<Char>),
  Two([PositionedChar<Char>; 2]),
  Three([PositionedChar<Char>; 3]),
  Four([PositionedChar<Char>; 4]),
}

impl<Char> InvalidUnicodeHexDigitsRepr<Char> {
  #[inline(always)]
  const fn len(&self) -> usize {
    match self {
      Self::Zero => 0,
      Self::One(_) => 1,
      Self::Two(_) => 2,
      Self::Three(_) => 3,
      Self::Four(_) => 4,
    }
  }

  #[inline(always)]
  const fn bump(&mut self, n: usize) -> &mut Self {
    match self {
      Self::Zero => {}
      Self::One(a) => {
        a.bump_position(n);
      }
      Self::Two([a, b]) => {
        a.bump_position(n);
        b.bump_position(n);
      }
      Self::Three([a, b, c]) => {
        a.bump_position(n);
        b.bump_position(n);
        c.bump_position(n);
      }
      Self::Four([a, b, c, d]) => {
        a.bump_position(n);
        b.bump_position(n);
        c.bump_position(n);
        d.bump_position(n);
      }
    }
    self
  }

  #[inline(always)]
  fn push(&mut self, ch: PositionedChar<Char>) {
    *self = match core::mem::take(self) {
      Self::Zero => Self::One(ch),
      Self::One(a) => Self::Two([a, ch]),
      Self::Two([a, b]) => Self::Three([a, b, ch]),
      Self::Three([a, b, c]) => Self::Four([a, b, c, ch]),
      Self::Four(_) => {
        panic!("InvalidUnicodeHexDigits can only hold up to 4 digits, tried to push a 5th");
      }
    };
  }
}

/// A container designed for storing between 1 and 4 invalid unicode hex digits.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnicodeHexDigits<Char>(InvalidUnicodeHexDigitsRepr<Char>);

impl<Char> Default for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn default() -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Zero)
  }
}

impl<Char> From<PositionedChar<Char>> for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn from(c: PositionedChar<Char>) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::One(c))
  }
}

impl<Char> From<[PositionedChar<Char>; 2]> for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn from(c: [PositionedChar<Char>; 2]) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Two(c))
  }
}

impl<Char> From<[PositionedChar<Char>; 3]> for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn from(c: [PositionedChar<Char>; 3]) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Three(c))
  }
}

impl<Char> From<[PositionedChar<Char>; 4]> for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn from(c: [PositionedChar<Char>; 4]) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Four(c))
  }
}

impl<Char> InvalidUnicodeHexDigits<Char> {
  /// Returns the length of the invalid unicode sequence.
  ///
  /// The returned length will be in the range of `1..=4`.
  #[inline(always)]
  pub const fn len(&self) -> usize {
    self.0.len()
  }

  /// Returns `true` if the length of the invalid unicode sequence is empty.
  #[inline(always)]
  pub const fn is_empty(&self) -> bool {
    matches!(self.0, InvalidUnicodeHexDigitsRepr::Zero)
  }

  /// Bumps the position of all characters by `n`.
  #[inline]
  pub const fn bump(&mut self, n: usize) -> &mut Self {
    self.0.bump(n);
    self
  }

  /// # Panics
  /// - If the length is already 4.
  #[inline(always)]
  pub(crate) fn push(&mut self, ch: PositionedChar<Char>) {
    self.0.push(ch);
  }
}

impl<Char> AsRef<[PositionedChar<Char>]> for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn as_ref(&self) -> &[PositionedChar<Char>] {
    self
  }
}

impl<Char> AsMut<[PositionedChar<Char>]> for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn as_mut(&mut self) -> &mut [PositionedChar<Char>] {
    self
  }
}

impl<Char> core::ops::Deref for InvalidUnicodeHexDigits<Char> {
  type Target = [PositionedChar<Char>];

  #[inline(always)]
  fn deref(&self) -> &Self::Target {
    match &self.0 {
      InvalidUnicodeHexDigitsRepr::Zero => &[],
      InvalidUnicodeHexDigitsRepr::One(a) => core::slice::from_ref(a),
      InvalidUnicodeHexDigitsRepr::Two(ab) => ab,
      InvalidUnicodeHexDigitsRepr::Three(abc) => abc,
      InvalidUnicodeHexDigitsRepr::Four(abcd) => abcd,
    }
  }
}

impl<Char> core::ops::DerefMut for InvalidUnicodeHexDigits<Char> {
  #[inline(always)]
  fn deref_mut(&mut self) -> &mut Self::Target {
    match &mut self.0 {
      InvalidUnicodeHexDigitsRepr::Zero => &mut [],
      InvalidUnicodeHexDigitsRepr::One(a) => core::slice::from_mut(a),
      InvalidUnicodeHexDigitsRepr::Two(ab) => ab,
      InvalidUnicodeHexDigitsRepr::Three(abc) => abc,
      InvalidUnicodeHexDigitsRepr::Four(abcd) => abcd,
    }
  }
}

/// An invalid unicode sequence error.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnicodeSequence<Char> {
  digits: InvalidUnicodeHexDigits<Char>,
  span: Span,
}

impl<Char> InvalidUnicodeSequence<Char> {
  /// Create a new invalid unicode sequence error.
  #[inline(always)]
  pub(crate) const fn new(digits: InvalidUnicodeHexDigits<Char>, span: Span) -> Self {
    Self { digits, span }
  }

  /// Returns `true` if the sequence also incomplete.
  #[inline(always)]
  pub const fn is_incomplete(&self) -> bool {
    self.span.len() < 6 // \u[0-9a-fA-F]{4} is 6 characters long
  }

  /// Get the invalid unicode hex digits.
  #[inline(always)]
  pub const fn digits(&self) -> InvalidUnicodeHexDigits<Char>
  where
    Char: Copy,
  {
    self.digits
  }

  /// Get the reference to the invalid unicode hex digits.
  #[inline(always)]
  pub const fn digits_ref(&self) -> &InvalidUnicodeHexDigits<Char> {
    &self.digits
  }

  /// Get the mutable reference to the invalid unicode hex digits.
  #[inline(always)]
  pub const fn digits_mut(&mut self) -> &mut InvalidUnicodeHexDigits<Char> {
    &mut self.digits
  }

  /// Get the span of the invalid unicode sequence.
  #[inline(always)]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the span of the invalid unicode sequence as a reference.
  #[inline(always)]
  pub const fn span_ref(&self) -> &Span {
    &self.span
  }

  /// Get the mutable span of the invalid unicode sequence.
  #[inline(always)]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Bumps the span of the invalid unicode sequence by `n`.
  #[inline]
  pub const fn bump(&mut self, n: usize) -> &mut Self {
    self.span.bump_span(n);
    self.digits_mut().bump(n);
    self
  }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Display, IsVariant)]
pub enum UnpairedSurrogateHint {
  #[display("high surrogate")]
  High,
  #[display("low surrogate")]
  Low,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum UnicodeError<Char> {
  #[from(skip)]
  Incomplete(Lexeme<Char>),
  InvalidSequence(InvalidUnicodeSequence<Char>),
  UnpairedSurrogate(UnexpectedLexeme<Char, UnpairedSurrogateHint>),
}

impl<Char> UnicodeError<Char> {
  /// Creates a unpaired high surrogate error.
  #[inline]
  pub const fn unpaired_high_surrogate(lexeme: Lexeme<Char>) -> Self {
    Self::UnpairedSurrogate(UnexpectedLexeme::new(lexeme, UnpairedSurrogateHint::High))
  }

  /// Creates a unpaired low surrogate error.
  #[inline]
  pub const fn unpaired_low_surrogate(lexeme: Lexeme<Char>) -> Self {
    Self::UnpairedSurrogate(UnexpectedLexeme::new(lexeme, UnpairedSurrogateHint::Low))
  }

  /// Bumps the span or position of the error by `n`.
  #[inline]
  pub const fn bump(&mut self, n: usize) -> &mut Self {
    match self {
      Self::Incomplete(lexeme) => {
        lexeme.bump(n);
      }
      Self::InvalidSequence(seq) => {
        seq.bump(n);
      }
      Self::UnpairedSurrogate(lexeme) => {
        lexeme.bump(n);
      }
    }
    self
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum UnterminatedHint {
  #[display("\"")]
  Quote,
  #[display(r#"""""#)]
  TripleQuote,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum LineTerminatorHint {
  #[display("'\\n'")]
  NewLine,
  #[display("'\\r'")]
  CarriageReturn,
  #[display("'\\r\\n'")]
  CarriageReturnNewLine,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EscapedCharacter<Char> {
  char: PositionedChar<Char>,
  span: Span,
}

impl<Char> EscapedCharacter<Char> {
  /// Create a new escaped character error.
  #[inline(always)]
  pub(crate) const fn new(char: PositionedChar<Char>, span: Span) -> Self {
    Self { char, span }
  }

  /// Get the escaped character.
  #[inline(always)]
  pub const fn char(&self) -> Char
  where
    Char: Copy,
  {
    self.char.char()
  }

  /// Get the reference to the escaped character.
  #[inline(always)]
  pub const fn char_ref(&self) -> &Char {
    self.char.char_ref()
  }

  /// Get the mutable reference to the escaped character.
  #[inline(always)]
  pub const fn char_mut(&mut self) -> &mut Char {
    self.char.char_mut()
  }

  /// Returns the position of the escaped character.
  #[inline(always)]
  pub const fn position(&self) -> usize {
    self.char.position()
  }

  /// Get the span of the escaped character. The span covers the entire escape sequence, e.g. `\d`, `\r`.
  #[inline(always)]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the reference to the span of the escaped character.
  #[inline(always)]
  pub const fn span_ref(&self) -> &Span {
    &self.span
  }

  /// Get the mutable reference to the span of the escaped character.
  #[inline(always)]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Bumps the span and the position of the character by `n`.
  #[inline]
  pub const fn bump(&mut self, n: usize) -> &mut Self {
    self.span.bump_span(n);
    self.char.bump_position(n);
    self
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum StringError<Char> {
  #[from(skip)]
  UnsupportedCharacter(Lexeme<Char>),
  UnexpectedLineTerminator(UnexpectedLexeme<Char, LineTerminatorHint>),
  UnexpectedEscapedCharacter(EscapedCharacter<Char>),
  Unterminated(UnexpectedEnd<UnterminatedHint>),
  Unicode(UnicodeError<Char>),
  Other(Cow<'static, str>),
}

impl<Char> Default for StringError<Char> {
  #[inline(always)]
  fn default() -> Self {
    Self::Other(Cow::Borrowed("unknown"))
  }
}

impl<Char> StringError<Char> {
  /// Creates an unterminated string error.
  #[inline]
  pub const fn unterminated_inline_string() -> Self {
    Self::Unterminated(UnexpectedEnd::with_name(
      Cow::Borrowed("string value"),
      UnterminatedHint::Quote,
    ))
  }

  /// Creates an unterminated block string error.
  #[inline]
  pub const fn unterminated_block_string() -> Self {
    Self::Unterminated(UnexpectedEnd::with_name(
      Cow::Borrowed("string value"),
      UnterminatedHint::TripleQuote,
    ))
  }

  /// Creates an unexpected new line error.
  #[inline]
  pub const fn unexpected_new_line(ch: Char, position: usize) -> Self {
    Self::UnexpectedLineTerminator(UnexpectedLexeme::from_char(
      PositionedChar::with_position(ch, position),
      LineTerminatorHint::NewLine,
    ))
  }

  /// Creates an unexpected carriage return error.
  #[inline]
  pub const fn unexpected_carriage_return(ch: Char, position: usize) -> Self {
    Self::UnexpectedLineTerminator(UnexpectedLexeme::from_char(
      PositionedChar::with_position(ch, position),
      LineTerminatorHint::CarriageReturn,
    ))
  }

  /// Creates an unexpected carriage return + new line error.
  #[inline]
  pub const fn unexpected_carriage_return_new_line(span: Span) -> Self {
    Self::UnexpectedLineTerminator(UnexpectedLexeme::from_span_const(
      span,
      LineTerminatorHint::CarriageReturnNewLine,
    ))
  }

  /// Creates an unexpected escaped character error.
  #[inline]
  pub(crate) const fn unexpected_escaped_character(span: Span, ch: Char, position: usize) -> Self {
    Self::UnexpectedEscapedCharacter(EscapedCharacter::new(
      PositionedChar::with_position(ch, position),
      span,
    ))
  }

  /// Bumps the span or position of the error by `n`.
  #[inline]
  pub fn bump(&mut self, n: usize) -> &mut Self {
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
      Self::Other(_) | Self::Unterminated(_) => {}
    };

    self
  }
}

#[cfg(feature = "smallvec")]
type DefaultStringErrorsContainer<Char> = smallvec::SmallVec<[StringError<Char>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultStringErrorsContainer<Char> = std::vec::Vec<StringError<Char>>;

#[derive(
  Debug,
  Default,
  Clone,
  PartialEq,
  Eq,
  Hash,
  derive_more::From,
  derive_more::Into,
  derive_more::Deref,
  derive_more::DerefMut,
  derive_more::AsMut,
  derive_more::AsRef,
)]
pub struct StringErrors<Char>(DefaultStringErrorsContainer<Char>);

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

/// An error encountered during lexing tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ErrorData<Char> {
  /// An error encountered during lexing for float literals.
  Float(FloatError<Char>),
  /// An error encountered during lexing for integer literals.
  Int(IntError<Char>),
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
  span: Span,
  data: ErrorData<Char>,
}

impl<Char> Default for Error<Char> {
  #[inline(always)]
  fn default() -> Self {
    Self::unexpected_eoi(Span::from(0..0))
  }
}

impl<Char> Error<Char> {
  /// Create a new error with the given span and data.
  #[inline]
  pub const fn const_new(span: Span, data: ErrorData<Char>) -> Self {
    Self { span, data }
  }

  /// Create a new error with the given span and data.
  #[inline]
  pub fn new(span: impl Into<Span>, data: ErrorData<Char>) -> Self {
    Self {
      span: span.into(),
      data,
    }
  }

  /// Creates a new float error.
  #[inline]
  pub const fn float(span: Span, error: FloatError<Char>) -> Self {
    Self::const_new(span, ErrorData::Float(error))
  }

  /// Creates a new int error.
  #[inline]
  pub const fn int(span: Span, error: IntError<Char>) -> Self {
    Self::const_new(span, ErrorData::Int(error))
  }

  /// Creates a new unexpected lexeme error.
  #[inline]
  pub const fn unexpected_lexeme(span: Span, lexeme: Lexeme<Char>) -> Self {
    Self::const_new(span, ErrorData::UnexpectedLexeme(lexeme))
  }

  /// Creates a new unknown lexeme error.
  #[inline]
  pub const fn unknown_lexeme(span: Span, lexeme: Lexeme<Char>) -> Self {
    Self::const_new(span, ErrorData::UnknownLexeme(lexeme))
  }

  /// Creates a new unexpected lexeme error from a positioned character.
  #[inline]
  pub const fn unexpected_char(span: Span, char: Char, position: usize) -> Self {
    Self::const_new(span, ErrorData::unexpected_char(char, position))
  }

  /// Creates a new unknown lexeme error from a positioned character.
  #[inline]
  pub const fn unknown_char(span: Span, char: Char, position: usize) -> Self {
    Self::const_new(span, ErrorData::unknown_char(char, position))
  }

  /// Creates an End of Input error.
  #[inline]
  pub const fn unexpected_eoi(span: Span) -> Self {
    Self::const_new(span, ErrorData::UnexpectedEndOfInput)
  }

  /// Get the span of the error. The span contains the start and end byte indices in the source,
  /// where the error occurred, the data may also contain a span for more precise information.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the data of the error.
  #[inline]
  pub const fn data(&self) -> &ErrorData<Char> {
    &self.data
  }

  /// Get the mutable data of the error.
  #[inline]
  pub fn data_mut(&mut self) -> &mut ErrorData<Char> {
    &mut self.data
  }

  /// Consume the error and return the error data.
  #[inline]
  pub fn into_data(self) -> ErrorData<Char> {
    self.data
  }

  /// Consumes the error and returns its components.
  #[inline]
  pub fn into_components(self) -> (Span, ErrorData<Char>) {
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
type DefaultErrorsContainer<Char> = smallvec::SmallVec<[Error<Char>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultErrorsContainer<Char> = std::vec::Vec<Error<Char>>;

#[derive(
  Debug,
  Default,
  Clone,
  PartialEq,
  Eq,
  Hash,
  derive_more::From,
  derive_more::Into,
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

impl<Char> TryFrom<StringErrors<Char>> for StringError<Char> {
  type Error = LengthError;

  #[inline]
  fn try_from(value: StringErrors<Char>) -> Result<Self, Self::Error> {
    match value.len() {
      0 => Err(LengthError::Empty),
      1 => Ok(value.0.into_iter().next().unwrap()),
      _ => Err(LengthError::TooManyErrors),
    }
  }
}
