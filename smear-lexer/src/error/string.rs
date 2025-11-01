use std::borrow::Cow;

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme};

use crate::hints::{LineTerminatorHint, LitStrDelimiterHint, UnpairedSurrogateHint};

use super::LengthError;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
enum InvalidUnicodeHexDigitsRepr<Char = char> {
  #[default]
  Zero,
  One(PositionedChar<Char>),
  Two([PositionedChar<Char>; 2]),
  Three([PositionedChar<Char>; 3]),
  Four([PositionedChar<Char>; 4]),
}

impl<Char> InvalidUnicodeHexDigitsRepr<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn len(&self) -> usize {
    match self {
      Self::Zero => 0,
      Self::One(_) => 1,
      Self::Two(_) => 2,
      Self::Three(_) => 3,
      Self::Four(_) => 4,
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
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

  #[cfg_attr(not(tarpaulin), inline(always))]
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

/// A container designed for storing between `1..=4` invalid unicode hex digits.
///
/// - For fixed-width unicode escapes (\uXXXX), we allow up to 4 digits.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnicodeHexDigits<Char = char>(InvalidUnicodeHexDigitsRepr<Char>);

impl<Char> Default for InvalidUnicodeHexDigits<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Zero)
  }
}

impl<Char> From<PositionedChar<Char>> for InvalidUnicodeHexDigits<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(c: PositionedChar<Char>) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::One(c))
  }
}

impl<Char> From<[PositionedChar<Char>; 2]> for InvalidUnicodeHexDigits<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(c: [PositionedChar<Char>; 2]) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Two(c))
  }
}

impl<Char> From<[PositionedChar<Char>; 3]> for InvalidUnicodeHexDigits<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(c: [PositionedChar<Char>; 3]) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Three(c))
  }
}

impl<Char> From<[PositionedChar<Char>; 4]> for InvalidUnicodeHexDigits<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(c: [PositionedChar<Char>; 4]) -> Self {
    Self(InvalidUnicodeHexDigitsRepr::Four(c))
  }
}

impl<Char> InvalidUnicodeHexDigits<Char> {
  /// Returns the length of the invalid unicode sequence.
  ///
  /// The returned length will be in the range of `1..=4`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn len(&self) -> usize {
    self.0.len()
  }

  /// Returns `true` if the length of the invalid unicode sequence is empty.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_empty(&self) -> bool {
    matches!(self.0, InvalidUnicodeHexDigitsRepr::Zero)
  }

  /// Bumps the position of all characters by `n`.
  #[inline]
  pub(crate) const fn bump(&mut self, n: usize) -> &mut Self {
    self.0.bump(n);
    self
  }

  /// # Panics
  /// - If the length is already 4.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) fn push_fixed(&mut self, ch: PositionedChar<Char>) {
    self.0.push(ch);
  }
}

impl<Char> AsRef<[PositionedChar<Char>]> for InvalidUnicodeHexDigits<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &[PositionedChar<Char>] {
    self
  }
}

impl<Char> AsMut<[PositionedChar<Char>]> for InvalidUnicodeHexDigits<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_mut(&mut self) -> &mut [PositionedChar<Char>] {
    self
  }
}

impl<Char> core::ops::Deref for InvalidUnicodeHexDigits<Char> {
  type Target = [PositionedChar<Char>];

  #[cfg_attr(not(tarpaulin), inline(always))]
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
  #[cfg_attr(not(tarpaulin), inline(always))]
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
pub struct InvalidUnicodeSequence<Char = char> {
  digits: InvalidUnicodeHexDigits<Char>,
  span: Span,
}

impl<Char> InvalidUnicodeSequence<Char> {
  /// Create a new invalid unicode sequence error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn fixed(digits: InvalidUnicodeHexDigits<Char>, span: Span) -> Self {
    Self { digits, span }
  }

  /// Returns `true` if the sequence also incomplete.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_incomplete(&self) -> bool {
    self.span.len() < 6 // \u[0-9a-fA-F]{4} is 6 characters long
  }

  /// Get the invalid unicode hex digits.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn digits(&self) -> InvalidUnicodeHexDigits<Char>
  where
    Char: Copy,
  {
    self.digits
  }

  /// Get the reference to the invalid unicode hex digits.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn digits_ref(&self) -> &InvalidUnicodeHexDigits<Char> {
    &self.digits
  }

  /// Get the mutable reference to the invalid unicode hex digits.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn digits_mut(&mut self) -> &mut InvalidUnicodeHexDigits<Char> {
    &mut self.digits
  }

  /// Get the span of the invalid unicode sequence.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the span of the invalid unicode sequence as a reference.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_ref(&self) -> &Span {
    &self.span
  }

  /// Get the mutable span of the invalid unicode sequence.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Bumps the span of the invalid unicode sequence by `n`.
  #[inline]
  pub(crate) const fn bump(&mut self, n: usize) -> &mut Self {
    self.span.bump_span(n);
    self.digits_mut().bump(n);
    self
  }
}

/// Why a parsed value is *not* a valid Unicode scalar.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InvalidUnicodeScalarKind {
  /// In the UTF-16 surrogate range: 0xD800..=0xDFFF.
  Surrogate,
  /// Above the Unicode max scalar value: > 0x10_FFFF.
  Overflow,
}

/// An invalid unicode scalar value error.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnicodeScalarValue {
  value: u32,
  span: Span,
  kind: InvalidUnicodeScalarKind,
}

impl InvalidUnicodeScalarValue {
  /// Create a new invalid unicode scalar value error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn new(value: u32, span: Span, kind: InvalidUnicodeScalarKind) -> Self {
    Self { value, span, kind }
  }

  /// Get the invalid unicode scalar codepoint.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn codepoint(&self) -> u32 {
    self.value
  }

  /// Get the span of the invalid unicode scalar value.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the span of the invalid unicode scalar value as a reference.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_ref(&self) -> &Span {
    &self.span
  }

  /// Get the mutable span of the invalid unicode scalar value.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Bumps the span of the invalid unicode scalar value by `n`.
  #[inline]
  pub(crate) const fn bump(&mut self, n: usize) -> &mut Self {
    self.span.bump_span(n);
    self
  }

  /// Returns the kind of invalid unicode scalar value.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> InvalidUnicodeScalarKind {
    self.kind
  }
}

/// An error encountered during lexing for `\u{...}` (braced) unicode sequences.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BracedUnicodeEscapeError {
  /// The opening brace was not closed: `\u{1234`.
  Unclosed(Span),

  /// The braces contained **no** digits: `\u{}`.
  Empty(Span),

  /// More than 6 hex digits inside the braces.
  TooManyDigits {
    /// The span of the entire braced unicode escape.
    span: Span,
    /// The count of digits found.
    count: usize,
  },

  /// An invalid sequence of unicode in the braces.
  InvalidSequence(Span),

  /// Parsed number is not a Unicode scalar value (surrogate or > 0x10_FFFF).
  InvalidScalar(InvalidUnicodeScalarValue),
}

impl BracedUnicodeEscapeError {
  /// Creates an empty braced unicode escape error.
  #[inline]
  pub const fn empty(span: Span) -> Self {
    Self::Empty(span)
  }

  /// Creates a too many digits in braced unicode escape error.
  #[inline]
  pub const fn too_many_digits(span: Span, count: usize) -> Self {
    Self::TooManyDigits { span, count }
  }

  /// Creates an unclosed brace in braced unicode escape error.
  #[inline]
  pub const fn unclosed(span: Span) -> Self {
    Self::Unclosed(span)
  }

  /// Creates an overflow error.
  #[inline]
  pub const fn overflow(span: Span, codepoint: u32) -> Self {
    Self::InvalidScalar(InvalidUnicodeScalarValue::new(
      codepoint,
      span,
      InvalidUnicodeScalarKind::Overflow,
    ))
  }

  /// Creates an surrogate error.
  #[inline]
  pub const fn surrogate(span: Span, codepoint: u32) -> Self {
    Self::InvalidScalar(InvalidUnicodeScalarValue::new(
      codepoint,
      span,
      InvalidUnicodeScalarKind::Surrogate,
    ))
  }

  /// Bumps the span of the error by `n`.
  #[inline]
  pub(crate) const fn bump(&mut self, n: usize) -> &mut Self {
    match self {
      Self::Unclosed(span) => {
        span.bump_span(n);
      }
      Self::Empty(span) => {
        span.bump_span(n);
      }
      Self::TooManyDigits { span, .. } => {
        span.bump_span(n);
      }
      Self::InvalidSequence(span) => {
        span.bump_span(n);
      }
      Self::InvalidScalar(err) => {
        err.bump(n);
      }
    }
    self
  }
}

/// An error encountered during lexing for `\uXXXX` (fixed-width) unicode sequences.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum FixedUnicodeEscapeError<Char = char> {
  /// An incomplete fixed-width unicode escape sequence.
  #[from(skip)]
  Incomplete(Lexeme<Char>),
  /// An invalid fixed-width unicode escape sequence.
  InvalidSequence(InvalidUnicodeSequence<Char>),
  /// An unpaired surrogate in a fixed-width unicode escape sequence.
  UnpairedSurrogate(UnexpectedLexeme<Char, UnpairedSurrogateHint>),
}

impl<Char> FixedUnicodeEscapeError<Char> {
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
  pub(crate) const fn bump(&mut self, n: usize) -> &mut Self {
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

/// An error encountered during lexing for unicode sequences.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum UnicodeError<Char = char> {
  /// An error in a fixed-width unicode escape sequence (`\uXXXX`).
  Fixed(FixedUnicodeEscapeError<Char>),
  /// An error in a braced unicode escape sequence (`\u{...}`).
  Braced(BracedUnicodeEscapeError),
}

impl<Char> UnicodeError<Char> {
  /// Creates a unpaired high surrogate error.
  #[inline]
  pub const fn unpaired_high_surrogate(lexeme: Lexeme<Char>) -> Self {
    Self::Fixed(FixedUnicodeEscapeError::unpaired_high_surrogate(lexeme))
  }

  /// Creates a unpaired low surrogate error.
  #[inline]
  pub const fn unpaired_low_surrogate(lexeme: Lexeme<Char>) -> Self {
    Self::Fixed(FixedUnicodeEscapeError::unpaired_low_surrogate(lexeme))
  }

  /// Creates an incomplete fixed-width unicode escape error.
  #[inline]
  pub const fn incomplete_fixed_unicode_escape(lexeme: Lexeme<Char>) -> Self {
    Self::Fixed(FixedUnicodeEscapeError::Incomplete(lexeme))
  }

  /// Creates an invalid fixed-width unicode escape sequence error.
  #[inline]
  pub const fn invalid_fixed_unicode_escape_sequence(
    digits: InvalidUnicodeHexDigits<Char>,
    span: Span,
  ) -> Self {
    Self::Fixed(FixedUnicodeEscapeError::InvalidSequence(
      InvalidUnicodeSequence::fixed(digits, span),
    ))
  }

  /// Creates an empty braced unicode escape error.
  #[inline]
  pub const fn empty_braced_unicode_escape(span: Span) -> Self {
    Self::Braced(BracedUnicodeEscapeError::Empty(span))
  }

  /// Creates a too many digits in braced unicode escape error.
  #[inline]
  pub const fn too_many_digits_in_braced_unicode_escape(span: Span, count: usize) -> Self {
    Self::Braced(BracedUnicodeEscapeError::TooManyDigits { span, count })
  }

  /// Creates an unclosed brace in braced unicode escape error.
  #[inline]
  pub const fn unclosed_braced_unicode_escape(span: Span) -> Self {
    Self::Braced(BracedUnicodeEscapeError::unclosed(span))
  }

  /// Creates an surrogate in braced unicode escape error.
  #[inline]
  pub const fn surrogate_braced_unicode_escape(span: Span, codepoint: u32) -> Self {
    Self::Braced(BracedUnicodeEscapeError::surrogate(span, codepoint))
  }

  /// Creates an overflow in braced unicode escape error.
  #[inline]
  pub const fn overflow_braced_unicode_escape(span: Span, codepoint: u32) -> Self {
    Self::Braced(BracedUnicodeEscapeError::overflow(span, codepoint))
  }

  /// Creates an invalid braced unicode escape sequence error.
  #[inline]
  pub const fn invalid_braced_unicode_escape_sequence(span: Span) -> Self {
    Self::Braced(BracedUnicodeEscapeError::InvalidSequence(span))
  }

  /// Bumps the span or position of the error by `n`.
  #[inline]
  pub(crate) const fn bump(&mut self, n: usize) -> &mut Self {
    match self {
      Self::Fixed(err) => {
        err.bump(n);
      }
      Self::Braced(err) => {
        err.bump(n);
      }
    }
    self
  }
}

/// An escaped character in a GraphQL inline string.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EscapedCharacter<Char = char> {
  char: PositionedChar<Char>,
  span: Span,
}

impl<Char> EscapedCharacter<Char> {
  /// Create a new escaped character error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn new(char: PositionedChar<Char>, span: Span) -> Self {
    Self { char, span }
  }

  /// Get the escaped character.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn char(&self) -> Char
  where
    Char: Copy,
  {
    self.char.char()
  }

  /// Get the reference to the escaped character.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn char_ref(&self) -> &Char {
    self.char.char_ref()
  }

  /// Get the mutable reference to the escaped character.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn char_mut(&mut self) -> &mut Char {
    self.char.char_mut()
  }

  /// Returns the position of the escaped character.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn position(&self) -> usize {
    self.char.position()
  }

  /// Get the span of the escaped character. The span covers the entire escape sequence, e.g. `\d`, `\r`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the reference to the span of the escaped character.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_ref(&self) -> &Span {
    &self.span
  }

  /// Get the mutable reference to the span of the escaped character.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Bumps the span and the position of the character by `n`.
  #[inline]
  pub(crate) const fn bump(&mut self, n: usize) -> &mut Self {
    self.span.bump_span(n);
    self.char.bump_position(n);
    self
  }
}

/// An error encountered during lexing for string literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum StringError<Char = char> {
  /// An unsupported character in a string literal.
  #[from(skip)]
  UnsupportedCharacter(Lexeme<Char>),
  /// An unexpected line terminator in a string literal.
  UnexpectedLineTerminator(UnexpectedLexeme<Char, LineTerminatorHint>),
  /// An unexpected escaped character in a string literal.
  UnexpectedEscapedCharacter(EscapedCharacter<Char>),
  /// A unopened string literal. e.g. a string literal that starts with a non-quote character.
  Unopened(UnexpectedLexeme<Option<Char>, LitStrDelimiterHint>),
  /// An unterminated string literal.
  Unterminated(UnexpectedEnd<LitStrDelimiterHint>),
  /// A unicode error in a string literal.
  Unicode(UnicodeError<Char>),
  /// Any other error in a string literal.
  Other(Cow<'static, str>),
}

impl<Char> Default for StringError<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
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
      LitStrDelimiterHint::Quote,
    ))
  }

  /// Creates an unterminated block string error.
  #[inline]
  pub const fn unterminated_block_string() -> Self {
    Self::Unterminated(UnexpectedEnd::with_name(
      Cow::Borrowed("string value"),
      LitStrDelimiterHint::TripleQuote,
    ))
  }

  /// Creates an expected open delimiter error.
  #[inline]
  pub const fn unopened_string(ch: Option<Char>, position: usize) -> Self {
    Self::Unopened(UnexpectedLexeme::from_char(
      PositionedChar::with_position(ch, position),
      LitStrDelimiterHint::QuoteOrTripleQuote,
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
