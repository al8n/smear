use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::{SimpleSpan, state::RecursionLimitExceeded, utils::DisplayHuman};

use crate::{
  LitBlockStr, LitComplexBlockStr, LitComplexInlineStr, LitInlineStr, LitPlainBlockStr,
  LitPlainInlineStr, LitStr, skip_block_str_from_bytes, skip_inline_str_simd,
  string_lexer::byte_str::ByteStrBody,
};

/// Errors for GraphQLx lexers
pub mod error;

/// GraphQLx contextual keywords recognized from identifier spellings.
pub mod keyword;

/// Syntactic tokens for GraphQLx - fast lexing that skips trivia.
///
/// This module provides [`SyntacticToken`](syntactic::SyntacticToken), which is optimized for
/// high-performance parsing by automatically filtering out whitespace, comments, and commas.
///
/// **Use this for**: GraphQLx servers, query execution, schema compilation, and any
/// performance-critical parsing where you don't need to preserve formatting.
///
/// **Key benefits**:
/// - Minimal memory footprint
/// - Maximum parsing speed
/// - Zero-copy token references
/// - Supports GraphQLx extensions (generics, `use` items, type paths, etc.)
///
/// See [`SyntacticToken`](syntactic::SyntacticToken) for detailed documentation.
pub mod syntactic;

/// Lossless tokens for GraphQLx - complete source preservation.
///
/// This module provides [`LosslessToken`](lossless::LosslessToken), which preserves all source
/// information including whitespace, comments, and formatting. Essential for developer tools
/// that need to maintain or manipulate source code without losing information.
///
/// **Use this for**: Code formatters, linters, IDEs, syntax highlighters, documentation tools,
/// and any application that needs perfect source reconstruction.
///
/// **Key benefits**:
/// - Complete source fidelity
/// - Access to all comments and formatting
/// - Build Concrete Syntax Trees (CST)
/// - Supports GraphQLx extensions (generics, `use` items, type paths, etc.)
///
/// See [`LosslessToken`](lossless::LosslessToken) for detailed documentation.
pub mod lossless;

pub use keyword::ContextualKeyword;

mod handlers;

/// The re-cooking doors' own cells — see [`scan_one_token`].
#[cfg(test)]
#[path = "number/tests.rs"]
mod number_tests;

#[cfg(test)]
mod block_tests;

/// A GraphQLx integer literal, which can be in decimal, hexadecimal, binary, or octal format.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, TryUnwrap, Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitInt<S> {
  /// A decimal integer literal.
  Decimal(S),
  /// A hexadecimal integer literal.
  Hex(S),
  /// A binary integer literal.
  Binary(S),
  /// An octal integer literal.
  Octal(S),
}

impl<S> AsRef<S> for LitInt<S> {
  #[inline(always)]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitInt<&str> {
  #[inline(always)]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitInt<&[u8]> {
  #[inline(always)]
  fn as_ref(&self) -> &[u8] {
    self.source_ref()
  }
}

impl<S: core::fmt::Display> core::fmt::Display for LitInt<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S: DisplayHuman> DisplayHuman for LitInt<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> LitInt<S> {
  /// Returns the underlying source.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Decimal(s) => *s,
      Self::Hex(s) => *s,
      Self::Binary(s) => *s,
      Self::Octal(s) => *s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Decimal(s) => s,
      Self::Hex(s) => s,
      Self::Binary(s) => s,
      Self::Octal(s) => s,
    }
  }
}

/// A GraphQLx float literal, which can be in decimal or hexadecimal format.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, TryUnwrap, Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitFloat<S> {
  /// A decimal float literal.
  Decimal(S),
  /// A hexadecimal float literal.
  Hex(S),
}

impl<S> AsRef<S> for LitFloat<S> {
  #[inline(always)]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitFloat<&str> {
  #[inline(always)]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitFloat<&[u8]> {
  #[inline(always)]
  fn as_ref(&self) -> &[u8] {
    self.source_ref()
  }
}

impl<S: DisplayHuman> DisplayHuman for LitFloat<S> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S: core::fmt::Display> core::fmt::Display for LitFloat<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> LitFloat<S> {
  /// Returns the underlying source.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Decimal(s) => *s,
      Self::Hex(s) => *s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Decimal(s) => s,
      Self::Hex(s) => s,
    }
  }
}

/// The suffix of a GraphQLx integer literal: `i` or `u`, and a width, as in `1u8` or `-3i256`.
///
/// The width is the one the literal spells. Whether a program may write it is the analyzer's to
/// say: `1u7` lexes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntSuffix {
  signed: bool,
  width: u32,
}

impl IntSuffix {
  /// Whether the suffix is `i`, a signed integer's.
  #[inline(always)]
  pub const fn is_signed(&self) -> bool {
    self.signed
  }

  /// The width the suffix spells: 8 for `u8`.
  #[inline(always)]
  pub const fn width(&self) -> u32 {
    self.width
  }
}

/// The suffix of a GraphQLx float literal, as in `1.5f32` or `1f64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum FloatSuffix {
  /// `f32`.
  F32,
  /// `f64`.
  F64,
}

/// The integer suffix `bytes` end with, and where it starts: at the one `_` directly before its
/// letter, or at the letter.
///
/// A suffix is `i` or `u` and a width, `0` or a digit other than `0` and more digits, that fits a
/// `u32`.
fn int_suffix_of(bytes: &[u8]) -> Option<(usize, IntSuffix)> {
  let digits = bytes
    .iter()
    .rev()
    .take_while(|byte: &&u8| byte.is_ascii_digit())
    .count();
  if digits == 0 {
    return None;
  }
  let letter = bytes.len().checked_sub(digits.checked_add(1)?)?;
  let signed = match bytes.get(letter)? {
    b'i' => true,
    b'u' => false,
    _ => return None,
  };
  let width_digits: &[u8] = bytes.get(letter.checked_add(1)?..)?;
  if width_digits.len() > 1 && width_digits.first() == Some(&b'0') {
    return None;
  }
  let width = width_digits
    .iter()
    .try_fold(0u32, |width: u32, digit: &u8| {
      width
        .checked_mul(10)?
        .checked_add(u32::from(digit.wrapping_sub(b'0')))
    })?;
  Some((suffix_start(bytes, letter), IntSuffix { signed, width }))
}

/// The float suffix `bytes` end with, and where it starts, as [`int_suffix_of`] answers it.
fn float_suffix_of(bytes: &[u8]) -> Option<(usize, FloatSuffix)> {
  let suffix = match bytes {
    [.., b'f', b'3', b'2'] => FloatSuffix::F32,
    [.., b'f', b'6', b'4'] => FloatSuffix::F64,
    _ => return None,
  };
  let letter = bytes.len().checked_sub(3)?;
  Some((suffix_start(bytes, letter), suffix))
}

/// Where a suffix whose letter is at `letter` starts: at the `_` directly before it, if one is.
fn suffix_start(bytes: &[u8], letter: usize) -> usize {
  match letter.checked_sub(1) {
    Some(before) if bytes.get(before) == Some(&b'_') => before,
    _ => letter,
  }
}

impl<S: AsRef<[u8]>> LitInt<S> {
  /// The suffix the literal ends with, as in `1u8`, or `None` for a literal with none.
  ///
  /// It reads the literal's own bytes, and never panics: a literal built over any bytes answers
  /// `None` unless they end in a well-formed suffix.
  #[inline]
  pub fn suffix(&self) -> Option<IntSuffix> {
    int_suffix_of(self.source_ref().as_ref()).map(|(_, suffix): (usize, IntSuffix)| suffix)
  }

  /// How many bytes the suffix takes: its letter, its width, and the one `_` directly before the
  /// letter if there is one. 0 for a literal with no suffix.
  #[inline]
  pub fn suffix_len(&self) -> usize {
    let bytes = self.source_ref().as_ref();
    int_suffix_of(bytes).map_or(0, |(start, _): (usize, IntSuffix)| {
      bytes.len().saturating_sub(start)
    })
  }
}

impl<S: AsRef<[u8]>> LitFloat<S> {
  /// The suffix the literal ends with, as in `1.5f32`, or `None` for a literal with none.
  ///
  /// It reads the literal's own bytes, and never panics: a literal built over any bytes answers
  /// `None` unless they end in a well-formed suffix.
  #[inline]
  pub fn suffix(&self) -> Option<FloatSuffix> {
    float_suffix_of(self.source_ref().as_ref()).map(|(_, suffix): (usize, FloatSuffix)| suffix)
  }

  /// How many bytes the suffix takes: `f32` or `f64`, and the one `_` directly before it if there
  /// is one. 0 for a literal with no suffix.
  #[inline]
  pub fn suffix_len(&self) -> usize {
    let bytes = self.source_ref().as_ref();
    float_suffix_of(bytes).map_or(0, |(start, _): (usize, FloatSuffix)| {
      bytes.len().saturating_sub(start)
    })
  }
}

/// A GraphQLx `char` literal: `'`, one character or one escape, and `'`.
///
/// The escapes are a string's, and `\'`; `"` may stand as it is. The literal spells exactly one
/// Unicode scalar value, which [`value`](Self::value) answers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LitChar<S> {
  source: S,
  value: char,
}

impl<S> LitChar<S> {
  /// A literal over `source`, whose character the lexer read as `value`.
  #[inline(always)]
  pub(crate) const fn new(source: S, value: char) -> Self {
    Self { source, value }
  }

  /// The literal's slice, both quotes included.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    &self.source
  }

  /// The character the literal spells, its escape applied.
  #[inline(always)]
  pub const fn value(&self) -> char {
    self.value
  }
}

/// What a string's prefix makes it, inside a `const { … }` block: `b"…"` bytes, `i"…"` an id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, IsVariant)]
#[non_exhaustive]
pub enum StrPrefix {
  /// `b`: the bytes its body spells, where `\x` and two hexadecimal digits is any one byte.
  Bytes,
  /// `i`: the id its body spells.
  Id,
}

/// A prefixed string, which only a lexer inside a `const { … }` block makes: its prefix, and the
/// string after it, which [`literal`](Self::literal) reads.
///
/// The token's span begins at the prefix's letter. What the token keeps begins at the string's
/// quote: the literal's own slice.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LitPrefixedStr<S> {
  prefix: StrPrefix,
  block: bool,
  source: S,
}

impl<S> LitPrefixedStr<S> {
  /// A prefixed string whose literal, a block string when `block`, is `source`.
  #[inline(always)]
  pub(crate) const fn new(prefix: StrPrefix, block: bool, source: S) -> Self {
    Self {
      prefix,
      block,
      source,
    }
  }

  /// The string's prefix.
  #[inline(always)]
  pub const fn prefix(&self) -> StrPrefix {
    self.prefix
  }

  /// Whether the string is a block string.
  #[inline(always)]
  pub(crate) const fn is_block(&self) -> bool {
    self.block
  }

  /// The string's slice, from its opening quote.
  #[inline(always)]
  pub(crate) const fn source_ref(&self) -> &S {
    &self.source
  }
}

impl<S: Clone + AsRef<[u8]>> LitPrefixedStr<S> {
  /// The string after the prefix, read again from its slice by the readers the lexers read it
  /// with: a block string, or an inline string, `Complex` when its body holds an escape.
  ///
  /// Under `b` an inline string's body may hold `\x` and two hexadecimal digits, one byte, which
  /// the literal's `required_capacity` counts as one. No text door reads that escape:
  /// [`decode_string_value`](crate::decode_string_value) refuses the backslash, and the `Cow`
  /// conversion copies it through. A block string's body has no such escape: `b"""\x41"""`
  /// is the four characters it shows.
  pub fn literal(&self) -> LitStr<S> {
    let bytes = self.source.as_ref();
    if self.block {
      let block = match skip_block_str_from_bytes::<true>(bytes.get(3..).unwrap_or_default()) {
        Ok(LitBlockStr::Complex(c)) => LitBlockStr::Complex(LitComplexBlockStr::new(
          self.source.clone(),
          c.num_escaped_triple_quotes(),
          c.has_cr_terminators(),
          c.leading_blank_lines(),
          c.trailing_blank_lines(),
          c.common_indent(),
          c.total_lines(),
          c.required_capacity(),
        )),
        // A literal the lexer accepted is not refused here; a refusal answers the plain literal.
        Ok(LitBlockStr::Plain(_)) | Err(_) => {
          LitBlockStr::Plain(LitPlainBlockStr::new(self.source.clone()))
        }
      };
      return LitStr::Block(block);
    }

    let body = bytes.get(1..).unwrap_or_default();
    let capacity = match self.prefix {
      StrPrefix::Bytes => <u8 as ByteStrBody>::read_byte_str(body, 0)
        .read
        .ok()
        .flatten(),
      StrPrefix::Id => match skip_inline_str_simd::<true>(1, body) {
        Ok(LitInlineStr::Complex(c)) => Some(c.required_capacity()),
        Ok(LitInlineStr::Plain(_)) | Err(_) => None,
      },
    };
    LitStr::Inline(match capacity {
      Some(capacity) => {
        LitInlineStr::Complex(LitComplexInlineStr::new(self.source.clone(), capacity))
      }
      None => LitInlineStr::Plain(LitPlainInlineStr::new(self.source.clone())),
    })
  }
}

/// One whole literal, re-scanned by the lexer that produced it.
///
/// The private half of `LitInt`'s, `LitFloat`'s and `LitChar`'s `TryFrom<&str>`, which carry the
/// contract.
///
/// # Why this exists: a payload with no door is a payload with a second custodian
///
/// A GraphQLx string literal is re-cooked through [`LitStr`](crate::LitStr)'s `TryFrom<&str>`,
/// which **is** the string sub-lexer, so a consumer that has to rebuild one — the CST → AST
/// projection is the consumer — gets the lexer's own answer rather than a second implementation of
/// the escape rules. Numbers had no such door. The tree keeps one `Int` image over four radices and
/// one `Float` image over two, the `logos` number grammars are private, and the projection was left
/// classifying by prefix: total over what the lexer accepts, and silent about everything else.
/// `0b2`, `0x`, `007`, and a `Float`-labelled `0x1` all produced a literal.
///
/// So the door is the scanner. It is not a *second* grammar reached from here — it is
/// [`SyntacticLexer`](syntactic::SyntacticLexer), the shipped one, run over the slice, which is why
/// there is nothing left for the two layers to disagree about. al8n/smear#58.
///
/// # What "whole" means
///
/// The slice must scan to **exactly one** token, that token must be a numeric literal, and its span
/// must be the whole slice. Leading or trailing trivia fails it, a second token fails it, and a
/// token of any other kind fails it — a caller asking for a literal is asking about the bytes it
/// handed over, not about a prefix of them. The lexer's own error passes through unchanged; the
/// failures the lexer has no opinion about are one
/// [`NotOneToken`](error::LexerErrorData::NotOneToken) over the slice, naming `what` — the kind
/// the calling door asked for.
///
/// # `&str` only
///
/// `LitStr` carries a `&[u8]` door beside its `&str` one because the string sub-lexer is
/// instantiated for both scan alphabets. This one is not, and the reason is the consumer: the
/// projection hands back an AST borrowing the caller's `&'src str`, so the slice it re-cooks is
/// always text. A byte door here would be surface with no caller.
fn scan_one_token<'a>(
  value: &'a str,
  what: &'static str,
) -> Result<syntactic::SyntacticToken<&'a str>, error::LexerErrors<char, RecursionLimitExceeded>> {
  scan_one_token_from(value, what, syntactic::SyntacticState::default())
}

/// [`scan_one_token`] from `state`: a door for a token only a lexer inside a `const { … }` block
/// makes reads its slice from a state that has entered one.
fn scan_one_token_from<'a>(
  value: &'a str,
  what: &'static str,
  state: syntactic::SyntacticState,
) -> Result<syntactic::SyntacticToken<&'a str>, error::LexerErrors<char, RecursionLimitExceeded>> {
  use tokora::Lexer as _;

  let mut lexer = syntactic::SyntacticLexer::<'a, str>::with_state(value, state);
  let Some(first) = lexer.lex() else {
    return Err(
      error::LexerError::const_new(
        SimpleSpan::new(0, value.len()),
        error::LexerErrorData::UnexpectedEndOfInput,
      )
      .into(),
    );
  };
  let token = first?;
  let span = lexer.span();
  if span.start() != 0 || span.end() != value.len() || lexer.lex().is_some() {
    return Err(not_one_token(value.len(), what));
  }
  Ok(token)
}

/// The slice is not exactly one `what` — the token kind the calling door asked for — and the
/// scanner had no complaint of its own to make.
///
/// [`NotOneToken`](error::LexerErrorData::NotOneToken), not the scanner's `UnexpectedLexeme`: no
/// character was refused, the slice as a whole was.
fn not_one_token(
  len: usize,
  what: &'static str,
) -> error::LexerErrors<char, RecursionLimitExceeded> {
  error::LexerError::const_new(
    SimpleSpan::new(0, len),
    error::LexerErrorData::NotOneToken(what),
  )
  .into()
}

/// The scanner's own reading of `value` as one whole identifier.
///
/// # Whole, and the scanner's
///
/// The slice must scan to **exactly one** token, that token must be an
/// [`Identifier`](syntactic::SyntacticToken::Identifier), and its span must be the whole slice:
/// leading or trailing trivia fails it, a second token fails it, and a token of any other kind
/// fails it. The scan is [`SyntacticLexer`](syntactic::SyntacticLexer) — the shipped one — so the
/// answer is the lexer's own rather than a second spelling of the name grammar.
///
/// It exists for the same consumer [`LitInt`]'s door does: the CST → AST projection reads a name
/// out of the source by the range of a token the *tree* labelled `Name`, and a tree it did not
/// build itself can label anything. Without this, `Name("1")` and `Name("a b")` were values the
/// projection would answer and the parser could never produce. al8n/smear#58.
///
/// A keyword passes: this dialect's keywords are contextual, so `query` and `on` are identifiers
/// to the scanner and it is [`ContextualKeyword`] that tells them apart afterwards.
///
/// The lexer's own error passes through unchanged; the failures the lexer has no opinion about are
/// one [`NotOneToken`](error::LexerErrorData::NotOneToken) over the slice.
#[inline]
pub fn identifier(value: &str) -> Result<&str, error::LexerErrors<char, RecursionLimitExceeded>> {
  const WHAT: &str = "name";
  match scan_one_token(value, WHAT)? {
    syntactic::SyntacticToken::Identifier(name) => Ok(name),
    _ => Err(not_one_token(value.len(), WHAT)),
  }
}

/// The scanner's own reading of `value` as one whole integer literal inside a `const { … }` block,
/// from a state that has entered one.
///
/// [`LitInt`]'s door for the block, and see [`LitInt::try_from`] for what "whole" means. The scanner
/// reads no sign into a number there, so `-1` is a `-` and then an integer, and fails here as every
/// other slice that is not one integer inside a block does.
#[inline]
pub fn int_in_block(
  value: &str,
) -> Result<LitInt<&str>, error::LexerErrors<char, RecursionLimitExceeded>> {
  const WHAT: &str = "integer";
  let mut state = syntactic::SyntacticState::default();
  state.enter_const_block();
  match scan_one_token_from(value, WHAT, state)? {
    syntactic::SyntacticToken::LitInt(lit) => Ok(lit),
    _ => Err(not_one_token(value.len(), WHAT)),
  }
}

/// [`int_in_block`]'s twin for a float literal: the scanner's own reading of `value` as one whole
/// float literal inside a `const { … }` block, where it reads no sign into a number.
#[inline]
pub fn float_in_block(
  value: &str,
) -> Result<LitFloat<&str>, error::LexerErrors<char, RecursionLimitExceeded>> {
  const WHAT: &str = "float";
  let mut state = syntactic::SyntacticState::default();
  state.enter_const_block();
  match scan_one_token_from(value, WHAT, state)? {
    syntactic::SyntacticToken::LitFloat(lit) => Ok(lit),
    _ => Err(not_one_token(value.len(), WHAT)),
  }
}

impl<'de: 'a, 'a> TryFrom<&'de str> for LitInt<&'a str> {
  type Error = error::LexerErrors<char, RecursionLimitExceeded>;

  /// The scanner's own reading of `value` as one whole integer literal.
  ///
  /// # Whole, and the scanner's
  ///
  /// The slice must scan to **exactly one** token, that token must be an integer literal, and its
  /// span must be the whole slice: leading or trailing trivia fails it, a second token fails it,
  /// and a token of any other kind fails it. A caller asking for a literal is asking about the
  /// bytes it handed over, not about a prefix of them.
  ///
  /// The scan is [`SyntacticLexer`](syntactic::SyntacticLexer) — the shipped one, not a second
  /// grammar — which is why there is nothing left for two layers to disagree about. It exists
  /// because a consumer that rebuilds a literal from a tree had nothing else to ask: the tree keeps
  /// one `Int` image over all four radices, the number grammars are private, and the CST → AST
  /// projection was classifying by prefix. `0b2`, `0x` and a `Float`-labelled `0x1` all produced a
  /// literal. al8n/smear#58.
  ///
  /// A slice the scanner reads as a **float** fails here rather than being coerced: the two
  /// carriers are different types because the grammar's two productions are different, and
  /// answering an `Int` for `0x1p3` would be this door inventing the classification it exists to
  /// stop inventing.
  ///
  /// The lexer's own error passes through unchanged; the failures the lexer has no opinion about
  /// are one [`NotOneToken`](error::LexerErrorData::NotOneToken) over the slice.
  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    const WHAT: &str = "integer";
    match scan_one_token(value, WHAT)? {
      syntactic::SyntacticToken::LitInt(lit) => Ok(lit),
      _ => Err(not_one_token(value.len(), WHAT)),
    }
  }
}

impl<'de: 'a, 'a> TryFrom<&'de str> for LitFloat<&'a str> {
  type Error = error::LexerErrors<char, RecursionLimitExceeded>;

  /// The scanner's own reading of `value` as one whole float literal.
  ///
  /// [`LitInt`]'s twin — see [`LitInt::try_from`] for what "whole" means, for why the door exists,
  /// and for why an integer slice fails here rather than being widened.
  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    const WHAT: &str = "float";
    match scan_one_token(value, WHAT)? {
      syntactic::SyntacticToken::LitFloat(lit) => Ok(lit),
      _ => Err(not_one_token(value.len(), WHAT)),
    }
  }
}

impl<'de: 'a, 'a> TryFrom<&'de str> for LitPrefixedStr<&'a str> {
  type Error = error::LexerErrors<char, RecursionLimitExceeded>;

  /// The scanner's own reading of `value` as one whole prefixed string, from a state that has
  /// entered a `const { … }` block, the only place a prefixed string is lexed: outside one the
  /// slice is a name and then a string.
  ///
  /// [`LitInt`]'s twin — see [`LitInt::try_from`] for what "whole" means. The lexer's own refusal
  /// of a reserved prefix or a malformed string passes through unchanged.
  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    const WHAT: &str = "prefixed string";
    let mut state = syntactic::SyntacticState::default();
    state.enter_const_block();
    match scan_one_token_from(value, WHAT, state)? {
      syntactic::SyntacticToken::LitPrefixedStr(lit) => Ok(lit),
      _ => Err(not_one_token(value.len(), WHAT)),
    }
  }
}

impl<'de: 'a, 'a> TryFrom<&'de str> for LitChar<&'a str> {
  type Error = error::LexerErrors<char, RecursionLimitExceeded>;

  /// The scanner's own reading of `value` as one whole `char` literal.
  ///
  /// [`LitInt`]'s twin — see [`LitInt::try_from`] for what "whole" means. The lexer's own refusal
  /// of a malformed literal passes through unchanged.
  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    const WHAT: &str = "character";
    match scan_one_token(value, WHAT)? {
      syntactic::SyntacticToken::LitChar(lit) => Ok(lit),
      _ => Err(not_one_token(value.len(), WHAT)),
    }
  }
}
