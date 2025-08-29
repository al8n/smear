use core::{marker::PhantomData, ops::{Bound, RangeBounds}};

use chumsky::{input::Input, span::SimpleSpan};

/// A token tracker which tracks the number of tokens.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct TokenTracker {
  max: Option<usize>,
  current: usize,
}

impl TokenTracker {
  /// Creates a new token tracker without limitation 
  #[inline]
  pub const fn new() -> Self {
    Self { max: None, current: 0 }
  }

  /// Creates a new token tracker with the given maximum number of tokens
  #[inline]
  pub const fn with_limitation(max: usize) -> Self {
    Self { max: Some(max), current: 0 }
  }

  /// Returns the current number of tokens tracked.
  #[inline]
  pub const fn tokens(&self) -> usize {
    self.current
  }

  /// Returns the maximum number of tokens.
  #[inline]
  pub const fn limitation(&self) -> Option<usize> {
    self.max
  }
}

/// The context of a span.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Context<I> {
  limiter: TokenTracker,
  data: I,
}

impl<I> Context<I> {
  /// Returns the limiter.
  #[inline]
  pub const fn limiter(&self) -> &TokenTracker {
    &self.limiter
  }

  /// Returns the data.
  #[inline]
  pub const fn data(&self) -> &I {
    &self.data
  }
}

/// The kind of the [`Token`]

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
  /// The punctuation token.
  Punctuator,
  /// The name token.
  Name,
  /// The integer value token.
  IntValue,
  /// The float value token.
  FloatValue,
  /// The string value token.
  StringValue,
  /// The block string token.
  BlockString,

  /// The white spaces.
  Whitespaces,
  /// The comment.
  Comment,
}

/// The token trait.
pub trait Token<'a, I>: Sized + 'a
where
  I: Source<'a>,
{
  /// Peeks a token from the input.
  fn peek(input: I) -> Option<Self>;

  /// Returns the size of this token.
  #[inline(always)]
  fn size(&self) -> usize {
    self.data().len()
  }

  /// Returns the kind of this token.
  fn kind(&self) -> TokenKind;

  /// Returns the source data for this token.
  fn data(&self) -> &I;
}

/// The source
pub trait Source<'a>: 'a {
  /// Returns a subslice of the source with the given range.
  fn slice(&self, range: impl RangeBounds<usize>) -> Self;

  /// Returns the length of the source.
  fn len(&self) -> usize;

  /// Returns `true` if the source is empty.
  #[inline(always)]
  fn is_empty(&self) -> bool {
    self.len() == 0
  }
}

macro_rules! slice {
  ($this:ident($range:ident)) => {{
    let len = $this.len();

    let begin = match $range.start_bound() {
      Bound::Included(&n) => n,
      Bound::Excluded(&n) => n.checked_add(1).expect("out of range"),
      Bound::Unbounded => 0,
    };

    let end = match $range.end_bound() {
      Bound::Included(&n) => n.checked_add(1).expect("out of range"),
      Bound::Excluded(&n) => n,
      Bound::Unbounded => len,
    };

    &$this[begin..end]
  }};
}

impl<'a> Source<'a> for &'a str {
  #[inline(always)]
  fn len(&self) -> usize {
    <str>::len(self)
  }

  #[inline(always)]
  fn slice(&self, range: impl RangeBounds<usize>) -> Self {
    slice!(self(range))
  }
}

impl<'a> Source<'a> for &'a [u8] {
  #[inline(always)]
  fn slice(&self, range: impl RangeBounds<usize>) -> Self {
    slice!(self(range))
  }

  #[inline(always)]
  fn len(&self) -> usize {
    <[u8]>::len(self) 
  }
}

/// The input stream for the lexer.
#[derive(Debug, Clone, Copy)]
pub struct TokenStream<'a, I: 'a, T: 'a> {
  input: I,
  token_limiter: TokenTracker,
  _m: PhantomData<&'a T>,
}

impl<'a, I, T> Input<'a> for TokenStream<'a, I, T>
where
  T: Token<'a, I>,
  I: Source<'a> + Clone,
{
  type Span = SimpleSpan<usize, Context<I>>;

  type Token = T;

  type MaybeToken = T;

  type Cursor = usize;

  type Cache = Self;

  #[inline(always)]
  fn begin(self) -> (Self::Cursor, Self::Cache) {
    (0, self)
  }

  #[inline(always)]
  fn cursor_location(cursor: &Self::Cursor) -> usize {
    *cursor
  }

  #[inline(always)]
  unsafe fn next_maybe(
    this: &mut Self::Cache,
    cursor: &mut Self::Cursor,
  ) -> Option<Self::MaybeToken> {
    if let Some(tok) = T::peek(this.input.slice(*cursor..)) {
      *cursor += tok.size();
      this.token_limiter.current += 1;
      Some(tok)
    } else {
      None
    }
  }

  #[inline(always)]
  unsafe fn span(cache: &mut Self::Cache, range: core::ops::Range<&Self::Cursor>) -> Self::Span {
    SimpleSpan {
      start: *range.start,
      end: *range.end,
      context: Context { limiter: cache.token_limiter, data: cache.input.slice(*range.start..*range.end) },
    }
  }
}
