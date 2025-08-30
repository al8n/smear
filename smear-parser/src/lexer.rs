use core::{
  marker::PhantomData,
  ops::{Bound, RangeBounds},
};

use chumsky::input::{ExactSizeInput, Input, ValueInput};

pub use error::*;
pub use span::*;
pub use state::*;

mod error;
mod span;
mod state;

/// a
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Positioned<I> {
  data: I,
  global_offset: usize,
}

impl<I> Positioned<I> {
  /// Creates a new positioned input.
  #[inline(always)]
  const fn new(data: I, global_offset: usize) -> Self {
    Self {
      data,
      global_offset,
    }
  }

  /// Returns the global position to the original input.
  #[inline(always)]
  pub const fn global_position(&self) -> usize {
    self.global_offset
  }

  /// Returns the data since the [`global_position`](Self::global_position).
  #[inline(always)]
  pub const fn data(&self) -> &I {
    &self.data
  }
}

/// The kind of the [`Token`]
pub trait TokenKind: Copy + Eq + core::fmt::Debug + core::fmt::Display {
  /// Returns the token kind represents for comment
  fn comment() -> Self;

  /// Returns the token kind represents for line terminator
  fn line_terminator() -> Self;

  /// Returns the token kind represents for the whitespace
  fn whitespace() -> Self;

  /// Returns the token kind represents for the ignore tokens in GraphQL spec
  fn ignored() -> Self;
}

/// The token error
pub trait TokenError<S>: core::error::Error + Clone {
  /// Returns the corresponding span for this error.
  fn span(&self) -> Span<S>;
}

/// The token trait.
pub trait Token<'a, I, S>: Sized + core::fmt::Debug + 'a {
  /// The error type for this token.
  type Error: TokenError<S>;
  /// The token kind associated with this token.
  type Kind: TokenKind;

  /// Peeks a token from the input, at the given position.
  fn peek(input: Positioned<I>, state: &mut S) -> Result<Option<(usize, Self)>, Self::Error>
  where
    I: Text<'a>,
    S: State;

  /// Returns the kind of this token.
  fn kind(&self) -> Self::Kind;

  /// Returns the span of this token.
  fn span(&self) -> Span<S>;

  /// Returns the source data for this token.
  fn data(&self) -> I;
}

/// A helper struct for unifying display of text.
pub struct DisplayText<'a, T>(&'a T);

impl<'a, T> From<&'a T> for DisplayText<'a, T> {
  #[inline(always)]
  fn from(t: &'a T) -> Self {
    Self(t)
  }
}

impl<'a, T: Text<'a>> core::fmt::Display for DisplayText<'_, T> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.0.format(f)
  }
}

/// The source text, e.g. `&[u8]`, `&str`
pub trait Text<'a>: Clone + 'a {
  /// Returns a subslice of the source with the given range.
  fn slice(&self, range: impl RangeBounds<usize>) -> Self;

  /// Returns the length of the source.
  fn len(&self) -> usize;

  /// Returns `true` if the source is empty.
  #[inline(always)]
  fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Format the text to the given formatter
  fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result;
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

impl<'a> Text<'a> for &'a str {
  #[inline(always)]
  fn len(&self) -> usize {
    <str>::len(self)
  }

  #[inline(always)]
  fn slice(&self, range: impl RangeBounds<usize>) -> Self {
    slice!(self(range))
  }

  #[inline]
  fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    <str as core::fmt::Display>::fmt(self, fmt)
  }
}

impl<'a> Text<'a> for &'a [u8] {
  #[inline(always)]
  fn slice(&self, range: impl RangeBounds<usize>) -> Self {
    slice!(self(range))
  }

  #[inline(always)]
  fn len(&self) -> usize {
    <[u8]>::len(self)
  }

  #[inline(always)]
  fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    <[u8] as core::fmt::Debug>::fmt(self, fmt)
  }
}

#[cfg(feature = "bstr")]
const _: () = {
  use bstr::BStr;

  impl<'a> Text<'a> for &'a BStr {
    #[inline]
    fn slice(&self, range: impl RangeBounds<usize>) -> Self {
      slice!(self(range))
    }

    #[inline]
    fn len(&self) -> usize {
      <[u8]>::len(self)
    }

    #[inline]
    fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      <BStr as core::fmt::Display>::fmt(self, fmt)
    }
  }
};

/// The generic lexer
#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a, I: 'a, T: 'a, S: 'a = ()> {
  input: I,
  state: S,
  _m: PhantomData<&'a T>,
}

impl<'a, I, T, S> Lexer<'a, I, T, S>
where
  S: Default,
{
  /// Creates a new lexer from the given input.
  #[inline(always)]
  pub fn new(input: I) -> Self {
    Self::with_state(input, S::default())
  }
}

impl<'a, I, T, S> Lexer<'a, I, T, S> {
  /// Creates a new lexer from the given input and state.
  #[inline]
  pub const fn with_state(input: I, state: S) -> Self {
    Self {
      input,
      state,
      _m: PhantomData,
    }
  }

  /// Returns a reference to the input.
  #[inline(always)]
  pub const fn input(&self) -> &I {
    &self.input
  }
}

impl<'a, I, T, S> Input<'a> for Lexer<'a, I, T, S>
where
  T: Token<'a, I, S>,
  I: Text<'a> + Clone,
  S: State,
{
  type Span = Span<S>;

  type Token = Result<T, LexerError<'a, I, T, S>>;

  type MaybeToken = Result<T, LexerError<'a, I, T, S>>;

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
    let positioned = Positioned::new(this.input.slice(*cursor..), *cursor);

    match T::peek(positioned, &mut this.state) {
      Ok(Some((consumed, tok))) => match this.state.check() {
        Ok(_) => {
          *cursor += consumed;
          Some(Ok(tok))
        }
        Err(e) => Some(Err(LexerError::new(LexerErrorData::State(e), tok.span()))),
      },
      Ok(None) => None,
      Err(e) => {
        let span = e.span();
        Some(Err(LexerError::new(LexerErrorData::Token(e), span)))
      }
    }
  }

  #[inline(always)]
  unsafe fn span(this: &mut Self::Cache, range: core::ops::Range<&Self::Cursor>) -> Self::Span {
    Span::new(*range.start, *range.end, this.state)
  }
}

impl<'a, I, T, S> ValueInput<'a> for Lexer<'a, I, T, S>
where
  T: Token<'a, I, S>,
  I: Text<'a> + Clone,
  S: State,
{
  unsafe fn next(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<Self::Token> {
    <Self as Input<'a>>::next_maybe(cache, cursor)
  }
}

impl<'a, I, T, S> ExactSizeInput<'a> for Lexer<'a, I, T, S>
where
  T: Token<'a, I, S>,
  I: Text<'a> + Clone,
  S: State,
{
  unsafe fn span_from(
    cache: &mut Self::Cache,
    range: core::ops::RangeFrom<&Self::Cursor>,
  ) -> Self::Span {
    Span::new(*range.start, cache.input.len(), cache.state)
  }
}

/// The state input
pub trait TokenStream<'a> {
  /// The state associated with the input.
  type State: State;
  /// The source text type associated with the input.
  type Text: Text<'a>;
  /// The item type associated with the stream.
  type Item: Token<'a, Self::Text, Self::State>;
}

impl<'a, I, T, S> TokenStream<'a> for Lexer<'a, I, T, S>
where
  T: Token<'a, I, S>,
  I: Text<'a>,
  S: State,
{
  type State = S;
  type Text = I;
  type Item = T;
}
