use core::marker::PhantomData;

use chumsky::input::{ExactSizeInput, Input, ValueInput};

pub use error::*;
pub use span::*;
pub use state::State;
pub use text::{DisplayText, Text, TextExt};
pub use token::Token;

mod error;
mod span;
mod text;

/// The state related structures and traits
pub mod state;

/// The token related structures and traits
pub mod token;

/// The lexer without any state information.
pub type StatelessLexer<'a, I, T> = Lexer<'a, I, T, ()>;

/// The lexer will maintain all state information.
///
/// - Recursion limit
/// - Token limit
/// - Position tracking
pub type StatefulLexer<'a, I, T> = Lexer<'a, I, T, state::Tracker>; // Fixed typo: "Statefull" -> "Stateful"

/// The lexer will limit the maximum recursion depth.
pub type RecursionLimitedLexer<'a, I, T> = Lexer<'a, I, T, state::RecursionLimiter>;

/// The lexer will limit the maximum tokens can be yielded.
pub type TokenLimitedLexer<'a, I, T> = Lexer<'a, I, T, state::TokenLimiter>;

/// The lexer will track the line and column position of tokens corresponding to original input
pub type PositionTrackingLexer<'a, I, T> = Lexer<'a, I, T, state::Position>;

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
  type Span = span::Span<S>;

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
    T::peek(this.input.slice(*cursor..), &mut this.state).map(|mut tok| {
      let span = tok.span_mut();
      let relative_start = span.start();
      let relative_end = span.end();

      let absolute_start = *cursor + relative_start;
      let absolute_end = *cursor + relative_end;

      span.start = absolute_start;
      span.end = absolute_end;

      *cursor = absolute_end;

      tok
    })
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

/// The tokenizer
pub trait Tokenizer<'a>: ValueInput<'a>
where
  <Self as Input<'a>>::Token: Token<'a, Self::Text, Self::State>,
{
  /// The state of the tokenizer
  type State: State + 'a;
  /// The source text of the tokenizer
  type Text: Text<'a>;
}

impl<'a, I, T, S> Tokenizer<'a> for Lexer<'a, I, T, S>
where
  T: Token<'a, I, S>,
  I: Text<'a>,
  S: State,
{
  type State = S;
  type Text = I;
}
