use std::{borrow::Cow, string::String};

use chumsky::{
  error::{EmptyErr, Rich, RichPattern, Simple},
  label::LabelError,
  span::SimpleSpan,
  util::Maybe,
};

use super::*;

/// A wrapper over a [`State::Error`] to avoid orphan rules.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateError<E>(E);

impl<E> From<E> for StateError<E> {
  #[inline(always)]
  fn from(e: E) -> Self {
    Self(e)
  }
}

impl<E: core::fmt::Display> core::fmt::Display for StateError<E> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.0.fmt(f)
  }
}

impl<E: core::error::Error> core::error::Error for StateError<E> {
  #[inline]
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    self.0.source()
  }
}

/// A token
#[derive(Debug, Clone)]
pub enum LexerErrorData<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  /// The token error happens when trying to pull the next token.
  Token(T::Error),
  /// The state error
  State(S::Error),
  /// The unexpected token error
  UnexpectedToken {
    /// The expected token name
    expected: Cow<'static, str>,
    /// The actual token found.
    found: T,
  },
  /// End of input
  EndOfInput,
  /// The other error message
  Other(Cow<'static, str>),
}

impl<'a, I, T, S> core::fmt::Display for LexerErrorData<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Token(tok) => tok.fmt(f),
      Self::State(err) => err.fmt(f),
      Self::UnexpectedToken { expected, found } => {
        write!(f, "unexpected token: expected {expected}, found {found}")
      }
      Self::EndOfInput => write!(f, "unexpected end of input"),
      Self::Other(cow) => write!(f, "{cow}"),
    }
  }
}

impl<'a, I, T, S> LexerErrorData<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  /// Creates a token error data
  #[inline]
  pub const fn token(data: T::Error) -> Self {
    Self::Token(data)
  }

  /// Creates a state error data
  #[inline]
  pub const fn state(data: S::Error) -> Self {
    Self::State(data)
  }
}

/// The lexer error
#[derive(Debug, Clone)]
pub struct LexerError<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  span: Span<S>,
  data: LexerErrorData<'a, I, T, S>,
}

impl<'a, I, T, S> core::fmt::Display for LexerError<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.data.fmt(f)
  }
}

impl<'a, I, T, S> LexerError<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  /// Create a lexer error from the error data and span
  #[inline(always)]
  pub const fn new(data: LexerErrorData<'a, I, T, S>, span: Span<S>) -> Self {
    Self { span, data }
  }

  /// Creates an End of Input error from the span
  #[inline(always)]
  pub const fn end_of_input(span: Span<S>) -> Self {
    Self::new(LexerErrorData::EndOfInput, span)
  }

  /// Returns the error data
  #[inline(always)]
  pub const fn data(&self) -> &LexerErrorData<'a, I, T, S> {
    &self.data
  }

  /// Returns the span contained by this error
  #[inline(always)]
  pub const fn span(&self) -> &Span<S> {
    &self.span
  }

  /// Consumes the `LexerError` and returns the span and error data
  #[inline(always)]
  pub fn into_components(self) -> (Span<S>, LexerErrorData<'a, I, T, S>) {
    (self.span, self.data)
  }
}

impl<'a, I, T, S> From<&'static str> for LexerErrorData<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(e: &'static str) -> Self {
    Self::Other(Cow::Borrowed(e))
  }
}

impl<'a, I, T, S> From<String> for LexerErrorData<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(e: String) -> Self {
    Self::Other(Cow::Owned(e))
  }
}

impl<'a, I, T, S> From<Cow<'static, str>> for LexerErrorData<'a, I, T, S>
where
  I: Text<'a>,
  S: State,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(e: Cow<'static, str>) -> Self {
    Self::Other(e)
  }
}

impl<'a, I, T, S> From<LexerError<'a, I, T, S>> for EmptyErr
where
  I: Text<'a>,
  S: State + 'a,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(value: LexerError<'a, I, T, S>) -> Self {
    <EmptyErr as LabelError<'a, Lexer<'a, I, T, S>, Span<S>>>::expected_found([], None, value.span)
  }
}

impl<'a, I, T, S> From<LexerError<'a, I, T, S>> for Simple<'a, T, Span<S>>
where
  I: Text<'a>,
  S: State + 'a,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(value: LexerError<'a, I, T, S>) -> Self {
    <Simple<'a, _, Span<S>> as LabelError<'a, Lexer<'a, I, T, S>, Span<S>>>::expected_found(
      [],
      None,
      value.span,
    )
  }
}

impl<'a, I, T, S> From<LexerError<'a, I, T, S>> for RichPattern<'a, T>
where
  I: Text<'a>,
  S: State + 'a,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(value: LexerError<'a, I, T, S>) -> Self {
    match value.into_components().1 {
      LexerErrorData::UnexpectedToken { expected, found } => {
        Self::Label(format!("unexpected token: expected {expected}, found {found}").into())
      }
      LexerErrorData::Token(err) => Self::Label(err.to_string().into()),
      LexerErrorData::State(err) => Self::Label(err.to_string().into()),
      LexerErrorData::EndOfInput => Self::EndOfInput,
      LexerErrorData::Other(cow) => Self::Label(cow),
    }
  }
}

impl<'a, I, T, S> From<LexerError<'a, I, T, S>> for RichPattern<'a, LexerError<'a, I, T, S>>
where
  I: Text<'a>,
  S: State + 'a,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(value: LexerError<'a, I, T, S>) -> Self {
    match value.into_components().1 {
      LexerErrorData::UnexpectedToken { expected, found } => {
        Self::Label(format!("unexpected token: expected {expected}, found {found}").into())
      }
      LexerErrorData::Token(err) => Self::Label(err.to_string().into()),
      LexerErrorData::State(err) => Self::Label(err.to_string().into()),
      LexerErrorData::EndOfInput => Self::EndOfInput,
      LexerErrorData::Other(cow) => Self::Label(cow),
    }
  }
}

impl<'a, I, T, S> From<LexerError<'a, I, T, S>> for Rich<'a, T, Span<S>>
where
  I: Text<'a>,
  S: State + 'a,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(value: LexerError<'a, I, T, S>) -> Self {
    let (span, data) = value.into_components();
    match data {
      LexerErrorData::UnexpectedToken { expected, found } => Self::custom(
        span,
        format!("unexpected token: expected {expected}, found {found}"),
      ),
      LexerErrorData::Token(err) => Self::custom(span, err.to_string()),
      LexerErrorData::State(err) => Self::custom(span, err.to_string()),
      LexerErrorData::EndOfInput => Self::custom(span, "<end-of-input>"),
      LexerErrorData::Other(cow) => Self::custom(span, cow),
    }
  }
}

impl<'a, I, T, S> From<LexerError<'a, I, T, S>> for Rich<'a, T, SimpleSpan<usize, S>>
where
  I: Text<'a>,
  S: State + 'a,
  T: Token<'a, I, S>,
{
  #[inline(always)]
  fn from(value: LexerError<'a, I, T, S>) -> Self {
    let (span, data) = value.into_components();
    match data {
      LexerErrorData::UnexpectedToken { expected, found } => Self::custom(
        to_simple_span(span),
        format!("unexpected token: expected {expected}, found {found}"),
      ),
      LexerErrorData::Token(err) => Self::custom(to_simple_span(span), err.to_string()),
      LexerErrorData::State(err) => Self::custom(to_simple_span(span), err.to_string()),
      LexerErrorData::EndOfInput => Self::custom(to_simple_span(span), "<end-of-input>"),
      LexerErrorData::Other(cow) => Self::custom(to_simple_span(span), cow),
    }
  }
}

impl<'a, I, T, S> From<LexerError<'a, I, T, S>> for Simple<'a, T, SimpleSpan<usize, S>>
where
  I: Text<'a>,
  S: State + 'a,
  T: Token<'a, I, S>,
{
  fn from(value: LexerError<'a, I, T, S>) -> Self {
    let (span, data) = value.into_components();

    match data {
      LexerErrorData::Token(_) => Simple::new(None, to_simple_span(span)),
      LexerErrorData::State(_) => Simple::new(None, to_simple_span(span)),
      LexerErrorData::UnexpectedToken { found, .. } => {
        Simple::new(Some(Maybe::Val(found)), to_simple_span(span))
      }
      LexerErrorData::EndOfInput => Simple::new(None, to_simple_span(span)),
      LexerErrorData::Other(_) => Simple::new(None, to_simple_span(span)),
    }
  }
}

#[inline(always)]
fn to_simple_span<S>(span: Span<S>) -> SimpleSpan<usize, S> {
  SimpleSpan {
    start: span.start(),
    end: span.end(),
    context: span.state,
  }
}
