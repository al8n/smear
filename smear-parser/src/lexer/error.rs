use std::{borrow::Cow, string::String};

use chumsky::{
  error::{EmptyErr, Rich, Simple},
  label::LabelError,
};

use super::*;

/// A wrapper over a [`Token::Error`] to avoid orphan rules.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenError<E>(E);

impl<E> From<E> for TokenError<E> {
  #[inline(always)]
  fn from(e: E) -> Self {
    Self(e)
  }
}

impl<E: core::fmt::Display> core::fmt::Display for TokenError<E> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.0.fmt(f)
  }
}

impl<E: core::error::Error> core::error::Error for TokenError<E> {
  #[inline]
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    self.0.source()
  }
}

/// A wrapper over a [`State::Error`] to avoid orphan rules.
#[derive(Clone, Copy)]
pub struct StateError<S: State> {
  state: S,
  error: S::Error,
}

impl<S> core::fmt::Debug for StateError<S>
where
  S: State,
  S::Error: core::fmt::Debug,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.error.fmt(f)
  }
}

impl<S> core::fmt::Display for StateError<S>
where
  S: State,
  S::Error: core::fmt::Display,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.error.fmt(f)
  }
}

impl<S> core::error::Error for StateError<S>
where
  S: State,
  S::Error: core::error::Error,
{
  #[inline]
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    self.error.source()
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
    /// The expected token kind
    expected: T::Kind,
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
        write!(
          f,
          "unexpected token: expected {}, found {}",
          expected,
          found.kind()
        )
      }
      Self::EndOfInput => write!(f, "unexpected end of input"),
      Self::Other(cow) => write!(f, "{}", cow),
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

  /// Returns the token if it matches the expected kind, or an error if it doesn't.
  #[inline(always)]
  pub fn check_token_kind(tok: T, expected: T::Kind) -> Result<T, Self> {
    if tok.kind() == expected {
      Ok(tok)
    } else {
      Err(Self {
        span: tok.span(),
        data: LexerErrorData::UnexpectedToken {
          expected,
          found: tok,
        },
      })
    }
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

impl<'a, I, T, S> From<LexerError<'a, I, T, S>>
  for Simple<'a, Result<T, LexerError<'a, I, T, S>>, Span<S>>
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
// impl<'a: 'l, 'l, I, T, S> chumsky::error::LabelError<'a, Lexer<'l, I, T, S>, LexerError<T::Error, S::Error>> for Simple<'a, T, SimpleSpan<usize, Context<I, S>>>

// {
//   fn expected_found<E: IntoIterator<Item = LexerError<T::Error, S::Error>>>(
//           expected: E,
//           found: Option<MaybeRef<'a, <Lexer<'l, I, T, S> as Input>::Token>>,
//           chumsky::span: <Lexer<'a, I, T, S> as Input>::Span,
//       ) -> Self {

//   }
// }

// impl<'a, I: Input<'a>, L> LabelError<'a, I, L> for Simple<'a, I::Item, I::Span> {
//     #[inline]
//     fn expected_found<E: IntoIterator<Item = L>>(
//         _expected: E,
//         found: Option<MaybeRef<'a, I::Item>>,
//         span: I::Span,
//     ) -> Self {
//         Self { span, found }
//     }
// }
