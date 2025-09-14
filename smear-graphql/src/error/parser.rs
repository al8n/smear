use std::borrow::Cow;

use chumsky::{
  DefaultExpected,
  error::{self, LabelError},
  util::{Maybe, MaybeRef},
};
use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into};
use logosky::{Lexed, Token, TokenStream, utils::Span};

use crate::error::LexerErrors;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedToken<T, TK> {
  found: Option<T>,
  expected: TK,
}

impl<T, TK> UnexpectedToken<T, TK> {
  /// Creates an unexpected token error without found token.
  #[inline]
  pub const fn new(expected: TK) -> Self {
    Self::maybe_found(None, expected)
  }

  /// Creates a new unexpected token error.
  #[inline]
  pub const fn maybe_found(found: Option<T>, expected: TK) -> Self {
    Self { found, expected }
  }

  /// Creates a new unexpected token error with found token.
  #[inline]
  pub const fn with_found(found: T, expected: TK) -> Self {
    Self::maybe_found(Some(found), expected)
  }
}

#[derive(Debug, Clone)]
pub enum ErrorData<T, TK, Char = char, StateError = ()> {
  Lexer(LexerErrors<Char, StateError>),
  UnexpectedToken(UnexpectedToken<T, TK>),
  /// An end of input was found.
  EndOfInput,
  Other(Cow<'static, str>),
}

#[derive(Debug, Clone)]
pub struct Error<T, TK, Char = char, StateError = ()> {
  span: Span,
  data: ErrorData<T, TK, Char, StateError>,
}

impl<T, TK, Char, StateError> Error<T, TK, Char, StateError> {
  /// Creates a new error.
  #[inline]
  pub const fn new(span: Span, data: ErrorData<T, TK, Char, StateError>) -> Self {
    Self { span, data }
  }

  /// Creates an unexpected token error.
  #[inline]
  pub const fn unexpected_token(found: T, expected: TK, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedToken(UnexpectedToken::with_found(found, expected)),
    )
  }

  /// Creates an error from a lexer error.
  #[inline]
  pub const fn from_lexer_errors(err: LexerErrors<Char, StateError>, span: Span) -> Self {
    Self::new(span, ErrorData::Lexer(err))
  }
}

#[cfg(feature = "smallvec")]
type DefaultErrorsContainer<T, TK, Char = char, StateError = ()> =
  smallvec::SmallVec<[Error<T, TK, Char, StateError>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultErrorsContainer<T, TK, Char = char, StateError = ()> =
  std::vec::Vec<Error<T, TK, Char, StateError>>;

/// A container for storing multiple lexer errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<T, TK, Char, StateError>(DefaultErrorsContainer<T, TK, Char, StateError>);

impl<T, TK, Char, StateError> Default for Errors<T, TK, Char, StateError> {
  #[inline(always)]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<T, TK, Char, StateError> From<Error<T, TK, Char, StateError>>
  for Errors<T, TK, Char, StateError>
{
  #[inline(always)]
  fn from(error: Error<T, TK, Char, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<T, TK, Char, StateError> Errors<T, TK, Char, StateError> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<T, TK, Char, StateError> IntoIterator for Errors<T, TK, Char, StateError> {
  type Item = Error<T, TK, Char, StateError>;
  type IntoIter = <DefaultErrorsContainer<T, TK, Char, StateError> as IntoIterator>::IntoIter;

  #[inline(always)]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<T, TK, Char, StateError> Extend<Error<T, TK, Char, StateError>>
  for Errors<T, TK, Char, StateError>
{
  #[inline(always)]
  fn extend<I: IntoIterator<Item = Error<T, TK, Char, StateError>>>(&mut self, iter: I) {
    self.0.extend(iter);
  }
}

impl<'a, T, Char, StateError> LabelError<'a, TokenStream<'a, T>, DefaultExpected<'a, Lexed<'a, T>>>
  for Errors<T, T::Kind, Char, StateError>
where
  T: Token<'a, Error = LexerErrors<Char, StateError>>,
  T::Extras: Copy,
  Char: Clone,
  StateError: Clone,
{
  fn expected_found<E: IntoIterator<Item = DefaultExpected<'a, Lexed<'a, T>>>>(
    expected: E,
    found: Option<MaybeRef<'a, Lexed<'a, T>>>,
    span: logosky::utils::Span,
  ) -> Self {
    let mut errs = Self::default();

    // Helper to extract Lexed from found option
    let found_lexed = found.as_ref().map(|f| match f {
      MaybeRef::Ref(lexed) => *lexed,
      MaybeRef::Val(lexed) => lexed,
    });

    for exp in expected {
      let ed = match exp {
        DefaultExpected::Token(maybe) => {
          // Extract Lexed from Maybe wrapper
          let expected_lexed = match maybe {
            Maybe::Ref(lexed) => lexed,
            Maybe::Val(ref lexed) => lexed,
          };

          match expected_lexed {
            Lexed::Token(expected_tok) => match found_lexed {
              None => ErrorData::UnexpectedToken(UnexpectedToken::new(expected_tok.kind())),
              Some(Lexed::Token(found_tok)) => ErrorData::UnexpectedToken(
                UnexpectedToken::with_found(found_tok.clone(), expected_tok.kind()),
              ),
              Some(Lexed::Error(err)) => ErrorData::Lexer(err.clone()),
            },
            Lexed::Error(err) => ErrorData::Lexer(err.clone()),
          }
        }
        DefaultExpected::Any => ErrorData::Other("DefaultExpected::Any".into()),
        DefaultExpected::SomethingElse => ErrorData::Other("DefaultExpected::SomethingElse".into()),
        DefaultExpected::EndOfInput => ErrorData::EndOfInput,
        _ => ErrorData::Other("unknown expected".into()),
      };

      errs.push(Error::new(span, ed));
    }

    errs
  }

  fn merge_expected_found<E: IntoIterator<Item = DefaultExpected<'a, Lexed<'a, T>>>>(
    mut self,
    expected: E,
    found: Option<MaybeRef<'a, Lexed<'a, T>>>,
    span: logosky::utils::Span,
  ) -> Self
  where
    Self: error::Error<'a, TokenStream<'a, T>>,
  {
    // Create new errors from the expected/found combination
    let new_errors = Self::expected_found(expected, found, span);

    // Merge the new errors into self
    self.extend(new_errors);
    self
  }
}

impl<'a, T, Char, StateError> chumsky::error::Error<'a, TokenStream<'a, T>>
  for Errors<T, T::Kind, Char, StateError>
where
  T: Token<'a, Error = LexerErrors<Char, StateError>>,
  T::Extras: Copy,
  Char: Clone,
  StateError: Clone,
{
}
