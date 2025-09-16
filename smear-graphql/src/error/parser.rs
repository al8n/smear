use std::borrow::Cow;

use chumsky::{
  DefaultExpected,
  error::{self, LabelError},
  util::{Maybe, MaybeRef},
};
use derive_more::{
  AsMut, AsRef, Deref, DerefMut, Display, From, Into, IsVariant, TryUnwrap, Unwrap,
};
use logosky::{
  Lexed, Token, TokenStream,
  utils::{Span, UnexpectedEnd},
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum VariableValueHint {
  /// A [`Name`](crate::parser::ast::Name) was expected.
  #[display("name")]
  Name,
  /// A [`Dollar`](crate::parser::ast::Dollar) was expected.
  #[display("dollar")]
  Dollar,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum ObjectFieldValueHint {
  /// A [`Colon`](crate::parser::ast::Colon) was expected.
  #[display("colon")]
  Colon,
  /// A value was expected.
  #[display("value")]
  Value,
  /// A [`Name`](crate::parser::ast::Name) was expected.
  #[display("name")]
  Name,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct UnexpectedObjectFieldValueShape {
  has_name: bool,
  has_colon: bool,
  has_value: bool,
}

impl UnexpectedObjectFieldValueShape {
  /// Creates a new unexpected object field value shape error.
  #[inline]
  pub const fn new(has_name: bool, has_colon: bool, has_value: bool) -> Self {
    Self {
      has_name,
      has_colon,
      has_value,
    }
  }

  /// Returns `true` if the object field value has a name.
  #[inline]
  pub const fn has_name(&self) -> bool {
    self.has_name
  }

  /// Returns `true` if the object field value has a colon.
  #[inline]
  pub const fn has_colon(&self) -> bool {
    self.has_colon
  }

  /// Returns `true` if the object field value has a value.
  #[inline]
  pub const fn has_value(&self) -> bool {
    self.has_value
  }
}

#[derive(Debug, Copy, Clone, IsVariant)]
pub enum Unclosed {
  List,
  Object,
}

#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ErrorData<'a, T, TK, Char = char, StateError = ()> {
  Lexer(LexerErrors<Char, StateError>),
  InvalidEnumValue(&'a str),
  Unclosed(Unclosed),
  UnexpectedToken(UnexpectedToken<T, TK>),
  UnexpectedObjectFieldValueShape(UnexpectedObjectFieldValueShape),
  UnexpectedEndOfVariableValue(UnexpectedEnd<VariableValueHint>),
  UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>),
  /// An end of input was found.
  EndOfInput,
  Other(Cow<'static, str>),
}

#[derive(Debug, Clone)]
pub struct Error<'a, T, TK, Char = char, StateError = ()> {
  span: Span,
  data: ErrorData<'a, T, TK, Char, StateError>,
}

impl<'a, T, TK, Char, StateError> Error<'a, T, TK, Char, StateError> {
  /// Creates a new error.
  #[inline]
  pub const fn new(span: Span, data: ErrorData<'a, T, TK, Char, StateError>) -> Self {
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

  /// Creates an unexpected end in variable value error.
  #[inline]
  pub const fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfVariableValue(UnexpectedEnd::with_name(
        Cow::Borrowed("variable value"),
        hint,
      )),
    )
  }

  /// Creates an unexpected object field value shape error.
  #[inline]
  pub const fn unexpected_object_field_value_shape(
    has_name: bool,
    has_colon: bool,
    has_value: bool,
    span: Span,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedObjectFieldValueShape(UnexpectedObjectFieldValueShape::new(
        has_name, has_colon, has_value,
      )),
    )
  }

  /// Creates an unexpected end in object field value error.
  #[inline]
  pub const fn unexpected_end_of_object_field_value(
    hint: ObjectFieldValueHint,
    span: Span,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfObjectFieldValue(UnexpectedEnd::with_name(
        Cow::Borrowed("object field value"),
        hint,
      )),
    )
  }

  /// Creates an unclosed list error.
  #[inline]
  pub const fn unclosed_list(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::List))
  }

  /// Creates an unclosed object error.
  #[inline]
  pub const fn unclosed_object(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Object))
  }

  /// Creates an error from a lexer error.
  #[inline]
  pub const fn from_lexer_errors(err: LexerErrors<Char, StateError>, span: Span) -> Self {
    Self::new(span, ErrorData::Lexer(err))
  }

  /// Creates an invalid enum value error.
  #[inline]
  pub const fn invalid_enum_value(value: &'a str, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidEnumValue(value))
  }

  /// Returns the span of the error.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the data of the error.
  #[inline]
  pub const fn data(&self) -> &ErrorData<'a, T, TK, Char, StateError> {
    &self.data
  }

  /// Returns a mutable reference to the data of the error.
  #[inline]
  pub const fn data_mut(&mut self) -> &mut ErrorData<'a, T, TK, Char, StateError> {
    &mut self.data
  }

  /// Consumes the error and returns its data.
  #[inline]
  pub fn into_data(self) -> ErrorData<'a, T, TK, Char, StateError> {
    self.data
  }
}

#[cfg(feature = "smallvec")]
type DefaultErrorsContainer<'a, T, TK, Char = char, StateError = ()> =
  smallvec::SmallVec<[Error<'a, T, TK, Char, StateError>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultErrorsContainer<'a, T, TK, Char = char, StateError = ()> =
  std::vec::Vec<Error<'a, T, TK, Char, StateError>>;

/// A container for storing multiple lexer errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<'a, T, TK, Char = char, StateError = ()>(
  DefaultErrorsContainer<'a, T, TK, Char, StateError>,
);

impl<'a, T, TK, Char, StateError> Default for Errors<'a, T, TK, Char, StateError> {
  #[inline(always)]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<'a, T, TK, Char, StateError> From<Error<'a, T, TK, Char, StateError>>
  for Errors<'a, T, TK, Char, StateError>
{
  #[inline(always)]
  fn from(error: Error<'a, T, TK, Char, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<'a, T, TK, Char, StateError> Errors<'a, T, TK, Char, StateError> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<'a, T, TK, Char, StateError> IntoIterator for Errors<'a, T, TK, Char, StateError> {
  type Item = Error<'a, T, TK, Char, StateError>;
  type IntoIter = <DefaultErrorsContainer<'a, T, TK, Char, StateError> as IntoIterator>::IntoIter;

  #[inline(always)]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<'a, T, TK, Char, StateError> Extend<Error<'a, T, TK, Char, StateError>>
  for Errors<'a, T, TK, Char, StateError>
{
  #[inline(always)]
  fn extend<I: IntoIterator<Item = Error<'a, T, TK, Char, StateError>>>(&mut self, iter: I) {
    self.0.extend(iter);
  }
}

impl<'a, T, Char, StateError> LabelError<'a, TokenStream<'a, T>, DefaultExpected<'a, Lexed<'a, T>>>
  for Errors<'a, T, T::Kind, Char, StateError>
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
  for Errors<'a, T, T::Kind, Char, StateError>
where
  T: Token<'a, Error = LexerErrors<Char, StateError>>,
  T::Extras: Copy,
  Char: Clone,
  StateError: Clone,
{
}
