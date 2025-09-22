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
use smear_parser::error::{InterfaceTypeExtensionHint, ObjectTypeExtensionHint};

use crate::error::LexerErrors;

pub use smear_parser::error::{ParseVariableValueError, VariableValueHint};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnexpectedKeyword<S> {
  found: S,
  expected: &'static str,
}

impl<S> UnexpectedKeyword<S> {
  /// Creates a new unexpected keyword error.
  #[inline]
  pub const fn new(found: S, expected_kw: &'static str) -> Self {
    Self {
      found,
      expected: expected_kw,
    }
  }

  /// Returns the found keyword.
  #[inline]
  pub const fn found(&self) -> &S {
    &self.found
  }

  /// Returns the name of the expected keyword.
  #[inline]
  pub const fn expected(&self) -> &'static str {
    self.expected
  }
}

#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ErrorData<S, T, TK, Char = char, StateError = ()> {
  Lexer(LexerErrors<Char, StateError>),
  InvalidEnumValue(S),
  InvalidBooleanValue(S),
  InvalidNullValue(S),
  InvalidFragmentName(S),
  Unclosed(Unclosed),
  UnexpectedToken(UnexpectedToken<T, TK>),
  UnexpectedKeyword(UnexpectedKeyword<S>),
  UnexpectedObjectFieldValueShape(UnexpectedObjectFieldValueShape),
  UnexpectedEndOfVariableValue(UnexpectedEnd<VariableValueHint>),
  UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>),
  UnknownDirectiveLocation(S),
  UnknownOperationType(S),
  UnexpectedEndOfObjectExtension(UnexpectedEnd<ObjectTypeExtensionHint>),
  UnexpectedEndOfInterfaceExtension(UnexpectedEnd<InterfaceTypeExtensionHint>),
  /// An end of input was found.
  EndOfInput,
  Other(Cow<'static, str>),
}

#[derive(Debug, Clone)]
pub struct Error<S, T, TK, Char = char, StateError = ()> {
  span: Span,
  data: ErrorData<S, T, TK, Char, StateError>,
}

impl<S, T, TK, Char, StateError> Error<S, T, TK, Char, StateError> {
  /// Creates a new error.
  #[inline]
  pub const fn new(span: Span, data: ErrorData<S, T, TK, Char, StateError>) -> Self {
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

  /// Creates an unexpected keyword error.
  #[inline]
  pub const fn unexpected_keyword(found: S, expected_kw: &'static str, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedKeyword(UnexpectedKeyword::new(found, expected_kw)),
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

  /// Creates an unexpected end in object type extension error.
  #[inline]
  pub const fn unexpected_end_of_object_extension(
    span: Span,
    hint: ObjectTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfObjectExtension(UnexpectedEnd::with_name(
        Cow::Borrowed("object type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in interface type extension error.
  #[inline]
  pub const fn unexpected_end_of_interface_extension(
    span: Span,
    hint: InterfaceTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfInterfaceExtension(UnexpectedEnd::with_name(
        Cow::Borrowed("interface type extension"),
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

  /// Creates an invalid fragment name error.
  #[inline]
  pub const fn invalid_fragment_name(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidFragmentName(value))
  }

  /// Creates an invalid enum value error.
  #[inline]
  pub const fn invalid_enum_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidEnumValue(value))
  }

  /// Creates an invalid boolean value error.
  #[inline]
  pub const fn invalid_boolean_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidBooleanValue(value))
  }

  /// Creates an invalid null value error.
  #[inline]
  pub const fn invalid_null_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidNullValue(value))
  }

  /// Creates an unknown directive location error.
  #[inline]
  pub const fn unknown_directive_location(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::UnknownDirectiveLocation(value))
  }

  /// Creates an unknown operation type error.
  #[inline]
  pub const fn unknown_operation_type(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::UnknownOperationType(value))
  }

  /// Returns the span of the error.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the data of the error.
  #[inline]
  pub const fn data(&self) -> &ErrorData<S, T, TK, Char, StateError> {
    &self.data
  }

  /// Returns a mutable reference to the data of the error.
  #[inline]
  pub const fn data_mut(&mut self) -> &mut ErrorData<S, T, TK, Char, StateError> {
    &mut self.data
  }

  /// Consumes the error and returns its data.
  #[inline]
  pub fn into_data(self) -> ErrorData<S, T, TK, Char, StateError> {
    self.data
  }
}

#[cfg(feature = "smallvec")]
type DefaultErrorsContainer<S, T, TK, Char = char, StateError = ()> =
  smallvec::SmallVec<[Error<S, T, TK, Char, StateError>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultErrorsContainer<S, T, TK, Char = char, StateError = ()> =
  std::vec::Vec<Error<S, T, TK, Char, StateError>>;

/// A container for storing multiple lexer errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<S, T, TK, Char = char, StateError = ()>(
  DefaultErrorsContainer<S, T, TK, Char, StateError>,
);

impl<S, T, TK, Char, StateError> Default for Errors<S, T, TK, Char, StateError> {
  #[inline(always)]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<S, T, TK, Char, StateError> From<Error<S, T, TK, Char, StateError>>
  for Errors<S, T, TK, Char, StateError>
{
  #[inline(always)]
  fn from(error: Error<S, T, TK, Char, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<S, T, TK, Char, StateError> Errors<S, T, TK, Char, StateError> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<S, T, TK, Char, StateError> IntoIterator for Errors<S, T, TK, Char, StateError> {
  type Item = Error<S, T, TK, Char, StateError>;
  type IntoIter = <DefaultErrorsContainer<S, T, TK, Char, StateError> as IntoIterator>::IntoIter;

  #[inline(always)]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<S, T, TK, Char, StateError> Extend<Error<S, T, TK, Char, StateError>>
  for Errors<S, T, TK, Char, StateError>
{
  #[inline(always)]
  fn extend<I: IntoIterator<Item = Error<S, T, TK, Char, StateError>>>(&mut self, iter: I) {
    self.0.extend(iter);
  }
}

impl<'a, S, T, Char, StateError>
  LabelError<'a, TokenStream<'a, T>, DefaultExpected<'a, Lexed<'a, T>>>
  for Errors<S, T, T::Kind, Char, StateError>
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
            Lexed::Token(expected_tok) => {
              let expected_tok = expected_tok.as_ref().into_data();
              match found_lexed {
                None => ErrorData::UnexpectedToken(UnexpectedToken::new(expected_tok.kind())),
                Some(Lexed::Token(found_tok)) => {
                  let found_tok = found_tok.as_ref().into_data();
                  ErrorData::UnexpectedToken(UnexpectedToken::with_found(
                    found_tok.clone(),
                    expected_tok.kind(),
                  ))
                }
                Some(Lexed::Error(err)) => ErrorData::Lexer(err.clone()),
              }
            }
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

impl<'a, S, T, Char, StateError> chumsky::error::Error<'a, TokenStream<'a, T>>
  for Errors<S, T, T::Kind, Char, StateError>
where
  T: Token<'a, Error = LexerErrors<Char, StateError>>,
  T::Extras: Copy,
  Char: Clone,
  StateError: Clone,
{
}
