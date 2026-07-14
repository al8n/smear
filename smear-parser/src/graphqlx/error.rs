use std::borrow::Cow;

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokora::{SimpleSpan as Span, utils::CowStr};

use super::Expectation;

pub use crate::{
  error::{UnexpectedKeyword, UnexpectedToken},
  hints::{
    EnumTypeExtensionHint, InputObjectTypeExtensionHint, InterfaceTypeExtensionHint,
    ObjectFieldValueHint, ObjectTypeExtensionHint, SchemaExtensionHint, UnionTypeExtensionHint,
    VariableValueHint,
  },
  lexer::graphqlx::error::LexerErrors,
};

/// An unclosed structure error.
#[derive(Debug, Copy, Clone, IsVariant)]
pub enum Unclosed {
  /// An unclosed bracket.
  Bracket,
  /// An unclosed brace.
  Brace,
}

/// Invalid enum value name
#[derive(Debug, Copy, Clone, IsVariant)]
pub enum InvalidEnumValue {
  /// Enum value is `true`
  True,
  /// Enum value is `false`
  False,
  /// Enum value is `null`
  Null,
}

/// The data of a parser error.
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ErrorData<S, T, Char = char, Exp = Expectation, StateError = ()> {
  /// One or more errors from the lexer.
  Lexer(LexerErrors<Char, StateError>),
  /// An integer value could not be parsed due to overflow.
  #[from(skip)]
  IntOverflow(S),
  /// A floating point value could not be parsed due to overflow
  #[from(skip)]
  FloatOverflow(S),
  /// An enum value is invalid.
  #[from(skip)]
  InvalidEnumValue(InvalidEnumValue),
  /// A boolean value is invalid.
  #[from(skip)]
  InvalidBooleanValue(S),
  /// A null value is invalid.
  #[from(skip)]
  InvalidNullValue(S),
  /// A fragment type path is invalid.
  #[from(skip)]
  InvalidFragmentTypePath,
  /// A list or object was not closed.
  Unclosed(Unclosed),
  /// An unexpected token was found.
  UnexpectedToken(UnexpectedToken<T, Exp>),
  /// An unexpected keyword was found.
  UnexpectedKeyword(UnexpectedKeyword<S>),
  /// An unexpected end was found in a variable value.
  UnexpectedEndOfVariableValue(smear_lexer::tokora::error::UnexpectedEnd<VariableValueHint>),
  /// An unexpected end was found in an object field value.
  UnexpectedEndOfObjectFieldValue(smear_lexer::tokora::error::UnexpectedEnd<ObjectFieldValueHint>),
  /// An unknown directive location was found.
  #[from(skip)]
  UnknownDirectiveLocation(S),
  /// An unknown operation type was found.
  #[from(skip)]
  UnknownOperationType(S),
  /// An unexpected end was found in an object type extension.
  UnexpectedEndOfObjectExtension(smear_lexer::tokora::error::UnexpectedEnd<ObjectTypeExtensionHint>),
  /// An unexpected end was found in an interface type extension.
  UnexpectedEndOfInterfaceExtension(
    smear_lexer::tokora::error::UnexpectedEnd<InterfaceTypeExtensionHint>,
  ),
  /// An unexpected end was found in an enum type extension.
  UnexpectedEndOfEnumExtension(smear_lexer::tokora::error::UnexpectedEnd<EnumTypeExtensionHint>),
  /// An unexpected end was found in an input object type extension.
  UnexpectedEndOfInputObjectExtension(
    smear_lexer::tokora::error::UnexpectedEnd<InputObjectTypeExtensionHint>,
  ),
  /// An unexpected end was found in a union type extension.
  UnexpectedEndOfUnionExtension(smear_lexer::tokora::error::UnexpectedEnd<UnionTypeExtensionHint>),
  /// An unexpected end was found in a schema extension.
  UnexpectedEndOfSchemaExtension(smear_lexer::tokora::error::UnexpectedEnd<SchemaExtensionHint>),
  /// An end of input was found.
  EndOfInput,
  /// Some other error.
  Other(Cow<'static, str>),
}

/// A parser error.
#[derive(Debug, Clone)]
pub struct Error<S, T, Char = char, Exp = Expectation, StateError = ()> {
  span: Span,
  data: ErrorData<S, T, Char, Exp, StateError>,
}

impl<S, T, Char, Expectation, StateError> Error<S, T, Char, Expectation, StateError> {
  /// Creates a new error.
  #[inline]
  pub const fn new(span: Span, data: ErrorData<S, T, Char, Expectation, StateError>) -> Self {
    Self { span, data }
  }

  /// Creates an unexpected token error.
  #[inline]
  pub const fn unexpected_token(found: T, expected: Expectation, span: Span) -> Self {
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
      ErrorData::UnexpectedEndOfVariableValue(smear_lexer::tokora::error::UnexpectedEnd::with_name(
        0,
        CowStr::from_static("variable value"),
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

  /// Creates an unexpected end in object field value error.
  #[inline]
  pub const fn unexpected_end_of_object_field_value(
    hint: ObjectFieldValueHint,
    span: Span,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfObjectFieldValue(
        smear_lexer::tokora::error::UnexpectedEnd::with_name(
          0,
          CowStr::from_static("object field value"),
          hint,
        ),
      ),
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
      ErrorData::UnexpectedEndOfObjectExtension(
        smear_lexer::tokora::error::UnexpectedEnd::with_name(
          0,
          CowStr::from_static("object type extension"),
          hint,
        ),
      ),
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
      ErrorData::UnexpectedEndOfInterfaceExtension(
        smear_lexer::tokora::error::UnexpectedEnd::with_name(
          0,
          CowStr::from_static("interface type extension"),
          hint,
        ),
      ),
    )
  }

  /// Creates an unexpected end in enum type extension error.
  #[inline]
  pub const fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfEnumExtension(smear_lexer::tokora::error::UnexpectedEnd::with_name(
        0,
        CowStr::from_static("enum type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in input object type extension error.
  #[inline]
  pub const fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfInputObjectExtension(
        smear_lexer::tokora::error::UnexpectedEnd::with_name(
          0,
          CowStr::from_static("input object type extension"),
          hint,
        ),
      ),
    )
  }

  /// Creates an unexpected end in union type extension error.
  #[inline]
  pub const fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfUnionExtension(
        smear_lexer::tokora::error::UnexpectedEnd::with_name(
          0,
          CowStr::from_static("union type extension"),
          hint,
        ),
      ),
    )
  }

  /// Creates an unexpected end in schema extension error.
  #[inline]
  pub const fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfSchemaExtension(
        smear_lexer::tokora::error::UnexpectedEnd::with_name(
          0,
          CowStr::from_static("schema extension"),
          hint,
        ),
      ),
    )
  }

  /// Creates an unclosed bracket error.
  #[inline]
  pub const fn unclosed_bracket(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Bracket))
  }

  /// Creates an unclosed brace error.
  #[inline]
  pub const fn unclosed_brace(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Brace))
  }

  /// Creates an error from a lexer error.
  #[inline]
  pub const fn from_lexer_errors(err: LexerErrors<Char, StateError>, span: Span) -> Self {
    Self::new(span, ErrorData::Lexer(err))
  }

  /// Creates an unexpected end of input error.
  #[inline]
  pub const fn unexpected_end_of_input(span: Span) -> Self {
    Self::new(span, ErrorData::EndOfInput)
  }

  /// Creates an invalid fragment type path error.
  #[inline]
  pub const fn invalid_fragment_type_path(span: Span) -> Self {
    Self::new(span, ErrorData::InvalidFragmentTypePath)
  }

  /// Creates an invalid enum value error.
  #[inline]
  pub const fn invalid_enum_value(value: InvalidEnumValue, span: Span) -> Self {
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
  pub const fn data(&self) -> &ErrorData<S, T, Char, Expectation, StateError> {
    &self.data
  }

  /// Returns a mutable reference to the data of the error.
  #[inline]
  pub const fn data_mut(&mut self) -> &mut ErrorData<S, T, Char, Expectation, StateError> {
    &mut self.data
  }

  /// Consumes the error and returns its data.
  #[inline]
  pub fn into_data(self) -> ErrorData<S, T, Char, Expectation, StateError> {
    self.data
  }
}

#[cfg(feature = "smallvec")]
type DefaultErrorsContainer<S, T, Char = char, Exp = super::Expectation, StateError = ()> =
  smallvec::SmallVec<[Error<S, T, Char, Exp, StateError>; 1]>;

#[cfg(not(feature = "smallvec"))]
type DefaultErrorsContainer<S, T, Char = char, Exp = super::Expectation, StateError = ()> =
  std::vec::Vec<Error<S, T, Char, Exp, StateError>>;

/// A container for storing multiple parser errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<S, T, Char = char, Exp = super::Expectation, StateError = ()>(
  DefaultErrorsContainer<S, T, Char, Exp, StateError>,
);

impl<S, T, Char, Expectation, StateError> Default for Errors<S, T, Char, Expectation, StateError> {
  #[inline(always)]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<S, T, Char, Expectation, StateError> From<Error<S, T, Char, Expectation, StateError>>
  for Errors<S, T, Char, Expectation, StateError>
{
  #[inline(always)]
  fn from(error: Error<S, T, Char, Expectation, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<S, T, Char, Expectation, StateError> Errors<S, T, Char, Expectation, StateError> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<S, T, Char, Expectation, StateError> IntoIterator
  for Errors<S, T, Char, Expectation, StateError>
{
  type Item = Error<S, T, Char, Expectation, StateError>;
  type IntoIter =
    <DefaultErrorsContainer<S, T, Char, Expectation, StateError> as IntoIterator>::IntoIter;

  #[inline(always)]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<S, T, Char, Expectation, StateError> Extend<Error<S, T, Char, Expectation, StateError>>
  for Errors<S, T, Char, Expectation, StateError>
{
  #[inline(always)]
  fn extend<I: IntoIterator<Item = Error<S, T, Char, Expectation, StateError>>>(
    &mut self,
    iter: I,
  ) {
    self.0.extend(iter);
  }
}
