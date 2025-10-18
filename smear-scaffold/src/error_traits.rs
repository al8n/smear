use logosky::utils::Span;

use crate::hints::*;


/// An error which can occur when an unexpected token is encountered.
pub trait UnexpectedTokenError {
  /// The actual token type.
  type Token<'a>
  where
    Self: 'a;
  /// The expected token kind type.
  type TokenKind;

  /// Creates a new error indicating that an unexpected token was encountered
  /// while parsing.
  fn unexpected_token<'a>(found: Self::Token<'a>, expected: Self::TokenKind, span: Span) -> Self
  where
    Self: 'a;
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing an object type extension.
pub trait UnexpectedEndOfObjectExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing an object type extension.
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self;
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing an interface type extension.
pub trait UnexpectedEndOfInterfaceExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing an interface type extension.
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self;
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing an enum type extension.
pub trait UnexpectedEndOfEnumExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing an enum type extension.
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self;
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing an input object type extension.
pub trait UnexpectedEndOfInputObjectExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing an input object type extension.
  fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self;
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing a union type extension.
pub trait UnexpectedEndOfUnionExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing a union type extension.
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self;
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing a schema extension.
pub trait UnexpectedEndOfSchemaExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing a schema extension.
  fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self
  where
    Self: Sized;
}

/// An error which can occur when an unclosed brace is encountered.
pub trait UnclosedBraceError {
  /// Creates a new error indicating that an unclosed brace value was encountered.
  fn unclosed_brace(span: Span) -> Self;
}

/// An error which can occur when an unclosed bracket is encountered.
pub trait UnclosedBracketError {
  /// Creates a new error indicating that an unclosed bracket value was encountered.
  fn unclosed_bracket(span: Span) -> Self;
}

/// An error which can occur when parsing a fragment path
pub trait InvalidFragmentTypePath {
  /// Creates a new error indicating that an invalid fragment path.
  fn invalid_fragment_type_path(span: Span) -> Self;
}
