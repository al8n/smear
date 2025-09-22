use derive_more::{Display, IsVariant};
use logosky::utils::Span;

/// Hints for parsing a variable value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum VariableValueHint {
  /// A [`Name`](crate::parser::ast::Name) was expected.
  #[display("name")]
  Name,
  /// A [`Dollar`](crate::parser::ast::Dollar) was expected.
  #[display("dollar")]
  Dollar,
}

/// An error which can occur when parsing a variable value.
pub trait ParseVariableValueError<Name> {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing a variable value.
  fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self;

  /// Creates a new error indicating that a dollar token is missing
  /// while parsing a variable value.
  fn missing_dollar_token(name: Name, span: Span) -> Self;
}

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

/// Hints for the next component was expected while parsing an object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum ObjectTypeExtensionHint {
  /// Implements interfaces.
  #[display("implements")]
  Implements,
  /// Directives.
  #[display("directives")]
  Directives,
  /// Fields definition.
  #[display("fields definition")]
  FieldsDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Type keyword.
  #[display("type keyword")]
  Type,
  /// Implements, directives, or fields definition.
  #[display("implements, directives, or fields definition")]
  ImplementsOrDirectivesOrFieldsDefinition,
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing an object type extension.
pub trait UnexpectedEndOfObjectExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing an object type extension.
  fn unexpected_end_of_object_extension(span: Span, hint: ObjectTypeExtensionHint) -> Self;
}

/// Hints for the next component was expected while parsing an interface type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum InterfaceTypeExtensionHint {
  /// Implements interfaces.
  #[display("implements")]
  Implements,
  /// Directives.
  #[display("directives")]
  Directives,
  /// Fields definition.
  #[display("fields definition")]
  FieldsDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Interface keyword.
  #[display("interface keyword")]
  Interface,
  /// Implements, directives, or fields definition.
  #[display("implements, directives, or fields definition")]
  ImplementsOrDirectivesOrFieldsDefinition,
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing an interface type extension.
pub trait UnexpectedEndOfInterfaceExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing an interface type extension.
  fn unexpected_end_of_interface_extension(span: Span, hint: InterfaceTypeExtensionHint) -> Self;
}
