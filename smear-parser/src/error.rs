use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::Span;

/// The hint about what is expected for the next character
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum ExponentHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
  /// Expect the next character to be a sign or a digit.
  #[display("'+', '-' or digit")]
  SignOrDigit,
  /// Expect the next character to be an exponent identifier 'e' or 'E'.
  #[display("'e' or 'E'")]
  Identifier,
}

/// The hint about what is expected for the next character
#[derive(
  Copy,
  Clone,
  Debug,
  Display,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Hash,
  From,
  IsVariant,
  Unwrap,
  TryUnwrap,
)]
pub enum IntHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
}

/// The hint about what is expected for the float
#[derive(
  Copy,
  Clone,
  Display,
  Debug,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Hash,
  From,
  IsVariant,
  Unwrap,
  TryUnwrap,
)]
pub enum FloatHint {
  /// Expect the next character to be fractional digits.
  #[display("fractional digits")]
  Fractional,
  /// Expect the next character to be an exponent part.
  #[display("_0")]
  Exponent(ExponentHint),
  /// Expect the next character to be a digit.
  #[display("digit")]
  Digit,
}

/// An unpaired surrogate error.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Display, IsVariant)]
pub enum UnpairedSurrogateHint {
  /// An unpaired high surrogate.
  #[display("high surrogate")]
  High,
  /// An unpaired low surrogate.
  #[display("low surrogate")]
  Low,
}

/// An unterminated string hint.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum LitStrDelimiterHint {
  /// A double quote character.
  #[display("\"")]
  Quote,
  /// A triple quote sequence.
  #[display(r#"""""#)]
  TripleQuote,

  /// A double quote character or a triple quote sequence.
  #[display(r#"" or """"#)]
  QuoteOrTripleQuote,
}

/// A hint about what line terminator was found.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum LineTerminatorHint {
  /// A line feed character.
  #[display("'\\n'")]
  NewLine,
  /// A carriage return character.
  #[display("'\\r'")]
  CarriageReturn,
  /// A carriage return followed by a line feed character.
  #[display("'\\r\\n'")]
  CarriageReturnNewLine,
}

/// A hint about what line terminator was found.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum WhiteSpaceHint {
  /// A space character.
  #[display(" ")]
  Space,
  /// A horizontal space character.
  #[display("'\\t'")]
  Tab,
}

/// A hint for what was expected in a object field value.
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

/// Hints for the next component was expected while parsing an enum type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum EnumTypeExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Enum values definition.
  #[display("enum values definition")]
  EnumValuesDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Enum keyword.
  #[display("enum keyword")]
  Enum,
  /// Directives or enum values definition.
  #[display("directives or enum values definition")]
  DirectivesOrEnumValuesDefinition,
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing an enum type extension.
pub trait UnexpectedEndOfEnumExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing an enum type extension.
  fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self;
}

/// Hints for the next component was expected while parsing an input object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum InputObjectTypeExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Input fields definition.
  #[display("input fields definition")]
  InputFieldsDefinition,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Input keyword.
  #[display("input keyword")]
  Input,
  /// Directives or input fields definition.
  #[display("directives or input fields definition")]
  DirectivesOrInputFieldsDefinition,
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

/// Hints for the next component was expected while parsing a union type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum UnionTypeExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Union member types.
  #[display("union member types")]
  UnionMemberTypes,
  /// Name.
  #[display("name")]
  Name,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Union keyword.
  #[display("union keyword")]
  Union,
  /// Directives or union member types.
  #[display("directives or union member types")]
  DirectivesOrUnionMemberTypes,
}

/// An error which can occur when an unexpected end of input is encountered
/// while parsing a union type extension.
pub trait UnexpectedEndOfUnionExtensionError {
  /// Creates a new error indicating that an unexpected end of input was encountered
  /// while parsing a union type extension.
  fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self;
}

/// Hints for the next component was expected while parsing a schema extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum SchemaExtensionHint {
  /// Directives.
  #[display("directives")]
  Directives,
  /// Root operation types definition.
  #[display("root operation types definition")]
  RootOperationTypesDefinition,
  /// Extend keyword.
  #[display("extend keyword")]
  Extend,
  /// Schema keyword.
  #[display("schema keyword")]
  Schema,
  /// Directives or root operation types definition.
  #[display("directives or root operation types definition")]
  DirectivesOrRootOperationTypesDefinition,
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

/// An error which can occur when an unclosed object value is encountered.
pub trait UnclosedObjectValueError {
  /// Creates a new error indicating that an unclosed object value was encountered.
  fn unclosed_object(span: Span) -> Self;
}

/// An error which can occur when an unclosed list value is encountered.
pub trait UnclosedListValueError {
  /// Creates a new error indicating that an unclosed list value was encountered.
  fn unclosed_list(span: Span) -> Self;
}
