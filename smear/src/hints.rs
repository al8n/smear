use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};

/// The hint about what is expected for the next character
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display, IsVariant)]
#[non_exhaustive]
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
/// in a hexadecimal float literal exponent.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display, IsVariant)]
#[non_exhaustive]
pub enum HexExponentHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
  /// Expect the next character to be a sign or a digit.
  #[display("'+', '-' or digit")]
  SignOrDigit,
  /// Expect the next character to be an exponent identifier 'p' or 'P'.
  #[display("'p' or 'P'")]
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
#[non_exhaustive]
pub enum DecimalHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
}

/// The hint about what is expected for the next character
/// in a hexadecimal literal.
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
#[non_exhaustive]
pub enum HexHint {
  /// Expect the next character to be a hex digit.
  #[display("hex digit")]
  Digit,
}

/// The hint about what is expected for the next character
/// in a octal literal.
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
#[non_exhaustive]
pub enum OctalHint {
  /// Expect the next character to be an octal digit.
  #[display("octal digit")]
  Digit,
}

/// The hint about what is expected for the next character
/// in a binary literal.
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
#[non_exhaustive]
pub enum BinaryHint {
  /// Expect the next character to be a binary digit.
  /// '0' or '1'.
  #[display("binary digit")]
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
#[non_exhaustive]
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

/// The hint about what is expected for the hexadecimal float
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
#[non_exhaustive]
pub enum HexFloatHint {
  /// Expect the next character to be fractional digits.
  #[display("fractional digits")]
  Fractional,
  /// Expect the next character to be a hex exponent part.
  #[display("_0")]
  Exponent(HexExponentHint),
  /// Expect the next character to be a hex digit.
  #[display("digit")]
  Digit,
}

/// An unpaired unicode surrogate error.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Display, IsVariant)]
#[non_exhaustive]
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
#[non_exhaustive]
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
#[non_exhaustive]
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
#[non_exhaustive]
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
#[non_exhaustive]
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
#[non_exhaustive]
pub enum VariableValueHint {
  /// A [`Name`](crate::parser::ast::Name) was expected.
  #[display("name")]
  Name,
  /// A [`Dollar`](crate::parser::ast::Dollar) was expected.
  #[display("dollar")]
  Dollar,
}

/// Hints for the next component was expected while parsing a schema extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
#[non_exhaustive]
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

/// Hints for the next component was expected while parsing a union type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
#[non_exhaustive]
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

/// Hints for the next component was expected while parsing an input object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
#[non_exhaustive]
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

/// Hints for the next component was expected while parsing an object type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
#[non_exhaustive]
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

/// Hints for the next component was expected while parsing an interface type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
#[non_exhaustive]
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

/// Hints for the next component was expected while parsing an enum type extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
#[non_exhaustive]
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
