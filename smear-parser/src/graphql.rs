/// Errors for GraphQL parsers
pub mod error;

pub use impls::*;

mod impls;

/// The abstract syntax tree (AST) definitions and parsers for GraphQL language.
pub mod ast;

/// The GraphQL syntax definitions descriptions.
pub mod syntax;

/// GraphQL Concrete Syntax Tree node kind.
///
/// Represents all possible node types in a GraphQL CST, including both tokens
/// (leaves) and composite nodes (branches). This enum can be converted to
/// `rowan::SyntaxKind` for use with the rowan library.
///
/// ## Node Categories
///
/// - **Tokens**: Leaf nodes representing individual tokens (identifiers, punctuation, literals)
/// - **Trivia**: Whitespace, comments, commas (preserved in lossless parsing)
/// - **Composite Nodes**: Branch nodes representing grammatical constructs (types, fields, operations)
#[allow(clippy::upper_case_acronyms, bad_style)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[non_exhaustive]
pub enum SyntaxKind {
  // ============================================================================
  // Tokens (Leaf Nodes)
  // ============================================================================
  /// Identifier (e.g., `User`, `query`, `id`)
  Identifier,

  /// Name
  Name,

  /// Integer literal (e.g., `42`, `-10`)
  Int,

  /// Float literal (e.g., `3.14`, `-2.5e10`)
  Float,

  /// Inline string literal (e.g., `"hello"`)
  InlineString,

  /// Block string literal (e.g., `"""multi\nline"""`)
  BlockString,

  /// String literal (either inline or block)
  String,

  // ============================================================================
  // Keyword Tokens
  // ============================================================================
  /// `query` keyword
  query_KW,

  /// `mutation` keyword
  mutation_KW,

  /// `subscription` keyword
  subscription_KW,

  /// `fragment` keyword
  fragment_KW,

  /// `on` keyword
  on_KW,

  /// `null` keyword
  null_KW,

  /// `true` keyword
  true_KW,

  /// `false` keyword
  false_KW,

  /// `type` keyword
  type_KW,

  /// `interface` keyword
  interface_KW,

  /// `union` keyword
  union_KW,

  /// `enum` keyword
  enum_KW,

  /// `input` keyword
  input_KW,

  /// `scalar` keyword
  scalar_KW,

  /// `schema` keyword
  schema_KW,

  /// `directive` keyword
  directive_KW,

  /// `extend` keyword
  extend_KW,

  /// `implements` keyword
  implements_KW,

  /// `repeatable` keyword
  repeatable_KW,

  // ============================================================================
  // Directive Locations Keywords
  // ============================================================================
  /// `QUERY` directive location
  QUERY_KW,
  /// `MUTATION` directive location
  MUTATION_KW,
  /// `SUBSCRIPTION` directive location
  SUBSCRIPTION_KW,
  /// `FIELD` directive location
  FIELD_KW,
  /// `FRAGMENT_DEFINITION` directive location
  FRAGMENT_DEFINITION_KW,
  /// `FRAGMENT_SPREAD` directive location
  FRAGMENT_SPREAD_KW,
  /// `INLINE_FRAGMENT` directive location
  INLINE_FRAGMENT_KW,
  /// `VARIABLE_DEFINITION` directive location
  VARIABLE_DEFINITION_KW,
  /// `SCHEMA` directive location
  SCHEMA_KW,
  /// `SCALAR` directive location
  SCALAR_KW,
  /// `OBJECT` directive location
  OBJECT_KW,
  /// `FIELD_DEFINITION` directive location
  FIELD_DEFINITION_KW,
  /// `ARGUMENT_DEFINITION` directive location
  ARGUMENT_DEFINITION_KW,
  /// `INTERFACE` directive location
  INTERFACE_KW,
  /// `UNION` directive location
  UNION_KW,
  /// `ENUM` directive location
  ENUM_KW,
  /// `ENUM_VALUE` directive location
  ENUM_VALUE_KW,
  /// `INPUT_OBJECT` directive location
  INPUT_OBJECT_KW,
  /// `INPUT_FIELD_DEFINITION` directive location
  INPUT_FIELD_DEFINITION_KW,

  // ============================================================================
  // Punctuation Tokens
  // ============================================================================
  /// Dollar sign `$` (variable prefix)
  Dollar,

  /// Left parenthesis `(`
  LParen,

  /// Right parenthesis `)`
  RParen,

  /// Spread operator `...`
  Spread,

  /// Colon `:`
  Colon,

  /// Equal sign `=`
  Equal,

  /// At sign `@`
  At,

  /// Left square bracket `[`
  LBracket,

  /// Right square bracket `]`
  RBracket,

  /// Left curly brace `{`
  LBrace,

  /// Right curly brace `}`
  RBrace,

  /// Pipe `|`
  Pipe,

  /// Exclamation mark `!` (non-null modifier)
  Bang,

  /// Ampersand `&` (implements separator)
  Ampersand,

  // ============================================================================
  // Trivia Nodes (Whitespace, Comments, Commas)
  // ============================================================================
  /// Whitespace
  Whitespace,

  /// Tab
  Tab,

  /// Newline
  Newline,

  /// Carriage return
  CarriageReturn,

  /// Carriage return + Newline
  CarriageReturnNewline,

  /// Comment (from `#` to end of line)
  Comment,

  /// Comma `,` (optional separator)
  Comma,

  // ============================================================================
  // Type System Nodes
  // ============================================================================
  /// Named type (e.g., `String`, `User`)
  NamedType,

  /// List type (e.g., `[String]`)
  ListType,

  /// Non-null type (e.g., `String!`, `[User]!`)
  NonNullType,

  // ============================================================================
  // Type Definition Nodes
  // ============================================================================
  /// Scalar type definition (e.g., `scalar DateTime`)
  ScalarTypeDefinition,

  /// Object type definition (e.g., `type User { ... }`)
  ObjectTypeDefinition,

  /// Interface type definition (e.g., `interface Node { ... }`)
  InterfaceTypeDefinition,

  /// Union type definition (e.g., `union SearchResult = User | Post`)
  UnionTypeDefinition,

  /// Enum type definition (e.g., `enum Role { ... }`)
  EnumTypeDefinition,

  /// Input object type definition (e.g., `input CreateUserInput { ... }`)
  InputObjectTypeDefinition,

  // ============================================================================
  // Type Extension Nodes
  // ============================================================================
  /// Scalar type extension
  ScalarTypeExtension,

  /// Object type extension
  ObjectTypeExtension,

  /// Interface type extension
  InterfaceTypeExtension,

  /// Union type extension
  UnionTypeExtension,

  /// Enum type extension
  EnumTypeExtension,

  /// Input object type extension
  InputObjectTypeExtension,

  // ============================================================================
  // Field and Argument Definition Nodes
  // ============================================================================
  /// Field definition (e.g., `name: String!`)
  FieldDefinition,

  /// Fields definition list (e.g., `{ id: ID! name: String }`)
  FieldsDefinition,

  /// Input value definition (argument or input field)
  InputValueDefinition,

  /// Arguments definition (e.g., `(id: ID!)`)
  ArgumentsDefinition,

  /// Input fields definition (e.g., `{ name: String }`)
  InputFieldsDefinition,

  // ============================================================================
  // Directive Nodes
  // ============================================================================
  /// Directive definition (e.g., `directive @auth ...`)
  DirectiveDefinition,

  /// Directive application (e.g., `@deprecated`)
  Directive,

  /// List of directives (e.g., `@auth @log`)
  Directives,

  /// Directive location (e.g., `on FIELD | ARGUMENT_DEFINITION`)
  DirectiveLocation,

  /// Directive locations (e.g., `on FIELD | ARGUMENT_DEFINITION`)
  DirectiveLocations,

  // ============================================================================
  // Schema Definition Nodes
  // ============================================================================
  /// Schema definition (e.g., `schema { query: Query }`)
  SchemaDefinition,

  /// Schema extension
  SchemaExtension,

  /// Root operation type definition (e.g., `query: Query`)
  RootOperationTypeDefinition,

  /// Root operation types definition list
  RootOperationTypesDefinition,

  // ============================================================================
  // Enum Nodes
  // ============================================================================
  /// Enum value definition (e.g., `ADMIN`)
  EnumValueDefinition,

  /// Enum values definition list
  EnumValuesDefinition,

  // ============================================================================
  // Union Nodes
  // ============================================================================
  /// Union member types (e.g., `User | Post`)
  UnionMembers,

  /// Union member type (e.g., `User` or `| Post`)
  UnionMember,

  // ============================================================================
  // Interface Implementation Nodes
  // ============================================================================
  /// Implements interfaces (e.g., `implements Node & Timestamped`)
  ImplementsInterfaces,
  /// Implements interface member (e.g., `Node` or `& Timestamped`)
  ImplementInterfaceMember,

  // ============================================================================
  // Executable Definition Nodes
  // ============================================================================
  /// The name of an operation (e.g., `query`, `mutation`, `subscription`)
  OperationName,

  /// Named operation definition (e.g., `query GetUser { ... }`)
  NamedOperationDefinition,

  /// Fragment name
  FragmentName,

  /// Fragment definition (e.g., `fragment UserFields on User { ... }`)
  FragmentDefinition,

  /// Fragment spread (e.g., `...UserFields`)
  FragmentSpread,

  /// Inline fragment (e.g., `... on User { ... }`)
  InlineFragment,

  // ============================================================================
  // Selection Nodes
  // ============================================================================
  /// Field selection (e.g., `name`, `user(id: 1) { name }`)
  Field,

  /// Selection set (e.g., `{ id name }`)
  SelectionSet,

  /// Field alias (e.g., `userName: name`)
  Alias,

  // ============================================================================
  // Variable Nodes
  // ============================================================================
  /// Variable definition (e.g., `$id: ID!`)
  VariableDefinition,

  /// Variables definition (e.g., `($id: ID!, $name: String)`)
  VariablesDefinition,

  /// Variable value (e.g., `$id`)
  VariableValue,

  // ============================================================================
  // Argument Nodes
  // ============================================================================
  /// Argument (e.g., `id: $userId`)
  Argument,

  /// Arguments list (e.g., `(id: $userId, name: "John")`)
  Arguments,

  // ============================================================================
  // Value Nodes
  // ============================================================================
  /// Input value (any value in input position)
  InputValue,

  /// Const input value (value without variables)
  ConstInputValue,

  /// Boolean value (`true` or `false`)
  BooleanValue,

  /// Null value (`null`)
  NullValue,

  /// Enum value (e.g., `ADMIN`)
  EnumValue,

  /// List value (e.g., `[1, 2, 3]`)
  ListValue,

  /// Object value (e.g., `{ name: "John" }`)
  ObjectValue,

  /// Object field (e.g., `name: "John"`)
  ObjectField,

  // ============================================================================
  // Miscellaneous Nodes
  // ============================================================================
  /// Type condition (e.g., `on User`)
  TypeCondition,

  /// Description (string literal used as documentation)
  Description,

  /// Default value (e.g., `= 42`)
  DefaultValue,

  // ============================================================================
  // Sum Types
  // ============================================================================
  /// Type Definition
  TypeDefinition,
  /// Type extension
  TypeExtension,
  /// Type system definition (type, directive, schema)
  TypeSystemDefinition,
  /// Executable definition (operation or fragment)
  ExecutableDefinition,
  /// Type system extension (type system extension)
  TypeSystemExtension,
  /// Type system definition or extension
  TypeSystemDefinitionOrExtension,
  /// Definition (type system or executable)
  Definition,
  /// Definition or extension (definition or extension)
  DefinitionOrExtension,

  // ============================================================================
  // Document Nodes
  // ============================================================================
  /// Type system document (schema definitions)
  TypeSystemDocument,

  /// Executable document (queries, mutations, subscriptions)
  ExecutableDocument,

  /// Full GraphQL document
  Document,

  // ============================================================================
  // Error Recovery Nodes
  // ============================================================================
  /// Error node (for parser error recovery)
  Error,

  /// Root node (top-level container)
  Root,
}
