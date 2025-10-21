//! GraphQLx Concrete Syntax Tree (CST) node kinds.
//!
//! This module provides `SyntaxKind`, an enum representing all possible node types
//! in a GraphQLx CST. GraphQLx extends standard GraphQL with generics, imports, type paths,
//! maps, sets, and extended numeric literals.
//!
//! ## Usage with Rowan
//!
//! ```rust,ignore
//! use smear_scaffold::cst::graphqlx::SyntaxKind;
//! use rowan::SyntaxKind;
//!
//! let kind = SyntaxKind::TypeParameter;
//! let rowan_kind: SyntaxKind = kind.into();
//!
//! // Keywords are distinct from identifiers
//! assert!(SyntaxKind::QueryKw.is_keyword());
//! assert!(SyntaxKind::ImportKw.is_keyword());
//! assert!(SyntaxKind::ImportKw.is_graphqlx_extension());
//! assert!(!SyntaxKind::Identifier.is_keyword());
//!
//! // Extended numeric literals
//! assert!(SyntaxKind::Hex.is_int_literal());
//! assert!(SyntaxKind::HexFloat.is_float_literal());
//! assert!(SyntaxKind::Binary.is_graphqlx_extension());
//! ```

/// GraphQLx Concrete Syntax Tree node kind.
///
/// Represents all possible node types in a GraphQLx CST, including standard GraphQL nodes
/// plus extensions for generics, imports, type paths, maps, sets, and extended numeric literals.
/// This enum can be converted to `rowan::SyntaxKind` for use with the rowan library.
///
/// ## Node Categories
///
/// - **Standard GraphQL Nodes**: All nodes from standard GraphQL
/// - **Extended Numeric Literals**: Hexadecimal (`0xFF`), binary (`0b101`), octal (`0o77`), hex float (`0x1.8p3`)
/// - **Generic Type System**: Type parameters, where clauses, constraints
/// - **Import System**: Import statements, import specifiers
/// - **Type Paths**: Namespaced type references (e.g., `user::User`, `::Global`)
/// - **Extended Collections**: Map types (`<K => V>`), set types (`<T>`)
#[allow(clippy::upper_case_acronyms, bad_style)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[non_exhaustive]
pub enum SyntaxKind {
  // ============================================================================
  // Tokens (Leaf Nodes)
  // ============================================================================
  /// Identifier token (e.g., `User`, `query`, `id`)
  Identifier,

  /// Int integer literal token (e.g., `42`, `-10`)
  Int,

  /// Hexadecimal integer literal token (e.g., `0xFF`, `0x2A`)
  Hex,

  /// Binary integer literal token (e.g., `0b101`, `0b1010`)
  Binary,

  /// Octal integer literal token (e.g., `0o77`, `0o52`)
  Octal,

  /// Decimal float literal token (e.g., `3.14`, `-2.5e10`)
  Float,

  /// Hexadecimal float literal token (e.g., `0x1.8p3`, `0xA.Bp-2`)
  HexFloat,

  /// Inline string literal token (e.g., `"hello"`)
  InlineString,

  /// Block string literal token (e.g., `"""multi\nline"""`)
  BlockString,

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

  /// `import` keyword (GraphQLx extension)
  import_KW,

  /// `from` keyword (GraphQLx extension)
  from_KW,

  /// `as` keyword (GraphQLx extension)
  as_KW,

  /// `where` keyword (GraphQLx extension)
  where_KW,

  /// `set` keyword (GraphQLx extension)
  set_KW,

  /// `map` keyword (GraphQLx extension)
  map_KW,

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

  /// Ampersand `&` (implements separator, where clause separator)
  Ampersand,

  /// Left angle bracket `<` (type parameters, set/map open)
  LAngle,

  /// Right angle bracket `>` (type parameters, set/map close)
  RAngle,

  /// Fat arrow `=>` (map type separator)
  FatArrow,

  /// Plus `+` (trait bound combiner)
  Plus,

  /// Minus `-` (for negative numbers)
  Minus,

  /// Path separator `::` (namespace separator)
  PathSeparator,

  /// Asterisk `*` (wildcard import)
  Asterisk,

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

  /// Set type (e.g., `<String>`, `<User!>`)
  SetType,

  /// Map type (e.g., `<String => Int>`, `<ID! => User>`)
  MapType,

  // ============================================================================
  // Type Path Nodes (GraphQLx Extension)
  // ============================================================================
  /// Type path (e.g., `user::User`, `::Global`, `std::Option<T>`)
  TypePath,

  /// Path segment (single identifier in a path)
  PathSegment,

  /// Fully qualified path (starts with `::`)
  FullyQualifiedPath,

  // ============================================================================
  // Generic Type System Nodes (GraphQLx Extension)
  // ============================================================================
  /// Type parameter (e.g., `T`, `K`, `V`)
  TypeParameter,

  /// Type parameters list (e.g., `<T, K, V>`)
  TypeParameters,

  /// Type generics (type parameters with constraints)
  TypeGenerics,

  /// Definition type generics (for type definitions)
  DefinitionTypeGenerics,

  /// Extension type generics (for type extensions)
  ExtensionTypeGenerics,

  /// Executable definition type generics (for operations/fragments)
  ExecutableDefinitionTypeGenerics,

  /// Where clause (e.g., `where T: Node`)
  WhereClause,

  /// Where predicate (e.g., `T: Node`)
  WherePredicate,

  /// Type bound (e.g., `Node`, `Node + Timestamped`)
  TypeBound,

  /// Type constraint (e.g., `: Node`)
  TypeConstraint,

  /// Default type (e.g., `= String`)
  DefaultType,

  // ============================================================================
  // Import System Nodes (GraphQLx Extension)
  // ============================================================================
  /// Import definition (e.g., `import { User } from "./types.graphqlx"`)
  ImportDefinition,

  /// Import clause (what to import)
  ImportClause,

  /// Import list (e.g., `{ User, Post }`)
  ImportList,

  /// Import member (single imported item)
  ImportMember,

  /// Named import specifier (e.g., `User`, `User as UserType`)
  NamedSpecifier,

  /// Wildcard import specifier (e.g., `*`, `* as types`)
  WildcardSpecifier,

  /// Import alias (e.g., `as UserType`)
  ImportAlias,

  // ============================================================================
  // Type Definition Nodes
  // ============================================================================
  /// Scalar type definition (e.g., `scalar DateTime`)
  ScalarTypeDefinition,

  /// Object type definition (e.g., `type User<T> { ... }`)
  ObjectTypeDefinition,

  /// Interface type definition (e.g., `interface Node<T> { ... }`)
  InterfaceTypeDefinition,

  /// Union type definition (e.g., `union SearchResult = User | Post`)
  UnionTypeDefinition,

  /// Enum type definition (e.g., `enum Role { ... }`)
  EnumTypeDefinition,

  /// Input object type definition (e.g., `input CreateUserInput<T> { ... }`)
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
  /// Directive definition (e.g., `directive @auth<T> ...`)
  DirectiveDefinition,

  /// Directive application (e.g., `@deprecated`, `@auth<User>`)
  Directive,

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
  UnionMemberTypes,

  // ============================================================================
  // Interface Implementation Nodes
  // ============================================================================
  /// Implements interfaces (e.g., `implements Node & Timestamped`)
  ImplementsInterfaces,

  // ============================================================================
  // Executable Definition Nodes
  // ============================================================================
  /// Operation definition (query, mutation, subscription)
  OperationDefinition,

  /// Named operation definition (e.g., `query GetUser<T> { ... }`)
  NamedOperationDefinition,

  /// Fragment definition (e.g., `fragment UserFields<T> on User<T> { ... }`)
  FragmentDefinition,

  /// Fragment spread (e.g., `...UserFields`, `...UserFields<String>`)
  FragmentSpread,

  /// Inline fragment (e.g., `... on User<T> { ... }`)
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

  /// Enum value (e.g., `ADMIN`, `user::Role::ADMIN`)
  EnumValue,

  /// List value (e.g., `[1, 2, 3]`)
  ListValue,

  /// Set value (e.g., `set { 1, 2, 3 }`)
  SetValue,

  /// Map value (e.g., `map { "key" => "value" }`)
  MapValue,

  /// Map entry (e.g., `"key" => "value"`)
  MapEntry,

  /// Object value (e.g., `{ name: "John" }`)
  ObjectValue,

  /// Object field (e.g., `name: "John"`)
  ObjectField,

  // ============================================================================
  // Miscellaneous Nodes
  // ============================================================================
  /// Type condition (e.g., `on User<T>`)
  TypeCondition,

  /// Description (string literal used as documentation)
  Description,

  /// Default value (e.g., `= 42`)
  DefaultValue,

  /// Definition name (name with optional generics)
  DefinitionName,

  /// Extension name (name for extensions)
  ExtensionName,

  /// Executable definition name (name for operations/fragments)
  ExecutableDefinitionName,

  /// Fragment type path (type path in fragment context)
  FragmentTypePath,

  // ============================================================================
  // Document Nodes
  // ============================================================================
  /// Type system document (schema definitions)
  TypeSystemDocument,

  /// Executable document (queries, mutations, subscriptions)
  ExecutableDocument,

  /// Full GraphQLx document
  Document,

  // ============================================================================
  // Error Recovery Nodes
  // ============================================================================
  /// Error node (for parser error recovery)
  Error,

  /// Root node (top-level container)
  Root,

  #[doc(hidden)]
  __LAST,
}

impl From<rowan::SyntaxKind> for SyntaxKind {
  #[inline]
  fn from(raw: rowan::SyntaxKind) -> Self {
    assert!(raw.0 <= (SyntaxKind::__LAST as u16));
    unsafe { core::mem::transmute::<u16, SyntaxKind>(raw.0) }
  }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
  #[inline]
  fn from(kind: SyntaxKind) -> Self {
    Self(kind as u16)
  }
}

impl From<SyntaxKind> for u16 {
  #[inline]
  fn from(kind: SyntaxKind) -> Self {
    kind as u16
  }
}

impl SyntaxKind {
  /// Returns `true` if this kind represents a trivia node (whitespace, comment, comma).
  #[inline]
  pub const fn is_trivia(self) -> bool {
    matches!(self, Self::Whitespace | Self::Comment | Self::Comma)
  }

  /// Returns `true` if this kind represents an integer literal token.
  #[inline]
  pub const fn is_int_literal(self) -> bool {
    matches!(self, Self::Int | Self::Hex | Self::Binary | Self::Octal)
  }

  /// Returns `true` if this kind represents a float literal token.
  #[inline]
  pub const fn is_float_literal(self) -> bool {
    matches!(self, Self::Float | Self::HexFloat)
  }

  /// Returns `true` if this kind represents any numeric literal token.
  #[inline]
  pub const fn is_numeric_literal(self) -> bool {
    self.is_int_literal() || self.is_float_literal()
  }
}

/// A language implementation for use in `Rowan`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GraphQLxLanguage {}

impl rowan::Language for GraphQLxLanguage {
  type Kind = SyntaxKind;

  #[inline]
  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    raw.into()
  }

  #[inline]
  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    kind.into()
  }
}

/// A syntax node in the GraphQLx CST.
///
/// This is a typed wrapper around `rowan::SyntaxNode` specialized for GraphQLx.
pub type SyntaxNode = rowan::SyntaxNode<GraphQLxLanguage>;

/// A syntax token (leaf node) in the GraphQLx CST.
///
/// This is a typed wrapper around `rowan::SyntaxToken` specialized for GraphQLx.
pub type SyntaxToken = rowan::SyntaxToken<GraphQLxLanguage>;

/// A syntax element (either a node or token) in the GraphQLx CST.
///
/// This is a typed wrapper around `rowan::SyntaxElement` specialized for GraphQLx.
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

/// Iterator over child nodes of a GraphQLx syntax node.
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<GraphQLxLanguage>;

/// Iterator over child elements (nodes and tokens) of a GraphQLx syntax node.
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<GraphQLxLanguage>;
