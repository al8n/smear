//! GraphQL Concrete Syntax Tree (CST) node kinds.
//!
//! This module provides `SyntaxKind`, an enum representing all possible node types
//! in a GraphQL CST. It can be converted to `rowan::SyntaxKind` for use with the rowan
//! red-green tree library.
//!
//! ## Usage with Rowan
//!
//! ```rust,ignore
//! use smear_scaffold::cst::graphql::SyntaxKind;
//! use rowan::SyntaxKind;
//!
//! let kind = SyntaxKind::Identifier;
//! let rowan_kind: SyntaxKind = kind.into();
//!
//! // Keywords are distinct from identifiers
//! assert!(SyntaxKind::QueryKw.is_keyword());
//! assert!(SyntaxKind::TypeKw.is_keyword());
//! assert!(!SyntaxKind::Identifier.is_keyword());
//! ```

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
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[non_exhaustive]
pub enum SyntaxKind {
  // ============================================================================
  // Tokens (Leaf Nodes)
  // ============================================================================
  /// Identifier token (e.g., `User`, `query`, `id`)
  Identifier,

  /// Integer literal token (e.g., `42`, `-10`)
  Int,

  /// Float literal token (e.g., `3.14`, `-2.5e10`)
  Float,

  /// Inline string literal token (e.g., `"hello"`)
  InlineString,

  /// Block string literal token (e.g., `"""multi\nline"""`)
  BlockString,

  // ============================================================================
  // Keyword Tokens
  // ============================================================================
  /// `query` keyword
  QueryKw,

  /// `mutation` keyword
  MutationKw,

  /// `subscription` keyword
  SubscriptionKw,

  /// `fragment` keyword
  FragmentKw,

  /// `on` keyword
  OnKw,

  /// `null` keyword
  NullKw,

  /// `true` keyword
  TrueKw,

  /// `false` keyword
  FalseKw,

  /// `type` keyword
  TypeKw,

  /// `interface` keyword
  InterfaceKw,

  /// `union` keyword
  UnionKw,

  /// `enum` keyword
  EnumKw,

  /// `input` keyword
  InputKw,

  /// `scalar` keyword
  ScalarKw,

  /// `schema` keyword
  SchemaKw,

  /// `directive` keyword
  DirectiveKw,

  /// `extend` keyword
  ExtendKw,

  /// `implements` keyword
  ImplementsKw,

  /// `repeatable` keyword
  RepeatableKw,

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
  /// Whitespace (spaces, tabs, newlines)
  Whitespace,

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

  /// Named operation definition (e.g., `query GetUser { ... }`)
  NamedOperationDefinition,

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

  #[doc(hidden)]
  __LAST,
}

impl From<SyntaxKind> for u16 {
  #[inline]
  fn from(kind: SyntaxKind) -> Self {
    kind as u16
  }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
  #[inline]
  fn from(kind: SyntaxKind) -> Self {
    Self(kind as u16)
  }
}

impl From<rowan::SyntaxKind> for SyntaxKind {
  #[inline]
  fn from(raw: rowan::SyntaxKind) -> Self {
    assert!(raw.0 <= (SyntaxKind::__LAST as u16));
    unsafe { core::mem::transmute::<u16, SyntaxKind>(raw.0) }
  }
}

impl SyntaxKind {
  /// Returns `true` if this kind represents a trivia node (whitespace, comment, comma).
  #[inline]
  pub const fn is_trivia(self) -> bool {
    matches!(self, Self::Whitespace | Self::Comment | Self::Comma)
  }

  /// Returns `true` if this kind represents a keyword token.
  #[inline]
  pub const fn is_keyword(self) -> bool {
    matches!(
      self,
      Self::QueryKw
        | Self::MutationKw
        | Self::SubscriptionKw
        | Self::FragmentKw
        | Self::OnKw
        | Self::NullKw
        | Self::TrueKw
        | Self::FalseKw
        | Self::TypeKw
        | Self::InterfaceKw
        | Self::UnionKw
        | Self::EnumKw
        | Self::InputKw
        | Self::ScalarKw
        | Self::SchemaKw
        | Self::DirectiveKw
        | Self::ExtendKw
        | Self::ImplementsKw
        | Self::RepeatableKw
    )
  }

  /// Returns `true` if this kind represents a token (leaf node).
  #[inline]
  pub const fn is_token(self) -> bool {
    matches!(
      self,
      Self::Identifier
        | Self::Int
        | Self::Float
        | Self::InlineString
        | Self::BlockString
        // Keywords
        | Self::QueryKw
        | Self::MutationKw
        | Self::SubscriptionKw
        | Self::FragmentKw
        | Self::OnKw
        | Self::NullKw
        | Self::TrueKw
        | Self::FalseKw
        | Self::TypeKw
        | Self::InterfaceKw
        | Self::UnionKw
        | Self::EnumKw
        | Self::InputKw
        | Self::ScalarKw
        | Self::SchemaKw
        | Self::DirectiveKw
        | Self::ExtendKw
        | Self::ImplementsKw
        | Self::RepeatableKw
        // Punctuation
        | Self::Dollar
        | Self::LParen
        | Self::RParen
        | Self::Spread
        | Self::Colon
        | Self::Equal
        | Self::At
        | Self::LBracket
        | Self::RBracket
        | Self::LBrace
        | Self::RBrace
        | Self::Pipe
        | Self::Bang
        | Self::Ampersand
        // Trivia
        | Self::Whitespace
        | Self::Comment
        | Self::Comma
    )
  }
}

/// A language implementation for use in `Rowan`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GraphQLLanguage {}

impl rowan::Language for GraphQLLanguage {
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

/// A syntax node in the GraphQL CST.
///
/// This is a typed wrapper around `rowan::SyntaxNode` specialized for GraphQL.
pub type SyntaxNode = rowan::SyntaxNode<GraphQLLanguage>;

/// A syntax token (leaf node) in the GraphQL CST.
///
/// This is a typed wrapper around `rowan::SyntaxToken` specialized for GraphQL.
pub type SyntaxToken = rowan::SyntaxToken<GraphQLLanguage>;

/// A syntax element (either a node or token) in the GraphQL CST.
///
/// This is a typed wrapper around `rowan::SyntaxElement` specialized for GraphQL.
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

/// Iterator over child nodes of a GraphQL syntax node.
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<GraphQLLanguage>;

/// Iterator over child elements (nodes and tokens) of a GraphQL syntax node.
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<GraphQLLanguage>;
