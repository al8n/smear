//! The GraphQL dialect's unified [`SyntaxKind`] space.
//!
//! One `#[repr(u16)]` enum holds every kind the lossless CST can carry: the token
//! *images* committed tokens enter the tree as, the node kinds the assemblies open
//! with [`node`](tokora::parser::node), and the `Root`/`Error`/`Gap` bookkeeping
//! kinds the sink needs. One enum, one space — a token kind and a node kind can
//! never collide (tokora guide `ch16_lossless_cst.md`).
//!
//! A kind is a plain `u16` ([`SyntaxKind::raw`]); it is *data*, not a lexer type,
//! so naming one here breaks no Lego rule. Assemblies open nodes with their own
//! `K::X.raw()`; atoms never open nodes. The tail (lossless) waves add
//! `rowan::Language` and the token mapper over this same space, so no shipped
//! production is re-touched — the space is declared complete now.
//!
//! The tombstone value `u16::MAX` is reserved crate-wide by tokora; nothing here
//! maps to it (default discriminants keep every kind far below it).

/// The unified GraphQL syntax-kind space: token images, node kinds, and the
/// `Root`/`Error`/`Gap` bookkeeping kinds, in one `#[repr(u16)]` enum.
///
/// Discriminants are the declaration index (default `#[repr(u16)]` numbering), so
/// [`raw`](Self::raw) is a plain cast and the space round-trips through a
/// declaration-order array — the property [`kind_decl_index_is_stable`] pins.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
  // ---- Token images (committed tokens enter the tree through the mapper) ----
  /// A `Name` token (identifier).
  Name,
  /// An integer literal token.
  Int,
  /// A float literal token.
  Float,
  /// An inline (single-line) string literal token.
  String,
  /// A block (triple-quoted) string literal token.
  BlockString,
  /// A `$` token.
  Dollar,
  /// A `(` token.
  LParen,
  /// A `)` token.
  RParen,
  /// A `...` token.
  Spread,
  /// A `:` token.
  Colon,
  /// A `=` token.
  Equal,
  /// An `@` token.
  At,
  /// A `[` token.
  LBracket,
  /// A `]` token.
  RBracket,
  /// A `{` token.
  LBrace,
  /// A `}` token.
  RBrace,
  /// A `|` token.
  Pipe,
  /// A `!` token.
  Bang,
  /// An `&` token.
  Ampersand,

  // ---- Trivia token images (surfaced only by the lossless lexer) ----
  /// A run of space characters.
  Space,
  /// A run of tab characters.
  Tab,
  /// A line terminator.
  Newline,
  /// A `,` (insignificant comma).
  Comma,
  /// A `#` comment.
  Comment,
  /// A byte-order mark.
  Bom,

  // ---- Value node kinds (Wave 1) ----
  /// A `Variable` value (`$name`).
  Variable,
  /// An `IntValue`.
  IntValue,
  /// A `FloatValue`.
  FloatValue,
  /// A `StringValue`.
  StringValue,
  /// A `BooleanValue`.
  BooleanValue,
  /// A `NullValue`.
  NullValue,
  /// An `EnumValue`.
  EnumValue,
  /// A `ListValue`.
  ListValue,
  /// An `ObjectValue`.
  ObjectValue,
  /// An `ObjectField` inside an object value.
  ObjectField,
  /// A `DefaultValue` (`= const-value`).
  DefaultValue,

  // ---- Type / argument / directive node kinds (Wave 2) ----
  /// A `NamedType`.
  NamedType,
  /// A `ListType` (`[T]`).
  ListType,
  /// A `NonNullType` (`T!`).
  NonNullType,
  /// An `Argument`.
  Argument,
  /// An `Arguments` list (`( … )`).
  Arguments,
  /// A `Directive` (`@name`).
  Directive,
  /// A `Directives` list.
  Directives,

  // ---- Selection / executable node kinds (Wave 3) ----
  /// A field `Alias` (`name :`).
  Alias,
  /// A `Field`.
  Field,
  /// A `SelectionSet` (`{ … }`).
  SelectionSet,
  /// A `FragmentSpread` (`... name`).
  FragmentSpread,
  /// An `InlineFragment` (`... on T { … }`).
  InlineFragment,
  /// A `VariableDefinition`.
  VariableDefinition,
  /// A `VariablesDefinition` list (`( … )`).
  VariablesDefinition,
  /// An `OperationDefinition`.
  OperationDefinition,
  /// A `FragmentDefinition`.
  FragmentDefinition,
  /// An `ExecutableDocument`.
  ExecutableDocument,

  // ---- SDL definition node kinds (Wave 4) ----
  /// A `Description` (leading string literal).
  Description,
  /// An `InputValueDefinition`.
  InputValueDefinition,
  /// An `ArgumentsDefinition` (`( … )`).
  ArgumentsDefinition,
  /// A `FieldDefinition`.
  FieldDefinition,
  /// A `FieldsDefinition` (`{ … }`).
  FieldsDefinition,
  /// An `InputFieldsDefinition` (`{ … }`).
  InputFieldsDefinition,
  /// An `ImplementsInterfaces` clause.
  ImplementsInterfaces,
  /// A `UnionMemberTypes` clause.
  UnionMemberTypes,
  /// A `DirectiveLocations` clause.
  DirectiveLocations,
  /// An `EnumValueDefinition`.
  EnumValueDefinition,
  /// An `EnumValuesDefinition` (`{ … }`).
  EnumValuesDefinition,
  /// An `OperationType` (`query` / `mutation` / `subscription`).
  OperationType,
  /// A `RootOperationTypeDefinition`.
  RootOperationTypeDefinition,
  /// A `RootOperationTypeDefinitions` block (`{ … }`).
  RootOperationTypeDefinitions,
  /// A `ScalarTypeDefinition`.
  ScalarTypeDefinition,
  /// An `ObjectTypeDefinition`.
  ObjectTypeDefinition,
  /// An `InterfaceTypeDefinition`.
  InterfaceTypeDefinition,
  /// A `UnionTypeDefinition`.
  UnionTypeDefinition,
  /// An `EnumTypeDefinition`.
  EnumTypeDefinition,
  /// An `InputObjectTypeDefinition`.
  InputObjectTypeDefinition,
  /// A `DirectiveDefinition`.
  DirectiveDefinition,
  /// A `SchemaDefinition`.
  SchemaDefinition,

  // ---- SDL extension / document node kinds (Wave 5) ----
  /// A `ScalarTypeExtension`.
  ScalarTypeExtension,
  /// An `ObjectTypeExtension`.
  ObjectTypeExtension,
  /// An `InterfaceTypeExtension`.
  InterfaceTypeExtension,
  /// A `UnionTypeExtension`.
  UnionTypeExtension,
  /// An `EnumTypeExtension`.
  EnumTypeExtension,
  /// An `InputObjectTypeExtension`.
  InputObjectTypeExtension,
  /// A `SchemaExtension`.
  SchemaExtension,
  /// A `Document` (mixed executable + type-system).
  Document,
  /// A `TypeSystemDocument`.
  TypeSystemDocument,

  // ---- Bookkeeping (recovery holes, gap tiles, synthetic root) ----
  /// A recovery hole: a malformed region the sink materializes as an error node.
  Error,
  /// A gap tile: a hole record the sink materializes to keep the tree textually
  /// complete.
  Gap,
  /// The synthetic document root.
  Root,
}

impl SyntaxKind {
  /// The raw `u16` the event channel and the sink speak.
  ///
  /// A plain cast: `#[repr(u16)]` with default discriminants makes the raw value
  /// the declaration index.
  #[inline]
  pub const fn raw(self) -> u16 {
    self as u16
  }
}

#[cfg(test)]
mod tests {
  use super::SyntaxKind as K;

  /// Every kind, in declaration order. `kind_from_raw` (added in the lossless
  /// wave) indexes this array by raw value, so its order must match the enum's.
  const KINDS: &[K] = &[
    // Token images.
    K::Name,
    K::Int,
    K::Float,
    K::String,
    K::BlockString,
    K::Dollar,
    K::LParen,
    K::RParen,
    K::Spread,
    K::Colon,
    K::Equal,
    K::At,
    K::LBracket,
    K::RBracket,
    K::LBrace,
    K::RBrace,
    K::Pipe,
    K::Bang,
    K::Ampersand,
    // Trivia.
    K::Space,
    K::Tab,
    K::Newline,
    K::Comma,
    K::Comment,
    K::Bom,
    // Value nodes.
    K::Variable,
    K::IntValue,
    K::FloatValue,
    K::StringValue,
    K::BooleanValue,
    K::NullValue,
    K::EnumValue,
    K::ListValue,
    K::ObjectValue,
    K::ObjectField,
    K::DefaultValue,
    // Type / argument / directive nodes.
    K::NamedType,
    K::ListType,
    K::NonNullType,
    K::Argument,
    K::Arguments,
    K::Directive,
    K::Directives,
    // Selection / executable nodes.
    K::Alias,
    K::Field,
    K::SelectionSet,
    K::FragmentSpread,
    K::InlineFragment,
    K::VariableDefinition,
    K::VariablesDefinition,
    K::OperationDefinition,
    K::FragmentDefinition,
    K::ExecutableDocument,
    // SDL definition nodes.
    K::Description,
    K::InputValueDefinition,
    K::ArgumentsDefinition,
    K::FieldDefinition,
    K::FieldsDefinition,
    K::InputFieldsDefinition,
    K::ImplementsInterfaces,
    K::UnionMemberTypes,
    K::DirectiveLocations,
    K::EnumValueDefinition,
    K::EnumValuesDefinition,
    K::OperationType,
    K::RootOperationTypeDefinition,
    K::RootOperationTypeDefinitions,
    K::ScalarTypeDefinition,
    K::ObjectTypeDefinition,
    K::InterfaceTypeDefinition,
    K::UnionTypeDefinition,
    K::EnumTypeDefinition,
    K::InputObjectTypeDefinition,
    K::DirectiveDefinition,
    K::SchemaDefinition,
    // SDL extension / document nodes.
    K::ScalarTypeExtension,
    K::ObjectTypeExtension,
    K::InterfaceTypeExtension,
    K::UnionTypeExtension,
    K::EnumTypeExtension,
    K::InputObjectTypeExtension,
    K::SchemaExtension,
    K::Document,
    K::TypeSystemDocument,
    // Bookkeeping.
    K::Error,
    K::Gap,
    K::Root,
  ];

  #[test]
  fn kind_decl_index_is_stable() {
    // The raw value of each kind is exactly its declaration index, so the
    // lossless `kind_from_raw` array (this same order) round-trips every kind.
    for (index, kind) in KINDS.iter().enumerate() {
      assert_eq!(
        kind.raw(),
        index as u16,
        "declaration index drifted at {kind:?}"
      );
    }
  }

  #[test]
  fn tombstone_value_is_unused() {
    // `u16::MAX` is tokora's reserved tombstone; no kind may occupy it.
    for kind in KINDS {
      assert_ne!(kind.raw(), u16::MAX, "{kind:?} collides with the tombstone");
    }
  }

  #[test]
  fn bookkeeping_kinds_are_last_and_distinct() {
    // `Root`/`Error`/`Gap` exist and sit past every content kind, so appending a
    // content kind before them is caught by `kind_decl_index_is_stable`.
    let content = KINDS.len() as u16 - 3;
    assert_eq!(K::Error.raw(), content);
    assert_eq!(K::Gap.raw(), content + 1);
    assert_eq!(K::Root.raw(), content + 2);
  }
}
