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
/// declaration-order array — the property `kind_decl_index_is_stable` pins.
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

  /// Every kind, in declaration order. [`from_raw`](Self::from_raw) indexes this by raw
  /// value, so its order must match the enum's — the `kind_decl_index_is_stable` property
  /// in this file's `mod tests` is what pins that.
  pub const ALL: &'static [SyntaxKind] = &[
    // Token images.
    Self::Name,
    Self::Int,
    Self::Float,
    Self::String,
    Self::BlockString,
    Self::Dollar,
    Self::LParen,
    Self::RParen,
    Self::Spread,
    Self::Colon,
    Self::Equal,
    Self::At,
    Self::LBracket,
    Self::RBracket,
    Self::LBrace,
    Self::RBrace,
    Self::Pipe,
    Self::Bang,
    Self::Ampersand,
    // Trivia.
    Self::Space,
    Self::Tab,
    Self::Newline,
    Self::Comma,
    Self::Comment,
    Self::Bom,
    // Value nodes.
    Self::Variable,
    Self::IntValue,
    Self::FloatValue,
    Self::StringValue,
    Self::BooleanValue,
    Self::NullValue,
    Self::EnumValue,
    Self::ListValue,
    Self::ObjectValue,
    Self::ObjectField,
    Self::DefaultValue,
    // Type / argument / directive nodes.
    Self::NamedType,
    Self::ListType,
    Self::NonNullType,
    Self::Argument,
    Self::Arguments,
    Self::Directive,
    Self::Directives,
    // Selection / executable nodes.
    Self::Alias,
    Self::Field,
    Self::SelectionSet,
    Self::FragmentSpread,
    Self::InlineFragment,
    Self::VariableDefinition,
    Self::VariablesDefinition,
    Self::OperationDefinition,
    Self::FragmentDefinition,
    Self::ExecutableDocument,
    // SDL definition nodes.
    Self::Description,
    Self::InputValueDefinition,
    Self::ArgumentsDefinition,
    Self::FieldDefinition,
    Self::FieldsDefinition,
    Self::InputFieldsDefinition,
    Self::ImplementsInterfaces,
    Self::UnionMemberTypes,
    Self::DirectiveLocations,
    Self::EnumValueDefinition,
    Self::EnumValuesDefinition,
    Self::OperationType,
    Self::RootOperationTypeDefinition,
    Self::RootOperationTypeDefinitions,
    Self::ScalarTypeDefinition,
    Self::ObjectTypeDefinition,
    Self::InterfaceTypeDefinition,
    Self::UnionTypeDefinition,
    Self::EnumTypeDefinition,
    Self::InputObjectTypeDefinition,
    Self::DirectiveDefinition,
    Self::SchemaDefinition,
    // SDL extension / document nodes.
    Self::ScalarTypeExtension,
    Self::ObjectTypeExtension,
    Self::InterfaceTypeExtension,
    Self::UnionTypeExtension,
    Self::EnumTypeExtension,
    Self::InputObjectTypeExtension,
    Self::SchemaExtension,
    Self::Document,
    Self::TypeSystemDocument,
    // Bookkeeping.
    Self::Error,
    Self::Gap,
    Self::Root,
  ];

  /// The fallible inverse of [`raw`](Self::raw).
  ///
  /// `rowan::Language::kind_from_raw` has no fallible form — it can only panic at query
  /// time — which is why tokora carries the kind validator as profile data. This is the door
  /// that validator uses.
  #[inline]
  pub fn from_raw(raw: u16) -> Option<SyntaxKind> {
    Self::ALL.get(raw as usize).copied()
  }
}

/// This dialect's side of the shared kind-space contract.
///
/// Every member forwards to the inherent item of the same name; the trait exists so the shared
/// runner, coverage counter and typed layer can be written once over `K: KindSpace` rather than
/// once per dialect. Gated on `rowan` because that is the only feature under which any of those
/// three is compiled.
#[cfg(feature = "rowan")]
impl crate::lossless::KindSpace for SyntaxKind {
  const NAME: &'static str = "graphql";
  const ERROR: Self = Self::Error;
  const GAP: Self = Self::Gap;
  const ROOT: Self = Self::Root;
  const ALL: &'static [Self] = Self::ALL;

  #[inline]
  fn raw(self) -> u16 {
    Self::raw(self)
  }

  #[inline]
  fn from_raw(raw: u16) -> Option<Self> {
    Self::from_raw(raw)
  }
}

/// The `rowan::Language` brand for the GraphQL lossless CST.
///
/// Separate from [`GraphQL`](crate::graphql::GraphQL), which is tokora's *grammar brand*
/// (`Dialect::Lang`, a marker keeping two grammars' vocabularies apart). A `rowan::Language`
/// is a kind authority; the two roles are deliberately not the same type.
#[cfg(feature = "rowan")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GraphQLLang;

#[cfg(feature = "rowan")]
impl rowan::Language for GraphQLLang {
  type Kind = SyntaxKind;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    // Infallible by contract: every raw value reaching a rowan query came from a kind this
    // crate emitted, and the sink's validator refuses out-of-space kinds at the emit door.
    //
    // The lookup is a safe, total conversion rather than a bounds-checked `transmute` into
    // the discriminant space: an upper-bound assert cannot catch a hole in the middle of
    // that space, and `ALL` needs no sentinel variant kept in sync by hand.
    SyntaxKind::from_raw(raw.0).unwrap_or_else(|| {
      panic!(
        "raw kind {} is outside the GraphQL syntax-kind space",
        raw.0
      )
    })
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(kind.raw())
  }
}

#[cfg(test)]
mod tests {
  #[cfg(feature = "rowan")]
  #[test]
  fn the_space_satisfies_the_shared_contract() {
    // The three properties this module used to assert by hand — declaration index, tombstone,
    // bookkeeping triple last — now live in one helper so the GraphQLx space inherits them by
    // declaring the impl instead of by copying three tests. The named-constant check and the
    // liveness check are in `tests/lossless_substrate.rs`, which can see the trait's constants.
    crate::lossless::test_support::assert_kind_space_is_well_formed::<super::SyntaxKind>();
  }
}
