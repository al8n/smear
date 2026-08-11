//! The GraphQLx concrete-syntax-tree kind space.
//!
//! # Where every kind in here came from
//!
//! This space is **derived from GraphQLx's own sources**, not adapted from another dialect's.
//! Three sources, and nothing else, produced it:
//!
//! 1. **The images** are [`crate::lexer::graphqlx::lossless::LosslessTokenKind`]'s thirty-four
//!    variants, in that enum's declaration order. The block is the *lexer's*, not the grammar's,
//!    because recovery commits every skipped token into an [`Error`](SyntaxKind::Error) node — an
//!    image no production expects (`+`, `-`, `*` outside an import) still has to be nameable when
//!    it lands in the tree.
//! 2. **The nodes** are GraphQLx's productions in `graphqlx/syntactic/**` and the AST carriers they
//!    build in `graphqlx/ast/**`. `tests/lossless_x_kinds.rs` re-extracts both sets from those
//!    directories and compares them against [`SyntaxKind::ALL`], so a kind that names nothing in
//!    GraphQLx's grammar, and a GraphQLx production this space cannot represent, are both test
//!    failures rather than review findings.
//! 3. **The bookkeeping triple** is the substrate's `KindSpace` contract's, and its position — the
//!    last three, in the order `Error`, `Gap`, `Root` — is that trait's invariant rather than a
//!    choice made here.
//!
//! # The two places the tree's vocabulary is not the lexer's, and why
//!
//! - **`Identifier` becomes [`Name`](SyntaxKind::Name).** GraphQLx's grammar calls this token a
//!   name — `graphqlx::syntactic::name` builds `graphqlx::ast::Name` — and every contextual
//!   keyword (`query`, `on`, `import`, `where`, `set`, `map`, `true`, …, forty-four of them) is
//!   *also* this image, told apart on its spelling and never on its kind.
//! - **The three line terminators fold onto one [`Newline`](SyntaxKind::Newline).** `\n`, `\r` and
//!   `\r\n` are three spellings of one lexical concept; the lexer splits them because it must match
//!   three byte sequences, and the tree has no such obligation because it keeps the token's text
//!   verbatim. This is the space's **only** many-to-one image mapping, and
//!   `the_image_block_matches_the_graphqlx_lexer` states it rather than absorbing it. `Space` and
//!   `Tab` are *not* folded: they are two different characters a formatter branches on, not two
//!   spellings of one.
//!
//! # What GraphQLx has that a GraphQL-shaped space would not predict
//!
//! Seven images (`<`, `>`, `::`, `=>`, `*`, `+`, `-`) and, among the nodes, the whole import and
//! generic families, [`SetValue`](SyntaxKind::SetValue)/[`MapValue`](SyntaxKind::MapValue)/
//! [`MapEntry`](SyntaxKind::MapEntry), [`SetType`](SyntaxKind::SetType)/
//! [`MapType`](SyntaxKind::MapType), [`Path`](SyntaxKind::Path),
//! [`TypePath`](SyntaxKind::TypePath) and [`TypeGenerics`](SyntaxKind::TypeGenerics).
//!
//! And what it *lacks*: GraphQLx has no non-null node. `graphqlx::syntactic::ty` folds a trailing
//! `!` into the type node it follows — `DefinitionTypePath`, `ListType`, `SetType` and `MapType`
//! each carry their own `required` flag (`smear/src/parser/ty.rs`) — so the `!` is a
//! [`Bang`](SyntaxKind::Bang) token child of the type it modifies rather than a wrapper around it.
//! It likewise has no bare named-type node: every type head is a `::`-separated
//! [`Path`](SyntaxKind::Path) with optional generic arguments, which is
//! [`DefinitionTypePath`](SyntaxKind::DefinitionTypePath).
//!
//! # Why some GraphQLx productions have no kind
//!
//! Three rules, applied uniformly, and `tests/lossless_x_kinds.rs` pins every application with its
//! reason:
//!
//! - **A choice is not a region.** `Type`, `Selection`, `InputValue`, `TypeDefinition`,
//!   `ImportClause` and the rest dispatch to alternatives that are each already a node; the tree
//!   records the alternative that won, so the choice itself would wrap exactly one child.
//! - **One token is not a region.** `Name`, `OperationType`, `Location`, `ExtensionTypeParam` and
//!   `Description` each cover exactly one token whose parent already says what it is.
//! - **Constness is a parse-time flavour, not a shape.** `[1, 2]` is the same syntax in a constant
//!   and a non-constant position, so `ConstList` and `List` are one
//!   [`ListValue`](SyntaxKind::ListValue).

#[cfg(feature = "rowan")]
use crate::parser::lossless::KindSpace;

/// A GraphQLx concrete-syntax-tree kind: one token image, one node, or one bookkeeping tile.
///
/// # Layout
///
/// The discriminants are the default `0..113` and [`ALL`](Self::ALL) is in declaration order, so
/// [`raw`](Self::raw) is an index and [`from_raw`](Self::from_raw) is a bounds check. Four blocks,
/// in this order:
///
/// | range | block |
/// |---|---|
/// | `0..26` | the twenty-six non-trivia images, in `LosslessTokenKind`'s declaration order |
/// | `26..32` | the six trivia images, in the same order, with the line terminators folded |
/// | `32..110` | the seventy-eight node kinds |
/// | `110..113` | [`Error`](Self::Error), [`Gap`](Self::Gap), [`Root`](Self::Root) |
///
/// **Nothing may be appended after [`Root`](Self::Root)** — the shared runner is parameterised over
/// the last three positions, and the seven images GraphQL lacks therefore live *inside* the image
/// block rather than on the tail.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
  // ---- token images, non-trivia: `LosslessTokenKind` in declaration order ----
  /// `*` — an import wildcard.
  Asterisk,
  /// `@` — a directive marker.
  At,
  /// `$` — a variable marker.
  Dollar,
  /// `=>` — a map entry's and a map type's separator.
  FatArrow,
  /// `<` — a generic list's and a set/map type's opener.
  LAngle,
  /// `>` — a generic list's and a set/map type's closer.
  RAngle,
  /// `(` — an argument list's opener.
  LParen,
  /// `)` — an argument list's closer.
  RParen,
  /// `...` — a fragment spread's marker.
  Spread,
  /// `:` — a field, argument, alias and `where`-predicate separator.
  Colon,
  /// `=` — a default-value and generic-default assignment.
  Equal,
  /// `[` — a list value's and a list type's opener.
  LBracket,
  /// `]` — a list value's and a list type's closer.
  RBracket,
  /// `{` — a selection set's, object value's, braced definition's and import list's opener.
  LBrace,
  /// `}` — the matching closer.
  RBrace,
  /// `|` — a union member separator.
  Pipe,
  /// `!` — the non-null modifier, a token child of the type it modifies.
  Bang,
  /// `&` — an `implements` and a `where`-bound separator.
  Ampersand,
  /// `+` — no GraphQLx production consumes it; it reaches the tree through recovery.
  Plus,
  /// `-` — a bare hyphen, distinct from a negative numeric literal; recovery-only, as `+` is.
  Minus,
  /// `::` — a path separator.
  PathSeparator,
  /// An identifier: every GraphQLx name, and every one of the forty-four contextual keywords.
  ///
  /// The lexer calls this image `Identifier`; the tree calls it what GraphQLx's grammar calls it.
  Name,
  /// A float literal, in any of the lexer's representations.
  Float,
  /// An integer literal, in any of the lexer's representations.
  Int,
  /// A `"…"` string literal.
  InlineString,
  /// A `"""…"""` string literal.
  BlockString,

  // ---- token images, trivia: `LosslessToken::is_trivia`, same declaration order ----
  /// A byte-order mark.
  Bom,
  /// `,` — insignificant in GraphQLx exactly as in GraphQL.
  Comma,
  /// A run of one space.
  Space,
  /// A horizontal tab.
  Tab,
  /// A line terminator: `\n`, `\r` or `\r\n`.
  ///
  /// The one many-to-one image mapping in this space. Which of the three it was is in the token's
  /// text, which the tree keeps verbatim.
  Newline,
  /// A `# …` comment.
  Comment,

  // ---- values ----
  /// An integer value.
  IntValue,
  /// A float value.
  FloatValue,
  /// A string value, inline or block; the token child says which.
  StringValue,
  /// `true` or `false`.
  ///
  /// A node rather than a bare token because `true`, `null` and an enum value are all the same
  /// image, and `InputValue`'s alternatives have to be told apart in the tree.
  BooleanValue,
  /// `null`, for the same reason [`BooleanValue`](Self::BooleanValue) is a node.
  NullValue,
  /// An enum value.
  ///
  /// GraphQLx widens GraphQL's single name to a whole [`Path`](Self::Path).
  EnumValue,
  /// `$name`.
  VariableValue,
  /// `[ Value* ]`.
  ListValue,
  /// `set { Value* }` — GraphQLx only.
  SetValue,
  /// `map { MapEntry* }` — GraphQLx only.
  MapValue,
  /// `Value => Value` inside a [`MapValue`](Self::MapValue) — GraphQLx only.
  MapEntry,
  /// `{ ObjectField* }`.
  ObjectValue,
  /// `Name : Value` inside an [`ObjectValue`](Self::ObjectValue).
  ObjectField,
  /// `= Value`.
  DefaultValue,

  // ---- type references, paths, arguments, directives ----
  /// `Name (:: Name)*`, optionally fully qualified by a leading `::` — GraphQLx only.
  Path,
  /// `< Type+ >`, the arguments applied to a path — GraphQLx only.
  TypeGenerics,
  /// A type reference's path head: `Path TypeGenerics? '!'?`.
  DefinitionTypePath,
  /// `[ Type ] '!'?`.
  ListType,
  /// `< Type > '!'?` — GraphQLx only.
  SetType,
  /// `< Type => Type > '!'?` — GraphQLx only.
  MapType,
  /// `Path TypeGenerics?` in a non-type position: a directive's name, an interface, a union member,
  /// a type condition, a fragment spread's target, a `where` bound — GraphQLx only.
  TypePath,
  /// `Name : Value`.
  Argument,
  /// `( Argument* )`.
  Arguments,
  /// `@ TypePath Arguments?`.
  ///
  /// GraphQLx widens GraphQL's single name after `@` to a [`TypePath`](Self::TypePath).
  Directive,
  /// A run of directives.
  Directives,

  // ---- selections and executable definitions ----
  /// `Name :` preceding a field's own name.
  Alias,
  /// `Alias? Name Arguments? Directives? SelectionSet?`.
  Field,
  /// `... TypePath Directives?`.
  ///
  /// GraphQLx widens GraphQL's fragment name to a [`TypePath`](Self::TypePath).
  FragmentSpread,
  /// `... TypeCondition? Directives? SelectionSet`.
  InlineFragment,
  /// `on TypePath`.
  TypeCondition,
  /// `{ Selection+ }`.
  SelectionSet,
  /// `Description? $name : Type DefaultValue? Directives?`.
  VariableDefinition,
  /// `( VariableDefinition+ )`.
  VariablesDefinition,
  /// A named operation or a shorthand selection set.
  ///
  /// One node for both forms: the named one carries the operation keyword and the shorthand one
  /// does not, which is the whole difference and is visible in the tree.
  OperationDefinition,
  /// `fragment ExecutableDefinitionTypeGenerics? ExecutableDefinitionName TypeCondition
  /// Directives? WhereClause? SelectionSet`.
  FragmentDefinition,

  // ---- SDL definitions ----
  /// `schema Directives? RootOperationTypesDefinition`.
  SchemaDefinition,
  /// `{ RootOperationTypeDefinition+ }`.
  RootOperationTypesDefinition,
  /// `OperationType : TypePath`.
  RootOperationTypeDefinition,
  /// `scalar DefinitionName Directives?`.
  ScalarTypeDefinition,
  /// `type DefinitionName ImplementInterfaces? Directives? WhereClause? FieldsDefinition?`.
  ObjectTypeDefinition,
  /// The `interface` form of the same shape.
  InterfaceTypeDefinition,
  /// `union DefinitionName Directives? UnionMemberTypes? WhereClause?`.
  ///
  /// The clause comes **after** the members and requires them —
  /// `graphqlx/syntactic/definition/union.rs:87-95` parses `try_union_members` and then
  /// `try_where_clause`, which is the reverse of every other `where` site.
  UnionTypeDefinition,
  /// `enum DefinitionName Directives? EnumValuesDefinition?`.
  EnumTypeDefinition,
  /// `input DefinitionName Directives? WhereClause? InputFieldsDefinition?`.
  InputObjectTypeDefinition,
  /// `directive @ DefinitionName ArgumentsDefinition? repeatable? on DirectiveLocations
  /// WhereClause?`.
  DirectiveDefinition,
  /// `implements &? TypePath (& TypePath)*`.
  ImplementInterfaces,
  /// `= |? TypePath (| TypePath)*`.
  UnionMemberTypes,
  /// `{ FieldDefinition+ }`.
  FieldsDefinition,
  /// `Description? Name ArgumentsDefinition? : Type Directives?`.
  FieldDefinition,
  /// `( InputValueDefinition+ )`.
  ArgumentsDefinition,
  /// `Description? Name : Type DefaultValue? Directives?`.
  InputValueDefinition,
  /// `{ InputValueDefinition+ }`.
  InputFieldsDefinition,
  /// `{ EnumValueDefinition+ }`.
  EnumValuesDefinition,
  /// `Description? Name Directives?`.
  ///
  /// A bare `Name` token and **not** an [`EnumValue`](Self::EnumValue), which is the one place the
  /// two spellings of an enum value come apart. In value position GraphQLx widens the name to a
  /// whole [`Path`](Self::Path); in *declaring* position it does not — `enum E { ns::RED }` is a
  /// syntax error in both suites, so a node that could hold a path would be claiming a grammar
  /// GraphQLx does not have. The declaring name is therefore one token whose parent already says
  /// what it is, which is the census's *one token is not a region* rule.
  EnumValueDefinition,
  /// `|? Name (| Name)*` after `on` in a directive definition.
  DirectiveLocations,

  // ---- SDL extensions ----
  /// `extend schema Directives? RootOperationTypesDefinition?`.
  SchemaExtension,
  /// `extend scalar ExtensionName Directives`.
  ScalarTypeExtension,
  /// `extend type ExtensionName ImplementInterfaces? Directives? WhereClause? FieldsDefinition?`.
  ObjectTypeExtension,
  /// The `extend interface` form of the same shape.
  InterfaceTypeExtension,
  /// `extend union ExtensionName Directives? UnionMemberTypes? WhereClause?`, for the reason
  /// [`UnionTypeDefinition`](Self::UnionTypeDefinition) records.
  UnionTypeExtension,
  /// `extend enum ExtensionName Directives? EnumValuesDefinition?`.
  EnumTypeExtension,
  /// `extend input ExtensionName Directives? WhereClause? InputFieldsDefinition?`.
  InputObjectTypeExtension,

  // ---- imports: GraphQLx only, no GraphQL counterpart at all ----
  /// `import ImportClause from "file"`.
  ImportDefinition,
  /// `{ ImportMember+ }`.
  ImportList,
  /// `Name (as Path)?`.
  NamedSpecifier,
  /// `* (as Path)?`.
  WildcardSpecifier,

  // ---- generics: GraphQLx only, no GraphQL counterpart at all ----
  /// `Name (= Type)?` inside a [`DefinitionTypeGenerics`](Self::DefinitionTypeGenerics).
  DefinitionTypeParam,
  /// `< DefinitionTypeParam+ >` declared by a definition.
  DefinitionTypeGenerics,
  /// `< Name+ >` applied by a type-system extension.
  ExtensionTypeGenerics,
  /// `< Name+ >` declared by an executable definition.
  ExecutableDefinitionTypeGenerics,
  /// `Name DefinitionTypeGenerics?` — a definition's name.
  DefinitionName,
  /// `Path ExtensionTypeGenerics?` — a type-system extension's target.
  ExtensionName,
  /// `Name ExecutableDefinitionTypeGenerics?` — an executable definition's name.
  ExecutableDefinitionName,
  /// `TypePath : TypePath (& TypePath)*`.
  WherePredicate,
  /// `where WherePredicate (, WherePredicate)*`.
  WhereClause,

  // ---- documents ----
  /// A complete document: imports, executable definitions, type-system definitions and extensions.
  Document,
  /// An executable document: imports and executable definitions only.
  ExecutableDocument,
  /// A type-system document: imports, type-system definitions and extensions only.
  TypeSystemDocument,

  // ---- bookkeeping: last three, in this order, and nothing after ----
  /// A recovery hole: the region the sink materialized from a skipped token run.
  Error,
  /// A gap tile: source bytes no committed token covered.
  Gap,
  /// The synthetic root `finish` names.
  Root,
}

impl SyntaxKind {
  /// Every kind, in declaration order, so the index is the raw value.
  pub const ALL: &'static [Self] = &[
    // images, non-trivia (0..26)
    Self::Asterisk,
    Self::At,
    Self::Dollar,
    Self::FatArrow,
    Self::LAngle,
    Self::RAngle,
    Self::LParen,
    Self::RParen,
    Self::Spread,
    Self::Colon,
    Self::Equal,
    Self::LBracket,
    Self::RBracket,
    Self::LBrace,
    Self::RBrace,
    Self::Pipe,
    Self::Bang,
    Self::Ampersand,
    Self::Plus,
    Self::Minus,
    Self::PathSeparator,
    Self::Name,
    Self::Float,
    Self::Int,
    Self::InlineString,
    Self::BlockString,
    // images, trivia (26..32)
    Self::Bom,
    Self::Comma,
    Self::Space,
    Self::Tab,
    Self::Newline,
    Self::Comment,
    // values (32..46)
    Self::IntValue,
    Self::FloatValue,
    Self::StringValue,
    Self::BooleanValue,
    Self::NullValue,
    Self::EnumValue,
    Self::VariableValue,
    Self::ListValue,
    Self::SetValue,
    Self::MapValue,
    Self::MapEntry,
    Self::ObjectValue,
    Self::ObjectField,
    Self::DefaultValue,
    // types, paths, arguments, directives (46..57)
    Self::Path,
    Self::TypeGenerics,
    Self::DefinitionTypePath,
    Self::ListType,
    Self::SetType,
    Self::MapType,
    Self::TypePath,
    Self::Argument,
    Self::Arguments,
    Self::Directive,
    Self::Directives,
    // selections and executable definitions (57..67)
    Self::Alias,
    Self::Field,
    Self::FragmentSpread,
    Self::InlineFragment,
    Self::TypeCondition,
    Self::SelectionSet,
    Self::VariableDefinition,
    Self::VariablesDefinition,
    Self::OperationDefinition,
    Self::FragmentDefinition,
    // SDL definitions (67..87)
    Self::SchemaDefinition,
    Self::RootOperationTypesDefinition,
    Self::RootOperationTypeDefinition,
    Self::ScalarTypeDefinition,
    Self::ObjectTypeDefinition,
    Self::InterfaceTypeDefinition,
    Self::UnionTypeDefinition,
    Self::EnumTypeDefinition,
    Self::InputObjectTypeDefinition,
    Self::DirectiveDefinition,
    Self::ImplementInterfaces,
    Self::UnionMemberTypes,
    Self::FieldsDefinition,
    Self::FieldDefinition,
    Self::ArgumentsDefinition,
    Self::InputValueDefinition,
    Self::InputFieldsDefinition,
    Self::EnumValuesDefinition,
    Self::EnumValueDefinition,
    Self::DirectiveLocations,
    // SDL extensions (87..94)
    Self::SchemaExtension,
    Self::ScalarTypeExtension,
    Self::ObjectTypeExtension,
    Self::InterfaceTypeExtension,
    Self::UnionTypeExtension,
    Self::EnumTypeExtension,
    Self::InputObjectTypeExtension,
    // imports (94..98)
    Self::ImportDefinition,
    Self::ImportList,
    Self::NamedSpecifier,
    Self::WildcardSpecifier,
    // generics (98..107)
    Self::DefinitionTypeParam,
    Self::DefinitionTypeGenerics,
    Self::ExtensionTypeGenerics,
    Self::ExecutableDefinitionTypeGenerics,
    Self::DefinitionName,
    Self::ExtensionName,
    Self::ExecutableDefinitionName,
    Self::WherePredicate,
    Self::WhereClause,
    // documents (107..110)
    Self::Document,
    Self::ExecutableDocument,
    Self::TypeSystemDocument,
    // bookkeeping (110..113)
    Self::Error,
    Self::Gap,
    Self::Root,
  ];

  /// How many of [`ALL`](Self::ALL)'s leading entries are token images.
  ///
  /// Twenty-six non-trivia plus six trivia. The images are a **prefix** because the bookkeeping
  /// triple owns the tail, so the seven images GraphQL has no counterpart for had nowhere else to
  /// go — and that is why a space derived from GraphQLx's lexer cannot agree with GraphQL's
  /// through the end of either image block.
  pub const IMAGE_BLOCK: usize = 32;

  /// The raw `u16` the event channel and the sink speak: the declaration index.
  #[inline]
  pub const fn raw(self) -> u16 {
    self as u16
  }

  /// The checked inverse of [`raw`](Self::raw).
  ///
  /// A bounds check rather than a match, which is what [`ALL`](Self::ALL) being in declaration
  /// order buys.
  #[inline]
  pub fn from_raw(raw: u16) -> Option<Self> {
    Self::ALL.get(raw as usize).copied()
  }
}

#[cfg(feature = "rowan")]
impl KindSpace for SyntaxKind {
  const NAME: &'static str = "graphqlx";
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

/// The [`rowan::Language`] a GraphQLx lossless tree is typed over.
///
/// **A second `Language`, and therefore a `SyntaxNode` incompatible with GraphQL's.** That is
/// intended: a GraphQLx tree is not a GraphQL tree, the two spaces do not even agree on what raw
/// `0` means, and a shared `Language` would let one dialect's node be handed to the other's typed
/// wrapper with nothing but a silent wrong answer to show for it.
#[cfg(feature = "rowan")]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GraphQLxLang {}

#[cfg(feature = "rowan")]
impl rowan::Language for GraphQLxLang {
  type Kind = SyntaxKind;

  #[inline]
  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    // `rowan::Language` has no fallible form. The sink's kind validator is what rejects a raw
    // value this space does not name, before a green node is ever built with it; reaching here
    // with one means that validator was bypassed.
    SyntaxKind::from_raw(raw.0)
      .unwrap_or_else(|| panic!("raw kind {} is not a graphqlx syntax kind", raw.0))
  }

  #[inline]
  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(kind.raw())
  }
}
