use derive_more::Display;
use generic_array::GenericArray;
use logosky::utils::{
  human_display::DisplayHuman,
  syntax::Syntax,
  typenum::{U1, U2, U3, U4, U5, U6},
};
use smear_lexer::graphqlx::GraphQLx;

// ============================================================================
// Component Enums
// ============================================================================

// Reuse GraphQL component enums
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum NamedTypeComponent {
  #[display("type name")]
  Name,
  #[display("type generics")]
  TypeGenerics,
  #[display("non-null modifier '!'")]
  Bang,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum ListTypeComponent {
  #[display("'[' left bracket")]
  LBracket,
  #[display("element type")]
  ElementType,
  #[display("']' right bracket")]
  RBracket,
  #[display("non-null modifier '!'")]
  Bang,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum SetTypeComponent {
  #[display("'<' left angle bracket")]
  LAngle,
  #[display("element type")]
  ElementType,
  #[display("'>' right angle bracket")]
  RAngle,
  #[display("non-null modifier '!'")]
  Bang,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum MapTypeComponent {
  #[display("'<' left angle bracket")]
  LAngle,
  #[display("key type")]
  KeyType,
  #[display("'=>' fat arrow")]
  FatArrow,
  #[display("value type")]
  ValueType,
  #[display("'>' right angle bracket")]
  RAngle,
  #[display("non-null modifier '!'")]
  Bang,
}

// GraphQLx-specific: Type paths
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum PathSegmentComponent {
  #[display("identifier")]
  Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum PathComponent {
  #[display("'::' path separator")]
  PathSeparator,
  #[display("path segments")]
  Segments,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum TypePathComponent {
  #[display("path")]
  Path,
  #[display("type generics")]
  TypeGenerics,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum DefinitionTypePathComponent {
  #[display("path")]
  Path,
  #[display("definition type generics")]
  DefinitionTypeGenerics,
}

// GraphQLx-specific: Generics
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum TypeParameterComponent {
  #[display("parameter name")]
  Name,
  #[display("type constraint")]
  TypeConstraint,
  #[display("default type")]
  DefaultType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum TypeGenericsComponent {
  #[display("'<' left angle bracket")]
  LAngle,
  #[display("type arguments")]
  TypeArguments,
  #[display("'>' right angle bracket")]
  RAngle,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum WhereClauseComponent {
  #[display("'where' keyword")]
  WhereKeyword,
  #[display("where predicates")]
  WherePredicates,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum WherePredicateComponent {
  #[display("type parameter")]
  TypeParameter,
  #[display("':' colon")]
  Colon,
  #[display("type bounds")]
  TypeBounds,
}

// GraphQLx-specific: Import system
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum ImportDefinitionComponent {
  #[display("'import' keyword")]
  ImportKeyword,
  #[display("import clause")]
  ImportClause,
  #[display("'from' keyword")]
  FromKeyword,
  #[display("module path")]
  ModulePath,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum ImportClauseComponent {
  #[display("import list")]
  ImportList,
  #[display("wildcard specifier")]
  WildcardSpecifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum NamedSpecifierComponent {
  #[display("imported name")]
  ImportedName,
  #[display("'as' keyword")]
  AsKeyword,
  #[display("local name")]
  LocalName,
}

// Type Definition Components (same as GraphQL but with generics support)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum ObjectTypeDefinitionComponent {
  #[display("'type' keyword")]
  TypeKeyword,
  #[display("type name")]
  Name,
  #[display("type parameters")]
  TypeParameters,
  #[display("'implements' clause")]
  ImplementsInterfaces,
  #[display("where clause")]
  WhereClause,
  #[display("directives")]
  Directives,
  #[display("fields definition")]
  FieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum InterfaceTypeDefinitionComponent {
  #[display("'interface' keyword")]
  InterfaceKeyword,
  #[display("type name")]
  Name,
  #[display("type parameters")]
  TypeParameters,
  #[display("'implements' clause")]
  ImplementsInterfaces,
  #[display("where clause")]
  WhereClause,
  #[display("directives")]
  Directives,
  #[display("fields definition")]
  FieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum UnionTypeDefinitionComponent {
  #[display("'union' keyword")]
  UnionKeyword,
  #[display("type name")]
  Name,
  #[display("type parameters")]
  TypeParameters,
  #[display("where clause")]
  WhereClause,
  #[display("directives")]
  Directives,
  #[display("union members")]
  UnionMembers,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum EnumTypeDefinitionComponent {
  #[display("'enum' keyword")]
  EnumKeyword,
  #[display("type name")]
  Name,
  #[display("directives")]
  Directives,
  #[display("enum values definition")]
  EnumValuesDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum InputObjectTypeDefinitionComponent {
  #[display("'input' keyword")]
  InputKeyword,
  #[display("type name")]
  Name,
  #[display("type parameters")]
  TypeParameters,
  #[display("where clause")]
  WhereClause,
  #[display("directives")]
  Directives,
  #[display("input fields definition")]
  InputFieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum ScalarTypeDefinitionComponent {
  #[display("'scalar' keyword")]
  ScalarKeyword,
  #[display("type name")]
  Name,
  #[display("type parameters")]
  TypeParameters,
  #[display("where clause")]
  WhereClause,
  #[display("directives")]
  Directives,
}

// Field and operations (same as GraphQL)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum FieldDefinitionComponent {
  #[display("field name")]
  Name,
  #[display("arguments definition")]
  ArgumentsDefinition,
  #[display("':' colon")]
  Colon,
  #[display("field type")]
  Type,
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum InputValueDefinitionComponent {
  #[display("input value name")]
  Name,
  #[display("':' colon")]
  Colon,
  #[display("input value type")]
  Type,
  #[display("default value")]
  DefaultValue,
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum NamedOperationDefinitionComponent {
  #[display("operation type")]
  OperationType,
  #[display("operation name")]
  Name,
  #[display("type parameters")]
  TypeParameters,
  #[display("variables definition")]
  VariablesDefinition,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum FragmentDefinitionComponent {
  #[display("'fragment' keyword")]
  FragmentKeyword,
  #[display("fragment type path")]
  FragmentTypePath,
  #[display("type condition")]
  TypeCondition,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum FragmentSpreadComponent {
  #[display("'...' spread operator")]
  Spread,
  #[display("fragment type path")]
  FragmentTypePath,
  #[display("directives")]
  Directives,
}

// Value components
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum MapEntryComponent {
  #[display("key")]
  Key,
  #[display("'=>' fat arrow")]
  FatArrow,
  #[display("value")]
  Value,
}

// Reuse other GraphQL components
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum DirectiveComponent {
  #[display("'@' at symbol")]
  At,
  #[display("directive name")]
  Name,
  #[display("arguments")]
  Arguments,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum ArgumentComponent {
  #[display("argument name")]
  Name,
  #[display("':' colon")]
  Colon,
  #[display("argument value")]
  Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum ObjectFieldComponent {
  #[display("field name")]
  Name,
  #[display("':' colon")]
  Colon,
  #[display("field value")]
  Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum TypeConditionComponent {
  #[display("'on' keyword")]
  OnKeyword,
  #[display("named type")]
  NamedType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum FieldComponent {
  #[display("field alias")]
  Alias,
  #[display("field name")]
  Name,
  #[display("arguments")]
  Arguments,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum VariableDefinitionComponent {
  #[display("'$' dollar sign")]
  Dollar,
  #[display("variable name")]
  Variable,
  #[display("':' colon")]
  Colon,
  #[display("variable type")]
  Type,
  #[display("default value")]
  DefaultValue,
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum InlineFragmentComponent {
  #[display("'...' spread operator")]
  Spread,
  #[display("type condition")]
  TypeCondition,
  #[display("directives")]
  Directives,
  #[display("selection set")]
  SelectionSet,
}

// ============================================================================
// Type System Syntax
// ============================================================================

/// A displayable named type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("named type")]
pub struct NamedTypeSyntax(pub(crate) ());

impl DisplayHuman for NamedTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for NamedTypeSyntax {
  type Lang = GraphQLx;
  type Component = NamedTypeComponent;
  type COMPONENTS = U3;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      NamedTypeComponent::Name,
      NamedTypeComponent::TypeGenerics,
      NamedTypeComponent::Bang,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [NamedTypeComponent::Name].into_iter().collect()
  }
}

/// A displayable list type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("list type")]
pub struct ListTypeSyntax(pub(crate) ());

impl DisplayHuman for ListTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for ListTypeSyntax {
  type Lang = GraphQLx;
  type Component = ListTypeComponent;
  type COMPONENTS = U4;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      ListTypeComponent::LBracket,
      ListTypeComponent::ElementType,
      ListTypeComponent::RBracket,
      ListTypeComponent::Bang,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      ListTypeComponent::LBracket,
      ListTypeComponent::ElementType,
      ListTypeComponent::RBracket,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable non-null type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("non-null type")]
pub struct NonNullTypeSyntax(pub(crate) ());

impl DisplayHuman for NonNullTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable set type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("set type")]
pub struct SetTypeSyntax(pub(crate) ());

impl DisplayHuman for SetTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for SetTypeSyntax {
  type Lang = GraphQLx;
  type Component = SetTypeComponent;
  type COMPONENTS = U4;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      SetTypeComponent::LAngle,
      SetTypeComponent::ElementType,
      SetTypeComponent::RAngle,
      SetTypeComponent::Bang,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      SetTypeComponent::LAngle,
      SetTypeComponent::ElementType,
      SetTypeComponent::RAngle,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable map type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("map type")]
pub struct MapTypeSyntax(pub(crate) ());

impl DisplayHuman for MapTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for MapTypeSyntax {
  type Lang = GraphQLx;
  type Component = MapTypeComponent;
  type COMPONENTS = U6;
  type REQUIRED = U5;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      MapTypeComponent::LAngle,
      MapTypeComponent::KeyType,
      MapTypeComponent::FatArrow,
      MapTypeComponent::ValueType,
      MapTypeComponent::RAngle,
      MapTypeComponent::Bang,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      MapTypeComponent::LAngle,
      MapTypeComponent::KeyType,
      MapTypeComponent::FatArrow,
      MapTypeComponent::ValueType,
      MapTypeComponent::RAngle,
    ]
    .into_iter()
    .collect()
  }
}

// ============================================================================
// Type Path Syntax (GraphQLx Extension)
// ============================================================================

/// A displayable path syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("path")]
pub struct PathSyntax(pub(crate) ());

impl DisplayHuman for PathSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for PathSyntax {
  type Lang = GraphQLx;
  type Component = PathComponent;
  type COMPONENTS = U2;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [PathComponent::PathSeparator, PathComponent::Segments]
      .into_iter()
      .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [PathComponent::Segments].into_iter().collect()
  }
}

/// A displayable type path syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type path")]
pub struct TypePathSyntax(pub(crate) ());

impl DisplayHuman for TypePathSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for TypePathSyntax {
  type Lang = GraphQLx;
  type Component = TypePathComponent;
  type COMPONENTS = U2;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [TypePathComponent::Path, TypePathComponent::TypeGenerics]
      .into_iter()
      .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [TypePathComponent::Path].into_iter().collect()
  }
}

/// A displayable definition type path syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("definition type path")]
pub struct DefinitionTypePathSyntax(pub(crate) ());

impl DisplayHuman for DefinitionTypePathSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for DefinitionTypePathSyntax {
  type Lang = GraphQLx;
  type Component = DefinitionTypePathComponent;
  type COMPONENTS = U2;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      DefinitionTypePathComponent::Path,
      DefinitionTypePathComponent::DefinitionTypeGenerics,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [DefinitionTypePathComponent::Path].into_iter().collect()
  }
}

/// A displayable path segment syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("path segment")]
pub struct PathSegmentSyntax(pub(crate) ());

impl DisplayHuman for PathSegmentSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for PathSegmentSyntax {
  type Lang = GraphQLx;
  type Component = PathSegmentComponent;
  type COMPONENTS = U1;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [PathSegmentComponent::Identifier].into_iter().collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [PathSegmentComponent::Identifier].into_iter().collect()
  }
}

// ============================================================================
// Generic Type System Syntax (GraphQLx Extension)
// ============================================================================

/// A displayable type parameter syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type parameter")]
pub struct TypeParameterSyntax(pub(crate) ());

impl DisplayHuman for TypeParameterSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for TypeParameterSyntax {
  type Lang = GraphQLx;
  type Component = TypeParameterComponent;
  type COMPONENTS = U3;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      TypeParameterComponent::Name,
      TypeParameterComponent::TypeConstraint,
      TypeParameterComponent::DefaultType,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [TypeParameterComponent::Name].into_iter().collect()
  }
}

/// A displayable type parameters syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type parameters")]
pub struct TypeParametersSyntax(pub(crate) ());

impl DisplayHuman for TypeParametersSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable type generics syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type generics")]
pub struct TypeGenericsSyntax(pub(crate) ());

impl DisplayHuman for TypeGenericsSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for TypeGenericsSyntax {
  type Lang = GraphQLx;
  type Component = TypeGenericsComponent;
  type COMPONENTS = U3;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      TypeGenericsComponent::LAngle,
      TypeGenericsComponent::TypeArguments,
      TypeGenericsComponent::RAngle,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      TypeGenericsComponent::LAngle,
      TypeGenericsComponent::TypeArguments,
      TypeGenericsComponent::RAngle,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable definition type generics syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("definition type generics")]
pub struct DefinitionTypeGenericsSyntax(pub(crate) ());

impl DisplayHuman for DefinitionTypeGenericsSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable extension type generics syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("extension type generics")]
pub struct ExtensionTypeGenericsSyntax(pub(crate) ());

impl DisplayHuman for ExtensionTypeGenericsSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable executable definition type generics syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("executable definition type generics")]
pub struct ExecutableDefinitionTypeGenericsSyntax(pub(crate) ());

impl DisplayHuman for ExecutableDefinitionTypeGenericsSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable where clause syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("where clause")]
pub struct WhereClauseSyntax(pub(crate) ());

impl DisplayHuman for WhereClauseSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for WhereClauseSyntax {
  type Lang = GraphQLx;
  type Component = WhereClauseComponent;
  type COMPONENTS = U2;
  type REQUIRED = U2;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      WhereClauseComponent::WhereKeyword,
      WhereClauseComponent::WherePredicates,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      WhereClauseComponent::WhereKeyword,
      WhereClauseComponent::WherePredicates,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable where predicate syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("where predicate")]
pub struct WherePredicateSyntax(pub(crate) ());

impl DisplayHuman for WherePredicateSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for WherePredicateSyntax {
  type Lang = GraphQLx;
  type Component = WherePredicateComponent;
  type COMPONENTS = U3;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      WherePredicateComponent::TypeParameter,
      WherePredicateComponent::Colon,
      WherePredicateComponent::TypeBounds,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      WherePredicateComponent::TypeParameter,
      WherePredicateComponent::Colon,
      WherePredicateComponent::TypeBounds,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable type bound syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type bound")]
pub struct TypeBoundSyntax(pub(crate) ());

impl DisplayHuman for TypeBoundSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable type constraint syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type constraint")]
pub struct TypeConstraintSyntax(pub(crate) ());

impl DisplayHuman for TypeConstraintSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable default type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("default type")]
pub struct DefaultTypeSyntax(pub(crate) ());

impl DisplayHuman for DefaultTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Import System Syntax (GraphQLx Extension)
// ============================================================================

/// A displayable import definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("import definition")]
pub struct ImportDefinitionSyntax(pub(crate) ());

impl DisplayHuman for ImportDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for ImportDefinitionSyntax {
  type Lang = GraphQLx;
  type Component = ImportDefinitionComponent;
  type COMPONENTS = U4;
  type REQUIRED = U4;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      ImportDefinitionComponent::ImportKeyword,
      ImportDefinitionComponent::ImportClause,
      ImportDefinitionComponent::FromKeyword,
      ImportDefinitionComponent::ModulePath,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      ImportDefinitionComponent::ImportKeyword,
      ImportDefinitionComponent::ImportClause,
      ImportDefinitionComponent::FromKeyword,
      ImportDefinitionComponent::ModulePath,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable import clause syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("import clause")]
pub struct ImportClauseSyntax(pub(crate) ());

impl DisplayHuman for ImportClauseSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for ImportClauseSyntax {
  type Lang = GraphQLx;
  type Component = ImportClauseComponent;
  type COMPONENTS = U2;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      ImportClauseComponent::ImportList,
      ImportClauseComponent::WildcardSpecifier,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [ImportClauseComponent::ImportList].into_iter().collect()
  }
}

/// A displayable import list syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("import list")]
pub struct ImportListSyntax(pub(crate) ());

impl DisplayHuman for ImportListSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable import member syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("import member")]
pub struct ImportMemberSyntax(pub(crate) ());

impl DisplayHuman for ImportMemberSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable named specifier syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("named specifier")]
pub struct NamedSpecifierSyntax(pub(crate) ());

impl DisplayHuman for NamedSpecifierSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for NamedSpecifierSyntax {
  type Lang = GraphQLx;
  type Component = NamedSpecifierComponent;
  type COMPONENTS = U3;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      NamedSpecifierComponent::ImportedName,
      NamedSpecifierComponent::AsKeyword,
      NamedSpecifierComponent::LocalName,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [NamedSpecifierComponent::ImportedName]
      .into_iter()
      .collect()
  }
}

/// A displayable wildcard specifier syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("wildcard specifier")]
pub struct WildcardSpecifierSyntax(pub(crate) ());

impl DisplayHuman for WildcardSpecifierSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable import alias syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("import alias")]
pub struct ImportAliasSyntax(pub(crate) ());

impl DisplayHuman for ImportAliasSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Type Definition Syntax (extended with generics)
// ============================================================================

/// A displayable scalar type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("scalar type definition")]
pub struct ScalarTypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for ScalarTypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// Continuing GraphQLx syntax implementations...
// Re-use the patterns established, now with GraphQLx types

/// A displayable object type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("object type definition")]
pub struct ObjectTypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for ObjectTypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable interface type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("interface type definition")]
pub struct InterfaceTypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for InterfaceTypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable union type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("union type definition")]
pub struct UnionTypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for UnionTypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable enum type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("enum type definition")]
pub struct EnumTypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for EnumTypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable input object type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("input object type definition")]
pub struct InputObjectTypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for InputObjectTypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type definition")]
pub struct TypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for TypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Type Extension Syntax
// ============================================================================

/// A displayable scalar type extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("scalar type extension")]
pub struct ScalarTypeExtensionSyntax(pub(crate) ());

impl DisplayHuman for ScalarTypeExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable object type extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("object type extension")]
pub struct ObjectTypeExtensionSyntax(pub(crate) ());

impl DisplayHuman for ObjectTypeExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable interface type extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("interface type extension")]
pub struct InterfaceTypeExtensionSyntax(pub(crate) ());

impl DisplayHuman for InterfaceTypeExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable union type extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("union type extension")]
pub struct UnionTypeExtensionSyntax(pub(crate) ());

impl DisplayHuman for UnionTypeExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable enum type extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("enum type extension")]
pub struct EnumTypeExtensionSyntax(pub(crate) ());

impl DisplayHuman for EnumTypeExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable input object type extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("input object type extension")]
pub struct InputObjectTypeExtensionSyntax(pub(crate) ());

impl DisplayHuman for InputObjectTypeExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable type extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type extension")]
pub struct TypeExtensionSyntax(pub(crate) ());

impl DisplayHuman for TypeExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Field and Argument Definition Syntax
// ============================================================================

/// A displayable field definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("field definition")]
pub struct FieldDefinitionSyntax(pub(crate) ());

impl DisplayHuman for FieldDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for FieldDefinitionSyntax {
  type Lang = GraphQLx;
  type Component = FieldDefinitionComponent;
  type COMPONENTS = U5;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      FieldDefinitionComponent::Name,
      FieldDefinitionComponent::ArgumentsDefinition,
      FieldDefinitionComponent::Colon,
      FieldDefinitionComponent::Type,
      FieldDefinitionComponent::Directives,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      FieldDefinitionComponent::Name,
      FieldDefinitionComponent::Colon,
      FieldDefinitionComponent::Type,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable fields definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("fields definition")]
pub struct FieldsDefinitionSyntax(pub(crate) ());

impl DisplayHuman for FieldsDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable input value definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("input value definition")]
pub struct InputValueDefinitionSyntax(pub(crate) ());

impl DisplayHuman for InputValueDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for InputValueDefinitionSyntax {
  type Lang = GraphQLx;
  type Component = InputValueDefinitionComponent;
  type COMPONENTS = U5;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      InputValueDefinitionComponent::Name,
      InputValueDefinitionComponent::Colon,
      InputValueDefinitionComponent::Type,
      InputValueDefinitionComponent::DefaultValue,
      InputValueDefinitionComponent::Directives,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      InputValueDefinitionComponent::Name,
      InputValueDefinitionComponent::Colon,
      InputValueDefinitionComponent::Type,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable arguments definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("arguments definition")]
pub struct ArgumentsDefinitionSyntax(pub(crate) ());

impl DisplayHuman for ArgumentsDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable input fields definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("input fields definition")]
pub struct InputFieldsDefinitionSyntax(pub(crate) ());

impl DisplayHuman for InputFieldsDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Directive Syntax
// ============================================================================

/// A displayable directive definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("directive definition")]
pub struct DirectiveDefinitionSyntax(pub(crate) ());

impl DisplayHuman for DirectiveDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable directive syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("directive")]
pub struct DirectiveSyntax(pub(crate) ());

impl DisplayHuman for DirectiveSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for DirectiveSyntax {
  type Lang = GraphQLx;
  type Component = DirectiveComponent;
  type COMPONENTS = U3;
  type REQUIRED = U2;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      DirectiveComponent::At,
      DirectiveComponent::Name,
      DirectiveComponent::Arguments,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [DirectiveComponent::At, DirectiveComponent::Name]
      .into_iter()
      .collect()
  }
}

/// A displayable directives syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("directives")]
pub struct DirectivesSyntax(pub(crate) ());

impl DisplayHuman for DirectivesSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable directive location syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("directive location")]
pub struct DirectiveLocationSyntax(pub(crate) ());

impl DisplayHuman for DirectiveLocationSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable directive locations syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("directive locations")]
pub struct DirectiveLocationsSyntax(pub(crate) ());

impl DisplayHuman for DirectiveLocationsSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Schema Definition Syntax
// ============================================================================

/// A displayable schema definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("schema definition")]
pub struct SchemaDefinitionSyntax(pub(crate) ());

impl DisplayHuman for SchemaDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable schema extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("schema extension")]
pub struct SchemaExtensionSyntax(pub(crate) ());

impl DisplayHuman for SchemaExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable root operation type definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("root operation type definition")]
pub struct RootOperationTypeDefinitionSyntax(pub(crate) ());

impl DisplayHuman for RootOperationTypeDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable root operation types definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("root operation types definition")]
pub struct RootOperationTypesDefinitionSyntax(pub(crate) ());

impl DisplayHuman for RootOperationTypesDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Enum Syntax
// ============================================================================

/// A displayable enum value definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("enum value definition")]
pub struct EnumValueDefinitionSyntax(pub(crate) ());

impl DisplayHuman for EnumValueDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable enum values definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("enum values definition")]
pub struct EnumValuesDefinitionSyntax(pub(crate) ());

impl DisplayHuman for EnumValuesDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Union Syntax
// ============================================================================

/// A displayable union member types syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("union member types")]
pub struct UnionMemberTypesSyntax(pub(crate) ());

impl DisplayHuman for UnionMemberTypesSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Interface Implementation Syntax
// ============================================================================

/// A displayable implements interfaces syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("implements interfaces")]
pub struct ImplementsInterfacesSyntax(pub(crate) ());

impl DisplayHuman for ImplementsInterfacesSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Executable Definition Syntax
// ============================================================================

/// A displayable operation name syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("operation name")]
pub struct OperationNameSyntax(pub(crate) ());

impl DisplayHuman for OperationNameSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable operation type syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("operation type")]
pub struct OperationTypeSyntax(pub(crate) ());

impl DisplayHuman for OperationTypeSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable named operation definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("named operation definition")]
pub struct NamedOperationDefinitionSyntax(pub(crate) ());

impl DisplayHuman for NamedOperationDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for NamedOperationDefinitionSyntax {
  type Lang = GraphQLx;
  type Component = NamedOperationDefinitionComponent;
  type COMPONENTS = U6;
  type REQUIRED = U2;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      NamedOperationDefinitionComponent::OperationType,
      NamedOperationDefinitionComponent::Name,
      NamedOperationDefinitionComponent::TypeParameters,
      NamedOperationDefinitionComponent::VariablesDefinition,
      NamedOperationDefinitionComponent::Directives,
      NamedOperationDefinitionComponent::SelectionSet,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      NamedOperationDefinitionComponent::OperationType,
      NamedOperationDefinitionComponent::SelectionSet,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable fragment definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("fragment definition")]
pub struct FragmentDefinitionSyntax(pub(crate) ());

impl DisplayHuman for FragmentDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for FragmentDefinitionSyntax {
  type Lang = GraphQLx;
  type Component = FragmentDefinitionComponent;
  type COMPONENTS = U5;
  type REQUIRED = U4;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      FragmentDefinitionComponent::FragmentKeyword,
      FragmentDefinitionComponent::FragmentTypePath,
      FragmentDefinitionComponent::TypeCondition,
      FragmentDefinitionComponent::Directives,
      FragmentDefinitionComponent::SelectionSet,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      FragmentDefinitionComponent::FragmentKeyword,
      FragmentDefinitionComponent::FragmentTypePath,
      FragmentDefinitionComponent::TypeCondition,
      FragmentDefinitionComponent::SelectionSet,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable fragment spread syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("fragment spread")]
pub struct FragmentSpreadSyntax(pub(crate) ());

impl DisplayHuman for FragmentSpreadSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for FragmentSpreadSyntax {
  type Lang = GraphQLx;
  type Component = FragmentSpreadComponent;
  type COMPONENTS = U3;
  type REQUIRED = U2;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      FragmentSpreadComponent::Spread,
      FragmentSpreadComponent::FragmentTypePath,
      FragmentSpreadComponent::Directives,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      FragmentSpreadComponent::Spread,
      FragmentSpreadComponent::FragmentTypePath,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable inline fragment syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("inline fragment")]
pub struct InlineFragmentSyntax(pub(crate) ());

impl DisplayHuman for InlineFragmentSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for InlineFragmentSyntax {
  type Lang = GraphQLx;
  type Component = InlineFragmentComponent;
  type COMPONENTS = U4;
  type REQUIRED = U2;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      InlineFragmentComponent::Spread,
      InlineFragmentComponent::TypeCondition,
      InlineFragmentComponent::Directives,
      InlineFragmentComponent::SelectionSet,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      InlineFragmentComponent::Spread,
      InlineFragmentComponent::SelectionSet,
    ]
    .into_iter()
    .collect()
  }
}

// ============================================================================
// Selection Syntax
// ============================================================================

/// A displayable field syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("field")]
pub struct FieldSyntax(pub(crate) ());

impl DisplayHuman for FieldSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for FieldSyntax {
  type Lang = GraphQLx;
  type Component = FieldComponent;
  type COMPONENTS = U5;
  type REQUIRED = U1;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      FieldComponent::Alias,
      FieldComponent::Name,
      FieldComponent::Arguments,
      FieldComponent::Directives,
      FieldComponent::SelectionSet,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [FieldComponent::Name].into_iter().collect()
  }
}

/// A displayable selection set syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("selection set")]
pub struct SelectionSetSyntax(pub(crate) ());

impl DisplayHuman for SelectionSetSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable alias syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("alias")]
pub struct AliasSyntax(pub(crate) ());

impl DisplayHuman for AliasSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Variable Syntax
// ============================================================================

/// A displayable variable definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("variable definition")]
pub struct VariableDefinitionSyntax(pub(crate) ());

impl DisplayHuman for VariableDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for VariableDefinitionSyntax {
  type Lang = GraphQLx;
  type Component = VariableDefinitionComponent;
  type COMPONENTS = U6;
  type REQUIRED = U4;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      VariableDefinitionComponent::Dollar,
      VariableDefinitionComponent::Variable,
      VariableDefinitionComponent::Colon,
      VariableDefinitionComponent::Type,
      VariableDefinitionComponent::DefaultValue,
      VariableDefinitionComponent::Directives,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      VariableDefinitionComponent::Dollar,
      VariableDefinitionComponent::Variable,
      VariableDefinitionComponent::Colon,
      VariableDefinitionComponent::Type,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable variables definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("variables definition")]
pub struct VariablesDefinitionSyntax(pub(crate) ());

impl DisplayHuman for VariablesDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable variable value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("variable value")]
pub struct VariableValueSyntax(pub(crate) ());

impl DisplayHuman for VariableValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Argument Syntax
// ============================================================================

/// A displayable argument syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("argument")]
pub struct ArgumentSyntax(pub(crate) ());

impl DisplayHuman for ArgumentSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for ArgumentSyntax {
  type Lang = GraphQLx;
  type Component = ArgumentComponent;
  type COMPONENTS = U3;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      ArgumentComponent::Name,
      ArgumentComponent::Colon,
      ArgumentComponent::Value,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      ArgumentComponent::Name,
      ArgumentComponent::Colon,
      ArgumentComponent::Value,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable arguments syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("arguments")]
pub struct ArgumentsSyntax(pub(crate) ());

impl DisplayHuman for ArgumentsSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Value Syntax
// ============================================================================

/// A displayable input value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("input value")]
pub struct InputValueSyntax(pub(crate) ());

impl DisplayHuman for InputValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable const input value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("const input value")]
pub struct ConstInputValueSyntax(pub(crate) ());

impl DisplayHuman for ConstInputValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable boolean value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("boolean value")]
pub struct BooleanValueSyntax(pub(crate) ());

impl DisplayHuman for BooleanValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable null value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("null value")]
pub struct NullValueSyntax(pub(crate) ());

impl DisplayHuman for NullValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable enum value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("enum value")]
pub struct EnumValueSyntax(pub(crate) ());

impl DisplayHuman for EnumValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable list value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("list value")]
pub struct ListValueSyntax(pub(crate) ());

impl DisplayHuman for ListValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable set value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("set value")]
pub struct SetValueSyntax(pub(crate) ());

impl DisplayHuman for SetValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable map value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("map value")]
pub struct MapValueSyntax(pub(crate) ());

impl DisplayHuman for MapValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable map entry syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("map entry")]
pub struct MapEntrySyntax(pub(crate) ());

impl DisplayHuman for MapEntrySyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for MapEntrySyntax {
  type Lang = GraphQLx;
  type Component = MapEntryComponent;
  type COMPONENTS = U3;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      MapEntryComponent::Key,
      MapEntryComponent::FatArrow,
      MapEntryComponent::Value,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      MapEntryComponent::Key,
      MapEntryComponent::FatArrow,
      MapEntryComponent::Value,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable object value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("object value")]
pub struct ObjectValueSyntax(pub(crate) ());

impl DisplayHuman for ObjectValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable object field syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("object field")]
pub struct ObjectFieldSyntax(pub(crate) ());

impl DisplayHuman for ObjectFieldSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for ObjectFieldSyntax {
  type Lang = GraphQLx;
  type Component = ObjectFieldComponent;
  type COMPONENTS = U3;
  type REQUIRED = U3;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      ObjectFieldComponent::Name,
      ObjectFieldComponent::Colon,
      ObjectFieldComponent::Value,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      ObjectFieldComponent::Name,
      ObjectFieldComponent::Colon,
      ObjectFieldComponent::Value,
    ]
    .into_iter()
    .collect()
  }
}

// ============================================================================
// Miscellaneous Syntax
// ============================================================================

/// A displayable type condition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type condition")]
pub struct TypeConditionSyntax(pub(crate) ());

impl DisplayHuman for TypeConditionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

impl Syntax for TypeConditionSyntax {
  type Lang = GraphQLx;
  type Component = TypeConditionComponent;
  type COMPONENTS = U2;
  type REQUIRED = U2;

  fn possible_components() -> GenericArray<Self::Component, Self::COMPONENTS> {
    [
      TypeConditionComponent::OnKeyword,
      TypeConditionComponent::NamedType,
    ]
    .into_iter()
    .collect()
  }

  fn required_components() -> GenericArray<Self::Component, Self::REQUIRED> {
    [
      TypeConditionComponent::OnKeyword,
      TypeConditionComponent::NamedType,
    ]
    .into_iter()
    .collect()
  }
}

/// A displayable description syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("description")]
pub struct DescriptionSyntax(pub(crate) ());

impl DisplayHuman for DescriptionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable default value syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("default value")]
pub struct DefaultValueSyntax(pub(crate) ());

impl DisplayHuman for DefaultValueSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable definition name syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("definition name")]
pub struct DefinitionNameSyntax(pub(crate) ());

impl DisplayHuman for DefinitionNameSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable extension name syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("extension name")]
pub struct ExtensionNameSyntax(pub(crate) ());

impl DisplayHuman for ExtensionNameSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable executable definition name syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("executable definition name")]
pub struct ExecutableDefinitionNameSyntax(pub(crate) ());

impl DisplayHuman for ExecutableDefinitionNameSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable fragment type path syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("fragment type path")]
pub struct FragmentTypePathSyntax(pub(crate) ());

impl DisplayHuman for FragmentTypePathSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Sum Types Syntax
// ============================================================================

/// A displayable type system definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type system definition")]
pub struct TypeSystemDefinitionSyntax(pub(crate) ());

impl DisplayHuman for TypeSystemDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable executable definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("executable definition")]
pub struct ExecutableDefinitionSyntax(pub(crate) ());

impl DisplayHuman for ExecutableDefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable type system extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type system extension")]
pub struct TypeSystemExtensionSyntax(pub(crate) ());

impl DisplayHuman for TypeSystemExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable type system definition or extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type system definition or extension")]
pub struct TypeSystemDefinitionOrExtensionSyntax(pub(crate) ());

impl DisplayHuman for TypeSystemDefinitionOrExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable definition syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("definition")]
pub struct DefinitionSyntax(pub(crate) ());

impl DisplayHuman for DefinitionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable definition or extension syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("definition or extension")]
pub struct DefinitionOrExtensionSyntax(pub(crate) ());

impl DisplayHuman for DefinitionOrExtensionSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

// ============================================================================
// Document Syntax
// ============================================================================

/// A displayable type system document syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("type system document")]
pub struct TypeSystemDocumentSyntax(pub(crate) ());

impl DisplayHuman for TypeSystemDocumentSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable executable document syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("executable document")]
pub struct ExecutableDocumentSyntax(pub(crate) ());

impl DisplayHuman for ExecutableDocumentSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}

/// A displayable document syntax description.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("document")]
pub struct DocumentSyntax(pub(crate) ());

impl DisplayHuman for DocumentSyntax {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    ::core::fmt::Display::fmt(self, f)
  }
}
