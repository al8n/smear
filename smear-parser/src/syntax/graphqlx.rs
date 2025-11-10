
use derive_more::Display;
use logosky::utils::{
  GenericArrayDeque,
  human_display::DisplayHuman,
  syntax::Syntax,
  typenum::{U1, U2, U3, U4, U5, U6, U7, U8},
};
use smear_lexer::graphqlx::GraphQLx;

// ============================================================================
// Component Enums
// ============================================================================

// Reuse GraphQL component enums
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of NamedType  syntax
pub enum NamedTypeComponent {
  /// type name
  #[display("type name")]
  Name,
  /// type generics
  #[display("type generics")]
  TypeGenerics,
  /// non-null modifier '!'
  #[display("non-null modifier '!'")]
  Bang,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ListType  syntax
pub enum ListTypeComponent {
  /// '[' left bracket
  #[display("'[' left bracket")]
  LBracket,
  /// element type
  #[display("element type")]
  ElementType,
  /// ']' right bracket
  #[display("']' right bracket")]
  RBracket,
  /// non-null modifier '!'
  #[display("non-null modifier '!'")]
  Bang,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of SetType  syntax
pub enum SetTypeComponent {
  /// '<' left angle bracket
  #[display("'<' left angle bracket")]
  LAngle,
  /// element type
  #[display("element type")]
  ElementType,
  /// '>' right angle bracket
  #[display("'>' right angle bracket")]
  RAngle,
  /// non-null modifier '!'
  #[display("non-null modifier '!'")]
  Bang,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of MapType  syntax
pub enum MapTypeComponent {
  /// '<' left angle bracket
  #[display("'<' left angle bracket")]
  LAngle,
  /// key type
  #[display("key type")]
  KeyType,
  /// '=>' fat arrow
  #[display("'=>' fat arrow")]
  FatArrow,
  /// value type
  #[display("value type")]
  ValueType,
  /// '>' right angle bracket
  #[display("'>' right angle bracket")]
  RAngle,
  /// non-null modifier '!'
  #[display("non-null modifier '!'")]
  Bang,
}

// GraphQLx-specific: Type paths
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of PathSegment  syntax
pub enum PathSegmentComponent {
  /// identifier
  #[display("identifier")]
  Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of Path  syntax
pub enum PathComponent {
  /// '::' path separator
  #[display("'::' path separator")]
  PathSeparator,
  /// path segments
  #[display("path segments")]
  Segments,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of TypePath  syntax
pub enum TypePathComponent {
  /// path
  #[display("path")]
  Path,
  /// type generics
  #[display("type generics")]
  TypeGenerics,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of DefinitionTypePath  syntax
pub enum DefinitionTypePathComponent {
  /// path
  #[display("path")]
  Path,
  /// definition type generics
  #[display("definition type generics")]
  DefinitionTypeGenerics,
}

// GraphQLx-specific: Generics
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of TypeParameter  syntax
pub enum TypeParameterComponent {
  /// parameter name
  #[display("parameter name")]
  Name,
  /// type constraint
  #[display("type constraint")]
  TypeConstraint,
  /// default type
  #[display("default type")]
  DefaultType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of TypeGenerics  syntax
pub enum TypeGenericsComponent {
  /// '<' left angle bracket
  #[display("'<' left angle bracket")]
  LAngle,
  /// type arguments
  #[display("type arguments")]
  TypeArguments,
  /// '>' right angle bracket
  #[display("'>' right angle bracket")]
  RAngle,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of WhereClause  syntax
pub enum WhereClauseComponent {
  /// 'where' keyword
  #[display("'where' keyword")]
  WhereKeyword,
  /// where predicates
  #[display("where predicates")]
  WherePredicates,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of WherePredicate  syntax
pub enum WherePredicateComponent {
  /// type parameter
  #[display("type parameter")]
  TypeParameter,
  /// ':' colon
  #[display("':' colon")]
  Colon,
  /// type bounds
  #[display("type bounds")]
  TypeBounds,
}

// GraphQLx-specific: Import system
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ImportDefinition  syntax
pub enum ImportDefinitionComponent {
  /// 'import' keyword
  #[display("'import' keyword")]
  ImportKeyword,
  /// import clause
  #[display("import clause")]
  ImportClause,
  /// 'from' keyword
  #[display("'from' keyword")]
  FromKeyword,
  /// module path
  #[display("module path")]
  ModulePath,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ImportClause  syntax
pub enum ImportClauseComponent {
  /// import list
  #[display("import list")]
  ImportList,
  /// wildcard specifier
  #[display("wildcard specifier")]
  WildcardSpecifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of NamedSpecifier  syntax
pub enum NamedSpecifierComponent {
  /// imported name
  #[display("imported name")]
  ImportedName,
  /// 'as' keyword
  #[display("'as' keyword")]
  AsKeyword,
  /// local name
  #[display("local name")]
  LocalName,
}

// Type Definition Components (same as GraphQL but with generics support)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ObjectTypeDefinition  syntax
pub enum ObjectTypeDefinitionComponent {
  /// 'type' keyword
  #[display("'type' keyword")]
  TypeKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// 'implements' clause
  #[display("'implements' clause")]
  ImplementsInterfaces,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// fields definition
  #[display("fields definition")]
  FieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of InterfaceTypeDefinition  syntax
pub enum InterfaceTypeDefinitionComponent {
  /// 'interface' keyword
  #[display("'interface' keyword")]
  InterfaceKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// 'implements' clause
  #[display("'implements' clause")]
  ImplementsInterfaces,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// fields definition
  #[display("fields definition")]
  FieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of UnionTypeDefinition  syntax
pub enum UnionTypeDefinitionComponent {
  /// 'union' keyword
  #[display("'union' keyword")]
  UnionKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// union members
  #[display("union members")]
  UnionMembers,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of EnumTypeDefinition  syntax
pub enum EnumTypeDefinitionComponent {
  /// 'enum' keyword
  #[display("'enum' keyword")]
  EnumKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// directives
  #[display("directives")]
  Directives,
  /// enum values definition
  #[display("enum values definition")]
  EnumValuesDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of InputObjectTypeDefinition  syntax
pub enum InputObjectTypeDefinitionComponent {
  /// 'input' keyword
  #[display("'input' keyword")]
  InputKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// input fields definition
  #[display("input fields definition")]
  InputFieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ScalarTypeDefinition  syntax
pub enum ScalarTypeDefinitionComponent {
  /// 'scalar' keyword
  #[display("'scalar' keyword")]
  ScalarKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
}

// Extension types
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ScalarTypeExtension  syntax
pub enum ScalarTypeExtensionComponent {
  /// 'extend' keyword
  #[display("'extend' keyword")]
  ExtendKeyword,
  /// 'scalar' keyword
  #[display("'scalar' keyword")]
  ScalarKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ObjectTypeExtension  syntax
pub enum ObjectTypeExtensionComponent {
  /// 'extend' keyword
  #[display("'extend' keyword")]
  ExtendKeyword,
  /// 'type' keyword
  #[display("'type' keyword")]
  TypeKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// 'implements' clause
  #[display("'implements' clause")]
  ImplementsInterfaces,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// fields definition
  #[display("fields definition")]
  FieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of InterfaceTypeExtension  syntax
pub enum InterfaceTypeExtensionComponent {
  /// 'extend' keyword
  #[display("'extend' keyword")]
  ExtendKeyword,
  /// 'interface' keyword
  #[display("'interface' keyword")]
  InterfaceKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// 'implements' clause
  #[display("'implements' clause")]
  ImplementsInterfaces,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// fields definition
  #[display("fields definition")]
  FieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of UnionTypeExtension  syntax
pub enum UnionTypeExtensionComponent {
  /// 'extend' keyword
  #[display("'extend' keyword")]
  ExtendKeyword,
  /// 'union' keyword
  #[display("'union' keyword")]
  UnionKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// union members
  #[display("union members")]
  UnionMembers,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of EnumTypeExtension  syntax
pub enum EnumTypeExtensionComponent {
  /// 'extend' keyword
  #[display("'extend' keyword")]
  ExtendKeyword,
  /// 'enum' keyword
  #[display("'enum' keyword")]
  EnumKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// directives
  #[display("directives")]
  Directives,
  /// enum values definition
  #[display("enum values definition")]
  EnumValuesDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of InputObjectTypeExtension  syntax
pub enum InputObjectTypeExtensionComponent {
  /// 'extend' keyword
  #[display("'extend' keyword")]
  ExtendKeyword,
  /// 'input' keyword
  #[display("'input' keyword")]
  InputKeyword,
  /// type name
  #[display("type name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// where clause
  #[display("where clause")]
  WhereClause,
  /// directives
  #[display("directives")]
  Directives,
  /// input fields definition
  #[display("input fields definition")]
  InputFieldsDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of SchemaExtension  syntax
pub enum SchemaExtensionComponent {
  /// 'extend' keyword
  #[display("'extend' keyword")]
  ExtendKeyword,
  /// 'schema' keyword
  #[display("'schema' keyword")]
  SchemaKeyword,
  /// directives
  #[display("directives")]
  Directives,
  /// root operation types definition
  #[display("root operation types definition")]
  RootOperationTypesDefinition,
}

// Field and operations (same as GraphQL)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of FieldDefinition  syntax
pub enum FieldDefinitionComponent {
  /// field name
  #[display("field name")]
  Name,
  /// arguments definition
  #[display("arguments definition")]
  ArgumentsDefinition,
  /// ':' colon
  #[display("':' colon")]
  Colon,
  /// field type
  #[display("field type")]
  Type,
  /// directives
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of InputValueDefinition  syntax
pub enum InputValueDefinitionComponent {
  /// input value name
  #[display("input value name")]
  Name,
  /// ':' colon
  #[display("':' colon")]
  Colon,
  /// input value type
  #[display("input value type")]
  Type,
  /// default value
  #[display("default value")]
  DefaultValue,
  /// directives
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of NamedOperationDefinition  syntax
pub enum NamedOperationDefinitionComponent {
  /// operation type
  #[display("operation type")]
  OperationType,
  /// operation name
  #[display("operation name")]
  Name,
  /// type parameters
  #[display("type parameters")]
  TypeParameters,
  /// variables definition
  #[display("variables definition")]
  VariablesDefinition,
  /// directives
  #[display("directives")]
  Directives,
  /// selection set
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of FragmentDefinition  syntax
pub enum FragmentDefinitionComponent {
  /// 'fragment' keyword
  #[display("'fragment' keyword")]
  FragmentKeyword,
  /// fragment type path
  #[display("fragment type path")]
  FragmentTypePath,
  /// type condition
  #[display("type condition")]
  TypeCondition,
  /// directives
  #[display("directives")]
  Directives,
  /// selection set
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of FragmentSpread  syntax
pub enum FragmentSpreadComponent {
  /// '...' spread operator
  #[display("'...' spread operator")]
  Spread,
  /// fragment type path
  #[display("fragment type path")]
  FragmentTypePath,
  /// directives
  #[display("directives")]
  Directives,
}

// Value components
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of MapEntry  syntax
pub enum MapEntryComponent {
  /// key
  #[display("key")]
  Key,
  /// '=>' fat arrow
  #[display("'=>' fat arrow")]
  FatArrow,
  /// value
  #[display("value")]
  Value,
}

// Reuse other GraphQL components
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of Directive  syntax
pub enum DirectiveComponent {
  /// '@' at symbol
  #[display("'@' at symbol")]
  At,
  /// directive name
  #[display("directive name")]
  Name,
  /// arguments
  #[display("arguments")]
  Arguments,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of Argument  syntax
pub enum ArgumentComponent {
  /// argument name
  #[display("argument name")]
  Name,
  /// ':' colon
  #[display("':' colon")]
  Colon,
  /// argument value
  #[display("argument value")]
  Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of ObjectField  syntax
pub enum ObjectFieldComponent {
  /// field name
  #[display("field name")]
  Name,
  /// ':' colon
  #[display("':' colon")]
  Colon,
  /// field value
  #[display("field value")]
  Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of TypeCondition  syntax
pub enum TypeConditionComponent {
  /// 'on' keyword
  #[display("'on' keyword")]
  OnKeyword,
  /// named type
  #[display("named type")]
  NamedType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of Field  syntax
pub enum FieldComponent {
  /// field alias
  #[display("field alias")]
  Alias,
  /// field name
  #[display("field name")]
  Name,
  /// arguments
  #[display("arguments")]
  Arguments,
  /// directives
  #[display("directives")]
  Directives,
  /// selection set
  #[display("selection set")]
  SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of VariableDefinition  syntax
pub enum VariableDefinitionComponent {
  /// '$' dollar sign
  #[display("'$' dollar sign")]
  Dollar,
  /// variable name
  #[display("variable name")]
  Variable,
  /// ':' colon
  #[display("':' colon")]
  Colon,
  /// variable type
  #[display("variable type")]
  Type,
  /// default value
  #[display("default value")]
  DefaultValue,
  /// directives
  #[display("directives")]
  Directives,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
/// Components of InlineFragment  syntax
pub enum InlineFragmentComponent {
  /// '...' spread operator
  #[display("'...' spread operator")]
  Spread,
  /// type condition
  #[display("type condition")]
  TypeCondition,
  /// directives
  #[display("directives")]
  Directives,
  /// selection set
  #[display("selection set")]
  SelectionSet,
}

/// Components of VariableValue syntax
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum VariableValueComponent {
  /// dollar
  #[display("dollar")]
  Dollar,
  /// name
  #[display("name")]
  Name,
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<NamedTypeComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(NamedTypeComponent::Name);
      deque.push_back(NamedTypeComponent::TypeGenerics);
      deque.push_back(NamedTypeComponent::Bang);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<NamedTypeComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(NamedTypeComponent::Name);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<ListTypeComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ListTypeComponent::LBracket);
      deque.push_back(ListTypeComponent::ElementType);
      deque.push_back(ListTypeComponent::RBracket);
      deque.push_back(ListTypeComponent::Bang);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<ListTypeComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ListTypeComponent::LBracket);
      deque.push_back(ListTypeComponent::ElementType);
      deque.push_back(ListTypeComponent::RBracket);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<SetTypeComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(SetTypeComponent::LAngle);
      deque.push_back(SetTypeComponent::ElementType);
      deque.push_back(SetTypeComponent::RAngle);
      deque.push_back(SetTypeComponent::Bang);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<SetTypeComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(SetTypeComponent::LAngle);
      deque.push_back(SetTypeComponent::ElementType);
      deque.push_back(SetTypeComponent::RAngle);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<MapTypeComponent, U6> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(MapTypeComponent::LAngle);
      deque.push_back(MapTypeComponent::KeyType);
      deque.push_back(MapTypeComponent::FatArrow);
      deque.push_back(MapTypeComponent::ValueType);
      deque.push_back(MapTypeComponent::RAngle);
      deque.push_back(MapTypeComponent::Bang);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<MapTypeComponent, U5> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(MapTypeComponent::LAngle);
      deque.push_back(MapTypeComponent::KeyType);
      deque.push_back(MapTypeComponent::FatArrow);
      deque.push_back(MapTypeComponent::ValueType);
      deque.push_back(MapTypeComponent::RAngle);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<PathComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(PathComponent::PathSeparator);
      deque.push_back(PathComponent::Segments);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<PathComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(PathComponent::Segments);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<TypePathComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypePathComponent::Path);
      deque.push_back(TypePathComponent::TypeGenerics);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<TypePathComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypePathComponent::Path);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<DefinitionTypePathComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(DefinitionTypePathComponent::Path);
      deque.push_back(DefinitionTypePathComponent::DefinitionTypeGenerics);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<DefinitionTypePathComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(DefinitionTypePathComponent::Path);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<PathSegmentComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(PathSegmentComponent::Identifier);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<PathSegmentComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(PathSegmentComponent::Identifier);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<TypeParameterComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypeParameterComponent::Name);
      deque.push_back(TypeParameterComponent::TypeConstraint);
      deque.push_back(TypeParameterComponent::DefaultType);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<TypeParameterComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypeParameterComponent::Name);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<TypeGenericsComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypeGenericsComponent::LAngle);
      deque.push_back(TypeGenericsComponent::TypeArguments);
      deque.push_back(TypeGenericsComponent::RAngle);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<TypeGenericsComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypeGenericsComponent::LAngle);
      deque.push_back(TypeGenericsComponent::TypeArguments);
      deque.push_back(TypeGenericsComponent::RAngle);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<WhereClauseComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(WhereClauseComponent::WhereKeyword);
      deque.push_back(WhereClauseComponent::WherePredicates);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<WhereClauseComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(WhereClauseComponent::WhereKeyword);
      deque.push_back(WhereClauseComponent::WherePredicates);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<WherePredicateComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(WherePredicateComponent::TypeParameter);
      deque.push_back(WherePredicateComponent::Colon);
      deque.push_back(WherePredicateComponent::TypeBounds);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<WherePredicateComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(WherePredicateComponent::TypeParameter);
      deque.push_back(WherePredicateComponent::Colon);
      deque.push_back(WherePredicateComponent::TypeBounds);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<ImportDefinitionComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ImportDefinitionComponent::ImportKeyword);
      deque.push_back(ImportDefinitionComponent::ImportClause);
      deque.push_back(ImportDefinitionComponent::FromKeyword);
      deque.push_back(ImportDefinitionComponent::ModulePath);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<ImportDefinitionComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ImportDefinitionComponent::ImportKeyword);
      deque.push_back(ImportDefinitionComponent::ImportClause);
      deque.push_back(ImportDefinitionComponent::FromKeyword);
      deque.push_back(ImportDefinitionComponent::ModulePath);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<ImportClauseComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ImportClauseComponent::ImportList);
      deque.push_back(ImportClauseComponent::WildcardSpecifier);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<ImportClauseComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ImportClauseComponent::ImportList);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<NamedSpecifierComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(NamedSpecifierComponent::ImportedName);
      deque.push_back(NamedSpecifierComponent::AsKeyword);
      deque.push_back(NamedSpecifierComponent::LocalName);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<NamedSpecifierComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(NamedSpecifierComponent::ImportedName);
      deque
    };
    &REQUIRED
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

impl Syntax for ScalarTypeExtensionSyntax {
  type Lang = GraphQLx;
  type Component = ScalarTypeExtensionComponent;
  type COMPONENTS = U6;
  type REQUIRED = U3;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<ScalarTypeExtensionComponent, U6> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ScalarTypeExtensionComponent::ExtendKeyword);
      deque.push_back(ScalarTypeExtensionComponent::ScalarKeyword);
      deque.push_back(ScalarTypeExtensionComponent::Name);
      deque.push_back(ScalarTypeExtensionComponent::TypeParameters);
      deque.push_back(ScalarTypeExtensionComponent::WhereClause);
      deque.push_back(ScalarTypeExtensionComponent::Directives);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<ScalarTypeExtensionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ScalarTypeExtensionComponent::ExtendKeyword);
      deque.push_back(ScalarTypeExtensionComponent::ScalarKeyword);
      deque.push_back(ScalarTypeExtensionComponent::Name);
      deque
    };
    &REQUIRED
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

impl Syntax for ObjectTypeExtensionSyntax {
  type Lang = GraphQLx;
  type Component = ObjectTypeExtensionComponent;
  type COMPONENTS = U8;
  type REQUIRED = U3;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<ObjectTypeExtensionComponent, U8> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ObjectTypeExtensionComponent::ExtendKeyword);
      deque.push_back(ObjectTypeExtensionComponent::TypeKeyword);
      deque.push_back(ObjectTypeExtensionComponent::Name);
      deque.push_back(ObjectTypeExtensionComponent::TypeParameters);
      deque.push_back(ObjectTypeExtensionComponent::ImplementsInterfaces);
      deque.push_back(ObjectTypeExtensionComponent::WhereClause);
      deque.push_back(ObjectTypeExtensionComponent::Directives);
      deque.push_back(ObjectTypeExtensionComponent::FieldsDefinition);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<ObjectTypeExtensionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ObjectTypeExtensionComponent::ExtendKeyword);
      deque.push_back(ObjectTypeExtensionComponent::TypeKeyword);
      deque.push_back(ObjectTypeExtensionComponent::Name);
      deque
    };
    &REQUIRED
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

impl Syntax for InterfaceTypeExtensionSyntax {
  type Lang = GraphQLx;
  type Component = InterfaceTypeExtensionComponent;
  type COMPONENTS = U8;
  type REQUIRED = U3;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<InterfaceTypeExtensionComponent, U8> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InterfaceTypeExtensionComponent::ExtendKeyword);
      deque.push_back(InterfaceTypeExtensionComponent::InterfaceKeyword);
      deque.push_back(InterfaceTypeExtensionComponent::Name);
      deque.push_back(InterfaceTypeExtensionComponent::TypeParameters);
      deque.push_back(InterfaceTypeExtensionComponent::ImplementsInterfaces);
      deque.push_back(InterfaceTypeExtensionComponent::WhereClause);
      deque.push_back(InterfaceTypeExtensionComponent::Directives);
      deque.push_back(InterfaceTypeExtensionComponent::FieldsDefinition);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<InterfaceTypeExtensionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InterfaceTypeExtensionComponent::ExtendKeyword);
      deque.push_back(InterfaceTypeExtensionComponent::InterfaceKeyword);
      deque.push_back(InterfaceTypeExtensionComponent::Name);
      deque
    };
    &REQUIRED
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

impl Syntax for UnionTypeExtensionSyntax {
  type Lang = GraphQLx;
  type Component = UnionTypeExtensionComponent;
  type COMPONENTS = U7;
  type REQUIRED = U3;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<UnionTypeExtensionComponent, U7> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(UnionTypeExtensionComponent::ExtendKeyword);
      deque.push_back(UnionTypeExtensionComponent::UnionKeyword);
      deque.push_back(UnionTypeExtensionComponent::Name);
      deque.push_back(UnionTypeExtensionComponent::TypeParameters);
      deque.push_back(UnionTypeExtensionComponent::WhereClause);
      deque.push_back(UnionTypeExtensionComponent::Directives);
      deque.push_back(UnionTypeExtensionComponent::UnionMembers);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<UnionTypeExtensionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(UnionTypeExtensionComponent::ExtendKeyword);
      deque.push_back(UnionTypeExtensionComponent::UnionKeyword);
      deque.push_back(UnionTypeExtensionComponent::Name);
      deque
    };
    &REQUIRED
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

impl Syntax for EnumTypeExtensionSyntax {
  type Lang = GraphQLx;
  type Component = EnumTypeExtensionComponent;
  type COMPONENTS = U5;
  type REQUIRED = U3;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<EnumTypeExtensionComponent, U5> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(EnumTypeExtensionComponent::ExtendKeyword);
      deque.push_back(EnumTypeExtensionComponent::EnumKeyword);
      deque.push_back(EnumTypeExtensionComponent::Name);
      deque.push_back(EnumTypeExtensionComponent::Directives);
      deque.push_back(EnumTypeExtensionComponent::EnumValuesDefinition);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<EnumTypeExtensionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(EnumTypeExtensionComponent::ExtendKeyword);
      deque.push_back(EnumTypeExtensionComponent::EnumKeyword);
      deque.push_back(EnumTypeExtensionComponent::Name);
      deque
    };
    &REQUIRED
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

impl Syntax for InputObjectTypeExtensionSyntax {
  type Lang = GraphQLx;
  type Component = InputObjectTypeExtensionComponent;
  type COMPONENTS = U7;
  type REQUIRED = U3;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<InputObjectTypeExtensionComponent, U7> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InputObjectTypeExtensionComponent::ExtendKeyword);
      deque.push_back(InputObjectTypeExtensionComponent::InputKeyword);
      deque.push_back(InputObjectTypeExtensionComponent::Name);
      deque.push_back(InputObjectTypeExtensionComponent::TypeParameters);
      deque.push_back(InputObjectTypeExtensionComponent::WhereClause);
      deque.push_back(InputObjectTypeExtensionComponent::Directives);
      deque.push_back(InputObjectTypeExtensionComponent::InputFieldsDefinition);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<InputObjectTypeExtensionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InputObjectTypeExtensionComponent::ExtendKeyword);
      deque.push_back(InputObjectTypeExtensionComponent::InputKeyword);
      deque.push_back(InputObjectTypeExtensionComponent::Name);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<FieldDefinitionComponent, U5> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FieldDefinitionComponent::Name);
      deque.push_back(FieldDefinitionComponent::ArgumentsDefinition);
      deque.push_back(FieldDefinitionComponent::Colon);
      deque.push_back(FieldDefinitionComponent::Type);
      deque.push_back(FieldDefinitionComponent::Directives);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<FieldDefinitionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FieldDefinitionComponent::Name);
      deque.push_back(FieldDefinitionComponent::Colon);
      deque.push_back(FieldDefinitionComponent::Type);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<InputValueDefinitionComponent, U5> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InputValueDefinitionComponent::Name);
      deque.push_back(InputValueDefinitionComponent::Colon);
      deque.push_back(InputValueDefinitionComponent::Type);
      deque.push_back(InputValueDefinitionComponent::DefaultValue);
      deque.push_back(InputValueDefinitionComponent::Directives);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<InputValueDefinitionComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InputValueDefinitionComponent::Name);
      deque.push_back(InputValueDefinitionComponent::Colon);
      deque.push_back(InputValueDefinitionComponent::Type);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<DirectiveComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(DirectiveComponent::At);
      deque.push_back(DirectiveComponent::Name);
      deque.push_back(DirectiveComponent::Arguments);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<DirectiveComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(DirectiveComponent::At);
      deque.push_back(DirectiveComponent::Name);
      deque
    };
    &REQUIRED
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

impl Syntax for SchemaExtensionSyntax {
  type Lang = GraphQLx;
  type Component = SchemaExtensionComponent;
  type COMPONENTS = U4;
  type REQUIRED = U2;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<SchemaExtensionComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(SchemaExtensionComponent::ExtendKeyword);
      deque.push_back(SchemaExtensionComponent::SchemaKeyword);
      deque.push_back(SchemaExtensionComponent::Directives);
      deque.push_back(SchemaExtensionComponent::RootOperationTypesDefinition);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<SchemaExtensionComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(SchemaExtensionComponent::ExtendKeyword);
      deque.push_back(SchemaExtensionComponent::SchemaKeyword);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<NamedOperationDefinitionComponent, U6> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(NamedOperationDefinitionComponent::OperationType);
      deque.push_back(NamedOperationDefinitionComponent::Name);
      deque.push_back(NamedOperationDefinitionComponent::TypeParameters);
      deque.push_back(NamedOperationDefinitionComponent::VariablesDefinition);
      deque.push_back(NamedOperationDefinitionComponent::Directives);
      deque.push_back(NamedOperationDefinitionComponent::SelectionSet);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<NamedOperationDefinitionComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(NamedOperationDefinitionComponent::OperationType);
      deque.push_back(NamedOperationDefinitionComponent::SelectionSet);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<FragmentDefinitionComponent, U5> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FragmentDefinitionComponent::FragmentKeyword);
      deque.push_back(FragmentDefinitionComponent::FragmentTypePath);
      deque.push_back(FragmentDefinitionComponent::TypeCondition);
      deque.push_back(FragmentDefinitionComponent::Directives);
      deque.push_back(FragmentDefinitionComponent::SelectionSet);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<FragmentDefinitionComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FragmentDefinitionComponent::FragmentKeyword);
      deque.push_back(FragmentDefinitionComponent::FragmentTypePath);
      deque.push_back(FragmentDefinitionComponent::TypeCondition);
      deque.push_back(FragmentDefinitionComponent::SelectionSet);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<FragmentSpreadComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FragmentSpreadComponent::Spread);
      deque.push_back(FragmentSpreadComponent::FragmentTypePath);
      deque.push_back(FragmentSpreadComponent::Directives);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<FragmentSpreadComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FragmentSpreadComponent::Spread);
      deque.push_back(FragmentSpreadComponent::FragmentTypePath);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<InlineFragmentComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InlineFragmentComponent::Spread);
      deque.push_back(InlineFragmentComponent::TypeCondition);
      deque.push_back(InlineFragmentComponent::Directives);
      deque.push_back(InlineFragmentComponent::SelectionSet);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<InlineFragmentComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(InlineFragmentComponent::Spread);
      deque.push_back(InlineFragmentComponent::SelectionSet);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<FieldComponent, U5> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FieldComponent::Alias);
      deque.push_back(FieldComponent::Name);
      deque.push_back(FieldComponent::Arguments);
      deque.push_back(FieldComponent::Directives);
      deque.push_back(FieldComponent::SelectionSet);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<FieldComponent, U1> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(FieldComponent::Name);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<VariableDefinitionComponent, U6> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(VariableDefinitionComponent::Dollar);
      deque.push_back(VariableDefinitionComponent::Variable);
      deque.push_back(VariableDefinitionComponent::Colon);
      deque.push_back(VariableDefinitionComponent::Type);
      deque.push_back(VariableDefinitionComponent::DefaultValue);
      deque.push_back(VariableDefinitionComponent::Directives);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<VariableDefinitionComponent, U4> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(VariableDefinitionComponent::Dollar);
      deque.push_back(VariableDefinitionComponent::Variable);
      deque.push_back(VariableDefinitionComponent::Colon);
      deque.push_back(VariableDefinitionComponent::Type);
      deque
    };
    &REQUIRED
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

impl Syntax for VariableValueSyntax {
  type Lang = GraphQLx;
  type Component = VariableValueComponent;
  type COMPONENTS = U2;
  type REQUIRED = U2;

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<VariableValueComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(VariableValueComponent::Dollar);
      deque.push_back(VariableValueComponent::Name);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<VariableValueComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(VariableValueComponent::Dollar);
      deque.push_back(VariableValueComponent::Name);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<ArgumentComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ArgumentComponent::Name);
      deque.push_back(ArgumentComponent::Colon);
      deque.push_back(ArgumentComponent::Value);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<ArgumentComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ArgumentComponent::Name);
      deque.push_back(ArgumentComponent::Colon);
      deque.push_back(ArgumentComponent::Value);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<MapEntryComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(MapEntryComponent::Key);
      deque.push_back(MapEntryComponent::FatArrow);
      deque.push_back(MapEntryComponent::Value);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<MapEntryComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(MapEntryComponent::Key);
      deque.push_back(MapEntryComponent::FatArrow);
      deque.push_back(MapEntryComponent::Value);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<ObjectFieldComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ObjectFieldComponent::Name);
      deque.push_back(ObjectFieldComponent::Colon);
      deque.push_back(ObjectFieldComponent::Value);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<ObjectFieldComponent, U3> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(ObjectFieldComponent::Name);
      deque.push_back(ObjectFieldComponent::Colon);
      deque.push_back(ObjectFieldComponent::Value);
      deque
    };
    &REQUIRED
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

  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    static COMPONENTS: GenericArrayDeque<TypeConditionComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypeConditionComponent::OnKeyword);
      deque.push_back(TypeConditionComponent::NamedType);
      deque
    };
    &COMPONENTS
  }

  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    static REQUIRED: GenericArrayDeque<TypeConditionComponent, U2> = {
      let mut deque = GenericArrayDeque::new();
      deque.push_back(TypeConditionComponent::OnKeyword);
      deque.push_back(TypeConditionComponent::NamedType);
      deque
    };
    &REQUIRED
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
