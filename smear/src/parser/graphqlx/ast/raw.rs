use super::{
  ConstInputValue, DefaultVec, DefinitionTypePath, FragmentTypePath, InputValue, StringValue, Type,
  TypeCondition, TypePath, VariableValue,
};
use crate::{
  parser::ident::Ident,
  scaffold::{
    self, Described, DirectiveLocations, ImplementInterfaces, Location, OperationType, Path,
    generic::WherePredicate,
  },
};

/// The default path segments container type used in the AST.
pub type DefaultPathSegmentsContainer<S> = DefaultVec<Ident<S>>;

/// The default type container type used in the AST.
pub type DefaultTypeContainer<S> = DefaultVec<Type<S>>;

pub type DefinitionTypeParam<S> = scaffold::generic::DefinitionTypeParam<Ident<S>, Type<S>>;

pub type ExtensionName<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
> = scaffold::generic::ExtensionName<Ident<S>, PathSegmentsContainer, TypeContainer>;
pub type DefinitionName<S, ParamsContainer = DefaultVec<DefinitionTypeParam<S>>> =
  scaffold::generic::DefinitionName<Ident<S>, Type<S>, ParamsContainer>;

/// The default container type used for arguments in the AST.
pub type DefaultArgumentsContainer<S> = DefaultVec<Argument<S>>;
/// The default container type used for directives in the AST.
pub type DefaultDirectivesContainer<S, ArgumentsContainer> =
  DefaultVec<Directive<S, ArgumentsContainer>>;
/// The default container type used for const arguments in the AST.
pub type DefaultConstArgumentsContainer<S> = DefaultVec<ConstArgument<S>>;
/// The default container type used for const directives in the AST.
pub type DefaultConstDirectivesContainer<S, ConstArgumentsContainer> =
  Vec<ConstDirective<S, ConstArgumentsContainer>>;

pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;
pub type Argument<S> = scaffold::Argument<Ident<S>, InputValue<S>>;
pub type Arguments<S, Container = DefaultArgumentsContainer<S>> =
  scaffold::Arguments<Argument<S>, Container>;

pub type ConstArgument<S> = scaffold::Argument<Ident<S>, ConstInputValue<S>>;
pub type ConstArguments<S, Container = DefaultConstArgumentsContainer<S>> =
  scaffold::Arguments<ConstArgument<S>, Container>;

pub type Directive<S, ArgumentsContainer = DefaultArgumentsContainer<S>> =
  scaffold::Directive<Ident<S>, Arguments<S, ArgumentsContainer>>;
pub type Directives<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  Container = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::Directives<Directive<S, ArgumentsContainer>, Container>;

pub type ConstDirective<S, ArgumentsContainer = DefaultConstArgumentsContainer<S>> =
  scaffold::Directive<Ident<S>, ConstArguments<S, ArgumentsContainer>>;
pub type ConstDirectives<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  Container = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::Directives<ConstDirective<S, ArgumentsContainer>, Container>;

/// The default container type used for input values in the AST.
pub type DefaultInputValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer> =
  Vec<InputValueDefinition<S, ConstArgumentsContainer, ConstDirectivesContainer>>;
/// The default container type used for enum values in the AST.
pub type DefaultEnumValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer> =
  Vec<EnumValueDefinition<S, ConstArgumentsContainer, ConstDirectivesContainer>>;
/// The default container type used for type paths in the AST.
pub type DefaultTypePathsContainer<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
> = DefaultVec<TypePath<S, PathSegmentsContainer, TypeContainer>>;
/// The default where predicates container type used in the AST.
pub type DefaultWherePredicatesContainer<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
> = DefaultVec<
  WherePredicate<Ident<S>, Type<S>, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
>;

/// The default container type used for locations in the AST.
pub type DefaultLocationsContainer = DefaultVec<Location>;
// /// The default container type used for root operation types in the AST.
// pub type DefaultRootOperationTypesContainer<S> = DefaultVec<RootOperationTypeDefinition<S>>;
/// The default container type used for variable definitions in the AST.
pub type DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer> =
  Vec<VariableDefinition<S, ArgumentsContainer, DirectivesContainer>>;
/// The default container type used for fields in the AST.
pub type DefaultFieldsContainer<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer> =
  Vec<FieldDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>>;

pub type FragmentSpread<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentContainer = Vec<Argument<S>>,
  Container = Vec<Directive<S, ArgumentContainer>>,
> = scaffold::FragmentSpread<
  FragmentTypePath<S, PathSegmentsContainer, TypeContainer>,
  Directives<S, ArgumentContainer, Container>,
>;

pub type InlineFragment<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = scaffold::InlineFragment<
  TypeCondition<S, PathSegmentsContainer, TypeContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  SelectionSet<S, PathSegmentsContainer, TypeContainer, ArgumentContainer, DirectiveContainer>,
>;

pub type Selection<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::StandardSelection<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S, PathSegmentsContainer, TypeContainer>,
  TypeCondition<S, PathSegmentsContainer, TypeContainer>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type SelectionSet<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::StandardSelectionSet<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S, PathSegmentsContainer, TypeContainer>,
  TypeCondition<S, PathSegmentsContainer, TypeContainer>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type Alias<S> = scaffold::Alias<Ident<S>>;

pub type Field<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::StandardField<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S, PathSegmentsContainer, TypeContainer>,
  TypeCondition<S, PathSegmentsContainer, TypeContainer>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ArgumentsDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::ArgumentsDefinition<
  InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  InputValuesContainer,
>;

pub type DirectiveDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
> = scaffold::DirectiveDefinition<
  Ident<S>,
  ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  DirectiveLocations<Location, LocationsContainer>,
>;

pub type VariableDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::VariableDefinition<
  VariableValue<S>,
  Type<S>,
  DefaultInputValue<S>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type VariablesDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  VariablesContainer = DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::VariablesDefinition<
  VariableDefinition<S, ArgumentsContainer, DirectivesContainer>,
  VariablesContainer,
>;

pub type InputValueDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  scaffold::InputValueDefinition<
    Ident<S>,
    Type<S>,
    DefaultInputValue<S>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type InputFieldsDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::generic::Constrained<
  Ident<S>,
  Type<S>,
  scaffold::InputFieldsDefinition<
    InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
    InputValuesContainer,
  >,
  PathSegmentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
>;

pub type FieldDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  scaffold::FieldDefinition<
    Ident<S>,
    ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
    Type<S>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type FieldsDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  FieldsContainer = DefaultFieldsContainer<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
> = scaffold::generic::Constrained<
  Ident<S>,
  Type<S>,
  scaffold::FieldsDefinition<
    FieldDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
    FieldsContainer,
  >,
  PathSegmentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
>;

pub type InputObjectTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InputObjectTypeDefinition<
  DefinitionName<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type DescribedInputObjectTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InputObjectTypeDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  StringValue<S>,
>;

pub type InputObjectTypeExtension<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InputObjectTypeExtension<
  ExtensionName<S, PathSegmentsContainer, TypeContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type FragmentDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::FragmentDefinition<
  DefinitionName<S>, // TODO: fragment<A, B, C> MyFragment<A, B, C> on User<A, B, C> { ... }
  TypeCondition<S, PathSegmentsContainer, TypeContainer>,
  Directives<S, ArgumentsContainer>,
  SelectionSet<S, PathSegmentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>,
>;

pub type ScalarTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::ScalarTypeDefinition<
  Ident<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type DescribedScalarTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>, StringValue<S>>;

pub type ScalarTypeExtension<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::ScalarTypeExtension<
  Ident<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ObjectTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::ObjectTypeDefinition<
  DefinitionName<S>,
  ImplementInterfaces<TypePath<S, PathSegmentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type DescribedObjectTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  ObjectTypeDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  StringValue<S>,
>;

pub type ObjectTypeExtension<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::ObjectTypeExtension<
  ExtensionName<S, PathSegmentsContainer, TypeContainer>,
  ImplementInterfaces<TypePath<S, PathSegmentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type InterfaceTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InterfaceTypeDefinition<
  DefinitionName<S>,
  ImplementInterfaces<TypePath<S, PathSegmentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type DescribedInterfaceTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InterfaceTypeDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  StringValue<S>,
>;

pub type InterfaceTypeExtension<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InterfaceTypeExtension<
  ExtensionName<S, PathSegmentsContainer, TypeContainer>,
  ImplementInterfaces<TypePath<S, PathSegmentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type UnionMemberTypes<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
> = scaffold::generic::Constrained<
  Ident<S>,
  Type<S>,
  scaffold::UnionMemberTypes<TypePath<S, PathSegmentsContainer, TypeContainer>, TypePathsContainer>,
  PathSegmentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
>;

pub type UnionTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::UnionTypeDefinition<
  DefinitionName<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
  >,
>;

pub type DescribedUnionTypeDefinition<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  UnionTypeDefinition<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
  StringValue<S>,
>;

pub type UnionTypeExtension<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, PathSegmentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::UnionTypeExtension<
  ExtensionName<S, PathSegmentsContainer, TypeContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<
    S,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
  >,
>;

pub type EnumValueDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  scaffold::EnumValueDefinition<
    Ident<S>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type EnumValuesDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::EnumValuesDefinition<
  EnumValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer,
>;

pub type EnumTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::EnumTypeDefinition<
  Ident<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
>;

pub type DescribedEnumTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
  StringValue<S>,
>;

pub type EnumTypeExtension<
  S,
  PathSegmentsContainer = DefaultPathSegmentsContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::EnumTypeExtension<
  Path<S, PathSegmentsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
>;

// pub type NamedOperationDefinition<S> = scaffold::NamedOperationDefinition<
//   Name<S>,
//   OperationType,
//   VariablesDefinition<S>,
//   Directives<S>,
//   SelectionSet<S>,
// >;

// pub type OperationDefinition<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
// > = scaffold::OperationDefinition<
//   Name<S>,
//   OperationType,
//   VariablesDefinition<S, ArgumentsContainer, DirectivesContainer>,
//   Directives<S, ArgumentsContainer, DirectivesContainer>,
//   SelectionSet<S, ArgumentsContainer, DirectivesContainer>,
// >;

// pub type RootOperationTypeDefinition<S> =
//   scaffold::RootOperationTypeDefinition<Name<S>, OperationType>;

// pub type RootOperationTypesDefinition<S, Container = DefaultRootOperationTypesContainer<S>> =
//   scaffold::RootOperationTypesDefinition<RootOperationTypeDefinition<S>, Container>;

// pub type SchemaDefinition<
//   S,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   Container = DefaultRootOperationTypesContainer<S>,
// > = scaffold::SchemaDefinition<
//   ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
//   RootOperationTypesDefinition<S, Container>,
// >;

// pub type SchemaExtension<
//   S,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   Container = DefaultRootOperationTypesContainer<S>,
// > = scaffold::SchemaExtension<
//   ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
//   RootOperationTypesDefinition<S, Container>,
// >;

// pub type TypeDefinition<
//   S,
//   NamesContainer = DefaultNamesContainer<S>,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
// > = scaffold::TypeDefinition<
//   ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>,
//   ObjectTypeDefinition<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//   >,
//   InterfaceTypeDefinition<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//   >,
//   UnionTypeDefinition<S, NamesContainer, ArgumentsContainer, DirectivesContainer>,
//   EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
//   InputObjectTypeDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
// >;

// pub type TypeSystemDefinition<
//   S,
//   NamesContainer = DefaultNamesContainer<S>,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
// > = scaffold::TypeSystemDefinition<
//   TypeDefinition<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//   >,
//   DirectiveDefinition<
//     S,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//     LocationsContainer,
//   >,
//   SchemaDefinition<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>,
// >;

// pub type DescribedTypeSystemDefinition<
//   S,
//   NamesContainer = DefaultNamesContainer<S>,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
// > = Described<
//   TypeSystemDefinition<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     LocationsContainer,
//     RootOperationTypesContainer,
//   >,
//   StringValue<S>,
// >;

// pub type TypeExtension<
//   S,
//   NamesContainer = DefaultNamesContainer<S>,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
// > = scaffold::TypeExtension<
//   ScalarTypeExtension<S, ArgumentsContainer, DirectivesContainer>,
//   ObjectTypeExtension<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//   >,
//   InterfaceTypeExtension<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//   >,
//   UnionTypeExtension<S, NamesContainer, ArgumentsContainer, DirectivesContainer>,
//   EnumTypeExtension<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
//   InputObjectTypeExtension<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
// >;

// pub type TypeSystemExtension<
//   S,
//   NamesContainer = DefaultNamesContainer<S>,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
// > = scaffold::TypeSystemExtension<
//   TypeExtension<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//   >,
//   SchemaExtension<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>,
// >;

// pub type TypeSystemDefinitionOrExtension<
//   S,
//   NamesContainer = DefaultNamesContainer<S>,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
// > = scaffold::TypeSystemDefinitionOrExtension<
//   DescribedTypeSystemDefinition<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     LocationsContainer,
//     RootOperationTypesContainer,
//   >,
//   TypeSystemExtension<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     RootOperationTypesContainer,
//   >,
// >;

// pub type ExecutableDefinition<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
// > = scaffold::ExecutableDefinition<
//   OperationDefinition<S, ArgumentsContainer, DirectivesContainer>,
//   FragmentDefinition<S, ArgumentsContainer, DirectivesContainer>,
// >;

// pub type DescribedExecutableDefinition<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
// > = Described<ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>, StringValue<S>>;

// pub type Definition<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
//   ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
//   NamesContainer = DefaultNamesContainer<S>,
//   InputValuesContainer = DefaultInputValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   EnumValuesContainer = DefaultEnumValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
// > = scaffold::Definition<
//   TypeSystemDefinition<
//     S,
//     NamesContainer,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     LocationsContainer,
//     RootOperationTypesContainer,
//   >,
//   ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>,
// >;

// pub type DescribedDefinition<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
//   ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
//   NamesContainer = DefaultNamesContainer<S>,
//   InputValuesContainer = DefaultInputValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   EnumValuesContainer = DefaultEnumValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
// > = Described<
//   Definition<
//     S,
//     ArgumentsContainer,
//     DirectivesContainer,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//     NamesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     LocationsContainer,
//     RootOperationTypesContainer,
//   >,
//   StringValue<S>,
// >;

// pub type DefinitionOrExtension<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
//   ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
//   NamesContainer = DefaultNamesContainer<S>,
//   InputValuesContainer = DefaultInputValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   EnumValuesContainer = DefaultEnumValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
// > = scaffold::DefinitionOrExtension<
//   DescribedDefinition<
//     S,
//     ArgumentsContainer,
//     DirectivesContainer,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//     NamesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     LocationsContainer,
//     RootOperationTypesContainer,
//   >,
//   TypeSystemExtension<
//     S,
//     NamesContainer,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     RootOperationTypesContainer,
//   >,
// >;

// pub type TypeSystemDocument<
//   S,
//   NamesContainer = DefaultNamesContainer<S>,
//   ArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
//   InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
//   DefinitionContainer = DefaultVec<
//     TypeSystemDefinitionOrExtension<
//       S,
//       NamesContainer,
//       ArgumentsContainer,
//       DirectivesContainer,
//       InputValuesContainer,
//       EnumValuesContainer,
//       LocationsContainer,
//       RootOperationTypesContainer,
//     >,
//   >,
// > = scaffold::Document<
//   TypeSystemDefinitionOrExtension<
//     S,
//     NamesContainer,
//     ArgumentsContainer,
//     DirectivesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     LocationsContainer,
//     RootOperationTypesContainer,
//   >,
//   DefinitionContainer,
// >;

// pub type ExecutableDocument<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
//   DefinitionContainer = DefaultVec<
//     ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>,
//   >,
// > = scaffold::Document<
//   ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>,
//   DefinitionContainer,
// >;

// pub type Document<
//   S,
//   ArgumentsContainer = DefaultArgumentsContainer<S>,
//   DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
//   ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
//   ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
//   NamesContainer = DefaultNamesContainer<S>,
//   InputValuesContainer = DefaultInputValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   EnumValuesContainer = DefaultEnumValuesContainer<
//     S,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//   >,
//   LocationsContainer = DefaultLocationsContainer,
//   RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
//   DefinitionContainer = DefaultVec<
//     DefinitionOrExtension<
//       S,
//       ArgumentsContainer,
//       DirectivesContainer,
//       ConstArgumentsContainer,
//       ConstDirectivesContainer,
//       NamesContainer,
//       InputValuesContainer,
//       EnumValuesContainer,
//       LocationsContainer,
//       RootOperationTypesContainer,
//     >,
//   >,
// > = scaffold::Document<
//   DefinitionOrExtension<
//     S,
//     ArgumentsContainer,
//     DirectivesContainer,
//     ConstArgumentsContainer,
//     ConstDirectivesContainer,
//     NamesContainer,
//     InputValuesContainer,
//     EnumValuesContainer,
//     LocationsContainer,
//     RootOperationTypesContainer,
//   >,
//   DefinitionContainer,
// >;
