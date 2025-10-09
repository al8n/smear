use super::{
  ConstInputValue, DefaultVec, DirectiveDefinition, FragmentDefinition, FragmentTypePath,
  InputValue, StringValue, Type, TypeCondition, TypePath, VariableValue,
};
use crate::{
  parser::ident::Ident,
  scaffold::{self, ImplementInterfaces, Location, OperationType, Path},
};

pub type Described<T, S> = scaffold::Described<T, StringValue<S>>;

/// The default path segments container type used in the AST.
pub type DefaultIdentsContainer<S> = DefaultVec<Ident<S>>;

/// The default type container type used in the AST.
pub type DefaultTypeContainer<S> = DefaultVec<Type<S>>;
/// The default definition type params container type used in the AST.
pub type DefaultDefinitionTypeParamsContainer<S> = DefaultVec<DefinitionTypeParam<S>>;
/// The default extension type params container type used in the AST.
pub type DefaultExtensionTypeParamsContainer<S> = DefaultVec<ExtensionTypeParam<S>>;

pub type DefinitionTypeParam<S> = scaffold::generic::DefinitionTypeParam<Ident<S>, Type<S>>;

pub type WherePredicate<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
> = scaffold::generic::WherePredicate<
  Ident<S>,
  Type<S>,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
>;

pub type WhereClause<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
> = scaffold::generic::WhereClause<
  Ident<S>,
  Type<S>,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
>;

pub type ExtensionTypeParam<S> = scaffold::generic::ExtensionTypeParam<Ident<S>>;

pub type ExtensionTypeGenerics<
  S,
  Container = DefaultVec<ExtensionTypeParam<S>>,
> = scaffold::generic::ExtensionTypeGenerics<Ident<S>, Container>;

pub type ExtensionName<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeParamContainer = DefaultVec<ExtensionTypeParam<S>>,
> = scaffold::generic::ExtensionName<Ident<S>, IdentsContainer, TypeParamContainer>;
pub type DefinitionName<S, ParamsContainer = DefaultDefinitionTypeParamsContainer<S>> =
  scaffold::generic::DefinitionName<Ident<S>, Type<S>, ParamsContainer>;
pub type ExecutableDefinitionName<S, IdentsContainer = DefaultIdentsContainer<S>> =
  scaffold::generic::ExecutableDefinitionName<Ident<S>, IdentsContainer>;
pub type ExecutableDefinitionTypeGenerics<S, IdentsContainer = DefaultIdentsContainer<S>> =
  scaffold::generic::ExecutableDefinitionTypeGenerics<Ident<S>, IdentsContainer>;

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
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
> = DefaultVec<TypePath<S, IdentsContainer, TypeContainer>>;
/// The default where predicates container type used in the AST.
pub type DefaultWherePredicatesContainer<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
> = DefaultVec<WherePredicate<S, IdentsContainer, TypeContainer, TypePathsContainer>>;

/// The default container type used for locations in the AST.
pub type DefaultLocationsContainer = DefaultVec<Location>;
/// The default container type used for root operation types in the AST.
pub type DefaultRootOperationTypesContainer<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
> = DefaultVec<RootOperationTypeDefinition<S, IdentsContainer, TypeContainer>>;
/// The default container type used for variable definitions in the AST.
pub type DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer> =
  Vec<DescribedVariableDefinition<S, ArgumentsContainer, DirectivesContainer>>;
/// The default container type used for fields in the AST.
pub type DefaultFieldsContainer<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer> =
  Vec<FieldDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>>;

pub type FragmentSpread<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentContainer = Vec<Argument<S>>,
  Container = Vec<Directive<S, ArgumentContainer>>,
> = scaffold::FragmentSpread<
  FragmentTypePath<S, IdentsContainer, TypeContainer>,
  Directives<S, ArgumentContainer, Container>,
>;

pub type InlineFragment<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = scaffold::InlineFragment<
  TypeCondition<S, IdentsContainer, TypeContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  SelectionSet<S, IdentsContainer, TypeContainer, ArgumentContainer, DirectiveContainer>,
>;

pub type Selection<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::StandardSelection<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S, IdentsContainer, TypeContainer>,
  TypeCondition<S, IdentsContainer, TypeContainer>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type SelectionSet<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::StandardSelectionSet<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S, IdentsContainer, TypeContainer>,
  TypeCondition<S, IdentsContainer, TypeContainer>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type Alias<S> = scaffold::Alias<Ident<S>>;

pub type Field<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::StandardField<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S, IdentsContainer, TypeContainer>,
  TypeCondition<S, IdentsContainer, TypeContainer>,
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

pub type DescribedVariableDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  scaffold::VariableDefinition<
    VariableValue<S>,
    Type<S>,
    DefaultInputValue<S>,
    Directives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  S,
>;

pub type VariablesDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  VariablesContainer = DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::VariablesDefinition<
  DescribedVariableDefinition<S, ArgumentsContainer, DirectivesContainer>,
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
  S,
>;

pub type InputFieldsDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
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
  IdentsContainer,
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
  S,
>;

pub type FieldsDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
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
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
>;

pub type InputObjectTypeDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InputObjectTypeDefinition<
  DefinitionName<S, ParametersContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<
    S,
    IdentsContainer,
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
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InputObjectTypeDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  S,
>;

pub type InputObjectTypeExtension<
  S,
  ParamsContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InputObjectTypeExtension<
  ExtensionName<S, IdentsContainer, ParamsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
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
> = Described<ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>, S>;

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
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::ObjectTypeDefinition<
  DefinitionName<S, ParamsContainer>,
  ImplementInterfaces<TypePath<S, IdentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    IdentsContainer,
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
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  ObjectTypeDefinition<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  S,
>;

pub type ObjectTypeExtension<
  S,
  ParamsContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::ObjectTypeExtension<
  ExtensionName<S, IdentsContainer, ParamsContainer>,
  ImplementInterfaces<TypePath<S, IdentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    IdentsContainer,
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
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InterfaceTypeDefinition<
  DefinitionName<S, ParamsContainer>,
  ImplementInterfaces<TypePath<S, IdentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    IdentsContainer,
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
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InterfaceTypeDefinition<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  S,
>;

pub type InterfaceTypeExtension<
  S,
  ParamsContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::InterfaceTypeExtension<
  ExtensionName<S, IdentsContainer, ParamsContainer>,
  ImplementInterfaces<TypePath<S, IdentsContainer, TypeContainer>, TypePathsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<
    S,
    IdentsContainer,
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
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
> = scaffold::generic::Constrained<
  Ident<S>,
  Type<S>,
  scaffold::UnionMemberTypes<TypePath<S, IdentsContainer, TypeContainer>, TypePathsContainer>,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
>;

pub type UnionTypeDefinition<
  S,
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::UnionTypeDefinition<
  DefinitionName<S, ParamsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<S, IdentsContainer, TypeContainer, TypePathsContainer, PredicatesContainer>,
>;

pub type DescribedUnionTypeDefinition<
  S,
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  UnionTypeDefinition<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
  S,
>;

pub type UnionTypeExtension<
  S,
  ParamsContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::UnionTypeExtension<
  ExtensionName<S, IdentsContainer, ParamsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<S, IdentsContainer, TypeContainer, TypePathsContainer, PredicatesContainer>,
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
  S,
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
> =
  Described<EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>, S>;

pub type EnumTypeExtension<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::EnumTypeExtension<
  Path<Ident<S>, IdentsContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
>;

pub type NamedOperationDefinition<
  S,
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::NamedOperationDefinition<
  DefinitionName<S, ParamsContainer>,
  OperationType,
  VariablesDefinition<S>,
  Directives<S>,
  SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>,
>;

pub type OperationDefinition<
  S,
  ParamsContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::OperationDefinition<
  DefinitionName<S, ParamsContainer>,
  OperationType,
  VariablesDefinition<S, ArgumentsContainer, DirectivesContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
  SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>,
>;

pub type RootOperationTypeDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
> =
  scaffold::RootOperationTypeDefinition<TypePath<S, IdentsContainer, TypeContainer>, OperationType>;

pub type RootOperationTypesDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  Container = DefaultRootOperationTypesContainer<S, IdentsContainer, TypeContainer>,
> = scaffold::RootOperationTypesDefinition<
  RootOperationTypeDefinition<S, IdentsContainer, TypeContainer>,
  Container,
>;

pub type SchemaDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  Container = DefaultRootOperationTypesContainer<S, IdentsContainer, TypeContainer>,
> = scaffold::SchemaDefinition<
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesDefinition<S, IdentsContainer, TypeContainer, Container>,
>;

pub type DescribedSchemaDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  Container = DefaultRootOperationTypesContainer<S, IdentsContainer, TypeContainer>,
> = Described<
  SchemaDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    ArgumentsContainer,
    DirectivesContainer,
    Container,
  >,
  S,
>;

pub type SchemaExtension<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  Container = DefaultRootOperationTypesContainer<S, IdentsContainer, TypeContainer>,
> = scaffold::SchemaExtension<
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesDefinition<S, IdentsContainer, TypeContainer, Container>,
>;

pub type TypeDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::TypeDefinition<
  ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>,
  ObjectTypeDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  InterfaceTypeDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  UnionTypeDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
  EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
  InputObjectTypeDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type TypeSystemDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
> = scaffold::TypeSystemDefinition<
  TypeDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >,
  DirectiveDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >,
  SchemaDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    ArgumentsContainer,
    DirectivesContainer,
    RootOperationTypesContainer,
  >,
>;

pub type DescribedTypeSystemDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
> = Described<
  TypeSystemDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  S,
>;

pub type TypeExtension<
  S,
  ParamsContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = scaffold::TypeExtension<
  ScalarTypeExtension<S, ArgumentsContainer, DirectivesContainer>,
  ObjectTypeExtension<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  InterfaceTypeExtension<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  UnionTypeExtension<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
  EnumTypeExtension<S, IdentsContainer, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
  InputObjectTypeExtension<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
>;

pub type TypeSystemExtension<
  S,
  ParamsContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
> = scaffold::TypeSystemExtension<
  TypeExtension<
    S,
    ParamsContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >,
  SchemaExtension<
    S,
    IdentsContainer,
    TypeContainer,
    ArgumentsContainer,
    DirectivesContainer,
    RootOperationTypesContainer,
  >,
>;

pub type TypeSystemDefinitionOrExtension<
  S,
  DefinitionParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  ExtensionParametersContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
> = scaffold::TypeSystemDefinitionOrExtension<
  DescribedTypeSystemDefinition<
    S,
    DefinitionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  TypeSystemExtension<
    S,
    ExtensionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >,
>;

pub type ExecutableDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::ExecutableDefinition<
  OperationDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
  FragmentDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
>;

pub type DescribedExecutableDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  ExecutableDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
  S,
>;

pub type Definition<
  S,
  DefinitionParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
> = scaffold::Definition<
  TypeSystemDefinition<
    S,
    DefinitionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  ExecutableDefinition<
    S,
    DefinitionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
>;

pub type DescribedDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  EnumValuesContainer = DefaultEnumValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
> = Described<
  Definition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  S,
>;

pub type DefinitionOrExtension<
  S,
  DefinitionParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  ExtensionParametersContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  EnumValuesContainer = DefaultEnumValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
> = scaffold::DefinitionOrExtension<
  DescribedDefinition<
    S,
    DefinitionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  TypeSystemExtension<
    S,
    ExtensionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >,
>;

pub type TypeSystemDocument<
  S,
  DefinitionParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  ExtensionParametersContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
  DefinitionContainer = DefaultVec<
    TypeSystemDefinitionOrExtension<
      S,
      DefinitionParametersContainer,
      ExtensionParametersContainer,
      IdentsContainer,
      TypeContainer,
      TypePathsContainer,
      PredicatesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
> = scaffold::Document<
  TypeSystemDefinitionOrExtension<
    S,
    DefinitionParametersContainer,
    ExtensionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  DefinitionContainer,
>;

pub type ExecutableDocument<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  DefinitionContainer = DefaultVec<
    DescribedExecutableDefinition<
      S,
      ParametersContainer,
      IdentsContainer,
      TypeContainer,
      TypePathsContainer,
      PredicatesContainer,
      ArgumentsContainer,
      DirectivesContainer,
    >,
  >,
> = scaffold::Document<
  DescribedExecutableDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
  DefinitionContainer,
>;

pub type Document<
  S,
  DefinitionParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  ExtensionParametersContainer = DefaultExtensionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  EnumValuesContainer = DefaultEnumValuesContainer<
    S,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
  >,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<
    S,
    IdentsContainer,
    TypeContainer,
  >,
  DefinitionContainer = DefaultVec<
    DefinitionOrExtension<
      S,
      DefinitionParametersContainer,
      ExtensionParametersContainer,
      IdentsContainer,
      TypeContainer,
      TypePathsContainer,
      PredicatesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      ConstArgumentsContainer,
      ConstDirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
> = scaffold::Document<
  DefinitionOrExtension<
    S,
    DefinitionParametersContainer,
    ExtensionParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  DefinitionContainer,
>;
