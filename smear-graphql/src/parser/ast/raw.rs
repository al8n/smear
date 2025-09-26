use super::{
  ConstInputValue, DefaultVec, FragmentName, InputValue, Name, StringValue, TypeCondition,
  Variable, field, selection_set,
};
use smear_parser::{
  definitions::{
    self,
    ast::{Described, Location, OperationType, Type},
  },
  lang,
};

pub use smear_parser::definitions::ast::{
  DirectiveLocations, ImplementInterfaces, UnionMemberTypes,
};

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

pub type DefaultInputValue<S> = smear_parser::lang::DefaultInputValue<ConstInputValue<S>>;
pub type Argument<S> = lang::Argument<Name<S>, InputValue<S>>;
pub type Arguments<S, Container = DefaultArgumentsContainer<S>> =
  lang::Arguments<Argument<S>, Container>;

pub type ConstArgument<S> = lang::Argument<Name<S>, ConstInputValue<S>>;
pub type ConstArguments<S, Container = DefaultConstArgumentsContainer<S>> =
  lang::Arguments<ConstArgument<S>, Container>;

pub type Directive<S, ArgumentsContainer = DefaultArgumentsContainer<S>> =
  lang::Directive<Name<S>, Arguments<S, ArgumentsContainer>>;
pub type Directives<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  Container = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = lang::Directives<Directive<S, ArgumentsContainer>, Container>;

pub type ConstDirective<S, ArgumentsContainer = DefaultConstArgumentsContainer<S>> =
  lang::Directive<Name<S>, ConstArguments<S, ArgumentsContainer>>;
pub type ConstDirectives<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  Container = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = lang::Directives<ConstDirective<S, ArgumentsContainer>, Container>;

/// The default container type used for input values in the AST.
pub type DefaultInputValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer> =
  Vec<InputValueDefinition<S, ConstArgumentsContainer, ConstDirectivesContainer>>;
/// The default container type used for enum values in the AST.
pub type DefaultEnumValuesContainer<S, ConstArgumentsContainer, ConstDirectivesContainer> =
  Vec<EnumValueDefinition<S, ConstArgumentsContainer, ConstDirectivesContainer>>;
/// The default container type used for names in the AST.
pub type DefaultNamesContainer<S> = DefaultVec<Name<S>>;
/// The default container type used for locations in the AST.
pub type DefaultLocationsContainer = DefaultVec<Location>;
/// The default container type used for root operation types in the AST.
pub type DefaultRootOperationTypesContainer<S> = DefaultVec<RootOperationTypeDefinition<S>>;
/// The default container type used for variable definitions in the AST.
pub type DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer> =
  Vec<VariableDefinition<S, ArgumentsContainer, DirectivesContainer>>;
/// The default container type used for fields in the AST.
pub type DefaultFieldsContainer<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer> =
  Vec<FieldDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>>;

pub type FragmentSpread<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  Container = Vec<Directive<S, ArgumentContainer>>,
> = lang::FragmentSpread<FragmentName<S>, Directives<S, ArgumentContainer, Container>>;

pub type InlineFragment<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = lang::InlineFragment<
  TypeCondition<S>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

pub type Selection<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = selection_set::Selection<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type SelectionSet<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = selection_set::SelectionSet<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type Alias<S> = lang::Alias<Name<S>>;

pub type Field<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = field::Field<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentsContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ArgumentsDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::ArgumentsDefinition<
  InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  InputValuesContainer,
>;

pub type DirectiveDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
> = definitions::ast::DirectiveDefinition<
  Name<S>,
  ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  DirectiveLocations<LocationsContainer>,
>;

pub type VariableDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = definitions::ast::VariableDefinition<
  Variable<S>,
  Type<Name<S>>,
  DefaultInputValue<S>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type VariablesDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  VariablesContainer = DefaultVariablesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::VariablesDefinition<
  VariableDefinition<S, ArgumentsContainer, DirectivesContainer>,
  VariablesContainer,
>;

pub type InputValueDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  definitions::ast::InputValueDefinition<
    Name<S>,
    Type<Name<S>>,
    DefaultInputValue<S>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type InputFieldsDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::InputFieldsDefinition<
  InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  InputValuesContainer,
>;

pub type FieldDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  definitions::ast::FieldDefinition<
    Name<S>,
    ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
    Type<Name<S>>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type FieldsDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  FieldsContainer = DefaultFieldsContainer<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
> = definitions::ast::FieldsDefinition<
  FieldDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  FieldsContainer,
>;

pub type InputObjectTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::InputObjectTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type DescribedInputObjectTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InputObjectTypeDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  StringValue<S>,
>;

pub type InputObjectTypeExtension<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::InputObjectTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  InputFieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type FragmentDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = definitions::ast::FragmentDefinition<
  FragmentName<S>,
  TypeCondition<S>,
  Directives<S, ArgumentsContainer>,
  SelectionSet<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ScalarTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = definitions::ast::ScalarTypeDefinition<
  Name<S>,
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
> = definitions::ast::ScalarTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type ObjectTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::ObjectTypeDefinition<
  Name<S>,
  ImplementInterfaces<Name<S>, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type DescribedObjectTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  ObjectTypeDefinition<
    S,
    ImplementInterfacesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  StringValue<S>,
>;

pub type ObjectTypeExtension<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type InterfaceTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::InterfaceTypeDefinition<
  Name<S>,
  ImplementInterfaces<Name<S>, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type DescribedInterfaceTypeDefinition<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = Described<
  InterfaceTypeDefinition<
    S,
    ImplementInterfacesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  StringValue<S>,
>;

pub type InterfaceTypeExtension<
  S,
  ImplementInterfacesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>, ImplementInterfacesContainer>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  FieldsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type UnionTypeDefinition<
  S,
  UnionMemberTypesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = definitions::ast::UnionTypeDefinition<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<Name<S>, UnionMemberTypesContainer>,
>;

pub type DescribedUnionTypeDefinition<
  S,
  UnionMemberTypesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  UnionTypeDefinition<S, UnionMemberTypesContainer, ArgumentsContainer, DirectivesContainer>,
  StringValue<S>,
>;

pub type UnionTypeExtension<
  S,
  UnionMemberTypesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = definitions::ast::UnionTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  UnionMemberTypes<Name<S>, UnionMemberTypesContainer>,
>;

pub type EnumValueDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
> = Described<
  definitions::ast::EnumValueDefinition<
    Name<S>,
    ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  >,
  StringValue<S>,
>;

pub type EnumValuesDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::EnumValuesDefinition<
  EnumValueDefinition<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer,
>;

pub type EnumTypeDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::EnumTypeDefinition<
  Name<S>,
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
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::EnumTypeExtension<
  Name<S>,
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
>;

pub type NamedOperationDefinition<S> = definitions::ast::NamedOperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S>,
  Directives<S>,
  SelectionSet<S>,
>;

pub type OperationDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = definitions::ast::OperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S, ArgumentsContainer, DirectivesContainer>,
  Directives<S, ArgumentsContainer, DirectivesContainer>,
  SelectionSet<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type RootOperationTypeDefinition<S> =
  definitions::ast::RootOperationTypeDefinition<Name<S>, OperationType>;

pub type RootOperationTypesDefinition<S, Container = DefaultRootOperationTypesContainer<S>> =
  definitions::ast::RootOperationTypesDefinition<RootOperationTypeDefinition<S>, Container>;

pub type SchemaDefinition<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  Container = DefaultRootOperationTypesContainer<S>,
> = definitions::ast::SchemaDefinition<
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesDefinition<S, Container>,
>;

pub type SchemaExtension<
  S,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  Container = DefaultRootOperationTypesContainer<S>,
> = definitions::ast::SchemaExtension<
  ConstDirectives<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesDefinition<S, Container>,
>;

pub type TypeDefinition<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::TypeDefinition<
  ScalarTypeDefinition<S, ArgumentsContainer, DirectivesContainer>,
  ObjectTypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  InterfaceTypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  UnionTypeDefinition<S, NamesContainer, ArgumentsContainer, DirectivesContainer>,
  EnumTypeDefinition<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
  InputObjectTypeDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type TypeSystemDefinition<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> = definitions::ast::TypeSystemDefinition<
  TypeDefinition<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >,
  DirectiveDefinition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >,
  SchemaDefinition<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>,
>;

pub type TypeExtension<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
> = definitions::ast::TypeExtension<
  ScalarTypeExtension<S, ArgumentsContainer, DirectivesContainer>,
  ObjectTypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  InterfaceTypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
  >,
  UnionTypeExtension<S, NamesContainer, ArgumentsContainer, DirectivesContainer>,
  EnumTypeExtension<S, ArgumentsContainer, DirectivesContainer, EnumValuesContainer>,
  InputObjectTypeExtension<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
>;

pub type TypeSystemExtension<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> = definitions::ast::TypeSystemExtension<
  TypeExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
  >,
  SchemaExtension<S, ArgumentsContainer, DirectivesContainer, RootOperationTypesContainer>,
>;

pub type TypeSystemDefinitionOrExtension<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> = definitions::ast::TypeSystemDefinitionOrExtension<
  Described<
    TypeSystemDefinition<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
    StringValue<S>,
  >,
  TypeSystemExtension<
    S,
    NamesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    RootOperationTypesContainer,
  >,
>;

pub type ExecutableDefinition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = definitions::ast::ExecutableDefinition<
  OperationDefinition<S, ArgumentsContainer, DirectivesContainer>,
  FragmentDefinition<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type Definition<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  NamesContainer = DefaultNamesContainer<S>,
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
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
> = definitions::ast::Definition<
  TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>,
>;

pub type TypeSystemDocument<
  S,
  NamesContainer = DefaultNamesContainer<S>,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  EnumValuesContainer = DefaultEnumValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
  DefinitionContainer = DefaultVec<
    TypeSystemDefinitionOrExtension<
      S,
      NamesContainer,
      ArgumentsContainer,
      DirectivesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
> = definitions::ast::Document<
  TypeSystemDefinitionOrExtension<
    S,
    NamesContainer,
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
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  DefinitionContainer = DefaultVec<
    ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>,
  >,
> = definitions::ast::Document<
  ExecutableDefinition<S, ArgumentsContainer, DirectivesContainer>,
  DefinitionContainer,
>;

pub type Document<
  S,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
  ConstArgumentsContainer = DefaultConstArgumentsContainer<S>,
  ConstDirectivesContainer = DefaultConstDirectivesContainer<S, ConstArgumentsContainer>,
  NamesContainer = DefaultNamesContainer<S>,
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
  RootOperationTypesContainer = DefaultRootOperationTypesContainer<S>,
  DefinitionContainer = DefaultVec<
    Definition<
      S,
      ArgumentsContainer,
      DirectivesContainer,
      ConstArgumentsContainer,
      ConstDirectivesContainer,
      NamesContainer,
      InputValuesContainer,
      EnumValuesContainer,
      LocationsContainer,
      RootOperationTypesContainer,
    >,
  >,
> = definitions::ast::Document<
  Definition<
    S,
    ArgumentsContainer,
    DirectivesContainer,
    ConstArgumentsContainer,
    ConstDirectivesContainer,
    NamesContainer,
    InputValuesContainer,
    EnumValuesContainer,
    LocationsContainer,
    RootOperationTypesContainer,
  >,
  DefinitionContainer,
>;
