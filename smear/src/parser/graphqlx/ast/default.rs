use super::*;
use crate::{
  parser::ident::Ident,
  scaffold::{self, Location, OperationType, Path},
};

pub use directive::*;
pub use fragment::*;
pub use input_object_type::*;
pub use interface_type::*;
pub use object_type::*;
pub use operation_definition::*;
pub use union_type::*;

mod directive;
mod fragment;

mod fields_definition;
mod input_fields_definition;
mod input_object_type;
mod interface_type;
mod object_type;
mod operation_definition;
mod union_type;

type ExtensionName<S> = scaffold::generic::ExtensionName<Ident<S>>;
type DefinitionName<S> = scaffold::generic::DefinitionName<Ident<S>, Type<S>>;
type ExecutableDefinitionName<S> = scaffold::generic::ExecutableDefinitionName<Ident<S>>;

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

pub type WherePredicate<S> = scaffold::generic::WherePredicate<Ident<S>, Type<S>>;

pub type WhereClause<S> = scaffold::generic::WhereClause<Ident<S>, Type<S>>;

pub type ExtensionTypeParam<S> = scaffold::generic::ExtensionTypeParam<Ident<S>>;

pub type ExtensionTypeGenerics<S> = scaffold::generic::ExtensionTypeGenerics<Ident<S>>;
pub type DefinitionTypeGenerics<S> = scaffold::generic::DefinitionTypeGenerics<Ident<S>, Type<S>>;
pub type ExecutableDefinitionTypeGenerics<S> =
  scaffold::generic::ExecutableDefinitionTypeGenerics<Ident<S>>;

pub type ImplementInterfaces<S> = scaffold::ImplementInterfaces<TypePath<S>>;

/// The default container type used for arguments in the AST.
pub type DefaultArgumentsContainer<S> = DefaultVec<Argument<S>>;
/// The default container type used for directives in the AST.
pub type DefaultDirectivesContainer<S> = DefaultVec<Directive<S>>;
/// The default container type used for const arguments in the AST.
pub type DefaultConstArgumentsContainer<S> = DefaultVec<ConstArgument<S>>;
/// The default container type used for const directives in the AST.
pub type DefaultConstDirectivesContainer<S> = Vec<ConstDirective<S>>;

pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;
pub type Argument<S> = scaffold::Argument<Ident<S>, InputValue<S>>;
pub type Arguments<S> = scaffold::Arguments<Argument<S>>;

pub type ConstArgument<S> = scaffold::Argument<Ident<S>, ConstInputValue<S>>;
pub type ConstArguments<S> = scaffold::Arguments<ConstArgument<S>>;

pub type Directive<S> = scaffold::Directive<Ident<S>, Arguments<S>>;
pub type Directives<S> = scaffold::Directives<Directive<S>>;

pub type ConstDirective<S> = scaffold::Directive<Ident<S>, ConstArguments<S>>;
pub type ConstDirectives<S> = scaffold::Directives<ConstDirective<S>>;

/// The default container type used for input values in the AST.
pub type DefaultInputValuesContainer<S> = Vec<InputValueDefinition<S>>;
/// The default container type used for enum values in the AST.
pub type DefaultEnumValuesContainer<S> = Vec<EnumValueDefinition<S>>;
/// The default container type used for type paths in the AST.
pub type DefaultTypePathsContainer<S> = DefaultVec<TypePath<S>>;
/// The default where predicates container type used in the AST.
pub type DefaultWherePredicatesContainer<S> = DefaultVec<WherePredicate<S>>;

/// The default container type used for locations in the AST.
pub type DefaultLocationsContainer = DefaultVec<Location>;
/// The default container type used for root operation types in the AST.
pub type DefaultRootOperationTypesContainer<S> = DefaultVec<RootOperationTypeDefinition<S>>;
/// The default container type used for variable definitions in the AST.
pub type DefaultVariablesContainer<S> = Vec<DescribedVariableDefinition<S>>;
/// The default container type used for fields in the AST.
pub type DefaultFieldsContainer<S> = Vec<FieldDefinition<S>>;

pub type FragmentSpread<S> = scaffold::FragmentSpread<FragmentTypePath<S>, Directives<S>>;

pub type InlineFragment<S> =
  scaffold::InlineFragment<TypeCondition<S>, Directives<S>, SelectionSet<S>>;

pub type Selection<S> = scaffold::StandardSelection<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S>,
  TypeCondition<S>,
  Arguments<S>,
  Directives<S>,
>;

pub type SelectionSet<S> = scaffold::StandardSelectionSet<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S>,
  TypeCondition<S>,
  Arguments<S>,
  Directives<S>,
>;

pub type Alias<S> = scaffold::Alias<Ident<S>>;

pub type Field<S> = scaffold::StandardField<
  Alias<S>,
  Ident<S>,
  FragmentTypePath<S>,
  TypeCondition<S>,
  Arguments<S>,
  Directives<S>,
>;

pub type ArgumentsDefinition<S> = scaffold::ArgumentsDefinition<InputValueDefinition<S>>;

pub type VariableDefinition<S> =
  scaffold::VariableDefinition<VariableValue<S>, Type<S>, DefaultInputValue<S>, Directives<S>>;

pub type DescribedVariableDefinition<S> = Described<
  scaffold::VariableDefinition<VariableValue<S>, Type<S>, DefaultInputValue<S>, Directives<S>>,
  S,
>;

pub type VariablesDefinition<S> = scaffold::VariablesDefinition<DescribedVariableDefinition<S>>;

pub type InputValueDefinition<S> = Described<
  scaffold::InputValueDefinition<Ident<S>, Type<S>, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

pub type InputFieldsDefinition<S> = scaffold::InputFieldsDefinition<InputValueDefinition<S>>;

pub type FieldDefinition<S> = Described<
  scaffold::FieldDefinition<Ident<S>, ArgumentsDefinition<S>, Type<S>, ConstDirectives<S>>,
  S,
>;

pub type FieldsDefinition<S> = scaffold::FieldsDefinition<FieldDefinition<S>>;

pub type ScalarTypeDefinition<S> = scaffold::ScalarTypeDefinition<Ident<S>, ConstDirectives<S>>;

pub type DescribedScalarTypeDefinition<S> = Described<ScalarTypeDefinition<S>, S>;

pub type ScalarTypeExtension<S> = scaffold::ScalarTypeExtension<Ident<S>, ConstDirectives<S>>;

pub type EnumValueDefinition<S> =
  Described<scaffold::EnumValueDefinition<Ident<S>, ConstDirectives<S>>, S>;

pub type EnumValuesDefinition<S> = scaffold::EnumValuesDefinition<EnumValueDefinition<S>>;

pub type EnumTypeDefinition<S> =
  scaffold::EnumTypeDefinition<Ident<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

pub type DescribedEnumTypeDefinition<S> = Described<EnumTypeDefinition<S>, S>;

pub type EnumTypeExtension<S> =
  scaffold::EnumTypeExtension<Path<Ident<S>>, ConstDirectives<S>, EnumValuesDefinition<S>>;

pub type UnionMemberTypes<S> = scaffold::UnionMemberTypes<TypePath<S>>;

pub type RootOperationTypeDefinition<S> =
  scaffold::RootOperationTypeDefinition<TypePath<S>, OperationType>;

pub type RootOperationTypesDefinition<S> =
  scaffold::RootOperationTypesDefinition<RootOperationTypeDefinition<S>>;

pub type SchemaDefinition<S> =
  scaffold::SchemaDefinition<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

pub type DescribedSchemaDefinition<S> = Described<SchemaDefinition<S>, S>;

pub type SchemaExtension<S> =
  scaffold::SchemaExtension<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

pub type TypeDefinition<S> = scaffold::TypeDefinition<
  ScalarTypeDefinition<S>,
  ObjectTypeDefinition<S>,
  InterfaceTypeDefinition<S>,
  UnionTypeDefinition<S>,
  EnumTypeDefinition<S>,
  InputObjectTypeDefinition<S>,
>;

pub type TypeSystemDefinition<S> =
  scaffold::TypeSystemDefinition<TypeDefinition<S>, DirectiveDefinition<S>, SchemaDefinition<S>>;

pub type DescribedTypeSystemDefinition<S> = Described<TypeSystemDefinition<S>, S>;

pub type TypeExtension<S> = scaffold::TypeExtension<
  ScalarTypeExtension<S>,
  ObjectTypeExtension<S>,
  InterfaceTypeExtension<S>,
  UnionTypeExtension<S>,
  EnumTypeExtension<S>,
  InputObjectTypeExtension<S>,
>;

pub type TypeSystemExtension<S> =
  scaffold::TypeSystemExtension<TypeExtension<S>, SchemaExtension<S>>;

pub type TypeSystemDefinitionOrExtension<S> = scaffold::TypeSystemDefinitionOrExtension<
  DescribedTypeSystemDefinition<S>,
  TypeSystemExtension<S>,
>;

pub type OperationDefinition<S> =
  scaffold::OperationDefinition<NamedOperationDefinition<S>, SelectionSet<S>>;

pub type ExecutableDefinition<S> =
  scaffold::ExecutableDefinition<OperationDefinition<S>, FragmentDefinition<S>>;

pub type DescribedExecutableDefinition<S> = Described<ExecutableDefinition<S>, S>;

pub type Definition<S> = scaffold::Definition<TypeSystemDefinition<S>, ExecutableDefinition<S>>;

pub type DescribedDefinition<S> = Described<Definition<S>, S>;

pub type DefinitionOrExtension<S> =
  scaffold::DefinitionOrExtension<DescribedDefinition<S>, TypeSystemExtension<S>>;

pub type TypeSystemDocument<S> = scaffold::Document<TypeSystemDefinitionOrExtension<S>>;

pub type ExecutableDocument<S> = scaffold::Document<DescribedExecutableDefinition<S>>;

pub type Document<S> = scaffold::Document<DefinitionOrExtension<S>>;
