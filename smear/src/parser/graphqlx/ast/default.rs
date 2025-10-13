use super::*;

use crate::{
  parser::ident::Ident,
  scaffold::{self, OperationType, Path},
};

pub use directive::*;
pub use field::*;
pub use fragment_definition::*;
pub use input_object_type::*;
pub use interface_type::*;
pub use object_type::*;
pub use operation_definition::*;
pub use union_type::*;

mod directive;
mod fragment_definition;

mod field;
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

pub type DefinitionTypeParam<S> = scaffold::generic::DefinitionTypeParam<Ident<S>, Type<S>>;

pub type WherePredicate<S> = scaffold::generic::WherePredicate<Ident<S>, Type<S>>;

pub type WhereClause<S> = scaffold::generic::WhereClause<Ident<S>, Type<S>>;

pub type ExtensionTypeParam<S> = scaffold::generic::ExtensionTypeParam<Ident<S>>;

pub type ExtensionTypeGenerics<S> = scaffold::generic::ExtensionTypeGenerics<Ident<S>>;
pub type DefinitionTypeGenerics<S> = scaffold::generic::DefinitionTypeGenerics<Ident<S>, Type<S>>;
pub type ExecutableDefinitionTypeGenerics<S> =
  scaffold::generic::ExecutableDefinitionTypeGenerics<Ident<S>>;

pub type ImplementInterfaces<S> = scaffold::ImplementInterfaces<TypePath<S>>;

pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;
pub type Argument<S> = scaffold::Argument<Ident<S>, InputValue<S>>;
pub type Arguments<S> = scaffold::Arguments<Argument<S>>;

pub type ConstArgument<S> = scaffold::Argument<Ident<S>, ConstInputValue<S>>;
pub type ConstArguments<S> = scaffold::Arguments<ConstArgument<S>>;

pub type Directives<S> = scaffold::Directives<Directive<S>>;
pub type ConstDirectives<S> = scaffold::Directives<ConstDirective<S>>;

pub type Alias<S> = scaffold::Alias<Ident<S>>;

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
