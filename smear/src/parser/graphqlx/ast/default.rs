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
type DefinitionName<S, Ty = Type<S>> = scaffold::generic::DefinitionName<Ident<S>, Ty>;
type ExecutableDefinitionName<S> = scaffold::generic::ExecutableDefinitionName<Ident<S>>;

pub type Described<T, S> = scaffold::Described<T, StringValue<S>>;

pub type DefinitionTypeParam<S, Ty = Type<S>> =
  scaffold::generic::DefinitionTypeParam<Ident<S>, Ty>;

pub type WherePredicate<S, Ty = Type<S>> = scaffold::generic::WherePredicate<Ident<S>, Ty>;

pub type WhereClause<S, Ty = Type<S>> = scaffold::generic::WhereClause<Ident<S>, Ty>;

pub type ExtensionTypeParam<S> = scaffold::generic::ExtensionTypeParam<Ident<S>>;

pub type ExtensionTypeGenerics<S> = scaffold::generic::ExtensionTypeGenerics<Ident<S>>;
pub type DefinitionTypeGenerics<S, Ty = Type<S>> =
  scaffold::generic::DefinitionTypeGenerics<Ident<S>, Ty>;
pub type ExecutableDefinitionTypeGenerics<S> =
  scaffold::generic::ExecutableDefinitionTypeGenerics<Ident<S>>;

pub type ImplementInterfaces<S, Ty = Type<S>> = scaffold::ImplementInterfaces<TypePath<S, Ty>>;

pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;
pub type Argument<S> = scaffold::Argument<Ident<S>, InputValue<S>>;
pub type Arguments<S> = scaffold::Arguments<Argument<S>>;

pub type ConstArgument<S> = scaffold::Argument<Ident<S>, ConstInputValue<S>>;
pub type ConstArguments<S> = scaffold::Arguments<ConstArgument<S>>;

pub type Directives<S, Ty = Type<S>> = scaffold::Directives<Directive<S, Ty>>;
pub type ConstDirectives<S, Ty = Type<S>> = scaffold::Directives<ConstDirective<S, Ty>>;

pub type Alias<S> = scaffold::Alias<Ident<S>>;

pub type ArgumentsDefinition<S, Ty = Type<S>> =
  scaffold::ArgumentsDefinition<InputValueDefinition<S, Ty>>;

pub type VariableDefinition<S, Ty = Type<S>> =
  scaffold::VariableDefinition<VariableValue<S>, Ty, DefaultInputValue<S>, Directives<S>>;

pub type DescribedVariableDefinition<S, Ty = Type<S>> = Described<
  scaffold::VariableDefinition<VariableValue<S>, Ty, DefaultInputValue<S>, Directives<S>>,
  S,
>;

pub type VariablesDefinition<S, Ty = Type<S>> =
  scaffold::VariablesDefinition<DescribedVariableDefinition<S, Ty>>;

pub type InputValueDefinition<S, Ty = Type<S>> = Described<
  scaffold::InputValueDefinition<Ident<S>, Ty, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

pub type InputFieldsDefinition<S, Ty = Type<S>> =
  scaffold::InputFieldsDefinition<InputValueDefinition<S, Ty>>;

pub type FieldDefinition<S, Ty = Type<S>> = Described<
  scaffold::FieldDefinition<Ident<S>, ArgumentsDefinition<S>, Ty, ConstDirectives<S, Ty>>,
  S,
>;

pub type FieldsDefinition<S, Ty = Type<S>> = scaffold::FieldsDefinition<FieldDefinition<S, Ty>>;

pub type ScalarTypeDefinition<S, Ty = Type<S>> =
  scaffold::ScalarTypeDefinition<Ident<S>, ConstDirectives<S, Ty>>;

pub type DescribedScalarTypeDefinition<S, Ty = Type<S>> = Described<ScalarTypeDefinition<S, Ty>, S>;

pub type ScalarTypeExtension<S, Ty = Type<S>> =
  scaffold::ScalarTypeExtension<Ident<S>, ConstDirectives<S, Ty>>;

pub type EnumValueDefinition<S, Ty = Type<S>> =
  Described<scaffold::EnumValueDefinition<Ident<S>, ConstDirectives<S, Ty>>, S>;

pub type EnumValuesDefinition<S, Ty = Type<S>> =
  scaffold::EnumValuesDefinition<EnumValueDefinition<S, Ty>>;

pub type EnumTypeDefinition<S, Ty = Type<S>> =
  scaffold::EnumTypeDefinition<Ident<S>, ConstDirectives<S, Ty>, EnumValuesDefinition<S, Ty>>;

pub type DescribedEnumTypeDefinition<S, Ty = Type<S>> = Described<EnumTypeDefinition<S, Ty>, S>;

pub type EnumTypeExtension<S, Ty = Type<S>> =
  scaffold::EnumTypeExtension<Path<Ident<S>>, ConstDirectives<S, Ty>, EnumValuesDefinition<S, Ty>>;

pub type UnionMemberTypes<S, Ty = Type<S>> = scaffold::UnionMemberTypes<TypePath<S, Ty>>;

pub type RootOperationTypesDefinition<S, Ty = Type<S>> =
  scaffold::RootOperationTypesDefinition<RootOperationTypeDefinition<S, Ty>>;

pub type SchemaDefinition<S, Ty = Type<S>> =
  scaffold::SchemaDefinition<ConstDirectives<S, Ty>, RootOperationTypesDefinition<S, Ty>>;

pub type DescribedSchemaDefinition<S, Ty = Type<S>> = Described<SchemaDefinition<S, Ty>, S>;

pub type SchemaExtension<S, Ty = Type<S>> =
  scaffold::SchemaExtension<ConstDirectives<S, Ty>, RootOperationTypesDefinition<S, Ty>>;

pub type TypeDefinition<S, Ty = Type<S>> = scaffold::TypeDefinition<
  ScalarTypeDefinition<S, Ty>,
  ObjectTypeDefinition<S, Ty>,
  InterfaceTypeDefinition<S, Ty>,
  UnionTypeDefinition<S, Ty>,
  EnumTypeDefinition<S, Ty>,
  InputObjectTypeDefinition<S, Ty>,
>;

pub type TypeSystemDefinition<S, Ty = Type<S>> = scaffold::TypeSystemDefinition<
  TypeDefinition<S, Ty>,
  DirectiveDefinition<S, Ty>,
  SchemaDefinition<S, Ty>,
>;

pub type DescribedTypeSystemDefinition<S, Ty = Type<S>> = Described<TypeSystemDefinition<S, Ty>, S>;

pub type TypeExtension<S, Ty = Type<S>> = scaffold::TypeExtension<
  ScalarTypeExtension<S, Ty>,
  ObjectTypeExtension<S, Ty>,
  InterfaceTypeExtension<S, Ty>,
  UnionTypeExtension<S, Ty>,
  EnumTypeExtension<S, Ty>,
  InputObjectTypeExtension<S, Ty>,
>;

pub type TypeSystemExtension<S, Ty = Type<S>> =
  scaffold::TypeSystemExtension<TypeExtension<S, Ty>, SchemaExtension<S, Ty>>;

pub type TypeSystemDefinitionOrExtension<S, Ty = Type<S>> =
  scaffold::TypeSystemDefinitionOrExtension<
    DescribedTypeSystemDefinition<S, Ty>,
    TypeSystemExtension<S, Ty>,
  >;

pub type ImportOrTypeSystemDefinitionOrExtension<S, Ty = Type<S>> =
  scaffold::ImportOrTypeSystemDefinitionOrExtension<
    ImportDefinition<S>,
    DescribedTypeSystemDefinition<S, Ty>,
    TypeSystemExtension<S, Ty>,
  >;

pub type OperationDefinition<S, Ty = Type<S>> =
  scaffold::OperationDefinition<NamedOperationDefinition<S, Ty>, SelectionSet<S, Ty>>;

pub type ExecutableDefinition<S, Ty = Type<S>> =
  scaffold::ExecutableDefinition<OperationDefinition<S, Ty>, FragmentDefinition<S, Ty>>;
pub type DescribedExecutableDefinition<S, Ty = Type<S>> = Described<ExecutableDefinition<S, Ty>, S>;

pub type ImportOrExecutableDefinition<S, Ty = Type<S>> =
  scaffold::ImportOrExecutableDefinition<ImportDefinition<S>, DescribedExecutableDefinition<S, Ty>>;

pub type Definition<S, Ty = Type<S>> =
  scaffold::Definition<TypeSystemDefinition<S, Ty>, ExecutableDefinition<S, Ty>>;

pub type DescribedDefinition<S, Ty = Type<S>> = Described<Definition<S, Ty>, S>;

pub type DefinitionOrExtension<S, Ty = Type<S>> =
  scaffold::DefinitionOrExtension<DescribedDefinition<S, Ty>, TypeSystemExtension<S, Ty>>;

pub type ImportOrDefinitionOrExtension<S, Ty = Type<S>> = scaffold::ImportOrDefinitionOrExtension<
  ImportDefinition<S>,
  DescribedDefinition<S, Ty>,
  TypeSystemExtension<S, Ty>,
>;

pub type TypeSystemDocument<S, Ty = Type<S>> =
  scaffold::Document<ImportOrTypeSystemDefinitionOrExtension<S, Ty>>;

pub type ExecutableDocument<S, Ty = Type<S>> =
  scaffold::Document<ImportOrExecutableDefinition<S, Ty>>;

pub type Document<S, Ty = Type<S>> = scaffold::Document<ImportOrDefinitionOrExtension<S, Ty>>;
