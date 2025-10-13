use super::*;
use crate::scaffold::{
  self, DirectiveLocations, ImplementInterfaces, Location, OperationType, UnionMemberTypes,
};

pub use field::*;

mod field;

pub use scaffold::FragmentName;

pub type Described<T, S> = scaffold::Described<T, StringValue<S>>;

pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;
pub type Argument<S> = scaffold::Argument<Name<S>, InputValue<S>>;
pub type Arguments<S> = scaffold::Arguments<Argument<S>>;

pub type ConstArgument<S> = scaffold::Argument<Name<S>, ConstInputValue<S>>;
pub type ConstArguments<S> = scaffold::Arguments<ConstArgument<S>>;

pub type Directive<S> = scaffold::Directive<Name<S>, Arguments<S>>;
pub type Directives<S> = scaffold::Directives<Directive<S>>;

pub type ConstDirective<S> = scaffold::Directive<Name<S>, ConstArguments<S>>;
pub type ConstDirectives<S> = scaffold::Directives<ConstDirective<S>>;

pub type Alias<S> = scaffold::Alias<Name<S>>;

pub type ArgumentsDefinition<S> = scaffold::ArgumentsDefinition<InputValueDefinition<S>>;

pub type DirectiveDefinition<S> =
  scaffold::DirectiveDefinition<Name<S>, ArgumentsDefinition<S>, DirectiveLocations<Location>>;

pub type VariableDefinition<S> = scaffold::VariableDefinition<
  VariableValue<S>,
  Type<Name<S>>,
  DefaultInputValue<S>,
  Directives<S>,
>;

pub type DescribedVariableDefinition<S> = Described<VariableDefinition<S>, S>;

pub type VariablesDefinition<S> = scaffold::VariablesDefinition<DescribedVariableDefinition<S>>;

pub type InputValueDefinition<S> = Described<
  scaffold::InputValueDefinition<Name<S>, Type<Name<S>>, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

pub type InputFieldsDefinition<S> = scaffold::InputFieldsDefinition<InputValueDefinition<S>>;

pub type FieldDefinition<S> = Described<
  scaffold::FieldDefinition<Name<S>, ArgumentsDefinition<S>, Type<Name<S>>, ConstDirectives<S>>,
  S,
>;

pub type FieldsDefinition<S> = scaffold::FieldsDefinition<FieldDefinition<S>>;

pub type InputObjectTypeDefinition<S> =
  scaffold::InputObjectTypeDefinition<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S>>;

pub type DescribedInputObjectTypeDefinition<S> = Described<InputObjectTypeDefinition<S>, S>;

pub type InputObjectTypeExtension<S> =
  scaffold::InputObjectTypeExtension<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S>>;

pub type FragmentDefinition<S> =
  scaffold::FragmentDefinition<FragmentName<S>, TypeCondition<S>, Directives<S>, SelectionSet<S>>;

pub type ScalarTypeDefinition<S> = scaffold::ScalarTypeDefinition<Name<S>, ConstDirectives<S>>;

pub type DescribedScalarTypeDefinition<S> = Described<ScalarTypeDefinition<S>, S>;

pub type ScalarTypeExtension<S> = scaffold::ScalarTypeExtension<Name<S>, ConstDirectives<S>>;

pub type ObjectTypeDefinition<S> = scaffold::ObjectTypeDefinition<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

pub type DescribedObjectTypeDefinition<S> = Described<ObjectTypeDefinition<S>, S>;

pub type ObjectTypeExtension<S> = scaffold::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

pub type InterfaceTypeDefinition<S> = scaffold::InterfaceTypeDefinition<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

pub type DescribedInterfaceTypeDefinition<S> = Described<InterfaceTypeDefinition<S>, S>;

pub type InterfaceTypeExtension<S> = scaffold::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

pub type UnionTypeDefinition<S> =
  scaffold::UnionTypeDefinition<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

pub type DescribedUnionTypeDefinition<S> = Described<UnionTypeDefinition<S>, S>;

pub type UnionTypeExtension<S> =
  scaffold::UnionTypeExtension<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

pub type EnumValueDefinition<S> =
  Described<scaffold::EnumValueDefinition<Name<S>, ConstDirectives<S>>, S>;

pub type EnumValuesDefinition<S> = scaffold::EnumValuesDefinition<EnumValueDefinition<S>>;

pub type EnumTypeDefinition<S> =
  scaffold::EnumTypeDefinition<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

pub type DescribedEnumTypeDefinition<S> = Described<EnumTypeDefinition<S>, S>;

pub type EnumTypeExtension<S> =
  scaffold::EnumTypeExtension<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

pub type NamedOperationDefinition<S> = scaffold::NamedOperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S>,
  Directives<S>,
  SelectionSet<S>,
>;

pub type OperationDefinition<S> =
  scaffold::OperationDefinition<NamedOperationDefinition<S>, SelectionSet<S>>;

pub type RootOperationTypeDefinition<S> =
  scaffold::RootOperationTypeDefinition<Name<S>, OperationType>;

pub type RootOperationTypesDefinition<S> =
  scaffold::RootOperationTypesDefinition<RootOperationTypeDefinition<S>>;

pub type SchemaDefinition<S> =
  scaffold::SchemaDefinition<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

pub type DescribedSchemaDefinition<S> = Described<SchemaDefinition<S>, S>;

pub type SchemaExtension<S> =
  scaffold::SchemaExtension<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

pub type TypeSystemDefinition<S> =
  scaffold::TypeSystemDefinition<TypeDefinition<S>, DirectiveDefinition<S>, SchemaDefinition<S>>;

pub type DescribedTypeSystemDefinition<S> = Described<TypeSystemDefinition<S>, S>;

pub type TypeSystemExtension<S> =
  scaffold::TypeSystemExtension<TypeExtension<S>, SchemaExtension<S>>;

pub type TypeSystemDefinitionOrExtension<S> = scaffold::TypeSystemDefinitionOrExtension<
  DescribedTypeSystemDefinition<S>,
  TypeSystemExtension<S>,
>;

pub type ExecutableDefinition<S> =
  scaffold::ExecutableDefinition<OperationDefinition<S>, FragmentDefinition<S>>;

pub type DescribedExecutableDefinition<S> = Described<ExecutableDefinition<S>, S>;

pub type Definition<S> = scaffold::Definition<TypeSystemDefinition<S>, ExecutableDefinition<S>>;

pub type DescribedDefinition<S> = Described<Definition<S>, S>;

pub type DefinitionOrExtension<S> =
  scaffold::DefinitionOrExtension<DescribedDefinition<S>, TypeSystemExtension<S>>;

pub type TypeSystemDocument<S> = scaffold::Document<TypeSystemDefinitionOrExtension<S>>;

pub type ExecutableDocument<S> = scaffold::Document<ExecutableDefinition<S>>;

pub type Document<S> = scaffold::Document<DefinitionOrExtension<S>>;
