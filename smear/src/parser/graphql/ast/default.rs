//! Type aliases for GraphQL AST nodes.
//!
//! This module provides convenient type aliases that instantiate the generic scaffold types
//! with concrete GraphQL types. These aliases form the public API for parsing standard GraphQL.

use super::*;
use crate::scaffold::{
  self, DirectiveLocations, ImplementInterfaces, Location, OperationType, UnionMemberTypes,
};

pub use field::*;

mod field;

pub use scaffold::FragmentName;

/// A described item wraps a value with an optional description string.
pub type Described<T, S> = scaffold::Described<T, StringValue<S>>;

/// Default value for input fields and arguments, using constant expressions.
pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;

/// Argument in an executable context (can contain variables).
pub type Argument<S> = scaffold::Argument<Name<S>, InputValue<S>>;

/// List of arguments in an executable context.
pub type Arguments<S> = scaffold::Arguments<Argument<S>>;

/// Argument in a constant context (no variables, used in schemas).
pub type ConstArgument<S> = scaffold::Argument<Name<S>, ConstInputValue<S>>;

/// List of constant arguments.
pub type ConstArguments<S> = scaffold::Arguments<ConstArgument<S>>;

/// Directive with executable arguments (can contain variables).
pub type Directive<S> = scaffold::Directive<Name<S>, Arguments<S>>;

/// List of directives with executable arguments.
pub type Directives<S> = scaffold::Directives<Directive<S>>;

/// Directive with constant arguments (no variables).
pub type ConstDirective<S> = scaffold::Directive<Name<S>, ConstArguments<S>>;

/// List of directives with constant arguments.
pub type ConstDirectives<S> = scaffold::Directives<ConstDirective<S>>;

/// Field alias in selection sets.
pub type Alias<S> = scaffold::Alias<Name<S>>;

/// Definition of arguments for a field or directive.
pub type ArgumentsDefinition<S> = scaffold::ArgumentsDefinition<InputValueDefinition<S>>;

/// Directive definition in a schema.
pub type DirectiveDefinition<S> =
  scaffold::DirectiveDefinition<Name<S>, ArgumentsDefinition<S>, DirectiveLocations<Location>>;

/// Variable definition in an operation.
pub type VariableDefinition<S> = scaffold::VariableDefinition<
  VariableValue<S>,
  Type<Name<S>>,
  DefaultInputValue<S>,
  Directives<S>,
>;

/// Variable definition with optional description.
pub type DescribedVariableDefinition<S> = Described<VariableDefinition<S>, S>;

/// List of variable definitions for an operation.
pub type VariablesDefinition<S> = scaffold::VariablesDefinition<DescribedVariableDefinition<S>>;

/// Input value definition (for input object fields or field arguments).
pub type InputValueDefinition<S> = Described<
  scaffold::InputValueDefinition<Name<S>, Type<Name<S>>, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

/// List of input field definitions for an input object type.
pub type InputFieldsDefinition<S> = scaffold::InputFieldsDefinition<InputValueDefinition<S>>;

/// Field definition in an object or interface type.
pub type FieldDefinition<S> = Described<
  scaffold::FieldDefinition<Name<S>, ArgumentsDefinition<S>, Type<Name<S>>, ConstDirectives<S>>,
  S,
>;

/// List of field definitions for an object or interface type.
pub type FieldsDefinition<S> = scaffold::FieldsDefinition<FieldDefinition<S>>;

/// Input object type definition.
pub type InputObjectTypeDefinition<S> =
  scaffold::InputObjectTypeDefinition<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S>>;

/// Input object type definition with optional description.
pub type DescribedInputObjectTypeDefinition<S> = Described<InputObjectTypeDefinition<S>, S>;

/// Extension of an input object type.
pub type InputObjectTypeExtension<S> =
  scaffold::InputObjectTypeExtension<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S>>;

/// Fragment definition in an executable document.
pub type FragmentDefinition<S> =
  scaffold::FragmentDefinition<FragmentName<S>, TypeCondition<S>, Directives<S>, SelectionSet<S>>;

/// Scalar type definition.
pub type ScalarTypeDefinition<S> = scaffold::ScalarTypeDefinition<Name<S>, ConstDirectives<S>>;

/// Scalar type definition with optional description.
pub type DescribedScalarTypeDefinition<S> = Described<ScalarTypeDefinition<S>, S>;

/// Extension of a scalar type.
pub type ScalarTypeExtension<S> = scaffold::ScalarTypeExtension<Name<S>, ConstDirectives<S>>;

/// Object type definition.
pub type ObjectTypeDefinition<S> = scaffold::ObjectTypeDefinition<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

/// Object type definition with optional description.
pub type DescribedObjectTypeDefinition<S> = Described<ObjectTypeDefinition<S>, S>;

/// Extension of an object type.
pub type ObjectTypeExtension<S> = scaffold::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

/// Interface type definition.
pub type InterfaceTypeDefinition<S> = scaffold::InterfaceTypeDefinition<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

/// Interface type definition with optional description.
pub type DescribedInterfaceTypeDefinition<S> = Described<InterfaceTypeDefinition<S>, S>;

/// Extension of an interface type.
pub type InterfaceTypeExtension<S> = scaffold::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

/// Union type definition.
pub type UnionTypeDefinition<S> =
  scaffold::UnionTypeDefinition<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

/// Union type definition with optional description.
pub type DescribedUnionTypeDefinition<S> = Described<UnionTypeDefinition<S>, S>;

/// Extension of a union type.
pub type UnionTypeExtension<S> =
  scaffold::UnionTypeExtension<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

/// Enum value definition.
pub type EnumValueDefinition<S> =
  Described<scaffold::EnumValueDefinition<Name<S>, ConstDirectives<S>>, S>;

/// List of enum value definitions.
pub type EnumValuesDefinition<S> = scaffold::EnumValuesDefinition<EnumValueDefinition<S>>;

/// Enum type definition.
pub type EnumTypeDefinition<S> =
  scaffold::EnumTypeDefinition<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

/// Enum type definition with optional description.
pub type DescribedEnumTypeDefinition<S> = Described<EnumTypeDefinition<S>, S>;

/// Extension of an enum type.
pub type EnumTypeExtension<S> =
  scaffold::EnumTypeExtension<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

/// Named operation definition (query, mutation, or subscription with a name).
pub type NamedOperationDefinition<S> = scaffold::NamedOperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S>,
  Directives<S>,
  SelectionSet<S>,
>;

/// Operation definition (can be named or anonymous).
pub type OperationDefinition<S> =
  scaffold::OperationDefinition<NamedOperationDefinition<S>, SelectionSet<S>>;

/// Root operation type definition (maps operation type to a named type).
pub type RootOperationTypeDefinition<S> =
  scaffold::RootOperationTypeDefinition<Name<S>, OperationType>;

/// List of root operation type definitions for a schema.
pub type RootOperationTypesDefinition<S> =
  scaffold::RootOperationTypesDefinition<RootOperationTypeDefinition<S>>;

/// Schema definition.
pub type SchemaDefinition<S> =
  scaffold::SchemaDefinition<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

/// Schema definition with optional description.
pub type DescribedSchemaDefinition<S> = Described<SchemaDefinition<S>, S>;

/// Schema extension.
pub type SchemaExtension<S> =
  scaffold::SchemaExtension<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

/// Type system definition (schema, type, or directive definition).
pub type TypeSystemDefinition<S> =
  scaffold::TypeSystemDefinition<TypeDefinition<S>, DirectiveDefinition<S>, SchemaDefinition<S>>;

/// Type system definition with optional description.
pub type DescribedTypeSystemDefinition<S> = Described<TypeSystemDefinition<S>, S>;

/// Type system extension (type or schema extension).
pub type TypeSystemExtension<S> =
  scaffold::TypeSystemExtension<TypeExtension<S>, SchemaExtension<S>>;

/// Type system definition or extension.
pub type TypeSystemDefinitionOrExtension<S> = scaffold::TypeSystemDefinitionOrExtension<
  DescribedTypeSystemDefinition<S>,
  TypeSystemExtension<S>,
>;

/// Executable definition (operation or fragment).
pub type ExecutableDefinition<S> =
  scaffold::ExecutableDefinition<OperationDefinition<S>, FragmentDefinition<S>>;

/// Executable definition with optional description.
pub type DescribedExecutableDefinition<S> = Described<ExecutableDefinition<S>, S>;

/// Definition (type system or executable).
pub type Definition<S> = scaffold::Definition<TypeSystemDefinition<S>, ExecutableDefinition<S>>;

/// Definition with optional description.
pub type DescribedDefinition<S> = Described<Definition<S>, S>;

/// Definition or extension.
pub type DefinitionOrExtension<S> =
  scaffold::DefinitionOrExtension<DescribedDefinition<S>, TypeSystemExtension<S>>;

/// Type system document (contains only schema and type definitions/extensions).
pub type TypeSystemDocument<S> = scaffold::Document<TypeSystemDefinitionOrExtension<S>>;

/// Executable document (contains only operations and fragments).
pub type ExecutableDocument<S> = scaffold::Document<ExecutableDefinition<S>>;

/// Full GraphQL document (can contain both type system and executable definitions).
pub type Document<S> = scaffold::Document<DefinitionOrExtension<S>>;
