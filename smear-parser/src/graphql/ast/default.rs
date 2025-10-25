//! Type aliases for GraphQL AST nodes.
//!
//! This module provides convenient type aliases that instantiate the generic scaffold types
//! with concrete GraphQL types. These aliases form the public API for parsing standard GraphQL.

use super::*;
use smear_scaffold::ast::{self as scaffold, DirectiveLocations, Location, OperationType};

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
pub type ArgumentsDefinition<S, Ty = Type<Name<S>>> =
  scaffold::ArgumentsDefinition<InputValueDefinition<S, Ty>>;

/// Directive definition in a schema.
pub type DirectiveDefinition<S, Ty = Type<Name<S>>> =
  scaffold::DirectiveDefinition<Name<S>, ArgumentsDefinition<S, Ty>, DirectiveLocations<Location>>;

/// Variable definition in an operation.
pub type VariableDefinition<S, Ty = Type<Name<S>>> =
  scaffold::VariableDefinition<VariableValue<S>, Ty, DefaultInputValue<S>, Directives<S>>;

/// Variable definition with optional description.
pub type DescribedVariableDefinition<S, Ty = Type<Name<S>>> =
  Described<VariableDefinition<S, Ty>, S>;

/// List of variable definitions for an operation.
pub type VariablesDefinition<S, Ty = Type<Name<S>>> =
  scaffold::VariablesDefinition<DescribedVariableDefinition<S, Ty>>;

/// Input value definition (for input object fields or field arguments).
pub type InputValueDefinition<S, Ty = Type<Name<S>>> = Described<
  scaffold::InputValueDefinition<Name<S>, Ty, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

/// List of input field definitions for an input object type.
pub type InputFieldsDefinition<S, Ty = Type<Name<S>>> =
  scaffold::InputFieldsDefinition<InputValueDefinition<S, Ty>>;

/// Field definition in an object or interface type.
pub type FieldDefinition<S, Ty = Type<Name<S>>> =
  Described<scaffold::FieldDefinition<Name<S>, ArgumentsDefinition<S>, Ty, ConstDirectives<S>>, S>;

/// List of field definitions for an object or interface type.
pub type FieldsDefinition<S, Ty = Type<Name<S>>> =
  scaffold::FieldsDefinition<FieldDefinition<S, Ty>>;

/// Input object type definition.
pub type InputObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  scaffold::InputObjectTypeDefinition<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S, Ty>>;

/// Input object type definition with optional description.
pub type DescribedInputObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  Described<InputObjectTypeDefinition<S, Ty>, S>;

/// Extension of an input object type.
pub type InputObjectTypeExtension<S, Ty = Type<Name<S>>> =
  scaffold::InputObjectTypeExtension<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S, Ty>>;

/// Fragment definition in an executable document.
pub type FragmentDefinition<S> =
  scaffold::FragmentDefinition<FragmentName<S>, TypeCondition<S>, Directives<S>, SelectionSet<S>>;

/// Scalar type definition.
pub type ScalarTypeDefinition<S> = scaffold::ScalarTypeDefinition<Name<S>, ConstDirectives<S>>;

/// Scalar type definition with optional description.
pub type DescribedScalarTypeDefinition<S> = Described<ScalarTypeDefinition<S>, S>;

/// Extension of a scalar type.
pub type ScalarTypeExtension<S> = scaffold::ScalarTypeExtension<Name<S>, ConstDirectives<S>>;

/// Implements interfaces clause for object and interface types.
pub type ImplementsInterfaces<S> = scaffold::ImplementsInterfaces<Name<S>>;

/// Object type definition.
pub type ObjectTypeDefinition<S, Ty = Type<Name<S>>> = scaffold::ObjectTypeDefinition<
  Name<S>,
  ImplementsInterfaces<S>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Object type definition with optional description.
pub type DescribedObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  Described<ObjectTypeDefinition<S, Ty>, S>;

/// Extension of an object type.
pub type ObjectTypeExtension<S, Ty = Type<Name<S>>> = scaffold::ObjectTypeExtension<
  Name<S>,
  ImplementsInterfaces<S>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Interface type definition.
pub type InterfaceTypeDefinition<S, Ty = Type<Name<S>>> = scaffold::InterfaceTypeDefinition<
  Name<S>,
  ImplementsInterfaces<S>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Interface type definition with optional description.
pub type DescribedInterfaceTypeDefinition<S, Ty = Type<Name<S>>> =
  Described<InterfaceTypeDefinition<S, Ty>, S>;

/// Extension of an interface type.
pub type InterfaceTypeExtension<S, Ty = Type<Name<S>>> = scaffold::InterfaceTypeExtension<
  Name<S>,
  ImplementsInterfaces<S>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Union member types (list of type names).
pub type UnionMembers<S> = scaffold::UnionMembers<Name<S>>;

/// Union type definition.
pub type UnionTypeDefinition<S> =
  scaffold::UnionTypeDefinition<Name<S>, ConstDirectives<S>, UnionMembers<S>>;

/// Union type definition with optional description.
pub type DescribedUnionTypeDefinition<S> = Described<UnionTypeDefinition<S>, S>;

/// Extension of a union type.
pub type UnionTypeExtension<S> =
  scaffold::UnionTypeExtension<Name<S>, ConstDirectives<S>, UnionMembers<S>>;

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
pub type NamedOperationDefinition<S, Ty = Type<Name<S>>> = scaffold::NamedOperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S, Ty>,
  Directives<S>,
  SelectionSet<S>,
>;

/// Operation definition (can be named or anonymous).
pub type OperationDefinition<S, Ty = Type<Name<S>>> =
  scaffold::OperationDefinition<NamedOperationDefinition<S, Ty>, SelectionSet<S>>;

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

/// Type system definition with optional description.
pub type DescribedTypeSystemDefinition<S, Ty = Type<Name<S>>> =
  Described<TypeSystemDefinition<S, Ty>, S>;

/// Executable definition with optional description.
pub type DescribedExecutableDefinition<S, Ty = Type<Name<S>>> =
  Described<ExecutableDefinition<S, Ty>, S>;

/// Definition with optional description.
pub type DescribedDefinition<S, Ty = Type<Name<S>>> = Described<Definition<S, Ty>, S>;

/// Type system document (contains only schema and type definitions/extensions).
pub type TypeSystemDocument<S, Ty = Type<Name<S>>> =
  scaffold::Document<TypeSystemDefinitionOrExtension<S, Ty>>;

/// Executable document (contains only operations and fragments).
pub type ExecutableDocument<S, Ty = Type<Name<S>>> =
  scaffold::Document<ExecutableDefinition<S, Ty>>;

/// Full GraphQL document (can contain both type system and executable definitions).
pub type Document<S, Ty = Type<Name<S>>> = scaffold::Document<DefinitionOrExtension<S, Ty>>;
