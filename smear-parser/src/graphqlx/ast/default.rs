use super::*;

use crate::ident::Ident;

use smear_scaffold::ast::{self as scaffold, OperationType, Path};

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

/// A type that can have a description string attached to it.
pub type Described<T, S> = scaffold::Described<T, StringValue<S>>;

/// A type parameter for a type definition with generic support.
pub type DefinitionTypeParam<S, Ty = Type<S>> =
  scaffold::generic::DefinitionTypeParam<Ident<S>, Ty>;

/// A where clause predicate constraining a type parameter.
pub type WherePredicate<S, Ty = Type<S>> = scaffold::generic::WherePredicate<Ident<S>, Ty>;

/// A where clause containing type parameter constraints.
pub type WhereClause<S, Ty = Type<S>> = scaffold::generic::WhereClause<Ident<S>, Ty>;

/// A type parameter for a type extension.
pub type ExtensionTypeParam<S> = scaffold::generic::ExtensionTypeParam<Ident<S>>;

/// Type generics for a type extension.
pub type ExtensionTypeGenerics<S> = scaffold::generic::ExtensionTypeGenerics<Ident<S>>;
/// Type generics for a type definition.
pub type DefinitionTypeGenerics<S, Ty = Type<S>> =
  scaffold::generic::DefinitionTypeGenerics<Ident<S>, Ty>;
/// Type generics for an executable definition.
pub type ExecutableDefinitionTypeGenerics<S> =
  scaffold::generic::ExecutableDefinitionTypeGenerics<Ident<S>>;

/// A list of interfaces that a type implements.
pub type ImplementsInterfaces<S, Ty = Type<S>> = scaffold::ImplementsInterfaces<TypePath<S, Ty>>;

/// A default value for an input field or variable.
pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;
/// A field argument with a name and value.
pub type Argument<S> = scaffold::Argument<Ident<S>, InputValue<S>>;
/// A list of field arguments.
pub type Arguments<S> = scaffold::Arguments<Argument<S>>;

/// A constant field argument with a name and constant value.
pub type ConstArgument<S> = scaffold::Argument<Ident<S>, ConstInputValue<S>>;
/// A list of constant field arguments.
pub type ConstArguments<S> = scaffold::Arguments<ConstArgument<S>>;

/// A list of directives that can be applied to various schema elements.
pub type Directives<S, Ty = Type<S>> = scaffold::Directives<Directive<S, Ty>>;
/// A list of constant directives (used in definitions).
pub type ConstDirectives<S, Ty = Type<S>> = scaffold::Directives<ConstDirective<S, Ty>>;

/// An alias for a field in a selection set.
pub type Alias<S> = scaffold::Alias<Ident<S>>;

/// Definition of arguments for a field.
pub type ArgumentsDefinition<S, Ty = Type<S>> =
  scaffold::ArgumentsDefinition<InputValueDefinition<S, Ty>>;

/// A variable definition in an operation.
pub type VariableDefinition<S, Ty = Type<S>> =
  scaffold::VariableDefinition<VariableValue<S>, Ty, DefaultInputValue<S>, Directives<S>>;

/// A variable definition with an optional description.
pub type DescribedVariableDefinition<S, Ty = Type<S>> = Described<
  scaffold::VariableDefinition<VariableValue<S>, Ty, DefaultInputValue<S>, Directives<S>>,
  S,
>;

/// A list of variable definitions for an operation.
pub type VariablesDefinition<S, Ty = Type<S>> =
  scaffold::VariablesDefinition<DescribedVariableDefinition<S, Ty>>;

/// An input value definition for input objects and field arguments.
pub type InputValueDefinition<S, Ty = Type<S>> = Described<
  scaffold::InputValueDefinition<Ident<S>, Ty, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

/// A list of input field definitions for input object types.
pub type InputFieldsDefinition<S, Ty = Type<S>> =
  scaffold::InputFieldsDefinition<InputValueDefinition<S, Ty>>;

/// A field definition in an object or interface type.
pub type FieldDefinition<S, Ty = Type<S>> = Described<
  scaffold::FieldDefinition<Ident<S>, ArgumentsDefinition<S>, Ty, ConstDirectives<S, Ty>>,
  S,
>;

/// A list of field definitions for object and interface types.
pub type FieldsDefinition<S, Ty = Type<S>> = scaffold::FieldsDefinition<FieldDefinition<S, Ty>>;

/// A scalar type definition.
pub type ScalarTypeDefinition<S, Ty = Type<S>> =
  scaffold::ScalarTypeDefinition<Ident<S>, ConstDirectives<S, Ty>>;

/// A scalar type definition with an optional description.
pub type DescribedScalarTypeDefinition<S, Ty = Type<S>> = Described<ScalarTypeDefinition<S, Ty>, S>;

/// An extension to an existing scalar type.
pub type ScalarTypeExtension<S, Ty = Type<S>> =
  scaffold::ScalarTypeExtension<Ident<S>, ConstDirectives<S, Ty>>;

/// A value definition in an enum type.
pub type EnumValueDefinition<S, Ty = Type<S>> =
  Described<scaffold::EnumValueDefinition<Ident<S>, ConstDirectives<S, Ty>>, S>;

/// A list of enum value definitions.
pub type EnumValuesDefinition<S, Ty = Type<S>> =
  scaffold::EnumValuesDefinition<EnumValueDefinition<S, Ty>>;

/// An enum type definition.
pub type EnumTypeDefinition<S, Ty = Type<S>> =
  scaffold::EnumTypeDefinition<Ident<S>, ConstDirectives<S, Ty>, EnumValuesDefinition<S, Ty>>;

/// An enum type definition with an optional description.
pub type DescribedEnumTypeDefinition<S, Ty = Type<S>> = Described<EnumTypeDefinition<S, Ty>, S>;

/// An extension to an existing enum type.
pub type EnumTypeExtension<S, Ty = Type<S>> =
  scaffold::EnumTypeExtension<Path<Ident<S>>, ConstDirectives<S, Ty>, EnumValuesDefinition<S, Ty>>;

/// A list of member types for a union type.
pub type UnionMembers<S, Ty = Type<S>> = scaffold::UnionMembers<TypePath<S, Ty>>;

/// Definition of root operation types for a schema (query, mutation, subscription).
pub type RootOperationTypesDefinition<S, Ty = Type<S>> =
  scaffold::RootOperationTypesDefinition<RootOperationTypeDefinition<S, Ty>>;

/// A schema definition specifying root operation types.
pub type SchemaDefinition<S, Ty = Type<S>> =
  scaffold::SchemaDefinition<ConstDirectives<S, Ty>, RootOperationTypesDefinition<S, Ty>>;

/// A schema definition with an optional description.
pub type DescribedSchemaDefinition<S, Ty = Type<S>> = Described<SchemaDefinition<S, Ty>, S>;

/// An extension to an existing schema.
pub type SchemaExtension<S, Ty = Type<S>> =
  scaffold::SchemaExtension<ConstDirectives<S, Ty>, RootOperationTypesDefinition<S, Ty>>;

/// A type definition (scalar, object, interface, union, enum, or input object).
pub type TypeDefinition<S, Ty = Type<S>> = scaffold::TypeDefinition<
  ScalarTypeDefinition<S, Ty>,
  ObjectTypeDefinition<S, Ty>,
  InterfaceTypeDefinition<S, Ty>,
  UnionTypeDefinition<S, Ty>,
  EnumTypeDefinition<S, Ty>,
  InputObjectTypeDefinition<S, Ty>,
>;

/// A type system definition (type, directive, or schema definition).
pub type TypeSystemDefinition<S, Ty = Type<S>> = scaffold::TypeSystemDefinition<
  TypeDefinition<S, Ty>,
  DirectiveDefinition<S, Ty>,
  SchemaDefinition<S, Ty>,
>;

/// A type system definition with an optional description.
pub type DescribedTypeSystemDefinition<S, Ty = Type<S>> = Described<TypeSystemDefinition<S, Ty>, S>;

/// A type extension (scalar, object, interface, union, enum, or input object extension).
pub type TypeExtension<S, Ty = Type<S>> = scaffold::TypeExtension<
  ScalarTypeExtension<S, Ty>,
  ObjectTypeExtension<S, Ty>,
  InterfaceTypeExtension<S, Ty>,
  UnionTypeExtension<S, Ty>,
  EnumTypeExtension<S, Ty>,
  InputObjectTypeExtension<S, Ty>,
>;

/// A type system extension (type extension or schema extension).
pub type TypeSystemExtension<S, Ty = Type<S>> =
  scaffold::TypeSystemExtension<TypeExtension<S, Ty>, SchemaExtension<S, Ty>>;

/// Either a type system definition or extension.
pub type TypeSystemDefinitionOrExtension<S, Ty = Type<S>> =
  scaffold::TypeSystemDefinitionOrExtension<
    DescribedTypeSystemDefinition<S, Ty>,
    TypeSystemExtension<S, Ty>,
  >;

/// An import statement or type system definition or extension.
pub type ImportOrTypeSystemDefinitionOrExtension<S, Ty = Type<S>> =
  scaffold::ImportOrTypeSystemDefinitionOrExtension<
    ImportDefinition<S>,
    DescribedTypeSystemDefinition<S, Ty>,
    TypeSystemExtension<S, Ty>,
  >;

/// An operation definition (query, mutation, or subscription).
pub type OperationDefinition<S, Ty = Type<S>> =
  scaffold::OperationDefinition<NamedOperationDefinition<S, Ty>, SelectionSet<S, Ty>>;

/// An executable definition (operation or fragment).
pub type ExecutableDefinition<S, Ty = Type<S>> =
  scaffold::ExecutableDefinition<OperationDefinition<S, Ty>, FragmentDefinition<S, Ty>>;
/// An executable definition with an optional description.
pub type DescribedExecutableDefinition<S, Ty = Type<S>> = Described<ExecutableDefinition<S, Ty>, S>;

/// An import statement or executable definition.
pub type ImportOrExecutableDefinition<S, Ty = Type<S>> =
  scaffold::ImportOrExecutableDefinition<ImportDefinition<S>, DescribedExecutableDefinition<S, Ty>>;

/// Either a type system definition or executable definition.
pub type Definition<S, Ty = Type<S>> =
  scaffold::Definition<TypeSystemDefinition<S, Ty>, ExecutableDefinition<S, Ty>>;

/// A definition with an optional description.
pub type DescribedDefinition<S, Ty = Type<S>> = Described<Definition<S, Ty>, S>;

/// Either a definition or a type system extension.
pub type DefinitionOrExtension<S, Ty = Type<S>> =
  scaffold::DefinitionOrExtension<DescribedDefinition<S, Ty>, TypeSystemExtension<S, Ty>>;

/// An import statement, definition, or type system extension.
pub type ImportOrDefinitionOrExtension<S, Ty = Type<S>> = scaffold::ImportOrDefinitionOrExtension<
  ImportDefinition<S>,
  DescribedDefinition<S, Ty>,
  TypeSystemExtension<S, Ty>,
>;

/// A type system document containing type system definitions and extensions.
pub type TypeSystemDocument<S, Ty = Type<S>> =
  scaffold::Document<ImportOrTypeSystemDefinitionOrExtension<S, Ty>>;

/// An executable document containing operations and fragments.
pub type ExecutableDocument<S, Ty = Type<S>> =
  scaffold::Document<ImportOrExecutableDefinition<S, Ty>>;

/// A GraphQL document containing definitions and extensions.
pub type Document<S, Ty = Type<S>> = scaffold::Document<ImportOrDefinitionOrExtension<S, Ty>>;
