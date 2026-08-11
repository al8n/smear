//! GraphQL-specialized type-system (SDL) AST node aliases.
//!
//! The shared type-system carriers live at crate level. This module binds them
//! to GraphQL names, types, constant values, directives, and operation kinds.
//! [`TypeDefinition`] remains a concrete, exhaustively matchable GraphQL enum.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::{
  ConstDirectives, DefaultInputValue, Described, ExecutableDefinition, Name, OperationType, Type,
};

pub use crate::type_system::{
  DirectiveLocations, ExecutableDirectiveLocation, ImplementInterfaces, Location,
  TypeSystemDirectiveLocation, UnionMemberTypes,
};

/// Definition of the arguments accepted by a field or directive
/// (`( InputValueDefinition+ )`).
pub type ArgumentsDefinition<S, Ty = Type<Name<S>>> =
  crate::type_system::ArgumentsDefinition<InputValueDefinition<S, Ty>>;

/// Input value definition (an input object field or a field/directive argument),
/// carrying an optional leading description
/// (`Description? Name ':' Type DefaultValue? Directives?`).
pub type InputValueDefinition<S, Ty = Type<Name<S>>> = Described<
  crate::type_system::InputValueDefinition<Name<S>, Ty, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

/// List of input field definitions for an input object type
/// (`{ InputValueDefinition+ }`).
pub type InputFieldsDefinition<S, Ty = Type<Name<S>>> =
  crate::type_system::InputFieldsDefinition<InputValueDefinition<S, Ty>>;

/// Field definition in an object or interface type, carrying an optional leading
/// description (`Description? Name ArgumentsDefinition? ':' Type Directives?`).
pub type FieldDefinition<S, Ty = Type<Name<S>>> = Described<
  crate::type_system::FieldDefinition<Name<S>, ArgumentsDefinition<S, Ty>, Ty, ConstDirectives<S>>,
  S,
>;

/// List of field definitions for an object or interface type
/// (`{ FieldDefinition+ }`).
pub type FieldsDefinition<S, Ty = Type<Name<S>>> =
  crate::type_system::FieldsDefinition<FieldDefinition<S, Ty>>;

/// Scalar type definition (`scalar Name Directives?`).
pub type ScalarTypeDefinition<S> =
  crate::type_system::ScalarTypeDefinition<Name<S>, ConstDirectives<S>>;

/// Scalar type definition with an optional leading description.
pub type DescribedScalarTypeDefinition<S> = Described<ScalarTypeDefinition<S>, S>;

/// Object type definition
/// (`type Name ImplementsInterfaces? Directives? FieldsDefinition?`).
pub type ObjectTypeDefinition<S, Ty = Type<Name<S>>> = crate::type_system::ObjectTypeDefinition<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Object type definition with an optional leading description.
pub type DescribedObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  Described<ObjectTypeDefinition<S, Ty>, S>;

/// Interface type definition
/// (`interface Name ImplementsInterfaces? Directives? FieldsDefinition?`).
pub type InterfaceTypeDefinition<S, Ty = Type<Name<S>>> =
  crate::type_system::InterfaceTypeDefinition<
    Name<S>,
    ImplementInterfaces<Name<S>>,
    ConstDirectives<S>,
    FieldsDefinition<S, Ty>,
  >;

/// Interface type definition with an optional leading description.
pub type DescribedInterfaceTypeDefinition<S, Ty = Type<Name<S>>> =
  Described<InterfaceTypeDefinition<S, Ty>, S>;

/// Union type definition (`union Name Directives? UnionMemberTypes?`).
pub type UnionTypeDefinition<S> =
  crate::type_system::UnionTypeDefinition<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

/// Union type definition with an optional leading description.
pub type DescribedUnionTypeDefinition<S> = Described<UnionTypeDefinition<S>, S>;

/// Enum value definition, carrying an optional leading description
/// (`Description? EnumValue Directives?`).
pub type EnumValueDefinition<S> =
  Described<crate::type_system::EnumValueDefinition<Name<S>, ConstDirectives<S>>, S>;

/// List of enum value definitions (`{ EnumValueDefinition+ }`).
pub type EnumValuesDefinition<S> = crate::type_system::EnumValuesDefinition<EnumValueDefinition<S>>;

/// Enum type definition (`enum Name Directives? EnumValuesDefinition?`).
pub type EnumTypeDefinition<S> =
  crate::type_system::EnumTypeDefinition<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

/// Enum type definition with an optional leading description.
pub type DescribedEnumTypeDefinition<S> = Described<EnumTypeDefinition<S>, S>;

/// Input object type definition (`input Name Directives? InputFieldsDefinition?`).
pub type InputObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  crate::type_system::InputObjectTypeDefinition<
    Name<S>,
    ConstDirectives<S>,
    InputFieldsDefinition<S, Ty>,
  >;

/// Input object type definition with an optional leading description.
pub type DescribedInputObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  Described<InputObjectTypeDefinition<S, Ty>, S>;

/// Scalar type extension (`extend scalar Name Directives`).
pub type ScalarTypeExtension<S> =
  crate::type_system::ScalarTypeExtension<Name<S>, ConstDirectives<S>>;

/// Object type extension (`extend type Name` plus a nonempty extension payload).
pub type ObjectTypeExtension<S, Ty = Type<Name<S>>> = crate::type_system::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Interface type extension (`extend interface Name` plus a nonempty extension payload).
pub type InterfaceTypeExtension<S, Ty = Type<Name<S>>> = crate::type_system::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Union type extension (`extend union Name` plus directives or member types).
pub type UnionTypeExtension<S> =
  crate::type_system::UnionTypeExtension<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

/// Enum type extension (`extend enum Name` plus directives or values).
pub type EnumTypeExtension<S> =
  crate::type_system::EnumTypeExtension<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

/// Input object type extension (`extend input Name` plus directives or fields).
pub type InputObjectTypeExtension<S, Ty = Type<Name<S>>> =
  crate::type_system::InputObjectTypeExtension<
    Name<S>,
    ConstDirectives<S>,
    InputFieldsDefinition<S, Ty>,
  >;

/// Directive definition
/// (`directive '@' Name ArgumentsDefinition? repeatable? 'on' DirectiveLocations`).
pub type DirectiveDefinition<S, Ty = Type<Name<S>>> = crate::type_system::DirectiveDefinition<
  Name<S>,
  ArgumentsDefinition<S, Ty>,
  DirectiveLocations<Location>,
>;

/// Root operation type definition (`OperationType ':' Name`).
pub type RootOperationTypeDefinition<S> =
  crate::type_system::RootOperationTypeDefinition<Name<S>, OperationType>;

/// List of root operation type definitions for a schema
/// (`{ RootOperationTypeDefinition+ }`).
pub type RootOperationTypesDefinition<S> =
  crate::type_system::RootOperationTypesDefinition<RootOperationTypeDefinition<S>>;

/// Schema definition (`schema Directives? RootOperationTypesDefinition`).
pub type SchemaDefinition<S> =
  crate::type_system::SchemaDefinition<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

/// Schema definition with an optional leading description.
pub type DescribedSchemaDefinition<S> = Described<SchemaDefinition<S>, S>;

/// A GraphQL type definition — one of the six named type-system shapes.
#[derive(Debug, Clone, PartialEq, Eq, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeDefinition<S, Ty = Type<Name<S>>> {
  /// The scalar type definition.
  Scalar(ScalarTypeDefinition<S>),
  /// The object type definition.
  Object(ObjectTypeDefinition<S, Ty>),
  /// The interface type definition.
  Interface(InterfaceTypeDefinition<S, Ty>),
  /// The union type definition.
  Union(UnionTypeDefinition<S>),
  /// The enum type definition.
  Enum(EnumTypeDefinition<S>),
  /// The input object type definition.
  InputObject(InputObjectTypeDefinition<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for TypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(value) => value.into_span(),
      Self::Object(value) => value.into_span(),
      Self::Interface(value) => value.into_span(),
      Self::Union(value) => value.into_span(),
      Self::Enum(value) => value.into_span(),
      Self::InputObject(value) => value.into_span(),
    }
  }
}

impl<S, Ty> TypeDefinition<S, Ty> {
  /// Returns the span of the type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(value) => value.span(),
      Self::Object(value) => value.span(),
      Self::Interface(value) => value.span(),
      Self::Union(value) => value.span(),
      Self::Enum(value) => value.span(),
      Self::InputObject(value) => value.span(),
    }
  }
}

/// A GraphQL type extension — one of the six named type-system extension shapes.
pub type TypeExtension<S, Ty = Type<Name<S>>> = crate::type_system::TypeExtension<
  ScalarTypeExtension<S>,
  ObjectTypeExtension<S, Ty>,
  InterfaceTypeExtension<S, Ty>,
  UnionTypeExtension<S>,
  EnumTypeExtension<S>,
  InputObjectTypeExtension<S, Ty>,
>;

/// Schema extension (`extend schema` plus directives or root operation types).
pub type SchemaExtension<S> =
  crate::type_system::SchemaExtension<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

/// Type-system definition: a named type, directive, or schema definition.
pub type TypeSystemDefinition<S, Ty = Type<Name<S>>> = crate::type_system::TypeSystemDefinition<
  TypeDefinition<S, Ty>,
  DirectiveDefinition<S, Ty>,
  SchemaDefinition<S>,
>;

/// Type-system definition with one optional leading description.
pub type DescribedTypeSystemDefinition<S, Ty = Type<Name<S>>> =
  Described<TypeSystemDefinition<S, Ty>, S>;

/// Type-system extension: a named type or schema extension.
pub type TypeSystemExtension<S, Ty = Type<Name<S>>> =
  crate::type_system::TypeSystemExtension<TypeExtension<S, Ty>, SchemaExtension<S>>;

/// A type-system definition (with optional description) or extension.
pub type TypeSystemDefinitionOrExtension<S, Ty = Type<Name<S>>> =
  crate::type_system::TypeSystemDefinitionOrExtension<
    DescribedTypeSystemDefinition<S, Ty>,
    TypeSystemExtension<S, Ty>,
  >;

/// A nonempty GraphQL type-system document.
pub type TypeSystemDocument<S, Ty = Type<Name<S>>> =
  crate::executable::Document<TypeSystemDefinitionOrExtension<S, Ty>>;

/// A GraphQL definition: either a type-system or executable definition.
pub type Definition<S, Ty = Type<Name<S>>> =
  crate::executable::Definition<TypeSystemDefinition<S, Ty>, ExecutableDefinition<S, Ty>>;

/// A GraphQL definition with an optional leading description.
///
/// Every keyworded definition carries one, executable and type-system alike. The
/// shorthand `OperationDefinition : SelectionSet` is the one alternative that
/// does not.
pub type DescribedDefinition<S, Ty = Type<Name<S>>> = Described<Definition<S, Ty>, S>;

/// A GraphQL definition with an optional description, or a type-system extension.
pub type DefinitionOrExtension<S, Ty = Type<Name<S>>> =
  crate::executable::DefinitionOrExtension<DescribedDefinition<S, Ty>, TypeSystemExtension<S, Ty>>;

/// A nonempty full GraphQL document.
///
/// Its entries may be executable definitions, type-system definitions, or
/// type-system extensions. Descriptions belong to definitions of either family;
/// a type-system extension never carries one.
pub type Document<S, Ty = Type<Name<S>>> =
  crate::executable::Document<DefinitionOrExtension<S, Ty>>;
