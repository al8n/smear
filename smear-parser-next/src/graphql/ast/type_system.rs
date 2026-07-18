//! GraphQL type-system (SDL) definition AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate (`graphql/ast/default.rs`
//! and `graphql/ast/type_system.rs`): the input-value/field/argument definitions, the
//! `implements`/union-member/directive-location clauses, the enum-value definitions,
//! the root-operation-type definitions, the scalar/object/interface/union/enum/
//! input-object/directive/schema type definitions *and extensions*, and the
//! definition-or-extension / document families, keyed by the source slice `S` with
//! spans pinned to [`SimpleSpan`](tokora::SimpleSpan). Most are aliases over the
//! generic `smear-scaffold` node types; [`TypeDefinition`] and [`TypeExtension`] are
//! real enums (copied verbatim from the frozen crate, not the scaffold's
//! `#[non_exhaustive]` generics) so the dispatches can match their variants
//! exhaustively.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_scaffold::ast as scaffold;
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::{
  ConstDirectives, DefaultInputValue, Described, ExecutableDefinition, Name, OperationType, Type,
};

pub use scaffold::{
  DirectiveLocations, ExecutableDirectiveLocation, ImplementInterfaces, Location,
  TypeSystemDirectiveLocation, UnionMemberTypes,
};

/// Definition of the arguments a field or directive takes (`( InputValueDefinition+ )`).
pub type ArgumentsDefinition<S, Ty = Type<Name<S>>> =
  scaffold::ArgumentsDefinition<InputValueDefinition<S, Ty>>;

/// Input value definition (an input object field or a field/directive argument),
/// carrying an optional leading description
/// (`Description? Name ':' Type DefaultValue? Directives?`).
pub type InputValueDefinition<S, Ty = Type<Name<S>>> = Described<
  scaffold::InputValueDefinition<Name<S>, Ty, DefaultInputValue<S>, ConstDirectives<S>>,
  S,
>;

/// List of input field definitions for an input object type (`{ InputValueDefinition+ }`).
pub type InputFieldsDefinition<S, Ty = Type<Name<S>>> =
  scaffold::InputFieldsDefinition<InputValueDefinition<S, Ty>>;

/// Field definition in an object or interface type, carrying an optional leading
/// description (`Description? Name ArgumentsDefinition? ':' Type Directives?`).
pub type FieldDefinition<S, Ty = Type<Name<S>>> =
  Described<scaffold::FieldDefinition<Name<S>, ArgumentsDefinition<S>, Ty, ConstDirectives<S>>, S>;

/// List of field definitions for an object or interface type (`{ FieldDefinition+ }`).
pub type FieldsDefinition<S, Ty = Type<Name<S>>> =
  scaffold::FieldsDefinition<FieldDefinition<S, Ty>>;

/// Scalar type definition (`scalar Name Directives?`).
pub type ScalarTypeDefinition<S> = scaffold::ScalarTypeDefinition<Name<S>, ConstDirectives<S>>;

/// Scalar type definition with an optional leading description.
pub type DescribedScalarTypeDefinition<S> = Described<ScalarTypeDefinition<S>, S>;

/// Object type definition
/// (`type Name ImplementsInterfaces? Directives? FieldsDefinition?`).
pub type ObjectTypeDefinition<S, Ty = Type<Name<S>>> = scaffold::ObjectTypeDefinition<
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
pub type InterfaceTypeDefinition<S, Ty = Type<Name<S>>> = scaffold::InterfaceTypeDefinition<
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
  scaffold::UnionTypeDefinition<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

/// Union type definition with an optional leading description.
pub type DescribedUnionTypeDefinition<S> = Described<UnionTypeDefinition<S>, S>;

/// Enum value definition, carrying an optional leading description
/// (`Description? EnumValue Directives?`).
pub type EnumValueDefinition<S> =
  Described<scaffold::EnumValueDefinition<Name<S>, ConstDirectives<S>>, S>;

/// List of enum value definitions (`{ EnumValueDefinition+ }`).
pub type EnumValuesDefinition<S> = scaffold::EnumValuesDefinition<EnumValueDefinition<S>>;

/// Enum type definition (`enum Name Directives? EnumValuesDefinition?`).
pub type EnumTypeDefinition<S> =
  scaffold::EnumTypeDefinition<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

/// Enum type definition with an optional leading description.
pub type DescribedEnumTypeDefinition<S> = Described<EnumTypeDefinition<S>, S>;

/// Input object type definition (`input Name Directives? InputFieldsDefinition?`).
pub type InputObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  scaffold::InputObjectTypeDefinition<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S, Ty>>;

/// Input object type definition with an optional leading description.
pub type DescribedInputObjectTypeDefinition<S, Ty = Type<Name<S>>> =
  Described<InputObjectTypeDefinition<S, Ty>, S>;

/// Directive definition
/// (`directive '@' Name ArgumentsDefinition? repeatable? 'on' DirectiveLocations`).
pub type DirectiveDefinition<S, Ty = Type<Name<S>>> =
  scaffold::DirectiveDefinition<Name<S>, ArgumentsDefinition<S, Ty>, DirectiveLocations<Location>>;

/// Root operation type definition (`OperationType ':' Name`).
pub type RootOperationTypeDefinition<S> =
  scaffold::RootOperationTypeDefinition<Name<S>, OperationType>;

/// List of root operation type definitions for a schema
/// (`{ RootOperationTypeDefinition+ }`).
pub type RootOperationTypesDefinition<S> =
  scaffold::RootOperationTypesDefinition<RootOperationTypeDefinition<S>>;

/// Schema definition (`schema Directives? RootOperationTypesDefinition`).
pub type SchemaDefinition<S> =
  scaffold::SchemaDefinition<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

/// Schema definition with an optional leading description.
pub type DescribedSchemaDefinition<S> = Described<SchemaDefinition<S>, S>;

/// A GraphQL type definition — one of the six named type-system shapes.
///
/// Copied verbatim from the frozen `smear-parser` crate (`graphql/ast/type_system.rs`)
/// rather than aliased to the `smear-scaffold` generic, so the dialect owns a concrete,
/// exhaustively-matchable enum with `From` conversions for each arm.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
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
      Self::Scalar(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
      Self::Interface(v) => v.into_span(),
      Self::Union(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::InputObject(v) => v.into_span(),
    }
  }
}

impl<S, Ty> TypeDefinition<S, Ty> {
  /// Returns the span of the type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(v) => v.span(),
      Self::Object(v) => v.span(),
      Self::Interface(v) => v.span(),
      Self::Union(v) => v.span(),
      Self::Enum(v) => v.span(),
      Self::InputObject(v) => v.span(),
    }
  }
}

/// Extension of a scalar type (`extend scalar Name Directives`).
pub type ScalarTypeExtension<S> = scaffold::ScalarTypeExtension<Name<S>, ConstDirectives<S>>;

/// Extension of an object type
/// (`extend type Name` + at least one of implements/directives/fields).
pub type ObjectTypeExtension<S, Ty = Type<Name<S>>> = scaffold::ObjectTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Extension of an interface type
/// (`extend interface Name` + at least one of implements/directives/fields).
pub type InterfaceTypeExtension<S, Ty = Type<Name<S>>> = scaffold::InterfaceTypeExtension<
  Name<S>,
  ImplementInterfaces<Name<S>>,
  ConstDirectives<S>,
  FieldsDefinition<S, Ty>,
>;

/// Extension of a union type
/// (`extend union Name` + at least one of directives/member types).
pub type UnionTypeExtension<S> =
  scaffold::UnionTypeExtension<Name<S>, ConstDirectives<S>, UnionMemberTypes<Name<S>>>;

/// Extension of an enum type
/// (`extend enum Name` + at least one of directives/values).
pub type EnumTypeExtension<S> =
  scaffold::EnumTypeExtension<Name<S>, ConstDirectives<S>, EnumValuesDefinition<S>>;

/// Extension of an input object type
/// (`extend input Name` + at least one of directives/fields).
pub type InputObjectTypeExtension<S, Ty = Type<Name<S>>> =
  scaffold::InputObjectTypeExtension<Name<S>, ConstDirectives<S>, InputFieldsDefinition<S, Ty>>;

/// Schema extension (`extend schema` + at least one of directives/root operations).
pub type SchemaExtension<S> =
  scaffold::SchemaExtension<ConstDirectives<S>, RootOperationTypesDefinition<S>>;

/// A GraphQL type extension — one of the six named type-system extension shapes.
///
/// Copied verbatim from the frozen `smear-parser` crate (`graphql/ast/type_system.rs`)
/// rather than aliased to the `smear-scaffold` generic, so the dialect owns a concrete,
/// exhaustively-matchable enum with `From` conversions for each arm.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeExtension<S, Ty = Type<Name<S>>> {
  /// The scalar type extension.
  Scalar(ScalarTypeExtension<S>),
  /// The object type extension.
  Object(ObjectTypeExtension<S, Ty>),
  /// The interface type extension.
  Interface(InterfaceTypeExtension<S, Ty>),
  /// The union type extension.
  Union(UnionTypeExtension<S>),
  /// The enum type extension.
  Enum(EnumTypeExtension<S>),
  /// The input object type extension.
  InputObject(InputObjectTypeExtension<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for TypeExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
      Self::Interface(v) => v.into_span(),
      Self::Union(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::InputObject(v) => v.into_span(),
    }
  }
}

impl<S, Ty> TypeExtension<S, Ty> {
  /// Returns the span of the type extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(v) => v.span(),
      Self::Object(v) => v.span(),
      Self::Interface(v) => v.span(),
      Self::Union(v) => v.span(),
      Self::Enum(v) => v.span(),
      Self::InputObject(v) => v.span(),
    }
  }
}

/// Type system definition (a type, directive, or schema definition).
pub type TypeSystemDefinition<S, Ty = Type<Name<S>>> = scaffold::TypeSystemDefinition<
  TypeDefinition<S, Ty>,
  DirectiveDefinition<S, Ty>,
  SchemaDefinition<S>,
>;

/// Type system definition with an optional leading description.
pub type DescribedTypeSystemDefinition<S, Ty = Type<Name<S>>> =
  Described<TypeSystemDefinition<S, Ty>, S>;

/// Type system extension (a type or schema extension).
pub type TypeSystemExtension<S, Ty = Type<Name<S>>> =
  scaffold::TypeSystemExtension<TypeExtension<S, Ty>, SchemaExtension<S>>;

/// Type system definition or extension.
pub type TypeSystemDefinitionOrExtension<S, Ty = Type<Name<S>>> =
  scaffold::TypeSystemDefinitionOrExtension<
    DescribedTypeSystemDefinition<S, Ty>,
    TypeSystemExtension<S, Ty>,
  >;

/// Executable definition with an optional leading description.
pub type DescribedExecutableDefinition<S, Ty = Type<Name<S>>> =
  Described<ExecutableDefinition<S, Ty>, S>;

/// Definition (type system or executable).
pub type Definition<S, Ty = Type<Name<S>>> =
  scaffold::Definition<TypeSystemDefinition<S, Ty>, ExecutableDefinition<S, Ty>>;

/// Definition with an optional leading description.
pub type DescribedDefinition<S, Ty = Type<Name<S>>> = Described<Definition<S, Ty>, S>;

/// Definition or extension — one entry of a full [`Document`].
pub type DefinitionOrExtension<S, Ty = Type<Name<S>>> =
  scaffold::DefinitionOrExtension<DescribedDefinition<S, Ty>, TypeSystemExtension<S, Ty>>;

/// Type system document (only schema/type definitions and extensions).
pub type TypeSystemDocument<S, Ty = Type<Name<S>>> =
  scaffold::Document<TypeSystemDefinitionOrExtension<S, Ty>>;

/// Full GraphQL document (both type-system and executable entries).
pub type Document<S, Ty = Type<Name<S>>> = scaffold::Document<DefinitionOrExtension<S, Ty>>;
