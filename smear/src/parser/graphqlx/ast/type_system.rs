//! GraphQLx-specialized type-system (SDL) AST aliases and enums.
//!
//! The shared type-system carriers retain standard GraphQL structure. This
//! module binds their names to GraphQLx paths and generic wrappers, including
//! the `where` positions supported by the GraphQLx grammar.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{
  ConstDirectives, Constrained, DefaultInputValue, DefaultVec, DefinitionName, Described,
  ExtensionName, Name, OperationType, Type, TypePath, WhereClause,
};

pub use crate::parser::type_system::{
  EnumTypeExtensionData, ExecutableDirectiveLocation, InputObjectTypeExtensionData,
  InterfaceTypeExtensionData, Location, ObjectTypeExtensionData, SchemaExtensionData,
  TypeSystemDirectiveLocation, UnionTypeExtensionData,
};

/// A GraphQLx list of directive locations.
pub type DirectiveLocations<Span = SimpleSpan, Container = DefaultVec<Location<Span>>> =
  crate::parser::type_system::DirectiveLocations<Location<Span>, Container, Span>;

/// An `implements` clause whose interfaces are generic-capable GraphQLx paths.
pub type ImplementInterfaces<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<TypePath<S, Span, Ty>>,
> = crate::parser::type_system::ImplementInterfaces<TypePath<S, Span, Ty>, Container, Span>;

/// A GraphQLx union-member clause whose members are generic-capable paths.
pub type UnionMemberTypes<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<TypePath<S, Span, Ty>>,
> = crate::parser::type_system::UnionMemberTypes<TypePath<S, Span, Ty>, Container, Span>;

/// Definition of the arguments accepted by a field or directive.
pub type ArgumentsDefinition<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<InputValueDefinition<S, Span, Ty>>,
> = crate::parser::type_system::ArgumentsDefinition<
  InputValueDefinition<S, Span, Ty>,
  Container,
  Span,
>;

/// A described GraphQLx input value definition.
pub type InputValueDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> = Described<
  crate::parser::type_system::InputValueDefinition<
    Name<S, Span>,
    Ty,
    DefaultInputValue<S, Span>,
    ConstDirectives<S, Span>,
    Span,
  >,
  S,
  Span,
>;

/// A braced collection of GraphQLx input value definitions.
pub type InputFieldsDefinition<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<InputValueDefinition<S, Span, Ty>>,
> = crate::parser::type_system::InputFieldsDefinition<
  InputValueDefinition<S, Span, Ty>,
  Container,
  Span,
>;

/// A described GraphQLx field definition.
pub type FieldDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> = Described<
  crate::parser::type_system::FieldDefinition<
    Name<S, Span>,
    ArgumentsDefinition<S, Span, Ty>,
    Ty,
    ConstDirectives<S, Span>,
    Span,
  >,
  S,
  Span,
>;

/// A braced collection of GraphQLx field definitions.
pub type FieldsDefinition<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<FieldDefinition<S, Span, Ty>>,
> = crate::parser::type_system::FieldsDefinition<FieldDefinition<S, Span, Ty>, Container, Span>;

/// A GraphQLx field collection preceded by an optional `where` clause.
pub type ConstrainedFieldsDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Constrained<FieldsDefinition<S, Span, Ty>, WhereClause<S, Span, Ty>, Span>;

/// A GraphQLx input-field collection preceded by an optional `where` clause.
pub type ConstrainedInputFieldsDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Constrained<InputFieldsDefinition<S, Span, Ty>, WhereClause<S, Span, Ty>, Span>;

/// A described GraphQLx enum value definition.
pub type EnumValueDefinition<S, Span = SimpleSpan> = Described<
  crate::parser::type_system::EnumValueDefinition<Name<S, Span>, ConstDirectives<S, Span>, Span>,
  S,
  Span,
>;

/// A braced collection of GraphQLx enum value definitions.
pub type EnumValuesDefinition<
  S,
  Span = SimpleSpan,
  Container = DefaultVec<EnumValueDefinition<S, Span>>,
> = crate::parser::type_system::EnumValuesDefinition<EnumValueDefinition<S, Span>, Container, Span>;

/// A GraphQLx scalar type definition with optional generic parameters.
pub type ScalarTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::ScalarTypeDefinition<
    DefinitionName<S, Span, Ty>,
    ConstDirectives<S, Span>,
    Span,
  >;

/// A described GraphQLx scalar type definition.
pub type DescribedScalarTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<ScalarTypeDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx object type definition with generic declaration and optional
/// `where` clause before its fields.
pub type ObjectTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::ObjectTypeDefinition<
    DefinitionName<S, Span, Ty>,
    ImplementInterfaces<S, Span, Ty>,
    ConstDirectives<S, Span>,
    ConstrainedFieldsDefinition<S, Span, Ty>,
    Span,
  >;

/// A described GraphQLx object type definition.
pub type DescribedObjectTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<ObjectTypeDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx interface type definition with generic declaration and optional
/// `where` clause before its fields.
pub type InterfaceTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::InterfaceTypeDefinition<
    DefinitionName<S, Span, Ty>,
    ImplementInterfaces<S, Span, Ty>,
    ConstDirectives<S, Span>,
    ConstrainedFieldsDefinition<S, Span, Ty>,
    Span,
  >;

/// A described GraphQLx interface type definition.
pub type DescribedInterfaceTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<InterfaceTypeDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx union type definition with generic declaration and optional
/// `where` clause before its member paths.
pub type UnionTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::UnionTypeDefinition<
    DefinitionName<S, Span, Ty>,
    ConstDirectives<S, Span>,
    Constrained<UnionMemberTypes<S, Span, Ty>, WhereClause<S, Span, Ty>, Span>,
    Span,
  >;

/// A described GraphQLx union type definition.
pub type DescribedUnionTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<UnionTypeDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx enum type definition with optional generic parameters.
pub type EnumTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::EnumTypeDefinition<
    DefinitionName<S, Span, Ty>,
    ConstDirectives<S, Span>,
    EnumValuesDefinition<S, Span>,
    Span,
  >;

/// A described GraphQLx enum type definition.
pub type DescribedEnumTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<EnumTypeDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx input object type definition with generic declaration and
/// optional `where` clause before its input fields.
pub type InputObjectTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::InputObjectTypeDefinition<
    DefinitionName<S, Span, Ty>,
    ConstDirectives<S, Span>,
    ConstrainedInputFieldsDefinition<S, Span, Ty>,
    Span,
  >;

/// A described GraphQLx input object type definition.
pub type DescribedInputObjectTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<InputObjectTypeDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx root operation type definition with a generic-capable path.
pub type RootOperationTypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::RootOperationTypeDefinition<
    TypePath<S, Span, Ty>,
    OperationType<Span>,
    Span,
  >;

/// A braced collection of GraphQLx root operation type definitions.
pub type RootOperationTypesDefinition<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<RootOperationTypeDefinition<S, Span, Ty>>,
> = crate::parser::type_system::RootOperationTypesDefinition<
  RootOperationTypeDefinition<S, Span, Ty>,
  Container,
  Span,
>;

/// A GraphQLx schema definition.
pub type SchemaDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::SchemaDefinition<
    ConstDirectives<S, Span>,
    RootOperationTypesDefinition<S, Span, Ty>,
    Span,
  >;

/// A described GraphQLx schema definition.
pub type DescribedSchemaDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<SchemaDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx directive definition with generic declaration and an optional
/// `where` clause after its directive locations.
pub type DirectiveDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::DirectiveDefinition<
    DefinitionName<S, Span, Ty>,
    ArgumentsDefinition<S, Span, Ty>,
    Constrained<DirectiveLocations<Span>, WhereClause<S, Span, Ty>, Span>,
    Span,
  >;

/// A GraphQLx scalar type extension with a generic-capable extension path.
pub type ScalarTypeExtension<S, Span = SimpleSpan> =
  crate::parser::type_system::ScalarTypeExtension<
    ExtensionName<S, Span>,
    ConstDirectives<S, Span>,
    Span,
  >;

/// A GraphQLx object type extension with generic arguments and an optional
/// `where` clause before its fields.
pub type ObjectTypeExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::ObjectTypeExtension<
    ExtensionName<S, Span>,
    ImplementInterfaces<S, Span, Ty>,
    ConstDirectives<S, Span>,
    ConstrainedFieldsDefinition<S, Span, Ty>,
    Span,
  >;

/// A GraphQLx interface type extension with generic arguments and an optional
/// `where` clause before its fields.
pub type InterfaceTypeExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::InterfaceTypeExtension<
    ExtensionName<S, Span>,
    ImplementInterfaces<S, Span, Ty>,
    ConstDirectives<S, Span>,
    ConstrainedFieldsDefinition<S, Span, Ty>,
    Span,
  >;

/// A GraphQLx union type extension with generic arguments and an optional
/// `where` clause before its member paths.
pub type UnionTypeExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::UnionTypeExtension<
    ExtensionName<S, Span>,
    ConstDirectives<S, Span>,
    Constrained<UnionMemberTypes<S, Span, Ty>, WhereClause<S, Span, Ty>, Span>,
    Span,
  >;

/// A GraphQLx enum type extension with a generic-capable extension path.
pub type EnumTypeExtension<S, Span = SimpleSpan> = crate::parser::type_system::EnumTypeExtension<
  ExtensionName<S, Span>,
  ConstDirectives<S, Span>,
  EnumValuesDefinition<S, Span>,
  Span,
>;

/// A GraphQLx input object type extension with generic arguments and an
/// optional `where` clause before its input fields.
pub type InputObjectTypeExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::InputObjectTypeExtension<
    ExtensionName<S, Span>,
    ConstDirectives<S, Span>,
    ConstrainedInputFieldsDefinition<S, Span, Ty>,
    Span,
  >;

/// A GraphQLx schema extension.
pub type SchemaExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::SchemaExtension<
    ConstDirectives<S, Span>,
    RootOperationTypesDefinition<S, Span, Ty>,
    Span,
  >;

/// A GraphQLx named type definition.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A scalar type definition.
  Scalar(ScalarTypeDefinition<S, Span, Ty>),
  /// An object type definition.
  Object(ObjectTypeDefinition<S, Span, Ty>),
  /// An interface type definition.
  Interface(InterfaceTypeDefinition<S, Span, Ty>),
  /// A union type definition.
  Union(UnionTypeDefinition<S, Span, Ty>),
  /// An enum type definition.
  Enum(EnumTypeDefinition<S, Span, Ty>),
  /// An input object type definition.
  InputObject(InputObjectTypeDefinition<S, Span, Ty>),
}

impl<S, Span, Ty> TypeDefinition<S, Span, Ty> {
  /// Returns the complete type-definition span.
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

impl<S, Span, Ty> AsSpan<Span> for TypeDefinition<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for TypeDefinition<S, Span, Ty> {
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

/// A GraphQLx named type extension.
pub type TypeExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::TypeExtension<
    ScalarTypeExtension<S, Span>,
    ObjectTypeExtension<S, Span, Ty>,
    InterfaceTypeExtension<S, Span, Ty>,
    UnionTypeExtension<S, Span, Ty>,
    EnumTypeExtension<S, Span>,
    InputObjectTypeExtension<S, Span, Ty>,
  >;

/// A GraphQLx type-system definition: a named type, directive, or schema.
pub type TypeSystemDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::TypeSystemDefinition<
    TypeDefinition<S, Span, Ty>,
    DirectiveDefinition<S, Span, Ty>,
    SchemaDefinition<S, Span, Ty>,
  >;

/// A described GraphQLx type-system definition.
pub type DescribedTypeSystemDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<TypeSystemDefinition<S, Span, Ty>, S, Span>;

/// A GraphQLx type-system extension: a named type or schema extension.
pub type TypeSystemExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::TypeSystemExtension<
    TypeExtension<S, Span, Ty>,
    SchemaExtension<S, Span, Ty>,
  >;

/// A described GraphQLx type-system definition or a type-system extension.
pub type TypeSystemDefinitionOrExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::type_system::TypeSystemDefinitionOrExtension<
    DescribedTypeSystemDefinition<S, Span, Ty>,
    TypeSystemExtension<S, Span, Ty>,
  >;
