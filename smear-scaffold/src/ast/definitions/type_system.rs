use derive_more::{IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

/// Type definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeDefinition<
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
> {
  /// A scalar type definition.
  Scalar(ScalarTypeDefinition),
  /// An object type definition.
  Object(ObjectTypeDefinition),
  /// An interface type definition.
  Interface(InterfaceTypeDefinition),
  /// A union type definition.
  Union(UnionTypeDefinition),
  /// An enum type definition.
  Enum(EnumTypeDefinition),
  /// An input object type definition.
  InputObject(InputObjectTypeDefinition),
}

impl<
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
> AsSpan<Span>
  for TypeDefinition<
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    InputObjectTypeDefinition,
  >
where
  ScalarTypeDefinition: AsSpan<Span>,
  ObjectTypeDefinition: AsSpan<Span>,
  InterfaceTypeDefinition: AsSpan<Span>,
  UnionTypeDefinition: AsSpan<Span>,
  EnumTypeDefinition: AsSpan<Span>,
  InputObjectTypeDefinition: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.as_span(),
      Self::InputObject(i) => i.as_span(),
      Self::Object(o) => o.as_span(),
      Self::Interface(i) => i.as_span(),
      Self::Union(u) => u.as_span(),
      Self::Enum(e) => e.as_span(),
    }
  }
}

impl<
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
> IntoSpan<Span>
  for TypeDefinition<
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    InputObjectTypeDefinition,
  >
where
  ScalarTypeDefinition: IntoSpan<Span>,
  ObjectTypeDefinition: IntoSpan<Span>,
  InterfaceTypeDefinition: IntoSpan<Span>,
  UnionTypeDefinition: IntoSpan<Span>,
  EnumTypeDefinition: IntoSpan<Span>,
  InputObjectTypeDefinition: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
      Self::Union(u) => u.into_span(),
      Self::Enum(e) => e.into_span(),
    }
  }
}

/// Type extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeExtension<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> {
  /// A scalar type extension.
  Scalar(ScalarTypeExtension),
  /// An object type extension.
  Object(ObjectTypeExtension),
  /// An interface type extension.
  Interface(InterfaceTypeExtension),
  /// A union type extension.
  Union(UnionTypeExtension),
  /// An enum type extension.
  Enum(EnumTypeExtension),
  /// An input object type extension.
  InputObject(InputObjectTypeExtension),
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> AsSpan<Span>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: AsSpan<Span>,
  ObjectTypeExtension: AsSpan<Span>,
  InterfaceTypeExtension: AsSpan<Span>,
  UnionTypeExtension: AsSpan<Span>,
  EnumTypeExtension: AsSpan<Span>,
  InputObjectTypeExtension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.as_span(),
      Self::InputObject(i) => i.as_span(),
      Self::Object(o) => o.as_span(),
      Self::Interface(i) => i.as_span(),
      Self::Union(u) => u.as_span(),
      Self::Enum(e) => e.as_span(),
    }
  }
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> IntoSpan<Span>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: IntoSpan<Span>,
  ObjectTypeExtension: IntoSpan<Span>,
  InterfaceTypeExtension: IntoSpan<Span>,
  UnionTypeExtension: IntoSpan<Span>,
  EnumTypeExtension: IntoSpan<Span>,
  InputObjectTypeExtension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
      Self::Union(u) => u.into_span(),
      Self::Enum(e) => e.into_span(),
    }
  }
}

/// Type system definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition> {
  /// A type definition.
  Type(TypeDefinition),
  /// A directive definition.
  Directive(DirectiveDefinition),
  /// A schema definition.
  Schema(SchemaDefinition),
}

impl<TypeDefinition, DirectiveDefinition, SchemaDefinition> AsSpan<Span>
  for TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
where
  TypeDefinition: AsSpan<Span>,
  DirectiveDefinition: AsSpan<Span>,
  SchemaDefinition: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Type(t) => t.as_span(),
      Self::Directive(d) => d.as_span(),
      Self::Schema(s) => s.as_span(),
    }
  }
}

impl<TypeDefinition, DirectiveDefinition, SchemaDefinition> IntoSpan<Span>
  for TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
where
  TypeDefinition: IntoSpan<Span>,
  DirectiveDefinition: IntoSpan<Span>,
  SchemaDefinition: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Directive(d) => d.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

/// Type system extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemExtension<TypeExtension, SchemaExtension> {
  /// A type extension.
  Type(TypeExtension),
  /// A schema extension.
  Schema(SchemaExtension),
}

impl<TypeExtension, SchemaExtension> AsSpan<Span>
  for TypeSystemExtension<TypeExtension, SchemaExtension>
where
  TypeExtension: AsSpan<Span>,
  SchemaExtension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Type(t) => t.as_span(),
      Self::Schema(s) => s.as_span(),
    }
  }
}

impl<TypeExtension, SchemaExtension> IntoSpan<Span>
  for TypeSystemExtension<TypeExtension, SchemaExtension>
where
  TypeExtension: IntoSpan<Span>,
  SchemaExtension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

/// Type system definition or extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinitionOrExtension<Definition, Extension> {
  /// A type system definition.
  Definition(Definition),
  /// A type system extension.
  Extension(Extension),
}

impl<Definition, Extension> AsSpan<Span> for TypeSystemDefinitionOrExtension<Definition, Extension>
where
  Definition: AsSpan<Span>,
  Extension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Definition(d) => d.as_span(),
      Self::Extension(e) => e.as_span(),
    }
  }
}

impl<Definition, Extension> IntoSpan<Span>
  for TypeSystemDefinitionOrExtension<Definition, Extension>
where
  Definition: IntoSpan<Span>,
  Extension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}

/// Executable definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDefinition<OperationDefinition, FragmentDefinition> {
  /// An operation definition.
  Operation(OperationDefinition),
  /// A fragment definition.
  Fragment(FragmentDefinition),
}

impl<OperationDefinition, FragmentDefinition> AsSpan<Span>
  for ExecutableDefinition<OperationDefinition, FragmentDefinition>
where
  OperationDefinition: AsSpan<Span>,
  FragmentDefinition: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Operation(o) => o.as_span(),
      Self::Fragment(f) => f.as_span(),
    }
  }
}

impl<OperationDefinition, FragmentDefinition> IntoSpan<Span>
  for ExecutableDefinition<OperationDefinition, FragmentDefinition>
where
  OperationDefinition: IntoSpan<Span>,
  FragmentDefinition: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Operation(o) => o.into_span(),
      Self::Fragment(f) => f.into_span(),
    }
  }
}

/// A definition of a GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Definition<TypeSystem, Executable> {
  /// A type system definition or extension.
  TypeSystem(TypeSystem),
  /// An executable definition.
  Executable(Executable),
}

impl<TypeSystem, Executable> AsSpan<Span> for Definition<TypeSystem, Executable>
where
  TypeSystem: AsSpan<Span>,
  Executable: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::TypeSystem(t) => t.as_span(),
      Self::Executable(e) => e.as_span(),
    }
  }
}

impl<TypeSystem, Executable> IntoSpan<Span> for Definition<TypeSystem, Executable>
where
  TypeSystem: IntoSpan<Span>,
  Executable: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::TypeSystem(t) => t.into_span(),
      Self::Executable(e) => e.into_span(),
    }
  }
}

/// A definition or extension of a GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum DefinitionOrExtension<Definition, Extension> {
  /// A definition.
  Definition(Definition),
  /// An extension.
  Extension(Extension),
}

impl<Definition, Extension> AsSpan<Span> for DefinitionOrExtension<Definition, Extension>
where
  Definition: AsSpan<Span>,
  Extension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Definition(d) => d.as_span(),
      Self::Extension(e) => e.as_span(),
    }
  }
}

impl<Definition, Extension> IntoSpan<Span> for DefinitionOrExtension<Definition, Extension>
where
  Definition: IntoSpan<Span>,
  Extension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}

/// Type system definition or extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ImportOrTypeSystemDefinitionOrExtension<Import, Definition, Extension> {
  /// An import.
  Import(Import),
  /// A type system definition.
  Definition(Definition),
  /// A type system extension.
  Extension(Extension),
}

impl<Import, Definition, Extension> AsSpan<Span>
  for ImportOrTypeSystemDefinitionOrExtension<Import, Definition, Extension>
where
  Import: AsSpan<Span>,
  Definition: AsSpan<Span>,
  Extension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Import(i) => i.as_span(),
      Self::Definition(d) => d.as_span(),
      Self::Extension(e) => e.as_span(),
    }
  }
}

impl<Import, Definition, Extension> IntoSpan<Span>
  for ImportOrTypeSystemDefinitionOrExtension<Import, Definition, Extension>
where
  Import: IntoSpan<Span>,
  Definition: IntoSpan<Span>,
  Extension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Import(i) => i.into_span(),
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}

/// Executable definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ImportOrExecutableDefinition<Import, Executable> {
  /// An import definition.
  Import(Import),
  /// An executable definition.
  Executable(Executable),
}

impl<Import, Executable> AsSpan<Span> for ImportOrExecutableDefinition<Import, Executable>
where
  Import: AsSpan<Span>,
  Executable: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Import(i) => i.as_span(),
      Self::Executable(e) => e.as_span(),
    }
  }
}

impl<Import, Executable> IntoSpan<Span> for ImportOrExecutableDefinition<Import, Executable>
where
  Import: IntoSpan<Span>,
  Executable: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Import(i) => i.into_span(),
      Self::Executable(e) => e.into_span(),
    }
  }
}

/// An import, definition or extension of a GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ImportOrDefinitionOrExtension<Import, Definition, Extension> {
  /// An import.
  Import(Import),
  /// A definition.
  Definition(Definition),
  /// An extension.
  Extension(Extension),
}

impl<Import, Definition, Extension> AsSpan<Span>
  for ImportOrDefinitionOrExtension<Import, Definition, Extension>
where
  Import: AsSpan<Span>,
  Definition: AsSpan<Span>,
  Extension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Definition(d) => d.as_span(),
      Self::Extension(e) => e.as_span(),
      Self::Import(i) => i.as_span(),
    }
  }
}

impl<Import, Definition, Extension> IntoSpan<Span>
  for ImportOrDefinitionOrExtension<Import, Definition, Extension>
where
  Import: IntoSpan<Span>,
  Definition: IntoSpan<Span>,
  Extension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Import(i) => i.into_span(),
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}
