//! Type-reference carriers shared by the GraphQL-family dialect ASTs.
//!
//! These source-independent nodes retain the referenced name or element type,
//! the complete source span, and whether the reference is non-null.

#[cfg(feature = "graphqlx")]
use std::vec::Vec;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

#[cfg(feature = "graphqlx")]
use crate::path::Path;

/// A named type reference with an optional non-null modifier.
///
/// Vanilla GraphQL names its type references directly; GraphQLx namespaces them and uses this
/// module's `DefinitionTypePath` instead, surfaced publicly as `graphqlx::ast::DefinitionTypePath`.
///
/// Both are code spans and not intra-doc links, deliberately. This module is private, so the
/// carrier has no public path to link to; and it is `graphqlx`-gated while this type is
/// `graphql`-gated, so under `graphql` alone there is no item in scope to resolve against at all.
/// A link here is a hard error under `RUSTDOCFLAGS="-D warnings"`, in both configurations.
#[cfg(feature = "graphql")]
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct NamedType<Name, Span = SimpleSpan> {
  span: Span,
  name: Name,
  required: bool,
}

#[cfg(feature = "graphql")]
impl<Name, Span> AsSpan<Span> for NamedType<Name, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

#[cfg(feature = "graphql")]
impl<Name, Span> IntoSpan<Span> for NamedType<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `NamedType` taken apart: every field, in the order it is written.
#[cfg(feature = "graphql")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NamedTypeComponents<Name, Span> {
  /// The span covering the complete type reference.
  pub span: Span,
  /// The referenced type name.
  pub name: Name,
  /// Whether this type reference is non-null.
  pub required: bool,
}

#[cfg(feature = "graphql")]
impl<Name, Span> IntoComponents for NamedType<Name, Span> {
  type Components = NamedTypeComponents<Name, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    NamedTypeComponents {
      span: self.span,
      name: self.name,
      required: self.required,
    }
  }
}

#[cfg(feature = "graphql")]
impl<Name, Span> NamedType<Name, Span> {
  /// Creates a named type reference.
  #[inline]
  pub const fn new(span: Span, name: Name, required: bool) -> Self
  where
    Name: crate::value::Leaf,
    Span: crate::value::Leaf,
  {
    Self {
      span,
      name,
      required,
    }
  }

  /// Returns the span covering the complete type reference.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the referenced type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns whether this type reference is non-null.
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }
}

/// A list type reference with an optional non-null modifier.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ListType<Type, Span = SimpleSpan> {
  span: Span,
  ty: Type,
  required: bool,
}

impl<Type, Span> AsSpan<Span> for ListType<Type, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type, Span> IntoSpan<Span> for ListType<Type, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `ListType` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ListTypeComponents<Type, Span> {
  /// The span covering the complete type reference.
  pub span: Span,
  /// The element type.
  pub ty: Type,
  /// Whether this type reference is non-null.
  pub required: bool,
}

impl<Type, Span> IntoComponents for ListType<Type, Span> {
  type Components = ListTypeComponents<Type, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    ListTypeComponents {
      span: self.span,
      ty: self.ty,
      required: self.required,
    }
  }
}

impl<Type, Span> ListType<Type, Span> {
  /// Creates a list type reference.
  #[inline]
  pub const fn new(span: Span, ty: Type, required: bool) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self { span, ty, required }
  }

  /// Returns the span covering the complete type reference.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the element type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns whether this type reference is non-null.
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }
}

/// Generic type arguments carried by a GraphQLx type path.
#[cfg(feature = "graphqlx")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenerics<Type, Span = SimpleSpan, Container = Vec<Type>> {
  span: Span,
  params: Container,
  _type: core::marker::PhantomData<Type>,
}

#[cfg(feature = "graphqlx")]
impl<Type, Span, Container> TypeGenerics<Type, Span, Container> {
  /// Creates a type-argument list from its enclosing span and parameters.
  #[inline]
  pub const fn new(span: Span, params: Container) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self {
      span,
      params,
      _type: core::marker::PhantomData,
    }
  }

  /// Returns the span covering `<...>`.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the type parameters as a slice.
  #[inline]
  pub fn params(&self) -> &[Type]
  where
    Container: AsRef<[Type]>,
  {
    self.params.as_ref()
  }

  /// Consumes these generics and returns their parameter container.
  #[inline]
  pub fn into_params(self) -> Container {
    self.params
  }
}

#[cfg(feature = "graphqlx")]
impl<Type, Span, Container> AsSpan<Span> for TypeGenerics<Type, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

#[cfg(feature = "graphqlx")]
impl<Type, Span, Container> IntoSpan<Span> for TypeGenerics<Type, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `TypeGenerics` taken apart: every field, in the order it is written.
#[cfg(feature = "graphqlx")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericsComponents<Span, Container> {
  /// The span covering `<...>`.
  pub span: Span,
  /// The type parameters.
  pub params: Container,
}

#[cfg(feature = "graphqlx")]
impl<Type, Span, Container> IntoComponents for TypeGenerics<Type, Span, Container> {
  type Components = TypeGenericsComponents<Span, Container>;

  #[inline]
  fn into_components(self) -> Self::Components {
    TypeGenericsComponents {
      span: self.span,
      params: self.params,
    }
  }
}

/// A namespaced GraphQLx type path, optional type arguments, and nullability.
#[cfg(feature = "graphqlx")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypePath<
  Name,
  Type,
  Span = SimpleSpan,
  PathContainer = Vec<Name>,
  TypeContainer = Vec<Type>,
> {
  span: Span,
  path: Path<Name, Span, PathContainer>,
  generics: Option<TypeGenerics<Type, Span, TypeContainer>>,
  required: bool,
}

#[cfg(feature = "graphqlx")]
impl<Name, Type, Span, PathContainer, TypeContainer>
  DefinitionTypePath<Name, Type, Span, PathContainer, TypeContainer>
{
  /// Creates a type path from its complete span, path, optional arguments, and
  /// non-null modifier.
  #[inline]
  pub const fn new(
    span: Span,
    path: Path<Name, Span, PathContainer>,
    generics: Option<TypeGenerics<Type, Span, TypeContainer>>,
    required: bool,
  ) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self {
      span,
      path,
      generics,
      required,
    }
  }

  /// Returns the span covering the complete type reference.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the namespaced path.
  #[inline]
  pub const fn path(&self) -> &Path<Name, Span, PathContainer> {
    &self.path
  }

  /// Returns optional generic type arguments.
  #[inline]
  pub const fn type_generics(&self) -> Option<&TypeGenerics<Type, Span, TypeContainer>> {
    self.generics.as_ref()
  }

  /// Returns whether this type reference is non-null.
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }
}

#[cfg(feature = "graphqlx")]
impl<Name, Type, Span, PathContainer, TypeContainer> AsSpan<Span>
  for DefinitionTypePath<Name, Type, Span, PathContainer, TypeContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

#[cfg(feature = "graphqlx")]
impl<Name, Type, Span, PathContainer, TypeContainer> IntoSpan<Span>
  for DefinitionTypePath<Name, Type, Span, PathContainer, TypeContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `DefinitionTypePath` taken apart: every field, in the order it is written.
#[cfg(feature = "graphqlx")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypePathComponents<Name, Type, Span, PathContainer, TypeContainer> {
  /// The span covering the complete type reference.
  pub span: Span,
  /// The namespaced path.
  pub path: Path<Name, Span, PathContainer>,
  /// The optional generic arguments.
  pub generics: Option<TypeGenerics<Type, Span, TypeContainer>>,
  /// Whether this type reference is non-null.
  pub required: bool,
}

#[cfg(feature = "graphqlx")]
impl<Name, Type, Span, PathContainer, TypeContainer> IntoComponents
  for DefinitionTypePath<Name, Type, Span, PathContainer, TypeContainer>
{
  type Components = DefinitionTypePathComponents<Name, Type, Span, PathContainer, TypeContainer>;

  #[inline]
  fn into_components(self) -> Self::Components {
    DefinitionTypePathComponents {
      span: self.span,
      path: self.path,
      generics: self.generics,
      required: self.required,
    }
  }
}

#[cfg(test)]
mod tests;
