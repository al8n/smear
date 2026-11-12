//! GraphQLx document AST aliases and the top-level entries `use` items stand among.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{
  DefaultVec, Described, DescribedExecutableDefinition, DescribedTypeSystemDefinition,
  ExecutableDefinition, Type, TypeSystemDefinition, TypeSystemExtension, UseDefinition,
};

/// A GraphQLx definition: either type-system or executable syntax.
pub type Definition<S, Span = SimpleSpan, Ty = Type<S, Span>> = crate::executable::Definition<
  TypeSystemDefinition<S, Span, Ty>,
  ExecutableDefinition<S, Span, Ty>,
>;

/// A described GraphQLx definition.
pub type DescribedDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<Definition<S, Span, Ty>, S, Span>;

/// A described GraphQLx definition or a type-system extension.
pub type DefinitionOrExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::executable::DefinitionOrExtension<
    DescribedDefinition<S, Span, Ty>,
    TypeSystemExtension<S, Span, Ty>,
  >;

/// A top-level GraphQLx type-system entry: a `use` item, a definition or an extension.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemItem<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A `use` item.
  Use(UseDefinition<S, Span>),
  /// A described type-system definition.
  Definition(DescribedTypeSystemDefinition<S, Span, Ty>),
  /// A type-system extension.
  Extension(TypeSystemExtension<S, Span, Ty>),
}

impl<S, Span, Ty> TypeSystemItem<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Use(item) => item.span(),
      Self::Definition(definition) => definition.span(),
      Self::Extension(extension) => extension.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for TypeSystemItem<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for TypeSystemItem<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Use(item) => item.into_span(),
      Self::Definition(definition) => definition.into_span(),
      Self::Extension(extension) => extension.into_span(),
    }
  }
}

/// A top-level GraphQLx executable entry: a `use` item or an executable definition.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum UseOrExecutableDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A `use` item.
  Use(UseDefinition<S, Span>),
  /// A described executable definition.
  Definition(DescribedExecutableDefinition<S, Span, Ty>),
}

impl<S, Span, Ty> UseOrExecutableDefinition<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Use(item) => item.span(),
      Self::Definition(definition) => definition.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for UseOrExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for UseOrExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Use(item) => item.into_span(),
      Self::Definition(definition) => definition.into_span(),
    }
  }
}

/// A complete top-level GraphQLx entry: a `use` item, an executable or type-system definition, or
/// a type-system extension.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum DocumentItem<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A `use` item.
  Use(UseDefinition<S, Span>),
  /// A described executable or type-system definition.
  Definition(DescribedDefinition<S, Span, Ty>),
  /// A type-system extension.
  Extension(TypeSystemExtension<S, Span, Ty>),
}

impl<S, Span, Ty> DocumentItem<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Use(item) => item.span(),
      Self::Definition(definition) => definition.span(),
      Self::Extension(extension) => extension.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for DocumentItem<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for DocumentItem<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Use(item) => item.into_span(),
      Self::Definition(definition) => definition.into_span(),
      Self::Extension(extension) => extension.into_span(),
    }
  }
}

/// A GraphQLx type-system document, whose entries are [`TypeSystemItem`]s.
pub type TypeSystemDocument<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<TypeSystemItem<S, Span, Ty>>,
> = crate::executable::Document<TypeSystemItem<S, Span, Ty>, Span, Container>;

/// A GraphQLx executable document, whose entries are `use` items and executable definitions.
pub type ExecutableDocument<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<UseOrExecutableDefinition<S, Span, Ty>>,
> = crate::executable::Document<UseOrExecutableDefinition<S, Span, Ty>, Span, Container>;

/// A complete GraphQLx document, whose entries may be `use` items, executable definitions,
/// type-system definitions and type-system extensions.
pub type Document<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<DocumentItem<S, Span, Ty>>,
> = crate::executable::Document<DocumentItem<S, Span, Ty>, Span, Container>;
