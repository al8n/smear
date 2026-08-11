//! GraphQLx document AST aliases and import-aware top-level variants.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{
  DefaultVec, Described, DescribedExecutableDefinition, DescribedTypeSystemDefinition,
  ExecutableDefinition, ImportDefinition, Type, TypeSystemDefinition, TypeSystemExtension,
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

/// A top-level GraphQLx type-system entry, including imports.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ImportOrTypeSystemDefinitionOrExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// An `import` definition.
  Import(ImportDefinition<S, Span>),
  /// A described type-system definition.
  Definition(DescribedTypeSystemDefinition<S, Span, Ty>),
  /// A type-system extension.
  Extension(TypeSystemExtension<S, Span, Ty>),
}

impl<S, Span, Ty> ImportOrTypeSystemDefinitionOrExtension<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Import(import) => import.span(),
      Self::Definition(definition) => definition.span(),
      Self::Extension(extension) => extension.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for ImportOrTypeSystemDefinitionOrExtension<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for ImportOrTypeSystemDefinitionOrExtension<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Import(import) => import.into_span(),
      Self::Definition(definition) => definition.into_span(),
      Self::Extension(extension) => extension.into_span(),
    }
  }
}

/// A top-level GraphQLx executable entry, including imports.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ImportOrExecutableDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// An `import` definition.
  Import(ImportDefinition<S, Span>),
  /// A described executable definition.
  Definition(DescribedExecutableDefinition<S, Span, Ty>),
}

impl<S, Span, Ty> ImportOrExecutableDefinition<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Import(import) => import.span(),
      Self::Definition(definition) => definition.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for ImportOrExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for ImportOrExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Import(import) => import.into_span(),
      Self::Definition(definition) => definition.into_span(),
    }
  }
}

/// A complete top-level GraphQLx entry, including imports and extensions.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ImportOrDefinitionOrExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// An `import` definition.
  Import(ImportDefinition<S, Span>),
  /// A described executable or type-system definition.
  Definition(DescribedDefinition<S, Span, Ty>),
  /// A type-system extension.
  Extension(TypeSystemExtension<S, Span, Ty>),
}

impl<S, Span, Ty> ImportOrDefinitionOrExtension<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Import(import) => import.span(),
      Self::Definition(definition) => definition.span(),
      Self::Extension(extension) => extension.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for ImportOrDefinitionOrExtension<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for ImportOrDefinitionOrExtension<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Import(import) => import.into_span(),
      Self::Definition(definition) => definition.into_span(),
      Self::Extension(extension) => extension.into_span(),
    }
  }
}

/// A GraphQLx type-system document whose entries may include imports.
pub type TypeSystemDocument<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<ImportOrTypeSystemDefinitionOrExtension<S, Span, Ty>>,
> = crate::executable::Document<
  ImportOrTypeSystemDefinitionOrExtension<S, Span, Ty>,
  Span,
  Container,
>;

/// A GraphQLx executable document whose entries may include imports.
pub type ExecutableDocument<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<ImportOrExecutableDefinition<S, Span, Ty>>,
> = crate::executable::Document<ImportOrExecutableDefinition<S, Span, Ty>, Span, Container>;

/// A complete GraphQLx document whose entries may include imports, executable
/// definitions, type-system definitions, and type-system extensions.
pub type Document<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<ImportOrDefinitionOrExtension<S, Span, Ty>>,
> = crate::executable::Document<ImportOrDefinitionOrExtension<S, Span, Ty>, Span, Container>;
