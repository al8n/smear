//! GraphQLx import AST nodes.

use core::marker::PhantomData;
use std::vec::Vec;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{InlineStringValue, Name, Path};

/// A named GraphQLx import specifier, optionally renamed with `as Path`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedSpecifier<S, Span = SimpleSpan, PathContainer = Vec<Name<S, Span>>> {
  span: Span,
  name: Name<S, Span>,
  alias: Option<Path<S, Span, PathContainer>>,
}

impl<S, Span, PathContainer> NamedSpecifier<S, Span, PathContainer> {
  /// Creates a named import specifier.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name<S, Span>,
    alias: Option<Path<S, Span, PathContainer>>,
  ) -> Self {
    Self { span, name, alias }
  }

  /// Returns the complete specifier span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the imported name.
  #[inline]
  pub const fn name(&self) -> &Name<S, Span> {
    &self.name
  }

  /// Returns the optional local path alias.
  #[inline]
  pub const fn alias(&self) -> Option<&Path<S, Span, PathContainer>> {
    self.alias.as_ref()
  }
}

impl<S, Span, PathContainer> AsSpan<Span> for NamedSpecifier<S, Span, PathContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, PathContainer> IntoSpan<Span> for NamedSpecifier<S, Span, PathContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Span, PathContainer> IntoComponents for NamedSpecifier<S, Span, PathContainer> {
  type Components = (Span, Name<S, Span>, Option<Path<S, Span, PathContainer>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.alias)
  }
}

/// A wildcard GraphQLx import specifier, optionally renamed with `as Path`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WildcardSpecifier<S, Span = SimpleSpan, PathContainer = Vec<Name<S, Span>>> {
  span: Span,
  alias: Option<Path<S, Span, PathContainer>>,
}

impl<S, Span, PathContainer> WildcardSpecifier<S, Span, PathContainer> {
  /// Creates a wildcard import specifier.
  #[inline]
  pub const fn new(span: Span, alias: Option<Path<S, Span, PathContainer>>) -> Self {
    Self { span, alias }
  }

  /// Returns the complete specifier span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional local path alias.
  #[inline]
  pub const fn alias(&self) -> Option<&Path<S, Span, PathContainer>> {
    self.alias.as_ref()
  }
}

impl<S, Span, PathContainer> AsSpan<Span> for WildcardSpecifier<S, Span, PathContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, PathContainer> IntoSpan<Span> for WildcardSpecifier<S, Span, PathContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Span, PathContainer> IntoComponents for WildcardSpecifier<S, Span, PathContainer> {
  type Components = (Span, Option<Path<S, Span, PathContainer>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.alias)
  }
}

/// A member of a braced GraphQLx import list.
#[derive(
  Debug,
  Clone,
  PartialEq,
  Eq,
  Hash,
  derive_more::From,
  derive_more::IsVariant,
  derive_more::TryUnwrap,
  derive_more::Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ImportMember<S> {
  /// A named import specifier.
  Named(NamedSpecifier<S>),
  /// A wildcard import specifier.
  Wildcard(WildcardSpecifier<S>),
}

impl<S> AsSpan<SimpleSpan> for ImportMember<S> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::Named(value) => value.as_span(),
      Self::Wildcard(value) => value.as_span(),
    }
  }
}

impl<S> IntoSpan<SimpleSpan> for ImportMember<S> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Named(value) => value.into_span(),
      Self::Wildcard(value) => value.into_span(),
    }
  }
}

/// A braced list of GraphQLx import members.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportList<S, Container = Vec<ImportMember<S>>> {
  span: SimpleSpan,
  members: Container,
  _member: PhantomData<ImportMember<S>>,
}

impl<S, Container> ImportList<S, Container> {
  /// Creates an import list from its brace-delimited span and members.
  #[inline]
  pub const fn new(span: SimpleSpan, members: Container) -> Self {
    Self {
      span,
      members,
      _member: PhantomData,
    }
  }

  /// Returns the complete list span.
  #[inline]
  pub const fn span(&self) -> &SimpleSpan {
    &self.span
  }

  /// Returns import members as a slice.
  #[inline]
  pub fn members(&self) -> &[ImportMember<S>]
  where
    Container: AsRef<[ImportMember<S>]>,
  {
    self.members.as_ref()
  }

  /// Consumes this list and returns its member container.
  #[inline]
  pub fn into_members(self) -> Container {
    self.members
  }
}

impl<S, Container> AsSpan<SimpleSpan> for ImportList<S, Container> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    self.span()
  }
}

impl<S, Container> IntoSpan<SimpleSpan> for ImportList<S, Container> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    self.span
  }
}

impl<S, Container> IntoComponents for ImportList<S, Container> {
  type Components = (SimpleSpan, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.members)
  }
}

/// The clause following `import`: a braced member list or a wildcard.
#[derive(
  Debug,
  Clone,
  PartialEq,
  Eq,
  Hash,
  derive_more::From,
  derive_more::IsVariant,
  derive_more::TryUnwrap,
  derive_more::Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ImportClause<S> {
  /// A braced member list.
  List(ImportList<S>),
  /// A wildcard member outside braces.
  Wildcard(WildcardSpecifier<S>),
}

impl<S> AsSpan<SimpleSpan> for ImportClause<S> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::List(value) => value.as_span(),
      Self::Wildcard(value) => value.as_span(),
    }
  }
}

impl<S> IntoSpan<SimpleSpan> for ImportClause<S> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::List(value) => value.into_span(),
      Self::Wildcard(value) => value.into_span(),
    }
  }
}

/// A complete `import <clause> from <inline-string>` definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportDefinition<S> {
  span: SimpleSpan,
  clause: ImportClause<S>,
  file: InlineStringValue<S>,
}

impl<S> ImportDefinition<S> {
  /// Creates an import definition from its complete span, clause, and file
  /// inline string.
  #[inline]
  pub const fn new(span: SimpleSpan, clause: ImportClause<S>, file: InlineStringValue<S>) -> Self {
    Self { span, clause, file }
  }

  /// Returns the complete definition span.
  #[inline]
  pub const fn span(&self) -> &SimpleSpan {
    &self.span
  }

  /// Returns the imported clause.
  #[inline]
  pub const fn clause(&self) -> &ImportClause<S> {
    &self.clause
  }

  /// Returns the module file inline string.
  #[inline]
  pub const fn file(&self) -> &InlineStringValue<S> {
    &self.file
  }
}

impl<S> AsSpan<SimpleSpan> for ImportDefinition<S> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    self.span()
  }
}

impl<S> IntoSpan<SimpleSpan> for ImportDefinition<S> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    self.span
  }
}

impl<S> IntoComponents for ImportDefinition<S> {
  type Components = (SimpleSpan, ImportClause<S>, InlineStringValue<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.clause, self.file)
  }
}
