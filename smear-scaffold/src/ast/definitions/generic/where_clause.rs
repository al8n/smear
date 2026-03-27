use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};
use core::marker::PhantomData;


use super::TypePath;

use std::vec::Vec;

/// A where predicate, which constrains a type to implement certain interfaces.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate<
  Ident,
  Type,
  PathSegmentsContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
  Container = Vec<TypePath<Ident, Type>>,
> {
  span: Span,
  bounded_type: TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>,
  bounds: Container,
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container> AsSpan<Span>
  for WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container> IntoSpan<Span>
  for WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container> IntoComponents
  for WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  type Components = (
    Span,
    TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>,
    Container,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.bounded_type, self.bounds)
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
  WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  /// Creates a new `WherePredicate` with the given bounded type and bounds.
  #[inline]
  pub const fn new(
    span: Span,
    bounded_type: TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>,
    bounds: Container,
  ) -> Self {
    Self {
      span,
      bounded_type,
      bounds,
    }
  }

  /// Returns the span of the where predicate.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the bounded type.
  #[inline]
  pub const fn bounded_type(&self) -> &TypePath<Ident, Type, PathSegmentsContainer, TypeContainer> {
    &self.bounded_type
  }

  /// Returns the bounds.
  #[inline]
  pub const fn bounds(&self) -> &Container {
    &self.bounds
  }

  /// Returns the bounds as a slice.
  #[inline]
  pub fn bounds_slice(&self) -> &[TypePath<Ident, Type>]
  where
    Container: AsRef<[TypePath<Ident, Type>]>,
  {
    self.bounds().as_ref()
  }
}

/// A where clause, which contains a list of where predicates.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause<
  Ident,
  Type,
  PathSegmentsContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
  TypePathsContainer = Vec<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>>,
  Container = Vec<WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer>>,
> {
  span: Span,
  predicates: Container,
  _m: PhantomData<
    WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
  >,
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container> AsSpan<Span>
  for WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoSpan<Span>
  for WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoComponents
  for WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.predicates)
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  /// Creates a new `WhereClause` with the given predicates.
  #[inline]
  pub const fn new(span: Span, predicates: Container) -> Self {
    Self {
      span,
      predicates,
      _m: PhantomData,
    }
  }

  /// Returns the span of the where clause.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the predicates.
  #[inline]
  pub const fn predicates(&self) -> &Container {
    &self.predicates
  }

  /// Returns the predicates as a slice.
  #[inline]
  pub fn predicates_slice(
    &self,
  ) -> &[WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>]
  where
    Container: AsRef<
      [WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>],
    >,
  {
    self.predicates.as_ref()
  }
}

/// A constrained `Target`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constrained<
  Ident,
  Type,
  Target,
  PathSegmentsContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
  TypePathsContainer = Vec<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>>,
  Container = Vec<
    WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
  >,
> {
  span: Span,
  where_clause: Option<
    WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
  >,
  target: Target,
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  AsSpan<Span>
  for Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoSpan<Span>
  for Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoComponents
  for Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  type Components = (
    Span,
    Option<
      WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
    >,
    Target,
  );

  fn into_components(self) -> Self::Components {
    (self.span, self.where_clause, self.target)
  }
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  /// Creates a new `Constrained` with the given target and optional where clause.
  #[inline]
  pub const fn new(
    span: Span,
    where_clause: Option<
      WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
    >,
    target: Target,
  ) -> Self {
    Self {
      span,
      where_clause,
      target,
    }
  }

  /// Returns the span of the constrained type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional where clause.
  #[inline]
  pub const fn where_clause(
    &self,
  ) -> Option<
    &WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
  > {
    self.where_clause.as_ref()
  }

  /// Returns the target.
  #[inline]
  pub const fn target(&self) -> &Target {
    &self.target
  }
}
