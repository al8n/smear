use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{super::Path, TypeGenerics};

use std::vec::Vec;

/// A GraphQLx type path.
///
/// ## Example
///
/// ```graphqlx
/// User<ID, Name>
/// v1::Comment<ID, Name>
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypePath<Ident, Type, PathSegmentContainer = Vec<Ident>, TypeContainer = Vec<Type>> {
  span: Span,
  path: Path<Ident, PathSegmentContainer>,
  generics: Option<TypeGenerics<Type, TypeContainer>>,
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> AsSpan<Span>
  for TypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> IntoSpan<Span>
  for TypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> IntoComponents
  for TypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  type Components = (
    Span,
    Path<Ident, PathSegmentContainer>,
    Option<TypeGenerics<Type, TypeContainer>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.path, self.generics)
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer>
  TypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  /// Creates a new path from the given segments.
  #[inline]
  pub const fn new(
    span: Span,
    path: Path<Ident, PathSegmentContainer>,
    generics: Option<TypeGenerics<Type, TypeContainer>>,
  ) -> Self {
    Self {
      span,
      path,
      generics,
    }
  }

  /// Returns a mutable reference to the path.
  #[inline]
  pub const fn path_mut(&mut self) -> &mut Path<Ident, PathSegmentContainer> {
    &mut self.path
  }

  /// Returns the path.
  #[inline]
  pub const fn path(&self) -> &Path<Ident, PathSegmentContainer> {
    &self.path
  }

  /// Returns the type generics.
  #[inline]
  pub const fn type_generics(&self) -> Option<&TypeGenerics<Type, TypeContainer>> {
    self.generics.as_ref()
  }

  /// Returns the span of the path.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
}
