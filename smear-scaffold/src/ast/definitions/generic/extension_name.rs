use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{super::Path, ExtensionTypeGenerics, ExtensionTypeParam};

use std::vec::Vec;

/// The AST for a extension name.
///
/// In the below example, `User<ID, Name>` is a extension name, where `User` is the identifier,
/// and `<ID, Name>` are the [`ExtensionTypeGenerics`].
///
/// ```graphqlx
/// import * as v1 from "comment.graphqlx"
///
/// type User<ID, Name = String> {
///   id: ID!
///   name: Name!
/// }
///
/// extend type User<ID, Name> {
///   age: Int
/// }
///
/// # An extension only works on `User<ID, Name = String>`, not `User<ID, Name>`.
/// extend type User<ID> {
///   field: Int
/// }
///
/// extend type v1::Comment<ID, Name> {
///   id: ID!
///   name: Name!
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionName<
  Ident,
  PathSegmentContainer = Vec<Ident>,
  Container = Vec<ExtensionTypeParam<Ident>>,
> {
  span: Span,
  path: Path<Ident, PathSegmentContainer>,
  generics: Option<ExtensionTypeGenerics<Ident, Container>>,
}

impl<Ident, PathSegmentContainer, Container> AsSpan<Span>
  for ExtensionName<Ident, PathSegmentContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, PathSegmentContainer, Container> IntoSpan<Span>
  for ExtensionName<Ident, PathSegmentContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, PathSegmentContainer, Container> IntoComponents
  for ExtensionName<Ident, PathSegmentContainer, Container>
{
  type Components = (
    Span,
    Path<Ident, PathSegmentContainer>,
    Option<ExtensionTypeGenerics<Ident, Container>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.path, self.generics)
  }
}

impl<Ident, PathSegmentContainer, Container> ExtensionName<Ident, PathSegmentContainer, Container> {
  /// Creates a new `ExtensionName` with the given identifier and optional generics.
  #[inline]
  pub const fn new(
    span: Span,
    path: Path<Ident, PathSegmentContainer>,
    generics: Option<ExtensionTypeGenerics<Ident, Container>>,
  ) -> Self {
    Self {
      span,
      path,
      generics,
    }
  }

  /// Returns the span of the extension name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the path of the extension .
  #[inline]
  pub const fn path(&self) -> &Path<Ident, PathSegmentContainer> {
    &self.path
  }

  /// Returns the optional generics of the extension name.
  #[inline]
  pub const fn generics(&self) -> Option<&ExtensionTypeGenerics<Ident, Container>> {
    self.generics.as_ref()
  }
}
