use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};


use super::{DefinitionTypeGenerics, DefinitionTypeParam};

use std::vec::Vec;

/// The AST for a definition name.
///
/// In the below example, `User<ID, Name = String>` is a definition name, where `User` is the identifier,
/// and `<ID, Name = String>` are the [`DefinitionTypeGenerics`].
///
/// ```graphqlx
/// type User<ID, Name = String> {
///   id: ID!
///   name: Name!
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionName<Ident, Type, Container = Vec<DefinitionTypeParam<Ident, Type>>> {
  span: Span,
  ident: Ident,
  generics: Option<DefinitionTypeGenerics<Ident, Type, Container>>,
}

impl<Ident, Type, Container> AsSpan<Span> for DefinitionName<Ident, Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Container> IntoSpan<Span> for DefinitionName<Ident, Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Container> IntoComponents for DefinitionName<Ident, Type, Container> {
  type Components = (
    Span,
    Ident,
    Option<DefinitionTypeGenerics<Ident, Type, Container>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident, self.generics)
  }
}

impl<Ident, Type, Container> DefinitionName<Ident, Type, Container> {
  /// Creates a new `DefinitionName` with the given identifier and optional generics.
  #[inline]
  pub const fn new(
    span: Span,
    ident: Ident,
    generics: Option<DefinitionTypeGenerics<Ident, Type, Container>>,
  ) -> Self {
    Self {
      span,
      ident,
      generics,
    }
  }

  /// Returns the span of the definition name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the name of the definition.
  #[inline]
  pub const fn name(&self) -> &Ident {
    &self.ident
  }

  /// Returns the optional generics of the definition name.
  #[inline]
  pub const fn generics(&self) -> Option<&DefinitionTypeGenerics<Ident, Type, Container>> {
    self.generics.as_ref()
  }
}
