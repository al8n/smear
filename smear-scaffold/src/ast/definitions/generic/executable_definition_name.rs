use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::ExecutableDefinitionTypeGenerics;

use std::vec::Vec;

/// The AST for a definition name.
///
/// In the below example, `User<ID, Name = String>` is a definition name, where `User` is the identifier,
/// and `<ID, Name = String>` are the [`DefinitionTypeGenerics`].
///
/// ```graphqlx
/// fragment<ID, Name = String> userFragment<Name> on User<ID, Name> {
///   id,
///   name,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionName<Ident, Container = Vec<Ident>> {
  span: Span,
  ident: Ident,
  generics: Option<ExecutableDefinitionTypeGenerics<Ident, Container>>,
}

impl<Ident, Container> AsSpan<Span> for ExecutableDefinitionName<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for ExecutableDefinitionName<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for ExecutableDefinitionName<Ident, Container> {
  type Components = (
    Span,
    Ident,
    Option<ExecutableDefinitionTypeGenerics<Ident, Container>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident, self.generics)
  }
}

impl<Ident, Container> ExecutableDefinitionName<Ident, Container> {
  /// Creates a new `ExecutableDefinitionName` with the given identifier and optional generics.
  #[inline]
  pub const fn new(
    span: Span,
    ident: Ident,
    generics: Option<ExecutableDefinitionTypeGenerics<Ident, Container>>,
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

  /// Returns the identifier of the definition name.
  #[inline]
  pub const fn ident(&self) -> &Ident {
    &self.ident
  }

  /// Returns the optional generics of the definition name.
  #[inline]
  pub const fn generics(&self) -> Option<&ExecutableDefinitionTypeGenerics<Ident, Container>> {
    self.generics.as_ref()
  }
}
