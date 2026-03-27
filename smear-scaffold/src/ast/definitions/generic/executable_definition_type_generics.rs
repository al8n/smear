use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};
use core::marker::PhantomData;


use std::vec::Vec;

/// A definition type generics with a list of type parameters.
///
/// ```graphqlx
/// <T, U> # A type generics with two type parameters: `T` and `U`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionTypeGenerics<Ident, Container = Vec<Ident>> {
  span: Span,
  params: Container,
  _ident: PhantomData<Ident>,
}

impl<Ident, Container> AsSpan<Span> for ExecutableDefinitionTypeGenerics<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for ExecutableDefinitionTypeGenerics<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for ExecutableDefinitionTypeGenerics<Ident, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Ident, Container> ExecutableDefinitionTypeGenerics<Ident, Container> {
  /// Creates a new `ExecutableDefinitionTypeGenerics` with the given parameters.
  #[inline]
  pub const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _ident: PhantomData,
    }
  }

  /// Returns the span of the type generics.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parameters of the type generics.
  #[inline]
  pub const fn params(&self) -> &Container {
    &self.params
  }

  /// Returns the mutable parameters of the type generics.
  #[inline]
  pub fn params_slice(&self) -> &[Ident]
  where
    Container: AsRef<[Ident]>,
  {
    self.params.as_ref()
  }
}
