use core::marker::PhantomData;
use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use std::vec::Vec;

/// A extension type parameter with an optional default type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionTypeParam<Ident> {
  span: Span,
  ident: Ident,
}

impl<Ident> AsSpan<Span> for ExtensionTypeParam<Ident> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident> IntoSpan<Span> for ExtensionTypeParam<Ident> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident> IntoComponents for ExtensionTypeParam<Ident> {
  type Components = (Span, Ident);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident)
  }
}

impl<Ident> ExtensionTypeParam<Ident> {
  /// Creates a new `ExtensionTypeParam` with the given identifier and optional default type.
  #[inline]
  pub const fn new(span: Span, ident: Ident) -> Self {
    Self { span, ident }
  }

  /// Returns the span of the type parameter.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the identifier of the type parameter.
  #[inline]
  pub const fn ident(&self) -> &Ident {
    &self.ident
  }
}

/// A extension type generics with a list of type parameters.
///
/// ```graphqlx
/// <T, U = String> # A type generics with two type parameters: `T` and `U` where `U` has a default type of `String`
/// <T, U> # A type generics with two type parameters: `T` and `U`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionTypeGenerics<Ident, Container = Vec<ExtensionTypeParam<Ident>>> {
  span: Span,
  params: Container,
  _ident: PhantomData<Ident>,
}

impl<Ident, Container> AsSpan<Span> for ExtensionTypeGenerics<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for ExtensionTypeGenerics<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for ExtensionTypeGenerics<Ident, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Ident, Container> ExtensionTypeGenerics<Ident, Container> {
  /// Creates a new `ExtensionTypeGenerics` with the given parameters.
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
  pub fn params_slice(&self) -> &[ExtensionTypeParam<Ident>]
  where
    Container: AsRef<[ExtensionTypeParam<Ident>]>,
  {
    self.params().as_ref()
  }
}
