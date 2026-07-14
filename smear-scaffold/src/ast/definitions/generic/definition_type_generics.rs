use core::marker::PhantomData;
use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use std::vec::Vec;

/// A definition type parameter with an optional default type.
///
/// ```graphqlx
/// T = String # A type parameter `T` with a default type of `String`
/// T # A type parameter `T` without a default type
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypeParam<Ident, Type> {
  span: Span,
  ident: Ident,
  default: Option<Type>,
}

impl<Ident, Type> AsSpan<Span> for DefinitionTypeParam<Ident, Type> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type> IntoSpan<Span> for DefinitionTypeParam<Ident, Type> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type> IntoComponents for DefinitionTypeParam<Ident, Type> {
  type Components = (Span, Ident, Option<Type>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident, self.default)
  }
}

impl<Ident, Type> DefinitionTypeParam<Ident, Type> {
  /// Creates a new `DefinitionTypeParam` with the given identifier and optional default type.
  #[inline]
  pub const fn new(span: Span, ident: Ident, default: Option<Type>) -> Self {
    Self {
      span,
      ident,
      default,
    }
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

  /// Returns the optional default type of the type parameter.
  #[inline]
  pub const fn default(&self) -> Option<&Type> {
    self.default.as_ref()
  }
}

/// A definition type generics with a list of type parameters.
///
/// ```graphqlx
/// <T, U = String> # A type generics with two type parameters: `T` and `U` where `U` has a default type of `String`
/// <T, U> # A type generics with two type parameters: `T` and `U`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypeGenerics<Ident, Type, Container = Vec<DefinitionTypeParam<Ident, Type>>> {
  span: Span,
  params: Container,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Container> AsSpan<Span> for DefinitionTypeGenerics<Ident, Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Container> IntoSpan<Span> for DefinitionTypeGenerics<Ident, Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Container> IntoComponents for DefinitionTypeGenerics<Ident, Type, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Ident, Type, Container> DefinitionTypeGenerics<Ident, Type, Container> {
  /// Creates a new `DefinitionTypeGenerics` with the given parameters.
  #[inline]
  pub const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _ident: PhantomData,
      _type: PhantomData,
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
  pub fn params_slice(&self) -> &[DefinitionTypeParam<Ident, Type>]
  where
    Container: AsRef<[DefinitionTypeParam<Ident, Type>]>,
  {
    self.params().as_ref()
  }
}
