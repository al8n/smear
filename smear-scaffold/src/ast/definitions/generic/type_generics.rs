use core::marker::PhantomData;
use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use std::vec::Vec;

/// A type generics.
///
/// ## Example
///
/// The `User<ID, Username>` where `ID` and `Username` are type generic params, and `<ID, Username>` are the type generics.
///
/// ```graphqlx
/// type User<I, U = String, V = Int> {
///   id: I!,
///   name: U!,
///   age: V,
/// }
///
/// type Comment {
///   user: User<ID, Username>!,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenerics<Type, Container = Vec<Type>> {
  span: Span,
  params: Container,
  _type: PhantomData<Type>,
}

impl<Type, Container> AsSpan<Span> for TypeGenerics<Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type, Container> IntoSpan<Span> for TypeGenerics<Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, Container> IntoComponents for TypeGenerics<Type, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Type, Container> TypeGenerics<Type, Container> {
  /// Creates a new `TypeGenerics` with the given parameters.
  #[inline]
  pub const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
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
  pub fn params_slice(&self) -> &[Type]
  where
    Container: AsRef<[Type]>,
  {
    self.params().as_ref()
  }
}
