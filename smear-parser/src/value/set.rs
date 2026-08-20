use core::marker::PhantomData;
use std::vec::Vec;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A GraphQLx set value with its enclosing source span.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Set<Value, Span = SimpleSpan, Container = Vec<Value>> {
  span: Span,
  values: Container,
  _value: PhantomData<Value>,
}

impl<Value, Span, Container> Set<Value, Span, Container> {
  /// Creates a set value from its span and values.
  #[inline]
  pub const fn new(span: Span, values: Container) -> Self {
    Self {
      span,
      values,
      _value: PhantomData,
    }
  }

  /// Returns the span covering the complete set.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parsed set values as a slice.
  #[inline]
  pub fn values(&self) -> &[Value]
  where
    Container: AsRef<[Value]>,
  {
    self.values.as_ref()
  }

  /// Consumes this set and returns its values.
  #[inline]
  pub fn into_values(self) -> Container {
    self.values
  }
}

impl<Value, Span, Container> AsSpan<Span> for Set<Value, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span, Container> IntoSpan<Span> for Set<Value, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Span, Container> IntoComponents for Set<Value, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.values)
  }
}
