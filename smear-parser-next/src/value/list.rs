use core::marker::PhantomData;
use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A list value with its enclosing source span.
#[derive(Debug, Clone)]
pub struct List<Value, Span = SimpleSpan, Container = Vec<Value>> {
  span: Span,
  values: Container,
  _value: PhantomData<Value>,
}

impl<Value, Span, Container> List<Value, Span, Container> {
  /// Creates a list value from its span and elements.
  #[inline]
  pub const fn new(span: Span, values: Container) -> Self {
    Self {
      span,
      values,
      _value: PhantomData,
    }
  }

  /// Returns the span covering the complete list.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parsed list elements.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }
}

impl<Value, Span, Container> AsSpan<Span> for List<Value, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span, Container> IntoSpan<Span> for List<Value, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Span, Container> IntoComponents for List<Value, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.values)
  }
}
