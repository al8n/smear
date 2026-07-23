use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A default input value assignment.
#[derive(Debug, Clone, Copy)]
pub struct DefaultInputValue<Value, Span = SimpleSpan> {
  span: Span,
  value: Value,
}

impl<Value, Span> DefaultInputValue<Value, Span> {
  /// Creates a default input value from its span and constant value.
  #[inline]
  pub const fn new(span: Span, value: Value) -> Self {
    Self { span, value }
  }

  /// Returns the span covering `=` and the value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the constant default value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

impl<Value, Span> AsSpan<Span> for DefaultInputValue<Value, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span> IntoSpan<Span> for DefaultInputValue<Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Span> IntoComponents for DefaultInputValue<Value, Span> {
  type Components = (Span, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}
