use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A node with an optional description.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Described<T, Description> {
  span: Span,
  description: Option<Description>,
  node: T,
}

impl<T, Description> core::ops::Deref for Described<T, Description> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.node
  }
}

impl<T, Description> core::ops::DerefMut for Described<T, Description> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.node
  }
}

impl<T, Description> AsSpan<Span> for Described<T, Description> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<T, Description> IntoSpan<Span> for Described<T, Description> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<T, Description> IntoComponents for Described<T, Description> {
  type Components = (Span, Option<Description>, T);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.description, self.node)
  }
}

impl<T, Description> Described<T, Description> {
  /// Creates a new described node with the given span, optional description, and node.
  #[inline]
  pub const fn new(span: Span, description: Option<Description>, node: T) -> Self {
    Self {
      span,
      description,
      node,
    }
  }

  /// Returns the span of the described node.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the description of the described node, if any.
  #[inline]
  pub const fn description(&self) -> Option<&Description> {
    self.description.as_ref()
  }

  /// Returns the inner node.
  #[inline]
  pub const fn node(&self) -> &T {
    &self.node
  }
}
