use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};

/// CST representation of a pair structure for two sequentially parsed elements.
///
/// Unlike the AST version, this can preserve trivia between the two elements
/// if needed by the parser.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct And<A, B> {
  span: Span,
  first: A,
  second: B,
}

impl<A, B> AsSpan<Span> for And<A, B> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<A, B> IntoSpan<Span> for And<A, B> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<A, B> IntoComponents for And<A, B> {
  type Components = (Span, A, B);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.first, self.second)
  }
}

impl<A, B> And<A, B> {
  /// Creates a new `And` with the given elements.
  #[inline]
  pub const fn new(span: Span, first: A, second: B) -> Self {
    Self {
      span,
      first,
      second,
    }
  }

  /// Returns the span of the pair.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the first element of the pair.
  #[inline]
  pub const fn first(&self) -> &A {
    &self.first
  }

  /// Returns the second element of the pair.
  #[inline]
  pub const fn second(&self) -> &B {
    &self.second
  }
}
