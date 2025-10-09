use core::fmt::Display;

use logosky::utils::{
  AsSpan, IntoComponents, IntoSpan, Span, cmp::Equivalent, human_display::DisplayHuman,
  sdl_display::DisplaySDL,
};

/// An identifier.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Ident<S> {
  span: Span,
  value: S,
}

impl<S> Ident<S> {
  /// Creates a new identifier with the given span and value.
  ///
  /// The span represents the location of the identifier in the source text,
  /// and the value is the actual source text.
  #[inline]
  pub const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the underlying source value.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns reference of the underlying source value.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    &self.value
  }
}

impl<S> AsSpan<Span> for Ident<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for Ident<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for Ident<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S> Display for Ident<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> core::ops::Deref for Ident<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source_ref()
  }
}

impl<S> DisplaySDL for Ident<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> Equivalent<Ident<S>> for str
where
  str: Equivalent<S>,
{
  #[inline]
  fn equivalent(&self, other: &Ident<S>) -> bool {
    self.equivalent(other.source_ref())
  }
}
