use core::fmt::Display;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{
    IntoComponents,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

/// A null value literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NullValue<S, Span = SimpleSpan> {
  source: S,
  span: Span,
}

impl<S, Span> Display for NullValue<S, Span>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S, Span> AsSpan<Span> for NullValue<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for NullValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Span> IntoComponents for NullValue<S, Span> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.source)
  }
}

impl<S, Span> core::ops::Deref for NullValue<S, Span> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source_ref()
  }
}

impl<S, Span> NullValue<S, Span> {
  /// Creates a new null value.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self {
      source: value,
      span,
    }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the source of the null value.
  #[inline]
  pub const fn source_ref(&self) -> &S {
    &self.source
  }

  /// Returns the source of the null value.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.source
  }
}

impl<S, Span> DisplayCompact for NullValue<S, Span>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S, Span> DisplayPretty for NullValue<S, Span>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}
