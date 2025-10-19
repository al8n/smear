use logosky::utils::{
  AsSpan, IntoComponents, IntoSpan, Span,
  human_display::DisplayHuman,
  sdl_display::{DisplayCompact, DisplayPretty},
};

use core::fmt::Display;

/// An enum value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumValue<S> {
  source: S,
  span: Span,
}

impl<S> Display for EnumValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> AsSpan<Span> for EnumValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for EnumValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for EnumValue<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.source)
  }
}

impl<S> core::ops::Deref for EnumValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source_ref()
  }
}

impl<S> EnumValue<S> {
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

  /// Returns the source of the enum value.
  #[inline]
  pub const fn source_ref(&self) -> &S {
    &self.source
  }

  /// Returns the source of the enum value.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.source
  }
}

impl<S> DisplayCompact for EnumValue<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> DisplayPretty for EnumValue<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}
