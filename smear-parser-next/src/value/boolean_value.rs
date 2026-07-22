use core::fmt::Display;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{
    IntoComponents,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

/// A boolean value literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanValue<Span = SimpleSpan> {
  span: Span,
  value: bool,
}

impl<Span> Display for BooleanValue<Span> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl<Span> AsSpan<Span> for BooleanValue<Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for BooleanValue<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for BooleanValue<Span> {
  type Components = (Span, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<Span> AsRef<bool> for BooleanValue<Span> {
  #[inline]
  fn as_ref(&self) -> &bool {
    &self.value
  }
}

impl<Span> core::ops::Deref for BooleanValue<Span> {
  type Target = bool;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<Span> BooleanValue<Span> {
  /// Creates a new boolean value.
  #[inline]
  pub(crate) const fn new(span: Span, value: bool) -> Self {
    Self { span, value }
  }

  /// Returns the span of the boolean value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the boolean value.
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }
}

impl<Span> DisplayCompact for BooleanValue<Span> {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl<Span> DisplayPretty for BooleanValue<Span> {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}
