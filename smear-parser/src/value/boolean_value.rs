use core::fmt::Display;
use logosky::utils::{
  AsSpan, IntoComponents, IntoSpan, Span,
  sdl_display::{DisplayCompact, DisplayPretty},
};

/// A boolean value literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanValue {

  span: Span,
  value: bool,
}

impl Display for BooleanValue {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl AsSpan<Span> for BooleanValue {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for BooleanValue {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl IntoComponents for BooleanValue {
  type Components = (Span, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl AsRef<bool> for BooleanValue {
  #[inline]
  fn as_ref(&self) -> &bool {
    &self.value
  }
}

impl core::ops::Deref for BooleanValue {
  type Target = bool;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl BooleanValue {
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

impl DisplayCompact for BooleanValue {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl DisplayPretty for BooleanValue {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}
