use core::{fmt::Display, marker::PhantomData};
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
pub struct BooleanValue<Span = SimpleSpan, Lang: ?Sized = ()> {
  span: Span,
  value: bool,
  _lang: PhantomData<Lang>,
}

impl<Span, Lang: ?Sized> Display for BooleanValue<Span, Lang> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl<Span, Lang: ?Sized> AsSpan<Span> for BooleanValue<Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Span, Lang: ?Sized> IntoSpan<Span> for BooleanValue<Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span, Lang: ?Sized> IntoComponents for BooleanValue<Span, Lang> {
  type Components = (Span, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<Span, Lang: ?Sized> AsRef<bool> for BooleanValue<Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &bool {
    &self.value
  }
}

impl<Span, Lang: ?Sized> core::ops::Deref for BooleanValue<Span, Lang> {
  type Target = bool;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<Span, Lang: ?Sized> BooleanValue<Span, Lang> {
  /// Creates a new boolean value.
  #[inline]
  pub(crate) const fn new(span: Span, value: bool) -> Self {
    Self {
      span,
      value,
      _lang: PhantomData,
    }
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

impl<Span, Lang: ?Sized> DisplayCompact for BooleanValue<Span, Lang> {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl<Span, Lang: ?Sized> DisplayPretty for BooleanValue<Span, Lang> {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}
