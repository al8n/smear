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
pub struct BooleanValue<S: ?Sized, Span = SimpleSpan, Lang: ?Sized = ()> {
  span: Span,
  value: bool,
  _slice: PhantomData<S>,
  _lang: PhantomData<Lang>,
}

impl<S: ?Sized, Span, Lang: ?Sized> Display for BooleanValue<S, Span, Lang> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> AsSpan<Span> for BooleanValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> IntoSpan<Span> for BooleanValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> IntoComponents for BooleanValue<S, Span, Lang> {
  type Components = (Span, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> AsRef<bool> for BooleanValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &bool {
    &self.value
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> core::ops::Deref for BooleanValue<S, Span, Lang> {
  type Target = bool;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> BooleanValue<S, Span, Lang> {
  /// Creates a new boolean value.
  #[inline]
  pub(crate) const fn new(span: Span, value: bool) -> Self {
    Self {
      span,
      value,
      _slice: PhantomData,
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

impl<S: ?Sized, Span, Lang: ?Sized> DisplayCompact for BooleanValue<S, Span, Lang> {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}

impl<S: ?Sized, Span, Lang: ?Sized> DisplayPretty for BooleanValue<S, Span, Lang> {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    write!(f, "{}", self.value())
  }
}
