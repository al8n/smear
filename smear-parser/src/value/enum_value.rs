use core::{fmt::Display, marker::PhantomData};

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{
    IntoComponents,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

/// An enum value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumValue<S, Span = SimpleSpan, Lang: ?Sized = ()> {
  source: S,
  span: Span,
  _lang: PhantomData<Lang>,
}

impl<S, Span, Lang: ?Sized> Display for EnumValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for EnumValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for EnumValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Span, Lang: ?Sized> IntoComponents for EnumValue<S, Span, Lang> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.source)
  }
}

impl<S, Span, Lang: ?Sized> core::ops::Deref for EnumValue<S, Span, Lang> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S, Span, Lang: ?Sized> EnumValue<S, Span, Lang> {
  /// Creates a new enum value.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self
  where
    S: crate::value::Leaf,
    Span: crate::value::Leaf,
  {
    Self {
      source: value,
      span,
      _lang: PhantomData,
    }
  }

  /// Returns the span covering the enum literal.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the enum literal's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    &self.source
  }
}

impl<S, Span, Lang: ?Sized> DisplayCompact for EnumValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source().fmt(f)
  }
}

impl<S, Span, Lang: ?Sized> DisplayPretty for EnumValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source().fmt(f)
  }
}
