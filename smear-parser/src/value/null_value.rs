use core::{fmt::Display, marker::PhantomData};

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{DisplayCompact, DisplayHuman, DisplayPretty, IntoComponents},
};

/// A null value literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NullValue<S, Span = SimpleSpan, Lang: ?Sized = ()> {
  source: S,
  span: Span,
  _lang: PhantomData<Lang>,
}

impl<S, Span, Lang: ?Sized> Display for NullValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for NullValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for NullValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// [`NullValue`] taken apart: every field, in the order it is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NullValueComponents<S, Span> {
  /// The span covering the null literal.
  pub span: Span,
  /// The null literal's source spelling.
  pub source: S,
}

impl<S, Span, Lang: ?Sized> IntoComponents for NullValue<S, Span, Lang> {
  type Components = NullValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    NullValueComponents {
      span: self.span,
      source: self.source,
    }
  }
}

impl<S, Span, Lang: ?Sized> core::ops::Deref for NullValue<S, Span, Lang> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S, Span, Lang: ?Sized> NullValue<S, Span, Lang> {
  /// Creates a new null value.
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

  /// Returns the span covering the null literal.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the null literal's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    &self.source
  }
}

impl<S, Span, Lang: ?Sized> DisplayCompact for NullValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source().fmt(f)
  }
}

impl<S, Span, Lang: ?Sized> DisplayPretty for NullValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}
