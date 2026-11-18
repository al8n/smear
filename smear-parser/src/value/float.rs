use core::{fmt::Display, marker::PhantomData};

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan, Span as SpanTrait},
  utils::{DisplayCompact, DisplayHuman, DisplayPretty, DisplaySyntaxTree, IntoComponents},
};

/// A floating-point value literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatValue<S, Span = SimpleSpan, Lang: ?Sized = ()> {
  span: Span,
  value: S,
  _lang: PhantomData<Lang>,
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for FloatValue<S, Span, Lang> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Lang: ?Sized> IntoSpan<Span> for FloatValue<S, Span, Lang> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `FloatValue` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatValueComponents<S, Span> {
  /// The span covering the floating-point literal.
  pub span: Span,
  /// The literal's source slice.
  pub value: S,
}

impl<S, Span, Lang: ?Sized> IntoComponents for FloatValue<S, Span, Lang> {
  type Components = FloatValueComponents<S, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    FloatValueComponents {
      span: self.span,
      value: self.value,
    }
  }
}

impl<S, Span, Lang: ?Sized> Display for FloatValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S, Span, Lang: ?Sized> AsRef<S> for FloatValue<S, Span, Lang> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S, Span, Lang: ?Sized> core::ops::Deref for FloatValue<S, Span, Lang> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S, Span, Lang: ?Sized> FloatValue<S, Span, Lang> {
  /// Creates a new float value.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self
  where
    S: crate::value::Leaf,
    Span: crate::value::Leaf,
  {
    Self {
      span,
      value,
      _lang: PhantomData,
    }
  }

  /// Returns the span covering the floating-point literal.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the floating-point literal's source spelling.
  #[inline]
  pub const fn source(&self) -> &S {
    &self.value
  }
}

impl<S, Span, Lang: ?Sized> DisplayCompact for FloatValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S, Span, Lang: ?Sized> DisplayPretty for FloatValue<S, Span, Lang>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S, Span, Lang: ?Sized> DisplaySyntaxTree for FloatValue<S, Span, Lang>
where
  S: DisplayHuman,
  Span: SpanTrait,
  <Span as SpanTrait>::Offset: Display,
{
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- FLOAT@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.source().display(),
    )
  }
}
