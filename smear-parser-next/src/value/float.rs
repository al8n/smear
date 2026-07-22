use core::fmt::Display;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan, Span as SpanTrait},
  utils::{
    IntoComponents,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
    syntax_tree_display::DisplaySyntaxTree,
  },
};

/// A floating-point value literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatValue<S, Span = SimpleSpan> {
  span: Span,
  value: S,
}

impl<S, Span> AsSpan<Span> for FloatValue<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for FloatValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Span> IntoComponents for FloatValue<S, Span> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S, Span> Display for FloatValue<S, Span>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S, Span> AsRef<S> for FloatValue<S, Span> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S, Span> core::ops::Deref for FloatValue<S, Span> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source_ref()
  }
}

impl<S, Span> FloatValue<S, Span> {
  /// Creates a new float value.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the source of the float.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns the source of the float.
  #[inline]
  pub const fn source_ref(&self) -> &S {
    &self.value
  }
}

impl<S, Span> DisplayCompact for FloatValue<S, Span>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S, Span> DisplayPretty for FloatValue<S, Span>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S, Span> DisplaySyntaxTree for FloatValue<S, Span>
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
      self.source_ref().display(),
    )
  }
}
