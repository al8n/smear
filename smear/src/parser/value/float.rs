use logosky::utils::{
  AsSpan, IntoComponents, IntoSpan, Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

use core::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatValue<S> {
  span: Span,
  value: S,
}

impl<S> AsSpan<Span> for FloatValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for FloatValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for FloatValue<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S> Display for FloatValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> AsRef<S> for FloatValue<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for FloatValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source_ref()
  }
}

impl<S> FloatValue<S> {
  /// Creates a new name.
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

impl<S> DisplaySDL for FloatValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> DisplaySyntaxTree for FloatValue<S>
where
  S: DisplayHuman,
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
