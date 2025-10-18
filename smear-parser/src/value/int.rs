use logosky::utils::{
  AsSpan, IntoComponents, IntoSpan, Span,
  human_display::DisplayHuman,
  sdl_display::{DisplayCompact, DisplayPretty},
  syntax_tree_display::DisplaySyntaxTree,
};

use core::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntValue<S> {
  span: Span,
  value: S,
}

impl<S> Display for IntValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> AsSpan<Span> for IntValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for IntValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for IntValue<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S> core::ops::Deref for IntValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<S> IntValue<S> {
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the source of the int.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns the source of the int.
  #[inline]
  pub const fn source_ref(&self) -> &S {
    &self.value
  }
}

impl<S> DisplayCompact for IntValue<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> DisplayPretty for IntValue<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> DisplaySyntaxTree for IntValue<S>
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
      "- INT@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.source_ref().display()
    )
  }
}
