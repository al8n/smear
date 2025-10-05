use logosky::utils::{
  AsSpan, IntoComponents, IntoSpan, Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

use core::fmt::Display;

use crate::lexer::LitStr;

/// A string value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValue<S> {
  span: Span,
  lit: LitStr<S>,
}

impl<S> AsSpan<Span> for StringValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for StringValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for StringValue<S> {
  type Components = (Span, LitStr<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.lit)
  }
}

impl<S> Display for StringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<S> StringValue<S> {
  #[inline(always)]
  pub(crate) const fn new(span: Span, lit: LitStr<S>) -> Self {
    Self { span, lit }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.lit.source()
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    self.lit.source_ref()
  }
}

impl<S> DisplaySDL for StringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> DisplaySyntaxTree for StringValue<S>
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
      "- STRING@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.source_ref().display(),
    )
  }
}
