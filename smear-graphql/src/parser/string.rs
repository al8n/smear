use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Kind {
  Inline,
  Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringValue<S> {
  pub(crate) span: Span,
  pub(crate) raw: S,
  pub(crate) content: S,
  pub(crate) kind: Kind,
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
  pub(crate) const fn new(span: Span, raw: S, content: S, kind: Kind) -> Self {
    Self {
      span,
      raw,
      content,
      kind,
    }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the full raw string, including delimiters.
  #[inline]
  pub const fn raw(&self) -> &S {
    &self.raw
  }

  /// Returns the string content, without delimiters.
  #[inline]
  pub const fn content(&self) -> &S {
    &self.content
  }
}

impl<S> DisplaySDL for StringValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(&self.raw, f)
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
      self.raw.display(),
    )
  }
}
