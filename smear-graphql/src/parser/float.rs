use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Float<S> {
  span: Span,
  value: S,
}

impl<S> Display for Float<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S> AsRef<S> for Float<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for Float<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S> Float<S> {
  /// Creates a new name.
  #[inline]
  pub const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn source(&self) -> &S {
    &self.value
  }
}

impl<S> DisplaySDL for Float<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> DisplaySyntaxTree for Float<S>
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
      self.source().display(),
    )
  }
}

#[test]
fn test_float_display_syntax_tree() {
  let name = Float::new(Span::new(0, 6), "-4.123");
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 6));
  assert_eq!(
    output,
    r#"- FLOAT@0..6 "-4.123"
"#
  );
}

#[test]
fn test_float_display_sdl() {
  let name = Float::new(Span::new(0, 6), "-4.123");
  let output = format!("{}", DisplaySDL::display(&name));
  assert_eq!(output, "-4.123");
}
