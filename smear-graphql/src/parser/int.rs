use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S> AsRef<S> for IntValue<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
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
  pub const fn source(&self) -> &S {
    &self.value
  }
}

impl<S> DisplaySDL for IntValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
      self.source().display()
    )
  }
}

#[test]
fn test_int_display_syntax_tree() {
  let name = IntValue::new(Span::new(0, 2), "-4");
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 2));
  assert_eq!(
    output,
    r#"- INT@0..2 "-4"
"#
  );
}

#[test]
fn test_int_display_sdl() {
  let name = IntValue::new(Span::new(0, 2), "-4");
  let output = format!("{}", DisplaySDL::display(&name));
  assert_eq!(output, "-4");
}
