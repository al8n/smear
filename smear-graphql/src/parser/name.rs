use core::fmt::Display;

use logosky::utils::{Span, sdl_display::DisplaySDL, syntax_tree_display::DisplaySyntaxTree};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name<'a> {
  span: Span,
  value: &'a str,
}

impl<'a> Display for Name<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<'a> AsRef<str> for Name<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.value
  }
}

impl<'a> core::ops::Deref for Name<'a> {
  type Target = str;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value
  }
}

impl<'a> Name<'a> {
  /// Creates a new name.
  #[inline]
  pub const fn new(span: Span, value: &'a str) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn as_str(&self) -> &'a str {
    self.value
  }
}

impl<'a> DisplaySDL for Name<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<'a> DisplaySyntaxTree for Name<'a> {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let mut padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(f, "- NAME@{}..{}", self.span.start(), self.span.end())?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.value
    )
  }
}

#[test]
fn test_name_display_syntax_tree() {
  let name = Name::new(Span::new(0, 4), "Test");
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 4));
  assert_eq!(
    output,
    r#"- NAME@0..4
    - IDENT@0..4 "Test""#
  );
}

#[test]
fn test_name_display_sdl() {
  let name = Name::new(Span::new(0, 4), "Test");
  let output = format!("{}", DisplaySDL::display(&name));
  assert_eq!(output, "Test");
}
