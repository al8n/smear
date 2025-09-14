use core::fmt::Display;

use logosky::utils::{sdl_display::DisplaySDL, syntax_tree_display::DisplaySyntaxTree, Span};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Float<'a> {
  span: Span,
  value: &'a str,
}

impl<'a> Display for Float<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<'a> AsRef<str> for Float<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.value
  }
}

impl<'a> core::ops::Deref for Float<'a> {
  type Target = str;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value
  }
}

impl<'a> Float<'a> {
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

impl<'a> DisplaySDL for Float<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<'a> DisplaySyntaxTree for Float<'a> {
  #[inline]
  fn fmt(&self, level: usize, indent: usize, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(f, "- FLOAT@{}..{} \"{}\"", self.span.start(), self.span.end(), self.as_str())
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
