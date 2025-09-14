use core::fmt::Display;

use logosky::utils::{Span, sdl_display::DisplaySDL, syntax_tree_display::DisplaySyntaxTree};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int<'a> {
  span: Span,
  value: &'a str,
}

impl<'a> Display for Int<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<'a> AsRef<str> for Int<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.value
  }
}

impl<'a> core::ops::Deref for Int<'a> {
  type Target = str;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value
  }
}

impl<'a> Int<'a> {
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

impl<'a> DisplaySDL for Int<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<'a> DisplaySyntaxTree for Int<'a> {
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
      self.as_str()
    )
  }
}

#[test]
fn test_int_display_syntax_tree() {
  let name = Int::new(Span::new(0, 2), "-4");
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 2));
  assert_eq!(
    output,
    r#"- INT@0..2 "-4"
"#
  );
}

#[test]
fn test_int_display_sdl() {
  let name = Int::new(Span::new(0, 2), "-4");
  let output = format!("{}", DisplaySDL::display(&name));
  assert_eq!(output, "-4");
}
