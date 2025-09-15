use core::fmt::Display;

use logosky::utils::{Span, sdl_display::DisplaySDL, syntax_tree_display::DisplaySyntaxTree};

use crate::parser::ast::Name;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumValue<'a>(Name<'a>);

impl<'a> Display for EnumValue<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<'a> AsRef<str> for EnumValue<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    self
  }
}

impl<'a> core::ops::Deref for EnumValue<'a> {
  type Target = str;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<'a> EnumValue<'a> {
  /// Creates a new name.
  #[inline]
  pub const fn new(span: Span, value: &'a str) -> Self {
    Self(Name::new(span, value))
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.0.span()
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn as_str(&self) -> &'a str {
    self.0.as_str()
  }
}

impl<'a> DisplaySDL for EnumValue<'a> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    <Name<'a> as DisplaySDL>::fmt(&self.0, f)
  }
}

impl<'a> DisplaySyntaxTree for EnumValue<'a> {
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
      "- ENUM_VALUE@{}..{}",
      self.span().start(),
      self.span().end()
    )?;
    <Name<'a> as DisplaySyntaxTree>::fmt(&self.0, level + 1, indent, f)
  }
}

#[test]
fn test_name_display_syntax_tree() {
  let name = EnumValue::new(Span::new(0, 4), "Test");
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 4));
  assert_eq!(
    output,
    r#"- ENUM_VALUE@0..4
    - NAME@0..4
        - IDENT@0..4 "Test""#
  );
}

#[test]
fn test_name_display_sdl() {
  let name = EnumValue::new(Span::new(0, 4), "Test");
  let output = format!("{}", DisplaySDL::display(&name));
  assert_eq!(output, "Test");
}
