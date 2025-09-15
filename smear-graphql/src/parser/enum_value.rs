use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

use crate::parser::ast::Name;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumValue<S>(Name<S>);

impl<S> Display for EnumValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S> AsRef<S> for EnumValue<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for EnumValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S> EnumValue<S> {
  /// Creates a new name.
  #[inline]
  pub const fn new(span: Span, value: S) -> Self {
    Self(Name::new(span, value))
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.0.span()
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn source(&self) -> &S {
    self.0.source()
  }
}

impl<S> DisplaySDL for EnumValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source().fmt(f)
  }
}

impl<S> DisplaySyntaxTree for EnumValue<S>
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
      "- ENUM_VALUE@{}..{}",
      self.span().start(),
      self.span().end()
    )?;
    <Name<S> as DisplaySyntaxTree>::fmt(&self.0, level + 1, indent, f)
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
