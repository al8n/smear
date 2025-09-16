use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

use crate::parser::ast::{Dollar, Name};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable<S> {
  span: Span,
  slice: S,
  dollar: Dollar,
  name: Name<S>,
}

impl<S> AsRef<S> for Variable<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for Variable<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.slice()
  }
}

impl<S> Display for Variable<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<S> Variable<S> {
  /// Creates a new name.
  #[inline]
  pub(crate) const fn new(span: Span, slice: S, dollar: Dollar, name: Name<S>) -> Self {
    Self {
      span,
      slice,
      dollar,
      name,
    }
  }

  /// Returns the source slice of the variable, including the `$`.
  #[inline]
  pub const fn slice(&self) -> &S {
    &self.slice
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn name(&self) -> &Name<S> {
    &self.name
  }
}

impl<S> DisplaySDL for Variable<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}{}", self.dollar, self.name.source().display())
  }
}

impl<S> DisplaySyntaxTree for Variable<S>
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
      "- VARIABLE@{}..{}",
      self.span().start(),
      self.span().end()
    )?;
    <Dollar as DisplaySyntaxTree>::fmt(&self.dollar, level + 1, indent, f)?;
    <Name<S> as DisplaySyntaxTree>::fmt(self.name(), level + 1, indent, f)
  }
}

#[test]
fn test_variable_display_syntax_tree() {
  let variable = Variable::new(
    Span::new(0, 5),
    "$Test",
    Dollar::new(Span::new(0, 1)),
    Name::new(Span::new(1, 5), "Test"),
  );
  let output = format!("{}", DisplaySyntaxTree::display(&variable, 0, 4));
  assert_eq!(
    output,
    r#"- VARIABLE@0..5
    - DOLLAR@0..1
    - NAME@1..5
        - IDENT@1..5 "Test""#
  );
}

#[test]
fn test_variable_display_sdl() {
  let variable = Variable::new(
    Span::new(0, 5),
    "$Test",
    Dollar::new(Span::new(0, 1)),
    Name::new(Span::new(1, 5), "Test"),
  );
  let output = format!("{}", DisplaySDL::display(&variable));
  assert_eq!(output, "$Test");
}
