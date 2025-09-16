use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanValue<S> {
  source: S,
  span: Span,
  value: bool,
}

impl<S> Display for BooleanValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S> AsRef<S> for BooleanValue<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for BooleanValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S> BooleanValue<S> {
  /// Creates a new boolean value.
  #[inline]
  pub(crate) const fn new(span: Span, source: S, value: bool) -> Self {
    Self {
      source,
      span,
      value,
    }
  }

  /// Returns the span of the boolean value.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn source(&self) -> &S {
    &self.source
  }

  /// Returns the boolean value.
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }
}

impl<S> DisplaySDL for BooleanValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source().fmt(f)
  }
}

impl<S> DisplaySyntaxTree for BooleanValue<S>
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
    let mut padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- BOOLEAN_VALUE@{}..{}",
      self.span().start(),
      self.span().end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    let kw = if self.value { "true_KW" } else { "false_KW" };
    write!(
      f,
      "- {}@{}..{} \"{}\"",
      kw,
      self.span().start(),
      self.span().end(),
      self.source().display(),
    )
  }
}

#[test]
fn test_boolean_display_syntax_tree() {
  let name = BooleanValue::new(Span::new(0, 4), "true", true);
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 4));
  assert_eq!(
    output,
    r#"- BOOLEAN_VALUE@0..4
    - true_KW@0..4 "true""#
  );

  let name = BooleanValue::new(Span::new(0, 5), "false", true);
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 4));
  assert_eq!(
    output,
    r#"- BOOLEAN_VALUE@0..5
    - false_KW@0..5 "false""#
  );
}

#[test]
fn test_boolean_display_sdl() {
  let name = BooleanValue::new(Span::new(0, 5), "false", false);
  let output = format!("{}", DisplaySDL::display(&name));
  assert_eq!(output, "false");
}
