use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NullValue<S> {
  source: S,
  span: Span,
}

impl<S> Display for NullValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source(), f)
  }
}

impl<S> AsRef<S> for NullValue<S> {
  #[inline]
  fn as_ref(&self) -> &S {
    self
  }
}

impl<S> core::ops::Deref for NullValue<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source()
  }
}

impl<S> NullValue<S> {
  /// Creates a new null value.
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self {
      source: value,
      span,
    }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn source(&self) -> &S {
    &self.source
  }
}

impl<S> DisplaySDL for NullValue<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source().fmt(f)
  }
}

impl<S> DisplaySyntaxTree for NullValue<S>
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
      "- NULL_VALUE@{}..{}",
      self.span().start(),
      self.span().end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- null_KW@{}..{} \"{}\"",
      self.span().start(),
      self.span().end(),
      self.source().display(),
    )
  }
}

#[test]
fn test_null_value_display_syntax_tree() {
  let name = NullValue::new(Span::new(0, 4), "null");
  let output = format!("{}", DisplaySyntaxTree::display(&name, 0, 4));
  assert_eq!(
    output,
    r#"- NULL_VALUE@0..4
    - null_KW@0..4 "null""#
  );
}

#[test]
fn test_null_value_display_sdl() {
  let name = NullValue::new(Span::new(0, 4), "null");
  let output = format!("{}", DisplaySDL::display(&name));
  assert_eq!(output, "null");
}
