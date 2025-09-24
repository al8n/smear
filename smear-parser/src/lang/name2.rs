use core::fmt::Display;

use logosky::utils::{
  Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
  syntax_tree_display::DisplaySyntaxTree,
};
use smear_utils::{IntoComponents, IntoSpan};

/// A GraphQL name identifier.
///
/// Represents a valid GraphQL name as defined by the specification. Names are
/// used throughout GraphQL for field names, type names, argument names, directive
/// names, and other identifiers. They follow strict lexical rules to ensure
/// consistent parsing across different GraphQL implementations.
///
/// ## Specification Rules
///
/// A GraphQL name must:
/// - Start with a letter (`A-Z`, `a-z`) or underscore (`_`)
/// - Contain only letters, digits (`0-9`), and underscores in subsequent positions
/// - Be at least one character long
/// - Be case-sensitive (`myField` and `MyField` are different names)
///
/// ## Grammar
///
/// ```text
/// Name ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
///
/// ## Examples
///
/// **Valid names:**
/// ```text
/// user           // Simple lowercase name
/// User           // Capitalized name
/// _private       // Starting with underscore
/// field123       // Contains digits
/// __typename     // GraphQL introspection field
/// MyCustomType   // PascalCase type name
/// _id            // Underscore prefix
/// a              // Single character
/// ```
///
/// **Invalid names:**
/// ```text
/// 123field       // Cannot start with digit
/// my-field       // Hyphens not allowed
/// my.field       // Dots not allowed
/// my field       // Spaces not allowed
/// my@field       // Special characters not allowed
/// ""             // Empty string not allowed
/// ```
///
/// ## Implementation Notes
///
/// This parser only handles the lexical structure of names and does not validate
/// GraphQL-specific naming conventions (e.g., type names should be PascalCase).
/// Such semantic validation should be performed at a higher level.
///
/// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Name<S> {
  span: Span,
  value: S,
}

impl<S> Name<S> {
  /// Creates a new name.
  #[inline]
  pub const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the underlying slice value.
  #[inline]
  pub const fn slice(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns reference of the underlying slice value.
  #[inline(always)]
  pub const fn slice_ref(&self) -> &S {
    &self.value
  }
}

impl<S> AsRef<Span> for Name<S> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for Name<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for Name<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S> Display for Name<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.slice_ref(), f)
  }
}

impl<S> core::ops::Deref for Name<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.slice_ref()
  }
}

impl<S> DisplaySDL for Name<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> DisplaySyntaxTree for Name<S>
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
    writeln!(f, "- NAME@{}..{}", self.span.start(), self.span.end())?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.value.display(),
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
