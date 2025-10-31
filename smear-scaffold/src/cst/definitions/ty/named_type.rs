use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};

use crate::cst::Padding;

/// Concrete Syntax Tree representation of a named GraphQL type with optional non-null modifier.
///
/// Unlike the AST version which only stores the name and required flag, this CST version
/// preserves:
/// - The exact name token with its trivia
/// - The optional bang token (`!`) with its trivia
/// - All whitespace and comments between tokens
///
/// This allows for lossless round-tripping and source-level manipulation.
///
/// ## Grammar
/// ```text
/// NamedType : Name !?
/// ```
///
/// ## Examples
/// ```text
/// String          # name: "String", bang: None
/// String!         # name: "String", bang: Some(!)
/// User  !         # name: "User  ", bang: Some(!)  (preserves spacing)
/// ```
#[derive(Debug, Clone)]
pub struct NamedType<Name, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// The type name with its padding
  name: Name,
  /// The optional bang token with its padding
  bang: Option<Padding<S, TriviaContainer>>,
}

impl<Name, S, TriviaContainer> AsSpan<Span> for NamedType<Name, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, S, TriviaContainer> IntoSpan<Span> for NamedType<Name, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, S, TriviaContainer> IntoComponents for NamedType<Name, S, TriviaContainer> {
  type Components = (Span, Name, Option<Padding<S, TriviaContainer>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.bang)
  }
}

impl<Name, S, TriviaContainer> NamedType<Name, S, TriviaContainer> {
  /// Creates a new CST NamedType.
  pub const fn new(span: Span, name: Name, bang: Option<Padding<S, TriviaContainer>>) -> Self {
    Self { span, name, bang }
  }

  /// Returns the span covering the entire named type including all trivia.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the bang token padding, if present.
  #[inline]
  pub const fn bang(&self) -> Option<&Padding<S, TriviaContainer>> {
    self.bang.as_ref()
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub const fn required(&self) -> bool {
    self.bang.is_some()
  }

  /// Converts this CST node into mutable references to its components.
  pub fn as_mut_parts(&mut self) -> (&mut Name, &mut Option<Padding<S, TriviaContainer>>) {
    (&mut self.name, &mut self.bang)
  }
}

// Note: Parser implementation would go here, but requires integration with
// a trivia-collecting tokenizer. For now, CST nodes are meant to be constructed
// programmatically or via a custom parser that handles lossless token streams.
