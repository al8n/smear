use logosky::utils::Span;
use core::marker::PhantomData;
use std::vec::Vec;

/// Concrete Syntax Tree representation of a GraphQL document.
///
/// Unlike the AST version which only stores definitions, the CST version preserves:
/// - Leading trivia before the first definition
/// - Trivia between each definition
/// - Trailing trivia after the last definition
///
/// This allows for complete source reconstruction including all formatting,
/// comments, and whitespace.
///
/// ## Examples
/// ```text
/// # GraphQL schema
/// type User {
///   id: ID!
/// }
///
/// # Another type
/// type Post {
///   title: String
/// }
/// ```
///
/// The CST would preserve the comment "# GraphQL schema", the blank line,
/// the comment "# Another type", etc.
#[derive(Debug, Clone)]
pub struct Document<Definition, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Definition>> {
  span: Span,
  /// Leading trivia before the first definition
  leading: TriviaContainer,
  /// Definitions with their associated padding (trivia after each definition)
  definitions: Container,
  /// Trailing trivia after the last definition
  trailing: TriviaContainer,
  _marker: PhantomData<(Definition, S)>,
}

impl<Definition, S, TriviaContainer, Container> Document<Definition, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST Document.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      leading: TriviaContainer::default(),
      definitions: Container::default(),
      trailing: TriviaContainer::default(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST Document with the given components.
  pub const fn with_parts(
    span: Span,
    leading: TriviaContainer,
    definitions: Container,
    trailing: TriviaContainer,
  ) -> Self {
    Self {
      span,
      leading,
      definitions,
      trailing,
      _marker: PhantomData,
    }
  }
}

impl<Definition, S, TriviaContainer, Container> Document<Definition, S, TriviaContainer, Container> {
  /// Returns the span covering the entire document.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the leading trivia.
  #[inline]
  pub const fn leading(&self) -> &TriviaContainer {
    &self.leading
  }

  /// Returns a reference to the definitions container.
  #[inline]
  pub const fn definitions(&self) -> &Container {
    &self.definitions
  }

  /// Returns a reference to the trailing trivia.
  #[inline]
  pub const fn trailing(&self) -> &TriviaContainer {
    &self.trailing
  }

  /// Returns a mutable reference to the leading trivia.
  #[inline]
  pub fn leading_mut(&mut self) -> &mut TriviaContainer {
    &mut self.leading
  }

  /// Returns a mutable reference to the definitions container.
  #[inline]
  pub fn definitions_mut(&mut self) -> &mut Container {
    &mut self.definitions
  }

  /// Returns a mutable reference to the trailing trivia.
  #[inline]
  pub fn trailing_mut(&mut self) -> &mut TriviaContainer {
    &mut self.trailing
  }

  /// Returns a slice of definitions if the container supports it.
  #[inline]
  pub fn definitions_slice(&self) -> &[Definition]
  where
    Container: AsRef<[Definition]>,
  {
    self.definitions().as_ref()
  }

  /// Deconstructs the Document into its components.
  pub fn into_parts(self) -> (Span, TriviaContainer, Container, TriviaContainer) {
    (self.span, self.leading, self.definitions, self.trailing)
  }
}
