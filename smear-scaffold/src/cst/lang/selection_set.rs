use logosky::utils::Span;
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a GraphQL selection set: `{ selection... }`
///
/// Unlike the AST version, preserves:
/// - Opening left brace `{` with its padding
/// - All selections with their separating trivia
/// - Closing right brace `}` with its padding
///
/// ## Examples
/// ```text
/// { id name }
/// {
///   id
///   name  # user name
/// }
/// { id  name  email }  # preserves spacing
/// ```
#[derive(Debug, Clone)]
pub struct SelectionSet<Selection, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Selection>> {
  span: Span,
  /// Padding around the left brace
  lbrace_padding: Padding<S, TriviaContainer>,
  /// Selections with their trivia
  selections: Container,
  /// Padding around the right brace
  rbrace_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<Selection>,
}

impl<Selection, S, TriviaContainer, Container> SelectionSet<Selection, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST SelectionSet.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      lbrace_padding: Padding::new(),
      selections: Container::default(),
      rbrace_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST SelectionSet with all components.
  pub const fn with_parts(
    span: Span,
    lbrace_padding: Padding<S, TriviaContainer>,
    selections: Container,
    rbrace_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      lbrace_padding,
      selections,
      rbrace_padding,
      _marker: PhantomData,
    }
  }
}

impl<Selection, S, TriviaContainer, Container> SelectionSet<Selection, S, TriviaContainer, Container> {
  /// Returns the span covering the entire selection set.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left brace padding.
  #[inline]
  pub const fn lbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lbrace_padding
  }

  /// Returns a reference to the selections container.
  #[inline]
  pub const fn selections(&self) -> &Container {
    &self.selections
  }

  /// Returns a reference to the right brace padding.
  #[inline]
  pub const fn rbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rbrace_padding
  }

  /// Returns a mutable reference to the left brace padding.
  #[inline]
  pub fn lbrace_padding_mut(&mut self) -> &mut Padding<S, TriviaContainer> {
    &mut self.lbrace_padding
  }

  /// Returns a mutable reference to the selections container.
  #[inline]
  pub fn selections_mut(&mut self) -> &mut Container {
    &mut self.selections
  }

  /// Returns a mutable reference to the right brace padding.
  #[inline]
  pub fn rbrace_padding_mut(&mut self) -> &mut Padding<S, TriviaContainer> {
    &mut self.rbrace_padding
  }

  /// Returns a slice of selections if the container supports it.
  #[inline]
  pub fn selections_slice(&self) -> &[Selection]
  where
    Container: AsRef<[Selection]>,
  {
    self.selections().as_ref()
  }
}
