use core::marker::PhantomData;
use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a GraphQL list value: `[item1, item2, ...]`
///
/// Unlike the AST version, preserves:
/// - Left bracket `[` with its padding
/// - All items with their separating trivia (commas, spaces)
/// - Right bracket `]` with its padding
///
/// ## Examples
/// ```text
/// [1, 2, 3]
/// [ "a" , "b" ]  # preserves spacing
/// [
///   "item1"  # comment
///   "item2"
/// ]
/// ```
#[derive(Debug, Clone)]
pub struct List<Item, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Item>> {
  span: Span,
  /// Padding around the left bracket
  lbracket_padding: Padding<S, TriviaContainer>,
  /// Items with their trivia
  items: Container,
  /// Padding around the right bracket
  rbracket_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<Item>,
}

impl<Item, S, TriviaContainer, Container> AsSpan<Span>
  for List<Item, S, TriviaContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Item, S, TriviaContainer, Container> IntoSpan<Span>
  for List<Item, S, TriviaContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Item, S, TriviaContainer, Container> IntoComponents
  for List<Item, S, TriviaContainer, Container>
{
  type Components = (
    Span,
    Padding<S, TriviaContainer>,
    Container,
    Padding<S, TriviaContainer>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.lbracket_padding,
      self.items,
      self.rbracket_padding,
    )
  }
}

impl<Item, S, TriviaContainer, Container> List<Item, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST List.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      lbracket_padding: Padding::new(),
      items: Container::default(),
      rbracket_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST List with all components.
  pub const fn with_parts(
    span: Span,
    lbracket_padding: Padding<S, TriviaContainer>,
    items: Container,
    rbracket_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      lbracket_padding,
      items,
      rbracket_padding,
      _marker: PhantomData,
    }
  }
}

impl<Item, S, TriviaContainer, Container> List<Item, S, TriviaContainer, Container> {
  /// Returns the span covering the entire list.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left bracket padding.
  #[inline]
  pub const fn lbracket_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lbracket_padding
  }

  /// Returns a reference to the items container.
  #[inline]
  pub const fn items(&self) -> &Container {
    &self.items
  }

  /// Returns a reference to the right bracket padding.
  #[inline]
  pub const fn rbracket_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rbracket_padding
  }

  /// Returns a slice of items if the container supports it.
  #[inline]
  pub fn items_slice(&self) -> &[Item]
  where
    Container: AsRef<[Item]>,
  {
    self.items().as_ref()
  }
}
