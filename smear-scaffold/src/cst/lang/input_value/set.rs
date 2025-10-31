use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a GraphQL set value: `<item1, item2, ...>`
///
/// Unlike the AST version, preserves all angle brackets and padding.
///
/// ## Examples
/// ```text
/// <1, 2, 3>
/// < "a" , "b" >  # preserves spacing
/// <
///   "item1"  # comment
///   "item2"
/// >
/// ```
#[derive(Debug, Clone)]
pub struct Set<Item, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Item>> {
  span: Span,
  /// Padding around the left angle bracket
  langle_padding: Padding<S, TriviaContainer>,
  /// Items with their trivia
  items: Container,
  /// Padding around the right angle bracket
  rangle_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<Item>,
}

impl<Item, S, TriviaContainer, Container> AsSpan<Span> for Set<Item, S, TriviaContainer, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Item, S, TriviaContainer, Container> IntoSpan<Span> for Set<Item, S, TriviaContainer, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Item, S, TriviaContainer, Container> IntoComponents for Set<Item, S, TriviaContainer, Container> {
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
      self.langle_padding,
      self.items,
      self.rangle_padding,
    )
  }
}

impl<Item, S, TriviaContainer, Container> Set<Item, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST Set.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      langle_padding: Padding::new(),
      items: Container::default(),
      rangle_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST Set with all components.
  pub const fn with_parts(
    span: Span,
    langle_padding: Padding<S, TriviaContainer>,
    items: Container,
    rangle_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      langle_padding,
      items,
      rangle_padding,
      _marker: PhantomData,
    }
  }
}

impl<Item, S, TriviaContainer, Container> Set<Item, S, TriviaContainer, Container> {
  /// Returns the span covering the entire set.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left angle bracket padding.
  #[inline]
  pub const fn langle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.langle_padding
  }

  /// Returns a reference to the items container.
  #[inline]
  pub const fn items(&self) -> &Container {
    &self.items
  }

  /// Returns a reference to the right angle bracket padding.
  #[inline]
  pub const fn rangle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rangle_padding
  }
}
