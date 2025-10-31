use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a map entry: `key => value`
///
/// Preserves:
/// - The key with its padding
/// - The fat arrow `=>` with its padding
/// - The value with its padding
#[derive(Debug, Clone)]
pub struct MapEntry<Key, Value, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// The key
  key: Key,
  /// Padding around the fat arrow
  fat_arrow_padding: Padding<S, TriviaContainer>,
  /// The value
  value: Value,
}

impl<Key, Value, S, TriviaContainer> AsSpan<Span> for MapEntry<Key, Value, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, S, TriviaContainer> IntoSpan<Span> for MapEntry<Key, Value, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value, S, TriviaContainer> IntoComponents for MapEntry<Key, Value, S, TriviaContainer> {
  type Components = (Span, Key, Padding<S, TriviaContainer>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.fat_arrow_padding, self.value)
  }
}

impl<Key, Value, S, TriviaContainer> MapEntry<Key, Value, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST MapEntry.
  pub fn new(span: Span, key: Key, value: Value) -> Self {
    Self {
      span,
      key,
      fat_arrow_padding: Padding::new(),
      value,
    }
  }
}

impl<Key, Value, S, TriviaContainer> MapEntry<Key, Value, S, TriviaContainer> {
  /// Returns the span covering the entire entry.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the key.
  #[inline]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns a reference to the fat arrow padding.
  #[inline]
  pub const fn fat_arrow_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.fat_arrow_padding
  }

  /// Returns a reference to the value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

/// CST representation of a GraphQL map value: `<key => value, ...>`
///
/// Unlike the AST version, preserves all angle brackets and padding.
#[derive(Debug, Clone)]
pub struct Map<Entry, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Entry>> {
  span: Span,
  /// Padding around the left angle bracket
  langle_padding: Padding<S, TriviaContainer>,
  /// Entries with their trivia
  entries: Container,
  /// Padding around the right angle bracket
  rangle_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<Entry>,
}

impl<Entry, S, TriviaContainer, Container> AsSpan<Span> for Map<Entry, S, TriviaContainer, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Entry, S, TriviaContainer, Container> IntoSpan<Span> for Map<Entry, S, TriviaContainer, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Entry, S, TriviaContainer, Container> IntoComponents for Map<Entry, S, TriviaContainer, Container> {
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
      self.entries,
      self.rangle_padding,
    )
  }
}

impl<Entry, S, TriviaContainer, Container> Map<Entry, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST Map.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      langle_padding: Padding::new(),
      entries: Container::default(),
      rangle_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST Map with all components.
  pub const fn with_parts(
    span: Span,
    langle_padding: Padding<S, TriviaContainer>,
    entries: Container,
    rangle_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      langle_padding,
      entries,
      rangle_padding,
      _marker: PhantomData,
    }
  }
}

impl<Entry, S, TriviaContainer, Container> Map<Entry, S, TriviaContainer, Container> {
  /// Returns the span covering the entire map.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left angle bracket padding.
  #[inline]
  pub const fn langle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.langle_padding
  }

  /// Returns a reference to the entries container.
  #[inline]
  pub const fn entries(&self) -> &Container {
    &self.entries
  }

  /// Returns a reference to the right angle bracket padding.
  #[inline]
  pub const fn rangle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rangle_padding
  }
}
