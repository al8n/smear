use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};

use crate::cst::Padding;

/// CST representation of a GraphQL map type: `<Key => Value>!?`
///
/// Preserves all tokens including angle brackets, fat arrow, and bang.
///
/// ## Grammar
/// ```text
/// MapType : < Key => Value > !?
/// ```
#[derive(Debug, Clone)]
pub struct MapType<Key, Value, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// Padding around the left angle bracket
  langle_padding: Padding<S, TriviaContainer>,
  /// The key type
  key: Key,
  /// Padding around the fat arrow
  fat_arrow_padding: Padding<S, TriviaContainer>,
  /// The value type
  value: Value,
  /// Padding around the right angle bracket
  rangle_padding: Padding<S, TriviaContainer>,
  /// Optional bang token with its padding
  bang: Option<Padding<S, TriviaContainer>>,
}

impl<Key, Value, S, TriviaContainer> AsSpan<Span> for MapType<Key, Value, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, S, TriviaContainer> IntoSpan<Span> for MapType<Key, Value, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value, S, TriviaContainer> IntoComponents for MapType<Key, Value, S, TriviaContainer> {
  type Components = (
    Span,
    Padding<S, TriviaContainer>,
    Key,
    Padding<S, TriviaContainer>,
    Value,
    Padding<S, TriviaContainer>,
    Option<Padding<S, TriviaContainer>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.langle_padding,
      self.key,
      self.fat_arrow_padding,
      self.value,
      self.rangle_padding,
      self.bang,
    )
  }
}

impl<Key, Value, S, TriviaContainer> MapType<Key, Value, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST MapType.
  pub fn new(span: Span, key: Key, value: Value) -> Self {
    Self {
      span,
      langle_padding: Padding::new(),
      key,
      fat_arrow_padding: Padding::new(),
      value,
      rangle_padding: Padding::new(),
      bang: None,
    }
  }

  /// Creates a new CST MapType with all components.
  #[allow(clippy::too_many_arguments)]
  pub const fn with_parts(
    span: Span,
    langle_padding: Padding<S, TriviaContainer>,
    key: Key,
    fat_arrow_padding: Padding<S, TriviaContainer>,
    value: Value,
    rangle_padding: Padding<S, TriviaContainer>,
    bang: Option<Padding<S, TriviaContainer>>,
  ) -> Self {
    Self {
      span,
      langle_padding,
      key,
      fat_arrow_padding,
      value,
      rangle_padding,
      bang,
    }
  }
}

impl<Key, Value, S, TriviaContainer> MapType<Key, Value, S, TriviaContainer> {
  /// Returns the span covering the entire map type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left angle bracket padding.
  #[inline]
  pub const fn langle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.langle_padding
  }

  /// Returns a reference to the key type.
  #[inline]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns a reference to the fat arrow padding.
  #[inline]
  pub const fn fat_arrow_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.fat_arrow_padding
  }

  /// Returns a reference to the value type.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns a reference to the right angle bracket padding.
  #[inline]
  pub const fn rangle_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rangle_padding
  }

  /// Returns a reference to the bang padding, if present.
  #[inline]
  pub const fn bang(&self) -> Option<&Padding<S, TriviaContainer>> {
    self.bang.as_ref()
  }

  /// Returns whether this type is required (non-null).
  #[inline]
  pub const fn required(&self) -> bool {
    self.bang.is_some()
  }
}
