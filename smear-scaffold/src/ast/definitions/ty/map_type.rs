use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// Represents a GraphQLx map type with optional non-null modifier.
///
/// Map types represent a collection of key-value pairs in GraphQLx. They wrap
/// another type (the element type) to indicate that fields of this type return
/// multiple values of the wrapped type.
///
/// Map types support complex nullability semantics:
/// - The map itself can be null or non-null
/// - The key and value within the map can be null or non-null
/// - These nullability rules are independent and composable
///
/// ## Grammar
/// ```text
/// MapType : < Key !? => Value !? > !?
/// ```
#[derive(Debug, Clone, Copy)]
pub struct MapType<Key, Value> {
  span: Span,
  key: Key,
  value: Value,
  required: bool,
}

impl<Key, Value> AsSpan<Span> for MapType<Key, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value> IntoSpan<Span> for MapType<Key, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value> IntoComponents for MapType<Key, Value> {
  type Components = (Span, Key, Value, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.value, self.required)
  }
}

impl<Key, Value> MapType<Key, Value> {
  /// Creates a new map type instance.
  #[inline]
  pub const fn new(span: Span, key: Key, value: Value, required: bool) -> Self {
    Self {
      span,
      key,
      value,
      required,
    }
  }

  /// Returns a reference to the span covering the entire map type.
  ///
  /// The span includes the brackets, element type, and optional bang modifier.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the key type contained within the map.
  ///
  /// This is the type of individual elements in the map. It can be any
  /// valid GraphQLx type including named types, other map types, or even
  /// nested map types for multi-dimensional arrays.
  #[inline]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns a reference to the value type contained within the map.
  ///
  /// This is the type of individual elements in the map. It can be any
  /// valid GraphQLx type including named types, other map types, or even
  /// nested map types for multi-dimensional arrays.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns whether the map type is non-null (required).
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }
}
