use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use core::marker::PhantomData;
use std::vec::Vec;

/// A single entry in a GraphQLx map literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapEntry<Key, Value> {
  span: Span,
  key: Key,
  value: Value,
}

impl<Key, Value> AsSpan<Span> for MapEntry<Key, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value> IntoSpan<Span> for MapEntry<Key, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value> IntoComponents for MapEntry<Key, Value> {
  type Components = (Span, Key, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.value)
  }
}

impl<Key, Value> MapEntry<Key, Value> {
  /// Creates a new map entry with the given key and value.
  #[inline]
  pub const fn new(span: Span, key: Key, value: Value) -> Self {
    Self { span, key, value }
  }

  /// Returns the span of the map entry.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the key of the map entry.
  #[inline]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns the value of the map entry.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

/// A GraphQLx map literal value.
///
/// Represents a complete map literal as defined by the GraphQLx specification.
/// Map literals are ordered collections of values enclosed in square brackets,
/// supporting any valid GraphQLx values including nested maps and objects.
///
/// ## Specification Rules
///
/// GraphQLx map literals follow these formatting rules:
/// - **Bracket delimiters**: Must be enclosed in `[` and `]`
/// - **Value separation**: Elements separated by whitespace (commas optional but conventional)
/// - **Trailing commas**: Allowed after the last element
/// - **Nested values**: Can contain any valid GraphQLx input values
/// - **Empty maps**: `[]` is a valid empty map
/// - **Whitespace handling**: Flexible whitespace and comments between elements
///
/// ## Grammar
///
/// ```text
/// Map ::= 'map' '{' Entries? '}'
/// Entries ::= MapEntry+
///
/// MapEntry ::= Key '=>' Value
/// ```
///
/// ## Generic Parameters
/// - `Key`: The type of keys in the map
/// - `Value`: The type of values contained in the map
/// - `Container`: The collection type (defaults to `Vec<MapEntry<Key, Value>>`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<MapEntry<Key, Value>>` (default): Standard dynamic array
/// - Any type implementing `tokit::container::Container<MapEntry<Key, Value>>`
///
/// ## Component Structure
///
/// Each map literal contains:
/// - **Overall span**: Covers the entire map including brackets
/// - **Left bracket**: The opening `[` token with its position
/// - **Right bracket**: The closing `]` token with its position  
/// - **Values**: The parsed elements in their container
#[derive(Debug, Clone)]
pub struct Map<Key, Value, Container = Vec<MapEntry<Key, Value>>> {
  span: Span,
  entries: Container,
  _m: PhantomData<MapEntry<Key, Value>>,
}

impl<Key, Value, Container> Map<Key, Value, Container> {
  /// Creates a new map literal with the given span and entries.
  #[inline]
  pub const fn new(span: Span, entries: Container) -> Self {
    Self {
      span,
      entries,
      _m: PhantomData,
    }
  }

  /// Returns the span covering the entire map literal.
  ///
  /// This span includes the opening and closing brackets as well as all contained values.
  /// It is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the parsed map elements.
  ///
  /// This provides access to all elements that were successfully parsed
  /// from the map literal.
  #[inline]
  pub const fn entries(&self) -> &Container {
    &self.entries
  }

  /// Returns the entries as a slice, if the container supports it.
  #[inline]
  pub fn entries_slice(&self) -> &[MapEntry<Key, Value>]
  where
    Container: AsRef<[MapEntry<Key, Value>]>,
  {
    self.entries().as_ref()
  }
}

impl<Key, Value, Container> AsSpan<Span> for Map<Key, Value, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Container> IntoSpan<Span> for Map<Key, Value, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value, Container> IntoComponents for Map<Key, Value, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.entries)
  }
}
