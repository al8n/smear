use core::marker::PhantomData;
use std::vec::Vec;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A key-value entry in a GraphQLx map value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MapEntry<Key, Value, Span = SimpleSpan> {
  span: Span,
  key: Key,
  value: Value,
}

impl<Key, Value, Span> MapEntry<Key, Value, Span> {
  /// Creates a map entry from its complete span, key, and value.
  #[inline]
  pub const fn new(span: Span, key: Key, value: Value) -> Self {
    Self { span, key, value }
  }

  /// Returns the span covering this entry.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the entry key.
  #[inline]
  pub const fn key(&self) -> &Key {
    &self.key
  }

  /// Returns the entry value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

impl<Key, Value, Span> AsSpan<Span> for MapEntry<Key, Value, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Span> IntoSpan<Span> for MapEntry<Key, Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// An entry nests through **both** halves: in GraphQLx a map's key is an input value too.
impl<Key, Value, Span> crate::value::Sealed for MapEntry<Key, Value, Span>
where
  Key: crate::value::Nestable,
  Value: crate::value::Nestable<Node = Key::Node>,
{
}

impl<Key, Value, Span> crate::value::Nestable for MapEntry<Key, Value, Span>
where
  Key: crate::value::Nestable,
  Value: crate::value::Nestable<Node = Key::Node>,
{
  type Node = Key::Node;

  #[inline]
  fn into_children(self, pending: &mut std::vec::Vec<Self::Node>) {
    // Both halves are `Nestable`, so both reach the worklist. `Span` carries no bound and is
    // released here, on the same terms as `ObjectField`'s unbounded slots (al8n/smear#176).
    self.key.into_children(pending);
    self.value.into_children(pending);
  }
}

impl<Key, Value, Span> IntoComponents for MapEntry<Key, Value, Span> {
  type Components = (Span, Key, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.value)
  }
}

/// A GraphQLx map value with its enclosing source span.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Map<Key, Value, Span = SimpleSpan, Container = Vec<MapEntry<Key, Value, Span>>> {
  span: Span,
  entries: Container,
  _entry: PhantomData<MapEntry<Key, Value, Span>>,
}

impl<Key, Value, Span, Container> Map<Key, Value, Span, Container> {
  /// Creates a map value from its span and entries.
  #[inline]
  pub const fn new(span: Span, entries: Container) -> Self {
    Self {
      span,
      entries,
      _entry: PhantomData,
    }
  }

  /// Returns the span covering the complete map.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parsed map entries as a slice.
  #[inline]
  pub fn entries(&self) -> &[MapEntry<Key, Value, Span>]
  where
    Container: AsRef<[MapEntry<Key, Value, Span>]>,
  {
    self.entries.as_ref()
  }

  /// Consumes this map and returns its entry container.
  #[inline]
  pub fn into_entries(self) -> Container {
    self.entries
  }
}

impl<Key, Value, Span, Container> AsSpan<Span> for Map<Key, Value, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Span, Container> IntoSpan<Span> for Map<Key, Value, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Key, Value, Span, Container> IntoComponents for Map<Key, Value, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.entries)
  }
}
