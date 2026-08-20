use core::marker::PhantomData;
use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A named field in an input object value.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ObjectField<Name, Value, Span = SimpleSpan> {
  span: Span,
  name: Name,
  value: Value,
}

impl<Name, Value, Span> ObjectField<Name, Value, Span> {
  /// Creates an object field from its span, name, and value.
  #[inline]
  pub const fn new(span: Span, name: Name, value: Value) -> Self {
    Self { span, name, value }
  }

  /// Returns the span covering the complete field.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the field name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the field value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

impl<Name, Value, Span> AsSpan<Span> for ObjectField<Name, Value, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Value, Span> IntoSpan<Span> for ObjectField<Name, Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Value, Span> IntoComponents for ObjectField<Name, Value, Span> {
  type Components = (Span, Name, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.value)
  }
}

/// An input object value with its enclosing source span.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Object<Name, Value, Span = SimpleSpan, Container = Vec<ObjectField<Name, Value, Span>>> {
  span: Span,
  fields: Container,
  _field: PhantomData<(Name, Value)>,
}

impl<Name, Value, Span, Container> Object<Name, Value, Span, Container> {
  /// Creates an object value from its span and fields.
  #[inline]
  pub const fn new(span: Span, fields: Container) -> Self {
    Self {
      span,
      fields,
      _field: PhantomData,
    }
  }

  /// Returns the span covering the complete object.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parsed object fields.
  #[inline]
  pub fn fields(&self) -> &[ObjectField<Name, Value, Span>]
  where
    Container: AsRef<[ObjectField<Name, Value, Span>]>,
  {
    self.fields.as_ref()
  }

  /// Consumes this object and returns its fields.
  #[inline]
  pub fn into_fields(self) -> Container {
    self.fields
  }

  /// The fields, mutably, so a release that only borrows this object can empty it.
  ///
  /// Crate-private for the reasons on [`List::values_mut`](crate::value::List::values_mut); see
  /// [`nesting`](crate::value::nesting) for what needs it.
  #[inline]
  pub(crate) fn fields_mut(&mut self) -> &mut Container {
    &mut self.fields
  }
}

impl<Name, Value, Span, Container> AsSpan<Span> for Object<Name, Value, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Value, Span, Container> IntoSpan<Span> for Object<Name, Value, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Value, Span, Container> IntoComponents for Object<Name, Value, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.fields)
  }
}
