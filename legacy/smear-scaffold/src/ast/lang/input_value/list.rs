use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use core::marker::PhantomData;
use std::vec::Vec;

/// A GraphQL list literal value.
///
/// Represents a complete list literal as defined by the GraphQL specification.
/// List literals are ordered collections of values enclosed in square brackets,
/// supporting any valid GraphQL values including nested lists and objects.
///
/// ## Specification Rules
///
/// GraphQL list literals follow these formatting rules:
/// - **Bracket delimiters**: Must be enclosed in `[` and `]`
/// - **Value separation**: Elements separated by whitespace (commas optional but conventional)
/// - **Trailing commas**: Allowed after the last element
/// - **Nested values**: Can contain any valid GraphQL input values
/// - **Empty lists**: `[]` is a valid empty list
/// - **Whitespace handling**: Flexible whitespace and comments between elements
///
/// ## Grammar
///
/// ```text
/// List ::= '[' Values? ']'
/// Values    ::= Value+
/// ```
///
/// ## Generic Parameters
///
/// - `Value`: The type of elements contained in the list
/// - `Span`: The span type for position information
/// - `Container`: The collection type (defaults to `Vec<Value, Container>`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<Value, Container>` (default): Standard dynamic array
/// - Any type implementing `tokora::container::Container<Value, Container>`
///
/// ## Component Structure
///
/// Each list literal contains:
/// - **Overall span**: Covers the entire list including brackets
/// - **Left bracket**: The opening `[` token with its position
/// - **Right bracket**: The closing `]` token with its position  
/// - **Values**: The parsed elements in their container
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone)]
pub struct List<Value, Container = Vec<Value>> {
  span: Span,
  values: Container,
  _m: PhantomData<Value>,
}

impl<Value, Container> List<Value, Container> {
  /// Creates a new list literal with the given span and values.
  #[inline]
  pub const fn new(span: Span, values: Container) -> Self {
    Self {
      span,
      values,
      _m: PhantomData,
    }
  }

  /// Returns the span covering the entire list literal.
  ///
  /// This span includes the opening and closing brackets as well as all contained values.
  /// It is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the parsed list elements.
  ///
  /// This provides access to all elements that were successfully parsed
  /// from the list literal.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }
}

impl<Value, Container> AsSpan<Span> for List<Value, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value, Container> IntoSpan<Span> for List<Value, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Container> IntoComponents for List<Value, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.values)
  }
}
