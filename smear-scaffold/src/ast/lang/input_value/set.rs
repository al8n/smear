use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use core::marker::PhantomData;
use std::vec::Vec;

/// A GraphQLx set literal value.
///
/// Represents a complete set literal as defined by the GraphQLx specification.
/// Set literals are ordered collections of values enclosed in square brackets,
/// supporting any valid GraphQLx values including nested sets and objects.
///
/// ## Specification Rules
///
/// GraphQLx set literals follow these formatting rules:
/// - **Bracket delimiters**: Must be enclosed in `[` and `]`
/// - **Value separation**: Elements separated by whitespace (commas optional but conventional)
/// - **Trailing commas**: Allowed after the last element
/// - **Nested values**: Can contain any valid GraphQLx input values
/// - **Empty sets**: `[]` is a valid empty set
/// - **Whitespace handling**: Flexible whitespace and comments between elements
///
/// ## Grammar
///
/// ```text
/// Set ::= 'set' '{' Values? '}'
/// Values    ::= Value+
/// ```
///
/// ## Generic Parameters
///
/// - `Value`: The type of elements contained in the set
/// - `Container`: The collection type (defaults to `Vec<Value>`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<Value>` (default): Standard dynamic array
/// - Any type implementing `tokit::container::Container<Value>`
///
/// ## Component Structure
///
/// Each set literal contains:
/// - **Overall span**: Covers the entire set including brackets
/// - **Left bracket**: The opening `[` token with its position
/// - **Right bracket**: The closing `]` token with its position  
/// - **Values**: The parsed elements in their container
#[derive(Debug, Clone)]
pub struct Set<Value, Container = Vec<Value>> {
  span: Span,
  values: Container,
  _m: PhantomData<Value>,
}

impl<Value, Container> Set<Value, Container> {
  /// Creates a new set literal with the given span and values.
  #[inline]
  pub const fn new(span: Span, values: Container) -> Self {
    Self {
      span,
      values,
      _m: PhantomData,
    }
  }

  /// Returns the span covering the entire set literal.
  ///
  /// This span includes the opening and closing brackets as well as all contained values.
  /// It is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the parsed set elements.
  ///
  /// This provides access to all elements that were successfully parsed
  /// from the set literal.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }
}

impl<Value, Container> AsSpan<Span> for Set<Value, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value, Container> IntoSpan<Span> for Set<Value, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Container> IntoComponents for Set<Value, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.values)
  }
}
