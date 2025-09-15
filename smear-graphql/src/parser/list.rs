use logosky::utils::{Span, sdl_display::DisplaySDL};
use smear_parser::source::{IntoComponents, IntoSpan};

use std::vec::Vec;

use crate::parser::ast::{LBracket, RBracket};

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
/// - `Container`: The collection type (defaults to `Vec<Value>`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<Value>` (default): Standard dynamic array
/// - Any type implementing `chumsky::container::Container<Value>`
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
pub struct List<Value> {
  span: Span,
  l_bracket: LBracket,
  r_bracket: RBracket,
  values: Vec<Value>,
}

impl<Value> List<Value> {
  pub(super) const fn new(
    span: Span,
    l_bracket: LBracket,
    r_bracket: RBracket,
    values: Vec<Value>,
  ) -> Self {
    Self {
      span,
      l_bracket,
      r_bracket,
      values,
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

  /// Returns the opening bracket token.
  ///
  /// This provides access to the `[` character that begins the list,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and precise error reporting at list boundaries.
  #[inline]
  pub const fn l_bracket(&self) -> &LBracket {
    &self.l_bracket
  }

  /// Returns the closing bracket token.
  ///
  /// This provides access to the `]` character that ends the list,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and detecting incomplete lists.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket {
    &self.r_bracket
  }

  /// Returns the container holding the parsed list elements.
  ///
  /// This provides access to all elements that were successfully parsed
  /// from the list literal.
  #[inline]
  pub const fn values(&self) -> &[Value] {
    self.values.as_slice()
  }
}

impl<Value> AsRef<Span> for List<Value> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Value> IntoSpan<Span> for List<Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value> IntoComponents for List<Value> {
  type Components = (Span, LBracket, Vec<Value>, RBracket);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_bracket, self.values, self.r_bracket)
  }
}
