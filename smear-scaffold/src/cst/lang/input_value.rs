use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use std::vec::Vec;

use crate::cst::Padding;

pub use list::*;
pub use map::*;
pub use object::*;
pub use set::*;

mod list;
mod map;
mod object;
mod set;

/// CST representation of a default value assignment: `= value`
///
/// Unlike the AST version, preserves:
/// - The equals sign `=` with its padding
/// - The value with its padding
///
/// ## Examples
/// ```text
/// = "default"
/// =  10
/// = { field: "value" }
/// ```
#[derive(Debug, Clone)]
pub struct DefaultInputValue<Value, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// Padding around the equals sign
  equals_padding: Padding<S, TriviaContainer>,
  /// The default value
  value: Value,
}

impl<Value, S, TriviaContainer> AsSpan<Span> for DefaultInputValue<Value, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value, S, TriviaContainer> IntoSpan<Span> for DefaultInputValue<Value, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, S, TriviaContainer> IntoComponents for DefaultInputValue<Value, S, TriviaContainer> {
  type Components = (Span, Padding<S, TriviaContainer>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.equals_padding, self.value)
  }
}

impl<Value, S, TriviaContainer> DefaultInputValue<Value, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST DefaultInputValue.
  pub fn new(span: Span, value: Value) -> Self {
    Self {
      span,
      equals_padding: Padding::new(),
      value,
    }
  }

  /// Creates a new CST DefaultInputValue with equals padding.
  pub const fn with_equals_padding(
    span: Span,
    equals_padding: Padding<S, TriviaContainer>,
    value: Value,
  ) -> Self {
    Self {
      span,
      equals_padding,
      value,
    }
  }
}

impl<Value, S, TriviaContainer> DefaultInputValue<Value, S, TriviaContainer> {
  /// Returns the span covering the entire default value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the equals padding.
  #[inline]
  pub const fn equals_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.equals_padding
  }

  /// Returns a reference to the value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}
