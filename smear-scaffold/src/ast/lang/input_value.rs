use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};


pub use list::List;
pub use map::{Map, MapEntry};
pub use object::*;
pub use set::Set;

mod list;
mod map;
mod object;
mod set;

/// A GraphQL default value assignment for input parameters.
///
/// Represents the default value assignment syntax used in GraphQL variable
/// declarations, field arguments, and input type definitions. Default values
/// provide fallback values when no explicit value is provided, following
/// GraphQL's default value semantics and constant expression requirements.
///
/// ## Specification Rules
///
/// GraphQL default values follow strict formatting and semantic rules:
/// - **Equals syntax**: Must use `=` to assign the default value
/// - **Constant requirement**: Default values must be constant expressions (no variables)
/// - **Type compatibility**: Default value type must match the declared type
/// - **Nullability handling**: Non-null types can have null defaults (making them effectively nullable)
/// - **Whitespace flexibility**: Optional whitespace around the `=` token
///
/// ## Grammar
///
/// ```text
/// DefaultValue ::= '=' Value
/// ```
#[derive(Debug, Clone, Copy)]
pub struct DefaultInputValue<Value> {
  span: Span,
  value: Value,
}

impl<Value> DefaultInputValue<Value> {
  /// Returns the source span of the entire default value assignment.
  ///
  /// This span covers from the `=` token through the last character of the
  /// default value, providing the complete source location for error reporting
  /// and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the default value expression.
  ///
  /// This provides access to the constant expression that serves as the
  /// default value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

impl<Value> AsSpan<Span> for DefaultInputValue<Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Value> IntoSpan<Span> for DefaultInputValue<Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value> IntoComponents for DefaultInputValue<Value> {
  type Components = (Span, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}
