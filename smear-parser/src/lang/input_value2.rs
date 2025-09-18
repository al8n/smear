use chumsky::{Parser, extra::ParserExtra};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_utils::{IntoComponents, IntoSpan};

use crate::lang::punctuator::Equal;

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
  eq: Equal,
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

  /// Returns the equals assignment token.
  ///
  /// This provides access to the `=` character that introduces the default
  /// value assignment, including its exact source position. Useful for syntax
  /// highlighting and precise error reporting.
  #[inline]
  pub const fn eq(&self) -> &Equal {
    &self.eq
  }

  /// Returns the default value expression.
  ///
  /// This provides access to the constant expression that serves as the
  /// default value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Creates a parser for default value assignments with constant validation.
  ///
  /// This parser handles the complete default value syntax including the equals
  /// token, optional whitespace, and the default value expression. It enforces
  /// GraphQL's requirement that default values must be constant expressions
  /// through compile-time type constraints.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, VP>(
    value_parser: VP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Error: 'a,
    Equal: Parseable<'a, I, T, Error> + 'a,
    VP: Parser<'a, I, Value, E> + Clone,
  {
    Equal::parser()
      .then(value_parser)
      .map_with(|(eq, value), exa| Self {
        span: exa.span(),
        eq,
        value,
      })
  }
}

impl<Value> AsRef<Span> for DefaultInputValue<Value> {
  #[inline]
  fn as_ref(&self) -> &Span {
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
  type Components = (Span, Equal, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.eq, self.value)
  }
}

impl<'a, Value, I, T, Error> Parseable<'a, I, T, Error> for DefaultInputValue<Value>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Value: Parseable<'a, I, T, Error>,
  Equal: Parseable<'a, I, T, Error>,
  Error: 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Value::parser())
  }
}
