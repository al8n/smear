use chumsky::{Parser, extra::ParserExtra};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_utils::{IntoComponents, IntoSpan};

use crate::lang::punctuator::Colon;

/// A single named argument in a GraphQL operation or directive.
///
/// Represents a name-value pair used to pass parameters to GraphQL fields,
/// directives, or other language constructs. Arguments follow the standard
/// GraphQL syntax of a name identifier followed by a colon and a value.
///
/// ## Grammar
///
/// ```text
/// Argument ::= Name ':' Value
/// ```
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument)
#[derive(Debug, Clone, Copy)]
pub struct Argument<Name, Value> {
  span: Span,
  name: Name,
  colon: Colon,
  value: Value,
}

impl<Name, Value> AsRef<Span> for Argument<Name, Value> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Value> IntoSpan<Span> for Argument<Name, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Value> IntoComponents for Argument<Name, Value> {
  type Components = (Span, Name, Colon, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<Name, Value> Argument<Name, Value> {
  /// Returns the source span of the entire argument.
  ///
  /// This span covers from the first character of the argument name through
  /// the last character of the argument value, providing the complete source
  /// location for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the colon separator token.
  ///
  /// This provides access to the `:` character that separates the argument
  /// name from its value, including its exact source position. Useful for
  /// syntax highlighting and precise error reporting.
  #[inline]
  pub const fn colon(&self) -> &Colon {
    &self.colon
  }

  /// Returns the argument name identifier.
  ///
  /// This provides access to the GraphQL name that identifies this argument.
  /// The name follows standard GraphQL identifier rules and is used to match
  /// the argument with its expected parameter in the schema.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the argument value.
  ///
  /// This provides access to the value assigned to this argument. The value
  /// can be any valid GraphQL input value including scalars, enums, objects,
  /// lists, variables, or null depending on the argument's expected type.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

impl<'a, Name, Value, I, T, Error> Parseable<'a, I, T, Error> for Argument<Name, Value>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Name: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error>,
  Value: Parseable<'a, I, T, Error>,
  Error: 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    <Name as Parseable<'a, I, T, Error>>::parser()
      .then(<Colon as Parseable<'a, I, T, Error>>::parser())
      .then(<Value as Parseable<'a, I, T, Error>>::parser())
      .map_with(|((name, colon), value), exa| {
        let span = exa.span();
        Self {
          span,
          name,
          colon,
          value,
        }
      })
  }
}
