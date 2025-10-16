use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser as _, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::error::UnclosedBracketError;

use crate::punctuator::{LBracket, RBracket};

use core::marker::PhantomData;

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
/// - Any type implementing `chumsky::container::Container<Value, Container>`
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
pub struct List<Value, Container = std::vec::Vec<Value>> {
  span: Span,
  values: Container,
  _m: PhantomData<Value>,
}

impl<Value, Container> List<Value, Container> {
  /// Creates a new list literal with the given span and values.
  #[inline]
  pub(crate) const fn new(span: Span, values: Container) -> Self {
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

  /// Creates a parser for GraphQL list literals with customizable value parsing.
  ///
  /// This parser handles the complete list syntax including brackets and
  /// enforces proper structure. It uses the provided `value_parser` to parse
  /// each individual element within the list.
  ///
  /// ## Error Handling
  ///
  /// If the closing bracket is missing, the parser invokes the provided
  /// `on_missing_rbracket` function to generate a custom error message.
  /// This allows for context-specific error reporting.
  pub fn parser_with<'src, I, T, Error, VP, E>(
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnclosedBracketError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    VP: Parser<'src, I, Value, E> + Clone + 'src,
    Container: chumsky::container::Container<Value>,
    LBracket: Parseable<'src, I, T, Error>,
    RBracket: Parseable<'src, I, T, Error>,
  {
    LBracket::parser()
      .ignore_then(value_parser.repeated().collect())
      .then(RBracket::parser().or_not())
      .try_map(move |(values, r), span| match r {
        Some(_) => Ok(List::new(span, values)),
        None => Err(Error::unclosed_bracket(span)),
      })
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

impl<'a, Value, Container, I, T, Error> Parseable<'a, I, T, Error> for List<Value, Container>
where
  Error: UnclosedBracketError + 'a,
  Value: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<Value>,
  LBracket: Parseable<'a, I, T, Error>,
  RBracket: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Value::parser())
  }
}
