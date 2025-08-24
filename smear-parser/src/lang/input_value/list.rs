use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{
    ignored,
    punct::{LBracket, RBracket},
    Const,
  },
  source::{Char, Slice, Source},
};

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
#[derive(Debug, Clone, Copy)]
pub struct List<Value, Span, Container = Vec<Value>> {
  span: Span,
  l_bracket: LBracket<Span>,
  r_bracket: RBracket<Span>,
  values: Container,
  _value: core::marker::PhantomData<Value>,
}

impl<Value, Span, Container> Const<true> for List<Value, Span, Container> where Value: Const<true> {}
impl<Value, Span, Container> Const<false> for List<Value, Span, Container> where Value: Const<false> {}

impl<Value, Span, Container> List<Value, Span, Container> {
  /// Returns the source span of the entire list literal.
  ///
  /// This span covers from the opening bracket through the closing bracket,
  /// including all whitespace and elements within. Useful for error reporting,
  /// source mapping, and extracting the complete list text.
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
  pub const fn l_bracket(&self) -> &LBracket<Span> {
    &self.l_bracket
  }

  /// Returns the closing bracket token.
  ///
  /// This provides access to the `]` character that ends the list,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and detecting incomplete lists.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket<Span> {
    &self.r_bracket
  }

  /// Returns the container holding the parsed list elements.
  ///
  /// This provides access to all elements that were successfully parsed
  /// from the list literal.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }

  /// Creates a parser for list literals with a custom value parser.
  ///
  /// This is the core parsing function that accepts any value parser and
  /// creates a complete list parser. It handles all GraphQL list syntax
  /// including whitespace, optional commas, trailing commas, and empty lists.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the list.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P, const CONST: bool>(
    value_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Value, E> + Clone + 'src,
    Container: chumsky::container::Container<Value>,
    Value: Const<CONST>,
  {
    // '[' ws? ( ']' | elem+ ']' )
    LBracket::parser()
      .then_ignore(ignored())
      .then(choice((
        // Empty fast path: immediately see ']'
        RBracket::parser().map(|r| (Container::default(), r)),
        // Non-empty: one-or-more elements; trailing commas handled by elem’s trailing ws
        value_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect::<Container>()
          .then(RBracket::parser()),
      )))
      .map_with(|(l_bracket, (values, r_bracket)), sp| Self {
        span: Span::from_map_extra(sp),
        l_bracket,
        r_bracket,
        values,
        _value: core::marker::PhantomData,
      })
  }
}

impl<Value, Span, Container> AsRef<Span> for List<Value, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span, Container> IntoSpan<Span> for List<Value, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Span, Container> IntoComponents for List<Value, Span, Container> {
  type Components = (Span, LBracket<Span>, Container, RBracket<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_bracket, self.values, self.r_bracket)
  }
}
