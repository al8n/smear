use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  super::{char::Char, convert::*, source::Source, spanned::Spanned},
  punct::{LBracket, RBracket},
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
/// ## Format
///
/// ```text
/// ListValue ::= '[' Values? ']'
/// Values    ::= Value+
/// ```
///
/// ## Generic Parameters
///
/// - `Value`: The type of elements contained in the list
/// - `Src`: The source slice type (typically `&str`)
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
/// ## Design Philosophy
///
/// This parser is designed to be flexible and efficient:
/// - **Generic value type**: Works with any parseable value type
/// - **Configurable container**: Allows optimization for different use cases
/// - **Precise spans**: Each component retains exact source location
/// - **Whitespace tolerant**: Handles GraphQL's flexible whitespace rules
/// - **Error friendly**: Detailed position information for parse errors
///
/// ## Usage in GraphQL
///
/// List literals appear throughout GraphQL syntax:
/// - **Query arguments**: `users(ids: [1, 2, 3])`
/// - **Variable values**: `{ "tags": ["urgent", "bug"] }`
/// - **Default values**: `field(items: [String] = ["default"])`
/// - **Input object fields**: `{ scores: [95, 87, 92] }`
/// - **Nested structures**: `{ users: [{ id: 1 }, { id: 2 }] }`
///
/// ## Memory and Performance
///
/// - **Zero-copy parsing**: Source references avoid string allocation when possible
/// - **Lazy evaluation**: Values parsed on-demand by the value parser
/// - **Container optimization**: Custom containers can optimize for specific patterns
/// - **Span preservation**: All position information retained for tooling
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone)]
pub struct ListValue<Value, Src, Span, Container = Vec<Value>> {
  span: Spanned<Src, Span>,
  l_bracket: LBracket<Src, Span>,
  r_bracket: RBracket<Src, Span>,
  values: Container,
  _value: core::marker::PhantomData<Value>,
}

impl<Value, Src, Span, Container> ListValue<Value, Src, Span, Container> {
  /// Returns the source span of the entire list literal.
  ///
  /// This span covers from the opening bracket through the closing bracket,
  /// including all whitespace and elements within. Useful for error reporting,
  /// source mapping, and extracting the complete list text.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the opening bracket token.
  ///
  /// This provides access to the `[` character that begins the list,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and precise error reporting at list boundaries.
  #[inline]
  pub const fn l_bracket(&self) -> &LBracket<Src, Span> {
    &self.l_bracket
  }

  /// Returns the closing bracket token.
  ///
  /// This provides access to the `]` character that ends the list,
  /// including its exact source position. Useful for syntax highlighting,
  /// bracket matching, and detecting incomplete lists.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket<Src, Span> {
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
  pub fn parser_with<'src, I, E, P, const CONST: bool>(
    value: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    Src: 'src,
    Span: 'src,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,

    P: Parser<'src, I, Value, E> + Clone + 'src,
    Container: chumsky::container::Container<Value>,
    Value: crate::language::input_value::InputValue<CONST>,
  {
    let ws = super::ignored::ignored();

    // Keep bracket token spans precise; don't pad the tokens themselves.
    let open = LBracket::parser();
    let close = RBracket::parser();

    // Each element owns only *trailing* ignored (incl. commas).
    let elem = value.then_ignore(ws.clone());

    // '[' ws? ( ']' | elem+ ']' )
    open
      .then_ignore(ws.clone())
      .then(choice((
        // Empty fast path: immediately see ']'
        close.clone().map(|r| (Container::default(), r)),
        // Non-empty: one-or-more elements; trailing commas handled by elem’s trailing ws
        elem
          .repeated()
          .at_least(1)
          .collect::<Container>()
          .then(close),
      )))
      .map_with(|(l_bracket, (values, r_bracket)), sp| Self {
        span: Spanned::from(sp),
        l_bracket,
        r_bracket,
        values,
        _value: core::marker::PhantomData,
      })
  }
}

impl<Value, Src, Span, Container> AsSpanned<Src, Span> for ListValue<Value, Src, Span, Container> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    self.span()
  }
}

impl<Value, Src, Span, Container> IntoSpanned<Src, Span>
  for ListValue<Value, Src, Span, Container>
{
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<Value, Src, Span, Container> IntoComponents for ListValue<Value, Src, Span, Container> {
  type Components = (
    Spanned<Src, Span>,
    LBracket<Src, Span>,
    RBracket<Src, Span>,
    Container,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_bracket, self.r_bracket, self.values)
  }
}
