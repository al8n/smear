use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::source::*,
  Name, ignored,
  punct::{Colon, LParen, RParen},
};

use core::marker::PhantomData;
use std::vec::Vec;

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
pub struct Argument<Value, Span> {
  span: Span,
  name: Name<Span>,
  colon: Colon<Span>,
  value: Value,
}

impl<Value, Span> AsRef<Span> for Argument<Value, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span> IntoSpan<Span> for Argument<Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Span> IntoComponents for Argument<Value, Span> {
  type Components = (Span, Name<Span>, Colon<Span>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<Value, Span> Argument<Value, Span> {
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
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// Returns the argument name identifier.
  ///
  /// This provides access to the GraphQL name that identifies this argument.
  /// The name follows standard GraphQL identifier rules and is used to match
  /// the argument with its expected parameter in the schema.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
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

  /// Creates a parser for GraphQL arguments with a custom value parser.
  ///
  /// This parser handles the complete argument syntax including the argument
  /// name, colon separator, and argument value. It manages whitespace around
  /// the colon according to GraphQL's flexible whitespace rules.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the argument.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, Value, E> + Clone,
  {
    Name::parser()
      .then(Colon::parser().padded_by(ignored()))
      .then(value)
      .map_with(|((name, colon), value), sp| Self {
        span: Span::from_map_extra(sp),
        name,
        colon,
        value,
      })
  }
}

/// A collection of arguments enclosed in parentheses.
///
/// Represents an argument list as used in GraphQL fields, directives, and other
/// language constructs. Arguments are enclosed in parentheses and separated by
/// whitespace, providing a structured way to pass parameters in GraphQL operations.
///
/// ## Specification Rules
///
/// GraphQL argument lists follow these formatting rules:
/// - **Parenthesis delimiters**: Must be enclosed in `(` and `)`
/// - **Argument format**: Each argument follows `name: value` syntax
/// - **Argument separation**: Arguments separated by whitespace (commas optional)
/// - **Non-empty requirement**: Argument lists must contain at least one argument
/// - **Flexible whitespace**: Whitespace and comments allowed throughout
/// - **Unique names**: Argument names should be unique within the list (semantic validation)
///
/// ## Grammar
///
/// ```text
/// Arguments ::= '(' Argument+ ')'
/// Argument ::= Name ':' Value
/// ```
///
/// ## Generic Parameters
///
/// - `Arg`: The type representing individual arguments
/// - `Span`: The span type for position information
/// - `Container`: The collection type for arguments (defaults to `Vec`, can be customized)
///
/// ## Container Flexibility
///
/// The `Container` parameter allows using different collection types:
/// - `Vec<Argument<Value, Span>>` (default): Standard dynamic array
/// - Any type implementing `chumsky::container::Container<Arg>`
///
/// ## Component Structure
///
/// Each argument list contains:
/// - **Overall span**: Covers the entire argument list including parentheses
/// - **Left parenthesis**: The opening `(` token with its position
/// - **Right parenthesis**: The closing `)` token with its position
/// - **Arguments**: The collection of parsed arguments
///
/// Spec: [Arguments](https://spec.graphql.org/draft/#Arguments)
#[derive(Debug, Clone, Copy)]
pub struct Arguments<Arg, Span, Container = Vec<Arg>> {
  span: Span,
  l_paren: LParen<Span>,
  arguments: Container,
  r_paren: RParen<Span>,
  _arg: PhantomData<Arg>,
}

impl<Arg, Span, Container> AsRef<Span> for Arguments<Arg, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Arg, Span, Container> IntoSpan<Span> for Arguments<Arg, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Arg, Span, Container> IntoComponents for Arguments<Arg, Span, Container> {
  type Components = (Span, LParen<Span>, Container, RParen<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.arguments, self.r_paren)
  }
}

impl<Arg, Span, Container> Arguments<Arg, Span, Container> {
  /// Returns the source span of the entire argument list.
  ///
  /// This span covers from the opening parenthesis through the closing
  /// parenthesis, including all arguments and whitespace within. Useful for
  /// error reporting, source mapping, and extracting the complete argument text.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the opening parenthesis token.
  ///
  /// This provides access to the `(` character that begins the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and precise error reporting at argument boundaries.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    &self.l_paren
  }

  /// Returns the closing parenthesis token.
  ///
  /// This provides access to the `)` character that ends the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and detecting incomplete argument lists.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    &self.r_paren
  }

  /// Returns the container holding the arguments.
  ///
  /// This provides access to all arguments that were successfully parsed
  /// from the argument list.
  #[inline]
  pub const fn arguments(&self) -> &Container {
    &self.arguments
  }

  /// Creates a parser for argument lists with a custom argument parser.
  ///
  /// This is the core parsing function that accepts any argument parser and
  /// creates a complete argument list parser. It handles all argument list syntax
  /// including parentheses, argument parsing, whitespace management, and requires
  /// at least one argument to be present.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the arguments.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(arg_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, Arg, E> + Clone,
    Container: chumsky::container::Container<Arg>,
  {
    LParen::parser()
      .then(
        arg_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then(RParen::parser())
      .map_with(|((l_paren, arguments), r_paren), sp| Self {
        span: Span::from_map_extra(sp),
        l_paren,
        arguments,
        r_paren,
        _arg: PhantomData,
      })
  }
}
