use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use core::marker::PhantomData;

use crate::punctuator::{Colon, LParen, RParen};

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
  value: Value,
}

impl<Name, Value> AsSpan<Span> for Argument<Name, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
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
  type Components = (Span, Name, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.value)
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

  /// Creates a parser for arguments with custom name and value parsers.
  ///
  /// This parser handles the complete argument syntax including the name,
  /// colon, and value. It allows for flexible parsing of both the name and
  /// value components by accepting custom parsers for each. This enables
  /// integration with different name and value parsing strategies as needed.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, N, V>(
    name_parser: N,
    value_parser: V,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Colon: Parseable<'a, I, T, Error>,
    N: Parser<'a, I, Name, E> + Clone,
    V: Parser<'a, I, Value, E> + Clone,
  {
    name_parser
      .then_ignore(Colon::parser())
      .then(value_parser)
      .map_with(|(name, value), exa| {
        let span = exa.span();
        Self { span, name, value }
      })
  }
}

impl<'a, Name, Value, I, T, Error> Parseable<'a, I, T, Error> for Argument<Name, Value>
where
  Name: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error>,
  Value: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser(), Value::parser())
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
pub struct Arguments<Arg, Container = std::vec::Vec<Arg>> {
  span: Span,
  l_paren: LParen,
  arguments: Container,
  r_paren: RParen,
  _arg: PhantomData<Arg>,
}

impl<Arg, Container> AsSpan<Span> for Arguments<Arg, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Arg, Container> IntoSpan<Span> for Arguments<Arg, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Arg, Container> IntoComponents for Arguments<Arg, Container> {
  type Components = (Span, LParen, Container, RParen);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.arguments, self.r_paren)
  }
}

impl<Arg, Container> Arguments<Arg, Container> {
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
  pub const fn l_paren(&self) -> &LParen {
    &self.l_paren
  }

  /// Returns the closing parenthesis token.
  ///
  /// This provides access to the `)` character that ends the argument list,
  /// including its exact source position. Useful for syntax highlighting,
  /// parenthesis matching, and detecting incomplete argument lists.
  #[inline]
  pub const fn r_paren(&self) -> &RParen {
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
}

impl<'a, Arg, Container, I, T, Error> Parseable<'a, I, T, Error> for Arguments<Arg, Container>
where
  Arg: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<Arg>,
  LParen: Parseable<'a, I, T, Error>,
  RParen: Parseable<'a, I, T, Error>,
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
    LParen::parser()
      .then(Arg::parser().repeated().at_least(1).collect())
      .then(RParen::parser())
      .map_with(|((l_paren, arguments), r_paren), exa| Self {
        span: exa.span(),
        l_paren,
        arguments,
        r_paren,
        _arg: PhantomData,
      })
  }
}
