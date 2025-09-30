use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use core::marker::PhantomData;
use std::vec::Vec;

use crate::lang::punctuator::{LParen, RParen};

/// Represents an arguments definition in GraphQL schema syntax.
///
/// An arguments definition specifies the input parameters that can be provided
/// to a field, directive, or other GraphQL construct. It consists of a parenthesized
/// list of input value definitions, where each definition specifies a parameter
/// name, type, optional default value, and optional description.
///
/// Arguments definitions are used throughout GraphQL schemas to define the
/// interface for fields, directives, and other callable constructs.
///
/// ## Examples
///
/// ```text
/// # Simple field with one argument
/// user(id: ID!): User
///
/// # Field with multiple arguments
/// users(
///   first: Int = 10
///   after: String
///   filter: UserFilter
/// ): UserConnection
///
/// # Directive with arguments
/// directive @auth(
///   requires: Role = USER
///   scopes: [String!]
/// ) on FIELD_DEFINITION
///
/// # Field with complex arguments including descriptions
/// search(
///   """
///   The search query string
///   """
///   query: String!
///   
///   """
///   Maximum number of results to return
///   """
///   limit: Int = 20
///   
///   """
///   Result offset for pagination
///   """
///   offset: Int = 0
/// ): [SearchResult!]!
/// ```
///
/// ## Type Parameters
///
/// * `InputValueDefinition` - The type representing individual input value definitions
/// * `Span` - The type representing source location information for error reporting and tooling
/// * `Container` - The container type for storing input value definitions (defaults to `Vec<InputValueDefinition>`)
///
/// ## Grammar
///
/// ```text
/// ArgumentsDefinition : ( InputValueDefinition+ )
/// ```
///
/// Note: The grammar requires at least one input value definition (the `+` indicates one-or-more).
/// Empty argument lists `()` are not valid in GraphQL schema definitions.
///
/// Spec: [ArgumentsDefinition](https://spec.graphql.org/draft/#ArgumentsDefinition)
#[derive(Debug, Clone, Copy)]
pub struct ArgumentsDefinition<InputValueDefinition, Container = Vec<InputValueDefinition>> {
  span: Span,
  values: Container,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Container> AsSpan<Span>
  for ArgumentsDefinition<InputValueDefinition, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<InputValueDefinition, Container> IntoSpan<Span>
  for ArgumentsDefinition<InputValueDefinition, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValueDefinition, Container> IntoComponents
  for ArgumentsDefinition<InputValueDefinition, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.values)
  }
}

impl<InputValueDefinition, Container> ArgumentsDefinition<InputValueDefinition, Container> {
  /// Returns a reference to the span covering the entire arguments definition.
  ///
  /// The span includes the opening parenthesis, all input value definitions,
  /// and the closing parenthesis. This is useful for error reporting, syntax
  /// highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all input value definitions.
  ///
  /// The input value definitions specify the individual arguments that can be
  /// provided, including their names, types, default values, and descriptions.
  /// This allows iteration over, indexing into, or otherwise working with
  /// the collection of argument definitions.
  #[inline]
  pub const fn input_value_definitions(&self) -> &Container {
    &self.values
  }

  /// Consumes and returns the input value definitions.
  #[inline]
  pub fn into_input_value_definitions(self) -> Container {
    self.values
  }

  /// Creates a parser that can parse an arguments definition with a custom input value definition parser.
  ///
  /// This parser handles the complete arguments definition syntax including the parentheses
  /// and ensures at least one input value definition is present. The parsing of individual
  /// input value definitions is delegated to the provided parser.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, P>(
    input_value_definition_parser: P,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LParen: Parseable<'a, I, T, Error> + 'a,
    RParen: Parseable<'a, I, T, Error> + 'a,
    P: Parser<'a, I, InputValueDefinition, E> + Clone,
    Container: chumsky::container::Container<InputValueDefinition>,
  {
    LParen::parser()
      .ignore_then(
        input_value_definition_parser
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then_ignore(RParen::parser())
      .map_with(|values, exa| Self {
        span: exa.span(),
        values,
        _input_value_definition: PhantomData,
      })
  }
}

impl<'a, InputValueDefinition, Container, I, T, Error> Parseable<'a, I, T, Error>
  for ArgumentsDefinition<InputValueDefinition, Container>
where
  InputValueDefinition: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<InputValueDefinition>,
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
    Self::parser_with(InputValueDefinition::parser())
  }
}
