use chumsky::{extra::ParserExtra, prelude::*};

use core::marker::PhantomData;
use std::vec::Vec;

use crate::{
  lang::{
    ignored,
    punct::{LParen, RParen},
  },
  source::*,
};

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
pub struct ArgumentsDefinition<InputValueDefinition, Span, Container = Vec<InputValueDefinition>> {
  span: Span,
  l_paren: LParen<Span>,
  values: Container,
  r_paren: RParen<Span>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Span, Container> AsRef<Span>
  for ArgumentsDefinition<InputValueDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<InputValueDefinition, Span, Container> IntoSpan<Span>
  for ArgumentsDefinition<InputValueDefinition, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValueDefinition, Span, Container> IntoComponents
  for ArgumentsDefinition<InputValueDefinition, Span, Container>
{
  type Components = (Span, LParen<Span>, Container, RParen<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.values, self.r_paren)
  }
}

impl<InputValueDefinition, Span, Container>
  ArgumentsDefinition<InputValueDefinition, Span, Container>
{
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

  /// Returns a reference to the opening left parenthesis (`(`) of the arguments definition.
  ///
  /// This provides access to the exact location and span information of the
  /// opening delimiter, which can be useful for precise error reporting or
  /// syntax highlighting.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    &self.l_paren
  }

  /// Returns a reference to the closing right parenthesis (`)`) of the arguments definition.
  ///
  /// This provides access to the exact location and span information of the
  /// closing delimiter, which can be useful for precise error reporting or
  /// syntax highlighting.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    &self.r_paren
  }

  /// Creates a parser that can parse an arguments definition with a custom input value definition parser.
  ///
  /// This parser handles the complete arguments definition syntax including the parentheses
  /// and ensures at least one input value definition is present. The parsing of individual
  /// input value definitions is delegated to the provided parser.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the arguments definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, E, P>(
    input_value_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, InputValueDefinition, E> + Clone,
    Container: chumsky::container::Container<InputValueDefinition>,
  {
    LParen::parser()
      .then(
        input_value_definition_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then(RParen::parser())
      .map_with(|((l_paren, values), r_paren), sp| Self {
        span: Span::from_map_extra(sp),
        values,
        l_paren,
        r_paren,
        _input_value_definition: PhantomData,
      })
  }
}
