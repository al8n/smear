use chumsky::{extra::ParserExtra, prelude::*};

use core::marker::PhantomData;
use std::vec::Vec;

use crate::{
  convert::*,
  lang::{
    ignored,
    punct::{LBrace, RBrace},
  },
  source::{Char, Slice, Source},
};

/// Represents a collection of input field definitions in a GraphQL input object type.
///
/// An input values definition is a braced collection of one or more input value definitions
/// that specify what values are available on an input object type. Input object types are
/// used to represent complex input data in GraphQL operations, particularly for mutation
/// arguments and complex query parameters.
///
/// Input values definitions define the structure and validation rules for data that
/// clients must provide when using the input type.
///
/// ## GraphQL Input Types Context
///
/// Input object types serve several important purposes in GraphQL:
/// - **Mutation arguments**: Structuring complex data for create/update operations
/// - **Query parameters**: Organizing filter, search, and pagination parameters
/// - **Nested inputs**: Creating hierarchical input data structures
/// - **Validation**: Defining required values, types, and default values
/// - **Documentation**: Providing clear interfaces for client developers
///
/// ## Examples
///
/// ```text
/// # Simple input values definition
/// {
///   name: String!
///   email: String!
///   age: Int
/// }
///
/// # Input values with descriptions and default values
/// {
///   """
///   The user's full name (required)
///   """
///   name: String!
///   
///   """
///   The user's email address (required)
///   """
///   email: String!
///   
///   """
///   The user's age (optional, defaults to null)
///   """
///   age: Int
///   
///   """
///   Account status (defaults to ACTIVE)
///   """
///   status: UserStatus = ACTIVE
///   
///   """
///   User preferences with nested structure
///   """
///   preferences: UserPreferencesInput
/// }
///
/// # Input values with directives
/// {
///   name: String! @constraint(minLength: 1, maxLength: 100)
///   email: String! @constraint(format: "email")
///   password: String! @sensitive @constraint(minLength: 8)
///   confirmPassword: String! @sensitive
/// }
///
/// # Complete input type definition
/// """
/// Input for creating a new user account
/// """
/// input CreateUserInput {
///   """
///   The user's display name
///   """
///   name: String!
///   
///   """
///   Unique email address for the account
///   """
///   email: String!
///   
///   """
///   Optional profile information
///   """
///   profile: UserProfileInput
///   
///   """
///   Initial account preferences
///   """
///   preferences: UserPreferencesInput = {
///     theme: LIGHT
///     notifications: true
///   }
/// }
/// ```
///
/// ## Type Parameters
///
/// * `InputValueDefinition` - The type representing individual input field definitions
/// * `Span` - The type representing source location information for error reporting and tooling
/// * `Container` - The container type for storing input field definitions (defaults to `Vec<InputValueDefinition>`)
///
/// ## Grammar
///
/// ```text
/// InputFieldsDefinition : { InputValueDefinition+ }
/// ```
///
/// Note: At least one input field definition is required (the `+` indicates one-or-more).
/// Empty input values definitions `{}` are not valid in GraphQL.
///
/// Spec: [InputFieldsDefinition](https://spec.graphql.org/draft/#InputFieldsDefinition)
#[derive(Debug, Clone, Copy)]
pub struct InputFieldsDefinition<InputValueDefinition, Span, Container = Vec<InputValueDefinition>>
{
  span: Span,
  l_brace: LBrace<Span>,
  values: Container,
  r_brace: RBrace<Span>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Span, Container> AsRef<Span>
  for InputFieldsDefinition<InputValueDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<InputValueDefinition, Span, Container> IntoSpan<Span>
  for InputFieldsDefinition<InputValueDefinition, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValueDefinition, Span, Container> IntoComponents
  for InputFieldsDefinition<InputValueDefinition, Span, Container>
{
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.values, self.r_brace)
  }
}

impl<InputValueDefinition, Span, Container>
  InputFieldsDefinition<InputValueDefinition, Span, Container>
{
  /// Returns a reference to the span covering the entire input values definition.
  ///
  /// The span includes the opening brace, all input field definitions, and the closing brace.
  /// This is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all input value definitions.
  ///
  /// The input value definitions specify the individual values that can be
  /// provided in input objects, including their names, types, default values,
  /// descriptions, and directives. This allows iteration over, indexing into,
  /// or otherwise working with the collection of input value definitions.
  #[inline]
  pub const fn input_value_definitions(&self) -> &Container {
    &self.values
  }

  /// Consumes and returns the input value definitions
  pub fn into_input_value_definitions(self) -> Container {
    self.values
  }

  /// Returns a reference to the opening left brace (`{`) of the input values definition.
  ///
  /// This provides access to the exact location and span information of the
  /// opening delimiter, which can be useful for precise error reporting or
  /// syntax highlighting in development tools.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// Returns a reference to the closing right brace (`}`) of the input values definition.
  ///
  /// This provides access to the exact location and span information of the
  /// closing delimiter, which can be useful for precise error reporting or
  /// syntax highlighting in development tools.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Creates a parser that can parse an input values definition with custom input value definition parsing.
  ///
  /// This parser handles the complete input values definition syntax including the braces
  /// and ensures at least one input field definition is present. The parsing of individual
  /// input value definitions is delegated to the provided parser.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the input values definition.
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
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, InputValueDefinition, E> + Clone,
    Container: chumsky::container::Container<InputValueDefinition>,
  {
    LBrace::parser()
      .then(
        input_value_definition_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then(RBrace::parser())
      .map_with(|((l_brace, values), r_brace), sp| Self {
        span: Span::from_map_extra(sp),
        values,
        l_brace,
        r_brace,
        _input_value_definition: PhantomData,
      })
  }
}
