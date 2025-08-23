use chumsky::{extra::ParserExtra, prelude::*};

use core::marker::PhantomData;
use std::vec::Vec;

use super::super::{
  lang::{
    ignored,
    punct::{LBrace, RBrace},
  },
  source::{Char, Slice, Source},
};

/// Represents a collection of input field definitions in a GraphQL input object type.
///
/// An input fields definition is a braced collection of one or more input value definitions
/// that specify what fields are available on an input object type. Input object types are
/// used to represent complex input data in GraphQL operations, particularly for mutation
/// arguments and complex query parameters.
///
/// Input fields definitions define the structure and validation rules for data that
/// clients must provide when using the input type.
///
/// ## GraphQL Input Types Context
///
/// Input object types serve several important purposes in GraphQL:
/// - **Mutation arguments**: Structuring complex data for create/update operations
/// - **Query parameters**: Organizing filter, search, and pagination parameters
/// - **Nested inputs**: Creating hierarchical input data structures
/// - **Validation**: Defining required fields, types, and default values
/// - **Documentation**: Providing clear interfaces for client developers
///
/// ## Examples
///
/// ```text
/// # Simple input fields definition
/// {
///   name: String!
///   email: String!
///   age: Int
/// }
///
/// # Input fields with descriptions and default values
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
/// # Input fields with directives
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
/// Empty input fields definitions `{}` are not valid in GraphQL.
///
/// Spec: [InputFieldsDefinition](https://spec.graphql.org/draft/#InputFieldsDefinition)
#[derive(Debug, Clone)]
pub struct InputFieldsDefinition<InputValueDefinition, Span, Container = Vec<InputValueDefinition>>
{
  span: Span,
  l_brace: LBrace<Span>,
  fields: Container,
  r_brace: RBrace<Span>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Span, Container>
  InputFieldsDefinition<InputValueDefinition, Span, Container>
{
  /// Returns a reference to the span covering the entire input fields definition.
  ///
  /// The span includes the opening brace, all input field definitions, and the closing brace.
  /// This is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all input field definitions.
  ///
  /// The input field definitions specify the individual fields that can be
  /// provided in input objects, including their names, types, default values,
  /// descriptions, and directives. This allows iteration over, indexing into,
  /// or otherwise working with the collection of input field definitions.
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }

  /// Returns a reference to the opening left brace (`{`) of the input fields definition.
  ///
  /// This provides access to the exact location and span information of the
  /// opening delimiter, which can be useful for precise error reporting or
  /// syntax highlighting in development tools.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// Returns a reference to the closing right brace (`}`) of the input fields definition.
  ///
  /// This provides access to the exact location and span information of the
  /// closing delimiter, which can be useful for precise error reporting or
  /// syntax highlighting in development tools.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Creates a parser that can parse an input fields definition with custom input value definition parsing.
  ///
  /// This parser handles the complete input fields definition syntax including the braces
  /// and ensures at least one input field definition is present. The parsing of individual
  /// input value definitions is delegated to the provided parser.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the input fields definition.
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
      // allow Ignored right after '{' (e.g., newlines/commas)
      .then_ignore(ignored())
      // one-or-more items, collected into `Container`
      .then(input_value_definition_parser.padded_by(ignored()).repeated().at_least(1).collect())
      // optional Ignored before '}'
      .then_ignore(ignored())
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
