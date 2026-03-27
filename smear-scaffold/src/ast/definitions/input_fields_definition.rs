use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};

use core::marker::PhantomData;
use std::vec::Vec;


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
pub struct InputFieldsDefinition<InputValueDefinition, Container = Vec<InputValueDefinition>> {
  span: Span,
  values: Container,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Container> AsSpan<Span>
  for InputFieldsDefinition<InputValueDefinition, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<InputValueDefinition, Container> IntoSpan<Span>
  for InputFieldsDefinition<InputValueDefinition, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValueDefinition, Container> IntoComponents
  for InputFieldsDefinition<InputValueDefinition, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.values)
  }
}

impl<InputValueDefinition, Container> InputFieldsDefinition<InputValueDefinition, Container> {
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
}
