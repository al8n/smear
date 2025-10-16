use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::{Colon, LParen, RParen};

use std::vec::Vec;

/// Represents a variable definition in a GraphQL operation.
///
/// A variable definition specifies a parameter that can be provided to a GraphQL
/// operation (query, mutation, or subscription). It defines the variable's name,
/// type, optional default value, optional description, and optional directives.
/// Variables enable operations to be reusable and parameterized, supporting
/// dynamic GraphQL queries.
///
/// Variable definitions are essential for creating flexible, reusable GraphQL
/// operations that can accept different inputs without requiring query recompilation.
///
/// ## GraphQL Variables Philosophy
///
/// Variables support GraphQL's approach to parameterized operations:
/// - **Reusability**: Operations can be reused with different inputs
/// - **Type safety**: Variables are statically typed and validated
/// - **Security**: Prevents injection attacks through proper parameterization
/// - **Performance**: Enables operation caching and optimization
/// - **Client flexibility**: Separates operation logic from data values
/// - **Validation**: Built-in validation through type system and directives
///
/// # Examples
///
/// ```text
/// # Simple variable definition
/// $id: ID!
///
/// # Variable with description
/// """
/// The unique identifier for the user to retrieve
/// """
/// $userId: ID!
///
/// # Variable with default value
/// $first: Int = 10
///
/// # Variable with directives
/// $email: String! @constraint(format: "email")
///
/// # Complex variable with all components
/// """
/// Search parameters with advanced filtering.
/// Supports pagination, sorting, and field-specific filters.
/// """
/// $searchParams: SearchInput! = {
///   query: "*"
///   limit: 20
///   sortBy: CREATED_AT
///   sortOrder: DESC
/// } @validate(schema: "search") @rateLimit(max: 100)
///
/// # Variables in operation context
/// query GetUser(
///   """
///   The user's unique identifier
///   """
///   $id: ID!
///   
///   """
///   Whether to include the user's profile information
///   """
///   $includeProfile: Boolean = false
///   
///   """
///   Maximum number of posts to return
///   """
///   $postLimit: Int = 5 @constraint(min: 1, max: 50)
/// ) {
///   user(id: $id) {
///     id
///     name
///     email
///     profile @include(if: $includeProfile) {
///       bio
///       website
///     }
///     posts(first: $postLimit) {
///       title
///       content
///     }
///   }
/// }
///
/// # Mutation with complex input variables
/// mutation CreateUser(
///   """
///   Complete user data for account creation
///   """
///   $userData: CreateUserInput!
///   
///   """
///   Whether to send welcome email
///   """
///   $sendWelcomeEmail: Boolean = true
///   
///   """
///   Initial user preferences
///   """
///   $preferences: UserPreferencesInput = {
///     theme: LIGHT
///     notifications: true
///     language: "en"
///   }
/// ) {
///   createUser(
///     input: $userData
///     sendWelcomeEmail: $sendWelcomeEmail
///     preferences: $preferences
///   ) {
///     id
///     name
///     email
///   }
/// }
/// ```
///
/// # Variable Usage
///
/// ```text
/// # Client provides variables separately from operation:
/// {
///   "id": "user-123",
///   "includeProfile": true,
///   "postLimit": 10
/// }
///
/// # Server validates variables against definitions before execution
/// ```
///
/// ## Type Parameters
///
/// * `Type` - The type representing the variable's GraphQL type
/// * `Directives` - The type representing directives applied to the variable
/// * `Value` - The type representing the default value (must implement InputValue<true> for const values)
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// VariableDefinition : Description? Variable : Type Directives? DefaultValue?
/// ```
///
/// Spec: [Variable Definition](https://spec.graphql.org/draft/#sec-Variable-Definition)
#[derive(Debug, Clone, Copy)]
pub struct VariableDefinition<Variable, Type, DefaultValue, Directives> {
  span: Span,
  variable: Variable,
  ty: Type,
  directives: Option<Directives>,
  default_value: Option<DefaultValue>,
}

impl<Variable, Type, DefaultValue, Directives> AsSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Variable, Type, DefaultValue, Directives> IntoSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Variable, Type, DefaultValue, Directives> IntoComponents
  for VariableDefinition<Variable, Type, DefaultValue, Directives>
{
  type Components = (
    Span,
    Variable,
    Type,
    Option<Directives>,
    Option<DefaultValue>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.variable,
      self.ty,
      self.directives,
      self.default_value,
    )
  }
}

impl<Variable, Type, DefaultValue, Directives>
  VariableDefinition<Variable, Type, DefaultValue, Directives>
{
  /// Returns a reference to the span covering the entire variable definition.
  ///
  /// The span includes the optional description, variable (with $ prefix), colon,
  /// type, optional directives, and optional default value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the variable identifier.
  ///
  /// The variable includes the `$` prefix and the variable name. This is how
  /// the variable will be referenced within the operation body and how clients
  /// will provide values in the variables object.
  #[inline]
  pub const fn variable(&self) -> &Variable {
    &self.variable
  }

  /// Returns a reference to the variable's type.
  ///
  /// The type specifies what kind of data this variable accepts. It determines
  /// the validation rules that will be applied to variable values provided by
  /// clients. The type system ensures type safety for all variable inputs.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns a reference to the optional directives applied to this variable.
  ///
  /// Directives provide metadata or specify behavior for the variable,
  /// such as validation rules, constraints, rate limiting, or custom processing.
  /// Variable-level directives are particularly useful for input validation
  /// and security enforcement.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the optional default value.
  ///
  /// The default value is used when the variable is not provided by the client
  /// in the variables object. Default values must be constant (no variables)
  /// and compatible with the variable's type. They make variables effectively
  /// optional even when the type is non-null.
  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }
}

impl<'a, Variable, Type, DefaultValue, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for VariableDefinition<Variable, Type, DefaultValue, Directives>
where
  Variable: Parseable<'a, I, T, Error>,
  Type: Parseable<'a, I, T, Error>,
  DefaultValue: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error>,
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
    Variable::parser()
      .then_ignore(Colon::parser())
      .then(Type::parser())
      .then(DefaultValue::parser().or_not())
      .then(Directives::parser().or_not())
      .map_with(|(((variable, ty), default_value), directives), exa| Self {
        span: exa.span(),
        variable,
        ty,
        directives,
        default_value,
      })
  }
}

/// Represents a collection of variable definitions for a GraphQL operation.
///
/// A variables definition is a parenthesized collection of zero or more variable
/// definitions that specify what parameters an operation can accept. This structure
/// enables operations to be parameterized and reusable while maintaining type safety.
///
/// Variables definitions are used in operation definitions (query, mutation,
/// subscription) to specify the external parameters that can be provided by clients.
///
/// ## Examples
///
/// ```text
/// # Empty variables definition (no parameters)
/// ()
///
/// # Single variable definition
/// ($id: ID!)
///
/// # Multiple variable definitions
/// (
///   $id: ID!
///   $includeProfile: Boolean = false
///   $limit: Int = 10
/// )
///
/// # Complex variables definition with descriptions and directives
/// (
///   """
///   The unique identifier for the user
///   """
///   $userId: ID!
///   
///   """
///   Whether to include sensitive user data
///   """
///   $includeSensitive: Boolean = false @auth(requires: ADMIN)
///   
///   """
///   Pagination limit with validation
///   """
///   $limit: Int = 20 @constraint(min: 1, max: 100)
///   
///   """
///   Search and filter parameters
///   """
///   $filter: UserFilterInput @validate(schema: "user-filter")
/// )
///
/// # Usage in operations
/// query GetUsers(
///   $search: String
///   $limit: Int = 20
///   $offset: Int = 0
/// ) {
///   users(search: $search, limit: $limit, offset: $offset) {
///     id
///     name
///     email
///   }
/// }
///
/// mutation CreateUser(
///   $userData: CreateUserInput!
///   $sendNotification: Boolean = true
/// ) {
///   createUser(input: $userData, sendNotification: $sendNotification) {
///     id
///     name
///   }
/// }
/// ```
///
/// ## Type Parameters
///
/// * `VariableDefinition` - The type representing individual variable definitions
/// * `Span` - The type representing source location information
/// * `Container` - The container type for storing variable definitions (defaults to `Vec<VariableDefinition>`)
///
/// ## Grammar
///
/// ```text
/// VariablesDefinition : ( VariableDefinition* )
/// ```
///
/// Note: Zero or more variable definitions are allowed (the `*` indicates zero-or-more).
/// Empty variables definitions `()` are valid and represent operations with no parameters.
#[derive(Debug, Clone, Copy)]
pub struct VariablesDefinition<VariableDefinition, Container = Vec<VariableDefinition>> {
  span: Span,
  variables: Container,
  _v: PhantomData<VariableDefinition>,
}

impl<VariableDefinition, Container> AsSpan<Span>
  for VariablesDefinition<VariableDefinition, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<VariableDefinition, Container> IntoSpan<Span>
  for VariablesDefinition<VariableDefinition, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<VariableDefinition, Container> IntoComponents
  for VariablesDefinition<VariableDefinition, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.variables)
  }
}

impl<VariableDefinition, Container> VariablesDefinition<VariableDefinition, Container> {
  /// Returns a reference to the span covering the entire variables definition.
  ///
  /// The span includes the opening parenthesis, all variable definitions,
  /// and the closing parenthesis.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all variables definition.
  ///
  /// This allows iteration over, indexing into, or otherwise working with
  /// the collection of variable definitions.
  #[inline]
  pub const fn variable_definitions(&self) -> &Container {
    &self.variables
  }

  /// Consumes and returns the variable definitions.
  #[inline]
  pub fn into_variable_definitions(self) -> Container {
    self.variables
  }

  /// Creates a parser that can parse a variables definition with custom variable parsing.
  ///
  /// This parser handles the complete variables definition syntax including the
  /// parentheses and supports zero or more variable definitions. The parsing of
  /// individual variable definitions is delegated to the provided parser.
  pub fn parser_with<'src, I, T, Error, E, P>(
    variable_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    LParen: Parseable<'src, I, T, Error>,
    RParen: Parseable<'src, I, T, Error>,
    Container: chumsky::container::Container<VariableDefinition>,
    P: Parser<'src, I, VariableDefinition, E> + Clone,
  {
    LParen::parser()
      .ignore_then(variable_definition_parser.repeated().at_least(1).collect())
      .then_ignore(RParen::parser())
      .map_with(|variables, exa| Self {
        span: exa.span(),
        variables,
        _v: PhantomData,
      })
  }
}

impl<'a, VariableDefinition, Container, I, T, Error> Parseable<'a, I, T, Error>
  for VariablesDefinition<VariableDefinition, Container>
where
  Container: chumsky::container::Container<VariableDefinition>,
  VariableDefinition: Parseable<'a, I, T, Error>,
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
    Self::parser_with(VariableDefinition::parser())
  }
}
