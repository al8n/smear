use core::marker::PhantomData;

use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{
    ignored,
    punct::{Colon, LParen, RParen},
    Const, StringValue, Variable,
  },
  source::{Char, Slice, Source},
};

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
pub struct VariableDefinition<Type, Directives, DefaultValue, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  variable: Variable<Span>,
  colon: Colon<Span>,
  ty: Type,
  directives: Option<Directives>,
  default_value: Option<DefaultValue>,
}

impl<Type, Directives, DefaultValue, Span> AsRef<Span>
  for VariableDefinition<Type, Directives, DefaultValue, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Type, Directives, DefaultValue, Span> IntoSpan<Span>
  for VariableDefinition<Type, Directives, DefaultValue, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, Directives, DefaultValue, Span> IntoComponents
  for VariableDefinition<Type, Directives, DefaultValue, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    Variable<Span>,
    Colon<Span>,
    Type,
    Option<Directives>,
    Option<DefaultValue>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.variable,
      self.colon,
      self.ty,
      self.directives,
      self.default_value,
    )
  }
}

impl<Type, Directives, DefaultValue, Span>
  VariableDefinition<Type, Directives, DefaultValue, Span>
{
  /// Returns a reference to the span covering the entire variable definition.
  ///
  /// The span includes the optional description, variable (with $ prefix), colon,
  /// type, optional directives, and optional default value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the variable definition.
  ///
  /// The description provides documentation for the variable's purpose and usage.
  /// It appears before the variable name and helps developers understand what
  /// data should be provided for this parameter.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the variable identifier.
  ///
  /// The variable includes the `$` prefix and the variable name. This is how
  /// the variable will be referenced within the operation body and how clients
  /// will provide values in the variables object.
  #[inline]
  pub const fn variable(&self) -> &Variable<Span> {
    &self.variable
  }

  /// Returns a reference to the colon separator between variable and type.
  ///
  /// The colon is a required part of variable definition syntax that separates
  /// the variable identifier from its type specification.
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
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

  /// Creates a parser that can parse a complete variable definition.
  ///
  /// This parser handles the full variable definition syntax including all
  /// optional and required components. The parsing of type, directives, and
  /// default value is delegated to the provided parser functions, allowing
  /// for context-specific validation and processing.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the variable definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E, TP, DP, VP>(
    type_parser: TP,
    directives_parser: DP,
    default_value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    VP: Parser<'src, I, DefaultValue, E> + Clone,
    Directives: Const<false>,
    DefaultValue: Const<true>,
  {
    StringValue::parser()
      .or_not()
      .then(Variable::parser().padded_by(ignored()))
      .then(Colon::parser())
      .then(ignored().ignore_then(type_parser))
      .then(ignored().ignore_then(directives_parser).or_not())
      .then(ignored().ignore_then(default_value_parser).or_not())
      .map_with(
        |(((((description, variable), colon), ty), directives), default_value), sp| Self {
          span: Span::from_map_extra(sp),
          variable,
          description,
          colon,
          ty,
          directives,
          default_value,
        },
      )
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
pub struct VariablesDefinition<VariableDefinition, Span, Container = Vec<VariableDefinition>> {
  span: Span,
  l_paren: LParen<Span>,
  r_paren: RParen<Span>,
  variables: Container,
  _v: PhantomData<VariableDefinition>,
}

impl<VariableDefinition, Span, Container> AsRef<Span>
  for VariablesDefinition<VariableDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<VariableDefinition, Span, Container> IntoSpan<Span>
  for VariablesDefinition<VariableDefinition, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<VariableDefinition, Span, Container> IntoComponents
  for VariablesDefinition<VariableDefinition, Span, Container>
{
  type Components = (Span, LParen<Span>, Container, RParen<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.variables, self.r_paren)
  }
}

impl<VariableDefinition, Span, Container> VariablesDefinition<VariableDefinition, Span, Container> {
  /// Returns a reference to the span covering the entire variables definition.
  ///
  /// The span includes the opening parenthesis, all variable definitions,
  /// and the closing parenthesis.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the opening left parenthesis (`(`) of the variables definition.
  ///
  /// This provides access to the exact location and span information of the
  /// opening delimiter.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    &self.l_paren
  }

  /// Returns a reference to the closing right parenthesis (`)`) of the variables definition.
  ///
  /// This provides access to the exact location and span information of the
  /// closing delimiter.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    &self.r_paren
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
  pub fn parser_with<'src, I, E, P>(
    variable_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    Container: chumsky::container::Container<VariableDefinition>,
    P: Parser<'src, I, VariableDefinition, E> + Clone,
  {
    LParen::parser()
      .then(
        variable_definition_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then(RParen::parser())
      .map_with(|((l_paren, variables), r_paren), sp| Self {
        span: Span::from_map_extra(sp),
        l_paren,
        r_paren,
        variables,
        _v: PhantomData,
      })
  }
}
