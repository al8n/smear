use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_utils::{IntoComponents, IntoSpan};

/// Represents a named GraphQL operation definition with explicit operation type and optional metadata.
///
/// Named operations are the full form of GraphQL operations that include an operation type
/// (query, mutation, or subscription), optional name, variables, directives, and a selection set.
/// They provide complete control over operation execution and enable advanced features like
/// variables, directives, and operation identification.
///
/// ## Examples
///
/// ```text
/// # Simple named query
/// query GetUser {
///   user(id: "123") { name email }
/// }
///
/// # Complex operation with all components
/// """
/// Retrieves user profile with posts and analytics data.
/// Used by the dashboard component.
/// """
/// query GetUserDashboard($userId: ID!, $includeAnalytics: Boolean = false)
///   @cached(ttl: 300)
///   @rateLimit(max: 100)
/// {
///   user(id: $userId) {
///     name
///     email
///     posts(first: 10) {
///       title
///       createdAt
///     }
///     analytics @include(if: $includeAnalytics) {
///       viewCount
///       engagementRate
///     }
///   }
/// }
///
/// # Mutation with variables
/// mutation CreatePost($input: CreatePostInput!) {
///   createPost(input: $input) {
///     id
///     title
///     publishedAt
///   }
/// }
///
/// # Subscription for real-time updates
/// subscription ChatMessages($channelId: ID!) {
///   messageAdded(channelId: $channelId) {
///     id
///     content
///     author { name }
///     timestamp
///   }
/// }
/// ```
///
/// ## Grammar
///
/// ```text
/// OperationDefinition:
///   Description? OperationType Name? VariablesDefinition? Directives? SelectionSet
/// ```
///
/// Spec: [Operation Definition](https://spec.graphql.org/draft/#sec-Language.Operations)
#[derive(Debug, Clone, Copy)]
pub struct NamedOperationDefinition<
  Name,
  OperationType,
  VariablesDefinition,
  Directives,
  SelectionSet,
> {
  span: Span,
  operation_type: OperationType,
  name: Option<Name>,
  variable_definitions: Option<VariablesDefinition>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet> AsRef<Span>
  for NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet> IntoSpan<Span>
  for NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet> IntoComponents
  for NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
{
  type Components = (
    Span,
    OperationType,
    Option<Name>,
    Option<VariablesDefinition>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.operation_type,
      self.name,
      self.variable_definitions,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
  NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
{
  /// Returns a reference to the span covering the entire operation definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the operation type (query, mutation, or subscription).
  ///
  /// The operation type determines the execution semantics:
  /// - **Query**: Read-only, can be executed in parallel
  /// - **Mutation**: Write operations, executed serially
  /// - **Subscription**: Real-time streaming operations
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns a reference to the optional operation name.
  ///
  /// Operation names serve multiple purposes:
  /// - Client-side operation identification and caching
  /// - Server-side logging and analytics
  /// - Development tools and debugging
  /// - Operation whitelisting and security
  ///
  /// Names should be descriptive and follow naming conventions like `GetUserProfile`
  /// or `CreateBlogPost`.
  #[inline]
  pub const fn name(&self) -> Option<&Name> {
    self.name.as_ref()
  }

  /// Returns a reference to the optional variable definitions.
  ///
  /// Variable definitions specify the parameters that can be passed to the operation,
  /// enabling parameterized and reusable operations. They include type information
  /// and optional default values.
  #[inline]
  pub const fn variable_definitions(&self) -> Option<&VariablesDefinition> {
    self.variable_definitions.as_ref()
  }

  /// Returns a reference to the optional directives applied to the operation.
  ///
  /// Operation-level directives can control caching, authentication, rate limiting,
  /// and other cross-cutting concerns. Common examples include `@cached`, `@auth`,
  /// and `@rateLimit`.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the selection set defining what data to fetch.
  ///
  /// The selection set is the core of the operation, specifying exactly which
  /// fields and nested data the client wants to retrieve. This is where GraphQL's
  /// "ask for what you need" philosophy is expressed.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }

  /// Creates a parser for named operation definitions.
  ///
  /// This parser handles the complete syntax for named operations, including all
  /// optional components. It delegates parsing of specific components to the
  /// provided sub-parsers for maximum flexibility.
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, NP, OP, VP, DP, SP>(
    name_parser: NP,
    operation_type_parser: OP,
    variable_definitions_parser: VP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    OP: Parser<'src, I, OperationType, E> + Clone,
    VP: Parser<'src, I, VariablesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    operation_type_parser
      .then(name_parser.or_not())
      .then(variable_definitions_parser.or_not())
      .then(directives_parser.or_not())
      .then(selection_set_parser)
      .map_with(
        |((((operation_type, name), variable_definitions), directives), selection_set), exa| Self {
          span: exa.span(),
          operation_type,
          name,
          variable_definitions,
          directives,
          selection_set,
        },
      )
  }
}

impl<'a, Name, OperationType, VariablesDefinition, Directives, SelectionSet, I, T, Error>
  Parseable<'a, I, T, Error>
  for NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
where
  Name: Parseable<'a, I, T, Error>,
  OperationType: Parseable<'a, I, T, Error>,
  VariablesDefinition: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  SelectionSet: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(
      Name::parser(),
      OperationType::parser(),
      VariablesDefinition::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
  }
}

/// Represents a complete GraphQL operation definition.
///
/// Operations are the entry points for GraphQL execution. They can be either
/// named operations with explicit types and metadata, or shorthand query operations.
/// Operations define what data to fetch (queries), what data to modify (mutations),
/// or what real-time updates to subscribe to (subscriptions).
///
/// GraphQL supports two operation definition syntaxes:
/// 1. **Named operations**: Full syntax with operation type, optional name, variables, and directives
/// 2. **Shorthand queries**: Simplified syntax for query operations without variables or directives
///
/// ## Operation Types and Semantics
///
/// - **Query operations**: Read-only data retrieval, can be executed in parallel
/// - **Mutation operations**: Write operations with side effects, executed serially  
/// - **Subscription operations**: Real-time streaming data over persistent connections
///
/// ## Examples
///
/// ```text
/// # Named query operation
/// query GetUserProfile($userId: ID!, $includeDetails: Boolean = false) @cached {
///   user(id: $userId) {
///     id
///     name
///     email
///     profile @include(if: $includeDetails) {
///       bio
///       website
///     }
///   }
/// }
///
/// # Named mutation operation
/// mutation UpdateUserProfile($input: UpdateProfileInput!) @auth(required: true) {
///   updateProfile(input: $input) {
///     success
///     user {
///       id
///       name
///       profile { bio }
///     }
///   }
/// }
///
/// # Named subscription operation
/// subscription MessageNotifications($channelId: ID!) {
///   messageAdded(channelId: $channelId) {
///     id
///     content
///     author { name }
///     timestamp
///   }
/// }
///
/// # Shorthand query operation (no variables, no directives)
/// {
///   user(id: "123") {
///     name
///     email
///   }
/// }
/// ```
///
/// ## Grammar
///
/// ```text
/// OperationDefinition:
///   OperationType Name? VariablesDefinition? Directives? SelectionSet
///   | SelectionSet
///
/// OperationType: one of
///   query mutation subscription
/// ```
///
/// Spec: [OperationDefinition](https://spec.graphql.org/draft/#OperationDefinition)
#[derive(Debug, Clone, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet> {
  /// Named operation with full metadata
  ///
  /// ## Examples
  /// ```text
  /// # Query with variables and directives
  /// query GetUserPosts($userId: ID!, $first: Int = 10) @cached(ttl: 300) {
  ///   user(id: $userId) {
  ///     name
  ///     posts(first: $first) {
  ///       title
  ///       publishedAt
  ///     }
  ///   }
  /// }
  ///
  /// # Mutation operation
  /// mutation CreatePost($input: CreatePostInput!) {
  ///   createPost(input: $input) {
  ///     id
  ///     title
  ///     author { name }
  ///   }
  /// }
  ///
  /// # Subscription operation
  /// subscription LiveUpdates {
  ///   messageAdded {
  ///     id
  ///     content
  ///     timestamp
  ///   }
  /// }
  /// ```
  Named(
    NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>,
  ),
  /// Shorthand query operation (selection set only)
  ///
  /// ## Examples
  /// ```text
  /// # Simple data retrieval
  /// {
  ///   user(id: "123") {
  ///     name
  ///     email
  ///   }
  /// }
  ///
  /// # Nested field selection
  /// {
  ///   posts(first: 5) {
  ///     title
  ///     author {
  ///       name
  ///       avatar
  ///     }
  ///     comments(first: 3) {
  ///       content
  ///       author { name }
  ///     }
  ///   }
  /// }
  ///
  /// # Multiple top-level fields
  /// {
  ///   currentUser { name email }
  ///   notifications { count unreadCount }
  ///   settings { theme language }
  /// }
  /// ```
  ///
  /// ## Equivalent Named Form
  /// The shorthand operation `{ user { name } }` is equivalent to:
  /// ```text
  /// query {
  ///   user {
  ///     name
  ///   }
  /// }
  /// ```
  Shorthand(SelectionSet),
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet> AsRef<Span>
  for OperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
where
  SelectionSet: AsRef<Span>,
{
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Named(named) => named.as_ref(),
      Self::Shorthand(selection_set) => selection_set.as_ref(),
    }
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet> IntoSpan<Span>
  for OperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
where
  SelectionSet: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Named(named) => named.into_span(),
      Self::Shorthand(selection_set) => selection_set.into_span(),
    }
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
  OperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
{
  /// Creates a parser for operation definitions.
  ///
  /// Handles both named and shorthand operation forms with proper precedence.
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, NP, OP, VP, DP, SP>(
    name_parser: NP,
    operation_type_parser: OP,
    variable_definitions_parser: VP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    OP: Parser<'src, I, OperationType, E> + Clone,
    VP: Parser<'src, I, VariablesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    choice((
      NamedOperationDefinition::parser_with(
        name_parser,
        operation_type_parser,
        variable_definitions_parser,
        directives_parser,
        selection_set_parser.clone(),
      )
      .map(Self::Named),
      selection_set_parser.map(Self::Shorthand),
    ))
  }
}

impl<'a, Name, OperationType, VariablesDefinition, Directives, SelectionSet, I, T, Error>
  Parseable<'a, I, T, Error>
  for OperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet>
where
  Name: Parseable<'a, I, T, Error>,
  OperationType: Parseable<'a, I, T, Error>,
  VariablesDefinition: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  SelectionSet: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(
      Name::parser(),
      OperationType::parser(),
      VariablesDefinition::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
  }
}
