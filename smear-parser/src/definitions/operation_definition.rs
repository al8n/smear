use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{ignored, Name, StringValue},
  source::{Char, Slice, Source},
};

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
  OperationType,
  VariablesDefinition,
  Directives,
  SelectionSet,
  Span,
> {
  span: Span,
  description: Option<StringValue<Span>>,
  operation_type: OperationType,
  name: Option<Name<Span>>,
  variable_definitions: Option<VariablesDefinition>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<OperationType, VariablesDefinition, Directives, SelectionSet, Span> AsRef<Span>
  for NamedOperationDefinition<OperationType, VariablesDefinition, Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<OperationType, VariablesDefinition, Directives, SelectionSet, Span> IntoSpan<Span>
  for NamedOperationDefinition<OperationType, VariablesDefinition, Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<OperationType, VariablesDefinition, Directives, SelectionSet, Span> IntoComponents
  for NamedOperationDefinition<OperationType, VariablesDefinition, Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    OperationType,
    Option<Name<Span>>,
    Option<VariablesDefinition>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.operation_type,
      self.name,
      self.variable_definitions,
      self.directives,
      self.selection_set,
    )
  }
}

impl<OperationType, VariablesDefinition, Directives, SelectionSet, Span>
  NamedOperationDefinition<OperationType, VariablesDefinition, Directives, SelectionSet, Span>
{
  /// Returns a reference to the span covering the entire operation definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the operation.
  ///
  /// The description documents the operation's purpose, usage, and any important
  /// implementation details. It's particularly useful for complex operations
  /// that may be used by multiple clients or team members.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
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
  pub const fn name(&self) -> Option<&Name<Span>> {
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
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the operation definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, E, OP, VP, DP, SP>(
    operation_type_parser: OP,
    variable_definitions_parser: VP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    OP: Parser<'src, I, OperationType, E> + Clone,
    VP: Parser<'src, I, VariablesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then(ignored().ignore_then(operation_type_parser))
      .then(Name::parser().or_not())
      .then(ignored().ignore_then(variable_definitions_parser).or_not())
      .then(ignored().ignore_then(directives_parser).or_not())
      .then(ignored().ignore_then(selection_set_parser))
      .map_with(
        |(
          ((((description, operation_type), name), variable_definitions), directives),
          selection_set,
        ),
         sp| {
          Self {
            span: Span::from_map_extra(sp),
            description,
            operation_type,
            name,
            variable_definitions,
            directives,
            selection_set,
          }
        },
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
#[derive(
  Debug,
  Clone,
  derive_more::IsVariant,
  derive_more::From,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationDefinition<OperationType, VariablesDefinition, Directives, SelectionSet, Span> {
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
    NamedOperationDefinition<OperationType, VariablesDefinition, Directives, SelectionSet, Span>,
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

impl<OperationType, VariablesDefinition, Directives, SelectionSet, Span>
  OperationDefinition<OperationType, VariablesDefinition, Directives, SelectionSet, Span>
{
  /// Creates a parser for operation definitions.
  ///
  /// Handles both named and shorthand operation forms with proper precedence.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the input values definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, E, OP, VP, DP, SP>(
    operation_type_parser: OP,
    variable_definitions_parser: VP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    OP: Parser<'src, I, OperationType, E> + Clone,
    VP: Parser<'src, I, VariablesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    choice((
      NamedOperationDefinition::parser_with(
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
