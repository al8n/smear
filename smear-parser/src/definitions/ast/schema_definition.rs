use logosky::{
  Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{
  error::{SchemaExtensionHint, UnexpectedEndOfSchemaExtensionError},
  lang::keywords::{Extend, Schema},
};

/// Represents a GraphQL schema definition that describes the structure and capabilities of a GraphQL service.
///
/// A schema definition is the root of a GraphQL type system and describes which types are available
/// at the root of each kind of operation. It serves as the entry point for GraphQL execution and
/// defines the complete API contract for a GraphQL service.
///
/// ## Examples
///
/// ```text
/// # Minimal schema with only query operations
/// schema {
///   query: Query
/// }
///
/// # Complete schema with all operation types
/// schema {
///   query: Query
///   mutation: Mutation
///   subscription: Subscription
/// }
///
/// # Schema with description
/// """
/// A comprehensive blog API that supports reading posts,
/// managing user accounts, and real-time notifications.
///
/// This schema follows RESTful principles adapted for GraphQL,
/// providing efficient data fetching and real-time capabilities.
/// """
/// schema {
///   query: Query
///   mutation: Mutation
///   subscription: Subscription
/// }
///
/// # Schema with directives
/// schema @auth(required: true) @rateLimit(max: 1000) {
///   query: Query
///   mutation: Mutation
/// }
///
/// # Schema with comprehensive metadata
/// """
/// E-commerce Platform API v2.0
///
/// This schema provides access to product catalog, user management,
/// order processing, and real-time inventory updates. It supports
/// multi-tenant operations with role-based access control.
///
/// Rate limits:
/// - Queries: 1000 requests/hour
/// - Mutations: 100 requests/hour
/// - Subscriptions: 10 concurrent connections
///
/// Authentication required for all operations except public product browsing.
/// """
/// schema
///   @auth(required: true)
///   @rateLimit(queries: 1000, mutations: 100)
///   @version(current: "2.0", deprecated: ["1.0", "1.1"])
///   @contact(team: "api-team@company.com")
/// {
///   query: Query
///   mutation: Mutation  
///   subscription: Subscription
/// }
///
/// # Specialized schema for specific use cases
/// """
/// Analytics and Reporting API
///
/// Optimized for read-heavy analytical workloads with complex
/// aggregations and time-series data. No mutations supported
/// as this is a read-only analytical interface.
/// """
/// schema @caching(maxAge: 300) @analytics(enabled: true) {
///   query: AnalyticsQuery
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the schema
/// * `RootOperationTypesDefinition` - The type representing the root operation type definitions
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// SchemaDefinition : Description? schema Directives? { RootOperationTypeDefinition+ }
/// ```
///
/// Spec: [Schema Definition](https://spec.graphql.org/draft/#sec-Schema)
#[derive(Debug, Clone, Copy)]
pub struct SchemaDefinition<Directives, RootOperationTypesDefinition> {
  span: Span,
  directives: Option<Directives>,
  operation_type_definitions: RootOperationTypesDefinition,
}

impl<Directives, RootOperationTypesDefinition> AsSpan<Span>
  for SchemaDefinition<Directives, RootOperationTypesDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Directives, RootOperationTypesDefinition> IntoSpan<Span>
  for SchemaDefinition<Directives, RootOperationTypesDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, RootOperationTypesDefinition> IntoComponents
  for SchemaDefinition<Directives, RootOperationTypesDefinition>
{
  type Components = (Span, Option<Directives>, RootOperationTypesDefinition);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.directives, self.operation_type_definitions)
  }
}

impl<Directives, RootOperationTypesDefinition>
  SchemaDefinition<Directives, RootOperationTypesDefinition>
{
  /// Returns a reference to the span covering the entire schema definition.
  ///
  /// The span includes the optional description, schema keyword, optional directives,
  /// and root operation type definitions. This is useful for error reporting,
  /// source mapping, and tooling integration.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional directives applied to this schema definition.
  ///
  /// Schema-level directives provide metadata and modify the behavior of the entire
  /// schema. They are evaluated once during schema construction and can affect
  /// global concerns such as authentication, caching, rate limiting, and access control.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the root operation type definitions for this schema.
  ///
  /// These definitions specify which Object types serve as the entry points for each
  /// kind of GraphQL operation. They form the foundation of the GraphQL execution
  /// model and define how clients can interact with the service.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> &RootOperationTypesDefinition {
    &self.operation_type_definitions
  }

  /// Creates a parser for schema definitions using the provided sub-parsers.
  ///
  /// This parser handles the complete syntax for a GraphQL schema definition,
  /// including all optional and required components. The parsing of directives
  /// and root operation type definitions is delegated to the provided parsers,
  /// allowing for context-specific validation and processing.
  pub fn parser_with<'src, I, T, Error, E, DP, RP>(
    directives_parser: DP,
    root_operation_types_definition_parser: RP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Schema: Parseable<'src, I, T, Error> + 'src,
    DP: Parser<'src, I, Directives, E> + Clone,
    RP: Parser<'src, I, RootOperationTypesDefinition, E> + Clone,
  {
    Schema::parser()
      .ignore_then(directives_parser.or_not())
      .then(root_operation_types_definition_parser)
      .map_with(|(directives, operation_type_definitions), exa| Self {
        span: exa.span(),
        directives,
        operation_type_definitions,
      })
  }
}

impl<'a, Directives, RootOperationTypesDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for SchemaDefinition<Directives, RootOperationTypesDefinition>
where
  Directives: Parseable<'a, I, T, Error>,
  RootOperationTypesDefinition: Parseable<'a, I, T, Error>,
  Schema: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Directives::parser(), RootOperationTypesDefinition::parser())
  }
}

/// The content portion of a schema extension, which can contain either directives or operations.
///
/// Schema extensions provide a mechanism to extend existing schemas with additional capabilities
/// without modifying the original schema definition. This is particularly useful for:
/// - Modular schema composition and federation
/// - Adding vendor-specific functionality to standard schemas  
/// - Incremental schema evolution and feature rollout
/// - Third-party plugin and extension systems
/// - Environment-specific schema customizations
///
/// Extensions enable GraphQL's approach to schema evolution:
/// - **Non-Breaking Changes**: Extensions can only add, never remove or modify existing definitions
/// - **Composability**: Multiple extensions can be applied to create composite schemas
/// - **Modularity**: Different teams can contribute extensions independently
/// - **Backwards Compatibility**: Base schemas remain functional without extensions
/// - **Flexible Deployment**: Extensions can be conditionally applied based on environment or features
///
/// ## Extension Types
///
/// Schema extensions can add two types of content:
/// 1. **Directive-only extensions**: Add metadata or behavior modifiers to the schema
/// 2. **Operational extensions**: Add new root operation types (with optional directives)
///
/// ## Examples
///
/// ```text
/// # Adding authentication directive to existing schema
/// extend schema @auth(provider: "oauth2", scopes: ["read", "write"])
///
/// # Adding rate limiting with multiple directives  
/// extend schema
///   @rateLimit(queries: 1000, mutations: 100)
///   @monitoring(enabled: true)
///   @caching(defaultTTL: 300)
///
/// # Adding subscription capability to query-only schema
/// extend schema {
///   subscription: Subscription
/// }
///
/// # Adding operations with directives
/// extend schema @federation(version: "2.0") {
///   subscription: RealtimeSubscription
/// }
///
/// # Complex extension with comprehensive metadata
/// extend schema
///   @version(extension: "analytics-v1.2")
///   @feature(name: "advanced-analytics", enabled: true)
///   @security(level: "enhanced", audit: true)
/// {
///   subscription: AnalyticsSubscription
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the schema extension
/// * `RootOperationTypesDefinition` - The type representing new root operation type definitions
#[derive(Debug, Clone, Copy, derive_more::IsVariant)]
pub enum SchemaExtensionData<Directives, RootOperationTypesDefinition> {
  /// Extension contains only directives applied to the schema.
  ///
  /// This variant represents extensions that add metadata, behavioral modifiers,
  /// or configuration to the schema without changing its operational structure.
  /// Directive-only extensions are commonly used for:
  ///
  /// - **Security Configuration**: Adding authentication, authorization, or CORS policies
  /// - **Performance Tuning**: Configuring caching, rate limiting, or optimization hints  
  /// - **Monitoring Setup**: Enabling tracing, metrics collection, or audit logging
  /// - **Feature Flags**: Conditional functionality based on environment or client
  /// - **Federation Config**: Schema composition and service mesh configuration
  /// - **Deprecation Notice**: Marking schemas for migration or retirement
  ///
  /// ## Example
  /// ```text
  /// extend schema
  ///   @auth(required: true, provider: "keycloak")
  ///   @rateLimit(max: 5000, window: "1h", burst: 100)
  ///   @deprecated(reason: "Migrate to v3 API by Q2 2025")
  /// ```
  Directives(Directives),

  /// Extension contains operation type definitions, optionally with directives.
  ///
  /// This variant represents extensions that add new root operation types to
  /// the schema, expanding its operational capabilities. It can simultaneously
  /// add directives and operation definitions, making it the most comprehensive
  /// form of schema extension.
  ///
  /// Operational extensions are commonly used for:
  /// - **Adding Subscriptions**: Extending query-only schemas with real-time capabilities
  /// - **Adding Mutations**: Extending read-only schemas with write operations  
  /// - **Feature Rollout**: Gradually introducing new operation types
  /// - **Service Composition**: Combining schemas from different services
  /// - **Environment Adaptation**: Different operation sets for different environments
  ///
  /// ## Structure
  /// - `directives`: Optional schema-level directives that apply globally
  /// - `definitions`: The new root operation type definitions being added
  ///
  /// ## Examples
  /// ```text
  /// # Adding subscription capability
  /// extend schema {
  ///   subscription: Subscription
  /// }
  ///
  /// # Adding operations with security requirements
  /// extend schema @auth(scopes: ["admin"]) {
  ///   mutation: AdminMutation
  /// }
  ///
  /// # Comprehensive extension with multiple concerns
  /// extend schema
  ///   @federation(serviceName: "payments")
  ///   @rateLimit(mutations: 50, subscriptions: 10)
  ///   @monitoring(service: "payment-service")
  /// {
  ///   mutation: PaymentMutation
  ///   subscription: PaymentSubscription
  /// }
  /// ```
  Operations {
    /// Optional directives applied to the schema extension.
    ///
    /// These directives provide metadata or modify the behavior of the schema
    /// in conjunction with the new operation definitions. They apply globally
    /// to the extended schema.
    directives: Option<Directives>,

    /// The new root operation type definitions being added to the schema.
    ///
    /// These definitions specify which Object types will serve as entry points
    /// for the new operations. The definitions must not conflict with existing
    /// root operation types in the base schema.
    definitions: RootOperationTypesDefinition,
  },
}

impl<Directives, RootOperationTypesDefinition>
  SchemaExtensionData<Directives, RootOperationTypesDefinition>
{
  /// Returns directives if present, regardless of the variant.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Operations { directives, .. } => directives.as_ref(),
    }
  }

  /// Returns operation definitions if present, otherwise `None`.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> Option<&RootOperationTypesDefinition> {
    match self {
      Self::Directives(_) => None,
      Self::Operations { definitions, .. } => Some(definitions),
    }
  }
}

/// A GraphQL schema extension that adds new capabilities to an existing schema.
///
/// Schema extensions are a powerful mechanism for evolving GraphQL schemas over time
/// without breaking existing clients or requiring complete schema rewrites. They embody
/// GraphQL's commitment to backwards compatibility and evolutionary API design.
///
/// Extensions enable sophisticated schema composition patterns:
/// - **Incremental Development**: Add features without disrupting existing functionality
/// - **Team Autonomy**: Different teams can extend schemas independently
/// - **Environment Customization**: Apply different extensions in different deployments
/// - **Feature Flagging**: Conditionally enable functionality through extensions
/// - **Service Composition**: Combine schemas from multiple services or domains
/// - **Third-Party Integration**: Allow external plugins to extend core schemas
///
/// ## Schema Extension Philosophy
///
/// Extensions reflect GraphQL's core design principles:
/// - **Additive Only**: Extensions can only add, never remove or modify existing definitions
/// - **Composable**: Multiple extensions can be safely combined
/// - **Backwards Compatible**: Base schemas continue working without extensions
/// - **Type Safe**: Extensions must maintain overall schema validity
/// - **Self-Documenting**: Extensions can include their own documentation
/// - **Tooling Friendly**: Extensions can be analyzed and validated by tools
///
/// ## Extension Lifecycle
///
/// 1. **Definition**: Extensions are defined separately from base schemas
/// 2. **Validation**: Extensions are validated for compatibility with base schema
/// 3. **Application**: Extensions are applied to create composite schemas
/// 4. **Execution**: The extended schema is used for query execution
/// 5. **Evolution**: Additional extensions can be applied over time
///
/// ## Use Cases
///
/// ### Modular API Development
/// ```text
/// # Base e-commerce schema
/// schema {
///   query: Query
///   mutation: Mutation
/// }
///
/// # Analytics extension
/// extend schema @analytics(enabled: true) {
///   subscription: AnalyticsSubscription
/// }
///
/// # Admin extension
/// extend schema @auth(roles: ["admin"]) {
///   mutation: AdminMutation
/// }
/// ```
///
/// ### Federation and Service Mesh
/// ```text
/// # User service extension
/// extend schema @federation(service: "users") {
///   subscription: UserSubscription
/// }
///
/// # Product service extension  
/// extend schema @federation(service: "products") {
///   subscription: ProductSubscription
/// }
/// ```
///
/// ### Feature Rollout
/// ```text
/// # Beta features extension
/// extend schema
///   @feature(name: "advanced-search", stage: "beta")
///   @rateLimit(queries: 100) # Reduced limits for beta
/// {
///   query: BetaQuery
/// }
/// ```
///
/// ## Examples
///
/// ```text
/// # Simple directive extension
/// extend schema @deprecated(reason: "Use schema v2")
///
/// # Adding real-time capabilities
/// extend schema {
///   subscription: Subscription
/// }
///
/// # Complex federated extension
/// extend schema
///   @federation(serviceName: "notifications", version: "1.2")
///   @auth(scopes: ["notifications:read", "notifications:write"])
///   @rateLimit(subscriptions: 50, mutations: 200)
///   @monitoring(traces: true, metrics: ["latency", "throughput"])
/// {
///   mutation: NotificationMutation
///   subscription: NotificationSubscription
/// }
///
/// # Environment-specific extension
/// extend schema
///   @environment(name: "production")
///   @security(level: "high", audit: true)
///   @performance(caching: true, compression: true)
/// {
///   query: ProductionQuery
/// }
/// ```
///
/// ## Validation Requirements
///
/// Schema extensions must satisfy several validation rules:
/// - Cannot redefine existing root operation types (this would be a conflict)
/// - Can only reference types that exist in the base schema or other applied extensions
/// - Applied directives must be valid for their locations
/// - Must maintain overall schema coherence and validity
/// - Cannot introduce circular dependencies
/// - Must respect any constraints defined by the base schema
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the schema extension
/// * `RootOperationTypesDefinition` - The type representing new root operation type definitions
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// SchemaExtension:
///   extend schema Directives? { RootOperationTypeDefinition+ }
///   | extend schema Directives
/// ```
///
/// Spec: [Schema Extension](https://spec.graphql.org/draft/#sec-Schema-Extension)
#[derive(Debug, Clone, Copy)]
pub struct SchemaExtension<Directives, RootOperationTypesDefinition> {
  span: Span,
  content: SchemaExtensionData<Directives, RootOperationTypesDefinition>,
}

impl<Directives, RootOperationTypesDefinition> AsSpan<Span>
  for SchemaExtension<Directives, RootOperationTypesDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Directives, RootOperationTypesDefinition> IntoSpan<Span>
  for SchemaExtension<Directives, RootOperationTypesDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, RootOperationTypesDefinition> IntoComponents
  for SchemaExtension<Directives, RootOperationTypesDefinition>
{
  type Components = (
    Span,
    SchemaExtensionData<Directives, RootOperationTypesDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.content)
  }
}

impl<Directives, RootOperationTypesDefinition>
  SchemaExtension<Directives, RootOperationTypesDefinition>
{
  /// Returns a reference to the span covering the entire schema extension.
  ///
  /// The span encompasses the complete extension from the `extend` keyword through
  /// all content (directives and/or operation definitions).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the directives applied to this schema extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.content.directives()
  }

  /// Returns the operation definitions added by this schema extension, if any.
  #[inline]
  pub const fn root_operation_types_definition(&self) -> Option<&RootOperationTypesDefinition> {
    self.content.root_operation_types_definition()
  }

  /// Returns a reference to the content of this schema extension.
  ///
  /// The content determines what capabilities are being added to the schema and
  /// represents the core functionality of the extension.
  #[inline]
  pub const fn data(&self) -> &SchemaExtensionData<Directives, RootOperationTypesDefinition> {
    &self.content
  }

  /// Creates a parser for schema extensions using the provided sub-parsers.
  ///
  /// This parser handles the complete syntax for GraphQL schema extensions,
  /// supporting both directive-only and operational extensions. The parser
  /// is designed to be composable and integrate seamlessly with larger
  /// GraphQL document parsers.
  pub fn parser_with<'src, I, T, Error, E, DP, RP>(
    directives_parser: DP,
    root_operation_types_definition_parser: RP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: UnexpectedEndOfSchemaExtensionError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Extend: Parseable<'src, I, T, Error> + 'src,
    Schema: Parseable<'src, I, T, Error> + 'src,
    DP: Parser<'src, I, Directives, E> + Clone + 'src,
    RP: Parser<'src, I, RootOperationTypesDefinition, E> + Clone + 'src,
  {
    Extend::parser()
      .then(Schema::parser())
      .ignore_then(directives_parser.or_not())
      .then(root_operation_types_definition_parser.or_not())
      .try_map_with(|(directives, root_operation_types_definition), exa| {
        let content = match (directives, root_operation_types_definition) {
          (directives, Some(definitions)) => SchemaExtensionData::Operations {
            directives,
            definitions,
          },
          (Some(directives), None) => SchemaExtensionData::Directives(directives),
          (None, None) => {
            return Err(Error::unexpected_end_of_schema_extension(
              exa.span(),
              SchemaExtensionHint::DirectivesOrRootOperationTypesDefinition,
            ));
          }
        };

        Ok(Self {
          span: exa.span(),
          content,
        })
      })
  }
}

impl<'a, Directives, RootOperationTypesDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for SchemaExtension<Directives, RootOperationTypesDefinition>
where
  Directives: Parseable<'a, I, T, Error>,
  RootOperationTypesDefinition: Parseable<'a, I, T, Error>,
  Extend: Parseable<'a, I, T, Error>,
  Schema: Parseable<'a, I, T, Error>,
  Error: UnexpectedEndOfSchemaExtensionError,
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
    Self::parser_with(Directives::parser(), RootOperationTypesDefinition::parser())
  }
}
