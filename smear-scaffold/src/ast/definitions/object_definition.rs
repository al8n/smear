use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};



/// Represents a GraphQL Object type definition that defines a concrete type with fields.
///
/// Object types are the most common type in GraphQL schemas and represent entities with
/// a specific set of fields. They can implement interfaces, have directives applied,
/// and serve as the building blocks for complex type hierarchies.
///
/// Object types are fundamental to GraphQL's type system and serve multiple roles:
/// - Define the shape and behavior of data entities
/// - Implement interfaces to share common field contracts
/// - Serve as root types for operations (Query, Mutation, Subscription)
/// - Enable polymorphic queries through interface and union relationships
///
/// ## Examples
///
/// ```text
/// # Simple object type
/// type User {
///   id: ID!
///   name: String!
///   email: String!
/// }
///
/// # Object implementing interfaces
/// type User implements Node & Timestamped {
///   id: ID!
///   createdAt: DateTime!
///   updatedAt: DateTime!
///   name: String!
///   email: String!
/// }
///
/// # Object with description and directives
/// """
/// Represents a user account in the system.
/// Contains personal information and account settings.
/// """
/// type User @auth(required: true) @cache(maxAge: 300) {
///   id: ID!
///   name: String!
///   email: String! @sensitive
///   posts: [Post!]!
///   createdAt: DateTime!
/// }
///
/// # Object without fields (marker type)
/// type DeletePayload @deprecated(reason: "Use generic MutationPayload")
/// ```
///
/// ## Grammar
/// ```text
/// ObjectTypeDefinition:
///   Description? type Name ImplementsInterfaces? Directives? FieldsDefinition?
/// ```
///
/// Spec: [Object Type Definition](https://spec.graphql.org/draft/#sec-Object-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct ObjectTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> AsSpan<Span>
  for ObjectTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for ObjectTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoComponents
  for ObjectTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Name,
    Option<ImplementInterfaces>,
    Option<Directives>,
    Option<FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.name,
      self.implements,
      self.directives,
      self.fields_definition,
    )
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition>
  ObjectTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  /// Creates a new object type definition.
  #[inline]
  pub const fn new(span: Span, name: Name, implements: Option<ImplementInterfaces>, directives: Option<Directives>, fields_definition: Option<FieldsDefinition>) -> Self {
    Self { span, name, implements, directives, fields_definition }
  }

  /// Returns a reference to the span covering the entire object definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the object type.
  ///
  /// The object name becomes part of the schema's public API and should follow
  /// GraphQL naming conventions (PascalCase). The name must be unique within
  /// the schema's type namespace.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the optional interfaces this object implements.
  ///
  /// Interface implementation creates a contract where the object must provide
  /// all fields defined by the implemented interfaces. This enables polymorphic
  /// queries and shared behavior across different object types.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
    self.implements.as_ref()
  }

  /// Returns a reference to the optional directives applied to the object type.
  ///
  /// Object-level directives can control authentication, caching, deprecation,
  /// and other cross-cutting concerns that apply to the entire type.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the optional fields definition.
  ///
  /// The fields definition specifies all the fields available on this object type.
  /// Objects without fields are valid (marker types) but uncommon in practice.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.fields_definition.as_ref()
  }
}

/// Represents the content portion of an object type extension.
///
/// Object extensions can add different types of content to existing object types:
/// - New interface implementations
/// - Additional directives
/// - New field definitions
/// - Combinations of the above
///
/// Extensions enable modular schema composition and gradual schema evolution
/// without breaking existing type definitions.
#[derive(Debug, Clone, Copy)]
pub enum ObjectTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition> {
  /// Extension adds only directives, optionally with new interface implementations.
  ///
  /// This variant is used when extending an object to add metadata or behavioral
  /// modifiers without changing its field structure.
  ///
  /// ## Examples
  /// ```text
  /// extend type User @deprecated(reason: "Use UserV2")
  ///
  /// extend type User implements Auditable @auth(level: "admin")
  /// ```
  Directives {
    /// Optional new interface implementations
    implements: Option<ImplementInterfaces>,
    /// Directives being added to the object
    directives: Directives,
  },

  /// Extension adds new fields, optionally with interface implementations and directives.
  ///
  /// This is the most comprehensive form of object extension, allowing the addition
  /// of new fields along with supporting interface implementations and directives.
  ///
  /// ## Examples
  /// ```text
  /// extend type User {
  ///   avatar: String
  ///   lastLoginAt: DateTime
  /// }
  ///
  /// extend type User implements Timestamped @cache(maxAge: 600) {
  ///   createdAt: DateTime!
  ///   updatedAt: DateTime!
  ///   preferences: UserPreferences
  /// }
  /// ```
  Fields {
    /// Optional new interface implementations
    implements: Option<ImplementInterfaces>,
    /// Optional directives being added
    directives: Option<Directives>,
    /// New field definitions being added
    fields: FieldsDefinition,
  },

  /// Extension adds only new interface implementations.
  ///
  /// This variant is used when an object needs to implement additional interfaces
  /// without adding new fields or directives.
  ///
  /// ## Examples
  /// ```text
  /// extend type User implements Timestamped
  ///
  /// extend type Post implements Node & Searchable
  /// ```
  Implements(ImplementInterfaces),
}

impl<ImplementInterfaces, Directives, FieldsDefinition>
  ObjectTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>
{
  /// Returns the interfaces if this extension includes them.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
    match self {
      Self::Fields { implements, .. } | Self::Directives { implements, .. } => implements.as_ref(),
      Self::Implements(implements) => Some(implements),
    }
  }

  /// Returns the directives if this extension includes them.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
      Self::Implements { .. } => None,
    }
  }

  /// Returns the fields definition if this extension includes them.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    match self {
      Self::Fields { fields, .. } => Some(fields),
      Self::Directives { .. } | Self::Implements { .. } => None,
    }
  }
}

/// Represents a GraphQL Object type extension that adds capabilities to an existing object.
///
/// Object extensions allow incremental enhancement of existing object types without
/// modifying the original definition. They support schema evolution, modular composition,
/// and distributed development workflows.
///
/// Extensions can add:
/// - New field definitions
/// - Additional interface implementations  
/// - New directives for metadata or behavior
/// - Combinations of the above
///
/// ## Examples
///
/// ```text
/// # Add new fields
/// extend type User {
///   avatar: String
///   preferences: UserPreferences
/// }
///
/// # Add interface implementation
/// extend type User implements Timestamped
///
/// # Add directives
/// extend type User @deprecated(reason: "Use UserV2")
///
/// # Comprehensive extension
/// extend type User implements Auditable @cache(maxAge: 300) {
///   lastAuditAt: DateTime
///   auditHistory: [AuditEvent!]!
///   avatar: String @resize(width: 200, height: 200)
/// }
/// ```
///
/// ## Grammar
///
/// ```text
/// ObjectTypeExtension:
///   extend type Name ImplementsInterfaces? Directives? FieldsDefinition
///   | extend type Name ImplementsInterfaces? Directives
///   | extend type Name ImplementsInterfaces
/// ```
///
/// Spec: [Object Type Extension](https://spec.graphql.org/draft/#sec-Object-Type-Extension)
#[derive(Debug, Clone, Copy)]
pub struct ObjectTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  data: ObjectTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> AsSpan<Span>
  for ObjectTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for ObjectTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoComponents
  for ObjectTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Name,
    ObjectTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.data)
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition>
  ObjectTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  /// Creates a new object type extension.
  #[inline]
  pub const fn new(span: Span, name: Name, data: ObjectTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>) -> Self {
    Self { span, name, data }
  }

  /// Returns a reference to the span covering the entire object extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the object type being extended.
  ///
  /// This must reference an existing object type defined elsewhere in the schema.
  /// The extension will add new capabilities to the named object type.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the directives if this extension includes them.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the interfaces if this extension includes them.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
    self.data.implements()
  }

  /// Returns the fields definition if this extension includes them.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.data.fields_definition()
  }

  /// Returns a reference to the content being added by this extension.
  ///
  /// The content determines what new capabilities are being added to the object:
  /// - New field definitions
  /// - Additional interface implementations
  /// - New directives
  /// - Combinations of the above
  #[inline]
  pub const fn data(
    &self,
  ) -> &ObjectTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition> {
    &self.data
  }
}
