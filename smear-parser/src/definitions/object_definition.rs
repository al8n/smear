use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{ignored, keywords, Name, StringValue},
  source::{Char, Slice, Source},
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
pub struct ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  ty: keywords::Type<Span>,
  name: Name<Span>,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> AsRef<Span>
  for ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoSpan<Span>
  for ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoComponents
  for ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Type<Span>,
    Name<Span>,
    Option<ImplementInterfaces>,
    Option<Directives>,
    Option<FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.ty,
      self.name,
      self.implements,
      self.directives,
      self.fields_definition,
    )
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span>
  ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  /// Returns a reference to the span covering the entire object definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the object type.
  ///
  /// The description documents the object's purpose, usage patterns, and any
  /// important behavioral notes. It's essential for API documentation and
  /// developer understanding of the type's role in the schema.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the `type` keyword token.
  ///
  /// This provides access to the exact `type` keyword and its source location,
  /// useful for error reporting and tooling integration.
  #[inline]
  pub const fn type_keyword(&self) -> &keywords::Type<Span> {
    &self.ty
  }

  /// Returns a reference to the name of the object type.
  ///
  /// The object name becomes part of the schema's public API and should follow
  /// GraphQL naming conventions (PascalCase). The name must be unique within
  /// the schema's type namespace.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
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

  /// Creates a parser for object type definitions.
  ///
  /// This parser handles the complete syntax for GraphQL object types, including
  /// all optional components like descriptions, interface implementations,
  /// directives, and field definitions.
  pub fn parser_with<'src, I, E, FDP, DP, IP>(
    fields_definition_parser: FDP,
    directives_parser: DP,
    implement_interfaces_parser: IP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    DP: Parser<'src, I, Directives, E> + Clone,
    FDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(keywords::Type::parser())
      .then_ignore(ignored())
      .then(Name::parser())
      .then(implement_interfaces_parser.padded_by(ignored()).or_not())
      .then(directives_parser.padded_by(ignored()).or_not())
      .then(fields_definition_parser.padded_by(ignored()).or_not())
      .map_with(
        |(((((description, ty), name), implements), directives), fields), sp| Self {
          span: Span::from_map_extra(sp),
          description,
          name,
          directives,
          fields_definition: fields,
          ty,
          implements,
        },
      )
      .padded_by(ignored())
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
pub enum ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition> {
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
  ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>
{
  /// Creates a parser for object extension content.
  ///
  /// This parser uses a choice combinator to try different extension patterns,
  /// ensuring that the most specific matches (like fields with directives) are
  /// attempted before more general ones (like directives only).
  pub fn parser_with<'src, I, E, IP, FDP, DP>(
    implement_interfaces_parser: impl Fn() -> IP,
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FDP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    FDP: Parser<'src, I, FieldsDefinition, E> + Clone,
  {
    choice((
      implement_interfaces_parser()
        .then_ignore(ignored())
        .or_not()
        .then(directives_parser().then_ignore(ignored()).or_not())
        .then(fields_definition_parser())
        .map(|((implements, directives), fields)| Self::Fields {
          implements,
          directives,
          fields,
        }),
      implement_interfaces_parser()
        .then_ignore(ignored())
        .or_not()
        .then(directives_parser())
        .map(|(implements, directives)| Self::Directives {
          implements,
          directives,
        }),
      implement_interfaces_parser().map(Self::Implements),
    ))
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
pub struct ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  interface: keywords::Type<Span>,
  name: Name<Span>,
  content: ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> AsRef<Span>
  for ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoSpan<Span>
  for ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoComponents
  for ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  type Components = (
    Span,
    keywords::Extend<Span>,
    keywords::Type<Span>,
    Name<Span>,
    ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.extend,
      self.interface,
      self.name,
      self.content,
    )
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span>
  ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  /// Returns a reference to the span covering the entire object extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `extend` keyword token.
  ///
  /// This provides access to the exact `extend` keyword that begins the extension,
  /// useful for error reporting and distinguishing extensions from definitions.
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  /// Returns a reference to the `type` keyword token.
  ///
  /// This provides access to the `type` keyword that follows `extend`,
  /// helping distinguish object extensions from other extension types.
  #[inline]
  pub const fn interface_keyword(&self) -> &keywords::Type<Span> {
    &self.interface
  }

  /// Returns a reference to the name of the object type being extended.
  ///
  /// This must reference an existing object type defined elsewhere in the schema.
  /// The extension will add new capabilities to the named object type.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the content being added by this extension.
  ///
  /// The content determines what new capabilities are being added to the object:
  /// - New field definitions
  /// - Additional interface implementations
  /// - New directives
  /// - Combinations of the above
  #[inline]
  pub const fn content(
    &self,
  ) -> &ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition> {
    &self.content
  }

  /// Creates a parser for object type extensions.
  ///
  /// This parser handles the `extend type` syntax followed by the object name
  /// and extension content. The content parsing is delegated to the
  /// `ObjectExtensionContent` parser for modularity.
  pub fn parser_with<'src, I, E, FDP, DP, IP>(
    implement_interfaces_parser: impl Fn() -> IP,
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FDP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    DP: Parser<'src, I, Directives, E> + Clone,
    FDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Type::parser())
      .then_ignore(ignored())
      .then(Name::parser().then_ignore(ignored()))
      .then(ObjectExtensionContent::<
        ImplementInterfaces,
        Directives,
        FieldsDefinition,
      >::parser_with(
        implement_interfaces_parser,
        directives_parser,
        fields_definition_parser,
      ))
      .map_with(|(((extend, interface), name), content), sp| Self {
        span: Span::from_map_extra(sp),
        extend,
        interface,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
