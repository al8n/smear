use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};

use core::marker::PhantomData;
use std::vec::Vec;


/// Represents a single enum value definition in a GraphQL enum type.
///
/// An enum value definition specifies one possible value that an enum type can have.
/// It consists of the enum value name, optional description for documentation,
/// and optional directives that provide metadata or behavior for the value.
///
/// ## Examples
///
/// ```text
/// # Simple enum value
/// ACTIVE
///
/// # Enum value with description
/// """
/// Represents an active user account
/// """
/// ACTIVE
///
/// # Enum value with directives
/// LEGACY_STATUS @deprecated(reason: "Use ACTIVE instead")
///
/// # Complex enum value with description and directives
/// """
/// Represents a suspended user account.
/// This status indicates temporary restrictions.
/// """
/// SUSPENDED @auth(requires: ADMIN) @internal
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to this enum value
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// EnumValueDefinition : Description? EnumValue Directives?
/// ```
///
/// Spec: [Enum Value Definition](https://spec.graphql.org/draft/#sec-Enum-Value-Definition)
#[derive(Debug, Clone, Copy)]
pub struct EnumValueDefinition<EnumValue, Directives> {
  span: Span,
  enum_value: EnumValue,
  directives: Option<Directives>,
}

impl<EnumValue, Directives> AsSpan<Span> for EnumValueDefinition<EnumValue, Directives> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<EnumValue, Directives> IntoSpan<Span> for EnumValueDefinition<EnumValue, Directives> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<EnumValue, Directives> IntoComponents for EnumValueDefinition<EnumValue, Directives> {
  type Components = (Span, EnumValue, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.enum_value, self.directives)
  }
}

impl<EnumValue, Directives> EnumValueDefinition<EnumValue, Directives> {
  /// Returns a reference to the span covering the entire enum value definition.
  ///
  /// The span includes the optional description, enum value name, and optional directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the enum value name.
  ///
  /// This is the identifier that represents this specific enum value.
  /// Enum values must be valid GraphQL names and cannot be the reserved
  /// words `true`, `false`, or `null`.
  #[inline]
  pub const fn value(&self) -> &EnumValue {
    &self.enum_value
  }

  /// Returns a reference to the optional directives applied to this enum value.
  ///
  /// Directives provide metadata or specify behavior for the enum value,
  /// such as deprecation information or access control.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// Represents the collection of enum value definitions within an enum type.
///
/// An enum values definition is a braced collection of one or more enum value
/// definitions. It defines all the possible values that an enum type can have.
/// This structure maintains the source span information and provides access
/// to the individual enum value definitions.
///
/// ## Examples
///
/// ```text
/// # Simple enum values
/// {
///   ACTIVE
///   INACTIVE
///   PENDING
/// }
///
/// # Enum values with descriptions and directives
/// {
///   """
///   Represents an active user account
///   """
///   ACTIVE
///   
///   """
///   Represents an inactive user account
///   """
///   INACTIVE @deprecated(reason: "Use SUSPENDED instead")
///   
///   """
///   Represents a suspended user account
///   """
///   SUSPENDED
/// }
/// ```
///
/// ## Type Parameters
///
/// * `EnumValueDefinition` - The type representing individual enum value definitions
/// * `Span` - The type representing source location information
/// * `Container` - The container type for storing enum value definitions (defaults to `Vec<EnumValueDefinition>`)
///
/// ## Grammar
///
/// ```text
/// EnumValuesDefinition : { EnumValueDefinition+ }
/// ```
///
/// Note: At least one enum value definition is required (the `+` indicates one-or-more).
/// Empty enum values definitions `{}` are not valid in GraphQL.
///
/// Spec: [Enum Values Definition](https://spec.graphql.org/draft/#sec-Enum-Values-Definition)
#[derive(Debug, Clone, Copy)]
pub struct EnumValuesDefinition<EnumValueDefinition, Container = Vec<EnumValueDefinition>> {
  span: Span,
  enum_values: Container,
  _m: PhantomData<EnumValueDefinition>,
}

impl<EnumValueDefinition, Container> AsSpan<Span>
  for EnumValuesDefinition<EnumValueDefinition, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<EnumValueDefinition, Container> IntoSpan<Span>
  for EnumValuesDefinition<EnumValueDefinition, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<EnumValueDefinition, Container> IntoComponents
  for EnumValuesDefinition<EnumValueDefinition, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.enum_values)
  }
}

impl<EnumValueDefinition, Container> EnumValuesDefinition<EnumValueDefinition, Container> {
  /// Returns a reference to the span covering the entire enum values definition.
  ///
  /// The span includes the opening brace, all enum value definitions, and the closing brace.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all enum value definitions.
  ///
  /// This allows iteration over, indexing into, or otherwise working with
  /// the collection of enum value definitions.
  #[inline]
  pub const fn enum_value_definitions(&self) -> &Container {
    &self.enum_values
  }

  /// Consumes the enum values definition and returns the enum value definitions
  #[inline]
  pub fn into_enum_value_definitions(self) -> Container {
    self.enum_values
  }
}

/// Represents a complete enum type definition in GraphQL schema.
///
/// An enum type definition specifies a type that can have one of a finite set of values.
/// It includes the enum keyword, name, optional description, optional directives,
/// and optional enum values definition. Enum types are useful for representing
/// a fixed set of possible values.
///
/// ## Examples
///
/// ```text
/// # Simple enum definition
/// enum Status {
///   ACTIVE
///   INACTIVE
/// }
///
/// # Enum with description and directives
/// """
/// Represents the status of a user account
/// """
/// enum UserStatus @auth(requires: ADMIN) {
///   """
///   Account is active and in good standing
///   """
///   ACTIVE
///   
///   """
///   Account is temporarily suspended
///   """
///   SUSPENDED @deprecated(reason: "Use INACTIVE")
///   
///   """
///   Account is permanently deactivated
///   """
///   INACTIVE
/// }
///
/// # Enum definition without values (for extensions)
/// enum Status @directive
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the enum type
/// * `EnumValuesDefinition` - The type representing the enum values collection
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// EnumTypeDefinition : Description? enum Name Directives? EnumValuesDefinition?
/// ```
///
/// Spec: [Enum Type Definition](https://spec.graphql.org/draft/#sec-Enum-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct EnumTypeDefinition<Name, Directives, EnumValuesDefinition> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
  enum_values: Option<EnumValuesDefinition>,
}

impl<Name, Directives, EnumValuesDefinition> AsSpan<Span>
  for EnumTypeDefinition<Name, Directives, EnumValuesDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, EnumValuesDefinition> IntoSpan<Span>
  for EnumTypeDefinition<Name, Directives, EnumValuesDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, EnumValuesDefinition> IntoComponents
  for EnumTypeDefinition<Name, Directives, EnumValuesDefinition>
{
  type Components = (Span, Name, Option<Directives>, Option<EnumValuesDefinition>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.directives, self.enum_values)
  }
}

impl<Name, Directives, EnumValuesDefinition>
  EnumTypeDefinition<Name, Directives, EnumValuesDefinition>
{
  /// Returns a reference to the span covering the entire enum definition.
  ///
  /// The span includes the optional description, enum keyword, name, optional
  /// directives, and optional enum values definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the enum type.
  ///
  /// This is the identifier that will be used to reference this enum type
  /// in other parts of the schema and in GraphQL operations.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the optional enum values definition.
  ///
  /// The enum values definition contains all the possible values for this enum type.
  /// It may be absent in enum definitions that are meant to be extended later.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&EnumValuesDefinition> {
    self.enum_values.as_ref()
  }

  /// Returns a reference to the optional directives applied to this enum type.
  ///
  /// Directives provide metadata or specify behavior for the enum type,
  /// such as access control, validation rules, or custom processing instructions.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// Represents the content of an enum type extension.
///
/// Enum extensions can add new enum values to an existing enum type and/or
/// add new directives to the enum type. This enum represents the different
/// kinds of content that can be added in an enum extension.
///
/// ## GraphQL Extension Context
///
/// GraphQL allows extending existing types to add new functionality without
/// modifying the original type definition. For enum types, extensions can:
/// - Add new enum values (with optional additional directives on the type)
/// - Add only directives to the type (without new values)
///
/// ## Examples
///
/// ```text
/// # Extension adding values only
/// extend enum Status {
///   ARCHIVED
///   MIGRATED
/// }
///
/// # Extension adding directives and values
/// extend enum Status @deprecated {
///   LEGACY_ACTIVE
///   LEGACY_INACTIVE
/// }
///
/// # Extension adding only directives
/// extend enum Status @auth(requires: ADMIN)
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the enum extension
/// * `EnumValuesDefinition` - The type representing the new enum values being added
#[derive(Debug, Clone, Copy)]
pub enum EnumTypeExtensionData<Directives, EnumValuesDefinition> {
  /// Extension that adds new enum values, optionally with additional directives on the type
  Values {
    /// Optional directives to add to the enum type itself
    directives: Option<Directives>,
    /// New enum values to add to the enum type
    values: EnumValuesDefinition,
  },
  /// Extension that adds only directives to the enum type without new values
  Directives(Directives),
}

impl<Directives, EnumValuesDefinition> EnumTypeExtensionData<Directives, EnumValuesDefinition> {
  /// Returns the directives associated with this enum type extension content, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Values { directives, .. } => directives.as_ref(),
      Self::Directives(directives) => Some(directives),
    }
  }

  /// Returns the enum values definition if this extension adds new values.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&EnumValuesDefinition> {
    match self {
      Self::Values { values, .. } => Some(values),
      Self::Directives(_) => None,
    }
  }
}

/// Represents a complete enum type extension in GraphQL schema.
///
/// An enum extension allows adding new enum values and/or directives to an
/// existing enum type without modifying the original definition. This is
/// particularly useful in schema composition scenarios where different
/// parts of a system need to extend shared types.
///
/// ## GraphQL Extension Philosophy
///
/// Extensions support GraphQL's modular approach to schema development:
/// - **Non-destructive**: Extensions don't modify original definitions
/// - **Additive**: Extensions can only add new capabilities
/// - **Composable**: Multiple extensions can be applied to the same type
/// - **Modular**: Different services can extend shared types
///
/// ## Examples
///
/// ```text
/// # Simple enum extension adding values
/// extend enum UserStatus {
///   ARCHIVED
///   MIGRATED
/// }
///
/// # Enum extension adding directives to the type and new values
/// extend enum UserStatus @deprecated(reason: "Use AccountStatus instead") {
///   LEGACY_ACTIVE
///   LEGACY_INACTIVE
/// }
///
/// # Enum extension adding only directives to the type
/// extend enum UserStatus @auth(requires: ADMIN) @rateLimit(max: 100)
///
/// # Complex enum extension with descriptions
/// extend enum UserStatus @internal {
///   """
///   Special status for system accounts
///   """
///   SYSTEM
///   
///   """
///   Status for accounts undergoing migration
///   """
///   MIGRATING @deprecated(reason: "Migration completed")
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied in the extension
/// * `EnumValuesDefinition` - The type representing the new enum values being added
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// EnumTypeExtension : extend enum Name ( Directives EnumValuesDefinition? | EnumValuesDefinition )
/// ```
///
/// Spec: [Enum Type Extension](https://spec.graphql.org/draft/#sec-Enum-Type-Extension)
#[derive(Debug, Clone, Copy)]
pub struct EnumTypeExtension<Name, Directives, EnumValuesDefinition> {
  span: Span,
  name: Name,
  data: EnumTypeExtensionData<Directives, EnumValuesDefinition>,
}

impl<Name, Directives, EnumValuesDefinition> AsSpan<Span>
  for EnumTypeExtension<Name, Directives, EnumValuesDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, EnumValuesDefinition> IntoSpan<Span>
  for EnumTypeExtension<Name, Directives, EnumValuesDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, EnumValuesDefinition> IntoComponents
  for EnumTypeExtension<Name, Directives, EnumValuesDefinition>
{
  type Components = (
    Span,
    Name,
    EnumTypeExtensionData<Directives, EnumValuesDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.data)
  }
}

impl<Name, Directives, EnumValuesDefinition>
  EnumTypeExtension<Name, Directives, EnumValuesDefinition>
{
  /// Returns a reference to the span covering the entire enum extension.
  ///
  /// The span includes the extend keyword, enum keyword, name, and all
  /// extension content (directives and/or enum values).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the enum type being extended.
  ///
  /// This must match the name of an existing enum type in the schema
  /// for the extension to be valid.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the directives associated with this enum type extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the enum values definition if this extension adds new values.
  #[inline]
  pub const fn enum_values_definition(&self) -> Option<&EnumValuesDefinition> {
    self.data.enum_values_definition()
  }

  /// Returns a reference to the extension data.
  ///
  /// The content specifies what is being added to the enum type:
  /// either new values (optionally with directives), or just directives.
  #[inline]
  pub const fn data(&self) -> &EnumTypeExtensionData<Directives, EnumValuesDefinition> {
    &self.data
  }
}
