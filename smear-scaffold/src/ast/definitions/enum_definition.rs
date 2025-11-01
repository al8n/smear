use logosky::{
  LogoStream, Logos, Source, Token,
  chumsky::{self, Parseable, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};
use smear_lexer::{
  keywords::{Enum, Extend},
  punctuator::{LBrace, RBrace},
};

use core::marker::PhantomData;
use std::vec::Vec;

use crate::{error::UnexpectedEndOfEnumExtensionError, hints::EnumTypeExtensionHint};

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

  /// Creates a parser that can parse an enum value definition with custom directives parsing.
  ///
  /// This parser handles the complete enum value definition syntax including optional
  /// description, required enum value name, and optional directives.
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, EP, DP>(
    enum_value_parser: EP,
    directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    DP: Parser<'src, I, Directives, E> + Clone,
    EP: Parser<'src, I, EnumValue, E> + Clone,
  {
    enum_value_parser
      .then(directives_parser.or_not())
      .map_with(|(enum_value, directives), exa| Self {
        span: exa.span(),
        enum_value,
        directives,
      })
  }
}

impl<'a, EnumValue, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for EnumValueDefinition<EnumValue, Directives>
where
  EnumValue: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(EnumValue::parser(), Directives::parser())
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

  /// Creates a parser that can parse an enum values definition with custom enum value parsing.
  ///
  /// This parser handles the complete enum values definition syntax including the braces
  /// and ensures at least one enum value definition is present.
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, P>(
    enum_value_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    LBrace: Parseable<'src, I, T, Error>,
    RBrace: Parseable<'src, I, T, Error>,
    P: Parser<'src, I, EnumValueDefinition, E> + Clone,
    Container: chumsky::container::Container<EnumValueDefinition>,
  {
    LBrace::parser()
      .ignore_then(enum_value_parser.repeated().at_least(1).collect())
      .then_ignore(RBrace::parser())
      .map_with(|enum_values, exa| Self {
        span: exa.span(),
        enum_values,
        _m: PhantomData,
      })
  }
}

impl<'a, EnumValueDefinition, Container, I, T, Error> Parseable<'a, I, T, Error>
  for EnumValuesDefinition<EnumValueDefinition, Container>
where
  LBrace: Parseable<'a, I, T, Error>,
  RBrace: Parseable<'a, I, T, Error>,
  EnumValueDefinition: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<EnumValueDefinition>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(EnumValueDefinition::parser())
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
  /// Creates a new `EnumTypeDefinition` with the given components.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    directives: Option<Directives>,
    enum_values: Option<EnumValuesDefinition>,
  ) -> Self {
    Self {
      span,
      name,
      directives,
      enum_values,
    }
  }

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

  /// Creates a parser that can parse a complete enum definition.
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, NP, P, DP>(
    name_parser: NP,
    directives_parser: DP,
    enum_values_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Enum: Parseable<'src, I, T, Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    P: Parser<'src, I, EnumValuesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    Enum::parser()
      .ignore_then(Self::content_parser_with(
        name_parser,
        directives_parser,
        enum_values_definition_parser,
      ))
      .map_with(|(name, directives, enum_values), exa| {
        Self::new(exa.span(), name, directives, enum_values)
      })
  }

  /// Creates a parser for enum type definitions without the leading `enum` keyword.
  #[inline]
  pub fn content_parser_with<'src, I, T, Error, E, NP, P, DP>(
    name_parser: NP,
    directives_parser: DP,
    enum_values_definition_parser: P,
  ) -> impl Parser<'src, I, (Name, Option<Directives>, Option<EnumValuesDefinition>), E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    P: Parser<'src, I, EnumValuesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    name_parser
      .then(directives_parser.or_not())
      .then(enum_values_definition_parser.or_not())
      .map(|((name, directives), enum_values)| (name, directives, enum_values))
  }
}

impl<'a, Name, Directives, EnumValuesDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for EnumTypeDefinition<Name, Directives, EnumValuesDefinition>
where
  Name: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  EnumValuesDefinition: Parseable<'a, I, T, Error>,
  Enum: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(
      Name::parser(),
      Directives::parser(),
      EnumValuesDefinition::parser(),
    )
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

  /// Creates a parser that can parse enum extension data.
  ///
  /// This parser handles both types of enum extensions: those that add values
  /// (optionally with directives) and those that add only directives.
  pub fn parser_with<'src, I, T, Error, E, DP, EVP>(
    directives_parser: impl Fn() -> DP,
    enum_values_parser: impl Fn() -> EVP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    DP: Parser<'src, I, Directives, E> + Clone,
    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone,
  {
    choice((
      directives_parser()
        .or_not()
        .then(enum_values_parser())
        .map(|(directives, values)| Self::Values { directives, values }),
      directives_parser().map(Self::Directives),
    ))
  }
}

impl<'a, Directives, EnumValuesDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for EnumTypeExtensionData<Directives, EnumValuesDefinition>
where
  Directives: Parseable<'a, I, T, Error>,
  EnumValuesDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Directives::parser, EnumValuesDefinition::parser)
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
  /// Creates a new `EnumTypeExtension` with the given components.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: EnumTypeExtensionData<Directives, EnumValuesDefinition>,
  ) -> Self {
    Self { span, name, data }
  }

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

  /// Creates a parser that can parse a complete enum extension.
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, NP, DP, EVP>(
    name_parser: NP,
    directives_parser: DP,
    enum_values_definition_parser: EVP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnexpectedEndOfEnumExtensionError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Extend: Parseable<'src, I, T, Error> + 'src,
    Enum: Parseable<'src, I, T, Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone + 'src,
    DP: Parser<'src, I, Directives, E> + Clone + 'src,
  {
    Extend::parser()
      .then(Enum::parser())
      .ignore_then(Self::content_parser_with(
        name_parser,
        directives_parser,
        enum_values_definition_parser,
      ))
      .map_with(|(name, data), exa| Self::new(exa.span(), name, data))
  }

  /// Creates a parser for enum type extensions without the leading `extend` and `enum` keywords.
  #[inline]
  pub fn content_parser_with<'src, I, T, Error, E, NP, DP, EVP>(
    name_parser: NP,
    directives_parser: DP,
    enum_values_definition_parser: EVP,
  ) -> impl Parser<
    'src,
    I,
    (
      Name,
      EnumTypeExtensionData<Directives, EnumValuesDefinition>,
    ),
    E,
  > + Clone
  where
    T: Token<'src>,
    I: LogoStream<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnexpectedEndOfEnumExtensionError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone + 'src,
    DP: Parser<'src, I, Directives, E> + Clone + 'src,
  {
    name_parser
      .then(directives_parser.or_not())
      .then(enum_values_definition_parser.or_not())
      .try_map_with(|((name, directives), fields), exa| {
        let data = match (directives, fields) {
          (Some(directives), None) => EnumTypeExtensionData::Directives(directives),
          (directives, Some(values)) => EnumTypeExtensionData::Values { directives, values },
          (None, None) => {
            return Err(Error::unexpected_end_of_enum_extension(
              exa.span(),
              EnumTypeExtensionHint::DirectivesOrEnumValuesDefinition,
            ));
          }
        };
        Ok((name, data))
      })
  }
}

impl<'a, Name, Directives, EnumValuesDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for EnumTypeExtension<Name, Directives, EnumValuesDefinition>
where
  Error: UnexpectedEndOfEnumExtensionError,
  Name: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  EnumValuesDefinition: Parseable<'a, I, T, Error>,
  Extend: Parseable<'a, I, T, Error>,
  Enum: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(
      Name::parser(),
      Directives::parser(),
      EnumValuesDefinition::parser(),
    )
  }
}
