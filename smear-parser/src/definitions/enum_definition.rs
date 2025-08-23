use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  convert::*,
  lang::{
    ignored, keywords,
    punct::{LBrace, RBrace},
    EnumValue, Name, StringValue,
  },
  source::{Char, Slice, Source},
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
#[derive(Debug, Clone)]
pub struct EnumValueDefinition<Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  enum_value: EnumValue<Span>,
  directives: Option<Directives>,
}

impl<Directives, Span> AsRef<Span> for EnumValueDefinition<Directives, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, Span> IntoSpan<Span> for EnumValueDefinition<Directives, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, Span> IntoComponents for EnumValueDefinition<Directives, Span> {
  type Components = (
    Span,
    Option<StringValue<Span>>,
    EnumValue<Span>,
    Option<Directives>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.enum_value,
      self.directives,
    )
  }
}

impl<Directives, Span> EnumValueDefinition<Directives, Span> {
  /// Returns a reference to the span covering the entire enum value definition.
  /// 
  /// The span includes the optional description, enum value name, and optional directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the enum value definition.
  /// 
  /// The description provides documentation for the enum value and appears before
  /// the value name. It can be either a single-line string or a block string.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the enum value name.
  /// 
  /// This is the identifier that represents this specific enum value.
  /// Enum values must be valid GraphQL names and cannot be the reserved
  /// words `true`, `false`, or `null`.
  #[inline]
  pub const fn enum_value(&self) -> &EnumValue<Span> {
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
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the enum value definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, E, DP>(directives_parser: DP) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(EnumValue::<Span>::parser().then_ignore(ignored()))
      .then(directives_parser.or_not())
      .map_with(|((description, enum_value), directives), sp| Self {
        span: Span::from_map_extra(sp),
        description,
        enum_value,
        directives,
      })
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
#[derive(Debug, Clone)]
pub struct EnumValuesDefinition<EnumValueDefinition, Span, Container = Vec<EnumValueDefinition>> {
  span: Span,
  l_brace: LBrace<Span>,
  r_brace: RBrace<Span>,
  enum_values: Container,
  _m: PhantomData<EnumValueDefinition>,
}

impl<EnumValueDefinition, Span, Container> AsRef<Span>
  for EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<EnumValueDefinition, Span, Container> IntoSpan<Span>
  for EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<EnumValueDefinition, Span, Container> IntoComponents
  for EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.enum_values, self.r_brace)
  }
}

impl<EnumValueDefinition, Span, Container>
  EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  /// Returns a reference to the span covering the entire enum values definition.
  /// 
  /// The span includes the opening brace, all enum value definitions, and the closing brace.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the opening left brace (`{`) of the enum values definition.
  /// 
  /// This provides access to the exact location and span information of the
  /// opening delimiter.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// Returns a reference to the closing right brace (`}`) of the enum values definition.
  /// 
  /// This provides access to the exact location and span information of the
  /// closing delimiter.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Returns a reference to the container holding all enum value definitions.
  /// 
  /// This allows iteration over, indexing into, or otherwise working with
  /// the collection of enum value definitions.
  #[inline]
  pub const fn enum_values(&self) -> &Container {
    &self.enum_values
  }

  /// Creates a parser that can parse an enum values definition with custom enum value parsing.
  /// 
  /// This parser handles the complete enum values definition syntax including the braces
  /// and ensures at least one enum value definition is present.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the enum values definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, E, P>(enum_value_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, EnumValueDefinition, E> + Clone,
    Container: chumsky::container::Container<EnumValueDefinition>,
  {
    LBrace::parser()
      .then_ignore(ignored())
      .then(
        enum_value_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then_ignore(ignored())
      .then(RBrace::parser())
      .map_with(|((l_brace, enum_values), r_brace), sp| Self {
        span: Span::from_map_extra(sp),
        l_brace,
        r_brace,
        enum_values,
        _m: PhantomData,
      })
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
#[derive(Debug, Clone)]
pub struct EnumDefinition<Directives, EnumValuesDefinition, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  keyword: keywords::Enum<Span>,
  name: Name<Span>,
  directives: Option<Directives>,
  enum_values: Option<EnumValuesDefinition>,
}

impl<Directives, EnumValuesDefinition, Span> AsRef<Span>
  for EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, EnumValuesDefinition, Span> IntoSpan<Span>
  for EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, EnumValuesDefinition, Span> IntoComponents
  for EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Enum<Span>,
    Name<Span>,
    Option<Directives>,
    Option<EnumValuesDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.keyword,
      self.name,
      self.directives,
      self.enum_values,
    )
  }
}

impl<Directives, EnumValuesDefinition, Span>
  EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  /// Returns a reference to the span covering the entire enum definition.
  /// 
  /// The span includes the optional description, enum keyword, name, optional
  /// directives, and optional enum values definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional description of the enum definition.
  /// 
  /// The description provides documentation for the enum type and appears before
  /// the enum keyword. It can be either a single-line string or a block string.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the name of the enum type.
  /// 
  /// This is the identifier that will be used to reference this enum type
  /// in other parts of the schema and in GraphQL operations.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the `enum` keyword.
  /// 
  /// This provides access to the exact location and span information of the
  /// enum keyword that defines this type.
  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Span> {
    &self.keyword
  }

  /// Returns a reference to the optional enum values definition.
  /// 
  /// The enum values definition contains all the possible values for this enum type.
  /// It may be absent in enum definitions that are meant to be extended later.
  #[inline]
  pub const fn enum_values(&self) -> Option<&EnumValuesDefinition> {
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
  /// 
  /// This parser handles the full enum definition syntax including all optional
  /// components. The parsing of enum values and directives is delegated to the
  /// provided parsers.
  #[inline]
  pub fn parser_with<'src, I, E, P, DP>(
    enum_values_definition: P,
    directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, EnumValuesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(keywords::Enum::parser())
      .then_ignore(ignored())
      .then(Name::<Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser.or_not())
      .then_ignore(ignored())
      .then(enum_values_definition.or_not())
      .map_with(
        |((((description, keyword), name), directives), enum_values), sp| Self {
          span: Span::from_map_extra(sp),
          description,
          keyword,
          name,
          directives,
          enum_values,
        },
      )
      .padded_by(ignored())
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
#[derive(Debug, Clone)]
pub enum EnumExtensionContent<Directives, EnumValuesDefinition> {
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

impl<Directives, EnumValuesDefinition> EnumExtensionContent<Directives, EnumValuesDefinition> {
  /// Creates a parser that can parse enum extension content.
  /// 
  /// This parser handles both types of enum extensions: those that add values
  /// (optionally with directives) and those that add only directives.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the extension content.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, DP, EVP>(
    directives_parser: impl Fn() -> DP,
    enum_values_parser: impl Fn() -> EVP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,

    DP: Parser<'src, I, Directives, E> + Clone,
    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone,
  {
    choice((
      directives_parser()
        .then_ignore(ignored())
        .or_not()
        .then(enum_values_parser())
        .map(|(directives, values)| Self::Values { directives, values }),
      directives_parser().map(Self::Directives),
    ))
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
#[derive(Debug, Clone)]
pub struct EnumExtension<Directives, EnumValuesDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  keyword: keywords::Enum<Span>,
  name: Name<Span>,
  content: EnumExtensionContent<Directives, EnumValuesDefinition>,
}

impl<Directives, EnumValuesDefinition, Span> AsRef<Span>
  for EnumExtension<Directives, EnumValuesDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, EnumValuesDefinition, Span> IntoSpan<Span>
  for EnumExtension<Directives, EnumValuesDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, EnumValuesDefinition, Span> IntoComponents
  for EnumExtension<Directives, EnumValuesDefinition, Span>
{
  type Components = (
    Span,
    keywords::Extend<Span>,
    keywords::Enum<Span>,
    Name<Span>,
    EnumExtensionContent<Directives, EnumValuesDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.extend,
      self.keyword,
      self.name,
      self.content,
    )
  }
}

impl<Directives, EnumValuesDefinition, Span> EnumExtension<Directives, EnumValuesDefinition, Span> {
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
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns a reference to the `extend` keyword.
  /// 
  /// This provides access to the exact location and span information of the
  /// extend keyword that starts the extension definition.
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  /// Returns a reference to the `enum` keyword.
  /// 
  /// This provides access to the exact location and span information of the
  /// enum keyword that specifies the type being extended.
  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Span> {
    &self.keyword
  }

  /// Returns a reference to the extension content.
  /// 
  /// The content specifies what is being added to the enum type:
  /// either new values (optionally with directives), or just directives.
  #[inline]
  pub const fn content(&self) -> &EnumExtensionContent<Directives, EnumValuesDefinition> {
    &self.content
  }

  /// Creates a parser that can parse a complete enum extension.
  /// 
  /// This parser handles the full enum extension syntax including the extend
  /// and enum keywords, target enum name, and extension content.
  #[inline]
  pub fn parser_with<'src, I, E, DP, EVP>(
    directives_parser: impl Fn() -> DP,
    enum_values_definition: impl Fn() -> EVP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Enum::parser())
      .then_ignore(ignored())
      .then(Name::<Span>::parser().then_ignore(ignored()))
      .then(EnumExtensionContent::parser_with(
        directives_parser,
        enum_values_definition,
      ))
      .map_with(|(((extend, keyword), name), content), sp| Self {
        span: Span::from_map_extra(sp),
        extend,
        keyword,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
