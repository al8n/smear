use core::marker::PhantomData;
use std::vec::Vec;

use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{self, Parseable, extra::ParserExtra, prelude::*},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};
use smear_lexer::{
  keywords::{Extend, Implements, Interface},
  punctuator::Ampersand,
};

use crate::{error::UnexpectedEndOfInterfaceExtensionError, hints::InterfaceTypeExtensionHint};

/// Represents a collection of interfaces that a GraphQL type or interface implements.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementsInterfaces<Name, Container = Vec<Name>> {
  span: Span,
  interfaces: Container,
  _m: PhantomData<Name>,
}

impl<Name, Container> ImplementsInterfaces<Name, Container> {
  /// Returns a reference to the span covering the entire implements clause.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding the implemented interfaces.
  #[inline]
  pub const fn interfaces(&self) -> &Container {
    &self.interfaces
  }

  /// Returns a slice of the implemented interfaces.
  #[inline]
  pub fn interfaces_slice(&self) -> &[Name]
  where
    Container: AsRef<[Name]>,
  {
    self.interfaces().as_ref()
  }
}

impl<'a, Name, Container, I, T, Error> Parseable<'a, I, T, Error>
  for ImplementsInterfaces<Name, Container>
where
  Container: chumsky::container::Container<Name>,
  Name: Parseable<'a, I, T, Error>,
  Ampersand: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    T: Token<'a>,
  {
    Name::parser()
      .separated_by(Ampersand::parser())
      .allow_leading()
      .at_least(1)
      .collect()
      .map_with(|names, exa| {
        let span = exa.span();
        Self {
          span,
          interfaces: names,
          _m: PhantomData,
        }
      })
  }
}

impl<Name, Container> DisplayCompact for ImplementsInterfaces<Name, Container>
where
  Container: AsRef<[Name]>,
  Name: DisplayCompact,
{
  type Options = Name::Options;

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, opts: &Self::Options) -> core::fmt::Result {
    let interfaces = self.interfaces().as_ref();

    for interface in interfaces.iter() {
      write!(f, "&{}", interface.display(opts))?;
    }
    Ok(())
  }
}

impl<Name, Container> DisplayPretty for ImplementsInterfaces<Name, Container>
where
  Container: AsRef<[Name]>,
  Name: DisplayPretty,
{
  type Options = Name::Options;

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, opts: &Self::Options) -> core::fmt::Result {
    let interfaces = self.interfaces().as_ref();
    for interface in interfaces.iter() {
      writeln!(f, "\t& {}", interface.display(opts))?;
    }
    Ok(())
  }
}

/// Represents a GraphQL Interface type definition that defines a contract for implementing types.
///
/// Interface types define a set of fields that implementing types must provide, enabling
/// polymorphic queries and shared behavior across different object types. Interfaces are
/// fundamental to GraphQL's type system and support complex inheritance hierarchies.
///
/// Interfaces can themselves implement other interfaces (interface inheritance), creating
/// sophisticated type hierarchies that promote code reuse and consistent API design.
///
/// ## Examples
///
/// ```text
/// # Simple interface
/// interface Node {
///   id: ID!
/// }
///
/// # Interface with multiple fields
/// interface Timestamped {
///   createdAt: DateTime!
///   updatedAt: DateTime!
/// }
///
/// # Interface implementing another interface
/// interface MutableNode implements Node {
///   id: ID!           # Required by Node
///   version: Int!     # Additional field
/// }
///
/// # Complex interface with description and directives
/// """
/// Represents entities that can be searched and indexed.
/// Provides common search functionality across different types.
/// """
/// interface Searchable @auth(required: false) @cache(maxAge: 600) {
///   searchText: String!
///   searchScore: Float
///   searchHighlights: [String!]
///   lastIndexed: DateTime
/// }
/// ```
///
/// ## Grammar
///
/// ```text
/// InterfaceTypeDefinition:
///   Description? interface Name ImplementsInterfaces? Directives? FieldsDefinition?
/// ```
///
/// Spec: [Interface Type Definition](https://spec.graphql.org/draft/#sec-Interface-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  implements: Option<ImplementsInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition> AsSpan<Span>
  for InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition> IntoComponents
  for InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Name,
    Option<ImplementsInterfaces>,
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

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition>
  InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  /// Creates a new `InterfaceTypeDefinition` with the given components.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    implements: Option<ImplementsInterfaces>,
    directives: Option<Directives>,
    fields_definition: Option<FieldsDefinition>,
  ) -> Self {
    Self {
      span,
      name,
      implements,
      directives,
      fields_definition,
    }
  }

  /// Returns a reference to the span covering the entire interface definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the interface type.
  ///
  /// Interface names should clearly indicate the contract or behavior
  /// they represent, following GraphQL naming conventions.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the optional interfaces this interface implements.
  ///
  /// Interface inheritance allows interfaces to extend other interfaces,
  /// creating hierarchical contracts that implementing types must fulfill.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces> {
    self.implements.as_ref()
  }

  /// Returns a reference to the optional directives applied to the interface.
  ///
  /// Interface-level directives can specify authorization requirements,
  /// caching behavior, or other metadata that applies to all implementing types.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the optional fields definition.
  ///
  /// Interface fields define the contract that implementing types must fulfill.
  /// Each implementing type must provide all interface fields with compatible
  /// types and arguments.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.fields_definition.as_ref()
  }

  /// Creates a parser for interface type definitions.
  pub fn parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: IP,
    directives_parser: DP,
    fields_definition_parser: FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Interface: Parseable<'src, I, T, Error>,
    Implements: Parseable<'src, I, T, Error>,
    Name: 'src,
    Directives: 'src,
    ImplementsInterfaces: 'src,
    FieldsDefinition: 'src,
    NP: Parser<'src, I, Name, E> + Clone + 'src,
    DP: Parser<'src, I, Directives, E> + Clone + 'src,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone + 'src,
    IP: Parser<'src, I, ImplementsInterfaces, E> + Clone + 'src,
  {
    Interface::parser()
      .ignore_then(Self::content_parser_with(
        name_parser,
        implement_interfaces_parser,
        directives_parser,
        fields_definition_parser,
      ))
      .map_with(|(name, implements, directives, fields_definition), exa| {
        Self::new(exa.span(), name, implements, directives, fields_definition)
      })
  }

  /// Creates a parser for interface type definitions without the leading `interface` keyword.
  pub fn content_parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: IP,
    directives_parser: DP,
    fields_definition_parser: FP,
  ) -> impl Parser<
    'src,
    I,
    (
      Name,
      Option<ImplementsInterfaces>,
      Option<Directives>,
      Option<FieldsDefinition>,
    ),
    E,
  > + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Interface: Parseable<'src, I, T, Error>,
    Implements: Parseable<'src, I, T, Error>,
    Name: 'src,
    Directives: 'src,
    ImplementsInterfaces: 'src,
    FieldsDefinition: 'src,
    NP: Parser<'src, I, Name, E> + Clone + 'src,
    DP: Parser<'src, I, Directives, E> + Clone + 'src,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone + 'src,
    IP: Parser<'src, I, ImplementsInterfaces, E> + Clone + 'src,
  {
    name_parser
      .then(
        Implements::parser()
          .ignore_then(implement_interfaces_parser)
          .or_not(),
      )
      .then(directives_parser.or_not())
      .then(fields_definition_parser.or_not())
      .map(|(((name, implements), directives), fields)| (name, implements, directives, fields))
  }
}

impl<'a, Name, ImplementsInterfaces, Directives, FieldsDefinition, I, T, Error>
  Parseable<'a, I, T, Error>
  for InterfaceTypeDefinition<Name, ImplementsInterfaces, Directives, FieldsDefinition>
where
  Interface: Parseable<'a, I, T, Error>,
  Implements: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  ImplementsInterfaces: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
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
    Self::parser_with(
      Name::parser(),
      ImplementsInterfaces::parser(),
      Directives::parser(),
      FieldsDefinition::parser(),
    )
  }
}

/// Represents the content portion of an interface type extension.
///
/// Interface extensions can add different types of content to existing interfaces:
/// - Directives for metadata and behavioral modifications
/// - Fields to extend the interface contract  
/// - Interface implementations to establish inheritance
///
/// These can be combined in various ways to create comprehensive extensions.
#[derive(Debug, Clone, Copy)]
pub enum InterfaceTypeExtensionData<ImplementsInterfaces, Directives, FieldsDefinition> {
  /// Extension adds directives to an interface, optionally with new interface implementations.
  ///
  /// This variant applies metadata or behavioral modifications without changing
  /// the interface's field contract. Commonly used for authorization, caching,
  /// deprecation, and federation directives.
  ///
  /// ## Examples
  /// ```text
  /// # Add deprecation directive
  /// extend interface Node @deprecated(reason: "Use NodeV2")
  ///
  /// # Add directive with interface implementation
  /// extend interface Node implements Timestamped @auth(required: true)
  ///
  /// # Multiple directives
  /// extend interface Entity @cache(maxAge: 3600) @monitoring(enabled: true)
  /// ```
  Directives {
    /// Optional new interface implementations
    implements: Option<ImplementsInterfaces>,
    /// Directives being added to the interface
    directives: Directives,
  },

  /// Extension adds new fields to an interface, optionally with implementations and directives.
  ///
  /// This is the most comprehensive extension type, adding new contract requirements
  /// that all implementing types must satisfy. Can include interface implementations
  /// and directives alongside the field additions.
  ///
  /// ## Examples
  /// ```text
  /// # Add new required fields
  /// extend interface Node {
  ///   version: Int!
  ///   lastModified: DateTime!
  /// }
  ///
  /// # Add fields with interface implementation
  /// extend interface Entity implements Timestamped {
  ///   createdAt: DateTime!  # Required by Timestamped
  ///   updatedAt: DateTime!  # Required by Timestamped
  ///   metadata: JSON
  /// }
  ///
  /// # Comprehensive extension
  /// extend interface SearchableNode implements Auditable @cache(maxAge: 600) {
  ///   searchTerms: [String!]!
  ///   searchBoost: Float
  ///   auditLog: [AuditEntry!]!  # Required by Auditable
  /// }
  /// ```
  Fields {
    /// Optional new interface implementations
    implements: Option<ImplementsInterfaces>,
    /// Optional directives to apply
    directives: Option<Directives>,
    /// New field definitions being added
    fields: FieldsDefinition,
  },

  /// Extension adds only interface implementations, creating inheritance relationships.
  ///
  /// This variant establishes new "implements" relationships without adding fields
  /// or directives. It's the safest form of extension as it only adds inheritance
  /// without changing existing contracts.
  ///
  /// ## Examples
  /// ```text
  /// # Simple interface inheritance
  /// extend interface MutableNode implements Node
  ///
  /// # Multiple interface inheritance
  /// extend interface ComplexEntity implements Node & Timestamped & Searchable
  /// ```
  Implements(ImplementsInterfaces),
}

impl<ImplementsInterfaces, Directives, FieldsDefinition>
  InterfaceTypeExtensionData<ImplementsInterfaces, Directives, FieldsDefinition>
{
  /// Returns the directives if this extension includes them.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
      Self::Implements { .. } => None,
    }
  }

  /// Returns the interface implementations if this extension includes them.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces> {
    match self {
      Self::Directives { implements, .. } => implements.as_ref(),
      Self::Fields { implements, .. } => implements.as_ref(),
      Self::Implements(implements) => Some(implements),
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

/// Represents a GraphQL Interface type extension.
///
/// Interface extensions add new capabilities to existing interfaces without
/// modifying the original definition, supporting schema evolution.
///
/// ## Examples
/// ```text
/// # Add fields
/// extend interface Node {
///   version: Int!
/// }
///
/// # Add inheritance
/// extend interface MutableNode implements Node
///
/// # Add directives
/// extend interface Node @deprecated(reason: "Use NodeV2")
/// ```
///
/// ## Grammar
/// ```text
/// InterfaceTypeExtension:
///   extend interface Name ImplementsInterfaces? Directives? FieldsDefinition
///   | extend interface Name ImplementsInterfaces? Directives  
///   | extend interface Name ImplementsInterfaces
/// ```
#[derive(Debug, Clone, Copy)]
pub struct InterfaceTypeExtension<Name, ImplementsInterfaces, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  data: InterfaceTypeExtensionData<ImplementsInterfaces, Directives, FieldsDefinition>,
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition> AsSpan<Span>
  for InterfaceTypeExtension<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for InterfaceTypeExtension<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition> IntoComponents
  for InterfaceTypeExtension<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Name,
    InterfaceTypeExtensionData<ImplementsInterfaces, Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.data)
  }
}

impl<Name, ImplementsInterfaces, Directives, FieldsDefinition>
  InterfaceTypeExtension<Name, ImplementsInterfaces, Directives, FieldsDefinition>
{
  /// Creates a new `InterfaceTypeExtension` with the given components.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    data: InterfaceTypeExtensionData<ImplementsInterfaces, Directives, FieldsDefinition>,
  ) -> Self {
    Self { span, name, data }
  }

  /// Returns a reference to the span covering the entire interface extension.
  ///
  /// Includes the `extend interface` keywords, interface name, and all extension data.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the interface being extended.
  ///
  /// This must reference an existing interface defined elsewhere in the schema.
  /// Used for extension resolution and validation.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns directives if this extension includes them.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns interface implementations if this extension includes them.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces> {
    self.data.implements()
  }

  /// Returns fields if this extension includes them.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.data.fields_definition()
  }

  /// Returns a reference to the content being added by this extension.
  ///
  /// The content determines what type of enhancement is being made:
  /// - `Fields`: New field definitions with optional implementations/directives
  /// - `Directives`: Metadata/behavioral modifications with optional implementations
  /// - `Implements`: Interface inheritance relationships only
  #[inline]
  pub const fn data(
    &self,
  ) -> &InterfaceTypeExtensionData<ImplementsInterfaces, Directives, FieldsDefinition> {
    &self.data
  }

  /// Creates a parser for interface type extensions.
  pub fn parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: IP,
    directives_parser: DP,
    fields_definition_parser: FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnexpectedEndOfInterfaceExtensionError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Extend: Parseable<'src, I, T, Error>,
    Interface: Parseable<'src, I, T, Error>,
    Implements: Parseable<'src, I, T, Error>,
    Name: 'src,
    Directives: 'src,
    ImplementsInterfaces: 'src,
    FieldsDefinition: 'src,
    NP: Parser<'src, I, Name, E> + Clone + 'src,
    DP: Parser<'src, I, Directives, E> + Clone + 'src,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone + 'src,
    IP: Parser<'src, I, ImplementsInterfaces, E> + Clone + 'src,
  {
    Extend::parser()
      .then(Interface::parser())
      .ignore_then(Self::content_parser_with(
        name_parser,
        implement_interfaces_parser,
        directives_parser,
        fields_definition_parser,
      ))
      .map_with(|(name, data), exa| Self::new(exa.span(), name, data))
  }

  /// Creates a parser for interface type extensions without the leading `extend` and `interface` keywords.
  pub fn content_parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: IP,
    directives_parser: DP,
    fields_definition_parser: FP,
  ) -> impl Parser<
    'src,
    I,
    (
      Name,
      InterfaceTypeExtensionData<ImplementsInterfaces, Directives, FieldsDefinition>,
    ),
    E,
  > + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnexpectedEndOfInterfaceExtensionError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Implements: Parseable<'src, I, T, Error>,
    Name: 'src,
    Directives: 'src,
    ImplementsInterfaces: 'src,
    FieldsDefinition: 'src,
    NP: Parser<'src, I, Name, E> + Clone + 'src,
    DP: Parser<'src, I, Directives, E> + Clone + 'src,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone + 'src,
    IP: Parser<'src, I, ImplementsInterfaces, E> + Clone + 'src,
  {
    name_parser
      .then(
        Implements::parser()
          .ignore_then(implement_interfaces_parser)
          .or_not(),
      )
      .then(directives_parser.or_not())
      .then(fields_definition_parser.or_not())
      .try_map_with(|(((name, implements), directives), fields), exa| {
        let data = match (implements, directives, fields) {
          (implements, directives, Some(fields)) => InterfaceTypeExtensionData::Fields {
            implements,
            directives,
            fields,
          },
          (implements, Some(directives), None) => InterfaceTypeExtensionData::Directives {
            implements,
            directives,
          },
          (Some(implements), None, None) => InterfaceTypeExtensionData::Implements(implements),
          (None, None, None) => {
            return Err(Error::unexpected_end_of_interface_extension(
              exa.span(),
              InterfaceTypeExtensionHint::ImplementsOrDirectivesOrFieldsDefinition,
            ));
          }
        };

        Ok((name, data))
      })
  }
}

impl<'a, Name, ImplementsInterfaces, Directives, FieldsDefinition, I, T, Error>
  Parseable<'a, I, T, Error>
  for InterfaceTypeExtension<Name, ImplementsInterfaces, Directives, FieldsDefinition>
where
  Error: UnexpectedEndOfInterfaceExtensionError,
  Extend: Parseable<'a, I, T, Error>,
  Interface: Parseable<'a, I, T, Error>,
  Implements: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  ImplementsInterfaces: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
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
    Self::parser_with(
      Name::parser(),
      ImplementsInterfaces::parser(),
      Directives::parser(),
      FieldsDefinition::parser(),
    )
  }
}
