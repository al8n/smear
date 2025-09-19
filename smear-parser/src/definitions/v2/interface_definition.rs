use chumsky::{extra::ParserExtra, prelude::*};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_utils::{IntoComponents, IntoSpan};

use crate::lang::{
  keywords2::{Extend, Implements, Interface},
  punctuator::Ampersand,
};

/// Represents the first interface in an `implements` list, where the ampersand is optional.
///
/// In GraphQL's interface implementation syntax, the first interface name can optionally
/// be preceded by an ampersand (`&`), but subsequent interfaces must have ampersands.
/// This distinction exists for backwards compatibility and readability.
///
/// ## Syntax Variations
/// ```text
/// # Both syntaxes are valid for the first interface:
/// type User implements Node { ... }           # No ampersand
/// type User implements & Node { ... }         # With ampersand
///
/// # Multiple interfaces - first can be with or without ampersand:
/// type User implements Node & Timestamped     # First without &
/// type User implements & Node & Timestamped   # First with &
/// ```
///
/// The optional ampersand design allows for consistent syntax when adding/removing
/// interface implementations while maintaining backwards compatibility with existing schemas.
///
/// ## Grammar
/// ```text
/// LeadingImplementInterface:
///   &? Name
/// ```
#[derive(Debug, Clone, Copy)]
pub struct LeadingImplementInterface<Name> {
  span: Span,
  amp: Option<Ampersand>,
  name: Name,
}

impl<Name> AsRef<Span> for LeadingImplementInterface<Name> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for LeadingImplementInterface<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for LeadingImplementInterface<Name> {
  type Components = (Span, Option<Ampersand>, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.amp, self.name)
  }
}

impl<Name> LeadingImplementInterface<Name> {
  /// Returns a reference to the span covering the leading interface implementation.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional ampersand token.
  ///
  /// The ampersand is optional for the first interface in an implementation list,
  /// allowing both `implements Node` and `implements & Node` syntax.
  #[inline]
  pub const fn ampersand(&self) -> Option<&Ampersand> {
    self.amp.as_ref()
  }

  /// Returns a reference to the interface name being implemented.
  ///
  /// This must be the name of an interface type defined elsewhere in the schema.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser for the first interface in an implementation list.
  ///
  /// This parser handles the optional ampersand syntax specific to the leading
  /// interface, accepting both `Name` and `& Name` patterns.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the leading implement interface.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, T, Error, E, NP>(
    name_parser: NP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Ampersand: Parseable<'src, I, T, Error>,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    Ampersand::parser()
      .or_not()
      .then(name_parser)
      .map_with(|(amp, name), exa| Self {
        span: exa.span(),
        amp,
        name,
      })
  }
}

impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for LeadingImplementInterface<Name>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Ampersand: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Name::parser())
  }
}

/// Represents a subsequent interface in an `implements` list, where the ampersand is required.
///
/// After the first interface in an implementation list, all subsequent interfaces must
/// be preceded by an ampersand (`&`) to clearly separate them and maintain unambiguous parsing.
///
/// ## Examples
///
/// ```text
/// # Multiple interfaces - all after first require ampersands:
/// type User implements Node & Timestamped & Searchable {
///   # Node is leading (& optional)
///   # Timestamped is subsequent (& required)  
///   # Searchable is subsequent (& required)
/// }
/// ```
///
/// ## Grammar
///
/// ```text
/// ImplementInterface: '&' Name
/// ```
#[derive(Debug, Clone, Copy)]
pub struct ImplementInterface<Name> {
  span: Span,
  amp: Ampersand,
  name: Name,
}

impl<Name> AsRef<Span> for ImplementInterface<Name> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for ImplementInterface<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for ImplementInterface<Name> {
  type Components = (Span, Ampersand, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.amp, self.name)
  }
}

impl<Name> ImplementInterface<Name> {
  /// Returns a reference to the span covering this interface implementation.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the required ampersand token.
  ///
  /// Unlike `LeadingImplementInterface`, the ampersand is always required for
  /// subsequent interfaces in an implementation list.
  #[inline]
  pub const fn ampersand(&self) -> &Ampersand {
    &self.amp
  }

  /// Returns a reference to the interface name being implemented.
  ///
  /// This must be the name of an interface type defined elsewhere in the schema.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser for subsequent interfaces in an implementation list.
  ///
  /// This parser requires an ampersand followed by the interface name,
  /// enforcing the mandatory ampersand syntax for non-leading interfaces.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the implement interface.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, T, Error, E, NP>(
    name_parser: NP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    E: ParserExtra<'src, I>,
    Ampersand: Parseable<'src, I, T, Error>,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    Ampersand::parser()
      .then(name_parser)
      .map_with(|(amp, name), exa| Self {
        span: exa.span(),
        amp,
        name,
      })
  }
}

impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for ImplementInterface<Name>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Ampersand: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Name::parser())
  }
}

/// Represents a complete interface implementation list with proper ampersand handling.
///
/// This structure correctly parses GraphQL's interface implementation syntax, which
/// has specific rules about when ampersands are required vs optional. The distinction
/// between leading and subsequent interfaces ensures correct parsing of complex
/// interface hierarchies.
///
/// ## Ampersand Rules Explained
///
/// GraphQL uses a specific ampersand syntax for interface implementations:
/// - **First Interface**: Ampersand is optional (`Node` or `& Node`)
/// - **Subsequent Interfaces**: Ampersand is required (`& Timestamped & Searchable`)
///
/// ## Examples
/// ```text
/// # Simple single interface
/// type User implements Node { id: ID! }
///
/// # Multiple interfaces with various syntax
/// type User implements Node & Timestamped {
///   id: ID!
///   createdAt: DateTime!
///   updatedAt: DateTime!
/// }
///
/// # Complex interface hierarchy
/// type AdminUser implements
///   & Node
///   & Timestamped
///   & Auditable
///   & Permissioned
/// {
///   id: ID!
///   createdAt: DateTime!
///   updatedAt: DateTime!
///   auditLog: [AuditEntry!]!
///   permissions: [Permission!]!
/// }
/// ```
///
/// ## Grammar
///
/// ```text
/// ImplementsInterfaces:
///   implements LeadingImplementInterface ImplementInterface*
///
/// LeadingImplementInterface:
///   &? Name
///
/// ImplementInterface:
///   & Name
/// ```
#[derive(Debug, Clone, Copy)]
pub struct ImplementInterfaces<Name, Container = Vec<ImplementInterface<Name>>> {
  span: Span,
  implements: Implements,
  leading: LeadingImplementInterface<Name>,
  remaining: Container,
}

impl<Name, Container> AsRef<Span> for ImplementInterfaces<Name, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Container> IntoSpan<Span> for ImplementInterfaces<Name, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Container> IntoComponents for ImplementInterfaces<Name, Container> {
  type Components = (Span, Implements, LeadingImplementInterface<Name>, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.implements, self.leading, self.remaining)
  }
}

impl<Name, Container> ImplementInterfaces<Name, Container> {
  /// Returns a reference to the span covering the entire implements clause.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `implements` keyword token.
  ///
  /// This provides access to the exact `implements` keyword and its location,
  /// useful for error reporting and syntax highlighting.
  #[inline]
  pub const fn implements_keyword(&self) -> &Implements {
    &self.implements
  }

  /// Returns a reference to the first interface in the implementation list.
  ///
  /// The leading interface has special parsing rules where the ampersand is optional,
  /// unlike subsequent interfaces where it's required.
  #[inline]
  pub const fn leading_implement_interface(&self) -> &LeadingImplementInterface<Name> {
    &self.leading
  }

  /// Returns a reference to the container holding subsequent interfaces.
  ///
  /// All interfaces in this container have required ampersands and represent
  /// the additional interfaces beyond the first one.
  #[inline]
  pub const fn remaining_implement_interfaces(&self) -> &Container {
    &self.remaining
  }

  /// Creates a parser for complete interface implementation lists.
  ///
  /// This parser correctly handles the ampersand rules, parsing the first interface
  /// with optional ampersand, followed by zero or more subsequent interfaces
  /// with required ampersands.
  pub fn parser_with<'src, I, T, Error, E, NP>(
    name_parser: impl Fn() -> NP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Implements: Parseable<'src, I, T, Error>,
    Ampersand: Parseable<'src, I, T, Error>,
    NP: Parser<'src, I, Name, E> + Clone,
    Container: chumsky::container::Container<ImplementInterface<Name>>,
  {
    Implements::parser()
      .then(LeadingImplementInterface::parser_with(name_parser()))
      .then(
        ImplementInterface::parser_with(name_parser())
          .repeated()
          .collect(),
      )
      .map_with(|((implements, leading), remaining), exa| Self {
        span: exa.span(),
        implements,
        leading,
        remaining,
      })
  }
}

impl<'a, Name, Container, I, T, Error> Parseable<'a, I, T, Error>
  for ImplementInterfaces<Name, Container>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Implements: Parseable<'a, I, T, Error>,
  Ampersand: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<ImplementInterface<Name>>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Name::parser)
  }
}

/// Represents the content of a GraphQL Interface type definition that defines a contract for implementing types.
///
/// The difference between this and [`InterfaceTypeDefinition`] is that this struct does not include
/// the description and `interface` keyword. This allows for more modular parsing and composition
/// of interface definitions in different contexts.
///
/// ## Grammar
///
/// ```text
/// InterfaceTypeDefinitionContent:
///   Name ImplementsInterfaces? Directives? FieldsDefinition?
/// ```
///
/// Spec: [Interface Type Definition](https://spec.graphql.org/draft/#sec-Interface-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct InterfaceTypeDefinitionContent<Name, ImplementInterfaces, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> AsRef<Span>
  for InterfaceTypeDefinitionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for InterfaceTypeDefinitionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoComponents
  for InterfaceTypeDefinitionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
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
  InterfaceTypeDefinitionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
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
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
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
  ///
  /// This parser handles the complete syntax for GraphQL interfaces, including
  /// interface inheritance through the implements clause.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the interface type definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: IP,
    directives_parser: DP,
    fields_definition_parser: FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    name_parser
      .then(implement_interfaces_parser.or_not())
      .then(directives_parser.or_not())
      .then(fields_definition_parser.or_not())
      .map_with(|(((name, implements), directives), fields), exa| Self {
        span: exa.span(),
        name,
        directives,
        fields_definition: fields,
        implements,
      })
  }
}

impl<'a, Name, ImplementInterfaces, Directives, FieldsDefinition, I, T, Error>
  Parseable<'a, I, T, Error>
  for InterfaceTypeDefinitionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Name: Parseable<'a, I, T, Error>,
  ImplementInterfaces: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      Name::parser(),
      ImplementInterfaces::parser(),
      Directives::parser(),
      FieldsDefinition::parser(),
    )
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
pub struct InterfaceTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition> {
  span: Span,
  interface: Interface,
  name: Name,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> AsRef<Span>
  for InterfaceTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for InterfaceTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoComponents
  for InterfaceTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Interface,
    Name,
    Option<ImplementInterfaces>,
    Option<Directives>,
    Option<FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.interface,
      self.name,
      self.implements,
      self.directives,
      self.fields_definition,
    )
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition>
  InterfaceTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  /// Returns a reference to the span covering the entire interface definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `interface` keyword token.
  #[inline]
  pub const fn interface_keyword(&self) -> &Interface {
    &self.interface
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
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
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
  ///
  /// This parser handles the complete syntax for GraphQL interfaces, including
  /// interface inheritance through the implements clause.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the interface type definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: IP,
    directives_parser: DP,
    fields_definition_parser: FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Interface: Parseable<'src, I, T, Error>,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    Interface::parser()
      .then(InterfaceTypeDefinitionContent::<
        Name,
        ImplementInterfaces,
        Directives,
        FieldsDefinition,
      >::parser_with(
        name_parser,
        implement_interfaces_parser,
        directives_parser,
        fields_definition_parser,
      ))
      .map_with(|(interface, content), exa| {
        let (_, name, implements, directives, fields) = content.into_components();
        Self {
          span: exa.span(),
          name,
          directives,
          fields_definition: fields,
          interface,
          implements,
        }
      })
  }
}

impl<'a, Name, ImplementInterfaces, Directives, FieldsDefinition, I, T, Error>
  Parseable<'a, I, T, Error>
  for InterfaceTypeDefinition<Name, ImplementInterfaces, Directives, FieldsDefinition>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Interface: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  ImplementInterfaces: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      Name::parser(),
      ImplementInterfaces::parser(),
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
pub enum InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition> {
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
    implements: Option<ImplementInterfaces>,
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
    implements: Option<ImplementInterfaces>,
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
  Implements(ImplementInterfaces),
}

impl<ImplementInterfaces, Directives, FieldsDefinition>
  InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>
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
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
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

  /// Creates a parser for interface extension content with proper precedence handling.
  ///
  /// The parser tries patterns in a specific order to resolve parsing ambiguity:
  /// 1. **Fields Pattern**: `implements? directives? fields` (most comprehensive)
  /// 2. **Directives Pattern**: `implements? directives` (metadata only)  
  /// 3. **Implements Pattern**: `implements` (inheritance only)
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the interface type extension data.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, T, Error, E, IP, DP, FP>(
    implement_interfaces_parser: impl Fn() -> IP,
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
  {
    choice((
      implement_interfaces_parser()
        .or_not()
        .then(directives_parser().or_not())
        .then(fields_definition_parser())
        .map(|((implements, directives), fields)| Self::Fields {
          implements,
          directives,
          fields,
        }),
      implement_interfaces_parser()
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

impl<'a, ImplementInterfaces, Directives, FieldsDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  ImplementInterfaces: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      ImplementInterfaces::parser,
      Directives::parser,
      FieldsDefinition::parser,
    )
  }
}

/// Represents content of a GraphQL Interface type extension.
///
/// The difference between this and [`InterfaceTypeExtension`] is that this struct does not include
/// the `extend` and `interface` keywords. This allows for more modular
/// parsing and composition when building up full type extensions.
///
/// ## Grammar
/// ```text
/// InterfaceTypeExtensionContent:
///   Name ImplementsInterfaces? Directives? FieldsDefinition
///   | Name ImplementsInterfaces? Directives  
///   | Name ImplementsInterfaces
/// ```
#[derive(Debug, Clone, Copy)]
pub struct InterfaceTypeExtensionContent<Name, ImplementInterfaces, Directives, FieldsDefinition> {
  span: Span,
  name: Name,
  data: InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> AsRef<Span>
  for InterfaceTypeExtensionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for InterfaceTypeExtensionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoComponents
  for InterfaceTypeExtensionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Name,
    InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.data)
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition>
  InterfaceTypeExtensionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
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
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
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
  ) -> &InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition> {
    &self.data
  }

  /// Creates a parser for interface type extensions.
  ///
  /// This parser handles the complete `extend interface` syntax, parsing the keywords,
  /// interface name, and delegating content parsing to the extension content parser.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the interface type extension.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: impl Fn() -> IP,
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    name_parser
      .then(InterfaceTypeExtensionData::parser_with(
        implement_interfaces_parser,
        directives_parser,
        fields_definition_parser,
      ))
      .map_with(|(name, data), exa| Self {
        span: exa.span(),
        name,
        data,
      })
  }
}

impl<'a, Name, ImplementInterfaces, Directives, FieldsDefinition, I, T, Error>
  Parseable<'a, I, T, Error>
  for InterfaceTypeExtensionContent<Name, ImplementInterfaces, Directives, FieldsDefinition>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Name: Parseable<'a, I, T, Error>,
  ImplementInterfaces: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      Name::parser(),
      ImplementInterfaces::parser,
      Directives::parser,
      FieldsDefinition::parser,
    )
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
pub struct InterfaceTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition> {
  span: Span,
  extend: Extend,
  interface: Interface,
  name: Name,
  data: InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> AsRef<Span>
  for InterfaceTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoSpan<Span>
  for InterfaceTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition> IntoComponents
  for InterfaceTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  type Components = (
    Span,
    Extend,
    Interface,
    Name,
    InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.extend, self.interface, self.name, self.data)
  }
}

impl<Name, ImplementInterfaces, Directives, FieldsDefinition>
  InterfaceTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
{
  /// Returns a reference to the span covering the entire interface extension.
  ///
  /// Includes the `extend interface` keywords, interface name, and all extension data.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `extend` keyword token.
  ///
  /// Useful for distinguishing extensions from definitions in error messages
  /// and source location reporting.
  #[inline]
  pub const fn extend_keyword(&self) -> &Extend {
    &self.extend
  }

  /// Returns a reference to the `interface` keyword token.
  ///
  /// Helps distinguish interface extensions from other extension types
  /// (object, scalar, enum, etc.) in tooling and analysis.
  #[inline]
  pub const fn interface_keyword(&self) -> &Interface {
    &self.interface
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
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
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
  ) -> &InterfaceTypeExtensionData<ImplementInterfaces, Directives, FieldsDefinition> {
    &self.data
  }

  /// Creates a parser for interface type extensions.
  ///
  /// This parser handles the complete `extend interface` syntax, parsing the keywords,
  /// interface name, and delegating content parsing to the extension content parser.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the interface type extension.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, T, Error, E, NP, IP, DP, FP>(
    name_parser: NP,
    implement_interfaces_parser: impl Fn() -> IP,
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Extend: Parseable<'src, I, T, Error>,
    Interface: Parseable<'src, I, T, Error>,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    FP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    Extend::parser()
      .then(Interface::parser())
      .then(InterfaceTypeExtensionContent::<
        Name,
        ImplementInterfaces,
        Directives,
        FieldsDefinition,
      >::parser_with(
        name_parser,
        implement_interfaces_parser,
        directives_parser,
        fields_definition_parser,
      ))
      .map_with(|((extend, interface), content), exa| {
        let (_, name, data) = content.into_components();

        Self {
          span: exa.span(),
          extend,
          interface,
          name,
          data,
        }
      })
  }
}

impl<'a, Name, ImplementInterfaces, Directives, FieldsDefinition, I, T, Error>
  Parseable<'a, I, T, Error>
  for InterfaceTypeExtension<Name, ImplementInterfaces, Directives, FieldsDefinition>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Extend: Parseable<'a, I, T, Error>,
  Interface: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  ImplementInterfaces: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  FieldsDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(
      Name::parser(),
      ImplementInterfaces::parser,
      Directives::parser,
      FieldsDefinition::parser,
    )
  }
}
