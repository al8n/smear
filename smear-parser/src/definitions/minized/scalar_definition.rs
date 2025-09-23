use chumsky::{extra::ParserExtra, prelude::*};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_utils::{IntoComponents, IntoSpan};

use crate::{
  error::UnexpectedEndOfSchemaExtensionError,
  lang::minized::keywords::{Extend, Scalar},
};

/// Represents a GraphQL scalar type definition that defines custom data types with specific serialization behavior.
///
/// Scalar types represent primitive leaf values in the GraphQL type system and are the fundamental
/// building blocks for all GraphQL schemas. While GraphQL provides several built-in scalar types
/// (String, Int, Float, Boolean, ID), custom scalar definitions allow developers to define
/// domain-specific data types with custom serialization, validation, and parsing logic.
///
/// ## Examples
///
/// ```text
/// # Simple scalar definition
/// scalar DateTime
///
/// # Scalar with description
/// """
/// A date-time string in ISO 8601 format, including timezone.
/// Example: "2023-12-25T10:30:00Z"
/// """
/// scalar DateTime
///
/// # Scalar with validation directive
/// scalar Email @constraint(format: "email")
///
/// # Complex scalar with multiple directives
/// """
/// A monetary value with currency code and precision handling.
/// Supports major world currencies and cryptocurrency.
///
/// Format: { amount: "123.45", currency: "USD" }
/// Precision: Up to 8 decimal places for crypto, 2 for fiat
/// """
/// scalar Money
///   @constraint(precision: 8)
///   @validation(currencies: ["USD", "EUR", "BTC", "ETH"])
///   @serialize(format: "object")
///
/// # Domain-specific scalars
/// """
/// A valid IPv4 or IPv6 address string.
/// Supports CIDR notation for network ranges.
/// """
/// scalar IPAddress @format(type: "ip")
///
/// """
/// A geographic coordinate pair (latitude, longitude).
/// Validates coordinate bounds and precision.
/// """
/// scalar GeoCoordinate
///   @constraint(lat: {min: -90, max: 90}, lng: {min: -180, max: 180})
///   @precision(decimal: 6)
///
/// # JSON scalar for flexible data
/// """
/// Arbitrary JSON data structure.
/// Can represent objects, arrays, or primitive values.
/// Use sparingly - prefer typed fields when structure is known.
/// """
/// scalar JSON @serialize(passthrough: true)
///
/// # Usage in schema
/// type User {
///   id: ID!
///   email: Email!
///   createdAt: DateTime!
///   lastLogin: DateTime
///   profileData: JSON
///   location: GeoCoordinate
/// }
///
/// type Product {
///   id: ID!
///   name: String!
///   price: Money!
///   website: URL
/// }
///
/// input CreateUserInput {
///   email: Email!
///   phone: PhoneNumber
///   birthDate: DateTime
///   preferences: JSON
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the scalar definition
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// ScalarTypeDefinition : Description? scalar Name Directives?
/// ```
///
/// Spec: [Scalar Type Definition](https://spec.graphql.org/draft/#sec-Scalar-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct ScalarTypeDefinition<Name, Directives> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
}

impl<Name, Directives> AsRef<Span> for ScalarTypeDefinition<Name, Directives> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives> IntoSpan<Span> for ScalarTypeDefinition<Name, Directives> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives> IntoComponents for ScalarTypeDefinition<Name, Directives> {
  type Components = (Span, Name, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.directives)
  }
}

impl<Name, Directives> ScalarTypeDefinition<Name, Directives> {
  /// Returns a reference to the span covering the entire scalar definition.
  ///
  /// The span encompasses the complete scalar definition from the optional description
  /// through the scalar name and any directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the scalar definition.
  ///
  /// The scalar name serves as the type identifier and must be unique within
  /// the schema's namespace. Scalar names follow GraphQL naming conventions
  /// and become part of the schema's public API contract.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the optional directives applied to this scalar definition.
  ///
  /// Scalar-level directives provide metadata and specify behavior for the custom scalar,
  /// particularly around validation, serialization, and processing rules. They enable
  /// declarative specification of scalar behavior without requiring code changes.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Creates a parser for scalar definitions using the provided directives parser.
  ///
  /// This parser handles the complete syntax for GraphQL scalar type definitions,
  /// supporting optional descriptions and directives while ensuring proper whitespace
  /// and comment handling throughout the definition.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the scalar type definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, NP, DP>(
    name_parser: NP,
    directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    Scalar: Parseable<'src, I, T, Error>,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    Scalar::parser()
      .ignore_then(name_parser.then(directives_parser.or_not()))
      .map_with(|(name, directives), exa| Self {
        span: exa.span(),
        name,
        directives,
      })
  }
}

impl<'a, Name, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for ScalarTypeDefinition<Name, Directives>
where
  Scalar: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser(), Directives::parser())
  }
}

/// Represents a GraphQL scalar type extension that adds new directives to an existing scalar.
///
/// Scalar extensions provide a mechanism to enhance existing scalar types with additional
/// metadata, validation rules, or behavioral modifications without redefining the scalar
/// entirely. This is particularly valuable for:
///
/// - **Modular Schema Evolution**: Adding functionality to scalars incrementally
/// - **Environment-Specific Behavior**: Different validation rules across environments
/// - **Third-Party Enhancements**: External libraries adding capabilities to core scalars
/// - **Federation and Composition**: Distributed teams extending shared scalar types
/// - **Feature Flags**: Conditionally applying scalar enhancements
/// - **Backward-Compatible Changes**: Adding new constraints without breaking existing code
///
/// ## Scalar Extension Philosophy
///
/// Extensions embody GraphQL's commitment to schema evolution:
/// - **Additive Only**: Extensions can only add directives, never remove or modify existing behavior
/// - **Composable**: Multiple extensions can be applied to the same scalar
/// - **Non-Breaking**: Base scalar functionality remains unchanged
/// - **Declarative**: Behavior changes are expressed through directives
/// - **Tooling Friendly**: Extensions can be analyzed and validated by schema tools
/// - **Environment Aware**: Different extensions for different deployment contexts
///
/// ## Examples
///
/// ```text
/// # Simple directive addition
/// extend scalar DateTime @timezone(default: "UTC")
///
/// # Multiple directive extension
/// extend scalar Email
///   @constraint(maxLength: 254)
///   @validation(checkMx: true)
///   @security(maskInLogs: true)
///
/// # Complex validation extension
/// extend scalar PhoneNumber
///   @constraint(format: "international", allowExtensions: true)
///   @geolocation(requireCountryCode: true)
///   @carrier(validation: "realtime", allowVoip: false)
///   @privacy(encrypt: true, auditAccess: true)
///
/// # Performance and monitoring extension
/// extend scalar DatabaseId
///   @cache(strategy: "write-through", ttl: 300)
///   @monitoring(track: ["access-frequency", "validation-time"])
///   @optimization(index: "btree", compression: "delta")
///
/// # Feature flag extension
/// extend scalar ExperimentalType
///   @featureFlag(name: "new-validation", defaultEnabled: false)
///   @rollout(percentage: 10, cohort: "beta-users")
///   @monitoring(experiment: "validation-improvement")
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives being added to the scalar (required)
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// ScalarTypeExtension : extend scalar Name Directives
/// ```
///
/// Note: Unlike scalar definitions, extensions require directives (they must add something).
///
/// Spec: [Scalar Type Extension](https://spec.graphql.org/draft/#sec-Scalar-Type-Extension)
#[derive(Debug, Clone, Copy)]
pub struct ScalarTypeExtension<Name, Directives> {
  span: Span,
  name: Name,
  directives: Directives,
}

impl<Name, Directives> AsRef<Span> for ScalarTypeExtension<Name, Directives> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives> IntoSpan<Span> for ScalarTypeExtension<Name, Directives> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives> IntoComponents for ScalarTypeExtension<Name, Directives> {
  type Components = (Span, Name, Directives);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.directives)
  }
}

impl<Name, Directives> ScalarTypeExtension<Name, Directives> {
  /// Returns a reference to the span covering the entire scalar extension.
  ///
  /// The span encompasses the complete extension from the `extend` keyword through
  /// the scalar name and all applied directives. This comprehensive location
  /// information enables precise error reporting, IDE integration, and source
  /// transformation tools.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the scalar being extended.
  ///
  /// The scalar name identifies which existing scalar type this extension applies to.
  /// The referenced scalar must be defined elsewhere in the schema (either in the
  /// base schema or in previously applied extensions).
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the directives being added to the scalar.
  ///
  /// Unlike scalar definitions where directives are optional, scalar extensions
  /// require directives because the extension must add something to the existing scalar.
  /// These directives enhance or modify the behavior of the base scalar type.
  #[inline]
  pub const fn directives(&self) -> &Directives {
    &self.directives
  }

  /// Creates a parser for scalar extensions using the provided directives parser.
  ///
  /// This parser handles the complete syntax for GraphQL scalar type extensions,
  /// which must include directives (unlike definitions where they are optional).
  /// The parser ensures proper keyword recognition, name validation, and directive
  /// processing while maintaining comprehensive error reporting capabilities.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the scalar type extension.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  #[inline]
  pub fn parser_with<'src, I, T, Error, E, NP, DP>(
    name_parser: NP,
    directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Extend: Parseable<'src, I, T, Error>,
    Scalar: Parseable<'src, I, T, Error>,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    Extend::parser()
      .then(Scalar::parser())
      .ignore_then(name_parser)
      .then(directives_parser)
      .map_with(|(name, directives), exa| Self {
        span: exa.span(),
        name,
        directives,
      })
  }
}

impl<'a, Name, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for ScalarTypeExtension<Name, Directives>
where
  Extend: Parseable<'a, I, T, Error>,
  Scalar: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser(), Directives::parser())
  }
}
