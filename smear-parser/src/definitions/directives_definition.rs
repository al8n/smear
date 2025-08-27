use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  lang::{
    ignored, keywords,
    punct::{At, Pipe},
    StringValue,
  },
  source::*,
};

word!(
  /// `QUERY` location - directives can be applied to query operations.
  ///
  /// Used when defining where a directive can be placed. Query directives
  /// affect the entire query operation and can be used for things like
  /// authentication, caching, or operation-level configuration.
  ///
  /// # Examples
  /// ```text
  /// directive @auth on QUERY
  /// directive @cache(ttl: 300) on QUERY
  /// ```
  "query location": QueryLocation: [I::Token::Q, I::Token::U, I::Token::E, I::Token::R, I::Token::Y],
  /// `MUTATION` location - directives can be applied to mutation operations.
  ///
  /// Mutation directives affect the entire mutation operation and can be used
  /// for authorization, rate limiting, or transaction control.
  ///
  /// # Examples
  /// ```text
  /// directive @rateLimit(max: 10) on MUTATION
  /// directive @requireAuth on MUTATION
  /// ```
  "mutation location": MutationLocation: [I::Token::M, I::Token::U, I::Token::T, I::Token::A, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `SUBSCRIPTION` location - directives can be applied to subscription operations.
  ///
  /// Subscription directives control real-time data flow and can be used
  /// for filtering, authentication, or subscription management.
  ///
  /// # Examples
  /// ```text
  /// directive @requireSubscription on SUBSCRIPTION
  /// directive @throttle(rate: "1/sec") on SUBSCRIPTION
  /// ```
  "subscription location": SubscriptionLocation: [I::Token::S, I::Token::U, I::Token::B, I::Token::S, I::Token::C, I::Token::R, I::Token::I, I::Token::P, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `FIELD_DEFINITION` location - directives can be applied to field definitions in schemas.
  ///
  /// Field definition directives control field behavior, validation, authorization,
  /// or provide metadata about fields in type definitions.
  ///
  /// # Examples
  /// ```text
  /// directive @deprecated(reason: String) on FIELD_DEFINITION
  /// directive @auth(requires: Role) on FIELD_DEFINITION
  /// ```
  "field definition location": FieldDefinitionLocation: [I::Token::F, I::Token::I, I::Token::E, I::Token::L, I::Token::D, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::A, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `FIELD` location - directives can be applied to field selections in queries.
  ///
  /// Field directives control individual field selection behavior, commonly
  /// used for conditional inclusion, skipping, or field-level configuration.
  ///
  /// # Examples
  /// ```text
  /// directive @include(if: Boolean!) on FIELD
  /// directive @skip(if: Boolean!) on FIELD
  /// ```
  "field location": FieldLocation: [I::Token::F, I::Token::I, I::Token::E, I::Token::L, I::Token::D],
  /// `FRAGMENT_DEFINITION` location - directives can be applied to named fragment definitions.
  ///
  /// Fragment definition directives control fragment behavior and can be used
  /// for conditional fragments, caching, or fragment-level metadata.
  ///
  /// # Examples
  /// ```text
  /// directive @experimental on FRAGMENT_DEFINITION
  /// directive @cache(scope: PRIVATE) on FRAGMENT_DEFINITION
  /// ```
  "fragment definition location": FragmentDefinitionLocation: [I::Token::F, I::Token::R, I::Token::A, I::Token::G, I::Token::M, I::Token::E, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `FRAGMENT_SPREAD` location - directives can be applied to fragment spreads.
  ///
  /// Fragment spread directives control when and how fragments are included
  /// in selection sets, commonly used for conditional fragment inclusion.
  ///
  /// # Examples
  /// ```text
  /// directive @include(if: Boolean!) on FRAGMENT_SPREAD
  /// directive @defer(label: String) on FRAGMENT_SPREAD
  /// ```
  "fragment spread location": FragmentSpreadLocation: [I::Token::F, I::Token::R, I::Token::A, I::Token::G, I::Token::M, I::Token::E, I::Token::UNDERSCORE, I::Token::S, I::Token::P, I::Token::R, I::Token::E, I::Token::A, I::Token::D],
  /// `INLINE_FRAGMENT` location - directives can be applied to inline fragments.
  ///
  /// Inline fragment directives control conditional type-specific field selections
  /// and can be used for conditional inclusion based on type or other criteria.
  ///
  /// # Examples
  /// ```text
  /// directive @include(if: Boolean!) on INLINE_FRAGMENT
  /// directive @skip(if: Boolean!) on INLINE_FRAGMENT
  /// ```
  "inline fragement location": InlineFragmentLocation: [I::Token::I, I::Token::N, I::Token::L, I::Token::I, I::Token::N, I::Token::E, I::Token::UNDERSCORE, I::Token::F, I::Token::R, I::Token::A, I::Token::G, I::Token::M, I::Token::E],
  /// `VARIABLE_DEFINITION` location - directives can be applied to variable definitions.
  ///
  /// Variable definition directives control variable behavior, validation,
  /// or provide metadata about operation variables.
  ///
  /// # Examples
  /// ```text
  /// directive @deprecated(reason: String) on VARIABLE_DEFINITION
  /// directive @validate(pattern: String) on VARIABLE_DEFINITION
  /// ```
  "variable definition location": VariableDefinitionLocation: [I::Token::V, I::Token::A, I::Token::R, I::Token::I, I::Token::A, I::Token::B, I::Token::L, I::Token::E, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `SCHEMA` location - directives can be applied to the schema definition.
  ///
  /// Schema directives provide global schema-level configuration, metadata,
  /// or behavior that applies to the entire GraphQL schema.
  ///
  /// # Examples
  /// ```text
  /// directive @link(url: String!) on SCHEMA
  /// directive @composeDirective(name: String!) on SCHEMA
  /// ```
  "schema location": SchemaLocation: [I::Token::S, I::Token::C, I::Token::H, I::Token::E, I::Token::M, I::Token::A],
  /// `SCALAR` location - directives can be applied to scalar type definitions.
  ///
  /// Scalar directives provide validation, serialization, or metadata
  /// for custom scalar types in the schema.
  ///
  /// # Examples
  /// ```text
  /// directive @specifiedBy(url: String!) on SCALAR
  /// directive @validate(regex: String) on SCALAR
  /// ```
  "scalar location": ScalarLocation: [I::Token::S, I::Token::C, I::Token::A, I::Token::L, I::Token::A, I::Token::R],
  /// `OBJECT` location - directives can be applied to object type definitions.
  ///
  /// Object type directives control object behavior, provide metadata,
  /// or enable features like interfaces, caching, or authorization.
  ///
  /// # Examples
  /// ```text
  /// directive @key(fields: String!) on OBJECT
  /// directive @cacheControl(maxAge: Int) on OBJECT
  /// ```
  "object location": ObjectLocation: [I::Token::O, I::Token::B, I::Token::J, I::Token::E, I::Token::C, I::Token::T],
  /// `ARGUMENT_DEFINITION` location - directives can be applied to argument definitions.
  ///
  /// Argument definition directives control argument validation, transformation,
  /// or provide metadata about field and directive arguments.
  ///
  /// # Examples
  /// ```text
  /// directive @deprecated(reason: String) on ARGUMENT_DEFINITION
  /// directive @constraint(min: Int, max: Int) on ARGUMENT_DEFINITION
  /// ```
  "argument definition location": ArgumentDefinitionLocation: [I::Token::A, I::Token::R, I::Token::G, I::Token::U, I::Token::M, I::Token::E, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `INTERFACE` location - directives can be applied to interface type definitions.
  ///
  /// Interface directives control interface behavior, provide metadata,
  /// or enable features for types that implement the interface.
  ///
  /// # Examples
  /// ```text
  /// directive @key(fields: String!) on INTERFACE
  /// directive @auth(requires: Role) on INTERFACE
  /// ```
  "interface location": InterfaceLocation: [I::Token::I, I::Token::N, I::Token::T, I::Token::E, I::Token::R, I::Token::F, I::Token::A, I::Token::C, I::Token::E],
  /// `UNION` location - directives can be applied to union type definitions.
  ///
  /// Union directives control union behavior, type resolution,
  /// or provide metadata for union types.
  ///
  /// # Examples
  /// ```text
  /// directive @unionMember(type: String!) on UNION
  /// directive @deprecated(reason: String) on UNION
  /// ```
  "union location": UnionLocation: [I::Token::U, I::Token::N, I::Token::I, I::Token::O, I::Token::N],
  /// `ENUM_VALUE` location - directives can be applied to enum value definitions.
  ///
  /// Enum value directives provide metadata, deprecation information,
  /// or control the behavior of specific enum values.
  ///
  /// # Examples
  /// ```text
  /// directive @deprecated(reason: String) on ENUM_VALUE
  /// directive @internal on ENUM_VALUE
  /// ```
  "enum value location": EnumValueLocation: [I::Token::E, I::Token::N, I::Token::U, I::Token::M, I::Token::UNDERSCORE, I::Token::V, I::Token::A, I::Token::L, I::Token::U, I::Token::E],
  /// `ENUM` location - directives can be applied to enum type definitions.
  ///
  /// Enum directives control enum behavior, validation,
  /// or provide metadata for the entire enum type.
  ///
  /// # Examples
  /// ```text
  /// directive @deprecated(reason: String) on ENUM
  /// directive @oneOf on ENUM
  /// ```
  "enum location": EnumLocation: [I::Token::E, I::Token::N, I::Token::U, I::Token::M],
  /// `INPUT_OBJECT` location - directives can be applied to input object type definitions.
  ///
  /// Input object directives control input validation, transformation,
  /// or provide metadata for input types used in arguments.
  ///
  /// # Examples
  /// ```text
  /// directive @oneOf on INPUT_OBJECT
  /// directive @validate(schema: String) on INPUT_OBJECT
  /// ```
  "input object location": InputObjectLocation: [I::Token::I, I::Token::N, I::Token::P, I::Token::U, I::Token::T, I::Token::UNDERSCORE, I::Token::O, I::Token::B, I::Token::J, I::Token::E, I::Token::C, I::Token::T],
  /// `INPUT_FIELD_DEFINITION` location - directives can be applied to input field definitions.
  ///
  /// Input field directives control input field validation, transformation,
  /// or provide metadata for fields within input object types.
  ///
  /// # Examples
  /// ```text
  /// directive @deprecated(reason: String) on INPUT_FIELD_DEFINITION
  /// directive @constraint(min: Int, max: Int) on INPUT_FIELD_DEFINITION
  /// ```
  "input field definition location": InputFieldDefinitionLocation: [I::Token::I, I::Token::N, I::Token::P, I::Token::U, I::Token::T, I::Token::UNDERSCORE, I::Token::F, I::Token::I, I::Token::E, I::Token::L, I::Token::D, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
);

/// Represents locations where directives can be applied during GraphQL execution.
///
/// Executable directive locations specify where directives can be used in GraphQL
/// operations (queries, mutations, subscriptions) and their components. These
/// directives are processed during query execution and can control field selection,
/// fragment inclusion, and operation behavior.
///
/// ## Examples
///
/// ```text
/// # Query operation directive
/// query @auth { user { name } }
///
/// # Field directive
/// { user { name @deprecated email } }
///
/// # Fragment spread directive
/// { ...UserFragment @include(if: $showUser) }
///
/// # Inline fragment directive
/// { ... on User @skip(if: $hideUser) { name } }
///
/// # Variable definition directive
/// query($id: ID! @validate(pattern: "^[0-9]+$")) { user(id: $id) { name } }
/// ```
///
/// Spec: [ExecutableDirectiveLocation](https://spec.graphql.org/draft/#ExecutableDirectiveLocation)
#[derive(
  Debug,
  Clone,
  Copy,
  derive_more::IsVariant,
  derive_more::From,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDirectiveLocation<Span> {
  /// `QUERY` - directive can be applied to query operations
  Query(QueryLocation<Span>),
  /// `MUTATION` - directive can be applied to mutation operations
  Mutation(MutationLocation<Span>),
  /// `SUBSCRIPTION` - directive can be applied to subscription operations
  Subscription(SubscriptionLocation<Span>),
  /// `FIELD` - directive can be applied to field selections
  Field(FieldLocation<Span>),
  /// `FRAGMENT_DEFINITION` - directive can be applied to fragment definitions
  FragmentDefinition(FragmentDefinitionLocation<Span>),
  /// `FRAGMENT_SPREAD` - directive can be applied to fragment spreads
  FragmentSpread(FragmentSpreadLocation<Span>),
  /// `INLINE_FRAGMENT` - directive can be applied to inline fragments
  InlineFragment(InlineFragmentLocation<Span>),
  /// `VARIABLE_DEFINITION` - directive can be applied to variable definitions
  VariableDefinition(VariableDefinitionLocation<Span>),
}

impl<Span> ExecutableDirectiveLocation<Span> {
  /// Returns a reference to the span covering the directive location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Query(loc) => loc.span(),
      Self::Mutation(loc) => loc.span(),
      Self::Subscription(loc) => loc.span(),
      Self::Field(loc) => loc.span(),
      Self::FragmentDefinition(loc) => loc.span(),
      Self::FragmentSpread(loc) => loc.span(),
      Self::InlineFragment(loc) => loc.span(),
      Self::VariableDefinition(loc) => loc.span(),
    }
  }

  /// Creates a parser that can parse any executable directive location.
  ///
  /// The parser tries each location type in order and returns the first successful match.
  /// All executable directive locations are keywords that must be matched exactly.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the executable directive location.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
  {
    choice((
      QueryLocation::parser().map(Self::Query),
      MutationLocation::parser().map(Self::Mutation),
      SubscriptionLocation::parser().map(Self::Subscription),
      FieldLocation::parser().map(Self::Field),
      FragmentDefinitionLocation::parser().map(Self::FragmentDefinition),
      FragmentSpreadLocation::parser().map(Self::FragmentSpread),
      InlineFragmentLocation::parser().map(Self::InlineFragment),
      VariableDefinitionLocation::parser().map(Self::VariableDefinition),
    ))
  }
}

/// Represents locations where directives can be applied in GraphQL schema definitions.
///
/// Type system directive locations specify where directives can be used in GraphQL
/// schemas to provide metadata, validation, transformation, or other behaviors
/// for types, fields, and other schema elements.
///
/// ## Examples
///
/// ```text
/// # Schema directive
/// schema @link(url: "https://specs.apollo.dev/federation/v2.0") { query: Query }
///
/// # Object type directive
/// type User @key(fields: "id") { id: ID! name: String }
///
/// # Field definition directive
/// type User { name: String @deprecated(reason: "Use fullName") }
///
/// # Scalar directive
/// scalar DateTime @specifiedBy(url: "https://scalars.graphql.org/andimarek/datetime")
///
/// # Enum value directive
/// enum Role { USER @deprecated ADMIN MODERATOR }
/// ```
///
/// Spec: [TypeSystemDirectiveLocation](https://spec.graphql.org/draft/#TypeSystemDirectiveLocation)
#[derive(
  Debug,
  Clone,
  Copy,
  derive_more::IsVariant,
  derive_more::From,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDirectiveLocation<Span> {
  /// `SCHEMA` - directive can be applied to schema definitions
  Schema(SchemaLocation<Span>),
  /// `SCALAR` - directive can be applied to scalar type definitions
  Scalar(ScalarLocation<Span>),
  /// `OBJECT` - directive can be applied to object type definitions
  Object(ObjectLocation<Span>),
  /// `FIELD_DEFINITION` - directive can be applied to field definitions
  FieldDefinition(FieldDefinitionLocation<Span>),
  /// `ARGUMENT_DEFINITION` - directive can be applied to argument definitions
  ArgumentDefinition(ArgumentDefinitionLocation<Span>),
  /// `INTERFACE` - directive can be applied to interface type definitions
  Interface(InterfaceLocation<Span>),
  /// `UNION` - directive can be applied to union type definitions
  Union(UnionLocation<Span>),
  /// `ENUM` - directive can be applied to enum type definitions
  Enum(EnumLocation<Span>),
  /// `ENUM_VALUE` - directive can be applied to enum value definitions
  EnumValue(EnumValueLocation<Span>),
  /// `INPUT_OBJECT` - directive can be applied to input object type definitions
  InputObject(InputObjectLocation<Span>),
  /// `INPUT_FIELD_DEFINITION` - directive can be applied to input field definitions
  InputFieldDefinition(InputFieldDefinitionLocation<Span>),
}

impl<Span> TypeSystemDirectiveLocation<Span> {
  /// Returns a reference to the span covering the directive location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Schema(loc) => loc.span(),
      Self::Scalar(loc) => loc.span(),
      Self::Object(loc) => loc.span(),
      Self::FieldDefinition(loc) => loc.span(),
      Self::ArgumentDefinition(loc) => loc.span(),
      Self::Interface(loc) => loc.span(),
      Self::Union(loc) => loc.span(),
      Self::Enum(loc) => loc.span(),
      Self::EnumValue(loc) => loc.span(),
      Self::InputObject(loc) => loc.span(),
      Self::InputFieldDefinition(loc) => loc.span(),
    }
  }

  /// Creates a parser that can parse any type system directive location.
  ///
  /// The parser tries each location type in order and returns the first successful match.
  /// All type system directive locations are keywords that must be matched exactly.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the type system directive location.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
  {
    choice((
      SchemaLocation::parser().map(Self::Schema),
      ScalarLocation::parser().map(Self::Scalar),
      ObjectLocation::parser().map(Self::Object),
      FieldDefinitionLocation::parser().map(Self::FieldDefinition),
      ArgumentDefinitionLocation::parser().map(Self::ArgumentDefinition),
      InterfaceLocation::parser().map(Self::Interface),
      UnionLocation::parser().map(Self::Union),
      EnumLocation::parser().map(Self::Enum),
      EnumValueLocation::parser().map(Self::EnumValue),
      InputObjectLocation::parser().map(Self::InputObject),
      InputFieldDefinitionLocation::parser().map(Self::InputFieldDefinition),
    ))
  }
}

/// Represents any location where a directive can be applied in GraphQL.
///
/// This is the top-level enum that encompasses both executable and type system
/// directive locations. It provides a unified interface for working with all
/// possible directive placement locations in GraphQL documents.
///
/// Spec: [DirectiveLocation](https://spec.graphql.org/draft/#DirectiveLocation)
#[derive(
  Debug,
  Clone,
  Copy,
  derive_more::IsVariant,
  derive_more::From,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Location<Span> {
  /// Executable directive location
  Executable(ExecutableDirectiveLocation<Span>),
  /// Type system directive location
  TypeSystem(TypeSystemDirectiveLocation<Span>),
}

impl<Span> Location<Span> {
  /// Returns a reference to the span covering the directive location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Executable(loc) => loc.span(),
      Self::TypeSystem(loc) => loc.span(),
    }
  }

  /// Creates a parser that can parse any directive location.
  ///
  /// The parser tries both executable and type system directive locations,
  /// returning the first successful match.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the location.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
  {
    choice((
      ExecutableDirectiveLocation::parser().map(Self::Executable),
      TypeSystemDirectiveLocation::parser().map(Self::TypeSystem),
    ))
  }
}

macro_rules! from_location {
  ($($variant:ident: [$($sub_variant:ident),+$(,)?]), +$(,)?) => {
    $(
      $(
        impl<Span> From<$sub_variant<Span>> for Location<Span> {
          fn from(location: $sub_variant<Span>) -> Self {
            Self::$variant(location.into())
          }
        }
      )*
    )*
  };
}

from_location!(
  Executable: [
    QueryLocation,
    MutationLocation,
    SubscriptionLocation,
    FieldLocation,
    FragmentDefinitionLocation,
    FragmentSpreadLocation,
    InlineFragmentLocation,
    VariableDefinitionLocation
  ],
  TypeSystem: [
    SchemaLocation,
    ScalarLocation,
    ObjectLocation,
    FieldDefinitionLocation,
    ArgumentDefinitionLocation,
    InterfaceLocation,
    UnionLocation,
    EnumLocation,
    EnumValueLocation,
    InputObjectLocation,
    InputFieldDefinitionLocation,
  ],
);

/// Represents the first (leading) directive location in a directive definition.
///
/// According to the GraphQL specification, directive locations are specified as
/// a pipe-separated list following the `on` keyword. The first location is special
/// because its pipe separator is optional, while all subsequent locations
/// must be preceded by a pipe (`|`).
///
/// This structure represents the leading location, which may optionally have a pipe
/// if the directive definition uses leading-pipe style formatting.
///
/// ## Examples
///
/// ```text
/// # Standard format (no leading pipe)
/// directive @auth on FIELD | FRAGMENT_SPREAD
///
/// # Leading-pipe format (optional leading pipe)
/// directive @auth on
///   | FIELD
///   | FRAGMENT_SPREAD
///
/// # Single location (no pipes needed)
/// directive @deprecated on FIELD_DEFINITION
/// ```
///
/// ## Type Parameters
///
/// * `Location` - The type representing individual directive locations
/// * `Span` - The type representing source location information
#[derive(Debug, Clone, Copy)]
pub struct LeadingDirectiveLocation<Location, Span> {
  span: Span,
  /// The location of the directive.
  location: Location,
  /// The pipe token.
  pipe: Option<Pipe<Span>>,
}

impl<Location, Span> LeadingDirectiveLocation<Location, Span> {
  /// Returns a reference to the span covering the entire leading directive location entry.
  ///
  /// The span includes the optional leading pipe separator and the location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the directive location.
  #[inline]
  pub const fn location(&self) -> &Location {
    &self.location
  }

  /// Returns a reference to the optional leading pipe separator.
  ///
  /// The leading pipe is present when using leading-pipe formatting style,
  /// which is often used for better readability with multiple locations.
  #[inline]
  pub const fn pipe(&self) -> Option<&Pipe<Span>> {
    self.pipe.as_ref()
  }

  /// Creates a parser that can parse the leading directive location with optional pipe separator.
  ///
  /// This parser handles the first location in a directive locations list, which according
  /// to the GraphQL specification can optionally start with a pipe separator for formatting.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the leading directive location.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(location_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, Location, E> + Clone,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .or_not()
      .then(location_parser)
      .map_with(|(pipe, location), sp| Self {
        span: Span::from_map_extra(sp),
        location,
        pipe,
      })
  }
}

impl<Location, Span> AsRef<Span> for LeadingDirectiveLocation<Location, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Location, Span> IntoSpan<Span> for LeadingDirectiveLocation<Location, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Location, Span> IntoComponents for LeadingDirectiveLocation<Location, Span> {
  type Components = (Span, Option<Pipe<Span>>, Location);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.pipe, self.location)
  }
}

/// Represents a subsequent (non-leading) directive location in a directive definition.
///
/// According to the GraphQL specification, all directive locations after the first one
/// must be preceded by a pipe (`|`) separator. This structure represents these subsequent
/// locations, which always have a required pipe separator.
///
/// ## GraphQL Specification Context
///
/// While the first location in a directive locations list may optionally have a leading pipe,
/// all subsequent locations must be preceded by a pipe separator. This enforces the proper
/// pipe-separated list syntax required by the GraphQL specification.
///
/// ## Examples
///
/// ```text
/// # In this directive definition:
/// directive @conditional on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
/// #                               ^                 ^
/// #                          required pipe      required pipe
///
/// # Or in leading-pipe format:
/// directive @conditional on
///   | FIELD
///   | FRAGMENT_SPREAD    # This is a DirectiveLocation (required pipe)
///   | INLINE_FRAGMENT    # This is also a DirectiveLocation (required pipe)
/// ```
///
/// ## Type Parameters
///
/// * `Location` - The type representing individual directive locations
/// * `Span` - The type representing source location information
#[derive(Debug, Clone, Copy)]
pub struct DirectiveLocation<Location, Span> {
  /// The span of the location
  span: Span,
  /// The pipe token.
  pipe: Pipe<Span>,
  /// The location of the directive.
  location: Location,
}

impl<Location, Span> DirectiveLocation<Location, Span> {
  /// Returns a reference to the span covering the entire directive location entry.
  ///
  /// The span includes the required pipe separator and the location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the directive location.
  #[inline]
  pub const fn location(&self) -> &Location {
    &self.location
  }

  /// Returns a reference to the required pipe separator.
  ///
  /// All subsequent directive locations (after the first) must have a pipe separator
  /// according to the GraphQL specification.
  #[inline]
  pub const fn pipe(&self) -> &Pipe<Span> {
    &self.pipe
  }

  /// Creates a parser that can parse a directive location with required pipe separator.
  ///
  /// This parser handles subsequent entries in directive location lists, which according
  /// to the GraphQL specification must be preceded by a pipe separator.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the directive location.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(location_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, Location, E> + Clone,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .then(location_parser)
      .map_with(|(pipe, location), sp| Self {
        span: Span::from_map_extra(sp),
        location,
        pipe,
      })
  }
}

impl<Location, Span> AsRef<Span> for DirectiveLocation<Location, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Location, Span> IntoSpan<Span> for DirectiveLocation<Location, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Location, Span> IntoComponents for DirectiveLocation<Location, Span> {
  type Components = (Span, Pipe<Span>, Location);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.pipe, self.location)
  }
}

/// Represents a complete collection of directive locations in a directive definition.
///
/// According to the GraphQL specification, directive locations follow a specific pattern:
/// - The first location is optional pipe-prefixed (can start with `|` or not)
/// - All subsequent locations must be pipe-prefixed with `|`
/// - At least one location is required
///
/// This structure properly models this specification by separating the leading location
/// (which handles the optional pipe) from the remaining locations (which require pipes).
///
/// ## GraphQL Specification Context
///
/// The GraphQL spec defines:
/// ```text
/// DirectiveLocations : | ? DirectiveLocation ( | DirectiveLocation )*
/// ```
///
/// This means:
/// - `+` requires one or more locations
/// - `|?` means the pipe is optional for each location
/// - However, in practice, only the first location can omit the pipe
///
/// ## Examples
///
/// ```text
/// # Standard format
/// directive @auth on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
/// #                  ^lead  ^remaining       ^remaining
///
/// # Leading-pipe format  
/// directive @auth on
///   | FIELD              # Leading location with optional pipe
///   | FRAGMENT_SPREAD    # Remaining location with required pipe
///   | INLINE_FRAGMENT    # Remaining location with required pipe
///
/// # Single location
/// directive @deprecated on FIELD_DEFINITION
/// #                        ^leading location only
/// ```
///
/// ## Type Parameters
///
/// * `Location` - The type representing individual directive locations
/// * `Span` - The type representing source location information
/// * `Container` - The container type for storing remaining locations (defaults to `Vec<DirectiveLocation<Location, Span>>`)
#[derive(Debug, Clone, Copy)]
pub struct DirectiveLocations<Location, Span, Container = Vec<DirectiveLocation<Location, Span>>> {
  span: Span,
  leading: LeadingDirectiveLocation<Location, Span>,
  remaining: Container,
}

impl<Location, Span, Container> AsRef<Span> for DirectiveLocations<Location, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    &self.span
  }
}

impl<Location, Span, Container> IntoSpan<Span> for DirectiveLocations<Location, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Location, Span, Container> IntoComponents for DirectiveLocations<Location, Span, Container> {
  type Components = (Span, LeadingDirectiveLocation<Location, Span>, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.leading, self.remaining)
  }
}

impl<Location, Span, Container> DirectiveLocations<Location, Span, Container> {
  /// Returns a reference to the span covering all directive locations.
  ///
  /// The span encompasses from the start of the leading location to the end
  /// of the last location, including all pipe separators.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the leading (first) directive location.
  ///
  /// The leading location may optionally have a pipe separator, supporting
  /// both standard and leading-pipe formatting styles.
  pub const fn leading_location(&self) -> &LeadingDirectiveLocation<Location, Span> {
    &self.leading
  }

  /// Returns a reference to the container holding remaining directive locations.
  ///
  /// Each remaining location must have a pipe separator according to the GraphQL
  /// specification. This container will be empty if there's only one location.
  pub const fn remaining_locations(&self) -> &Container {
    &self.remaining
  }

  /// Creates a parser that can parse a complete directive locations collection.
  ///
  /// This parser properly handles the GraphQL specification for directive locations,
  /// parsing the leading location (with optional pipe) followed by any remaining
  /// locations (each with required pipe).
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the directive locations.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(
    location_parser: impl Fn() -> P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    Container: chumsky::container::Container<DirectiveLocation<Location, Span>>,
    P: Parser<'src, I, Location, E> + Clone,
  {
    LeadingDirectiveLocation::parser_with(location_parser())
      .then(
        ignored()
          .ignore_then(DirectiveLocation::parser_with(location_parser()))
          .repeated()
          .collect(),
      )
      .map_with(|(leading, remaining), sp| Self {
        span: Span::from_map_extra(sp),
        leading,
        remaining,
      })
  }
}

/// Represents a complete GraphQL directive definition.
///
/// A directive definition specifies a custom directive that can be used in GraphQL
/// documents. It includes the directive's name, optional arguments, whether it's
/// repeatable, and the locations where it can be applied.
///
/// Directive definitions are a fundamental part of GraphQL schema definitions,
/// allowing for extensible metadata and behavior specification.
///
/// ## Examples
///
/// ```text
/// # Simple directive without arguments
/// directive @deprecated on FIELD_DEFINITION | ENUM_VALUE
///
/// # Directive with arguments
/// directive @auth(
///   requires: Role = USER
///   scopes: [String!]
/// ) on OBJECT | FIELD_DEFINITION
///
/// # Repeatable directive
/// directive @tag(name: String!) repeatable on
///   | FIELD_DEFINITION
///   | OBJECT
///   | INTERFACE
///   | UNION
///   | ENUM
///
/// # Directive with description
/// """
/// Marks a field as deprecated with an optional reason.
/// """
/// directive @deprecated(
///   """
///   The reason for the deprecation.
///   """
///   reason: String = "No longer supported"
/// ) on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE
/// ```
///
/// ## Type Parameters
///
/// * `Args` - The type representing the directive's arguments definition
/// * `Locations` - The type representing the directive locations collection
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// DirectiveDefinition : Description? directive @ Name ArgumentsDefinition? repeatable? on DirectiveLocations
/// ```
///
/// Spec: [DirectiveDefinition](https://spec.graphql.org/draft/#DirectiveDefinition)
#[derive(Debug, Clone, Copy)]
pub struct DirectiveDefinition<Name, Args, Locations, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  keyword: keywords::Directive<Span>,
  at: At<Span>,
  name: Name,
  arguments_definition: Option<Args>,
  repeateable: Option<keywords::Repeatable<Span>>,
  on: keywords::On<Span>,
  directive_locations: Locations,
}

impl<Name, Args, Locations, Span> AsRef<Span> for DirectiveDefinition<Name, Args, Locations, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Args, Locations, Span> IntoSpan<Span>
  for DirectiveDefinition<Name, Args, Locations, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Args, Locations, Span> IntoComponents
  for DirectiveDefinition<Name, Args, Locations, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Directive<Span>,
    At<Span>,
    Name,
    Option<Args>,
    Option<keywords::Repeatable<Span>>,
    keywords::On<Span>,
    Locations,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.keyword,
      self.at,
      self.name,
      self.arguments_definition,
      self.repeateable,
      self.on,
      self.directive_locations,
    )
  }
}

impl<Name, Args, Locations, Span> DirectiveDefinition<Name, Args, Locations, Span> {
  /// Returns a reference to the span covering the entire directive definition.
  ///
  /// The span includes the optional description, directive keyword, name,
  /// arguments, repeatable keyword, on keyword, and locations list.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the at symbol (`@`) that prefixes the directive name.
  ///
  /// This provides access to the exact location and span information of the
  /// at symbol used in the directive definition.
  #[inline]
  pub const fn at(&self) -> &At<Span> {
    &self.at
  }

  /// Returns a reference to the optional description of the directive definition.
  ///
  /// The description provides documentation for the directive and is typically
  /// a string value (single or block string) that appears before the directive keyword.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the `directive` keyword.
  ///
  /// This provides access to the exact location and span information of the
  /// directive keyword that starts the definition.
  #[inline]
  pub const fn directive_keyword(&self) -> &keywords::Directive<Span> {
    &self.keyword
  }

  /// Returns a reference to the optional arguments definition.
  ///
  /// The arguments definition specifies what parameters can be provided when
  /// using this directive, including their types, default values, and descriptions.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Args> {
    self.arguments_definition.as_ref()
  }

  /// Returns a reference to the optional `repeatable` keyword.
  ///
  /// If present, the repeatable keyword indicates that this directive can be
  /// applied multiple times to the same location.
  #[inline]
  pub const fn repeatable(&self) -> Option<&keywords::Repeatable<Span>> {
    self.repeateable.as_ref()
  }

  /// Returns a reference to the `on` keyword.
  ///
  /// The on keyword precedes the directive locations list and indicates
  /// where the directive can be applied.
  #[inline]
  pub const fn on_keyword(&self) -> &keywords::On<Span> {
    &self.on
  }

  /// Returns a reference to the name of the directive definition.
  ///
  /// The name identifies the directive and is used when applying the directive
  /// in GraphQL documents (preceded by `@`).
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the directive locations specification.
  ///
  /// The locations specify where this directive can be applied in GraphQL
  /// documents, such as on fields, types, arguments, etc.
  #[inline]
  pub const fn locations(&self) -> &Locations {
    &self.directive_locations
  }

  /// Creates a parser that can parse a complete directive definition.
  ///
  /// This parser handles the full directive definition syntax including all
  /// optional components. The parsing of arguments and locations is delegated
  /// to the provided parsers.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the directive definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, NP, AP, LP>(
    name_parser: NP,
    args_parser: AP,
    directive_locations_parser: LP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    AP: Parser<'src, I, Args, E> + Clone,
    LP: Parser<'src, I, Locations, E> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    // description? ~ 'directive' ~ '@' ~ name ~ arguments_definition? ~ repeatable? ~ 'on' ~ directive_locations
    StringValue::parser()
      .or_not()
      .then(keywords::Directive::parser().padded_by(ignored()))
      .then(At::parser().padded_by(ignored()))
      .then(name_parser)
      .then(ignored().ignore_then(args_parser).or_not())
      .then(
        ignored()
          .ignore_then(keywords::Repeatable::parser())
          .or_not(),
      )
      .then(keywords::On::parser().padded_by(ignored()))
      .then(directive_locations_parser)
      .map_with(
        |(
          ((((((description, keyword), at), name), arguments_definition), repeateable), on),
          directive_locations,
        ),
         sp| {
          Self {
            span: Span::from_map_extra(sp),
            description,
            keyword,
            at,
            name,
            arguments_definition,
            repeateable,
            on,
            directive_locations,
          }
        },
      )
  }
}
