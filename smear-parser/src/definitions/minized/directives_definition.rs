use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Parseable, Source, Token, Tokenizer,
  utils::{
    Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};
use smear_utils::{IntoComponents, IntoSpan};

use crate::{
  keyword,
  lang::{
    minized::keywords::{Directive, On, Repeatable},
    punctuator::At,
  },
};

keyword! {
  /// `QUERY` location - directives can be applied to query operations.
  ///
  /// Used when defining where a directive can be placed. Query directives
  /// affect the entire query operation and can be used for things like
  /// authentication, caching, or operation-level configuration.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @auth on QUERY
  /// directive @cache(ttl: 300) on QUERY
  /// ```
  (QueryLocation, "QUERY_KW", "QUERY"),

  /// `MUTATION` location - directives can be applied to mutation operations.
  ///
  /// Mutation directives affect the entire mutation operation and can be used
  /// for authorization, rate limiting, or transaction control.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @rateLimit(max: 10) on MUTATION
  /// directive @requireAuth on MUTATION
  /// ```
  (MutationLocation, "MUTATION_KW", "MUTATION"),

  /// `SUBSCRIPTION` location - directives can be applied to subscription operations.
  ///
  /// Subscription directives control real-time data flow and can be used
  /// for filtering, authentication, or subscription management.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @requireSubscription on SUBSCRIPTION
  /// directive @throttle(rate: "1/sec") on SUBSCRIPTION
  /// ```
  (SubscriptionLocation, "SUBSCRIPTION_KW", "SUBSCRIPTION"),

  /// `FIELD_DEFINITION` location - directives can be applied to field definitions in schemas.
  ///
  /// Field definition directives control field behavior, validation, authorization,
  /// or provide metadata about fields in type definitions.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on FIELD_DEFINITION
  /// directive @auth(requires: Role) on FIELD_DEFINITION
  /// ```
  (FieldDefinitionLocation, "FIELD_DEFINITION_KW", "FIELD_DEFINITION"),

  /// `FIELD` location - directives can be applied to field selections in queries.
  ///
  /// Field directives control individual field selection behavior, commonly
  /// used for conditional inclusion, skipping, or field-level configuration.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @include(if: Boolean!) on FIELD
  /// directive @skip(if: Boolean!) on FIELD
  /// ```
  (FieldLocation, "FIELD_KW", "FIELD"),

  /// `FRAGMENT_DEFINITION` location - directives can be applied to named fragment definitions.
  ///
  /// Fragment definition directives control fragment behavior and can be used
  /// for conditional fragments, caching, or fragment-level metadata.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @experimental on FRAGMENT_DEFINITION
  /// directive @cache(scope: PRIVATE) on FRAGMENT_DEFINITION
  /// ```
  (FragmentDefinitionLocation, "FRAGMENT_DEFINITION_KW", "FRAGMENT_DEFINITION"),

  /// `FRAGMENT_SPREAD` location - directives can be applied to fragment spreads.
  ///
  /// Fragment spread directives control when and how fragments are included
  /// in selection sets, commonly used for conditional fragment inclusion.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @include(if: Boolean!) on FRAGMENT_SPREAD
  /// directive @defer(label: String) on FRAGMENT_SPREAD
  /// ```
  (FragmentSpreadLocation, "FRAGMENT_SPREAD_KW", "FRAGMENT_SPREAD"),
  /// `INLINE_FRAGMENT` location - directives can be applied to inline fragments.
  ///
  /// Inline fragment directives control conditional type-specific field selections
  /// and can be used for conditional inclusion based on type or other criteria.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @include(if: Boolean!) on INLINE_FRAGMENT
  /// directive @skip(if: Boolean!) on INLINE_FRAGMENT
  /// ```
  (InlineFragmentLocation, "INLINE_FRAGMENT_KW", "INLINE_FRAGMENT"),

  /// `VARIABLE_DEFINITION` location - directives can be applied to variable definitions.
  ///
  /// Variable definition directives control variable behavior, validation,
  /// or provide metadata about operation variables.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on VARIABLE_DEFINITION
  /// directive @validate(pattern: String) on VARIABLE_DEFINITION
  /// ```
  (VariableDefinitionLocation, "VARIABLE_DEFINITION_KW", "VARIABLE_DEFINITION"),

  /// `SCHEMA` location - directives can be applied to the schema definition.
  ///
  /// Schema directives provide global schema-level configuration, metadata,
  /// or behavior that applies to the entire GraphQL schema.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @link(url: String!) on SCHEMA
  /// directive @composeDirective(name: String!) on SCHEMA
  /// ```
  (SchemaLocation, "SCHEMA_KW", "SCHEMA"),

  /// `SCALAR` location - directives can be applied to scalar type definitions.
  ///
  /// Scalar directives provide validation, serialization, or metadata
  /// for custom scalar types in the schema.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @specifiedBy(url: String!) on SCALAR
  /// directive @validate(regex: String) on SCALAR
  /// ```
  (ScalarLocation, "SCALAR_KW", "SCALAR"),

  /// `OBJECT` location - directives can be applied to object type definitions.
  ///
  /// Object type directives control object behavior, provide metadata,
  /// or enable features like interfaces, caching, or authorization.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @key(fields: String!) on OBJECT
  /// directive @cacheControl(maxAge: Int) on OBJECT
  /// ```
  (ObjectLocation, "OBJECT_KW", "OBJECT"),

  /// `ARGUMENT_DEFINITION` location - directives can be applied to argument definitions.
  ///
  /// Argument definition directives control argument validation, transformation,
  /// or provide metadata about field and directive arguments.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on ARGUMENT_DEFINITION
  /// directive @constraint(min: Int, max: Int) on ARGUMENT_DEFINITION
  /// ```
  (ArgumentDefinitionLocation, "ARGUMENT_DEFINITION_KW", "ARGUMENT_DEFINITION"),

  /// `INTERFACE` location - directives can be applied to interface type definitions.
  ///
  /// Interface directives control interface behavior, provide metadata,
  /// or enable features for types that implement the interface.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @key(fields: String!) on INTERFACE
  /// directive @auth(requires: Role) on INTERFACE
  /// ```
  (InterfaceLocation, "INTERFACE_KW", "INTERFACE"),

  /// `UNION` location - directives can be applied to union type definitions.
  ///
  /// Union directives control union behavior, type resolution,
  /// or provide metadata for union types.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @unionMember(type: String!) on UNION
  /// directive @deprecated(reason: String) on UNION
  /// ```
  (UnionLocation, "UNION_KW", "UNION"),

  /// `ENUM_VALUE` location - directives can be applied to enum value definitions.
  ///
  /// Enum value directives provide metadata, deprecation information,
  /// or control the behavior of specific enum values.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on ENUM_VALUE
  /// directive @internal on ENUM_VALUE
  /// ```
  (EnumValueLocation, "ENUM_VALUE_KW", "ENUM_VALUE"),

  /// `ENUM` location - directives can be applied to enum type definitions.
  ///
  /// Enum directives control enum behavior, validation,
  /// or provide metadata for the entire enum type.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on ENUM
  /// directive @oneOf on ENUM
  /// ```
  (EnumLocation, "ENUM_KW", "ENUM"),

  /// `INPUT_OBJECT` location - directives can be applied to input object type definitions.
  ///
  /// Input object directives control input validation, transformation,
  /// or provide metadata for input types used in arguments.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @oneOf on INPUT_OBJECT
  /// directive @validate(schema: String) on INPUT_OBJECT
  /// ```
  (InputObjectLocation, "INPUT_OBJECT_KW", "INPUT_OBJECT"),

  /// `INPUT_FIELD_DEFINITION` location - directives can be applied to input field definitions.
  ///
  /// Input field directives control input field validation, transformation,
  /// or provide metadata for fields within input object types.
  ///
  /// ## Examples
  ///
  /// ```text
  /// directive @deprecated(reason: String) on INPUT_FIELD_DEFINITION
  /// directive @constraint(min: Int, max: Int) on INPUT_FIELD_DEFINITION
  /// ```
  (InputFieldDefinitionLocation, "INPUT_FIELD_DEFINITION_KW", "INPUT_FIELD_DEFINITION"),
}

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
#[derive(Debug, Clone, Copy, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDirectiveLocation {
  /// `QUERY` - directive can be applied to query operations
  Query(QueryLocation),
  /// `MUTATION` - directive can be applied to mutation operations
  Mutation(MutationLocation),
  /// `SUBSCRIPTION` - directive can be applied to subscription operations
  Subscription(SubscriptionLocation),
  /// `FIELD` - directive can be applied to field selections
  Field(FieldLocation),
  /// `FRAGMENT_DEFINITION` - directive can be applied to fragment definitions
  FragmentDefinition(FragmentDefinitionLocation),
  /// `FRAGMENT_SPREAD` - directive can be applied to fragment spreads
  FragmentSpread(FragmentSpreadLocation),
  /// `INLINE_FRAGMENT` - directive can be applied to inline fragments
  InlineFragment(InlineFragmentLocation),
  /// `VARIABLE_DEFINITION` - directive can be applied to variable definitions
  VariableDefinition(VariableDefinitionLocation),
}

impl AsRef<str> for ExecutableDirectiveLocation {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl core::borrow::Borrow<str> for ExecutableDirectiveLocation {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

impl DisplaySDL for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

impl DisplayHuman for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl DisplaySyntaxTree for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let mut padding = level * indent;
    let span = self.span();
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- {}@{}..{}",
      self.syntax_tree_name(),
      span.start(),
      span.end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      span.start(),
      span.end(),
      self.as_str(),
    )
  }
}

impl ExecutableDirectiveLocation {
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

  /// Returns the string representation of the directive location.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query(_) => QueryLocation::raw(),
      Self::Mutation(_) => MutationLocation::raw(),
      Self::Subscription(_) => SubscriptionLocation::raw(),
      Self::Field(_) => FieldLocation::raw(),
      Self::FragmentDefinition(_) => FragmentDefinitionLocation::raw(),
      Self::FragmentSpread(_) => FragmentSpreadLocation::raw(),
      Self::InlineFragment(_) => InlineFragmentLocation::raw(),
      Self::VariableDefinition(_) => VariableDefinitionLocation::raw(),
    }
  }

  #[inline]
  fn syntax_tree_name(&self) -> &'static str {
    match self {
      Self::Query(_) => "QUERY_KW",
      Self::Mutation(_) => "MUTATION_KW",
      Self::Subscription(_) => "SUBSCRIPTION_KW",
      Self::Field(_) => "FIELD_KW",
      Self::FragmentDefinition(_) => "FRAGMENT_DEFINITION_KW",
      Self::FragmentSpread(_) => "FRAGMENT_SPREAD_KW",
      Self::InlineFragment(_) => "INLINE_FRAGMENT_KW",
      Self::VariableDefinition(_) => "VARIABLE_DEFINITION_KW",
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDirectiveLocation {
  /// `SCHEMA` - directive can be applied to schema definitions
  Schema(SchemaLocation),
  /// `SCALAR` - directive can be applied to scalar type definitions
  Scalar(ScalarLocation),
  /// `OBJECT` - directive can be applied to object type definitions
  Object(ObjectLocation),
  /// `FIELD_DEFINITION` - directive can be applied to field definitions
  FieldDefinition(FieldDefinitionLocation),
  /// `ARGUMENT_DEFINITION` - directive can be applied to argument definitions
  ArgumentDefinition(ArgumentDefinitionLocation),
  /// `INTERFACE` - directive can be applied to interface type definitions
  Interface(InterfaceLocation),
  /// `UNION` - directive can be applied to union type definitions
  Union(UnionLocation),
  /// `ENUM` - directive can be applied to enum type definitions
  Enum(EnumLocation),
  /// `ENUM_VALUE` - directive can be applied to enum value definitions
  EnumValue(EnumValueLocation),
  /// `INPUT_OBJECT` - directive can be applied to input object type definitions
  InputObject(InputObjectLocation),
  /// `INPUT_FIELD_DEFINITION` - directive can be applied to input field definitions
  InputFieldDefinition(InputFieldDefinitionLocation),
}

impl AsRef<str> for TypeSystemDirectiveLocation {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl core::borrow::Borrow<str> for TypeSystemDirectiveLocation {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

impl DisplaySDL for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

impl DisplayHuman for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl DisplaySyntaxTree for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let mut padding = level * indent;
    let span = self.span();
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- {}@{}..{}",
      self.syntax_tree_name(),
      span.start(),
      span.end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      span.start(),
      span.end(),
      self.as_str(),
    )
  }
}

impl TypeSystemDirectiveLocation {
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

  /// Returns the string representation of the directive location.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Schema(_) => SchemaLocation::raw(),
      Self::Scalar(_) => ScalarLocation::raw(),
      Self::Object(_) => ObjectLocation::raw(),
      Self::FieldDefinition(_) => FieldDefinitionLocation::raw(),
      Self::ArgumentDefinition(_) => ArgumentDefinitionLocation::raw(),
      Self::Interface(_) => InterfaceLocation::raw(),
      Self::Union(_) => UnionLocation::raw(),
      Self::Enum(_) => EnumLocation::raw(),
      Self::EnumValue(_) => EnumValueLocation::raw(),
      Self::InputObject(_) => InputObjectLocation::raw(),
      Self::InputFieldDefinition(_) => InputFieldDefinitionLocation::raw(),
    }
  }

  #[inline]
  fn syntax_tree_name(&self) -> &'static str {
    match self {
      Self::Schema(_) => "SCHEMA_KW",
      Self::Scalar(_) => "SCALAR_KW",
      Self::Object(_) => "OBJECT_KW",
      Self::FieldDefinition(_) => "FIELD_DEFINITION_KW",
      Self::ArgumentDefinition(_) => "ARGUMENT_DEFINITION_KW",
      Self::Interface(_) => "INTERFACE_KW",
      Self::Union(_) => "UNION_KW",
      Self::Enum(_) => "ENUM_KW",
      Self::EnumValue(_) => "ENUM_VALUE_KW",
      Self::InputObject(_) => "INPUT_OBJECT_KW",
      Self::InputFieldDefinition(_) => "INPUT_FIELD_DEFINITION_KW",
    }
  }
}

/// Represents any location where a directive can be applied in GraphQL.
///
/// This is the top-level enum that encompasses both executable and type system
/// directive locations. It provides a unified interface for working with all
/// possible directive placement locations in GraphQL documents.
///
/// Spec: [DirectiveLocation](https://spec.graphql.org/draft/#DirectiveLocation)
#[derive(Debug, Clone, Copy, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Location {
  /// Executable directive location
  Executable(ExecutableDirectiveLocation),
  /// Type system directive location
  TypeSystem(TypeSystemDirectiveLocation),
}

impl DisplaySDL for Location {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Executable(loc) => DisplaySDL::fmt(loc, f),
      Self::TypeSystem(loc) => DisplaySDL::fmt(loc, f),
    }
  }
}

impl DisplayHuman for Location {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Executable(loc) => DisplayHuman::fmt(loc, f),
      Self::TypeSystem(loc) => DisplayHuman::fmt(loc, f),
    }
  }
}

impl DisplaySyntaxTree for Location {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    match self {
      Self::Executable(loc) => DisplaySyntaxTree::fmt(loc, level, indent, f),
      Self::TypeSystem(loc) => DisplaySyntaxTree::fmt(loc, level, indent, f),
    }
  }
}

impl Location {
  /// Returns a reference to the span covering the directive location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Executable(loc) => loc.span(),
      Self::TypeSystem(loc) => loc.span(),
    }
  }
}

macro_rules! from_location {
  ($($variant:ident: [$($sub_variant:ident),+$(,)?]), +$(,)?) => {
    $(
      $(
        impl From<$sub_variant> for Location {
          fn from(location: $sub_variant) -> Self {
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

// /// Represents the first (leading) directive location in a directive definition.
// ///
// /// According to the GraphQL specification, directive locations are specified as
// /// a pipe-separated list following the `on` keyword. The first location is special
// /// because its pipe separator is optional, while all subsequent locations
// /// must be preceded by a pipe (`|`).
// ///
// /// This structure represents the leading location, which may optionally have a pipe
// /// if the directive definition uses leading-pipe style formatting.
// ///
// /// ## Examples
// ///
// /// ```text
// /// # Standard format (no leading pipe)
// /// directive @auth on FIELD | FRAGMENT_SPREAD
// ///
// /// # Leading-pipe format (optional leading pipe)
// /// directive @auth on
// ///   | FIELD
// ///   | FRAGMENT_SPREAD
// ///
// /// # Single location (no pipes needed)
// /// directive @deprecated on FIELD_DEFINITION
// /// ```
// ///
// /// ## Type Parameters
// ///
// /// * `Location` - The type representing individual directive locations
// /// * `Span` - The type representing source location information
// #[derive(Debug, Clone, Copy)]
// pub struct LeadingDirectiveLocation<Location> {
//   span: Span,
//   /// The location of the directive.
//   location: Location,
//   /// The pipe token.
//   pipe: Option<Pipe>,
// }

// impl<Location> LeadingDirectiveLocation<Location> {
//   /// Returns a reference to the span covering the entire leading directive location entry.
//   ///
//   /// The span includes the optional leading pipe separator and the location token.
//   #[inline]
//   pub const fn span(&self) -> &Span {
//     &self.span
//   }

//   /// Returns a reference to the directive location.
//   #[inline]
//   pub const fn location(&self) -> &Location {
//     &self.location
//   }

//   /// Returns a reference to the optional leading pipe separator.
//   ///
//   /// The leading pipe is present when using leading-pipe formatting style,
//   /// which is often used for better readability with multiple locations.
//   #[inline]
//   pub const fn pipe(&self) -> Option<&Pipe> {
//     self.pipe.as_ref()
//   }

//   /// Creates a parser that can parse the leading directive location with optional pipe separator.
//   ///
//   /// This parser handles the first location in a directive locations list, which according
//   /// to the GraphQL specification can optionally start with a pipe separator for formatting.
//   ///
//   /// ## Notes
//   ///
//   /// This parser does not handle surrounding [ignored tokens].
//   /// The calling parser is responsible for handling any necessary
//   /// whitespace skipping or comment processing around the leading directive location.
//   ///
//   /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
//   pub fn parser_with<'src, I, T, Error, E, P>(location_parser: P) -> impl Parser<'src, I, Self, E> + Clone
//   where
//     T: Token<'src>,
//     I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
//     Error: 'src,
//     E: ParserExtra<'src, I, Error = Error> + 'src,
//     P: Parser<'src, I, Location, E> + Clone,
//     Pipe: Parseable<I, T, Error> + 'src,
//   {
//     Pipe::parser()
//       .or_not()
//       .then(location_parser)
//       .map_with(|(pipe, location), exa| Self {
//         span: exa.span(),
//         location,
//         pipe,
//       })
//   }
// }

// impl<Location> AsRef<Span> for LeadingDirectiveLocation<Location> {
//   #[inline]
//   fn as_ref(&self) -> &Span {
//     self.span()
//   }
// }

// impl<Location> IntoSpan<Span> for LeadingDirectiveLocation<Location> {
//   #[inline]
//   fn into_span(self) -> Span {
//     self.span
//   }
// }

// impl<Location> IntoComponents for LeadingDirectiveLocation<Location> {
//   type Components = (Span, Option<Pipe>, Location);

//   #[inline]
//   fn into_components(self) -> Self::Components {
//     (self.span, self.pipe, self.location)
//   }
// }

// impl<'a, Location, I, T, Error> Parseable<I, T, Error> for LeadingDirectiveLocation<Location>
// where
//   T: Token<'a>,
//   I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
//   Error: 'a,
//   Location: Parseable<I, T, Error> + 'a,
//   Pipe: Parseable<I, T, Error> + 'a,
// {
//   #[inline]
//   fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     E: ParserExtra<'a, I, Error = Error> + 'a,
//   {
//     Self::parser_with(Location::parser())
//   }
// }

// /// Represents a subsequent (non-leading) directive location in a directive definition.
// ///
// /// According to the GraphQL specification, all directive locations after the first one
// /// must be preceded by a pipe (`|`) separator. This structure represents these subsequent
// /// locations, which always have a required pipe separator.
// ///
// /// ## GraphQL Specification Context
// ///
// /// While the first location in a directive locations list may optionally have a leading pipe,
// /// all subsequent locations must be preceded by a pipe separator. This enforces the proper
// /// pipe-separated list syntax required by the GraphQL specification.
// ///
// /// ## Examples
// ///
// /// ```text
// /// # In this directive definition:
// /// directive @conditional on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
// /// #                               ^                 ^
// /// #                          required pipe      required pipe
// ///
// /// # Or in leading-pipe format:
// /// directive @conditional on
// ///   | FIELD
// ///   | FRAGMENT_SPREAD    # This is a DirectiveLocation (required pipe)
// ///   | INLINE_FRAGMENT    # This is also a DirectiveLocation (required pipe)
// /// ```
// ///
// /// ## Type Parameters
// ///
// /// * `Location` - The type representing individual directive locations
// /// * `Span` - The type representing source location information
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct DirectiveLocation<Location> {
//   /// The span of the location
//   span: Span,
//   /// The pipe token.
//   pipe: Pipe,
//   /// The location of the directive.
//   location: Location,
// }

// impl<Location> DirectiveLocation<Location> {
//   /// Returns a reference to the span covering the entire directive location entry.
//   ///
//   /// The span includes the required pipe separator and the location token.
//   #[inline]
//   pub const fn span(&self) -> &Span {
//     &self.span
//   }

//   /// Returns a reference to the directive location.
//   #[inline]
//   pub const fn location(&self) -> &Location {
//     &self.location
//   }

//   /// Returns a reference to the required pipe separator.
//   ///
//   /// All subsequent directive locations (after the first) must have a pipe separator
//   /// according to the GraphQL specification.
//   #[inline]
//   pub const fn pipe(&self) -> &Pipe {
//     &self.pipe
//   }

//   /// Creates a parser that can parse a directive location with required pipe separator.
//   ///
//   /// This parser handles subsequent entries in directive location lists, which according
//   /// to the GraphQL specification must be preceded by a pipe separator.
//   ///
//   /// ## Notes
//   ///
//   /// This parser does not handle surrounding [ignored tokens].
//   /// The calling parser is responsible for handling any necessary
//   /// whitespace skipping or comment processing around the directive location.
//   ///
//   /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
//   pub fn parser_with<'src, I, T, Error, E, P>(location_parser: P) -> impl Parser<'src, I, Self, E> + Clone
//   where
//     T: Token<'src>,
//     I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
//     Error: 'src,
//     E: ParserExtra<'src, I, Error = Error> + 'src,
//     Pipe: Parseable<I, T, Error> + 'src,
//     P: Parser<'src, I, Location, E> + Clone,
//   {
//     Pipe::parser()
//       .then(location_parser)
//       .map_with(|(pipe, location), exa| Self {
//         span: exa.span(),
//         location,
//         pipe,
//       })
//   }
// }

// impl<Location> AsRef<Span> for DirectiveLocation<Location> {
//   #[inline]
//   fn as_ref(&self) -> &Span {
//     self.span()
//   }
// }

// impl<Location> IntoSpan<Span> for DirectiveLocation<Location> {
//   #[inline]
//   fn into_span(self) -> Span {
//     self.span
//   }
// }

// impl<Location> IntoComponents for DirectiveLocation<Location> {
//   type Components = (Span, Pipe, Location);

//   #[inline]
//   fn into_components(self) -> Self::Components {
//     (self.span, self.pipe, self.location)
//   }
// }

// impl<'a, Location, I, T, Error> Parseable<I, T, Error> for DirectiveLocation<Location>
// where
//   T: Token<'a>,
//   I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
//   Error: 'a,
//   Location: Parseable<I, T, Error> + 'a,
//   Pipe: Parseable<I, T, Error> + 'a,
// {
//   #[inline]
//   fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     E: ParserExtra<'a, I, Error = Error> + 'a,
//   {
//     Self::parser_with(Location::parser())
//   }
// }

// /// Represents a complete collection of directive locations in a directive definition.
// ///
// /// According to the GraphQL specification, directive locations follow a specific pattern:
// /// - The first location is optional pipe-prefixed (can start with `|` or not)
// /// - All subsequent locations must be pipe-prefixed with `|`
// /// - At least one location is required
// ///
// /// This structure properly models this specification by separating the leading location
// /// (which handles the optional pipe) from the remaining locations (which require pipes).
// ///
// /// ## GraphQL Specification Context
// ///
// /// The GraphQL spec defines:
// /// ```text
// /// DirectiveLocations : | ? DirectiveLocation ( | DirectiveLocation )*
// /// ```
// ///
// /// This means:
// /// - `+` requires one or more locations
// /// - `|?` means the pipe is optional for each location
// /// - However, in practice, only the first location can omit the pipe
// ///
// /// ## Examples
// ///
// /// ```text
// /// # Standard format
// /// directive @auth on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
// /// #                  ^lead  ^remaining       ^remaining
// ///
// /// # Leading-pipe format
// /// directive @auth on
// ///   | FIELD              # Leading location with optional pipe
// ///   | FRAGMENT_SPREAD    # Remaining location with required pipe
// ///   | INLINE_FRAGMENT    # Remaining location with required pipe
// ///
// /// # Single location
// /// directive @deprecated on FIELD_DEFINITION
// /// #                        ^leading location only
// /// ```
// ///
// /// ## Type Parameters
// ///
// /// * `Location` - The type representing individual directive locations
// /// * `Span` - The type representing source location information
// /// * `Container` - The container type for storing remaining locations (defaults to `Vec<DirectiveLocation<Location>>`)
// #[derive(Debug, Clone, Copy)]
// pub struct DirectiveLocations<Location, Container = Vec<DirectiveLocation<Location>>> {
//   span: Span,
//   leading: LeadingDirectiveLocation<Location>,
//   remaining: Container,
// }

// impl<Location, Container> AsRef<Span> for DirectiveLocations<Location, Container> {
//   #[inline]
//   fn as_ref(&self) -> &Span {
//     &self.span
//   }
// }

// impl<Location, Container> IntoSpan<Span> for DirectiveLocations<Location, Container> {
//   #[inline]
//   fn into_span(self) -> Span {
//     self.span
//   }
// }

// impl<Location, Container> IntoComponents for DirectiveLocations<Location, Container> {
//   type Components = (Span, LeadingDirectiveLocation<Location>, Container);

//   #[inline]
//   fn into_components(self) -> Self::Components {
//     (self.span, self.leading, self.remaining)
//   }
// }

// impl<Location, Container> DirectiveLocations<Location, Container> {
//   /// Returns a reference to the span covering all directive locations.
//   ///
//   /// The span encompasses from the start of the leading location to the end
//   /// of the last location, including all pipe separators.
//   #[inline]
//   pub const fn span(&self) -> &Span {
//     &self.span
//   }

//   /// Returns a reference to the leading (first) directive location.
//   ///
//   /// The leading location may optionally have a pipe separator, supporting
//   /// both standard and leading-pipe formatting styles.
//   pub const fn leading_location(&self) -> &LeadingDirectiveLocation<Location> {
//     &self.leading
//   }

//   /// Returns a reference to the container holding remaining directive locations.
//   ///
//   /// Each remaining location must have a pipe separator according to the GraphQL
//   /// specification. This container will be empty if there's only one location.
//   pub const fn remaining_locations(&self) -> &Container {
//     &self.remaining
//   }

//   /// Creates a parser that can parse a complete directive locations collection.
//   ///
//   /// This parser properly handles the GraphQL specification for directive locations,
//   /// parsing the leading location (with optional pipe) followed by any remaining
//   /// locations (each with required pipe).
//   ///
//   /// ## Notes
//   ///
//   /// This parser does not handle surrounding [ignored tokens].
//   /// The calling parser is responsible for handling any necessary
//   /// whitespace skipping or comment processing around the directive locations.
//   ///
//   /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
//   pub fn parser_with<'src, I, T, Error, E, P>(
//     location_parser: impl Fn() -> P,
//   ) -> impl Parser<'src, I, Self, E> + Clone
//   where
//     T: Token<'src>,
//     I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
//     Error: 'src,
//     E: ParserExtra<'src, I, Error = Error> + 'src,
//     Pipe: Parseable<I, T, Error> + 'src,
//     LeadingDirectiveLocation<Location>: Parseable<I, T, Error> + 'src,
//     DirectiveLocation<Location>: Parseable<I, T, Error> + 'src,
//     Container: chumsky::container::Container<DirectiveLocation<Location>>,
//     P: Parser<'src, I, Location, E> + Clone,
//   {
//     LeadingDirectiveLocation::parser_with(location_parser())
//       .then(
//         DirectiveLocation::parser_with(location_parser())
//           .repeated()
//           .collect(),
//       )
//       .map_with(|(leading, remaining), exa| Self {
//         span: exa.span(),
//         leading,
//         remaining,
//       })
//   }
// }

// impl<'a, Location, Container, I, T, Error> Parseable<I, T, Error>
//   for DirectiveLocations<Location, Container>
// where
//   T: Token<'a>,
//   I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
//   Error: 'a,
//   Location: Parseable<I, T, Error> + 'a,
//   Container: chumsky::container::Container<DirectiveLocation<Location>>,
//   Pipe: Parseable<I, T, Error> + 'a,
//   LeadingDirectiveLocation<Location>: Parseable<I, T, Error> + 'a,
//   DirectiveLocation<Location>: Parseable<I, T, Error> + 'a,
// {
//   #[inline]
//   fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     E: ParserExtra<'a, I, Error = Error> + 'a,
//   {
//     Self::parser_with(Location::parser)
//   }
// }

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
pub struct DirectiveDefinition<Name, Args, Locations> {
  span: Span,
  name: Name,
  arguments_definition: Option<Args>,
  repeateable: bool,
  directive_locations: Locations,
}

impl<Name, Args, Locations> AsRef<Span> for DirectiveDefinition<Name, Args, Locations> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Args, Locations> IntoSpan<Span> for DirectiveDefinition<Name, Args, Locations> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Args, Locations> IntoComponents for DirectiveDefinition<Name, Args, Locations> {
  type Components = (Span, Name, Option<Args>, bool, Locations);

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.name,
      self.arguments_definition,
      self.repeateable,
      self.directive_locations,
    )
  }
}

impl<Name, Args, Locations> DirectiveDefinition<Name, Args, Locations> {
  /// Returns a reference to the span covering the entire directive definition.
  ///
  /// The span includes the optional description, directive keyword, name,
  /// arguments, repeatable keyword, on keyword, and locations list.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
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
  pub const fn repeatable(&self) -> bool {
    self.repeateable
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
  pub fn parser_with<'src, I, T, Error, E, NP, AP, LP>(
    name_parser: NP,
    args_parser: AP,
    directive_locations_parser: LP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    At: Parseable<'src, I, T, Error> + 'src,
    Directive: Parseable<'src, I, T, Error> + 'src,
    On: Parseable<'src, I, T, Error> + 'src,
    Repeatable: Parseable<'src, I, T, Error> + 'src,
    AP: Parser<'src, I, Args, E> + Clone,
    LP: Parser<'src, I, Locations, E> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    Directive::parser()
      .then(At::parser())
      .ignore_then(name_parser)
      .then(args_parser.or_not())
      .then(Repeatable::parser().or_not())
      .then_ignore(On::parser())
      .then(directive_locations_parser)
      .map_with(
        |(((name, arguments_definition), repeateable), directive_locations), exa| Self {
          span: exa.span(),
          name,
          arguments_definition,
          repeateable: repeateable.is_some(),
          directive_locations,
        },
      )
  }
}

impl<'a, Name, Args, Locations, I, T, Error> Parseable<'a, I, T, Error>
  for DirectiveDefinition<Name, Args, Locations>
where
  At: Parseable<'a, I, T, Error>,
  Directive: Parseable<'a, I, T, Error>,
  On: Parseable<'a, I, T, Error>,
  Repeatable: Parseable<'a, I, T, Error>,
  Args: Parseable<'a, I, T, Error>,
  Locations: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser(), Args::parser(), Locations::parser())
  }
}
