use core::marker::PhantomData;
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, extra::ParserExtra, prelude::*},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use smear_lexer::{
  keywords::*,
  punctuator::{At, Pipe},
};

use std::vec::Vec;

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

impl AsSpan<Span> for ExecutableDirectiveLocation {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for ExecutableDirectiveLocation {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Query(loc) => loc.into_span(),
      Self::Mutation(loc) => loc.into_span(),
      Self::Subscription(loc) => loc.into_span(),
      Self::Field(loc) => loc.into_span(),
      Self::FragmentDefinition(loc) => loc.into_span(),
      Self::FragmentSpread(loc) => loc.into_span(),
      Self::InlineFragment(loc) => loc.into_span(),
      Self::VariableDefinition(loc) => loc.into_span(),
    }
  }
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

impl core::fmt::Display for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

impl DisplayCompact for ExecutableDirectiveLocation {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self, f)
  }
}

impl DisplayPretty for ExecutableDirectiveLocation {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayCompact::fmt(self, f, &())
  }
}

impl DisplayHuman for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
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
      Self::Query(_) => QueryLocation::<()>::raw(),
      Self::Mutation(_) => MutationLocation::<()>::raw(),
      Self::Subscription(_) => SubscriptionLocation::<()>::raw(),
      Self::Field(_) => FieldLocation::<()>::raw(),
      Self::FragmentDefinition(_) => FragmentDefinitionLocation::<()>::raw(),
      Self::FragmentSpread(_) => FragmentSpreadLocation::<()>::raw(),
      Self::InlineFragment(_) => InlineFragmentLocation::<()>::raw(),
      Self::VariableDefinition(_) => VariableDefinitionLocation::<()>::raw(),
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

impl AsSpan<Span> for TypeSystemDirectiveLocation {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for TypeSystemDirectiveLocation {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Schema(loc) => loc.into_span(),
      Self::Scalar(loc) => loc.into_span(),
      Self::Object(loc) => loc.into_span(),
      Self::FieldDefinition(loc) => loc.into_span(),
      Self::ArgumentDefinition(loc) => loc.into_span(),
      Self::Interface(loc) => loc.into_span(),
      Self::Union(loc) => loc.into_span(),
      Self::Enum(loc) => loc.into_span(),
      Self::EnumValue(loc) => loc.into_span(),
      Self::InputObject(loc) => loc.into_span(),
      Self::InputFieldDefinition(loc) => loc.into_span(),
    }
  }
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

impl core::fmt::Display for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

impl DisplayCompact for TypeSystemDirectiveLocation {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self, f)
  }
}

impl DisplayPretty for TypeSystemDirectiveLocation {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayCompact::fmt(self, f, &())
  }
}

impl DisplayHuman for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
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
      Self::Schema(_) => SchemaLocation::<()>::raw(),
      Self::Scalar(_) => ScalarLocation::<()>::raw(),
      Self::Object(_) => ObjectLocation::<()>::raw(),
      Self::FieldDefinition(_) => FieldDefinitionLocation::<()>::raw(),
      Self::ArgumentDefinition(_) => ArgumentDefinitionLocation::<()>::raw(),
      Self::Interface(_) => InterfaceLocation::<()>::raw(),
      Self::Union(_) => UnionLocation::<()>::raw(),
      Self::Enum(_) => EnumLocation::<()>::raw(),
      Self::EnumValue(_) => EnumValueLocation::<()>::raw(),
      Self::InputObject(_) => InputObjectLocation::<()>::raw(),
      Self::InputFieldDefinition(_) => InputFieldDefinitionLocation::<()>::raw(),
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

impl AsSpan<Span> for Location {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for Location {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Executable(loc) => loc.into_span(),
      Self::TypeSystem(loc) => loc.into_span(),
    }
  }
}

impl core::fmt::Display for Location {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Executable(loc) => core::fmt::Display::fmt(loc, f),
      Self::TypeSystem(loc) => core::fmt::Display::fmt(loc, f),
    }
  }
}

impl DisplayCompact for Location {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self, f)
  }
}

impl DisplayPretty for Location {
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayCompact::fmt(self, f, &())
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
        impl ::core::convert::From<$sub_variant> for Location {
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

/// Represents a collection of directive locations where a directive can be applied.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirectiveLocations<Location, Container = Vec<Location>> {
  span: Span,
  locations: Container,
  _m: PhantomData<Location>,
}

impl<Location, Container> AsSpan<Span> for DirectiveLocations<Location, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Location, Container> IntoSpan<Span> for DirectiveLocations<Location, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Location, Container> IntoComponents for DirectiveLocations<Location, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.locations)
  }
}

impl<Location, Container> DirectiveLocations<Location, Container> {
  /// Returns a reference to the span covering the entire directive locations.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding the directive locations.
  #[inline]
  pub const fn locations(&self) -> &Container {
    &self.locations
  }

  /// Returns a slice of the directive locations.
  #[inline]
  pub fn locations_slice(&self) -> &[Location]
  where
    Container: AsRef<[Location]>,
  {
    self.locations().as_ref()
  }
}

impl<'a, Location, Container, I, T, Error> Parseable<'a, I, T, Error>
  for DirectiveLocations<Location, Container>
where
  Container: chumsky::container::Container<Location> + 'a,
  Pipe: Parseable<'a, I, T, Error>,
  Location: Parseable<'a, I, T, Error> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
  {
    Location::parser()
      .separated_by(Pipe::parser())
      .allow_leading()
      .at_least(1)
      .collect()
      .map_with(|locations, exa| {
        let span = exa.span();
        Self {
          span,
          locations,
          _m: PhantomData,
        }
      })
  }
}

impl<Location, Container> DisplayCompact for DirectiveLocations<Location, Container>
where
  Container: AsRef<[Location]>,
  Location: DisplayCompact,
{
  type Options = Location::Options;

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, opts: &Self::Options) -> core::fmt::Result {
    let locations = self.locations().as_ref();

    for location in locations.iter() {
      write!(f, "|{}", location.display(opts))?;
    }
    Ok(())
  }
}

impl<Location, Container> DisplayPretty for DirectiveLocations<Location, Container>
where
  Container: AsRef<[Location]>,
  Location: DisplayPretty,
{
  type Options = Location::Options;

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, opts: &Self::Options) -> core::fmt::Result {
    let locations = self.locations().as_ref();

    for location in locations.iter() {
      writeln!(f, "\t| {}", location.display(opts))?;
    }
    Ok(())
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
pub struct DirectiveDefinition<Name, Args, Locations> {
  span: Span,
  name: Name,
  arguments_definition: Option<Args>,
  repeateable: bool,
  directive_locations: Locations,
}

impl<Name, Args, Locations> AsSpan<Span> for DirectiveDefinition<Name, Args, Locations> {
  #[inline]
  fn as_span(&self) -> &Span {
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
  /// Creates a new `DirectiveDefinition` with the given components.
  #[inline]
  pub const fn new(
    span: Span,
    name: Name,
    arguments_definition: Option<Args>,
    repeateable: bool,
    directive_locations: Locations,
  ) -> Self {
    Self {
      span,
      name,
      arguments_definition,
      repeateable,
      directive_locations,
    }
  }

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
  pub fn parser_with<'src, I, T, Error, E, NP, AP, LP>(
    name_parser: NP,
    args_parser: AP,
    directive_locations_parser: LP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
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
      .ignore_then(Self::content_parser_with(
        name_parser,
        args_parser,
        directive_locations_parser,
      ))
      .map_with(
        |(name, arguments_definition, repeateable, directive_locations), exa| {
          Self::new(
            exa.span(),
            name,
            arguments_definition,
            repeateable,
            directive_locations,
          )
        },
      )
  }

  /// Creates a parser for directive definitions without the leading `directive` keyword.
  pub fn content_parser_with<'src, I, T, Error, E, NP, AP, LP>(
    name_parser: NP,
    args_parser: AP,
    directive_locations_parser: LP,
  ) -> impl Parser<'src, I, (Name, Option<Args>, bool, Locations), E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    At: Parseable<'src, I, T, Error> + 'src,
    On: Parseable<'src, I, T, Error> + 'src,
    Repeatable: Parseable<'src, I, T, Error> + 'src,
    AP: Parser<'src, I, Args, E> + Clone,
    LP: Parser<'src, I, Locations, E> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    At::parser()
      .ignore_then(name_parser)
      .then(args_parser.or_not())
      .then(Repeatable::parser().or_not())
      .then_ignore(On::parser())
      .then(directive_locations_parser)
      .map(
        |(((name, arguments_definition), repeateable), directive_locations)| {
          (
            name,
            arguments_definition,
            repeateable.is_some(),
            directive_locations,
          )
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
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Name::parser(), Args::parser(), Locations::parser())
  }
}
