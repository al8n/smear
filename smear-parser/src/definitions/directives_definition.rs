use core::marker::PhantomData;

use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::super::{
  char::Char,
  keywords,
  language::{
    ignored::ignored,
    input_value::StringValue,
    punct::{At, Pipe},
  },
  name::Name,
  spanned::Spanned,
};

word!(
  /// `QUERY` location.
  QueryLocation: [I::Token::Q, I::Token::U, I::Token::E, I::Token::R, I::Token::Y],
  /// `MUTATION` location.
  MutationLocation: [I::Token::M, I::Token::U, I::Token::T, I::Token::A, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `SUBSCRIPTION` location.
  SubscriptionLocation: [I::Token::S, I::Token::U, I::Token::B, I::Token::S, I::Token::C, I::Token::R, I::Token::I, I::Token::P, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `FIELD_DEFINITION` location.
  FieldDefinitionLocation: [I::Token::F, I::Token::I, I::Token::E, I::Token::L, I::Token::D, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::A, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `FIELD` location.
  FieldLocation: [I::Token::F, I::Token::I, I::Token::E, I::Token::L, I::Token::D],
  /// `FRAGMENT_DEFINITION` location.
  FragmentDefinitionLocation: [I::Token::F, I::Token::R, I::Token::A, I::Token::G, I::Token::M, I::Token::E, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `FRAGMENT_SPREAD` location.
  FragmentSpreadLocation: [I::Token::F, I::Token::R, I::Token::A, I::Token::G, I::Token::M, I::Token::E, I::Token::UNDERSCORE, I::Token::S, I::Token::P, I::Token::R, I::Token::E, I::Token::A, I::Token::D],
  /// `INLINE_FRAGMENT` location.
  InlineFragmentLocation: [I::Token::I, I::Token::N, I::Token::L, I::Token::I, I::Token::N, I::Token::E, I::Token::UNDERSCORE, I::Token::F, I::Token::R, I::Token::A, I::Token::G, I::Token::M, I::Token::E],
  /// `VARIABLE_DEFINITION` location.
  VariableDefinitionLocation: [I::Token::V, I::Token::A, I::Token::R, I::Token::I, I::Token::A, I::Token::B, I::Token::L, I::Token::E, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `SCHEMA` location.
  SchemaLocation: [I::Token::S, I::Token::C, I::Token::H, I::Token::E, I::Token::M, I::Token::A],
  /// `SCALAR` location.
  ScalarLocation: [I::Token::S, I::Token::C, I::Token::A, I::Token::L, I::Token::A, I::Token::R],
  /// `OBJECT` location.
  ObjectLocation: [I::Token::O, I::Token::B, I::Token::J, I::Token::E, I::Token::C, I::Token::T],
  /// `ARGUMENT_DEFINITION` location.
  ArgumentDefinitionLocation: [I::Token::A, I::Token::R, I::Token::G, I::Token::U, I::Token::M, I::Token::E, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
  /// `INTERFACE` location.
  InterfaceLocation: [I::Token::I, I::Token::N, I::Token::T, I::Token::E, I::Token::R, I::Token::F, I::Token::A, I::Token::C, I::Token::E],
  /// `UNION` location.
  UnionLocation: [I::Token::U, I::Token::N, I::Token::I, I::Token::O, I::Token::N],
  /// `ENUM_VALUE` location.
  EnumValueLocation: [I::Token::E, I::Token::N, I::Token::U, I::Token::M, I::Token::UNDERSCORE, I::Token::V, I::Token::A, I::Token::L, I::Token::U, I::Token::E],
    /// `ENUM` location.
  EnumLocation: [I::Token::E, I::Token::N, I::Token::U, I::Token::M],
  /// `INPUT_OBJECT` location.
  InputObjectLocation: [I::Token::I, I::Token::N, I::Token::P, I::Token::U, I::Token::T, I::Token::UNDERSCORE, I::Token::O, I::Token::B, I::Token::J, I::Token::E, I::Token::C, I::Token::T],
  /// `INPUT_FIELD_DEFINITION` location.
  InputFieldDefinitionLocation: [I::Token::I, I::Token::N, I::Token::P, I::Token::U, I::Token::T, I::Token::UNDERSCORE, I::Token::F, I::Token::I, I::Token::E, I::Token::L, I::Token::D, I::Token::UNDERSCORE, I::Token::D, I::Token::E, I::Token::F, I::Token::I, I::Token::N, I::Token::I, I::Token::T, I::Token::I, I::Token::O, I::Token::N],
);

/// Executable directive location
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
pub enum ExecutableDirectiveLocation<Src, Span> {
  /// `QUERY`
  Query(QueryLocation<Src, Span>),
  /// `MUTATION`
  Mutation(MutationLocation<Src, Span>),
  /// `SUBSCRIPTION`
  Subscription(SubscriptionLocation<Src, Span>),
  /// `FIELD`
  Field(FieldLocation<Src, Span>),
  /// `FRAGMENT_DEFINITION`
  FragmentDefinition(FragmentDefinitionLocation<Src, Span>),
  /// `FRAGMENT_SPREAD`
  FragmentSpread(FragmentSpreadLocation<Src, Span>),
  /// `INLINE_FRAGMENT`
  InlineFragment(InlineFragmentLocation<Src, Span>),
  /// `VARIABLE_DEFINITION`
  VariableDefinition(VariableDefinitionLocation<Src, Span>),
}

impl<Src, Span> ExecutableDirectiveLocation<Src, Span> {
  /// Returns the span of the location.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
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

  /// Returns a parser to parse the location.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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

/// Type system directive location
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
pub enum TypeSystemDirectiveLocation<Src, Span> {
  /// `SCHEMA`
  Schema(SchemaLocation<Src, Span>),
  /// `SCALAR`
  Scalar(ScalarLocation<Src, Span>),
  /// `OBJECT`
  Object(ObjectLocation<Src, Span>),
  /// `FIELD_DEFINITION`
  FieldDefinition(FieldDefinitionLocation<Src, Span>),
  /// `ARGUMENT_DEFINITION`
  ArgumentDefinition(ArgumentDefinitionLocation<Src, Span>),
  /// `INTERFACE`
  Interface(InterfaceLocation<Src, Span>),
  /// `UNION`
  Union(UnionLocation<Src, Span>),
  /// `ENUM`
  Enum(EnumLocation<Src, Span>),
  /// `ENUM_VALUE`
  EnumValue(EnumValueLocation<Src, Span>),
  /// `INPUT_OBJECT`
  InputObject(InputObjectLocation<Src, Span>),
  /// `INPUT_FIELD_DEFINITION`
  InputFieldDefinition(InputFieldDefinitionLocation<Src, Span>),
}

impl<Src, Span> TypeSystemDirectiveLocation<Src, Span> {
  /// Returns the span of the location.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
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

  /// Returns a parser to parse the location.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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

/// Directive location
///
/// Represents the location of a directive in a GraphQL document.
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
pub enum Location<Src, Span> {
  /// Executable directive location
  Executable(ExecutableDirectiveLocation<Src, Span>),
  /// Type system directive location
  TypeSystem(TypeSystemDirectiveLocation<Src, Span>),
}

impl<Src, Span> Location<Src, Span> {
  /// Returns the span of the location.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    match self {
      Self::Executable(loc) => loc.span(),
      Self::TypeSystem(loc) => loc.span(),
    }
  }

  /// Returns a parser to parse the directive location.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
        impl<Src, Span> From<$sub_variant<Src, Span>> for Location<Src, Span> {
          fn from(location: $sub_variant<Src, Span>) -> Self {
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

#[derive(Debug, Clone, Copy)]
pub struct DirectiveLocation<Location, Src, Span> {
  /// The span of the location
  span: Spanned<Src, Span>,
  /// The location of the directive.
  location: Location,
  /// The pipe token.
  pipe: Option<Pipe<Src, Span>>,
}

impl<Location, Src, Span> DirectiveLocation<Location, Src, Span> {
  /// Returns the span of the directive location.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the location of the directive.
  #[inline]
  pub const fn location(&self) -> &Location {
    &self.location
  }

  /// Returns the pipe token if present.
  #[inline]
  pub const fn pipe(&self) -> Option<&Pipe<Src, Span>> {
    self.pipe.as_ref()
  }

  /// Returns a parser to parse the directive location.
  pub fn parser_with<'src, I, E>(
    location_parser: impl Parser<'src, I, Location, E> + Clone,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Pipe::parser()
      .or_not()
      .then_ignore(ignored())
      .then(location_parser)
      .map_with(|(pipe, location), sp| Self {
        span: Spanned::from(sp),
        location,
        pipe,
      })
  }
}

#[derive(Debug, Clone)]
pub struct DirectiveLocations<Location, Src, Span, Container = Vec<Location>> {
  /// The span of the directive locations.
  span: Spanned<Src, Span>,
  /// The directive locations.
  locations: Container,
  _location: PhantomData<Location>,
}

impl<Location, Src, Span, Container> DirectiveLocations<Location, Src, Span, Container> {
  /// Returns the span of the directive locations.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the directive locations.
  #[inline]
  pub const fn locations(&self) -> &Container {
    &self.locations
  }

  /// Returns a parser to parse the directive location.
  pub fn parser_with<'src, I, E>(
    directive_location_parser: impl Parser<'src, I, Location, E> + Clone,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    Container: chumsky::container::Container<Location>,
  {
    directive_location_parser
      .repeated()
      .at_least(1)
      .collect::<Container>()
      .map_with(|locs, sp| Self {
        span: Spanned::from(sp),
        locations: locs,
        _location: PhantomData,
      })
  }
}

/// The definition of the Directive
///
/// Spec: [DirectiveDefinition](https://spec.graphql.org/draft/#DirectiveDefinition)
#[derive(Debug, Clone)]
pub struct DirectiveDefinition<Args, Locations, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  keyword: keywords::Directive<Src, Span>,
  at: At<Src, Span>,
  name: Name<Src, Span>,
  arguments_definition: Option<Args>,
  repeateable: Option<keywords::Repeatable<Src, Span>>,
  on: keywords::On<Src, Span>,
  directive_locations: Locations,
}

impl<Args, Locations, Src, Span> DirectiveDefinition<Args, Locations, Src, Span> {
  /// Returns the span of the directive definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the at punctuation of the directive definition.
  #[inline]
  pub const fn at(&self) -> &At<Src, Span> {
    &self.at
  }

  /// Returns the description of the directive definition
  #[inline]
  pub const fn description(&self) -> &Option<StringValue<Src, Span>> {
    &self.description
  }

  /// Returns the directive keyword of the directive definition.
  #[inline]
  pub const fn directive(&self) -> &keywords::Directive<Src, Span> {
    &self.keyword
  }

  /// Returns the arguments definitions of the directive definition.
  #[inline]
  pub const fn arguments(&self) -> &Option<Args> {
    &self.arguments_definition
  }

  /// Returns the repeatable keyword of the directive definition.
  #[inline]
  pub const fn repeatable(&self) -> Option<&keywords::Repeatable<Src, Span>> {
    self.repeateable.as_ref()
  }

  /// Returns the on keyword of the directive definition.
  #[inline]
  pub const fn on(&self) -> &keywords::On<Src, Span> {
    &self.on
  }

  /// Returns the name of the derictive definition
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  /// Returns the directive locations of the directive definition.
  #[inline]
  pub const fn locations(&self) -> &Locations {
    &self.directive_locations
  }

  /// Returns a parser to parse the directive location.
  pub fn parser_with<'src, I, E>(
    locations_parser: impl Parser<'src, I, Locations, E> + Clone,
    args_parser: impl Parser<'src, I, Args, E> + Clone,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    // description? ~ 'directive' ~ '@' ~ name ~ arguments_definition? ~ repeatable? ~ 'on' ~ directive_locations
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(keywords::Directive::parser())
      .then_ignore(ignored())
      .then(At::parser())
      .then_ignore(ignored())
      .then(Name::parser())
      .then_ignore(ignored())
      .then(args_parser.or_not())
      .then_ignore(ignored())
      .then(keywords::Repeatable::parser().or_not())
      .then_ignore(ignored())
      .then(keywords::On::parser())
      .then_ignore(ignored())
      .then(locations_parser)
      .map_with(
        |(
          ((((((description, keyword), at), name), arguments_definition), repeateable), on),
          directive_locations,
        ),
         sp| {
          Self {
            span: Spanned::from(sp),
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
