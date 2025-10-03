use core::marker::PhantomData;
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, sdl_display::DisplaySDL},
};

use crate::{
  keywords::*,
  punctuator::{At, Pipe},
};

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
  pub fn as_slice(&self) -> &[Location]
  where
    Container: AsRef<[Location]>,
  {
    self.locations.as_ref()
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

impl<Location, Container> DisplaySDL for DirectiveLocations<Location, Container>
where
  Container: AsRef<[Location]>,
  Location: DisplaySDL,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let locations = self.locations().as_ref();

    for (i, location) in locations.iter().enumerate() {
      if i == 0 {
        write!(f, " {}", DisplaySDL::display(location))?;
        continue;
      }
      write!(f, " | {}", DisplaySDL::display(location))?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_compact(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let locations = self.locations().as_ref();

    for location in locations.iter() {
      write!(f, "|{}", DisplaySDL::display_compact(location))?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let locations = self.locations().as_ref();
    for location in locations.iter() {
      writeln!(f, "\t| {}", DisplaySDL::display_pretty(location))?;
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
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Name::parser(), Args::parser(), Locations::parser())
  }
}
