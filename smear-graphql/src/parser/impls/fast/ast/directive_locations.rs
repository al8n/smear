use chumsky::{Parser, extra::ParserExtra, prelude::*};
use logosky::{
  Lexed, Parseable,
  utils::{Span, sdl_display::DisplaySDL},
};
use smear_parser::{
  definitions::minized::{
    self, ExecutableDirectiveLocation, Location, TypeSystemDirectiveLocation,
  },
  lang::punctuator::Pipe,
};

use crate::error::Error;

use super::*;

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for Location
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          FastToken::Identifier(name) => Ok(match name {
            "QUERY" => Self::Executable(ExecutableDirectiveLocation::Query(
              minized::QueryLocation::new(span),
            )),
            "MUTATION" => Self::Executable(ExecutableDirectiveLocation::Mutation(
              minized::MutationLocation::new(span),
            )),
            "SUBSCRIPTION" => Self::Executable(ExecutableDirectiveLocation::Subscription(
              minized::SubscriptionLocation::new(span),
            )),
            "FIELD_DEFINITION" => Self::TypeSystem(TypeSystemDirectiveLocation::FieldDefinition(
              minized::FieldDefinitionLocation::new(span),
            )),
            "FIELD" => Self::Executable(ExecutableDirectiveLocation::Field(
              minized::FieldLocation::new(span),
            )),
            "FRAGMENT_DEFINITION" => {
              Self::Executable(ExecutableDirectiveLocation::FragmentDefinition(
                minized::FragmentDefinitionLocation::new(span),
              ))
            }
            "FRAGMENT_SPREAD" => Self::Executable(ExecutableDirectiveLocation::FragmentSpread(
              minized::FragmentSpreadLocation::new(span),
            )),
            "INLINE_FRAGMENT" => Self::Executable(ExecutableDirectiveLocation::InlineFragment(
              minized::InlineFragmentLocation::new(span),
            )),
            "VARIABLE_DEFINITION" => {
              Self::Executable(ExecutableDirectiveLocation::VariableDefinition(
                minized::VariableDefinitionLocation::new(span),
              ))
            }
            "SCHEMA" => Self::TypeSystem(TypeSystemDirectiveLocation::Schema(
              minized::SchemaLocation::new(span),
            )),
            "SCALAR" => Self::TypeSystem(TypeSystemDirectiveLocation::Scalar(
              minized::ScalarLocation::new(span),
            )),
            "OBJECT" => Self::TypeSystem(TypeSystemDirectiveLocation::Object(
              minized::ObjectLocation::new(span),
            )),

            "ARGUMENT_DEFINITION" => {
              Self::TypeSystem(TypeSystemDirectiveLocation::ArgumentDefinition(
                minized::ArgumentDefinitionLocation::new(span),
              ))
            }
            "INTERFACE" => Self::TypeSystem(TypeSystemDirectiveLocation::Interface(
              minized::InterfaceLocation::new(span),
            )),
            "UNION" => Self::TypeSystem(TypeSystemDirectiveLocation::Union(
              minized::UnionLocation::new(span),
            )),
            "ENUM_VALUE" => Self::TypeSystem(TypeSystemDirectiveLocation::EnumValue(
              minized::EnumValueLocation::new(span),
            )),
            "ENUM" => Self::TypeSystem(TypeSystemDirectiveLocation::Enum(
              minized::EnumLocation::new(span),
            )),
            "INPUT_OBJECT" => Self::TypeSystem(TypeSystemDirectiveLocation::InputObject(
              minized::InputObjectLocation::new(span),
            )),
            "INPUT_FIELD_DEFINITION" => {
              Self::TypeSystem(TypeSystemDirectiveLocation::InputFieldDefinition(
                minized::InputFieldDefinitionLocation::new(span),
              ))
            }
            val => return Err(Error::unknown_directive_location(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, FastTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirectiveLocations<Container = Vec<Location>> {
  span: Span,
  locations: Container,
}

impl<Container> DirectiveLocations<Container> {
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  #[inline]
  pub const fn locations(&self) -> &Container {
    &self.locations
  }
}

impl<'a, Container> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for DirectiveLocations<Container>
where
  Container: chumsky::container::Container<Location>,
  Pipe: Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    Location::parser()
      .separated_by(Pipe::parser())
      .allow_leading()
      .at_least(1)
      .collect()
      .map_with(|locations, exa| {
        let span = exa.span();
        Self { span, locations }
      })
  }
}

impl<Container> DisplaySDL for DirectiveLocations<Container>
where
  Container: AsRef<[Location]>,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let locations = self.locations().as_ref();

    for (i, location) in locations.iter().enumerate() {
      if i == 0 {
        write!(f, " {}", location.display())?;
        continue;
      }
      write!(f, " | {}", location.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_compact(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let locations = self.locations().as_ref();

    for location in locations.iter() {
      write!(f, "|{}", location.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let locations = self.locations().as_ref();
    for location in locations.iter() {
      writeln!(f, "\t| {}", location.display())?;
    }
    Ok(())
  }
}
