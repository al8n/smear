use logosky::{
  Lexed, Parseable,
  chumsky::{self, Parser, extra::ParserExtra, prelude::*},
  utils::{Span, sdl_display::DisplaySDL},
};
use smear_parser::{
  definitions::ast::{self, ExecutableDirectiveLocation, Location, TypeSystemDirectiveLocation},
  lang::punctuator::Pipe,
};

use crate::error::Error;

use super::*;

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>> for Location {
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a>, Error = AstTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<'_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(name) => Ok(match name {
            "QUERY" => Self::Executable(ExecutableDirectiveLocation::Query(
              ast::QueryLocation::new(span),
            )),
            "MUTATION" => Self::Executable(ExecutableDirectiveLocation::Mutation(
              ast::MutationLocation::new(span),
            )),
            "SUBSCRIPTION" => Self::Executable(ExecutableDirectiveLocation::Subscription(
              ast::SubscriptionLocation::new(span),
            )),
            "FIELD_DEFINITION" => Self::TypeSystem(TypeSystemDirectiveLocation::FieldDefinition(
              ast::FieldDefinitionLocation::new(span),
            )),
            "FIELD" => Self::Executable(ExecutableDirectiveLocation::Field(
              ast::FieldLocation::new(span),
            )),
            "FRAGMENT_DEFINITION" => {
              Self::Executable(ExecutableDirectiveLocation::FragmentDefinition(
                ast::FragmentDefinitionLocation::new(span),
              ))
            }
            "FRAGMENT_SPREAD" => Self::Executable(ExecutableDirectiveLocation::FragmentSpread(
              ast::FragmentSpreadLocation::new(span),
            )),
            "INLINE_FRAGMENT" => Self::Executable(ExecutableDirectiveLocation::InlineFragment(
              ast::InlineFragmentLocation::new(span),
            )),
            "VARIABLE_DEFINITION" => {
              Self::Executable(ExecutableDirectiveLocation::VariableDefinition(
                ast::VariableDefinitionLocation::new(span),
              ))
            }
            "SCHEMA" => Self::TypeSystem(TypeSystemDirectiveLocation::Schema(
              ast::SchemaLocation::new(span),
            )),
            "SCALAR" => Self::TypeSystem(TypeSystemDirectiveLocation::Scalar(
              ast::ScalarLocation::new(span),
            )),
            "OBJECT" => Self::TypeSystem(TypeSystemDirectiveLocation::Object(
              ast::ObjectLocation::new(span),
            )),

            "ARGUMENT_DEFINITION" => {
              Self::TypeSystem(TypeSystemDirectiveLocation::ArgumentDefinition(
                ast::ArgumentDefinitionLocation::new(span),
              ))
            }
            "INTERFACE" => Self::TypeSystem(TypeSystemDirectiveLocation::Interface(
              ast::InterfaceLocation::new(span),
            )),
            "UNION" => Self::TypeSystem(TypeSystemDirectiveLocation::Union(
              ast::UnionLocation::new(span),
            )),
            "ENUM_VALUE" => Self::TypeSystem(TypeSystemDirectiveLocation::EnumValue(
              ast::EnumValueLocation::new(span),
            )),
            "ENUM" => Self::TypeSystem(TypeSystemDirectiveLocation::Enum(ast::EnumLocation::new(
              span,
            ))),
            "INPUT_OBJECT" => Self::TypeSystem(TypeSystemDirectiveLocation::InputObject(
              ast::InputObjectLocation::new(span),
            )),
            "INPUT_FIELD_DEFINITION" => {
              Self::TypeSystem(TypeSystemDirectiveLocation::InputFieldDefinition(
                ast::InputFieldDefinitionLocation::new(span),
              ))
            }
            val => return Err(Error::unknown_directive_location(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
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

impl<'a, Container> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for DirectiveLocations<Container>
where
  Container: chumsky::container::Container<Location>,
  Pipe: Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a>, Error = AstTokenErrors<'a, &'a str>> + 'a,
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
