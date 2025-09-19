use chumsky::{Parser, extra::ParserExtra, prelude::*};
use logosky::{
  Lexed, Parseable, utils::{Span, sdl_display::DisplaySDL}
};
use smear_parser::{definitions::v2::{self, ExecutableDirectiveLocation, Location, TypeSystemDirectiveLocation}, lang::punctuator::Pipe};

use crate::error::Error;

use super::{*, Token as FastToken};

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
              v2::QueryLocation::new(span)
            )),
            "MUTATION" => Self::Executable(ExecutableDirectiveLocation::Mutation(
              v2::MutationLocation::new(span)
            )),
            "SUBSCRIPTION" => Self::Executable(
              ExecutableDirectiveLocation::Subscription(
               v2::SubscriptionLocation::new(span)
              ),
            ),
            "FIELD" => Self::Executable(ExecutableDirectiveLocation::Field(
              v2::FieldLocation::new(span)
            )),
            "FRAGMENT_DEFINITION" => Self::Executable(
              ExecutableDirectiveLocation::FragmentDefinition(v2::FragmentDefinitionLocation::new(span)),
            ),
            "FRAGMENT_SPREAD" => Self::Executable(
              ExecutableDirectiveLocation::FragmentSpread(v2::FragmentSpreadLocation::new(span)),
            ),
            "INLINE_FRAGMENT" => Self::Executable(
              ExecutableDirectiveLocation::InlineFragment(v2::InlineFragmentLocation::new(span)),
            ),
            "SCHEMA" => Self::TypeSystem(TypeSystemDirectiveLocation::Schema(
              v2::SchemaLocation::new(span)
            )),
            "SCALAR" => Self::TypeSystem(TypeSystemDirectiveLocation::Scalar(
              v2::ScalarLocation::new(span)
            )),
            "OBJECT" => Self::TypeSystem(TypeSystemDirectiveLocation::Object(
              v2::ObjectLocation::new(span)
            )),
            "FIELD_DEFINITION" => Self::TypeSystem(
              TypeSystemDirectiveLocation::FieldDefinition(v2::FieldDefinitionLocation::new(span)),
            ),
            "ARGUMENT_DEFINITION" => Self::TypeSystem(
              TypeSystemDirectiveLocation::ArgumentDefinition(v2::ArgumentDefinitionLocation::new(span)),
            ),
            "INTERFACE" => Self::TypeSystem(
              TypeSystemDirectiveLocation::Interface(v2::InterfaceLocation::new(span)),
            ),
            "UNION" => Self::TypeSystem(TypeSystemDirectiveLocation::Union(
              v2::UnionLocation::new(span),
            )),
            "ENUM" => Self::TypeSystem(TypeSystemDirectiveLocation::Enum(
              v2::EnumLocation::new(span),
            )),
            "ENUM_VALUE" => Self::TypeSystem(
              TypeSystemDirectiveLocation::EnumValue(v2::EnumValueLocation::new(span)),
            ),
            "INPUT_OBJECT" => Self::TypeSystem(
              TypeSystemDirectiveLocation::InputObject(v2::InputObjectLocation::new(span)),
            ),
            "INPUT_FIELD_DEFINITION" => Self::TypeSystem(
              TypeSystemDirectiveLocation::InputFieldDefinition(v2::InputFieldDefinitionLocation::new(span)),
            ),
            val => return Err(Error::unknown_directive_location(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
        }
      },
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

impl<'a, Container> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>> for DirectiveLocations<Container>
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
    Pipe::parser().or_not().ignored()
      .then(
        Location::parser()
          .separated_by(Pipe::parser())
          .collect(),
      )
      .map_with(|(_, locations), exa| {
        let span = exa.span();
        Self {
          span,
          locations,
        }
      })
  }
}

impl<Container> DisplaySDL for DirectiveLocations<Container>
where
  Container: AsRef<[Location]>,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "implements")?;
    let locations = self.locations().as_ref();

    for (i, location) in locations.iter().enumerate() {
      if i == 0 {
        write!(f, " {}", location.display())?;
        continue;
      }
      write!(f, " & {}", location.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_compact(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "implements")?;
    let locations = self.locations().as_ref();

    for location in locations.iter() {
      write!(f, "&{}", location.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    writeln!(f, "implements")?;
    let locations = self.locations().as_ref();
    for location in locations.iter() {
      writeln!(f, "\t| {}", location.display())?;
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::fast::{FastParserExtra, FastTokenStream};

  use super::*;

  #[test]
  fn test_implement_interfaces() {
    let parser = DirectiveLocations::parser::<FastParserExtra<&str>>();
    let ifs = parser
      .parse(FastTokenStream::new(" |  Foo | Bar | Baz "))
      .into_result()
      .expect("should parse");
    assert_eq!(ifs.locations().len(), 3);
    assert_eq!(*ifs.locations()[0].source(), "Foo");
    assert_eq!(*ifs.locations()[1].source(), "Bar");
    assert_eq!(*ifs.locations()[2].source(), "Baz");

    let ifs = parser
      .parse(FastTokenStream::new(" Foo & Bar & Baz "))
      .into_result()
      .expect("should parse");
    assert_eq!(ifs.locations().len(), 3);
    assert_eq!(*ifs.locations()[0].source(), "Foo");
    assert_eq!(*ifs.locations()[1].source(), "Bar");
    assert_eq!(*ifs.locations()[2].source(), "Baz");
  }
}
