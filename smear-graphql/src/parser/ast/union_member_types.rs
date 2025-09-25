use core::marker::PhantomData;

use logosky::{
  Parseable, Source, Token, Tokenizer,
  chumsky::{self, Parser, extra::ParserExtra, prelude::*},
  utils::{Span, sdl_display::DisplaySDL},
};
use smear_parser::lang::punctuator::Pipe;

use super::Name;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionMemberTypes<S, Container = Vec<Name<S>>> {
  span: Span,
  members: Container,
  _m: PhantomData<S>,
}

impl<S, Container> UnionMemberTypes<S, Container> {
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  #[inline]
  pub const fn members(&self) -> &Container {
    &self.members
  }
}

impl<'a, S, Container, I, T, Error> Parseable<'a, I, T, Error> for UnionMemberTypes<S, Container>
where
  Container: chumsky::container::Container<Name<S>>,
  Name<S>: Parseable<'a, I, T, Error>,
  Pipe: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
    T: Token<'a>,
  {
    Name::<S>::parser()
      .separated_by(Pipe::parser())
      .allow_leading()
      .at_least(1)
      .collect()
      .map_with(|members, exa| {
        let span = exa.span();
        Self {
          span,
          members,
          _m: PhantomData,
        }
      })
  }
}

impl<S, Container> DisplaySDL for UnionMemberTypes<S, Container>
where
  Container: AsRef<[Name<S>]>,
  Name<S>: DisplaySDL,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let members = self.members().as_ref();

    for (i, member) in members.iter().enumerate() {
      if i == 0 {
        write!(f, " {}", member.display())?;
        continue;
      }
      write!(f, " | {}", member.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_compact(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let members = self.members().as_ref();

    for member in members.iter() {
      write!(f, "|{}", member.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let members = self.members().as_ref();
    for member in members.iter() {
      writeln!(f, "\t| {}", member.display())?;
    }
    Ok(())
  }
}
