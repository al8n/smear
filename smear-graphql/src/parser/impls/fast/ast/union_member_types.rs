use core::marker::PhantomData;

use chumsky::{Parser, extra::ParserExtra, prelude::*};
use logosky::{
  Parseable, Source, Token, Tokenizer,
  utils::{Span, sdl_display::DisplaySDL},
};
use smear_parser::lang::punctuator::{Equal, Pipe};

use super::Name;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionMemberTypes<S, Container = Vec<Name<S>>> {
  span: Span,
  eq: Equal,
  members: Container,
  _m: PhantomData<S>,
}

impl<S, Container> UnionMemberTypes<S, Container> {
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  #[inline]
  pub const fn eq(&self) -> &Equal {
    &self.eq
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
  Equal: Parseable<'a, I, T, Error>,
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
    Equal::parser()
      .then(Pipe::parser().or_not().ignored())
      .then(Name::<S>::parser().separated_by(Pipe::parser()).collect())
      .map_with(|((eq, _), names), exa| {
        let span = exa.span();
        Self {
          span,
          eq,
          members: names,
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
    write!(f, "=")?;
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
    write!(f, "=")?;
    let members = self.members().as_ref();

    for member in members.iter() {
      write!(f, "|{}", member.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    writeln!(f, "=")?;
    let members = self.members().as_ref();
    for member in members.iter() {
      writeln!(f, "\t| {}", member.display())?;
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::fast::{FastParserExtra, FastTokenStream};

  use super::*;

  #[test]
  fn test_union_member_types() {
    let parser = UnionMemberTypes::<&str>::parser::<FastParserExtra<&str>>();
    let ifs = parser
      .parse(FastTokenStream::new("=|Foo|Bar|Baz"))
      .into_result()
      .expect("should parse");
    assert_eq!(ifs.members().len(), 3);
    assert_eq!(*ifs.members()[0].source(), "Foo");
    assert_eq!(*ifs.members()[1].source(), "Bar");
    assert_eq!(*ifs.members()[2].source(), "Baz");

    let ifs = parser
      .parse(FastTokenStream::new("=Foo|Bar|Baz"))
      .into_result()
      .expect("should parse");
    assert_eq!(ifs.members().len(), 3);
    assert_eq!(*ifs.members()[0].source(), "Foo");
    assert_eq!(*ifs.members()[1].source(), "Bar");
    assert_eq!(*ifs.members()[2].source(), "Baz");
  }
}
