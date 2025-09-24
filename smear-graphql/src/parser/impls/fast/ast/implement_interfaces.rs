use core::marker::PhantomData;

use chumsky::{Parser, extra::ParserExtra, prelude::*};
use logosky::{
  Parseable, Source, Token, Tokenizer,
  utils::{Span, sdl_display::DisplaySDL},
};
use smear_parser::lang::punctuator::Ampersand;

use super::Name;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementInterfaces<S, Container = Vec<Name<S>>> {
  span: Span,
  interfaces: Container,
  _m: PhantomData<S>,
}

impl<S, Container> ImplementInterfaces<S, Container> {
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  #[inline]
  pub const fn interfaces(&self) -> &Container {
    &self.interfaces
  }
}

impl<'a, S, Container, I, T, Error> Parseable<'a, I, T, Error> for ImplementInterfaces<S, Container>
where
  Container: chumsky::container::Container<Name<S>>,
  Name<S>: Parseable<'a, I, T, Error>,
  Ampersand: Parseable<'a, I, T, Error>,
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
      .separated_by(Ampersand::parser())
      .allow_leading()
      .at_least(1)
      .collect()
      .map_with(|names, exa| {
        let span = exa.span();
        Self {
          span,
          interfaces: names,
          _m: PhantomData,
        }
      })
  }
}

impl<S, Container> DisplaySDL for ImplementInterfaces<S, Container>
where
  Container: AsRef<[Name<S>]>,
  Name<S>: DisplaySDL,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let interfaces = self.interfaces().as_ref();

    for (i, interface) in interfaces.iter().enumerate() {
      if i == 0 {
        write!(f, " {}", interface.display())?;
        continue;
      }
      write!(f, " & {}", interface.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_compact(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let interfaces = self.interfaces().as_ref();

    for interface in interfaces.iter() {
      write!(f, "&{}", interface.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let interfaces = self.interfaces().as_ref();
    for interface in interfaces.iter() {
      writeln!(f, "\t& {}", interface.display())?;
    }
    Ok(())
  }
}
