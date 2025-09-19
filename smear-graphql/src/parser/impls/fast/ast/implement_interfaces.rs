use core::marker::PhantomData;

use chumsky::{Parser, extra::ParserExtra, prelude::*};
use logosky::{
  Parseable, Source, Token, Tokenizer,
  utils::{Span, sdl_display::DisplaySDL},
};
use smear_parser::lang::{keywords2::Implements, punctuator::Ampersand};

use super::Name;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementInterfaces<S, Container = Vec<Name<S>>> {
  span: Span,
  implements: Implements,
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
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Error: 'a,
  Container: chumsky::container::Container<Name<S>>,
  Name<S>: Parseable<'a, I, T, Error>,
  Ampersand: Parseable<'a, I, T, Error>,
  Implements: Parseable<'a, I, T, Error>,
  T: Token<'a>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Implements::parser()
      .then(Ampersand::parser().or_not().ignored())
      .then(
        Name::<S>::parser()
          .separated_by(Ampersand::parser())
          .collect(),
      )
      .map_with(|((implements, _), names), exa| {
        let span = exa.span();
        Self {
          span,
          implements,
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
    write!(f, "implements")?;
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
    write!(f, "implements")?;
    let interfaces = self.interfaces().as_ref();

    for interface in interfaces.iter() {
      write!(f, "&{}", interface.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    writeln!(f, "implements")?;
    let interfaces = self.interfaces().as_ref();
    for interface in interfaces.iter() {
      writeln!(f, "\t& {}", interface.display())?;
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
    let parser = ImplementInterfaces::<&str>::parser::<FastParserExtra<&str>>();
    let ifs = parser
      .parse(FastTokenStream::new(" &  Foo & Bar & Baz "))
      .into_result()
      .expect("should parse");
    assert_eq!(ifs.interfaces().len(), 3);
    assert_eq!(*ifs.interfaces()[0].source(), "Foo");
    assert_eq!(*ifs.interfaces()[1].source(), "Bar");
    assert_eq!(*ifs.interfaces()[2].source(), "Baz");

    let ifs = parser
      .parse(FastTokenStream::new(" Foo & Bar & Baz "))
      .into_result()
      .expect("should parse");
    assert_eq!(ifs.interfaces().len(), 3);
    assert_eq!(*ifs.interfaces()[0].source(), "Foo");
    assert_eq!(*ifs.interfaces()[1].source(), "Bar");
    assert_eq!(*ifs.interfaces()[2].source(), "Baz");
  }
}
