use chumsky::{Parser, extra::ParserExtra};
use logosky::Parseable;
use smear_parser::lang::minized::Object;

use crate::{error::Error, parser::ast::Name};

use super::*;

impl<'a, V> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for Object<Name<&'a str>, V>
where
  V: Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    Object::parser_with(Name::parser(), V::parser(), |span| {
      Error::unclosed_object(span).into()
    })
  }
}
