use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};
use smear_parser::lang::v2::keyword::{self, *};

use crate::error::Error;

use super::*;

macro_rules! keyword_parser {
  ($($name:ty:$kw:literal),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>> for $name {
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
                Token::Identifier(name) => if name.eq($kw) {
                  Ok(<$name>::new(span))
                } else {
                  Err(Error::unexpected_keyword(name, $kw, span).into())
                },
                tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
              }
            },
            Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
          })
        }
      }
    )*
  };
}

keyword_parser! {
  On:"on",
  Fragment:"fragment",
  Query:"query",
  Mutation:"mutation",
  Subscription:"subscription",
  Type:"type",
  Interface:"interface",
  Union:"union",
  Enum:"enum",
  Input:"input",
  Implements:"implements",
  Extend:"extend",
  keyword::Directive:"directive",
  Schema:"schema",
  Scalar:"scalar",
  Repeatable:"repeatable",
}
