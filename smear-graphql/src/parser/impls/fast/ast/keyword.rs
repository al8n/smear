use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};
use smear_parser::lang::v2::On;

use crate::error::Error;

use super::*;

macro_rules! keyword_parser {
  ($($name:ident:$kw:literal),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>> for $name {
        #[inline]
        fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
        where
          Self: Sized,
          E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
        {
          any().try_map(|res, span: Span| match res {
            Lexed::Token(tok) => match tok {
              Token::Identifier(name) => if name.eq($kw) {
                Ok($name::new(span))
              } else {
                Err(Error::unexpected_keyword(name, $kw, span).into())
              },
              tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
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
}
