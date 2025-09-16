use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{error::Error, parser::punctuator::*};

use super::*;

macro_rules! punctuator_parser {
  ($($name:ident),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>> for $name {
        type Error = LosslessTokenErrors<'a>;

        #[inline]
        fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
        where
          Self: Sized,
          E: ParserExtra<'a, LosslessTokenStream<'a>, Error = Self::Error> + 'a,
        {
          any().try_map(|res, span: Span| match res {
            Lexed::Token(tok) => match tok {
              Token::$name => Ok($name::new(span)),
              tok => Err(Error::unexpected_token(tok, TokenKind::$name, span).into()),
            },
            Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
          })
        }
      }
    )*
  };
}

punctuator_parser! {
  At,
  Ampersand,
  Bang,
  Colon,
  Comma,
  Dollar,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  LParen,
  RParen,
}
