use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
};
use smear_parser::lang::punctuator::*;

use crate::error::Error;

use super::*;

macro_rules! punctuator_parser {
  ($($name:ident),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, StrAstTokenStream<'a>, StrAstToken<'a>, StrAstTokenErrors<'a, &'a str>> for $name {
        #[inline]
        fn parser<E>() -> impl Parser<'a, StrAstTokenStream<'a>, Self, E> + Clone
        where
          Self: Sized,
          E: ParserExtra<'a, StrAstTokenStream<'a>, Error = StrAstTokenErrors<'a, &'a str>> + 'a,
        {
          any().try_map(|res: Lexed<'_, StrAstToken<'_>>, span: Span| match res {
            Lexed::Token(tok) => {
              let (span, tok) = tok.into_components();
              match tok {
                StrAstToken::$name => Ok($name::new(span)),
                tok => Err(Error::unexpected_token(tok, StrAstTokenKind::$name, span).into()),
              }
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
  Dollar,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  LParen,
  RParen,
  Spread,
  Pipe,
  Equal,
}
