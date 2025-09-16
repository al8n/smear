use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable, TokenStream, Tokenizer};

use crate::{
  error::{Error, Errors},
  parser::punctuator::*,
};

use super::*;

macro_rules! punctuator_parser {
  ($($name:ident),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, TokenStream<'a, Token<'a>>> for $name {
        type Token = Token<'a>;
        type Error = Errors<'a, Token<'a>, TokenKind, char, RecursionLimitExceeded>;

        #[inline]
        fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E> + Clone
        where
          Self: Sized,
          TokenStream<'a, Token<'a>>: Tokenizer<'a, Self::Token>,
          E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error> + 'a,
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
  Dollar,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  LParen,
  RParen,
}
