use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable, TokenStream, Tokenizer};

use crate::{
  error::{Error, Errors},
  parser::punctuator::{Dollar, LBracket, RBracket},
};

use super::*;

impl<'a> Parseable<'a, TokenStream<'a, Token<'a>>> for LBracket {
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, LimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E>
  where
    Self: Sized,
    TokenStream<'a, Token<'a>>: Tokenizer<'a, Self::Token>,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error>,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::BracketOpen => Ok(LBracket::new(span)),
        tok => Err(Error::unexpected_token(tok, TokenKind::BracketOpen, span).into()),
      },
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

impl<'a> Parseable<'a, TokenStream<'a, Token<'a>>> for RBracket {
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, LimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E>
  where
    Self: Sized,
    TokenStream<'a, Token<'a>>: Tokenizer<'a, Self::Token>,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error>,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::BracketClose => Ok(RBracket::new(span)),
        tok => Err(Error::unexpected_token(tok, TokenKind::BracketClose, span).into()),
      },
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

impl<'a> Parseable<'a, TokenStream<'a, Token<'a>>> for Dollar {
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, LimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E>
  where
    Self: Sized,
    TokenStream<'a, Token<'a>>: Tokenizer<'a, Self::Token>,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error>,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Dollar => Ok(Dollar::new(span)),
        tok => Err(Error::unexpected_token(tok, TokenKind::Dollar, span).into()),
      },
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
