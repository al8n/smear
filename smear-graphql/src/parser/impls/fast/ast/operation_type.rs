use chumsky::{Parser, extra::ParserExtra, prelude::*};
use logosky::{Lexed, Parseable, utils::Span};
use smear_parser::{
  definitions::minized::OperationType,
  lang::minized::keywords::{Mutation, Query, Subscription},
};

use crate::error::Error;

use super::{Token as FastToken, *};

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for OperationType
{
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
          FastToken::Identifier(name) => Ok(match name {
            "query" => Self::Query(Query::new(span)),
            "mutation" => Self::Mutation(Mutation::new(span)),
            "subscription" => Self::Subscription(Subscription::new(span)),
            val => return Err(Error::unknown_operation_type(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
