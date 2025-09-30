use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  utils::Span,
};
use smear_parser::{
  definitions::ast::OperationType,
  lang::keywords::{Mutation, Query, Subscription},
};

use crate::error::Error;

use super::*;

impl<'a> Parseable<'a, StrAstTokenStream<'a>, StrAstToken<'a>, StrAstTokenErrors<'a, &'a str>>
  for OperationType
{
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
          StrAstToken::Identifier(name) => Ok(match name {
            "query" => Self::Query(Query::new(span)),
            "mutation" => Self::Mutation(Mutation::new(span)),
            "subscription" => Self::Subscription(Subscription::new(span)),
            val => return Err(Error::unknown_operation_type(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, StrAstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
