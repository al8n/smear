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

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for OperationType
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a>, Error = AstTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<'_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(name) => Ok(match name {
            "query" => Self::Query(Query::new(span)),
            "mutation" => Self::Mutation(Mutation::new(span)),
            "subscription" => Self::Subscription(Subscription::new(span)),
            val => return Err(Error::unknown_operation_type(val, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
