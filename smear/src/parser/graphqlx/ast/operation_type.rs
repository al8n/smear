use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  utils::{Span, cmp::Equivalent},
};

use crate::{
  keywords::{Mutation, Query, Subscription},
  lexer::graphqlx::ast::SyntacticLexerErrors,
  scaffold::OperationType,
};

use super::*;

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for OperationType
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, SyntacticToken<S>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          SyntacticToken::Identifier(name) => Ok(match () {
            () if "query".equivalent(&name) => Self::Query(Query::new(span)),
            () if "mutation".equivalent(&name) => Self::Mutation(Mutation::new(span)),
            () if "subscription".equivalent(&name) => Self::Subscription(Subscription::new(span)),
            _ => return Err(Error::unknown_operation_type(name, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, Expectation::OperationName, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
