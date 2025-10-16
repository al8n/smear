use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{Span, cmp::Equivalent},
};

use crate::lexer::graphql::ast::AstLexerErrors;

use super::super::*;

pub use crate::parser::value::BooleanValue;

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for BooleanValue
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, SyntacticToken<_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          SyntacticToken::Identifier(ident) => Ok(match () {
            () if "true".equivalent(&ident) => BooleanValue::new(span, true),
            () if "false".equivalent(&ident) => BooleanValue::new(span, false),
            _ => return Err(Error::invalid_boolean_value(ident, span).into()),
          }),
          tok => Err(Error::unexpected_token(tok, Expectation::BooleanValue, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
