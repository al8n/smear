use logosky::{
  Lexed, Logos, Token,
  chumsky::{Parseable, Parser, extra::ParserExtra, prelude::any},
  utils::{Span, cmp::Equivalent},
};
use smear_lexer::graphql::syntactic::SyntacticLexerErrors;

use super::super::*;

pub use crate::value::EnumValue;

impl<'a, S> Parseable<'a, SyntacticTokenizer<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for EnumValue<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenizer<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenizer<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, SyntacticToken<S>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          SyntacticToken::Identifier(name) => match () {
            () if "true".equivalent(&name)
              || "false".equivalent(&name)
              || "null".equivalent(&name) =>
            {
              Err(Error::invalid_enum_value(name, span).into())
            }
            _ => Ok(EnumValue::new(span, name)),
          },
          tok => Err(Error::unexpected_token(tok, Expectation::EnumValue, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
