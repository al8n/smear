use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{Span, cmp::Equivalent},
};

use crate::lexer::graphqlx::ast::{AstLexerErrors, AstTokenKind};

use super::super::*;

pub use crate::parser::value::EnumValue;

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for EnumValue<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<S>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(name) => match () {
            () if "true".equivalent(&name)
              || "false".equivalent(&name)
              || "null".equivalent(&name) =>
            {
              Err(Error::invalid_enum_value(name, span).into())
            }
            _ => Ok(EnumValue::new(span, name)),
          },
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
