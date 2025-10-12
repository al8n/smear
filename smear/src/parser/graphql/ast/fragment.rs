use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{IntoComponents, Span, cmp::Equivalent},
};

use crate::{
  lexer::graphql::ast::AstLexerErrors,
  scaffold::{self, FragmentName},
};

use super::*;

pub type TypeCondition<S> = scaffold::TypeCondition<Name<S>>;

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for FragmentName<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: logosky::utils::cmp::Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(name) => {
            if "on".equivalent(&name) {
              Err(Error::invalid_fragment_name(name, span).into())
            } else {
              Ok(FragmentName::new(span, name))
            }
          }
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(AstTokenError::from_lexer_errors(err, span).into()),
    })
  }
}

impl<S> From<FragmentName<S>> for Name<S> {
  #[inline]
  fn from(value: FragmentName<S>) -> Self {
    let (span, value) = value.into_components();
    Self::new(span, value)
  }
}
