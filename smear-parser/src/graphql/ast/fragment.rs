use logosky::{
  Lexed, Logos, Token,
  chumsky::{Parseable, Parser, extra::ParserExtra, prelude::any},
  utils::{IntoComponents, Span, cmp::Equivalent},
};
use smear_lexer::graphql::syntactic::SyntacticLexerErrors;
use smear_scaffold::ast::{self as scaffold, FragmentName};

use super::*;

/// A type condition for a fragment, specifying the type it applies to.
pub type TypeCondition<S> = scaffold::TypeCondition<Name<S>>;

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for FragmentName<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: logosky::utils::cmp::Equivalent<S>,
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
          SyntacticToken::Identifier(name) => {
            if "on".equivalent(&name) {
              Err(Error::invalid_fragment_name(name, span).into())
            } else {
              Ok(FragmentName::new(span, name))
            }
          }
          tok => Err(Error::unexpected_token(tok, Expectation::FragmentName, span).into()),
        }
      }
      Lexed::Error(err) => Err(SyntacticTokenError::from_lexer_errors(err, span).into()),
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
