use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
};
use smear_parser::lang::{self, FragmentName, Name};

use crate::error::Error;

use super::*;

pub type TypeCondition<S> = lang::TypeCondition<Name<S>>;

pub type FragmentSpread<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  Container = Vec<Directive<S, ArgumentContainer>>,
> = lang::FragmentSpread<FragmentName<S>, Directives<S, ArgumentContainer, Container>>;

pub type InlineFragment<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = lang::InlineFragment<
  TypeCondition<S>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for FragmentName<&'a str>
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
          AstToken::Identifier(name) => {
            if name.eq("on") {
              Err(Error::invalid_fragment_name(name, span).into())
            } else {
              Ok(FragmentName::new(span, name))
            }
          }
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
