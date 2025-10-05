use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::Span,
};

use crate::lexer::graphql::ast::{AstLexerErrors, AstTokenKind};

use super::super::*;

pub use crate::parser::value::StringValue;

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for StringValue<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
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
        Ok(match tok {
          AstToken::LitInlineStr(raw) => StringValue::new(span, raw.into()),
          AstToken::LitBlockStr(raw) => StringValue::new(span, raw.into()),
          tok => return Err(Error::unexpected_token(tok, AstTokenKind::String, span).into()),
        })
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
