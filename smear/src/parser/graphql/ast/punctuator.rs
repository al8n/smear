use crate::{lexer::graphql::syntactic::SyntacticLexerErrors, punctuator::*};
use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
};

use super::*;

macro_rules! punctuator_parser {
  ($($name:ident),+$(,)?) => {
    $(
      impl<'a, S> Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> for $name
      where
        SyntacticToken<S>: Token<'a>,
        <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
        <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
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
                SyntacticToken::$name => Ok($name::new(span)),
                tok => Err(Error::unexpected_token(tok, Expectation::$name, span).into()),
              }
            },
            Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
          })
        }
      }
    )*
  };
}

punctuator_parser! {
  At,
  Ampersand,
  Bang,
  Colon,
  Dollar,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  LParen,
  RParen,
  Spread,
  Pipe,
  Equal,
}
