use crate::{lexer::graphqlx::ast::AstLexerErrors, punctuator::*};
use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::Span,
};

use super::*;

macro_rules! punctuator_parser {
  ($($name:ident),+$(,)?) => {
    $(
      impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> for $name
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
          any().try_map(|res: Lexed<'_, AstToken<S>>, span: Span| match res {
            Lexed::Token(tok) => {
              let (span, tok) = tok.into_components();
              match tok {
                AstToken::$name => Ok($name::new(span)),
                tok => Err(Error::unexpected_token(tok, AstTokenKind::$name, span).into()),
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
  LAngle,
  RAngle,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  LParen,
  RParen,
  Spread,
  Pipe,
  Equal,
  PathSeparator,
  FatArrow,
}
