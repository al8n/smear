use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  span::Spanned,
};
use smear_lexer::punctuator::*;

use super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphqlx::syntactic::{SyntacticLexer, SyntacticToken};

macro_rules! punctuator_parser {
  ($($name:ident),+$(,)?) => {
    $(
      paste::paste! {
        /// Parses the punctuator token from the input.
        pub fn [<parse_ $name:snake>]<'inp, S, Ctx, Lang>(
          input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
        ) -> Result<$name, SyntacticTokenErrors<S>>
        where
          S: Clone,
          SyntacticToken<S>: FromLogos<'inp>,
          SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
          Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
          Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
          Lang: ?Sized,
        {
          let Spanned { span, data: token } = next_token(input)?;
          match token {
            SyntacticToken::$name => Ok($name::new(span)),
            tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::$name, span).into()),
          }
        }
      }
    )*
  };
}

punctuator_parser! {
  At,
  Asterisk,
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
