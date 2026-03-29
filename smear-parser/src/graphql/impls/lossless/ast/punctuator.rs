use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span,
  span::Spanned,
};
use smear_lexer::punctuator::*;

use crate::lexer::graphql::lossless::{LosslessLexer, LosslessToken};
use crate::graphql::Expectation;

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

macro_rules! punctuator_parser {
  ($($name:ident),+$(,)?) => {
    $(
      paste::paste! {
        /// Parses the punctuator token from the lossless input.
        pub fn [<parse_ $name:snake>]<'inp, S, Ctx, Lang>(
          input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
        ) -> Result<$name, LosslessTokenErrors<S>>
        where
          S: Clone,
          LosslessToken<S>: FromLogos<'inp>,
          LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
          Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
          Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
          Lang: ?Sized,
        {
          let Spanned { span, data: token } = next_token(input)?;
          match token {
            LosslessToken::$name => Ok($name::new(span)),
            tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::$name, span).into()),
          }
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
