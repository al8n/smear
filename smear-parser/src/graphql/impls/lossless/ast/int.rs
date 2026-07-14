use smear_lexer::tokora::{Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned};

use crate::{
  graphql::Expectation,
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
  value::IntValue,
};

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// Parses a GraphQL int value from the lossless input.
pub fn parse_int_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<IntValue<S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>:
    Lexer<'inp, Token = LosslessToken<S>, Span = smear_lexer::tokora::SimpleSpan>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    LosslessToken::LitInt(val) => Ok(IntValue::new(span, val)),
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::IntValue, span).into()),
  }
}
