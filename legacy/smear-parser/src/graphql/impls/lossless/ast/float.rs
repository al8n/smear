use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned,
};

use crate::{
  graphql::Expectation,
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
  value::FloatValue,
};

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// Parses a GraphQL float value from the lossless input.
pub fn parse_float_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<FloatValue<S>, LosslessTokenErrors<S>>
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
    LosslessToken::LitFloat(val) => Ok(FloatValue::new(span, val)),
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::FloatValue, span).into()),
  }
}
