use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext,
  span::Spanned,
  utils::cmp::Equivalent,
};

use crate::lexer::graphql::lossless::{LosslessLexer, LosslessToken};
use crate::graphql::Expectation;
use crate::value::NullValue;

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// Parses a GraphQL null value from the lossless input.
pub fn parse_null_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<NullValue<S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    LosslessToken::Identifier(name) => match () {
      () if "null".equivalent(&name) => Ok(NullValue::new(span, name)),
      _ => Err(LosslessTokenError::invalid_null_value(name, span).into()),
    },
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::NullValue, span).into()),
  }
}
