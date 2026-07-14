use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned, utils::cmp::Equivalent,
};

use crate::{
  graphql::Expectation,
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
  value::BooleanValue,
};

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// Parses a GraphQL boolean value from the lossless input.
pub fn parse_boolean_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<BooleanValue, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>:
    Lexer<'inp, Token = LosslessToken<S>, Span = smear_lexer::tokora::SimpleSpan>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    LosslessToken::Identifier(ident) => match () {
      () if "true".equivalent(&ident) => Ok(BooleanValue::new(span, true)),
      () if "false".equivalent(&ident) => Ok(BooleanValue::new(span, false)),
      _ => Err(LosslessTokenError::invalid_boolean_value(ident, span).into()),
    },
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::BooleanValue, span).into()),
  }
}
