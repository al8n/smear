use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned, utils::cmp::Equivalent,
};

use crate::{
  graphql::Expectation,
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
  value::NullValue,
};

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// Parses a GraphQL null value from the lossless input.
pub fn parse_null_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<NullValue<S>, LosslessTokenErrors<S>>
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
    LosslessToken::Identifier(name) => match () {
      () if "null".equivalent(&name) => Ok(NullValue::new(span, name)),
      _ => Err(LosslessTokenError::invalid_null_value(name, span).into()),
    },
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::NullValue, span).into()),
  }
}
