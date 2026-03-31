use smear_lexer::tokit::{
  Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned, utils::cmp::Equivalent,
};

use crate::{
  graphql::Expectation,
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
  value::EnumValue,
};

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// Parses a GraphQL enum value from the lossless input.
pub fn parse_enum_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<EnumValue<S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>:
    Lexer<'inp, Token = LosslessToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    LosslessToken::Identifier(name) => match () {
      () if "true".equivalent(&name) || "false".equivalent(&name) || "null".equivalent(&name) => {
        Err(LosslessTokenError::invalid_enum_value(name, span).into())
      }
      _ => Ok(EnumValue::new(span, name)),
    },
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::EnumValue, span).into()),
  }
}
