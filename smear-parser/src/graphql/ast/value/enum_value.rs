use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned, utils::cmp::Equivalent,
};

use super::super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

pub use crate::value::EnumValue;

/// Parses an enum value from the input.
pub fn parse_enum_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<EnumValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>:
    Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokora::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::Identifier(name) => match () {
      () if "true".equivalent(&name) || "false".equivalent(&name) || "null".equivalent(&name) => {
        Err(SyntacticTokenError::invalid_enum_value(name, span).into())
      }
      _ => Ok(EnumValue::new(span, name)),
    },
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::EnumValue, span).into()),
  }
}
