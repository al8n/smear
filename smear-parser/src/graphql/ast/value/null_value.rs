use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned, utils::cmp::Equivalent,
};

use super::super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

pub use crate::value::NullValue;

/// Parses a null value from the input.
pub fn parse_null_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<NullValue<S>, SyntacticTokenErrors<S>>
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
      () if "null".equivalent(&name) => Ok(NullValue::new(span, name)),
      _ => Err(SyntacticTokenError::invalid_null_value(name, span).into()),
    },
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::NullValue, span).into()),
  }
}
