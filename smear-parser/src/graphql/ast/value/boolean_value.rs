use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext,
  span::Spanned,
  utils::cmp::Equivalent,
};

use super::super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

pub use crate::value::BooleanValue;

/// Parses a boolean value from the input.
pub fn parse_boolean_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<BooleanValue, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::Identifier(ident) => match () {
      () if "true".equivalent(&ident) => Ok(BooleanValue::new(span, true)),
      () if "false".equivalent(&ident) => Ok(BooleanValue::new(span, false)),
      _ => Err(SyntacticTokenError::invalid_boolean_value(ident, span).into()),
    },
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::BooleanValue, span).into()),
  }
}
