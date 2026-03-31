use smear_lexer::tokit::{Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned};

use super::super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

pub use crate::value::{BlockStringValue, InlineStringValue, StringValue};

/// Parses a string value (inline or block) from the input.
pub fn parse_string_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<StringValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>:
    Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::LitInlineStr(raw) => Ok(StringValue::new(span, raw.into())),
    SyntacticToken::LitBlockStr(raw) => Ok(StringValue::new(span, raw.into())),
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::StringValue, span).into()),
  }
}

/// Parses an inline string value from the input.
pub fn parse_inline_string_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<InlineStringValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>:
    Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::LitInlineStr(raw) => Ok(InlineStringValue::new(span, raw)),
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::InlineString, span).into()),
  }
}

/// Parses a block string value from the input.
pub fn parse_block_string_value<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<BlockStringValue<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>:
    Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::LitBlockStr(raw) => Ok(BlockStringValue::new(span, raw)),
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::BlockString, span).into()),
  }
}
