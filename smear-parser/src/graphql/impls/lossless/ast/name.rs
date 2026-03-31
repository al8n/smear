use smear_lexer::tokit::{Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned};

use crate::{
  graphql::{Expectation, ast::Name},
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
};

use super::{LosslessTokenError, LosslessTokenErrors, next_token};

/// Parses a GraphQL name from the lossless input.
pub fn parse_name<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<Name<S>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>:
    Lexer<'inp, Token = LosslessToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    LosslessToken::Identifier(name) => Ok(Name::new(span, name)),
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::Name, span).into()),
  }
}
