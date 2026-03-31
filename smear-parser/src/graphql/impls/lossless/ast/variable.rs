use smear_lexer::tokit::{
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span, lexer::FromLogos, span::Spanned,
};

use crate::{
  graphql::{Expectation, ast::Name},
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
  value::VariableValue,
};

use super::{LosslessTokenError, LosslessTokenErrors, name::parse_name, next_token};

/// Parses a GraphQL variable reference from the lossless input.
pub fn parse_variable<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<VariableValue<Name<S>>, LosslessTokenErrors<S>>
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
    LosslessToken::Dollar => {
      let name = parse_name(input)?;
      let full_span = Span::new(span.start(), name.span().end());
      Ok(VariableValue::new(full_span, name))
    }
    tok => Err(LosslessTokenError::unexpected_token(tok, Expectation::Dollar, span).into()),
  }
}
