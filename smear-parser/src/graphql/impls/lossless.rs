use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span, lexer::FromLogos, span::Spanned,
};

use super::super::{
  Expectation,
  error::{Error, Errors},
};
use crate::lexer::graphql::lossless::{LosslessLexer, LosslessToken, LosslessTokenKind};

pub use ast::*;

mod ast;

/// The error type used for the lossless parser implementation.
pub type LosslessTokenError<S> = Error<S, LosslessToken<S>, char, Expectation>;
/// The errors type used for the lossless parser implementation.
pub type LosslessTokenErrors<S> = Errors<S, LosslessToken<S>, char, Expectation>;

/// Helper to consume a token from an InputRef and produce a span.
///
/// Returns `Ok(Spanned { span, data: token })` on success, or an appropriate error.
#[inline]
fn next_token<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
) -> Result<Spanned<LosslessToken<S>>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  let cursor = input.cursor().clone();
  match input.next()? {
    Some(spanned) => Ok(spanned),
    None => {
      let span = input.span_since(&cursor);
      Err(LosslessTokenError::unexpected_end_of_input(span).into())
    }
  }
}
