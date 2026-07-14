use smear_lexer::tokora::{
  Emitter, InputRef, Lexer, ParseContext, SimpleSpan as Span, lexer::FromLogos, span::Spanned,
};

use crate::{
  graphql::Expectation,
  lexer::graphql::lossless::{LosslessLexer, LosslessToken},
};
use smear_scaffold::ast as scaffold;

use super::{
  LosslessTokenError, LosslessTokenErrors, next_token,
  padded::{Padded, parse_padded},
};

/// List value in GraphQL CST (preserves trivia).
pub type List<V, S> = scaffold::List<Padded<V, S>>;

/// Parses a list value from the lossless input.
///
/// Uses the provided `parse_value` function to parse each element,
/// with trivia (whitespace, comments, commas) preserved around each element.
pub fn parse_list<'inp, S, Ctx, Lang, V>(
  input: &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  parse_value: impl Fn(
    &mut InputRef<'inp, '_, LosslessLexer<'inp, S>, Ctx, Lang>,
  ) -> Result<V, LosslessTokenErrors<S>>,
) -> Result<scaffold::List<Padded<V, S>>, LosslessTokenErrors<S>>
where
  S: Clone,
  LosslessToken<S>: FromLogos<'inp>,
  LosslessLexer<'inp, S>: Lexer<'inp, Token = LosslessToken<S>, Span = Span>,
  Ctx: ParseContext<'inp, LosslessLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, LosslessLexer<'inp, S>, Lang, Error = LosslessTokenErrors<S>>,
  Lang: ?Sized,
{
  // Parse opening bracket
  let Spanned {
    span: open_span,
    data: token,
  } = next_token(input)?;
  match token {
    LosslessToken::LBracket => {}
    tok => {
      return Err(
        LosslessTokenError::unexpected_token(tok, Expectation::LBracket, open_span).into(),
      );
    }
  }

  let mut elements = std::vec::Vec::new();

  loop {
    // Check for closing bracket
    let saved = input.save();
    match next_token(input) {
      Ok(Spanned {
        data: LosslessToken::RBracket,
        ..
      }) => {
        let full_span = Span::new(open_span.start(), input.span_since(&saved.cursor()).end());
        return Ok(scaffold::List::new(full_span, elements));
      }
      Ok(_) => {
        input.restore(saved);
        let padded = parse_padded(input, &parse_value)?;
        elements.push(padded);
      }
      Err(_) => {
        return Err(LosslessTokenError::unclosed_list(open_span).into());
      }
    }
  }
}
