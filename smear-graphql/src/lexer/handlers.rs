use logosky::{
  Logos,
  logos::Lexer,
  utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
};

use crate::error;

pub(super) mod slice;
pub(super) mod str;

#[inline(always)]
pub(super) fn unterminated_spread_operator<'a, C, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, Extras>>
where
  T: Logos<'a>,
{
  Err(error::LexerError::new(
    lexer.span(),
    error::LexerErrorData::UnterminatedSpreadOperator,
  ))
}

#[inline(always)]
pub(super) fn increase_recursion_depth<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, RecursionLimitExceeded>>
where
  T: Logos<'a, Extras = RecursionLimiter>,
{
  lexer.extras.increase();

  lexer
    .extras
    .check()
    .map_err(|e| error::LexerError::new(lexer.span(), error::LexerErrorData::State(e)))
}

#[inline(always)]
pub(super) fn decrease_recursion_depth<'a, T>(lexer: &mut Lexer<'a, T>)
where
  T: Logos<'a, Extras = RecursionLimiter>,
{
  lexer.extras.decrease();
}
