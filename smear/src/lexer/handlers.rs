use logosky::{
  Logos,
  logos::Lexer,
  utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
};

use crate::error::{BadStateError, UnterminatedSpreadOperatorError};

#[inline(always)]
pub(super) fn unterminated_spread_operator_error<'a, T, E>(
  lexer: &mut Lexer<'a, T>,
) -> E
where
  T: Logos<'a>,
  E: UnterminatedSpreadOperatorError,
{
  E::unterminated_spread_operator(lexer.span().into())
}

#[inline(always)]
pub(super) fn decrease_recursion_depth<'a, T>(lexer: &mut Lexer<'a, T>)
where
  T: Logos<'a, Extras = RecursionLimiter>,
{
  lexer.extras.decrease();
}