use logosky::{
  Logos, Source, logos::Lexer, utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter}
};

use crate::lexer::{handlers::{GraphQLNumber, ValidateNumberChar, handle_graphql_decimal_suffix}};

use super::error;

pub(super) mod slice;
pub(super) mod str;

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

#[allow(clippy::result_large_err)]
#[inline]
pub(super) fn handle_float_missing_integer_part_error_and_suffix<'a, Char, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
) -> Result<S::Slice<'a>, error::LexerErrors<Char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  Char: Copy + ValidateNumberChar<GraphQLNumber>,
{
  let mut errs = error::LexerErrors::default();
  errs.push(error::LexerError::new(
    lexer.span(),
    error::LexerErrorData::Float(error::FloatError::MissingIntegerPart),
  ));

  match handle_graphql_decimal_suffix(lexer, remainder_len, remainder, error::FloatError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

