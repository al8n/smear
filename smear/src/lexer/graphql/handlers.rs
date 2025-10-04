use logosky::{
  Logos, Source,
  logos::Lexer,
  utils::{
    Lexeme, UnexpectedEnd, UnexpectedLexeme,
    recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
  },
};

use crate::{
  hints::FloatHint,
  lexer::handlers::{self, ValidateNumberChar, handle_number_suffix},
};

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
pub(super) fn handle_float_missing_integer_part_error_then_check_suffix<'a, Char, S, T, Extras>(
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

  match handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder,
    error::FloatError::UnexpectedSuffix,
  ) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

#[inline]
fn fractional_error<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  is_ignored_char: impl FnOnce(&Char) -> bool,
) -> E
where
  Char: Copy + ValidateNumberChar<GraphQLNumber>,
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  E: From<UnexpectedEnd<FloatHint>> + From<UnexpectedLexeme<Char, FloatHint>>,
{
  handlers::fractional_error(lexer, remainder_len, remainder, is_ignored_char, || {
    FloatHint::Fractional
  })
}


#[inline]
fn handle_decimal_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + ValidateNumberChar<GraphQLNumber>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_number_suffix(lexer, remainder_len, remainder, unexpected_suffix)
}

pub(super) struct GraphQLNumber;

impl ValidateNumberChar<GraphQLNumber> for char {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, 'a'..='z' | 'A'..='Z' | '_' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.')
  }
}

impl ValidateNumberChar<GraphQLNumber> for u8 {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')
  }
}
