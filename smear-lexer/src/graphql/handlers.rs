use logosky::{
  Logos, Source,
  logos::Lexer,
  utils::{
    Lexeme, UnexpectedEnd, UnexpectedLexeme,
    recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
    tracker::{LimitExceeded, Tracker},
  },
};

use crate::{
  handlers::{self, ValidateNumberChar, handle_number_suffix},
  hints::FloatHint,
};

use super::error;

pub(super) mod slice;
pub(super) mod str;

#[inline(always)]
pub(super) fn increase_recursion_depth_and_token<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = Tracker>,
{
  handlers::increase_recursion_depth_and_token(lexer)
}

#[inline(always)]
pub(super) fn tt_hook_and_then<'a, C, T, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> Result<O, error::LexerError<C, LimitExceeded>>,
) -> Result<O, error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = Tracker>,
{
  handlers::tt_hook_and_then(lexer, f)
}

#[allow(clippy::result_large_err)]
#[inline(always)]
pub(super) fn tt_hook_and_then_into_errors<'a, C, T, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> Result<O, error::LexerErrors<C, LimitExceeded>>,
) -> Result<O, error::LexerErrors<C, LimitExceeded>>
where
  T: Logos<'a, Extras = Tracker>,
{
  handlers::tt_hook_and_then_into_errors(lexer, f)
}

#[inline(always)]
pub(super) fn tt_hook_map<'a, C, T, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> O,
) -> Result<O, error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = Tracker>,
{
  handlers::tt_hook_map(lexer, f)
}

#[inline(always)]
pub(super) fn tt_hook<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = Tracker>,
{
  handlers::tt_hook(lexer)
}

#[inline(always)]
pub(super) fn increase_recursion_depth<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, RecursionLimitExceeded>>
where
  T: Logos<'a, Extras = RecursionLimiter>,
{
  handlers::increase_recursion_depth(lexer)
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
  handlers::lit_float_suffix_error(
    "float",
    lexer,
    remainder_len,
    remainder,
    is_ignored_char,
    || FloatHint::Fractional,
  )
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
