use super::error::{self, FloatError};
use logosky::{
  Source,
  logos::{Lexer, Logos},
  utils::{Lexeme, PositionedChar, Span, tracker::Tracker},
};

use crate::{
  handlers::{self, is_ignored_char},
  hints::{ExponentHint, FloatHint},
};

type LexerError<Extras> = error::LexerError<char, Extras>;
type LexerErrors<Extras> = error::LexerErrors<char, Extras>;
type LexerErrorData<Extras> = error::LexerErrorData<char, Extras>;

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<char, Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  match lexer.slice().as_ref().chars().next() {
    Some(ch) => LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start),
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }
  .into()
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn cst_default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<char, Extras>
where
  T: Logos<'a, Source = S, Extras = Tracker>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  match lexer.slice().as_ref().chars().next() {
    Some(ch) => {
      lexer.extras.increase_token();
      LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start)
    }
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }
  .into()
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn unexpected_plus_token<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, error::LexerError<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span = lexer.span();
  let pos = span.start;
  Err(error::LexerError::unexpected_char(span.into(), '+', pos))
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn unexpected_minus_token<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, error::LexerError<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span = lexer.span();
  let pos = span.start;
  Err(error::LexerError::unexpected_char(span.into(), '-', pos))
}

#[inline]
pub(crate) fn leading_zero_error<'a, S, E, T, Extras>(
  lexer: &mut Lexer<'a, T>,
  leading_zeros: impl FnOnce(Lexeme<char>) -> E,
) -> LexerError<Extras>
where
  E: Into<LexerErrorData<Extras>>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  T: Logos<'a, Source = S>,
{
  let slice = lexer.slice();
  let mut zeros = 0;

  let mut chars = slice.as_ref().chars();

  let zero_start_at = match chars.next() {
    Some('-') => lexer.span().start + 1,
    Some('0') => {
      zeros += 1;
      lexer.span().start
    }
    Some(_) | None => unreachable!("regex should ensure the first char is '-' or '0'"),
  };

  for ch in chars {
    if ch == '0' {
      zeros += 1;
    } else {
      break;
    }
  }

  let l = if zeros == 1 {
    let pc = PositionedChar::with_position('0', zero_start_at);
    Lexeme::Char(pc)
  } else {
    Lexeme::Span(Span::from(zero_start_at..(zero_start_at + zeros)))
  };

  LexerError::new(lexer.span(), leading_zeros(l).into())
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_leading_zero_and_number_suffix_error<'a, S, T, LE, SE, Extras>(
  lexer: &mut Lexer<'a, T>,
  leading_zeros: impl FnOnce(Lexeme<char>) -> LE,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> SE,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  LE: Into<LexerErrorData<Extras>>,
  SE: Into<LexerErrorData<Extras>>,
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let err = leading_zero_error(lexer, leading_zeros);
  let mut errs = LexerErrors::default();
  errs.push(err);
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  match super::handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  ) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(LexerError::const_new(lexer.span().into(), e.into()));
      Err(errs)
    }
  }
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_float_missing_integer_part_error_then_check_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_float_missing_integer_part_error_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
  )
}

#[inline]
pub(crate) fn fractional_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();
  LexerError::float(
    lexer.span().into(),
    super::fractional_error(lexer, remainder_len, iter, is_ignored_char),
  )
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  Err(fractional_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_leading_zeros_and_fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let err = leading_zero_error(lexer, FloatError::LeadingZeros);
  let mut errs = LexerErrors::with_capacity(2);
  errs.push(err);
  errs.push(fractional_error(lexer));
  Err(errs)
}

#[inline]
pub(crate) fn exponent_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();

  let slice = lexer.slice();
  let slice_str = slice.as_ref();

  LexerError::float(
    lexer.span().into(),
    handlers::lit_float_suffix_error::<_, super::GraphQLNumber, _, _, _, _>(
      "float",
      lexer,
      remainder_len,
      iter,
      is_ignored_char,
      || match slice_str.chars().last() {
        Some('e' | 'E') => FloatHint::Exponent(ExponentHint::SignOrDigit),
        Some('+' | '-') => FloatHint::Exponent(ExponentHint::Digit),
        _ => unreachable!("regex should ensure the last char is 'e', 'E', '+' or '-'"),
      },
    ),
  )
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  Err(exponent_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_leading_zeros_and_exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let err = leading_zero_error(lexer, FloatError::LeadingZeros);
  let mut errs = LexerErrors::with_capacity(2);
  errs.push(err);
  errs.push(exponent_error(lexer));
  Err(errs)
}

pub(crate) fn handle_decimal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}
