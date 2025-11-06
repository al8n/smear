use super::error;
use logosky::{
  Source,
  error::UnexpectedEnd,
  logos::{Lexer, Logos},
  utils::{Lexeme, Span, tracker::Limiter},
};

use crate::{
  graphqlx::{
    GraphQLx,
    error::{BinaryError, DecimalError, FloatError, HexError, HexFloatError, OctalError},
  },
  handlers::{self, is_ignored_char},
  hints::{BinaryHint, ExponentHint, FloatHint, HexExponentHint, HexFloatHint, HexHint, OctalHint},
};

type LexerError<Extras> = error::LexerError<char, Extras>;
type LexerErrors<Extras> = error::LexerErrors<char, Extras>;

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<char, Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  crate::handlers::str::default_error::<S, T, GraphQLx, LexerError<Extras>>(lexer).into()
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn cst_default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<char, Extras>
where
  T: Logos<'a, Source = S, Extras = Limiter>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  match lexer.slice().as_ref().chars().next() {
    Some(ch) => {
      lexer.extras.increase_token();
      LexerError::unknown_char(lexer.span().start, ch)
    }
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }
  .into()
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
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();
  Err(LexerError::float(super::fractional_error(
    lexer,
    remainder_len,
    iter,
    is_ignored_char,
  )))
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_hex_fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();
  Err(LexerError::hex_float(super::hex_fractional_error(
    lexer,
    remainder_len,
    iter,
    is_ignored_char,
  )))
}

#[inline]
fn exponent_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
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
  LexerError::float(handlers::lit_float_suffix_error::<
    _,
    super::GraphQLxNumber,
    _,
    _,
    _,
    _,
  >(
    "float",
    lexer,
    remainder_len,
    iter,
    is_ignored_char,
    || match slice_str.chars().last() {
      Some('e' | 'E') => FloatHint::Exponent(ExponentHint::SignOrDigit),
      Some('+' | '-') => FloatHint::Exponent(ExponentHint::Digit),
      _ => unreachable!("regex should ensure the last char is 'e', 'E', '+' or '-"),
    },
  ))
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

#[inline]
fn hex_exponent_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
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

  LexerError::hex_float(handlers::lit_float_suffix_error::<
    _,
    super::GraphQLxHexExponent,
    _,
    _,
    _,
    _,
  >(
    "hex float",
    lexer,
    remainder_len,
    iter,
    is_ignored_char,
    || match slice_str.chars().last() {
      Some('p' | 'P') => HexFloatHint::Exponent(HexExponentHint::SignOrDigit),
      Some('+' | '-' | '_') => HexFloatHint::Exponent(HexExponentHint::Digit),
      _ => unreachable!("regex should ensure the last char is 'p', 'P', '+', '-' or '_'"),
    },
  ))
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_hex_exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  Err(hex_exponent_error(lexer))
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

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_hex_float_missing_integer_part_error_then_check_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_hex_float_missing_integer_part_error_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
  )
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_hex_float_missing_exponent_then_check_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_hex_float_missing_exponent_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
  )
}

pub(crate) fn handle_decimal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<LexerError<Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(Into::into)
}

pub(crate) fn handle_int_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span: Span = lexer.span().into();
  handle_decimal_suffix(lexer, |err| DecimalError::unexpected_suffix(span, err))
}

pub(crate) fn handle_float_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span: Span = lexer.span().into();
  handle_decimal_suffix(lexer, |err| FloatError::unexpected_suffix(span, err))
}

pub(crate) fn handle_valid_binary_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerError<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_binary_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(Into::into)
}

#[inline]
pub(crate) fn handle_invalid_binary_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let mut errs = LexerErrors::new();
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    let span = lexer.span().into();
    errs.push(error::LexerError::binary(BinaryError::UnexpectedEnd(
      UnexpectedEnd::with_name(span, "binary".into(), BinaryHint::Digit),
    )));
    return Err(errs);
  }

  match handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e);
      Err(errs)
    }
  }
}

pub(crate) fn handle_valid_octal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerError<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_octal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(Into::into)
}

#[inline]
pub(crate) fn handle_invalid_octal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let mut errs = LexerErrors::new();
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    let span = lexer.span().into();
    errs.push(error::LexerError::octal(OctalError::UnexpectedEnd(
      UnexpectedEnd::with_name(span, "octal".into(), OctalHint::Digit),
    )));
    return Err(errs);
  }

  match handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e);
      Err(errs)
    }
  }
}

pub(crate) fn handle_valid_hex_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span: Span = lexer.span().into();
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_hex_suffix(lexer, remainder_len, remainder.as_ref().chars(), |err| {
    HexFloatError::unexpected_suffix(span, err)
  })
  .map_err(Into::into)
}

#[inline]
pub(crate) fn handle_invalid_hex_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let mut errs = LexerErrors::new();
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    let span = lexer.span().into();
    errs.push(error::LexerError::hex(HexError::UnexpectedEnd(
      UnexpectedEnd::with_name(span, "hex".into(), HexHint::Digit),
    )));
    return Err(errs);
  }

  match handle_valid_hex_suffix(lexer) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e);
      Err(errs)
    }
  }
}
