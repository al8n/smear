use crate::{
  graphqlx::error::{BinaryError, HexError, OctalError},
  handlers::{self, is_ignored_byte},
  hints::{BinaryHint, ExponentHint, FloatHint, HexExponentHint, HexFloatHint, HexHint, OctalHint},
};
use logosky::{
  Source,
  logos::{Lexer, Logos},
  utils::{Lexeme, UnexpectedEnd, tracker::Tracker},
};

use super::error;

type LexerError<Extras> = error::LexerError<u8, Extras>;
type LexerErrors<Extras> = error::LexerErrors<u8, Extras>;
type LexerErrorData<Extras> = error::LexerErrorData<u8, Extras>;

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<u8, Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  match lexer.slice().as_ref().iter().next() {
    Some(&ch) => LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start),
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }
  .into()
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn cst_default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<u8, Extras>
where
  T: Logos<'a, Source = S, Extras = Tracker>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  match lexer.slice().as_ref().iter().next() {
    Some(&ch) => {
      lexer.extras.increase_token();
      LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start)
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
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_slice = remainder.as_ref();
  let remainder_len = remainder_slice.len();
  let iter = remainder_slice.iter().copied();
  Err(LexerError::float(
    lexer.span().into(),
    super::fractional_error(lexer, remainder_len, iter, |ch| {
      is_ignored_byte(remainder_slice, ch)
    }),
  ))
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_hex_fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_slice = remainder.as_ref();
  let remainder_len = remainder_slice.len();
  let iter = remainder_slice.iter().copied();
  Err(LexerError::hex_float(
    lexer.span().into(),
    super::hex_fractional_error(lexer, remainder_len, iter, |ch| {
      is_ignored_byte(remainder_slice, ch)
    }),
  ))
}

#[inline]
fn exponent_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_slice = remainder.as_ref();
  let remainder_len = remainder_slice.len();
  let iter = remainder_slice.iter().copied();

  let slice = lexer.slice();
  let slice_bytes = slice.as_ref();

  LexerError::float(
    lexer.span().into(),
    handlers::lit_float_suffix_error::<_, super::GraphQLxNumber, _, _, _, _>(
      "float",
      lexer,
      remainder_len,
      iter,
      |b| is_ignored_byte(remainder_slice, b),
      || match slice_bytes.iter().last().copied() {
        Some(b'e' | b'E') => FloatHint::Exponent(ExponentHint::SignOrDigit),
        Some(b'+' | b'-') => FloatHint::Exponent(ExponentHint::Digit),
        _ => unreachable!("regex should ensure the last char is 'e', 'E', '+' or '-"),
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
  S::Slice<'a>: AsRef<[u8]>,
{
  Err(exponent_error(lexer))
}

#[inline]
fn hex_exponent_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_slice = remainder.as_ref();
  let remainder_len = remainder_slice.len();
  let iter = remainder_slice.iter().copied();

  let slice = lexer.slice();
  let slice_bytes = slice.as_ref();

  LexerError::hex_float(
    lexer.span().into(),
    handlers::lit_float_suffix_error::<_, super::GraphQLxHexExponent, _, _, _, _>(
      "hex float",
      lexer,
      remainder_len,
      iter,
      |b| is_ignored_byte(remainder_slice, b),
      || match slice_bytes.last().copied() {
        Some(b'p' | b'P') => HexFloatHint::Exponent(HexExponentHint::SignOrDigit),
        Some(b'+' | b'-' | b'_') => HexFloatHint::Exponent(HexExponentHint::Digit),
        _ => unreachable!("regex should ensure the last char is 'p', 'P', '+', '-' or '_'"),
      },
    ),
  )
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_hex_exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
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
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_float_missing_integer_part_error_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
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
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_hex_float_missing_integer_part_error_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
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
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_hex_float_missing_exponent_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
  )
}

pub(crate) fn handle_decimal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<u8>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
  E: Into<LexerErrorData<Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

pub(crate) fn handle_valid_binary_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<u8>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
  E: Into<LexerErrorData<Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_binary_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_invalid_binary_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let mut errs = error::LexerErrors::default();
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    errs.push(error::LexerError::binary(
      lexer.span().into(),
      BinaryError::UnexpectedEnd(UnexpectedEnd::with_name("binary".into(), BinaryHint::Digit)),
    ));
    return Err(errs);
  }

  match handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

pub(crate) fn handle_valid_octal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<u8>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
  E: Into<LexerErrorData<Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_octal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_invalid_octal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let mut errs = error::LexerErrors::default();
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    errs.push(error::LexerError::octal(
      lexer.span().into(),
      OctalError::UnexpectedEnd(UnexpectedEnd::with_name("octal".into(), OctalHint::Digit)),
    ));
    return Err(errs);
  }

  match handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

pub(crate) fn handle_valid_hex_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<u8>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
  E: Into<LexerErrorData<Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_hex_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_invalid_hex_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let mut errs = error::LexerErrors::default();
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    errs.push(error::LexerError::hex(
      lexer.span().into(),
      HexError::UnexpectedEnd(UnexpectedEnd::with_name("hex".into(), HexHint::Digit)),
    ));
    return Err(errs);
  }

  match handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}
