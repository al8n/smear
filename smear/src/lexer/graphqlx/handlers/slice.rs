use crate::{
  hints::{ExponentHint, FloatHint},
  lexer::handlers::is_ignored_byte,
};
use logosky::{
  Source,
  logos::{Lexer, Logos},
  utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme},
};

use super::error;

type LexerError<Extras> = error::LexerError<u8, Extras>;
type LexerErrors<Extras> = error::LexerErrors<u8, Extras>;
type LexerErrorData<Extras> = error::LexerErrorData<u8, Extras>;

#[inline(always)]
pub(in crate::lexer) fn default_error<'a, S, T, Extras>(
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

#[inline(always)]
pub(in crate::lexer) fn handle_fractional_error<'a, S, T, Extras>(
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

#[inline(always)]
pub(in crate::lexer) fn handle_hex_fractional_error<'a, S, T, Extras>(
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
pub(in crate::lexer) fn exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> LexerError<Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_slice = remainder.as_ref();
  let mut iter = remainder_slice.iter().copied();
  let slice = lexer.slice();

  let hint = || match slice.as_ref().iter().last().copied() {
    Some(b'e' | b'E') => FloatHint::Exponent(ExponentHint::SignOrDigit),
    Some(b'+' | b'-') => FloatHint::Exponent(ExponentHint::Digit),
    _ => unreachable!("regex should ensure the last char is 'e', 'E', '+' or '-"),
  };

  let err = match iter.next() {
    Some(0xEF)
      if remainder_slice.len() >= 3 && remainder_slice[1] == 0xBB && remainder_slice[2] == 0xBF =>
    {
      // BOM
      UnexpectedEnd::with_name("float".into(), hint()).into()
    }
    None | Some(b' ' | b'\t' | b'\r' | b'\n' | b',') => {
      UnexpectedEnd::with_name("float".into(), hint()).into()
    }
    Some(ch @ (b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')) => {
      // The first char is already consumed.
      let mut curr = 1;
      let span = lexer.span();

      for ch in iter {
        if matches!(ch, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.') {
          curr += 1;
          continue;
        }

        // bump the lexer to the end of the invalid sequence
        lexer.bump(curr);

        let l = if curr == 1 {
          let pc = PositionedChar::with_position(ch, span.end);
          Lexeme::Char(pc)
        } else {
          Lexeme::Span(Span::from(span.end..(span.end + curr)))
        };

        return LexerError::float(lexer.span().into(), UnexpectedLexeme::new(l, hint()).into());
      }

      // we reached the end of remainder
      let len = remainder_slice.len();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);
      let l = if len == 1 {
        let pc = PositionedChar::with_position(ch, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(Span::from(span.end..(span.end + len)))
      };

      UnexpectedLexeme::new(l, hint()).into()
    }
    // For other characters, just yield one
    Some(ch) => {
      let span = lexer.span();
      lexer.bump(1);

      let l = Lexeme::Char(PositionedChar::with_position(ch, span.end));
      UnexpectedLexeme::new(l, hint()).into()
    }
  };

  LexerError::float(lexer.span().into(), err)
}

#[inline(always)]
pub(in crate::lexer) fn handle_exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  Err(exponent_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(in crate::lexer) fn handle_float_missing_integer_part_error_then_check_suffix<
  'a,
  S,
  T,
  Extras,
>(
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
pub(in crate::lexer) fn handle_hex_float_missing_integer_part_error_then_check_suffix<
  'a,
  S,
  T,
  Extras,
>(
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
pub(in crate::lexer) fn handle_hex_float_missing_exponent_then_check_suffix<
  'a,
  S,
  T,
  Extras,
>(
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

pub(in crate::lexer) fn handle_decimal_suffix<'a, S, T, E, Extras>(
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

pub(in crate::lexer) fn handle_binary_suffix<'a, S, T, E, Extras>(
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
  super::handle_binary_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

pub(in crate::lexer) fn handle_octal_suffix<'a, S, T, E, Extras>(
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
  super::handle_octal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

pub(in crate::lexer) fn handle_hex_suffix<'a, S, T, E, Extras>(
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
  super::handle_hex_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().iter().copied(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}
