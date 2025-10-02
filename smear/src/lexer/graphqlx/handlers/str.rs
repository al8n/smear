use super::error::{self, FloatError};
use logosky::{
  Source,
  logos::{Lexer, Logos},
  utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme},
};

use crate::hints::{ExponentHint, FloatHint};

type LexerError<Extras> = error::LexerError<char, Extras>;
type LexerErrors<Extras> = error::LexerErrors<char, Extras>;
type LexerErrorData<Extras> = error::LexerErrorData<char, Extras>;

#[inline(always)]
pub(in crate::lexer) fn default_error<'a, S, T, Extras>(
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

#[allow(clippy::result_large_err)]
#[inline]
pub(in crate::lexer) fn handle_float_missing_integer_part_error_and_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let mut errs = LexerErrors::default();
  errs.push(LexerError::new(
    lexer.span(),
    LexerErrorData::Float(FloatError::MissingIntegerPart),
  ));

  match handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e);
      Err(errs)
    }
  }
}

#[inline]
pub(in crate::lexer) fn fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> LexerError<Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let mut iter = remainder.as_ref().chars();

  let err = match iter.next() {
    None | Some(' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',') => {
      UnexpectedEnd::with_name("float".into(), FloatHint::Fractional).into()
    }
    Some(ch @ ('a'..='z' | 'A'..='Z' | '_' | '.')) => {
      // The first char is already consumed.
      let mut curr = 1;
      let span = lexer.span();

      for ch in iter {
        if matches!(ch, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.') {
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

        return LexerError::float(
          lexer.span().into(),
          UnexpectedLexeme::new(l, FloatHint::Fractional).into(),
        );
      }

      // we reached the end of remainder
      let len = remainder.as_ref().len();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);
      let l = if len == 1 {
        let pc = PositionedChar::with_position(ch, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(Span::from(span.end..(span.end + len)))
      };

      UnexpectedLexeme::new(l, FloatHint::Fractional).into()
    }
    Some(ch) => {
      let span = lexer.span();
      lexer.bump(ch.len_utf8());

      let l = Lexeme::Char(PositionedChar::with_position(ch, span.end));
      UnexpectedLexeme::new(l, FloatHint::Fractional).into()
    }
  };

  LexerError::float(lexer.span().into(), err)
}

#[inline(always)]
pub(in crate::lexer) fn handle_fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  Err(fractional_error(lexer))
}

#[inline]
pub(in crate::lexer) fn exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> LexerError<Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let mut iter = remainder.as_ref().chars();
  let slice = lexer.slice();

  let hint = || match slice.as_ref().chars().last() {
    Some('e' | 'E') => FloatHint::Exponent(ExponentHint::SignOrDigit),
    Some('+' | '-') => FloatHint::Exponent(ExponentHint::Digit),
    _ => unreachable!("regex should ensure the last char is 'e', 'E', '+' or '-"),
  };

  let err = match iter.next() {
    None | Some(' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',') => {
      UnexpectedEnd::with_name("float".into(), hint()).into()
    }
    Some(ch @ ('a'..='z' | 'A'..='Z' | '_' | '.')) => {
      // The first char is already consumed.
      let mut curr = 1;
      let span = lexer.span();

      for ch in iter {
        if matches!(ch, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.') {
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
      let len = remainder.as_ref().len();
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
      lexer.bump(ch.len_utf8());

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
  S::Slice<'a>: AsRef<str>,
{
  Err(exponent_error(lexer))
}

#[inline]
pub(in crate::lexer) fn handle_decimal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<LexerErrorData<Extras>>,
  T: Logos<'a, Source = S>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let mut iter = remainder_str.chars();

  let mut curr = 0;

  match iter.next() {
    // we have a following character after the float literal, need to report the error
    Some(item @ ('a'..='z' | 'A'..='Z' | '.')) => {
      // the first char is already consumed and it cannot be a digit,
      curr += 1;

      let span = lexer.span();
      // try to consume the longest invalid sequence,
      // the first char is already consumed and it cannot be a digit,
      // but the following chars can be digits as well
      for ch in iter {
        if matches!(ch, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.') {
          curr += 1;
          continue;
        }

        // bump the lexer to the end of the invalid sequence
        lexer.bump(curr);

        let l = if curr == 1 {
          // only one invalid char
          let pc = PositionedChar::with_position(item, span.end);
          Lexeme::Char(pc)
        } else {
          Lexeme::Span(Span::from(span.end..(span.end + curr)))
        };
        return Err(LexerError::new(lexer.span(), unexpected_suffix(l).into()));
      }

      // we reached the end of remainder
      let len = remainder_str.len();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);

      let l = if len == 1 {
        let pc = PositionedChar::with_position(item, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(Span::from(span.end..(span.end + len)))
      };

      // return the range of the invalid sequence
      Err(LexerError::new(lexer.span(), unexpected_suffix(l).into()))
    }
    // For other characters, just return the float literal
    Some(_) | None => Ok(lexer.slice()),
  }
}
