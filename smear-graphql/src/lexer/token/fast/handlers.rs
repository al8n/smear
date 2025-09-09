use logos::Lexer;
use logosky::utils::{PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme};

use super::{Lexeme, Error, Errors, ExponentHint, FloatError, FloatHint, ErrorData, Token};

#[inline(always)]
pub(super) fn increase_recursion_depth<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<(), Error> {
  lexer.extras.increase();

  lexer
    .extras
    .check()
    .map_err(|e| Error::new(lexer.span(), e.into()))
}

#[inline(always)]
pub(super) fn decrease_recursion_depth<'a>(lexer: &mut Lexer<'a, Token<'a>>) {
  lexer.extras.decrease();
}

#[inline(always)]
pub(super) fn unterminated_spread_operator<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<(), Error> {
  Err(Error::new(
    lexer.span(),
    ErrorData::UnterminatedSpreadOperator,
  ))
}

#[inline]
pub(super) fn leading_zero_error<'a, E>(
  lexer: &mut Lexer<'a, Token<'a>>,
  leading_zeros: impl FnOnce(Lexeme<char>) -> E,
) -> Error
where
  E: Into<ErrorData>,
{
  let slice = lexer.slice();
  let mut zeros = 0;

  let mut chars = slice.chars();

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

  Error::new(lexer.span(), leading_zeros(l).into())
}

#[allow(clippy::result_large_err)]
#[inline(always)]
pub(super) fn handle_leading_zero_and_number_suffix_error<'a, LE, SE>(
  lexer: &mut Lexer<'a, Token<'a>>,
  leading_zeros: impl FnOnce(Lexeme<char>) -> LE,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> SE,
) -> Result<&'a str, Errors>
where
  LE: Into<ErrorData>,
  SE: Into<ErrorData>,
{
  let err = leading_zero_error(lexer, leading_zeros);
  let mut errs = Errors::default();
  errs.push(err);
  match handle_number_suffix(lexer, unexpected_suffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e);
      Err(errs)
    }
  }
}

#[allow(clippy::result_large_err)]
#[inline]
pub(super) fn handle_float_missing_integer_part_error_and_suffix<'a>(
  lexer: &mut Lexer<'a, Token<'a>>,
) -> Result<&'a str, Errors> {
  let mut errs = Errors::default();
  errs.push(Error::new(
    lexer.span(),
    ErrorData::Float(FloatError::MissingIntegerPart),
  ));

  match handle_number_suffix(lexer, FloatError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e);
      Err(errs)
    }
  }
}

#[inline]
pub(super) fn fractional_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Error {
  let remainder = lexer.remainder();
  let mut iter = remainder.chars();

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

        return Error::float(
          lexer.span().into(),
          UnexpectedLexeme::new(l, FloatHint::Fractional).into(),
        );
      }

      // we reached the end of remainder
      let len = remainder.len();
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

  Error::float(lexer.span().into(), err)
}

#[inline(always)]
pub(super) fn handle_fractional_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  Err(fractional_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(super) fn handle_leading_zeros_and_fractional_error<'a>(
  lexer: &mut Lexer<'a, Token<'a>>,
) -> Result<&'a str, Errors> {
  let err = leading_zero_error(lexer, FloatError::LeadingZeros);
  let mut errs = Errors::with_capacity(2);
  errs.push(err);
  errs.push(fractional_error(lexer));
  Err(errs)
}

#[inline]
pub(super) fn exponent_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Error {
  let remainder = lexer.remainder();
  let mut iter = remainder.chars();
  let slice = lexer.slice();

  let hint = || match slice.chars().last() {
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

        return Error::float(lexer.span().into(), UnexpectedLexeme::new(l, hint()).into());
      }

      // we reached the end of remainder
      let len = remainder.len();
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

  Error::float(lexer.span().into(), err)
}

#[inline(always)]
pub(super) fn handle_exponent_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  Err(exponent_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(super) fn handle_leading_zeros_and_exponent_error<'a>(
  lexer: &mut Lexer<'a, Token<'a>>,
) -> Result<&'a str, Errors> {
  let err = leading_zero_error(lexer, FloatError::LeadingZeros);
  let mut errs = Errors::with_capacity(2);
  errs.push(err);
  errs.push(exponent_error(lexer));
  Err(errs)
}

#[inline]
pub(super) fn handle_number_suffix<'a, E>(
  lexer: &mut Lexer<'a, Token<'a>>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<&'a str, Error>
where
  E: Into<ErrorData>,
{
  let remainder = lexer.remainder();
  let mut iter = remainder.chars();

  let mut curr = 0;

  match iter.next() {
    // we have a following character after the float literal, need to report the error
    Some(item @ ('a'..='z' | 'A'..='Z' | '_' | '.')) => {
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
        return Err(Error::new(lexer.span(), unexpected_suffix(l).into()));
      }

      // we reached the end of remainder
      let len = remainder.len();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);

      let l = if len == 1 {
        let pc = PositionedChar::with_position(item, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(Span::from(span.end..(span.end + len)))
      };

      // return the range of the invalid sequence
      Err(Error::new(lexer.span(), unexpected_suffix(l).into()))
    }
    // For other characters, just return the float literal
    Some(_) | None => Ok(lexer.slice()),
  }
}
