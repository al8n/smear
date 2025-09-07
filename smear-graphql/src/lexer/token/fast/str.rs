use core::fmt;
use logos::{Lexer, Logos};
use logosky::utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme};

use super::{
  super::error::{self, *},
  TokenOptions,
};

pub use string_token::*;

mod string_token;

pub type Error = error::Error<char>;
pub type ErrorData = error::ErrorData<char>;
pub type Errors = error::Errors<char>;

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(
  extras = TokenOptions,
  skip r"[ \t,\u{FEFF}]+|#([^\n\r]*(\r\n|\r|\n))*",
  error(Errors, |lexer| match lexer.slice().chars().next() {
    Some(ch) => Error::unexpected_char(lexer.span().into(), ch, lexer.span().start),
    None => Error::unexpected_eoi(lexer.span().into()),
  }.into())
)]
pub enum Token<'a> {
  // Valid tokens
  #[token("&")]
  Ampersand,

  #[token("@")]
  At,

  #[token("}")]
  BraceClose,

  #[token("]")]
  BracketClose,

  #[token(")")]
  ParenClose,

  #[token(":")]
  Colon,

  #[token("$")]
  Dollar,

  #[token("=")]
  Equals,

  #[token("!")]
  Bang,

  #[token("{")]
  BraceOpen,

  #[token("[")]
  BracketOpen,

  #[token("(")]
  ParenOpen,

  #[token("|")]
  Pipe,

  #[token("...")]
  #[token("..", unterminated_spread_operator)]
  #[token(".", unterminated_spread_operator)]
  Spread,

  #[regex("-?0[0-9]+(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix))]
  #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handle_number_suffix(lexer, FloatError::UnexpectedSuffix))]
  #[regex(
    "-?\\.[0-9]+([eE][+-]?[0-9]+)?",
    handle_float_missing_integer_part_error_and_suffix
  )]
  #[regex("-?0[0-9]+\\.[0-9]+[eE][+-]?", handle_leading_zeros_and_exponent_error)]
  #[regex("-?(0|[1-9][0-9]*)\\.[0-9]+[eE][+-]?", handle_exponent_error)]
  #[regex("-?0[0-9]+\\.", handle_leading_zeros_and_fractional_error)]
  #[regex("-?(0|[1-9][0-9]*)\\.", handle_fractional_error)]
  #[regex("-?0[0-9]+[eE][+-]?", handle_leading_zeros_and_exponent_error)]
  #[regex("-?(0|[1-9][0-9]*)[eE][+-]?", handle_exponent_error)]
  FloatLiteral(&'a str),

  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
  Identifier(&'a str),

  #[regex("-?(0|[1-9][0-9]*)", |lexer| handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
  #[regex("-?0[0-9]+", |lexer| handle_leading_zero_and_number_suffix_error(lexer, IntError::LeadingZeros, IntError::UnexpectedSuffix))]
  #[token("-", |lexer| Err(Error::unexpected_char(lexer.span().into(), '-', lexer.span().start)))]
  #[token("+", |lexer| Err(Error::unexpected_char(lexer.span().into(), '+', lexer.span().start)))]
  IntegerLiteral(&'a str),
  #[token("\"", lex_string)]
  StringLiteral(&'a str),
  #[token("\"\"\"", lex_block_string)]
  BlockStringLiteral(&'a str),
}

#[inline(always)]
fn unterminated_spread_operator<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<(), Error> {
  Err(Error::new(
    lexer.span(),
    ErrorData::UnterminatedSpreadOperator,
  ))
}

#[inline(always)]
fn leading_zero_error<'a, E>(
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
fn handle_leading_zero_and_number_suffix_error<'a, LE, SE>(
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
fn handle_float_missing_integer_part_error_and_suffix<'a>(
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
fn fractional_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Error {
  let remainder = lexer.remainder();
  let mut iter = remainder.chars();

  let err = match iter.next() {
    None | Some(' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',') => {
      UnexpectedEnd::with_name("float".into(), FloatHint::Fractional).into()
    }
    Some(ch @ ('a'..='z' | 'A'..='Z' | '_' | '.' | '+' | '-')) => {
      // The first char is already consumed.
      let mut curr = 1;
      let span = lexer.span();

      for ch in iter {
        if matches!(ch, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.' | '+' | '-') {
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
fn handle_fractional_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  Err(fractional_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
fn handle_leading_zeros_and_fractional_error<'a>(
  lexer: &mut Lexer<'a, Token<'a>>,
) -> Result<&'a str, Errors> {
  let err = leading_zero_error(lexer, FloatError::LeadingZeros);
  let mut errs = Errors::with_capacity(2);
  errs.push(err);
  errs.push(fractional_error(lexer));
  Err(errs)
}

#[inline]
fn exponent_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Error {
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
    Some(ch @ ('a'..='z' | 'A'..='Z' | '_' | '.' | '+' | '-')) => {
      // The first char is already consumed.
      let mut curr = 1;
      let span = lexer.span();

      for ch in iter {
        if matches!(ch, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.' | '+' | '-') {
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
fn handle_exponent_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  Err(exponent_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
fn handle_leading_zeros_and_exponent_error<'a>(
  lexer: &mut Lexer<'a, Token<'a>>,
) -> Result<&'a str, Errors> {
  let err = leading_zero_error(lexer, FloatError::LeadingZeros);
  let mut errs = Errors::with_capacity(2);
  errs.push(err);
  errs.push(exponent_error(lexer));
  Err(errs)
}

#[inline]
fn handle_number_suffix<'a, E>(
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

impl fmt::Display for Token<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let message = match self {
      Token::Ampersand => "ampersand ('&')",
      Token::At => "at ('@')",
      Token::BraceClose => "closing brace ('}')",
      Token::BracketClose => "closing bracket (']')",
      Token::ParenClose => "closing paren (')')",
      Token::Colon => "colon (':')",
      Token::Dollar => "dollar ('$')",
      Token::Equals => "equals ('=')",
      Token::Bang => "exclamation mark ('!')",
      // Token::FloatLiteral(_) => "floating point value (e.g. '3.14')",
      Token::Identifier(_) => "non-variable identifier (e.g. 'x' or 'Foo')",
      // Token::IntegerLiteral(_) => "integer value (e.g. '0' or '42')",
      Token::BraceOpen => "open brace ('{')",
      Token::BracketOpen => "open bracket ('[')",
      Token::ParenOpen => "open parenthesis ('(')",
      Token::Pipe => "pipe ('|')",
      Token::Spread => "spread ('...')",
      // Token::BlockStringLiteral(_) => "block string (e.g. '\"\"\"hi\"\"\"')",
      // // Token::ErrorFloatLiteralMissingZero => "unsupported number (int or float) literal",
      // // Token::ErrorNumberLiteralLeadingZero => "unsupported number (int or float) literal",
      // // Token::ErrorNumberLiteralTrailingInvalid => "unsupported number (int or float) literal",
      // Token::StringLiteral(_) => "string literal (e.g. '\"...\"')",
      // Token::ErrorUnterminatedString => "unterminated string",
      // Token::ErrorUnsupportedStringCharacter => "unsupported character in string",
      // Token::ErrorUnterminatedBlockString => "unterminated block string",
      _ => todo!(),
    };
    f.write_str(message)
  }
}

#[cfg(test)]
mod tests;
