/*
* The code in this file is taken from
* https://github.com/facebook/relay/blob/main/compiler/crates/graphql-syntax/src/lexer.rs
*
* Licensed under the MIT license:
*
* Copyright (c) Meta Platforms, Inc. and affiliates.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*/

use core::fmt;
use chumsky::extra::Err;
use logos::{Lexer, Logos};
use logosky::utils::{Lexeme, PositionedChar, UnexpectedEnd, UnexpectedLexeme};

use super::{super::error::{self, *}, TokenOptions};

pub type Error = error::Error<char>;

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(
  extras = TokenOptions,
  skip r"[ \t,\u{FEFF}]+|#([^\n\r]*(\r\n|\r|\n))*",
  error(Error, |lexer| match lexer.slice().chars().next() {
    Some(ch) => Error::UnexpectedCharacter(PositionedChar::with_position(ch, lexer.span().start)),
    None => Error::UnexpectedEndOfInput,
  })
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

  #[regex("-?0[0-9]+(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handle_leading_zero_error(lexer, FloatError::LeadingZeros))]
  #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handle_number_suffix(lexer, FloatError::UnexpectedSuffix))]
  #[regex("-?\\.[0-9]+([eE][+-]?[0-9]+)?", float_missing_integer_part_error)]
  #[regex("-?(0|[1-9][0-9]*)\\.[0-9]+[eE][+-]?", handle_exponent_error)]
  #[regex("-?(0|[1-9][0-9]*)\\.", handle_fractional_error)]
  #[regex("-?(0|[1-9][0-9]*)[eE][+-]?", handle_exponent_error)]
  FloatLiteral(&'a str),

  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
  Identifier(&'a str),

  #[regex("-?(0|[1-9][0-9]*)", |lexer| handle_number_suffix(lexer, IntError::UnexpectedSuffix))] 
  #[regex("-?0[0-9]+", |lexer| handle_leading_zero_error(lexer, IntError::LeadingZeros))]
  #[token("-", |lexer| Err(Error::UnexpectedCharacter(PositionedChar::with_position('-', lexer.span().start))))]
  #[token("+", |lexer| Err(Error::UnexpectedCharacter(PositionedChar::with_position('+', lexer.span().start))))]
  IntegerLiteral(&'a str),

  // #[token("\"", lex_string)]
  // StringLiteral(&'a str),

  // #[token("\"\"\"", lex_block_string)]
  // BlockStringLiteral(&'a str),
}

#[inline(always)]
fn unterminated_spread_operator<'a>(_: &mut Lexer<'a, Token<'a>>) -> Result<(), Error> {
  Err(Error::UnterminatedSpreadOperator)
}

#[inline(always)]
fn handle_leading_zero_error<'a, E>(lexer: &mut Lexer<'a, Token<'a>>, leading_zeros: impl FnOnce(Lexeme<char>) -> E) -> Result<&'a str, Error>
where
  E: Into<Error>,
{
  let slice = lexer.slice();
  let mut zeros = 0;
  
  for ch in slice.chars() {
    if ch == '0' {
      zeros += 1;
    } else {
      break;
    }
  }

  let l = if zeros == 1 {
    let pc = PositionedChar::with_position('0', lexer.span().start);
    Lexeme::Char(pc)
  } else {
    Lexeme::Span(lexer.span().start..(lexer.span().start + zeros))
  };

  Err(leading_zeros(l).into())
}

#[inline(always)]
fn float_missing_integer_part_error<'a>(_: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  Err(Error::Float(FloatError::MissingIntegerPart))
}

#[inline(always)]
fn handle_fractional_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let remainder = lexer.remainder();
  let mut iter = remainder.chars();

  Err(Error::Float(match iter.next() {
    None | Some(' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',') => {
      UnexpectedEnd::with_name("float".into(), FloatHint::Fractional).into()
    },
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
          Lexeme::Span(span.end..(span.end + curr))
        };

        return Err(Error::Float(UnexpectedLexeme::new(l, FloatHint::Fractional).into()));
      }

      // we reached the end of remainder
      let len = remainder.len();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);
      let l = if len == 1 {
        let pc = PositionedChar::with_position(ch, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(span.end..(span.end + len))
      };

      UnexpectedLexeme::new(l, FloatHint::Fractional).into()
    },
    Some(ch) => {
      let span = lexer.span();
      lexer.bump(ch.len_utf8());

      let l = Lexeme::Char(PositionedChar::with_position(ch, span.end));
      UnexpectedLexeme::new(l, FloatHint::Fractional).into()
    }
  }))
}

#[inline(always)]
fn handle_exponent_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let remainder = lexer.remainder();
  let mut iter = remainder.chars();
  let slice = lexer.slice();


  let hint = || {
    match slice.chars().last() {
      Some('e' | 'E') => FloatHint::Exponent(ExponentHint::SignOrDigit),
      Some('+' | '-') => FloatHint::Exponent(ExponentHint::Digit),
      _ => unreachable!("regex should ensure the last char is 'e', 'E', '+' or '-"),
    }
  };

  Err(Error::Float(match iter.next() {
    None | Some(' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',') => {
      UnexpectedEnd::with_name("float".into(), hint()).into()
    },
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
          Lexeme::Span(span.end..(span.end + curr))
        };

        return Err(Error::Float(UnexpectedLexeme::new(l, hint()).into()));
      }

      // we reached the end of remainder
      let len = remainder.len();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);
      let l = if len == 1 {
        let pc = PositionedChar::with_position(ch, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(span.end..(span.end + len))
      };

      UnexpectedLexeme::new(l, hint()).into()
    },
    // For other characters, just yield one
    Some(ch) => {
      let span = lexer.span();
      lexer.bump(ch.len_utf8());

      let l = Lexeme::Char(PositionedChar::with_position(ch, span.end));
      UnexpectedLexeme::new(l, hint()).into()
    }
  }))
}

#[inline]
fn handle_number_suffix<'a, E>(lexer: &mut Lexer<'a, Token<'a>>, unexpected_suffix: impl FnOnce(Lexeme<char>) -> E) -> Result<&'a str, Error>
where
  E: Into<Error>,
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
          Lexeme::Span(span.end..(span.end + curr))
        };
        return Err(unexpected_suffix(l).into());
      }

      // we reached the end of remainder
      let len = remainder.len();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);

      let l = if len == 1 {
        let pc = PositionedChar::with_position(item, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(span.end..(span.end + len))
      };

      // return the range of the invalid sequence
      Err(unexpected_suffix(l).into())
    }
    // For other characters, just return the float literal
    Some(_) | None => Ok(lexer.slice()),
  }
}

#[derive(Logos, Debug)]
pub enum StringToken {
  #[regex(r#"\\["\\/bfnrt]"#)]
  EscapedCharacter,

  #[regex(r#"\\u[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"#)]
  EscapedUnicode,

  #[token("\"")]
  Quote,

  #[regex(r#"\n|\r|\r\n"#)]
  LineTerminator,

  #[regex(r#"[\u0009\u0020\u0021\u0023-\u005B\u005D-\uFFFF]+"#)]
  StringCharacters,
}

// fn lex_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
//   let remainder = lexer.remainder();
//   let mut string_lexer = StringToken::lexer(remainder);
//   // TODO: Maybe track the properties of the string at parsing time,
//   // so we only need to unescape/split when absolutely required...
//   while let Some(string_token) = string_lexer.next() {
//     match string_token {
//       Ok(StringToken::Quote) => {
//         lexer.bump(string_lexer.span().end);
//         return Ok(lexer.slice());
//       }
//       Ok(StringToken::LineTerminator) => {
//         lexer.bump(string_lexer.span().start);
//         // lexer.extras.error_token = Some(Token::ErrorUnterminatedString);
//         return Err(Error::new(ErrorData::UnterminatedString, lexer.span()));
//       }
//       Ok(
//         StringToken::EscapedCharacter | StringToken::EscapedUnicode | StringToken::StringCharacters,
//       ) => {}
//       Err(_) => {
//         // lexer.extras.error_token = Some(Token::ErrorUnsupportedStringCharacter);
//         return Err(Error::new(ErrorData::UnsupportedStringCharacter, lexer.span()));
//       }
//     }
//   }
//   // lexer.extras.error_token = Some(Token::ErrorUnterminatedString);
//   // None
//   Err(Error::new(ErrorData::UnterminatedString, lexer.span()))
// }

// fn lex_block_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
//   let remainder = lexer.remainder();
//   let mut string_lexer = BlockStringToken::lexer(remainder);
//   while let Some(string_token) = string_lexer.next() {
//     match string_token {
//       Ok(BlockStringToken::TripleQuote) => {
//         lexer.bump(string_lexer.span().end);
//         return Ok(lexer.slice());
//       }
//       Ok(BlockStringToken::EscapedTripleQuote | BlockStringToken::Other) => {}
//       Err(_) => unreachable!(),
//     }
//   }
//   // lexer.extras.error_token = Some(Token::ErrorUnterminatedBlockString);
//   // None
//   Err(Error::new(ErrorData::UnterminatedBlockString, lexer.span()))
// }

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum BlockStringToken {
  #[token("\\\"\"\"")]
  EscapedTripleQuote,

  #[token("\"\"\"")]
  TripleQuote,

  #[regex(r#"[\u0009\u000A\u000D\u0020-\uFFFF]"#)]
  Other,
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
mod tests {
  use super::*;

  fn assert_token(source: &str, kind: Token, length: usize) {
    let mut lexer = Token::lexer(source);
    assert_eq!(
      lexer.next(),
      Some(Ok(kind)),
      "Testing the lexing of string '{source}'"
    );
    assert_eq!(
      lexer.span(),
      0..length,
      "Testing the lexing of string '{source}'"
    );
  }

  fn assert_error(source: &str, length: usize) {
    let mut lexer = Token::lexer(source);
    assert!(matches!(lexer.next(), Some(Err(_))), "Testing lexing fails for string '{source}'");
    assert_eq!(
      lexer.span(),
      0..length,
      "Testing the lexing of string '{source}'"
    );
  }

  #[test]
  fn test_unexpected_character() {
    let mut lexer = Token::lexer("+1");
    let err = lexer.next().unwrap().unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.char(), &'+');
    assert_eq!(err.position(), 0);

    let mut lexer = Token::lexer("-A");
    let err = lexer.next().unwrap().unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.char(), &'-');
    assert_eq!(err.position(), 0);
  }

  #[test]
  fn test_unknown_character() {
    let mut lexer = Token::lexer("<");
    let err = lexer.next().unwrap().unwrap_err().unwrap_unknown_character();
    assert_eq!(err.char(), &'<');
    assert_eq!(err.position(), 0);
  }

  #[test]
  fn test_number_leading_zero() {
    let mut lexer = Token::lexer("00");
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("-01");
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("01.");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("-01.");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("01.23");
    // let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_leading_zero();
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("-01.23");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("01e3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("-01E3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("01e+3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("-01E+3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("01e-3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));

    let mut lexer = Token::lexer("-01E-3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZeros(_))))));
  }

  #[test]
  fn test_invalid_number_suffix() {
    let mut lexer = Token::lexer("0abc");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_span();
    assert_eq!(err, 1..4);

    let mut lexer = Token::lexer("0a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 1);

    let mut lexer = Token::lexer("-0abc");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_span();
    assert_eq!(err, 2..5);

    let mut lexer = Token::lexer("-0a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 2);

    let mut lexer = Token::lexer("123abc");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_span();
    assert_eq!(err, 3..6);

    let mut lexer = Token::lexer("123a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 3);

    let mut lexer = Token::lexer("-123abc");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_span();
    assert_eq!(err, 4..7);

    let mut lexer = Token::lexer("-123a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_int().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 4);

    let mut lexer = Token::lexer("123.45a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 6);

    let mut lexer = Token::lexer("-123.45a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 7);

    let mut lexer = Token::lexer("123e3a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 5);

    let mut lexer = Token::lexer("-123E3a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 6);

    let mut lexer = Token::lexer("123e+3a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 6);

    let mut lexer = Token::lexer("-123E+3a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 7);

    let mut lexer = Token::lexer("123e-3a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 6);

    let mut lexer = Token::lexer("-123E-3a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'a');
    assert_eq!(err.position(), 7);


    let mut lexer = Token::lexer("1.23.4");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_span();
    assert_eq!(err, 4..6);

    let mut lexer = Token::lexer("-1.23.4 ");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_span();
    assert_eq!(err, 5..7);
    assert_eq!(lexer.span(), 0..7);

    // check that we don't consume trailing valid items
    let mut lexer = Token::lexer("1.23.{}");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'.');
    assert_eq!(err.position(), 4);
    assert_eq!(lexer.span(), 0..5);

    let mut lexer = Token::lexer("1.23. {}");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'.');
    assert_eq!(err.position(), 4);
    assert_eq!(lexer.span(), 0..5);

    let mut lexer = Token::lexer("1.23. []");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'.');
    assert_eq!(err.position(), 4);
    assert_eq!(lexer.span(), 0..5);

    let mut lexer = Token::lexer("1.23. foo");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'.');
    assert_eq!(err.position(), 4);
    assert_eq!(lexer.span(), 0..5);

    let mut lexer = Token::lexer("1.23. $foo");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_suffix().unwrap_char();
    assert_eq!(err.char(), &'.');
    assert_eq!(err.position(), 4);
    assert_eq!(lexer.span(), 0..5);

    // // assert_token(".123", Token::ErrorFloatLiteralMissingZero, 4);
  }

  #[test]
  fn test_missing_integer_part() {
    let mut lexer = Token::lexer(".123");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::MissingIntegerPart)))));

    let mut lexer = Token::lexer("-.123");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::MissingIntegerPart)))));

    let mut lexer = Token::lexer(".123e3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::MissingIntegerPart)))));
    let mut lexer = Token::lexer("-.123E3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::MissingIntegerPart)))));

    let mut lexer = Token::lexer(".123e+3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::MissingIntegerPart)))));
    let mut lexer = Token::lexer("-.123E+3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::MissingIntegerPart)))));
  }

  #[test]
  fn test_unexpected_float_eof() {
    let mut lexer = Token::lexer("1.");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Fractional);

    let mut lexer = Token::lexer("-1.");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Fractional);

    let mut lexer = Token::lexer("1e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("-1e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("1e+");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("-1e+");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("1e-");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("-1e-");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("1.0e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("-1.0e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("1.0e-");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("-1.0e-");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("1.0e+");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("-1.0e+");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));
  }

  #[test]
  fn test_unexpected_number_lexme() {
    let mut lexer = Token::lexer("1.a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'a');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
    assert_eq!(lexer.span(), 0..3);

    let mut lexer = Token::lexer("-1.a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'a');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
    assert_eq!(lexer.span(), 0..4);

    let mut lexer = Token::lexer("1.A");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'A');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
    assert_eq!(lexer.span(), 0..3);

    let mut lexer = Token::lexer("-1.A");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'A');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
    assert_eq!(lexer.span(), 0..4);

    let mut lexer = Token::lexer("1.abc");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 2..5);
    assert_eq!(lexer.span(), 0..5);

    let mut lexer = Token::lexer("-1.abc");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 3..6);
    assert_eq!(lexer.span(), 0..6);


    let mut lexer = Token::lexer("1.e1");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 2..4);
    assert_eq!(lexer.span(), 0..4);

    let mut lexer = Token::lexer("-1.e1");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Fractional);
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 3..5);
    assert_eq!(lexer.span(), 0..5);

    let mut lexer = Token::lexer("1.0eA");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'A');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 4);
    assert_eq!(lexer.span(), 0..5);

    let mut lexer = Token::lexer("-1.0eA");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'A');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 5);
    assert_eq!(lexer.span(), 0..6);

    let mut lexer = Token::lexer("1.0eA123.456 some_name");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 4..12);
    assert_eq!(lexer.span(), 0..12);

    let mut lexer = Token::lexer("-1.0eA123.456 some_name");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 5..13);
    assert_eq!(lexer.span(), 0..13);

    let mut lexer = Token::lexer("1eA");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'A');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
    assert_eq!(lexer.span(), 0..3);

    let mut lexer = Token::lexer("-1eA");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_char_ref().char(), &'A');
    assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
    assert_eq!(lexer.span(), 0..4);


    let mut lexer = Token::lexer("1eA123.456");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 2..10);
    assert_eq!(lexer.span(), 0..10);

    let mut lexer = Token::lexer("-1eA123.456");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 3..11);
    assert_eq!(lexer.span(), 0..11);

    let mut lexer = Token::lexer("1eA123.456 some_name");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 2..10);
    assert_eq!(lexer.span(), 0..10);

    let mut lexer = Token::lexer("-1eA123.456 some_name");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_lexeme();
    assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.lexeme().unwrap_span_ref().clone(), 3..11);
    assert_eq!(lexer.span(), 0..11);
  }

  #[test]
  fn test_integer_ok() {
    const INPUT: &[(&str, Token, usize)] = &[
      ("4", Token::IntegerLiteral("4"), 1),
      ("-4", Token::IntegerLiteral("-4"), 2),
      ("9", Token::IntegerLiteral("9"), 1),
      ("0", Token::IntegerLiteral("0"), 1),
      ("-0", Token::IntegerLiteral("-0"), 2),
    ];

    for (source, kind, length) in INPUT {
      assert_token(source, *kind, *length);
    }
  }

  #[test]
  fn test_float_ok() {
    const INPUT: &[(&str, Token, usize)] = &[
      ("4.123", Token::FloatLiteral("4.123"), 5),
      ("-4.123", Token::FloatLiteral("-4.123"), 6),
      ("0.123", Token::FloatLiteral("0.123"), 5),
      ("123e4", Token::FloatLiteral("123e4"), 5),
      ("123E4", Token::FloatLiteral("123E4"), 5),
      ("123e-4", Token::FloatLiteral("123e-4"), 6),
      ("123e+4", Token::FloatLiteral("123e+4"), 6),
      ("-1.123e4", Token::FloatLiteral("-1.123e4"), 8),
      ("-1.123E4", Token::FloatLiteral("-1.123E4"), 8),
      ("-1.123e-4", Token::FloatLiteral("-1.123e-4"), 9),
      ("-1.123e+4", Token::FloatLiteral("-1.123e+4"), 9),
      ("-1.123e4567", Token::FloatLiteral("-1.123e4567"), 11),
    ];

    for (source, kind, length) in INPUT {
      assert_token(source, *kind, *length);
    }
  }

  // #[test]
  // fn test_string_lexing() {
  //   let input = r#"
  //            "test"
  //            "escaped \" quote"
  //            "unterminated
  //            "
  //        "#;
  //   let mut lexer = Token::lexer(input);

  //   assert_eq!(lexer.next(), Some(Ok(Token::StringLiteral("\"test\""))));
  //   assert_eq!(lexer.slice(), "\"test\"");

  //   assert_eq!(
  //     lexer.next(),
  //     Some(Ok(Token::StringLiteral(r#""escaped \" quote""#)))
  //   );
  //   assert_eq!(lexer.slice(), r#""escaped \" quote""#);

  //   // assert_eq!(lexer.next(), Some(Err(())));
  //   // assert_eq!(
  //   //   lexer.extras.error_token,
  //   //   Some(Token::ErrorUnterminatedString)
  //   // );
  //   assert_eq!(lexer.slice(), "\"unterminated");
  // }

  // #[test]
  // fn test_invalid_character_lexing() {
  //   let input = r#"
  //            {
  //                %%%
  //                __typename
  //                *
  //            }
  //        "#;
  //   let mut lexer = Token::lexer(input);

  //   assert_eq!(lexer.next(), Some(Ok(Token::BraceOpen)));
  //   assert_eq!(lexer.slice(), "{");

  //   assert_eq!(lexer.next(), Some(Err(())));
  //   assert_eq!(lexer.slice(), "%");

  //   assert_eq!(lexer.next(), Some(Err(())));
  //   assert_eq!(lexer.slice(), "%");

  //   assert_eq!(lexer.next(), Some(Err(())));
  //   assert_eq!(lexer.slice(), "%");

  //   assert_eq!(lexer.next(), Some(Ok(Token::Identifier("__typename"))));
  //   assert_eq!(lexer.slice(), "__typename");

  //   assert_eq!(lexer.next(), Some(Err(())));
  //   assert_eq!(lexer.slice(), "*");

  //   assert_eq!(lexer.next(), Some(Ok(Token::BraceClose)));
  //   assert_eq!(lexer.slice(), "}");

  //   assert_eq!(lexer.next(), None);
  // }

  // #[test]
  // fn test_block_string_lexing() {
  //   let input = r#"
  //            # escaped
  //            """tes\"""t"""
  //            # empty
  //            """"""
  //            # 2 quotes in a string
  //            """"" """
  //            """
  //                multi-
  //                line
  //            """
  //            """unterminated
  //        "#;
  //   let mut lexer = Token::lexer(input);

  //   assert_eq!(
  //     lexer.next(),
  //     Some(Ok(Token::BlockStringLiteral(r#""""tes\"""t""""#)))
  //   );
  //   assert_eq!(lexer.slice(), r#""""tes\"""t""""#);

  //   assert_eq!(
  //     lexer.next(),
  //     Some(Ok(Token::BlockStringLiteral(r#""""""""#)))
  //   );
  //   assert_eq!(lexer.slice(), r#""""""""#);

  //   assert_eq!(
  //     lexer.next(),
  //     Some(Ok(Token::BlockStringLiteral(r#"""""" """"#)))
  //   );
  //   assert_eq!(lexer.slice(), r#"""""" """"#);

  //   assert_eq!(
  //     lexer.next(),
  //     Some(Ok(Token::BlockStringLiteral(
  //       r#""""
  //                multi-
  //                line
  //            """"#
  //     )))
  //   );
  //   assert_eq!(
  //     lexer.slice(),
  //     r#""""
  //                multi-
  //                line
  //            """"#
  //   );

  //   // assert_eq!(lexer.next(), Some(Err(())));
  //   // assert_eq!(
  //   //   lexer.extras.error_token,
  //   //   Some(Token::ErrorUnterminatedBlockString)
  //   // );
  //   // Unterminated string just consumes the starting quotes
  //   assert_eq!(lexer.slice(), r#"""""#);
  // }

  #[test]
  fn test_bom_lexing() {
    let input = "\u{feff}";

    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), None);
  }
}
