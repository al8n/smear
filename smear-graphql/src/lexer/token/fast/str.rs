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
use logos::{Lexer, Logos};

use crate::lexer::number::{lex_exponent, lex_fractional};

use super::super::error::{self, *};

pub type Error = error::Error<char>;

#[derive(Default, Eq, PartialEq)]
pub struct TokenExtras {
  /// Token callbacks might store an error token kind in here before failing.
  /// This is then picked up in the parser to turn the `Error` token into a
  /// more specific variant.
  error_token: Option<Token<'static>>,
}

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(
  extras = TokenExtras,
  skip r"[ \t,\u{FEFF}]+|#([^\n\r]*(\r\n|\r|\n))*",
  error(Error, |lexer| match lexer.slice().chars().next() {
    Some(ch) => if ch == '+' {
      UnexpectedCharacter::new(ch, 0).into()
    } else {
      UnknownCharacter::new(ch, 0).into()
    },
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

  #[regex(r"-?(0|[1-9][0-9]*)[.eE]", lex_float)]
  FloatLiteral(&'a str),

  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
  Identifier(&'a str),

  #[regex(r"-?(0|[1-9][0-9]*)", |lexer| lexer.slice())] 
  #[regex(r"-?(0|[1-9][0-9]*)([^\d.eE])", unexpected_int_suffix)]
  #[regex(r"-?0[0-9]+", leading_zero_error)]
  #[regex(r"-[^\d \t,\r\n\ufeff]", unexpected_character)]
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
fn leading_zero_error<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let remainder = lexer.remainder();
  match remainder.chars().next() {
    Some('.' | 'e' | 'E') => Err(Error::Float(FloatError::LeadingZero)), 
    Some(_)  => Err(Error::Int(IntError::LeadingZero)),
    None => Err(Error::Int(IntError::LeadingZero)),
  }
}

#[inline(always)]
fn unexpected_int_suffix<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  Err(IntError::UnexpectedSuffix(lexer.slice().chars().last().expect("must have an invalid character")).into())
}

#[inline(always)]
fn unexpected_character<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let (pos, ch) = lexer.slice().char_indices().last().expect("must have an invalid character");
  Err(UnexpectedCharacter::new(ch, pos + lexer.span().start).into())
}

#[inline(always)]
fn lex_float<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let last = lexer.slice().chars().last().expect("must have a character");

  match last {
    '.' => match lex_fractional(lexer) {
      Ok(()) => Ok(lexer.slice()),
      Err(e) => Err(Error::Float(e)),
    },
    'e' | 'E' => match lex_exponent(lexer) {
      Ok(()) => Ok(lexer.slice()),
      Err(e) => Err(Error::Float(e.into())),
    },
    _ => unreachable!("must be '.' or 'e' or 'E'"),
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
  use crate::lexer::number::{ExponentHint, FractionalHint};

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
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::LeadingZero)))));

    let mut lexer = Token::lexer("-01");
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::LeadingZero)))));

    let mut lexer = Token::lexer("01.");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("-01.");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("01.23");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("-01.23");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("01e3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("-01E3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("01e+3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("-01E+3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("01e-3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));

    let mut lexer = Token::lexer("-01E-3");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::LeadingZero)))));
  }

  #[test]
  fn test_invalid_number_suffix() {
    let mut lexer = Token::lexer("0a");
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::UnexpectedSuffix('a'))))));

    let mut lexer = Token::lexer("-0a");
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::UnexpectedSuffix('a'))))));

    let mut lexer = Token::lexer("123a");
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::UnexpectedSuffix('a'))))));

    let mut lexer = Token::lexer("-123a");
    assert!(matches!(lexer.next(), Some(Err(Error::Int(IntError::UnexpectedSuffix('a'))))));

    let mut lexer = Token::lexer("123.45a");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('a'))))));

    let mut lexer = Token::lexer("-123.45a");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('a'))))));

    let mut lexer = Token::lexer("1.2e3e");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('e'))))));

    let mut lexer = Token::lexer("-1.2e3e");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('e'))))));

    let mut lexer = Token::lexer("1.2e3.4");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('.'))))));

    let mut lexer = Token::lexer("-1.2e3.4");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('.'))))));

    let mut lexer = Token::lexer("1.23.4");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('.'))))));

    let mut lexer = Token::lexer("-1.23.4");
    assert!(matches!(lexer.next(), Some(Err(Error::Float(FloatError::UnexpectedSuffix('.'))))));

    // // assert_token(".123", Token::ErrorFloatLiteralMissingZero, 4);

    // // check that we don't consume trailing valid items
    // assert_token("1.23.{}", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. {}", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. []", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. foo", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. $foo", Token::ErrorNumberLiteralTrailingInvalid, 5);
  }

  #[test]
  fn test_unexpected_float_eof() {
    let mut lexer = Token::lexer("1.");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Fractional(FractionalHint::Digit));

    let mut lexer = Token::lexer("-1.");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Fractional(FractionalHint::Digit));

    let mut lexer = Token::lexer("1e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("-1e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("1.0e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("-1.0e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Exponent(ExponentHint::SignOrDigit));
  }

  #[test]
  fn test_unexpected_number_character() {
    let mut lexer = Token::lexer("1.a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), &'a');

    let mut lexer = Token::lexer("-1.a");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), &'a');

    let mut lexer = Token::lexer("1.e1");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), &'e');

    let mut lexer = Token::lexer("-1.e1");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), &'e');

    let mut lexer = Token::lexer("1.A");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), &'A');

    let mut lexer = Token::lexer("-1.A");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), &'A');

    let mut lexer = Token::lexer("-A");
    let err = lexer.next().unwrap().unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.char(), &'A');
    assert_eq!(err.position(), 1);

    let mut lexer = Token::lexer("-A.1");
    let err = lexer.next().unwrap().unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.char(), &'A');
    assert_eq!(err.position(), 1);

    let mut lexer = Token::lexer("-A123456.1e2");
    let err = lexer.next().unwrap().unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.char(), &'A');
    assert_eq!(err.position(), 1);

    let mut lexer = Token::lexer("1.0eA");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.found(), &'A');

    let mut lexer = Token::lexer("-1.0eA");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_character();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert_eq!(err.found(), &'A');
  }

  #[test]
  fn test_float_error() {
    let mut lexer = Token::lexer("123.45e");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Exponent(ExponentHint::SignOrDigit));

    let mut lexer = Token::lexer("123.45e-");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Exponent(ExponentHint::Digit));

    let mut lexer = Token::lexer("123.45e+");
    let err = lexer.next().unwrap().unwrap_err().unwrap_float().unwrap_unexpected_eof();
    assert_eq!(err, FloatHint::Exponent(ExponentHint::Digit));
  }

  // #[test]
  // fn test_invliad_float() {
  //   const INPUT: &[&str] = &["01.23", "1.", "1e", "1e-", "1.e1", "1.A", "1.0e", "1.0eA", "1.2e3e", "1.2e3.4", ".123", "1.23.{}", "1.23. {}", "1.23. []", "1.23. foo", "1.23. $foo"];

  //   for s in INPUT {
  //     let mut lexer = Token::lexer(s);

  //     match lexer.next().unwrap() {
  //       Ok(_) => {
  //         panic!("should fail for source: {s}");
  //       }
  //       Err(e) => {
  //         assert_eq!(e.data, ErrorData::InvalidNumberLiteral, "source: {s}");
  //         assert_eq!(e.span, 0..s.len(), "source: {s}");
  //       }
  //     }
  //   }
  // }

  // #[test]
  // fn test_number_successes() {
  //   assert_token("4", Token::IntegerLiteral("4"), 1);
  //   assert_token("4.123", Token::FloatLiteral("4.123"), 5);
  //   assert_token("-4", Token::IntegerLiteral("-4"), 2);
  //   assert_token("9", Token::IntegerLiteral("9"), 1);
  //   assert_token("0", Token::IntegerLiteral("0"), 1);
  //   assert_token("-4.123", Token::FloatLiteral("-4.123"), 6);
  //   assert_token("0.123", Token::FloatLiteral("0.123"), 5);
  //   assert_token("123e4", Token::FloatLiteral("123e4"), 5);
  //   assert_token("123E4", Token::FloatLiteral("123E4"), 5);
  //   assert_token("123e-4", Token::FloatLiteral("123e-4"), 6);
  //   assert_token("123e+4", Token::FloatLiteral("123e+4"), 6);
  //   assert_token("-1.123e4", Token::FloatLiteral("-1.123e4"), 8);
  //   assert_token("-1.123E4", Token::FloatLiteral("-1.123E4"), 8);
  //   assert_token("-1.123e-4", Token::FloatLiteral("-1.123e-4"), 9);
  //   assert_token("-1.123e+4", Token::FloatLiteral("-1.123e+4"), 9);
  //   assert_token("-1.123e4567", Token::FloatLiteral("-1.123e4567"), 11);
  //   assert_token("-0", Token::IntegerLiteral("-0"), 2);
  // }

  

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
