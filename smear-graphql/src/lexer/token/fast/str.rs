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

use core::{fmt, ops::Range};
use std::borrow::Cow;

use logos::{Lexer, Logos};
use smear_parser::lexer::State;

#[derive(Default, Eq, PartialEq)]
pub struct TokenExtras {
  /// Token callbacks might store an error token kind in here before failing.
  /// This is then picked up in the parser to turn the `Error` token into a
  /// more specific variant.
  error_token: Option<Token<'static>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ExpectedExponentComponent {
  #[display("digit")]
  Digit,
  #[display("sign")]
  Sign,
  #[display("digit or sign")]
  DigitOrSign,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorData {
  UnterminatedString,
  UnsupportedStringCharacter,
  UnterminatedBlockString,
  InvalidNumberLiteral,
  LeadingZero,
  UnexpectedIntSuffix(char),
  UnexpectedFloatSuffix(char),
  UnexpectedExponentSuffix(char),
  UnexpectedFloatEof,
  UnexpectedExponentEof(ExpectedExponentComponent),
  #[default]
  Empty,
}

impl core::fmt::Display for ErrorData {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UnterminatedString => write!(f, "unterminated string"),
      Self::UnsupportedStringCharacter => write!(f, "unsupported string character"),
      Self::UnterminatedBlockString => write!(f, "unterminated block string"),
      Self::InvalidNumberLiteral => write!(f, "invalid number literal"),
      Self::LeadingZero => write!(f, "leading zero"),
      Self::UnexpectedIntSuffix(ch) => write!(f, "unexpected character `{ch}` as integer suffix"),
      Self::UnexpectedFloatSuffix(ch) => write!(f, "unexpected character `{ch}` as float suffix"),
      Self::UnexpectedFloatEof => write!(f, "unexpected end of float"),
      Self::UnexpectedExponentEof(exp) => write!(f, "unexpected end of exponent, expected exponent {exp}"),
      Self::UnexpectedExponentSuffix(ch) => write!(f, "unexpected character `{ch}` as exponent suffix"),
      Self::Empty => write!(f, "empty error"),
    }
  }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Error {
  data: ErrorData,
  span: Range<usize>,
  msg: Cow<'static, str>,
}

impl Error {
  /// Creates a new error.
  #[inline]
  pub const fn new(data: ErrorData, span: Range<usize>) -> Self {
    Self { data, span, msg: Cow::Borrowed("") }
  }

  /// Creates a new error with a message.
  #[inline]
  pub fn with_message(
    data: ErrorData,
    span: Range<usize>,
    msg: impl Into<Cow<'static, str>>,
  ) -> Self {
    Self { data, span, msg: msg.into() }
  }
}

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(
  extras = TokenExtras,
  skip r"[ \t\f,\u{FEFF}]+|#([^\n\r]*(\r\n|\r|\n))*",
  error = Error,
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
  Spread,

  // IntegerPart:    -?(0|[1-9][0-9]*)
  // FractionalPart: \\.[0-9]+
  // ExponentPart:   [eE][+-]?[0-9]+
  // #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lex| lex.slice())]
  // #[regex("-?0[0-9]+(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", invalid_number)]
  // // #[regex("-?(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+)", invalid_number)]
  // // #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)[.a-zA-Z_]", invalid_number)]
  // #[regex("-?(0|[1-9][0-9]*)\\.", lex_fractional)] 
  // #[regex("-?(0|[1-9][0-9]*)[eE]", lex_exponent)]
  // FloatLiteral(&'a str),

  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
  Identifier(&'a str),

  // #[regex("-?(0|[1-9][0-9]*)", |lex| lex.slice())]
  // #[regex("-?0[0-9]+", invalid_number)]
  // // #[regex("-?(0|[1-9][0-9]*)([a-zA-Z_]|[eE][+-]?[0-9]?)", invalid_number)]
  // IntegerLiteral(&'a str),

  #[token("\"", lex_string)]
  StringLiteral(&'a str),

  #[token("\"\"\"", lex_block_string)]
  BlockStringLiteral(&'a str),
}

pub enum NumberToken<'a> {
  Float(&'a str),
  Int(&'a str),
}

// fn lex_number<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<NumberToken<'a>, Error> {
//   let remainder = lexer.remainder();
//   match remainder.chars().next() {
//     None => Ok(NumberToken::Int(lexer.slice())),
//     Some(ch) => {
//       match ch {
//         '_' | 'a'..='d' | 'f'..='z' | 'A'..='D' | 'F'..='Z' => {
//           lexer.bump(1);
//           Err(Error::new(ErrorData::UnexpectedIntSuffix(ch), lexer.span()))
//         },
//         '.' => {
//           let mut float_part = FractionalPart::lexer(remainder);

//           match float_part.next() {
//             Some(Ok(FractionalPart::FractionalOnly)) => {
//               lexer.bump(float_part.span().end);
//               Ok(NumberToken::Float(lexer.slice()))
//             }
//             _ => Err(Error {
//               kind: ErrorKind::InvalidNumberLiteral,
//               span: lexer.span(),
//             }),
//           }
//         },
//         'e' | 'E' => {
//           let mut float_part = FractionalPart::lexer(remainder);

//           match float_part.next() {
//             Some(Ok(FractionalPart::FractionalOnly)) => {
//               lexer.bump(float_part.span().end);
//               Ok(NumberToken::Float(lexer.slice()))
//             }
//             _ => Err(Error::new(ErrorD, span) {
//               kind: ErrorKind::InvalidNumberLiteral,
//               span: lexer.span(),
//             }),
//           }
//         },
//         _ => Ok(NumberToken::Int(lexer.slice())),
//       }
//     },
//   }
// }


#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum ExponentPart {
  #[regex(r"[+-]?[0-9]+")]
  Value,
  #[regex(r"[+-](\D)?", |lexer| lexer.slice().chars().nth(1))]
  UnexpectedExponentSuffix(Option<char>),
  #[regex(r"(\D)?", |lexer| lexer.slice().chars().next())]
  UnexpectedExponentWithoutSignSuffix(Option<char>),
}

#[inline(always)]
fn lex_exponent_part<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let remainder = lexer.remainder();
  if remainder.is_empty() {
    return Err(Error::new(ErrorData::UnexpectedFloatEof, lexer.span()));
  }

  let mut exponent_lexer = ExponentPart::lexer(remainder);
  match exponent_lexer.next() {
    Some(Ok(ExponentPart::Value)) => {
      lexer.bump(exponent_lexer.span().end);
      Ok(lexer.slice())
    }
    Some(Ok(ExponentPart::UnexpectedExponentSuffix(ch))) => {
      lexer.bump(exponent_lexer.span().end);
      match ch {
        Some(ch) => {
          Err(Error::new(ErrorData::UnexpectedExponentSuffix(ch), lexer.span()))
        },
        None => {
          Err(Error::new(ErrorData::UnexpectedExponentEof(ExpectedExponentComponent::Digit), lexer.span()))
        }
      }
    }
    Some(Ok(ExponentPart::UnexpectedExponentWithoutSignSuffix(ch))) => {
      lexer.bump(exponent_lexer.span().end);
      match ch {
        Some(ch) => {
          Err(Error::new(ErrorData::UnexpectedExponentSuffix(ch), lexer.span()))
        },
        None => {
          Err(Error::new(ErrorData::UnexpectedExponentEof(ExpectedExponentComponent::DigitOrSign), lexer.span()))
        }
      }
    }
    _ => {
      lexer.bump(exponent_lexer.span().end);
      Err(Error::new(ErrorData::UnexpectedExponentEof(ExpectedExponentComponent::DigitOrSign), lexer.span()))
    },
  }
}

// #[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
// #[logos(error = Error)]
// enum FractionalPart {
//   #[regex(r"\\.[0-9]+")]
//   FractionalOnly,
//   #[regex(r"[0-9]+[eE]", lex_fractional_exponent)]
//   // #[regex(r"[0-9][eE][+-]?", invalid_number)]
//   Exponent,
//   #[regex(r"[eE][+-]?([.a-zA-Z0-9_])")]
//   Invalid,
// }

// #[inline(always)]
// fn lex_fractional_exponent<'a>(lexer: &mut Lexer<'a, FractionalPart>) -> Result<(), Error> {
//   let remainder = lexer.remainder();
//   let mut exponent_lexer = ExponentPart::lexer(remainder);
//   match exponent_lexer.next() {
//     Some(Ok(ExponentPart::Value)) => {
//       lexer.bump(exponent_lexer.span().end);
//       Ok(())
//     }
//     _ => Err(Error {
//       kind: ErrorKind::InvalidNumberLiteral,
//       span: lexer.span(),
//     }),
//   }
// }


// #[inline(always)]
// fn lex_fractional<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
//   let remainder = lexer.remainder();
//   let mut fractional_lexer = FractionalPart::lexer(remainder);
//   match fractional_lexer.next() {
//     Some(Ok(FractionalPart::FractionalOnly)) => {
//       lexer.bump(fractional_lexer.span().end);
//       Ok(lexer.slice())
//     }
//     _ => Err(Error {
//       kind: ErrorKind::InvalidNumberLiteral,
//       span: lexer.span(),
//     }),
//   }
// }

// #[inline(always)]
// fn invalid_number<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
//   let span = lexer.span();
//   Err(Error {
//     kind: ErrorKind::InvalidNumberLiteral,
//     span,
//   })
// }

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

fn lex_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let remainder = lexer.remainder();
  let mut string_lexer = StringToken::lexer(remainder);
  // TODO: Maybe track the properties of the string at parsing time,
  // so we only need to unescape/split when absolutely required...
  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Ok(StringToken::Quote) => {
        lexer.bump(string_lexer.span().end);
        return Ok(lexer.slice());
      }
      Ok(StringToken::LineTerminator) => {
        lexer.bump(string_lexer.span().start);
        // lexer.extras.error_token = Some(Token::ErrorUnterminatedString);
        return Err(Error::new(ErrorData::UnterminatedString, lexer.span()));
      }
      Ok(
        StringToken::EscapedCharacter | StringToken::EscapedUnicode | StringToken::StringCharacters,
      ) => {}
      Err(_) => {
        // lexer.extras.error_token = Some(Token::ErrorUnsupportedStringCharacter);
        return Err(Error::new(ErrorData::UnsupportedStringCharacter, lexer.span()));
      }
    }
  }
  // lexer.extras.error_token = Some(Token::ErrorUnterminatedString);
  // None
  Err(Error::new(ErrorData::UnterminatedString, lexer.span()))
}

fn lex_block_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let remainder = lexer.remainder();
  let mut string_lexer = BlockStringToken::lexer(remainder);
  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Ok(BlockStringToken::TripleQuote) => {
        lexer.bump(string_lexer.span().end);
        return Ok(lexer.slice());
      }
      Ok(BlockStringToken::EscapedTripleQuote | BlockStringToken::Other) => {}
      Err(_) => unreachable!(),
    }
  }
  // lexer.extras.error_token = Some(Token::ErrorUnterminatedBlockString);
  // None
  Err(Error::new(ErrorData::UnterminatedBlockString, lexer.span()))
}

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
      Token::BlockStringLiteral(_) => "block string (e.g. '\"\"\"hi\"\"\"')",
      // Token::ErrorFloatLiteralMissingZero => "unsupported number (int or float) literal",
      // Token::ErrorNumberLiteralLeadingZero => "unsupported number (int or float) literal",
      // Token::ErrorNumberLiteralTrailingInvalid => "unsupported number (int or float) literal",
      Token::StringLiteral(_) => "string literal (e.g. '\"...\"')",
      // Token::ErrorUnterminatedString => "unterminated string",
      // Token::ErrorUnsupportedStringCharacter => "unsupported character in string",
      // Token::ErrorUnterminatedBlockString => "unterminated block string",
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
  fn test_exponent_part() {
    let mut lexer = Token::lexer("+123");
    let val = lex_exponent_part(&mut lexer).unwrap();
    assert_eq!(val, "+123");

    let mut lexer = Token::lexer("-1");
    let val = lex_exponent_part(&mut lexer).unwrap();
    assert_eq!(val, "-1");

    let mut lexer = Token::lexer("-");
    let err = lex_exponent_part(&mut lexer).unwrap_err();
    assert_eq!(err.data, ErrorData::UnexpectedExponentEof(ExpectedExponentComponent::Digit));
    assert_eq!(&lexer.slice()[lexer.span()], "-");

    let mut lexer = Token::lexer("abcd");
    let err = lex_exponent_part(&mut lexer).unwrap_err();
    assert_eq!(err.data, ErrorData::UnexpectedExponentSuffix('a'));
    assert_eq!(&lexer.slice()[lexer.span()], "a");

    let mut lexer = Token::lexer("-abcd");
    let err = lex_exponent_part(&mut lexer).unwrap_err();
    assert_eq!(err.data, ErrorData::UnexpectedExponentSuffix('a'));
    assert_eq!(&lexer.slice()[lexer.span()], "-a");

    let mut lexer = Token::lexer(".");
    let err = lex_exponent_part(&mut lexer).unwrap_err();
    assert_eq!(err.data, ErrorData::UnexpectedExponentSuffix('.'));
    assert_eq!(&lexer.slice()[lexer.span()], ".");

    let mut lexer = Token::lexer("+.");
    let err = lex_exponent_part(&mut lexer).unwrap_err();
    assert_eq!(err.data, ErrorData::UnexpectedExponentSuffix('.'));
    assert_eq!(&lexer.slice()[lexer.span()], "+.");
  }

  #[test]
  fn test_number_failures() {
    // assert_token("00", Token::ErrorNumberLiteralLeadingZero, 2);
    // assert_token("01", Token::ErrorNumberLiteralLeadingZero, 2);
    // assert_token("-01", Token::ErrorNumberLiteralLeadingZero, 3);
    // assert_error("+1", 1);
    // assert_token("01.23", Token::ErrorNumberLiteralLeadingZero, 5);
    // assert_token("1.", Token::ErrorNumberLiteralTrailingInvalid, 2);
    // assert_token("1e", Token::ErrorNumberLiteralTrailingInvalid, 2);
    // assert_token("1.e1", Token::ErrorNumberLiteralTrailingInvalid, 2);
    // assert_token("1.A", Token::ErrorNumberLiteralTrailingInvalid, 2);
    // assert_error("-A", 1);
    // assert_token("1.0e", Token::ErrorNumberLiteralTrailingInvalid, 4);
    // assert_token("1.0eA", Token::ErrorNumberLiteralTrailingInvalid, 4);
    // assert_token("1.2e3e", Token::ErrorNumberLiteralTrailingInvalid, 6);
    // assert_token("1.2e3.4", Token::ErrorNumberLiteralTrailingInvalid, 6);
    // assert_token("1.23.4", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // // assert_token(".123", Token::ErrorFloatLiteralMissingZero, 4);

    // // check that we don't consume trailing valid items
    // assert_token("1.23.{}", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. {}", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. []", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. foo", Token::ErrorNumberLiteralTrailingInvalid, 5);
    // assert_token("1.23. $foo", Token::ErrorNumberLiteralTrailingInvalid, 5);
  }

  #[test]
  fn test_invliad_float() {
    const INPUT: &[&str] = &["01.23", "1.", "1e", "1e-", "1.e1", "1.A", "1.0e", "1.0eA", "1.2e3e", "1.2e3.4", ".123", "1.23.{}", "1.23. {}", "1.23. []", "1.23. foo", "1.23. $foo"];

    for s in INPUT {
      let mut lexer = Token::lexer(s);

      match lexer.next().unwrap() {
        Ok(_) => {
          panic!("should fail for source: {s}");
        }
        Err(e) => {
          assert_eq!(e.data, ErrorData::InvalidNumberLiteral, "source: {s}");
          assert_eq!(e.span, 0..s.len(), "source: {s}");
        }
      }
    }
  }

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

  

  #[test]
  fn test_string_lexing() {
    let input = r#"
             "test"
             "escaped \" quote"
             "unterminated
             "
         "#;
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::StringLiteral("\"test\""))));
    assert_eq!(lexer.slice(), "\"test\"");

    assert_eq!(
      lexer.next(),
      Some(Ok(Token::StringLiteral(r#""escaped \" quote""#)))
    );
    assert_eq!(lexer.slice(), r#""escaped \" quote""#);

    // assert_eq!(lexer.next(), Some(Err(())));
    // assert_eq!(
    //   lexer.extras.error_token,
    //   Some(Token::ErrorUnterminatedString)
    // );
    assert_eq!(lexer.slice(), "\"unterminated");
  }

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

  #[test]
  fn test_block_string_lexing() {
    let input = r#"
             # escaped
             """tes\"""t"""
             # empty
             """"""
             # 2 quotes in a string
             """"" """
             """
                 multi-
                 line
             """
             """unterminated
         "#;
    let mut lexer = Token::lexer(input);

    assert_eq!(
      lexer.next(),
      Some(Ok(Token::BlockStringLiteral(r#""""tes\"""t""""#)))
    );
    assert_eq!(lexer.slice(), r#""""tes\"""t""""#);

    assert_eq!(
      lexer.next(),
      Some(Ok(Token::BlockStringLiteral(r#""""""""#)))
    );
    assert_eq!(lexer.slice(), r#""""""""#);

    assert_eq!(
      lexer.next(),
      Some(Ok(Token::BlockStringLiteral(r#"""""" """"#)))
    );
    assert_eq!(lexer.slice(), r#"""""" """"#);

    assert_eq!(
      lexer.next(),
      Some(Ok(Token::BlockStringLiteral(
        r#""""
                 multi-
                 line
             """"#
      )))
    );
    assert_eq!(
      lexer.slice(),
      r#""""
                 multi-
                 line
             """"#
    );

    // assert_eq!(lexer.next(), Some(Err(())));
    // assert_eq!(
    //   lexer.extras.error_token,
    //   Some(Token::ErrorUnterminatedBlockString)
    // );
    // Unterminated string just consumes the starting quotes
    assert_eq!(lexer.slice(), r#"""""#);
  }

  #[test]
  fn test_bom_lexing() {
    let input = "\u{feff}";

    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), None);
  }
}
