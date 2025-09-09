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

#[derive(Default, Eq, PartialEq)]
pub struct TokenExtras {
  /// Token callbacks might store an error token kind in here before failing.
  /// This is then picked up in the parser to turn the `Error` token into a
  /// more specific variant.
  error_token: Option<Token<'static>>,
}

/// Lexer for the GraphQL specification: http://spec.graphql.org/
#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(extras = TokenExtras)]
pub enum Token<'a> {
  ErrorUnterminatedString,
  ErrorUnsupportedStringCharacter,
  ErrorUnterminatedBlockString,

  /// Spec: [Byte Order Mark](https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens.Byte-Order-Mark)
  #[token("\u{FEFF}")]
  UnicodeBOM,

  /// Spec: [LineTerminator](https://spec.graphql.org/draft/#LineTerminator).
  #[regex(r"\r\n|\r|\n")]
  LineTerminator,

  /// Spec: [WhiteSpace](https://spec.graphql.org/draft/#WhiteSpace)
  #[regex(r"[ \t]")]
  Whitespace,

  /// Spec: [Comma](https://spec.graphql.org/draft/#Comma)
  #[token(",")]
  Comma,

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

  // IntegerPart:    -?(0|[1-9][0-9]*)
  // FractionalPart: \\.[0-9]+
  // ExponentPart:   [eE][+-]?[0-9]+
  #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lex| lex.slice())]
  FloatLiteral(&'a str),

  #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
  Identifier(&'a str),

  #[regex("-?(0|[1-9][0-9]*)", |lex| lex.slice())]
  IntegerLiteral(&'a str),

  #[regex("-?0[0-9]+(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)?")]
  ErrorNumberLiteralLeadingZero,

  #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)?[.a-zA-Z_]")]
  ErrorNumberLiteralTrailingInvalid,

  #[regex("-?(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+)")]
  ErrorFloatLiteralMissingZero,

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

  #[token("\"", lex_string)]
  StringLiteral(&'a str),

  #[token("\"\"\"", lex_block_string)]
  BlockStringLiteral(&'a str),
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

fn lex_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
  let remainder = lexer.remainder();
  let mut string_lexer = StringToken::lexer(remainder);
  // TODO: Maybe track the properties of the string at parsing time,
  // so we only need to unescape/split when absolutely required...
  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Ok(StringToken::Quote) => {
        lexer.bump(string_lexer.span().end);
        return Some(lexer.slice());
      }
      Ok(StringToken::LineTerminator) => {
        lexer.bump(string_lexer.span().start);
        lexer.extras.error_token = Some(Token::ErrorUnterminatedString);
        return None;
      }
      Ok(
        StringToken::EscapedCharacter | StringToken::EscapedUnicode | StringToken::StringCharacters,
      ) => {}
      Err(_) => {
        lexer.extras.error_token = Some(Token::ErrorUnsupportedStringCharacter);
        return None;
      }
    }
  }
  lexer.extras.error_token = Some(Token::ErrorUnterminatedString);
  None
}

fn lex_block_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
  let remainder = lexer.remainder();
  let mut string_lexer = BlockStringToken::lexer(remainder);
  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Ok(BlockStringToken::TripleQuote) => {
        lexer.bump(string_lexer.span().end);
        return Some(lexer.slice());
      }
      Ok(BlockStringToken::EscapedTripleQuote | BlockStringToken::Other) => {}
      Err(_) => unreachable!(),
    }
  }
  lexer.extras.error_token = Some(Token::ErrorUnterminatedBlockString);
  None
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
      Token::FloatLiteral(_) => "floating point value (e.g. '3.14')",
      Token::Identifier(_) => "non-variable identifier (e.g. 'x' or 'Foo')",
      Token::IntegerLiteral(_) => "integer value (e.g. '0' or '42')",
      Token::BraceOpen => "open brace ('{')",
      Token::BracketOpen => "open bracket ('[')",
      Token::ParenOpen => "open parenthesis ('(')",
      Token::Pipe => "pipe ('|')",
      Token::Spread => "spread ('...')",
      Token::BlockStringLiteral(_) => "block string (e.g. '\"\"\"hi\"\"\"')",
      Token::ErrorFloatLiteralMissingZero => "unsupported number (int or float) literal",
      Token::ErrorNumberLiteralLeadingZero => "unsupported number (int or float) literal",
      Token::ErrorNumberLiteralTrailingInvalid => "unsupported number (int or float) literal",
      Token::StringLiteral(_) => "string literal (e.g. '\"...\"')",
      Token::ErrorUnterminatedString => "unterminated string",
      Token::ErrorUnsupportedStringCharacter => "unsupported character in string",
      Token::ErrorUnterminatedBlockString => "unterminated block string",
      _ => "a",
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
    assert_eq!(
      lexer.next(),
      Some(Err(())),
      "Testing lexing fails for string '{source}'"
    );
    assert_eq!(
      lexer.span(),
      0..length,
      "Testing the lexing of string '{source}'"
    );
  }

  #[test]
  fn t() {
    let mut lexer = Token::lexer("4 213");
    lexer.bump(2);
    let val = lexer.next().unwrap().unwrap();
    assert_eq!(val, Token::IntegerLiteral("213"));
    println!("{:?}", lexer.span());
  }

  #[test]
  fn test_number_successes() {
    assert_token("4", Token::IntegerLiteral("4"), 1);
    assert_token("4.123", Token::FloatLiteral("4.123"), 5);
    assert_token("-4", Token::IntegerLiteral("-4"), 2);
    assert_token("9", Token::IntegerLiteral("9"), 1);
    assert_token("0", Token::IntegerLiteral("0"), 1);
    assert_token("-4.123", Token::FloatLiteral("-4.123"), 6);
    assert_token("0.123", Token::FloatLiteral("0.123"), 5);
    assert_token("123e4", Token::FloatLiteral("123e4"), 5);
    assert_token("123E4", Token::FloatLiteral("123E4"), 5);
    assert_token("123e-4", Token::FloatLiteral("123e-4"), 6);
    assert_token("123e+4", Token::FloatLiteral("123e+4"), 6);
    assert_token("-1.123e4", Token::FloatLiteral("-1.123e4"), 8);
    assert_token("-1.123E4", Token::FloatLiteral("-1.123E4"), 8);
    assert_token("-1.123e-4", Token::FloatLiteral("-1.123e-4"), 9);
    assert_token("-1.123e+4", Token::FloatLiteral("-1.123e+4"), 9);
    assert_token("-1.123e4567", Token::FloatLiteral("-1.123e4567"), 11);
    assert_token("-0", Token::IntegerLiteral("-0"), 2);
  }

  #[test]
  fn test_a() {
    let mut lexer = Token::lexer("1e-");
    lexer.next().unwrap().unwrap_err();
  }

  #[test]
  fn test_number_failures() {
    let mut lexer = Token::lexer("1e-");
    lexer.next().unwrap().unwrap();

    assert_token("00", Token::ErrorNumberLiteralLeadingZero, 2);
    assert_token("01", Token::ErrorNumberLiteralLeadingZero, 2);
    assert_token("-01", Token::ErrorNumberLiteralLeadingZero, 3);
    assert_error("+1", 1);
    assert_token("01.23", Token::ErrorNumberLiteralLeadingZero, 5);
    assert_token("1.", Token::ErrorNumberLiteralTrailingInvalid, 2);
    assert_token("1e", Token::ErrorNumberLiteralTrailingInvalid, 2);
    assert_token("1.e1", Token::ErrorNumberLiteralTrailingInvalid, 2);
    assert_token("1.A", Token::ErrorNumberLiteralTrailingInvalid, 2);
    assert_error("-A", 1);
    assert_token("1.0e", Token::ErrorNumberLiteralTrailingInvalid, 4);
    assert_token("1.0eA", Token::ErrorNumberLiteralTrailingInvalid, 4);
    assert_token("1.2e3e", Token::ErrorNumberLiteralTrailingInvalid, 6);
    assert_token("1.2e3.4", Token::ErrorNumberLiteralTrailingInvalid, 6);
    assert_token("1.23.4", Token::ErrorNumberLiteralTrailingInvalid, 5);
    assert_token(".123", Token::ErrorFloatLiteralMissingZero, 4);

    // check that we don't consume trailing valid items
    assert_token("1.23.{}", Token::ErrorNumberLiteralTrailingInvalid, 5);
    assert_token("1.23. {}", Token::ErrorNumberLiteralTrailingInvalid, 5);
    assert_token("1.23. []", Token::ErrorNumberLiteralTrailingInvalid, 5);
    assert_token("1.23. foo", Token::ErrorNumberLiteralTrailingInvalid, 5);
    assert_token("1.23. $foo", Token::ErrorNumberLiteralTrailingInvalid, 5);
  }

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

    assert_eq!(lexer.next(), Some(Err(())));
    assert_eq!(
      lexer.extras.error_token,
      Some(Token::ErrorUnterminatedString)
    );
    assert_eq!(lexer.slice(), "\"unterminated");
  }

  #[test]
  fn test_invalid_character_lexing() {
    let input = r#"
             {
                 %%%
                 __typename
                 *
             }
         "#;
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::BraceOpen)));
    assert_eq!(lexer.slice(), "{");

    assert_eq!(lexer.next(), Some(Err(())));
    assert_eq!(lexer.slice(), "%");

    assert_eq!(lexer.next(), Some(Err(())));
    assert_eq!(lexer.slice(), "%");

    assert_eq!(lexer.next(), Some(Err(())));
    assert_eq!(lexer.slice(), "%");

    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("__typename"))));
    assert_eq!(lexer.slice(), "__typename");

    assert_eq!(lexer.next(), Some(Err(())));
    assert_eq!(lexer.slice(), "*");

    assert_eq!(lexer.next(), Some(Ok(Token::BraceClose)));
    assert_eq!(lexer.slice(), "}");

    assert_eq!(lexer.next(), None);
  }

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

    assert_eq!(lexer.next(), Some(Err(())));
    assert_eq!(
      lexer.extras.error_token,
      Some(Token::ErrorUnterminatedBlockString)
    );
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
