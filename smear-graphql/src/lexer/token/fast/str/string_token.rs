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

use logos::{Lexer, Logos};

use crate::lexer::token::error::{LineTerminator, StringError};

use super::{Error, Token};

#[derive(Logos, Debug)]
enum StringToken {
  #[regex(r#"\\["\\/bfnrt]"#)]
  EscapedCharacter,

  #[regex(r#"\\u[0-9A-Fa-f]{4}"#)]
  EscapedUnicode,

  #[token("\"")]
  Quote,

  #[regex(r#"\n|\r|\r\n"#, |lexer| {
    let slice = lexer.slice();
    match slice {
      "\n" => LineTerminator::NewLine,
      "\r" => LineTerminator::CarriageReturn,
      "\r\n" => LineTerminator::CarriageReturnNewLine,
      _ => unreachable!("regex matched unexpected line terminator"),
    }
  })]
  LineTerminator(LineTerminator),

  /// Valid Unicode scalar values excluding surrogates and controls
  #[regex(r#"[\u0009\u0020-\u0021\u0023-\u005B\u005D-\uD7FF\uE000-\uFFFF\u{10000}-\u{10FFFF}]+"#)]
  StringCharacters,
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum BlockStringToken {
  /// \\"\\"\\" inside block string
  #[token("\\\"\"\"")]
  EscapedTripleQuote,
  /// terminator
  #[token("\"\"\"")]
  TripleQuote,
  /// Any run without a double-quote and excluding disallowed C0 controls.
  /// Newlines are fine in block strings; normalization is done later.
  #[regex(r#"[^"\x00-\x08\x0B\x0C\x0E-\x1F]+"#)]
  Other,
}

pub(super) fn lex_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
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
      Ok(StringToken::LineTerminator(lt)) => {
        lexer.bump(string_lexer.span().start);

        let span = lexer.span();
        let err = match lt {
          LineTerminator::NewLine => {
            StringError::unexpected_new_line('\n', span.end)
          },
          LineTerminator::CarriageReturn => {
            StringError::unexpected_carriage_return('\r', span.end)
          },
          LineTerminator::CarriageReturnNewLine => {
            StringError::unexpected_carriage_return_new_line((span.end..span.end + 2).into())
          }
        };
        return Err(Error::new(
          lexer.span(),
          err.into(),
        ));
      }
      Ok(
        StringToken::EscapedCharacter | StringToken::EscapedUnicode | StringToken::StringCharacters,
      ) => {}
      Err(_) => {
        return Err(Error::new(
          lexer.span(),
          StringError::UnsupportedStringCharacter.into(),
        ));
      }
    }
  }

  Err(Error::new(
    lexer.span(),
    StringError::unterminated_string().into(),
  ))
}

pub(super) fn lex_block_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
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

  Err(Error::new(
    lexer.span(),
    StringError::unterminated_block_string().into(),
  ))
}

