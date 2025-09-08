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
use logosky::utils::{Lexeme, PositionedChar, Span};

use crate::lexer::token::error::{InvalidUnicodeHexDigits, InvalidUnicodeSequence, LineTerminatorHint, StringError, StringErrors, UnicodeError};

use super::{Error, Token};

#[derive(Logos, Debug)]
#[logos(error(StringError<char>))]
#[logos(extras = bool)] // whether collect verbose errors or not.
enum StringToken {
  #[regex(r#"\\["\\/bfnrt]"#)]
  #[regex(r#"\\[^"\\/bfnrtu]"#, handle_invalid_escaped_character)]
  EscapedCharacter,

  #[regex(r#"\\u[0-9A-Fa-f]{4}"#)]
  #[regex(r#"\\u"#, handle_invalid_escaped_unicode)]
  EscapedUnicode,

  #[token("\"")]
  Quote,

  #[regex(r#"\n|\r|\r\n"#, |lexer| {
    let slice = lexer.slice();
    match slice {
      "\n" => LineTerminatorHint::NewLine,
      "\r" => LineTerminatorHint::CarriageReturn,
      "\r\n" => LineTerminatorHint::CarriageReturnNewLine,
      _ => unreachable!("regex matched unexpected line terminator"),
    }
  })]
  LineTerminator(LineTerminatorHint),

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

#[inline(always)]
fn handle_invalid_escaped_unicode<'a>(lexer: &mut Lexer<'a, StringToken>) -> Result<(), StringError<char>> {
  let remainder = lexer.remainder();
  let mut idx = 0;

  let chars = {
    let mut buf = ['\0'; 4];
    for (i, c) in remainder.chars().enumerate().take(4) {
      buf[i] = c;
      idx += 1;
    }
    buf
  };


  if idx != 4 && chars[..idx].iter().all(|c| c.is_ascii_hexdigit()) {
    let span = lexer.span();
    lexer.bump(idx);
    return Err(UnicodeError::Incomplete(Lexeme::Span(Span::from(span.start..span.end + idx))).into());
  }

  Err(StringError::Unicode(match &chars[..idx] {
    [a, b, c, d] => {
      // we already know they aren't all hex digits
      let span = lexer.span();
      lexer.bump(4);

      let mut invalid_digits = InvalidUnicodeHexDigits::default();

      if !a.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*a, span.end));
      }

      if !b.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*b, span.end + 1));
      }

      if !c.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*c, span.end + 2));
      }

      if !d.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*d, span.end + 3));
      }

      InvalidUnicodeSequence::new(invalid_digits, lexer.span().into()).into()
    },
    [a, b, c] => {
      let span = lexer.span();
      lexer.bump(3);

      let mut invalid_digits = InvalidUnicodeHexDigits::default();
      if !a.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*a, span.end));
      }

      if !b.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*b, span.end + 1));
      }

      if !c.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*c, span.end + 2));
      }

      if invalid_digits.is_empty() {
        UnicodeError::Incomplete(Lexeme::Span(lexer.span().into()))
      } else {
        InvalidUnicodeSequence::new(invalid_digits, lexer.span().into()).into()
      }
    },
    [a, b] => {
      let span = lexer.span();
      lexer.bump(2);

      let mut invalid_digits = InvalidUnicodeHexDigits::default();
      if !a.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*a, span.end));
      }

      if !b.is_ascii_hexdigit() {
        invalid_digits.push(PositionedChar::with_position(*b, span.end + 1));
      }

      if invalid_digits.is_empty() {
        UnicodeError::Incomplete(Lexeme::Span(lexer.span().into()))
      } else {
        InvalidUnicodeSequence::new(invalid_digits, lexer.span().into()).into()
      }
    },
    [a] => {
      let span = lexer.span();
      lexer.bump(1);
      
      if a.is_ascii_hexdigit() {  
        UnicodeError::Incomplete(Lexeme::Span(lexer.span().into()))
      } else {
        InvalidUnicodeSequence::new(
          PositionedChar::with_position(*a, span.end).into(),
          lexer.span().into(),
        ).into()
      }
    },
    [] => UnicodeError::Incomplete(Lexeme::Span(lexer.span().into())),
    _ => unreachable!("impossible array length"),
  }))
}

#[inline(always)]
fn handle_invalid_escaped_character<'a>(lexer: &mut Lexer<'a, StringToken>) -> Result<(), StringError<char>> {
  let slice = lexer.slice();
  
  match slice.chars().last() {
    Some(c) => {
      let span = lexer.span();
      let pos = span.start + 1;
      Err(StringError::unexpected_escaped_character(span.into(), c, pos))
    },
    None => unreachable!("regex must match at least two characters"),
  }
}

pub(super) fn lex_string<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Result<&'a str, Error> {
  let remainder = lexer.remainder();
  let mut string_lexer = StringToken::lexer(remainder);

  let mut errs = StringErrors::default();

  // TODO: Maybe track the properties of the string at parsing time,
  // so we only need to unescape/split when absolutely required...
  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Ok(StringToken::Quote) => {
        lexer.bump(string_lexer.span().end);
        if !errs.is_empty() {
          return Err(Error::new(lexer.span(), errs.into()));
        }
        return Ok(lexer.slice());
      }
      Ok(StringToken::LineTerminator(lt)) => {
        let span = lexer.span();
        lexer.bump(string_lexer.span().end);
        
        let pos = span.end + string_lexer.span().start;
        let err = match lt {
          LineTerminatorHint::NewLine => {
            StringError::unexpected_new_line('\n', pos)
          },
          LineTerminatorHint::CarriageReturn => {
            StringError::unexpected_carriage_return('\r', pos)
          },
          LineTerminatorHint::CarriageReturnNewLine => {
            StringError::unexpected_carriage_return_new_line((pos..pos + 2).into())
          }
        };
        errs.push(err);
      }
      Ok(
        StringToken::EscapedCharacter | StringToken::EscapedUnicode | StringToken::StringCharacters,
      ) => {}
      Err(e) => {
        errs.push(e);
      }
    }
  }

  lexer.bump(string_lexer.span().end);
  Err(Error::new(
    lexer.span(),
    StringErrors::from(StringError::unterminated_inline_string()).into()
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
    StringErrors::from(StringError::unterminated_block_string()).into()
  ))
}

