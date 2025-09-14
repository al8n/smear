/*
* The code in this file is modified based on
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

use crate::error::{
  InvalidUnicodeHexDigits, InvalidUnicodeSequence, LexerError, LineTerminatorHint, StringError,
  StringErrors, UnicodeError,
};

#[derive(Logos, Debug)]
#[logos(error(StringError))]
enum StringToken {
  #[regex(r#"\\["\\/bfnrt]"#)]
  #[regex(r#"\\[^"\\/bfnrtu]"#, handle_invalid_escaped_character)]
  EscapedCharacter,

  #[regex(r#"\\u[0-9A-Fa-f]{4}"#, handle_unicode_escape)]
  #[regex(r#"\\u"#, handle_invalid_escaped_unicode)]
  EscapedUnicode,

  #[token("\"")]
  Quote,

  #[regex(r#"\r\n|\n|\r"#, |lexer| {
    match lexer.slice() {
      "\r\n" => LineTerminatorHint::CarriageReturnNewLine,
      "\n"   => LineTerminatorHint::NewLine,
      "\r"   => LineTerminatorHint::CarriageReturn,
      _ => unreachable!("regex matched unexpected line terminator"),
    }
  })]
  LineTerminator(LineTerminatorHint),

  /// Any run of allowed string characters:
  /// all Unicode scalars except: quote, backslash, and line terminators.
  #[regex(r#"[^"\\\r\n]+"#)]
  StringCharacters,
}

#[inline(always)]
const fn is_high_surrogate(code_point: u32) -> bool {
  matches!(code_point, 0xD800..=0xDBFF)
}

#[inline(always)]
const fn is_low_surrogate(code_point: u32) -> bool {
  matches!(code_point, 0xDC00..=0xDFFF)
}

#[inline(always)]
fn try_parse_next_unicode_escape(remainder: &str) -> Option<u32> {
  match (remainder.len() >= 6, remainder.strip_prefix("\\u")) {
    (true, Some(src)) => {
      if !src.is_char_boundary(4) {
        return None;
      }
      u32::from_str_radix(&src[..4], 16).ok()
    }
    _ => None,
  }
}

#[inline(always)]
fn handle_unicode_escape<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), StringError<char>>
where
  T: Logos<'a, Source = str>,
{
  let slice = lexer.slice();
  let hex_part = &slice[2..];
  let code_point = u32::from_str_radix(hex_part, 16).unwrap(); // Safe due to regex

  if is_high_surrogate(code_point) {
    // Look ahead for low surrogate
    let remainder = lexer.remainder();
    if let Some(low_cp) = try_parse_next_unicode_escape(remainder)
      && is_low_surrogate(low_cp)
    {
      // Valid surrogate pair
      lexer.bump(6); // consume the low surrogate
      return Ok(());
    }

    // Unpaired high surrogate
    return Err(UnicodeError::unpaired_high_surrogate(Lexeme::Span(lexer.span().into())).into());
  }

  if is_low_surrogate(code_point) {
    // Low surrogate without preceding high surrogate is always invalid
    return Err(UnicodeError::unpaired_low_surrogate(Lexeme::Span(lexer.span().into())).into());
  }

  Ok(())
}

#[inline]
fn handle_invalid_escaped_unicode<'a>(
  lexer: &mut Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
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
    return Err(
      UnicodeError::Incomplete(Lexeme::Span(Span::from(span.start..span.end + idx))).into(),
    );
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
    }
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
    }
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
    }
    [a] => {
      let span = lexer.span();
      lexer.bump(1);

      if a.is_ascii_hexdigit() {
        UnicodeError::Incomplete(Lexeme::Span(lexer.span().into()))
      } else {
        InvalidUnicodeSequence::new(
          PositionedChar::with_position(*a, span.end).into(),
          lexer.span().into(),
        )
        .into()
      }
    }
    [] => UnicodeError::Incomplete(Lexeme::Span(lexer.span().into())),
    _ => unreachable!("impossible array length"),
  }))
}

#[inline(always)]
fn handle_invalid_escaped_character<'a>(
  lexer: &mut Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
  let slice = lexer.slice();

  match slice.chars().last() {
    Some(c) => {
      let span = lexer.span();
      let pos = span.start + 1;
      Err(StringError::unexpected_escaped_character(
        span.into(),
        c,
        pos,
      ))
    }
    None => unreachable!("regex must match at least two characters"),
  }
}

pub(super) fn lex_inline_string<'a, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<&'a str, LexerError<char, Extras>>
where
  T: Logos<'a, Source = str>,
{
  let lexer_span = lexer.span();
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
          return Err(LexerError::new(lexer.span(), errs.into()));
        }
        return Ok(lexer.slice());
      }
      Ok(StringToken::LineTerminator(lt)) => {
        let pos = lexer_span.end + string_lexer.span().start;
        let err = match lt {
          LineTerminatorHint::NewLine => StringError::unexpected_new_line('\n', pos),
          LineTerminatorHint::CarriageReturn => StringError::unexpected_carriage_return('\r', pos),
          LineTerminatorHint::CarriageReturnNewLine => {
            StringError::unexpected_carriage_return_new_line((pos..pos + 2).into())
          }
        };
        errs.push(err);
      }
      Ok(
        StringToken::EscapedCharacter | StringToken::EscapedUnicode | StringToken::StringCharacters,
      ) => {}
      Err(mut e) => {
        e.bump(lexer_span.end);
        errs.push(e);
      }
    }
  }

  lexer.bump(string_lexer.span().end);
  errs.push(StringError::unterminated_inline_string());
  Err(LexerError::new(lexer.span(), errs.into()))
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(StringError<char>))]
enum BlockStringToken {
  /// \\"\\"\\" inside block string
  #[token("\\\"\"\"")]
  EscapedTripleQuote,
  /// terminator
  #[token("\"\"\"")]
  TripleQuote,
  /// Runs of any characters except the double quote; includes newlines and C0 controls.
  #[regex(r#"[^"]+"#)]
  Continue,
  /// A single quote that is **not** part of `"""` (content)
  #[token("\"")]
  Quote,
}

pub(super) fn lex_block_string<'a, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<&'a str, LexerError<char, Extras>>
where
  T: Logos<'a, Source = str>,
{
  let remainder = lexer.remainder();
  let lexer_span = lexer.span();
  let mut string_lexer = BlockStringToken::lexer(remainder);
  let mut errs = StringErrors::default();
  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Ok(BlockStringToken::TripleQuote) => {
        lexer.bump(string_lexer.span().end);
        return Ok(lexer.slice());
      }
      Ok(
        BlockStringToken::EscapedTripleQuote | BlockStringToken::Continue | BlockStringToken::Quote,
      ) => {}
      Err(mut e) => {
        e.bump(lexer_span.end);
        errs.push(e);
      }
    }
  }

  lexer.bump(string_lexer.span().end);
  errs.push(StringError::unterminated_block_string());
  Err(LexerError::new(lexer.span(), errs.into()))
}
