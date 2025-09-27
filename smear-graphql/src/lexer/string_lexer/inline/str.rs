use logosky::{
  Lexable,
  logos::{Lexer, Logos, Source},
  utils::{Lexeme, PositionedChar, Span},
};

use crate::error::{
  InvalidUnicodeHexDigits, InvalidUnicodeSequence, LexerError, LineTerminatorHint, StringError,
  StringErrors, UnicodeError,
};

use super::{super::SealedLexer, InlineString};

#[derive(Logos, Debug)]
#[logos(crate = logosky::logos, error(StringError))]
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

impl<'a, S, T, StateError> Lexable<SealedLexer<'_, 'a, T>, LexerError<char, StateError>>
  for InlineString<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<str>,
{
  #[inline]
  fn lex(mut lexer: SealedLexer<'_, 'a, T>) -> Result<Self, LexerError<char, StateError>>
  where
    Self: Sized,
  {
    let lexer_span = lexer.span();
    let remainder = lexer.remainder();
    let mut string_lexer = StringToken::lexer(remainder.as_ref());

    let mut errs = StringErrors::default();

    let mut has_unicode = false;
    let mut has_escape = false;

    while let Some(string_token) = string_lexer.next() {
      match string_token {
        Ok(StringToken::Quote) => {
          lexer.bump(string_lexer.span().end);
          if !errs.is_empty() {
            return Err(LexerError::new(lexer.span(), errs.into()));
          }

          let src = lexer.slice();
          return Ok(match (has_escape, has_unicode) {
            (false, false) => Self::Clean(src),
            (true, false) => Self::SimpleEscape(src),
            (false, true) => Self::UnicodeEscape(src),
            (true, true) => Self::MixedEscape(src),
          });
        }
        Ok(StringToken::LineTerminator(lt)) => {
          let pos = lexer_span.end + string_lexer.span().start;
          let err = match lt {
            LineTerminatorHint::NewLine => StringError::unexpected_new_line('\n', pos),
            LineTerminatorHint::CarriageReturn => {
              StringError::unexpected_carriage_return('\r', pos)
            }
            LineTerminatorHint::CarriageReturnNewLine => {
              StringError::unexpected_carriage_return_new_line((pos..pos + 2).into())
            }
          };
          errs.push(err);
        }
        Ok(StringToken::StringCharacters) => {}
        Ok(StringToken::EscapedUnicode) => {
          has_unicode = true;
        }
        Ok(StringToken::EscapedCharacter) => {
          has_escape = true;
        }
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
}
