use logosky::{
  Lexable,
  logos::{Lexer, Logos, Source},
  utils::{Lexeme, PositionedChar, Span},
};

use crate::{
  error::{InvalidUnicodeHexDigits, StringError, StringErrors, UnicodeError},
  hints::LineTerminatorHint,
};

use super::{super::SealedWrapper, LitComplexInlineStr, LitInlineStr, LitPlainStr};

#[derive(Logos, Debug)]
#[logos(crate = logosky::logos, extras = usize, error(StringError))]
pub(crate) enum StringToken {
  #[regex(r#"\\["\\/bfnrt]"#)]
  #[regex(r#"\\[^"\\/bfnrtu]"#, handle_invalid_escaped_character)]
  EscapedCharacter,

  #[regex(r#"\\u[0-9A-Fa-f]{4}"#, handle_fixed_width_escape_unicode)]
  #[regex(r#"\\u"#, handle_invalid_escaped_unicode)]
  #[regex(r#"\\u\{[0-9A-Fa-f]{1,6}\}"#, handle_braced_escape_unicode)]
  #[regex(r#"\\u\{\}"#, empty_braced_unicode_escape)]
  #[regex(
    r#"\\u\{[0-9A-Fa-f]{7,}\}"#,
    too_many_hex_digits_in_braced_unicode_escape
  )]
  #[regex(r#"\\u\{[0-9A-Fa-f]{1,6}"#, unclosed_brace_in_braced_unicode_escape)]
  #[regex(r#"\\u\{[^0-9A-Fa-f}]"#, unclosed_brace_in_braced_unicode_escape)]
  #[regex(r#"\\u\{"#, handle_semi_braced_escape_unicode)]
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
fn handle_fixed_width_escape_unicode<'a>(
  lexer: &mut Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
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
      lexer.extras += 4; // surrogate pair is 4 bytes
      return Ok(());
    }

    // Unpaired high surrogate
    return Err(UnicodeError::unpaired_high_surrogate(Lexeme::Span(lexer.span().into())).into());
  }

  if is_low_surrogate(code_point) {
    // Low surrogate without preceding high surrogate is always invalid
    return Err(UnicodeError::unpaired_low_surrogate(Lexeme::Span(lexer.span().into())).into());
  }

  // Single BMP scalar
  lexer.extras += super::utf8_len_for_scalar(code_point);
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
      UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Span(Span::from(
        span.start..span.end + idx,
      )))
      .into(),
    );
  }

  Err(StringError::Unicode(match &chars[..idx] {
    [a, b, c, d] => {
      // we already know they aren't all hex digits
      let span = lexer.span();
      lexer.bump(4);

      let mut invalid_digits = InvalidUnicodeHexDigits::default();

      if !a.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*a, span.end));
      }

      if !b.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*b, span.end + 1));
      }

      if !c.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*c, span.end + 2));
      }

      if !d.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*d, span.end + 3));
      }

      UnicodeError::invalid_fixed_unicode_escape_sequence(invalid_digits, lexer.span().into())
    }
    [a, b, c] => {
      let span = lexer.span();
      lexer.bump(3);

      let mut invalid_digits = InvalidUnicodeHexDigits::default();
      if !a.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*a, span.end));
      }

      if !b.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*b, span.end + 1));
      }

      if !c.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*c, span.end + 2));
      }

      if invalid_digits.is_empty() {
        UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Span(lexer.span().into()))
      } else {
        UnicodeError::invalid_fixed_unicode_escape_sequence(invalid_digits, lexer.span().into())
      }
    }
    [a, b] => {
      let span = lexer.span();
      lexer.bump(2);

      let mut invalid_digits = InvalidUnicodeHexDigits::default();
      if !a.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*a, span.end));
      }

      if !b.is_ascii_hexdigit() {
        invalid_digits.push_fixed(PositionedChar::with_position(*b, span.end + 1));
      }

      if invalid_digits.is_empty() {
        UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Span(lexer.span().into()))
      } else {
        UnicodeError::invalid_fixed_unicode_escape_sequence(invalid_digits, lexer.span().into())
      }
    }
    [a] => {
      let span = lexer.span();
      lexer.bump(1);

      if a.is_ascii_hexdigit() {
        UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Span(lexer.span().into()))
      } else {
        UnicodeError::invalid_fixed_unicode_escape_sequence(
          PositionedChar::with_position(*a, span.end).into(),
          lexer.span().into(),
        )
      }
    }
    [] => UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Span(lexer.span().into())),
    _ => unreachable!("impossible array length"),
  }))
}

#[inline(always)]
fn handle_braced_escape_unicode<'a>(
  lexer: &mut logosky::logos::Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
  // Slice looks like r#"\u{...}"#, guaranteed by the regex.
  let s = lexer.slice();

  // Hex part between the braces. ASCII-only indexing is safe here.
  let hex = &s[3..s.len() - 1];

  // Regex already ensures 1..=6 hex digits, so this can't fail.
  let cp = u32::from_str_radix(hex, 16).expect("regex guarantees hex");

  // Reject non-scalar values.
  match () {
    () if cp > 0x10_FFFF => {
      Err(UnicodeError::overflow_braced_unicode_escape(lexer.span().into(), cp).into())
    }
    () if (0xD800..=0xDFFF).contains(&cp) => {
      Err(UnicodeError::surrogate_braced_unicode_escape(lexer.span().into(), cp).into())
    }
    () => {
      lexer.extras += super::utf8_len_for_scalar(cp);
      Ok(())
    }
  }
}

#[inline(always)]
fn handle_semi_braced_escape_unicode<'a>(
  lexer: &mut logosky::logos::Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
  let remainder = lexer.remainder();

  Err(match remainder.find(['}']) {
    None => UnicodeError::unclosed_braced_unicode_escape(lexer.span().into()).into(),
    Some(pos) => {
      lexer.bump(pos + 1);
      UnicodeError::invalid_braced_unicode_escape_sequence(lexer.span().into()).into()
    }
  })
}

#[inline(always)]
fn empty_braced_unicode_escape<'a>(
  lexer: &mut logosky::logos::Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
  Err(StringError::Unicode(
    UnicodeError::empty_braced_unicode_escape(lexer.span().into()),
  ))
}

#[inline(always)]
fn too_many_hex_digits_in_braced_unicode_escape<'a>(
  lexer: &mut logosky::logos::Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
  let slice = lexer.slice();
  let counts = slice.len() - 4; // subtract \u{}
  Err(StringError::Unicode(
    UnicodeError::too_many_digits_in_braced_unicode_escape(lexer.span().into(), counts),
  ))
}

#[inline(always)]
fn unclosed_brace_in_braced_unicode_escape<'a>(
  lexer: &mut logosky::logos::Lexer<'a, StringToken>,
) -> Result<(), StringError<char>> {
  Err(StringError::Unicode(
    UnicodeError::unclosed_braced_unicode_escape(lexer.span().into()),
  ))
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

impl<'a, S, T> Lexable<&mut SealedWrapper<Lexer<'a, T>>, StringErrors<char>>
  for LitInlineStr<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<str>,
{
  #[inline]
  fn lex(lexer: &mut SealedWrapper<Lexer<'a, T>>) -> Result<Self, StringErrors<char>>
  where
    Self: Sized,
  {
    lex_inline_str_from_str(lexer)
  }
}

#[inline]
pub(crate) fn lex_inline_str_from_str<'a, S, T>(
  lexer: &mut SealedWrapper<Lexer<'a, T>>,
) -> Result<LitInlineStr<S::Slice<'a>>, StringErrors<char>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<str>,
{
  let lexer_span = lexer.span();
  let remainder = lexer.remainder();
  let mut string_lexer = StringToken::lexer(remainder.as_ref());

  let mut errs = StringErrors::default();

  let mut num_escapes = 0;
  let mut num_unicodes = 0;

  while let Some(string_token) = string_lexer.next() {
    match string_token {
      Ok(StringToken::Quote) => {
        lexer.bump(string_lexer.span().end);
        if !errs.is_empty() {
          return Err(errs);
        }

        let src = lexer.slice();
        return Ok(match (num_escapes != 0, num_unicodes != 0) {
          (false, false) => LitPlainStr::new(src).into(),
          _ => LitComplexInlineStr::new(src, string_lexer.extras).into(),
        });
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
      Ok(StringToken::StringCharacters) => {
        // Plain content normalizes byte-for-byte
        string_lexer.extras += string_lexer.slice().len();
      }
      Ok(StringToken::EscapedUnicode) => {
        num_unicodes += 1;
      }
      Ok(StringToken::EscapedCharacter) => {
        num_escapes += 1;
        // \" \\ \/ \b \f \n \r \t  → each becomes exactly 1 UTF-8 byte
        string_lexer.extras += 1;
      }
      Err(mut e) => {
        e.bump(lexer_span.end);
        errs.push(e);
      }
    }
  }

  lexer.bump(string_lexer.span().end);
  errs.push(StringError::unterminated_inline_string());
  Err(errs)
}
