use tokora::{
  SimpleSpan,
  lexer::Lexable,
  logos::{Lexer, Logos, Source},
  utils::{Lexeme, PositionedChar},
};

type Span = SimpleSpan;

use crate::{
  error::{InvalidUnicodeHexDigits, StringError, StringErrors, UnicodeError},
  hints::LineTerminatorHint,
};

use super::{super::SealedWrapper, LitComplexInlineStr, LitInlineStr, LitPlainInlineStr};

#[derive(Logos, Debug)]
#[logos(crate = tokora::logos, extras = usize, error(StringError, unmatched))]
#[allow(
  clippy::duplicated_attributes,
  reason = "each `#[regex]` is one logos rule, and three rules of `EscapedUnicode` share a callback"
)]
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
  // `\u{` without its closing brace, whose end the callback finds. The one-character class leaves
  // out every delimiter that search stops at, so before one the bare `\u{` rule takes the escape
  // and the delimiter keeps its own rule.
  #[regex(r#"\\u\{[0-9A-Fa-f]{1,6}"#, handle_partial_braced_escape_unicode)]
  #[regex(r#"\\u\{[^0-9A-Fa-f}"\\\r\n]"#, handle_partial_braced_escape_unicode)]
  #[regex(r#"\\u\{"#, handle_partial_braced_escape_unicode)]
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

/// The error for input no rule of [`StringToken`] matches.
///
/// Every scalar other than `"`, `\`, CR and LF is
/// [`StringCharacters`](StringToken::StringCharacters), those four have rules
/// of their own, and `\\[^"\\/bfnrtu]` takes a backslash before any scalar. So
/// the one shape that reaches this is a lone `\` at the end of input, and it is
/// [`StringError::IncompleteEscape`] at the backslash's byte — what the byte
/// twin of this grammar answers for the same input.
///
/// Anything else reaching here would be a grammar change, and answers the
/// default rather than a position this has not established.
#[inline]
fn unmatched(lexer: &mut Lexer<'_, StringToken>) -> StringError<char> {
  if lexer.slice().starts_with('\\') {
    let start = lexer.span().start;
    StringError::IncompleteEscape(Span::new(start, start + 1))
  } else {
    StringError::default()
  }
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
fn handle_fixed_width_escape_unicode(
  lexer: &mut Lexer<'_, StringToken>,
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
    return Err(UnicodeError::unpaired_high_surrogate(Lexeme::Range(lexer.span().into())).into());
  }

  if is_low_surrogate(code_point) {
    // Low surrogate without preceding high surrogate is always invalid
    return Err(UnicodeError::unpaired_low_surrogate(Lexeme::Range(lexer.span().into())).into());
  }

  // Single BMP scalar
  lexer.extras += super::utf8_len_for_scalar(code_point);
  Ok(())
}

/// The callback of the bare `\u` rule: a fixed-width escape whose four hex digits are not all
/// there.
///
/// It takes up to four characters after the `\u` into the escape's token, as its digits. The token
/// is measured in bytes and each character is taken whole, so a digit spelled in more than one
/// byte moves the token's end by its whole width, and each digit that is not hex is reported at
/// its own byte. Counting characters as bytes would end the token inside a character, which the
/// sub-lexer refuses with a panic.
#[inline]
fn handle_invalid_escaped_unicode(
  lexer: &mut Lexer<'_, StringToken>,
) -> Result<(), StringError<char>> {
  let span = lexer.span();

  // Each character taken, with its byte offset after the `\u`.
  let mut buf = [('\0', 0usize); 4];
  let mut count = 0usize;
  for (index, (offset, c)) in lexer.remainder().char_indices().take(4).enumerate() {
    buf[index] = (c, offset);
    count = index + 1;
  }
  let taken = &buf[..count];
  let len = taken
    .last()
    .map_or(0, |&(c, offset): &(char, usize)| offset + c.len_utf8());
  lexer.bump(len);

  // Four hex digits are the fixed-width rule's, so digits that are all hex here are too few.
  if taken
    .iter()
    .all(|&(c, _): &(char, usize)| c.is_ascii_hexdigit())
  {
    return Err(
      UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Range(lexer.span().into())).into(),
    );
  }

  let mut invalid_digits = InvalidUnicodeHexDigits::default();
  for &(c, offset) in taken {
    if !c.is_ascii_hexdigit() {
      invalid_digits.push_fixed(PositionedChar::with_position(c, span.end + offset));
    }
  }
  Err(
    UnicodeError::invalid_fixed_unicode_escape_sequence(invalid_digits, lexer.span().into()).into(),
  )
}

#[inline(always)]
fn handle_braced_escape_unicode(
  lexer: &mut tokora::logos::Lexer<'_, StringToken>,
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

/// The callback of the three partial-brace rules.
///
/// A `}` before any delimiter closes the escape over content that is not one to six hex digits,
/// so the escape is an invalid sequence through the brace. Otherwise it is unclosed over what the
/// rule matched, and the sub-lexer goes on at the delimiter, or at the end of the input.
#[inline(always)]
fn handle_partial_braced_escape_unicode(
  lexer: &mut tokora::logos::Lexer<'_, StringToken>,
) -> Result<(), StringError<char>> {
  let close = super::braced_escape_close(lexer.remainder().as_bytes());
  Err(StringError::Unicode(match close {
    Some(at) => {
      lexer.bump(at + 1);
      UnicodeError::invalid_braced_unicode_escape_sequence(lexer.span().into())
    }
    None => UnicodeError::unclosed_braced_unicode_escape(lexer.span().into()),
  }))
}

#[inline(always)]
fn empty_braced_unicode_escape(
  lexer: &mut tokora::logos::Lexer<'_, StringToken>,
) -> Result<(), StringError<char>> {
  Err(StringError::Unicode(
    UnicodeError::empty_braced_unicode_escape(lexer.span().into()),
  ))
}

#[inline(always)]
fn too_many_hex_digits_in_braced_unicode_escape(
  lexer: &mut tokora::logos::Lexer<'_, StringToken>,
) -> Result<(), StringError<char>> {
  let slice = lexer.slice();
  let counts = slice.len() - 4; // subtract \u{}
  Err(StringError::Unicode(
    UnicodeError::too_many_digits_in_braced_unicode_escape(lexer.span().into(), counts),
  ))
}

#[inline(always)]
fn handle_invalid_escaped_character(
  lexer: &mut Lexer<'_, StringToken>,
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
          (false, false) => LitPlainInlineStr::new(src).into(),
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
