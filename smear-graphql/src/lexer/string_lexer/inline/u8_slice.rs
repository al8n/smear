use logosky::{
  Lexable, Source,
  logos::{Lexer, Logos},
  utils::{Lexeme, PositionedChar, Span},
};

use crate::error::{
  InvalidUnicodeHexDigits, InvalidUnicodeSequence, LexerError, LineTerminatorHint, StringError,
  StringErrors, UnicodeError,
};

use super::{super::SealedLexer, InlineString};

#[derive(Logos, Debug)]
#[logos(crate = logosky::logos, source = [u8], error(StringError<u8>))]
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
      b"\r\n" => LineTerminatorHint::CarriageReturnNewLine,
      b"\n"   => LineTerminatorHint::NewLine,
      b"\r"   => LineTerminatorHint::CarriageReturn,
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
fn hex_val(b: u8) -> Option<u32> {
  match b {
    b'0'..=b'9' => Some((b - b'0') as u32),
    b'a'..=b'f' => Some((b - b'a' + 10) as u32),
    b'A'..=b'F' => Some((b - b'A' + 10) as u32),
    _ => None,
  }
}

#[inline(always)]
fn parse_u4(hex: &[u8]) -> Option<u32> {
  debug_assert!(hex.len() == 4);
  Some(
    (hex_val(hex[0])? << 12)
      | (hex_val(hex[1])? << 8)
      | (hex_val(hex[2])? << 4)
      | (hex_val(hex[3])?),
  )
}

#[inline(always)]
fn try_parse_next_unicode_escape(remainder: &[u8]) -> Option<u32> {
  if remainder.len() < 6 {
    return None;
  }
  if remainder[0] != b'\\' || remainder[1] != b'u' {
    return None;
  }
  parse_u4(&remainder[2..6])
}

#[inline(always)]
fn handle_unicode_escape<'a>(lexer: &mut Lexer<'a, StringToken>) -> Result<(), StringError<u8>> {
  let slice = lexer.slice(); // b"\\uXXXX"
  debug_assert!(slice.len() == 6);
  let code_point = parse_u4(&slice[2..6]).unwrap(); // safe by regex

  if is_high_surrogate(code_point) {
    // look ahead for low surrogate
    let remainder = lexer.remainder();
    if let Some(low_cp) = try_parse_next_unicode_escape(remainder)
      && is_low_surrogate(low_cp)
    {
      lexer.bump(6); // consume the trailing \uXXXX
      return Ok(());
    }
    // unpaired high surrogate
    return Err(UnicodeError::unpaired_high_surrogate(Lexeme::Span(lexer.span().into())).into());
  }

  if is_low_surrogate(code_point) {
    // low surrogate without preceding high surrogate is invalid
    return Err(UnicodeError::unpaired_low_surrogate(Lexeme::Span(lexer.span().into())).into());
  }

  Ok(())
}

#[inline]
fn handle_invalid_escaped_unicode<'a>(
  lexer: &mut Lexer<'a, StringToken>,
) -> Result<(), StringError<u8>> {
  // We are at b"\\u" with no/partial hex after.
  let remainder = lexer.remainder();

  // Gather up to 4 following bytes (not decoding UTF-8; we only need ASCII-hex)
  let take = remainder.len().min(4);
  let mut buf = [0u8; 4];
  buf[..take].copy_from_slice(&remainder[..take]);

  // Case 1: some hex provided but fewer than 4 and all seen are hex => Incomplete
  if take != 4 && buf[..take].iter().all(|b| hex_val(*b).is_some()) {
    let span = lexer.span();
    lexer.bump(take); // consume what was there to make progress
    return Err(
      UnicodeError::Incomplete(Lexeme::Span(Span::from(span.start..span.end + take))).into(),
    );
  }

  // Case 2: we have 4 bytes, but one or more are non-hex => collect precise invalids
  let mk_pos_char = |b: u8, pos: usize| PositionedChar::with_position(b, pos);

  let err = match &buf[..take] {
    [a, b, c, d] => {
      let span = lexer.span();
      lexer.bump(4);
      let mut invalid = InvalidUnicodeHexDigits::default();
      if hex_val(*a).is_none() {
        invalid.push(mk_pos_char(*a, span.end));
      }
      if hex_val(*b).is_none() {
        invalid.push(mk_pos_char(*b, span.end + 1));
      }
      if hex_val(*c).is_none() {
        invalid.push(mk_pos_char(*c, span.end + 2));
      }
      if hex_val(*d).is_none() {
        invalid.push(mk_pos_char(*d, span.end + 3));
      }
      InvalidUnicodeSequence::new(invalid, lexer.span().into()).into()
    }
    [a, b, c] => {
      let span = lexer.span();
      lexer.bump(3);
      let mut invalid = InvalidUnicodeHexDigits::default();
      if hex_val(*a).is_none() {
        invalid.push(mk_pos_char(*a, span.end));
      }
      if hex_val(*b).is_none() {
        invalid.push(mk_pos_char(*b, span.end + 1));
      }
      if hex_val(*c).is_none() {
        invalid.push(mk_pos_char(*c, span.end + 2));
      }
      if invalid.is_empty() {
        UnicodeError::Incomplete(Lexeme::Span(lexer.span().into()))
      } else {
        InvalidUnicodeSequence::new(invalid, lexer.span().into()).into()
      }
    }
    [a, b] => {
      let span = lexer.span();
      lexer.bump(2);
      let mut invalid = InvalidUnicodeHexDigits::default();
      if hex_val(*a).is_none() {
        invalid.push(mk_pos_char(*a, span.end));
      }
      if hex_val(*b).is_none() {
        invalid.push(mk_pos_char(*b, span.end + 1));
      }
      if invalid.is_empty() {
        UnicodeError::Incomplete(Lexeme::Span(lexer.span().into()))
      } else {
        InvalidUnicodeSequence::new(invalid, lexer.span().into()).into()
      }
    }
    [a] => {
      let span = lexer.span();
      lexer.bump(1);
      if hex_val(*a).is_some() {
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
    _ => unreachable!("we only copied up to 4 bytes"),
  };

  Err(StringError::Unicode(err))
}

#[inline(always)]
fn handle_invalid_escaped_character<'a>(
  lexer: &mut Lexer<'a, StringToken>,
) -> Result<(), StringError<u8>> {
  let slice = lexer.slice(); // b'\' <bad>'
  // last byte must exist due to regex
  let bad = *slice.last().expect("regex matched at least two bytes");
  let span = lexer.span();
  let pos = span.start + 1;
  Err(StringError::unexpected_escaped_character(
    span.into(),
    bad,
    pos,
  ))
}

impl<'a, S, T, StateError> Lexable<SealedLexer<'_, 'a, T>, LexerError<u8, StateError>>
  for InlineString<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + 'a,
  S::Slice<'a>: AsRef<[u8]>,
{
  #[inline]
  fn lex(mut lexer: SealedLexer<'_, 'a, T>) -> Result<Self, LexerError<u8, StateError>>
  where
    Self: Sized,
  {
    let base_span = lexer.span();
    let remainder = lexer.remainder();
    let mut string_lexer = StringToken::lexer(remainder.as_ref());

    let mut errs = StringErrors::default();
    let mut has_unicode = false;
    let mut has_escape = false;

    while let Some(tok) = string_lexer.next() {
      match tok {
        Ok(StringToken::Quote) => {
          // Consume closing quote in the outer lexer by the inner span end
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
          let pos = base_span.end + string_lexer.span().start;
          let err = match lt {
            LineTerminatorHint::NewLine => StringError::unexpected_new_line(b'\n', pos),
            LineTerminatorHint::CarriageReturn => {
              StringError::unexpected_carriage_return(b'\r', pos)
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
          // Make inner-byte-lexer relative errors absolute for the outer source
          e.bump(base_span.end);
          errs.push(e);
        }
      }
    }

    // No closing quote
    lexer.bump(string_lexer.span().end);
    errs.push(StringError::unterminated_inline_string());
    Err(LexerError::new(lexer.span(), errs.into()))
  }
}
