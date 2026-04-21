//! SIMD-accelerated inline string scanner.
//!
//! Replaces the Logos `StringToken` DFA with `lexsimd::skip::skip_until` for
//! bulk plain-character runs (the dominant case in real queries), falling back
//! to a tight scalar loop only for the rare special bytes (`"`, `\`, `\r`, `\n`).
//!
//! # Algorithm
//!
//! ```text
//! loop:
//!   skip_until(src[pos..], ['"', '\\', '\r', '\n'])   ← SIMD / memchr2-4
//!   capacity += plain bytes skipped
//!   match stopped-at byte:
//!     '"'  → done (plain or complex)
//!     '\\' → handle escape inline
//!     '\r' | '\n' → push error, continue
//! ```
//!
//! # API
//!
//! [`skip_inline_str_simd`] is a drop-in for `skip_inline_str_in_bytes`; the
//! source argument `src` starts **after** the opening `"`.

use tokit::{
  SimpleSpan,
  utils::{Lexeme, PositionedChar},
};

use crate::error::{InvalidUnicodeHexDigits, StringError, StringErrors, UnicodeError};

use super::{LitComplexInlineStr, LitInlineStr, LitPlainStr, utf8_len_for_scalar};

type Span = SimpleSpan;

// ─── helpers ──────────────────────────────────────────────────────────────────

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
  debug_assert_eq!(hex.len(), 4);
  Some(
    (hex_val(hex[0])? << 12) | (hex_val(hex[1])? << 8) | (hex_val(hex[2])? << 4) | hex_val(hex[3])?,
  )
}

#[inline(always)]
fn is_high_surrogate(cp: u32) -> bool {
  matches!(cp, 0xD800..=0xDBFF)
}

#[inline(always)]
fn is_low_surrogate(cp: u32) -> bool {
  matches!(cp, 0xDC00..=0xDFFF)
}

/// Peek ahead for a `\uXXXX` low-surrogate starting in `remainder`.
#[inline(always)]
fn try_parse_low_surrogate(remainder: &[u8]) -> Option<u32> {
  if remainder.len() < 6 {
    return None;
  }
  if remainder[0] != b'\\' || remainder[1] != b'u' {
    return None;
  }
  let cp = parse_u4(&remainder[2..6])?;
  if is_low_surrogate(cp) { Some(cp) } else { None }
}

// ─── unicode escape handlers ───────────────────────────────────────────────────

/// Handle `\uXXXX` (fixed-width) at `src_pos` (first hex char after `u`).
///
/// `abs_backslash` — absolute position of the leading `\`.
///
/// Returns `(bytes_consumed_from_src_pos, capacity_delta)`.
#[inline]
fn handle_fixed_unicode(
  src: &[u8],
  src_pos: usize,
  abs_backslash: usize,
  errs: &mut StringErrors<u8>,
) -> (usize, usize) {
  let available = (src.len() - src_pos).min(4);

  if available == 0 {
    let span = Span::new(abs_backslash, abs_backslash + 2);
    errs.push(UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Range(span.into())).into());
    return (0, 0);
  }

  if available < 4 {
    // Might be incomplete (all hex so far) or invalid (some non-hex).
    let hex_bytes = &src[src_pos..src_pos + available];
    let all_hex = hex_bytes.iter().all(|&b| hex_val(b).is_some());
    let span = Span::new(abs_backslash, abs_backslash + 2 + available);
    if all_hex {
      errs.push(UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Range(span.into())).into());
    } else {
      let mut invalid = InvalidUnicodeHexDigits::<u8>::default();
      for (i, &b) in hex_bytes.iter().enumerate() {
        if hex_val(b).is_none() {
          invalid.push_fixed(PositionedChar::with_position(b, abs_backslash + 2 + i));
        }
      }
      errs.push(UnicodeError::invalid_fixed_unicode_escape_sequence(invalid, span.into()).into());
    }
    return (available, 0);
  }

  // available == 4: collect any non-hex digits.
  let hex_bytes = &src[src_pos..src_pos + 4];
  let mut cp = 0u32;
  let mut invalid = InvalidUnicodeHexDigits::<u8>::default();

  for (i, &b) in hex_bytes.iter().enumerate() {
    match hex_val(b) {
      Some(v) => {
        if invalid.is_empty() {
          cp = (cp << 4) | v;
        }
      }
      None => {
        invalid.push_fixed(PositionedChar::with_position(b, abs_backslash + 2 + i));
      }
    }
  }

  if !invalid.is_empty() {
    let span = Span::new(abs_backslash, abs_backslash + 6);
    errs.push(UnicodeError::invalid_fixed_unicode_escape_sequence(invalid, span.into()).into());
    return (4, 0);
  }

  // All 4 hex digits valid — check for surrogate pair.
  if is_high_surrogate(cp) {
    if let Some(low_cp) = try_parse_low_surrogate(&src[src_pos + 4..]) {
      let combined = 0x10000 + (((cp - 0xD800) << 10) | (low_cp - 0xDC00));
      // consumed = 4 hex digits + \uXXXX (6 bytes) for low surrogate
      return (10, utf8_len_for_scalar(combined));
    }
    let span = Span::new(abs_backslash, abs_backslash + 6);
    errs.push(UnicodeError::unpaired_high_surrogate(Lexeme::Range(span.into())).into());
    return (4, 0);
  }

  if is_low_surrogate(cp) {
    let span = Span::new(abs_backslash, abs_backslash + 6);
    errs.push(UnicodeError::unpaired_low_surrogate(Lexeme::Range(span.into())).into());
    return (4, 0);
  }

  (4, utf8_len_for_scalar(cp))
}

/// Handle `\u{...}` (braced) at `src_pos` (first content char after `{`).
///
/// `abs_backslash` — absolute position of the leading `\`.
///
/// Returns `(bytes_consumed_from_src_pos, capacity_delta)`.
#[inline]
fn handle_braced_unicode(
  src: &[u8],
  src_pos: usize,
  abs_backslash: usize,
  errs: &mut StringErrors<u8>,
) -> (usize, usize) {
  let rest = &src[src_pos..];

  // abs_backslash + 3 == abs position of src[src_pos]  (\ u { content...)
  //                                                          ^
  let mut hex_count = 0usize;
  let mut cp = 0u32;
  let mut all_hex = true;

  for (i, &b) in rest.iter().enumerate() {
    if b == b'}' {
      let consumed = i + 1; // content bytes + '}'
      let abs_end = abs_backslash + 3 + i + 1;
      let span = Span::new(abs_backslash, abs_end);

      if hex_count == 0 && all_hex {
        errs.push(UnicodeError::empty_braced_unicode_escape(span.into()).into());
        return (consumed, 0);
      }
      if !all_hex {
        errs.push(UnicodeError::invalid_braced_unicode_escape_sequence(span.into()).into());
        return (consumed, 0);
      }
      if hex_count > 6 {
        errs.push(
          UnicodeError::too_many_digits_in_braced_unicode_escape(span.into(), hex_count).into(),
        );
        return (consumed, 0);
      }
      // Valid 1-6 hex digits
      if cp > 0x10_FFFF {
        errs.push(UnicodeError::overflow_braced_unicode_escape(span.into(), cp).into());
        return (consumed, 0);
      }
      if (0xD800..=0xDFFF).contains(&cp) {
        errs.push(UnicodeError::surrogate_braced_unicode_escape(span.into(), cp).into());
        return (consumed, 0);
      }
      return (consumed, utf8_len_for_scalar(cp));
    }

    match hex_val(b) {
      Some(v) => {
        if all_hex && hex_count < 6 {
          cp = (cp << 4) | v;
        }
        hex_count += 1;
      }
      None => {
        all_hex = false;
      }
    }
  }

  // No closing '}' found — unterminated braced escape.
  let consumed = rest.len();
  let abs_end = abs_backslash + 3 + consumed;
  let span = Span::new(abs_backslash, abs_end);
  errs.push(UnicodeError::unclosed_braced_unicode_escape(span.into()).into());
  (consumed, 0)
}

// ─── public interface ─────────────────────────────────────────────────────────

/// SIMD-accelerated inline string scanner (drop-in for `skip_inline_str_in_bytes`).
///
/// `src`    — bytes starting **after** the opening `"` (the string body).
/// `offset` — absolute byte position of `src[0]` in the original source;
///            used only for error spans.
///
/// Returns:
/// - `Ok(LitInlineStr<usize>)` — success. The inner `usize` is the number of
///   bytes consumed from `src` **including** the closing `"`.
/// - `Err((consumed, errors))` — unterminated string or validation errors.
#[inline]
pub(crate) fn skip_inline_str_simd(
  offset: usize,
  src: &[u8],
) -> Result<LitInlineStr<usize>, (usize, StringErrors<u8>)> {
  let mut pos = 0usize;
  let mut capacity = 0usize;
  let mut has_escapes = false;
  let mut errs = StringErrors::default();

  loop {
    // ── SIMD bulk plain-character scan ───────────────────────────────────────
    // 4 needles → NEON on aarch64 (≥16 bytes); scalar memchr4 otherwise.
    // Stops at the first `"`, `\`, `\r`, or `\n`.
    let next = match lexsimd::skip::skip_until(&src[pos..], [b'"', b'\\', b'\r', b'\n']) {
      Some(n) => pos + n,
      None => {
        // No special byte found — string runs off end of input.
        errs.push(StringError::unterminated_inline_string());
        return Err((src.len(), errs));
      }
    };

    capacity += next - pos;
    pos = next;

    match src[pos] {
      // ── end of string ──────────────────────────────────────────────────────
      b'"' => {
        pos += 1;
        if !errs.is_empty() {
          return Err((pos, errs));
        }
        return Ok(if has_escapes {
          LitComplexInlineStr::new(pos, capacity).into()
        } else {
          LitPlainStr::new(pos).into()
        });
      }

      // ── escape sequence ────────────────────────────────────────────────────
      b'\\' => {
        let abs_backslash = offset + pos;
        pos += 1; // consume '\'

        if pos >= src.len() {
          errs.push(StringError::unterminated_inline_string());
          return Err((pos, errs));
        }

        match src[pos] {
          b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' => {
            has_escapes = true;
            capacity += 1;
            pos += 1;
          }
          b'u' => {
            pos += 1; // consume 'u'
            let (consumed, cap_delta) = if pos < src.len() && src[pos] == b'{' {
              pos += 1; // consume '{'
              handle_braced_unicode(src, pos, abs_backslash, &mut errs)
            } else {
              handle_fixed_unicode(src, pos, abs_backslash, &mut errs)
            };
            if cap_delta > 0 {
              has_escapes = true;
              capacity += cap_delta;
            }
            pos += consumed;
          }
          other => {
            errs.push(StringError::unexpected_escaped_character(
              Span::new(abs_backslash, offset + pos + 1).into(),
              other,
              offset + pos,
            ));
            pos += 1;
          }
        }
      }

      // ── forbidden line terminators (errors) ────────────────────────────────
      b'\r' => {
        let abs = offset + pos;
        if pos + 1 < src.len() && src[pos + 1] == b'\n' {
          errs.push(StringError::unexpected_carriage_return_new_line(
            Span::new(abs, abs + 2).into(),
          ));
          pos += 2;
        } else {
          errs.push(StringError::unexpected_carriage_return(b'\r', abs));
          pos += 1;
        }
      }
      b'\n' => {
        errs.push(StringError::unexpected_new_line(b'\n', offset + pos));
        pos += 1;
      }

      _ => unreachable!("skip_until stops only at one of the four needles"),
    }
  }
}
