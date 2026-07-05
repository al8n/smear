use tokit::{
  SimpleSpan,
  utils::{Lexeme, PositionedChar},
};

use crate::error::{InvalidUnicodeHexDigits, StringError, StringErrors, UnicodeError};

use super::{LitBlockStr, LitComplexBlockStr, LitPlainStr};

type Span = SimpleSpan;

/// SIMD-accelerated inline string scanner (drop-in for `skip_block_str_in_bytes`).
///
/// `src`    — bytes starting **after** the opening `"` (the string body).
/// `offset` — absolute byte position of `src[0]` in the original source;
///            used only for error spans.
///
/// Returns:
/// - `Ok(LitBlockStr<usize>)` — success. The inner `usize` is the number of
///   bytes consumed from `src` **including** the closing `"`.
/// - `Err((consumed, errors))` — unterminated string or validation errors.
#[inline]
pub(crate) fn skip_block_str_simd(
  offset: usize,
  src: &[u8],
) -> Result<LitBlockStr<usize>, (usize, StringErrors<u8>)> {
  let mut pos = 0usize;
  let mut capacity = 0usize;
  let mut has_escapes = false;
  let mut errs = StringErrors::default();

  let mut num_escaped_triple_quotes = 0usize;

  loop {
    // ── SIMD bulk plain-character scan ───────────────────────────────────────
    // 4 needles → NEON on aarch64 (≥16 bytes); scalar memchr4 otherwise.
    // Stops at the first `"`, `\`, `\r`, or `\n`.
    let next = match memspan::skip::skip_until(&src[pos..], [b'"', b'\\']) {
      Some(n) => pos + n,
      None => {
        // No special byte found — string runs off end of input.
        errs.push(StringError::unterminated_block_string());
        return Err((src.len(), errs));
      }
    };

    capacity += next - pos;
    pos = next;

    match src[pos] {
      // ── end of string ──────────────────────────────────────────────────────
      b'"' => match src.get(pos..pos + 3) {
        Some([b'"', b'"', b'"']) => {
          pos += 3;
          if !errs.is_empty() {
            return Err((pos, errs));
          }
          return Ok(if has_escapes {
            LitComplexBlockStr::new(pos, num_escaped_triple_quotes).into()
          } else {
            LitPlainStr::new(pos).into()
          });
        }
        None => todo!(),
      },
      // ── escape sequence ────────────────────────────────────────────────────
      b'\\' => {
        let abs_backslash = offset + pos;
        pos += 1; // consume '\'

        if pos >= src.len() {
          errs.push(StringError::unterminated_block_string());
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
              Span::new(abs_backslash, offset + pos + 1),
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
          errs.push(StringError::unexpected_carriage_return_new_line(Span::new(
            abs,
            abs + 2,
          )));
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
