use tokit::{
  lexer::Lexable,
  logos::{Lexer, Logos, Source},
};

use crate::error::{StringError, StringErrors};

use super::{super::SealedWrapper, BlockLineExtras, LitBlockStr, LitComplexBlockStr, LitPlainStr};

/// SIMD-accelerated scan over a block-string body to locate the closing
/// `"""` and count any `\"""` escape sequences along the way.
///
/// This replaces the inner per-byte `BlockStringToken` Logos DFA with a
/// memchr-style hop: we use `memspan::skip::skip_until` to jump straight
/// to the next `"`, then verify the surrounding bytes scalar-side. Most
/// real description blocks contain only a handful of `"` candidates
/// (often zero before the closing triple), so this is essentially a
/// single SIMD scan over the whole body.
///
/// The grammar is GraphQL §2.9.5: `\"""` is the only escape; a lone `\`
/// is just content. A `"""` is the end iff the immediately preceding
/// byte is *not* `\`.
///
/// Returns `(end_offset, escaped_count)` where `end_offset` is the byte
/// offset (within `body`) of the start of the closing `"""`, or `None`
/// if `body` does not contain a closing triple-quote (caller will emit
/// `StringError::unterminated_block_string`).
#[inline]
fn find_block_close_simd(body: &[u8]) -> (Option<usize>, usize) {
  let mut pos = 0usize;
  let mut escaped = 0usize;

  while pos < body.len() {
    // Hop to the next `"` candidate. memchr inside lexsimd routes to
    // the platform-optimal SIMD path; on aarch64 this is the NEON
    // `vceqq + vorrq + shrn-extract` chain seen in `memspan::skip::*`.
    let q_off = match memspan::skip::skip_until(&body[pos..], b'"') {
      Some(n) => pos + n,
      None => return (None, escaped),
    };

    // Need three bytes to form `"""`. If we don't have them, the body
    // ends inside an opening run of fewer than three quotes — that's
    // unterminated.
    if q_off + 3 > body.len() {
      return (None, escaped);
    }

    // Verify the next two bytes are also `"` (i.e. this `"` starts a
    // triple). If not, this is a single `"` of content; resume past it.
    if body[q_off + 1] != b'"' || body[q_off + 2] != b'"' {
      pos = q_off + 1;
      continue;
    }

    // We have `"""` at `q_off`. Is it `\"""` (escape)?
    //
    // GraphQL block-string escape rules: only `\"""` is recognized.
    // A standalone `\` is just literal content. So we only need to
    // look at the single byte immediately before — no need to count
    // backslashes for "escape the escape" semantics. (`\\"""` is
    // parsed as `\` + `\"""`, which our caller scans byte-by-byte
    // when computing line metadata anyway.)
    if q_off > 0 && body[q_off - 1] == b'\\' {
      escaped += 1;
      pos = q_off + 3;
      continue;
    }

    return (Some(q_off), escaped);
  }

  (None, escaped)
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(crate = tokit::logos, error(StringError<char>))]
pub(crate) enum BlockStringToken {
  /// \\"\\"\\" inside block string
  #[token("\\\"\"\"")]
  EscapedTripleQuote,
  /// terminator
  #[token("\"\"\"")]
  TripleQuote,
  /// Runs of any characters except the double quote **and backslash**;
  /// includes newlines and C0 controls.
  #[regex(r#"[^"\\]+"#)]
  Continue,
  /// A lone backslash (not followed by `"""`) is just content.
  #[token("\\")]
  Backslash,
  /// A single quote that is **not** part of `"""` (content)
  #[token("\"")]
  Quote,
}

impl<'l, S, T> Lexable<&mut SealedWrapper<Lexer<'l, T>>, StringErrors<char>>
  for LitBlockStr<S::Slice<'l>>
where
  T: Logos<'l, Source = S>,
  S: Source + ?Sized + 'l,
  S::Slice<'l>: AsRef<str>,
{
  #[inline]
  fn lex(lexer: &mut SealedWrapper<Lexer<'l, T>>) -> Result<Self, StringErrors<char>>
  where
    Self: Sized,
  {
    lex_block_str_from_str(lexer)
  }
}

#[inline]
pub(crate) fn lex_block_str_from_str<'l, S, T>(
  lexer: &mut SealedWrapper<Lexer<'l, T>>,
) -> Result<LitBlockStr<S::Slice<'l>>, StringErrors<char>>
where
  T: Logos<'l, Source = S>,
  S: Source + ?Sized + 'l,
  S::Slice<'l>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let body_bytes = remainder_str.as_bytes();

  let (close_off, escaped_triple_count) = find_block_close_simd(body_bytes);

  match close_off {
    Some(start) => {
      // Consume up to (and including) the closing """
      let end_off = start + 3;
      lexer.bump(end_off);

      // Inner content (between opening and closing)
      let content = &remainder_str[..start];

      // Sub-lex inner content to gather normalization facts + capacity.
      // This pass is per-line (terminator-driven state machine) and
      // stays scalar.
      let mut lines = BlockLineTok::lexer_with_extras(content, BlockLineExtras::default());
      while lines.next().is_some() {
        // callbacks update lines.extras
      }

      // Build the normalization plan + exact capacity
      let plan = super::compute_block_normalization_plan(
        &lines.extras,
        !content.is_empty(),
        escaped_triple_count,
      );

      if plan.is_clean {
        return Ok(LitPlainStr::new(lexer.slice()).into());
      }

      Ok(
        LitComplexBlockStr::new(
          lexer.slice(),
          escaped_triple_count,
          lines.extras.has_cr_terminators,
          lines.extras.leading_blank_lines,
          plan.effective_trailing,
          plan.common_indent,
          plan.total_lines,
          plan.required_capacity,
        )
        .into(),
      )
    }
    None => {
      // EOF without closing """ — bump to end of input and report.
      lexer.bump(body_bytes.len());
      let mut errs = StringErrors::default();
      errs.push(StringError::unterminated_block_string());
      Err(errs)
    }
  }
}

#[derive(Logos, Debug)]
#[logos(crate = tokit::logos, extras = BlockLineExtras)]
enum BlockLineTok {
  /// Body of a line (never includes a terminator).
  #[regex(r#"[^\r\n]+"#, on_line_body, allow_greedy = true)]
  LineBody,
  /// One line terminator: \r\n | \r | \n
  #[regex(r#"\r\n|\r|\n"#, on_terminator)]
  Terminator,
}

#[inline]
fn on_line_body(lex: &mut Lexer<'_, BlockLineTok>) {
  let line = lex.slice();
  let len = line.len(); // UTF-8 bytes
  let blank = super::is_blank_line(line.as_bytes());
  let line_idx = lex.extras.terminators; // 0 for first logical line

  if !lex.extras.saw_nonblank_any {
    if blank {
      lex.extras.leading_blank_lines += 1;
      lex.extras.pending_blank_body_bytes += len;
    } else {
      lex.extras.saw_nonblank_any = true;

      if line_idx > 0 {
        let ind = super::leading_ws_indent(line.as_bytes());
        lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
        lex.extras.nonblank_after_first_count += 1;
      }
      lex.extras.nonblank_body_bytes += len;

      // discard pending leading blanks
      lex.extras.pending_blank_body_bytes = 0;
      lex.extras.trailing_blank_lines = 0;
    }
  } else if blank {
    // may end up middle or trailing
    lex.extras.pending_blank_body_bytes += len;
    lex.extras.trailing_blank_lines += 1;
  } else {
    // nonblank after some content
    if line_idx > 0 {
      let ind = super::leading_ws_indent(line.as_bytes());
      lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
      lex.extras.nonblank_after_first_count += 1;
    }
    lex.extras.nonblank_body_bytes += len;

    // pending blanks are now 'middle' (kept)
    lex.extras.middle_blank_body_bytes += lex.extras.pending_blank_body_bytes;
    lex.extras.pending_blank_body_bytes = 0;

    lex.extras.trailing_blank_lines = 0;
  }

  lex.extras.saw_body_this_line = true;
}

#[inline]
fn on_terminator(lex: &mut Lexer<'_, BlockLineTok>) {
  let t = lex.slice().as_bytes();
  if !t.is_empty() && t[0] == b'\r' {
    lex.extras.has_cr_terminators = true;
  }

  // Empty physical line (no LineBody since last terminator) is a blank line
  if !lex.extras.saw_body_this_line {
    if !lex.extras.saw_nonblank_any {
      lex.extras.leading_blank_lines += 1;
      // (body bytes = 0)
    } else {
      lex.extras.trailing_blank_lines += 1;
      // (body bytes = 0; pending already accounts for it if any)
    }
  }

  lex.extras.terminators += 1;
  lex.extras.saw_body_this_line = false;
}

#[cfg(test)]
mod tests {
  //! GraphQL §2.9.5 block-string compliance tests for the SIMD scanner.
  //!
  //! These tests pin the *behavior* of `find_block_close_simd` against
  //! the reference Logos-DFA implementation. We keep the reference impl
  //! below as `find_block_close_logos_reference` and assert byte-for-byte
  //! parity across every spec edge case.
  //!
  //! Spec summary:
  //!   BlockStringCharacter ::
  //!     - SourceCharacter but not """ or \"""
  //!     - \"""
  //!
  //! In particular: a lone `\` is just literal content (NOT an escape
  //! character), and the only escape sequence is `\"""`. So `\\"""`
  //! parses as `\` + `\"""` (the backslash is content; the next four
  //! bytes form an escape).

  use super::*;

  /// Reference impl: byte-for-byte equivalent to the `BlockStringToken`
  /// Logos DFA, as a pure `(Option<usize>, usize)` function so we can
  /// compare directly to `find_block_close_simd`.
  fn find_block_close_logos_reference(body: &str) -> (Option<usize>, usize) {
    let mut lexer = BlockStringToken::lexer(body);
    let mut escaped = 0usize;
    while let Some(tok) = lexer.next() {
      match tok {
        Ok(BlockStringToken::EscapedTripleQuote) => escaped += 1,
        Ok(BlockStringToken::TripleQuote) => {
          // span.start is the offset of the closing """ within `body`.
          return (Some(lexer.span().start), escaped);
        }
        Ok(_) | Err(_) => {}
      }
    }
    (None, escaped)
  }

  /// Drive both implementations on every fixture and assert parity.
  fn parity(body: &str, label: &str) {
    let bytes = body.as_bytes();
    let simd = find_block_close_simd(bytes);
    let logos = find_block_close_logos_reference(body);
    assert_eq!(
      simd, logos,
      "[{label}] body = {body:?}\n  simd  = {simd:?}\n  logos = {logos:?}"
    );
  }

  #[test]
  fn empty_body_means_unterminated() {
    // After the opening `"""`, an empty remaining slice = no closing.
    parity("", "empty_body");
  }

  #[test]
  fn closing_at_offset_zero_empty_payload() {
    // Source: `""""""` → opening, body=``, closing. Caller passes the
    // bytes after the opening: `"""`. Closing is at offset 0.
    parity("\"\"\"", "closing_at_zero");
  }

  #[test]
  fn ascii_payload_then_closing() {
    parity("hello world\"\"\"", "ascii_payload");
  }

  #[test]
  fn single_quote_in_body() {
    parity("ab\"cd\"\"\"", "one_quote");
  }

  #[test]
  fn two_quotes_in_body() {
    // `ab""cd"""`  — two adjacent quotes are still content.
    parity("ab\"\"cd\"\"\"", "two_quotes");
  }

  #[test]
  fn long_quote_run_at_end() {
    // `abc""""""""""` (3 + body = abc + 7 quotes). The first three of
    // the trailing quotes form the closing; the rest are content the
    // outer lexer will see *after* this token.
    parity("abc\"\"\"\"\"\"\"", "long_quote_run");
  }

  #[test]
  fn single_escape() {
    // `ab\"""cd"""` — one escaped triple, then real closing.
    parity("ab\\\"\"\"cd\"\"\"", "single_escape");
  }

  #[test]
  fn escape_at_start_of_body() {
    // Body starts with `\"""xyz"""`.
    parity("\\\"\"\"xyz\"\"\"", "escape_at_start");
  }

  #[test]
  fn escape_immediately_before_close() {
    // `\"""` then closing — the literal triple is the only content.
    parity("\\\"\"\"\"\"\"", "escape_then_close");
  }

  #[test]
  fn double_escape() {
    // `\"""\"""rest"""` — two escapes back-to-back.
    parity("\\\"\"\"\\\"\"\"rest\"\"\"", "double_escape");
  }

  #[test]
  fn many_escapes() {
    // Stress: 10 escapes in a row.
    let body = "\\\"\"\"".repeat(10) + "\"\"\"";
    parity(&body, "many_escapes");
  }

  #[test]
  fn backslash_then_double_quote_only() {
    // `\""abc"""` — `\""` is not an escape (escape is `\"""` only).
    parity("\\\"\"abc\"\"\"", "backslash_then_two_quotes");
  }

  #[test]
  fn backslash_then_single_quote() {
    // `\"abc"""` — `\"` is two literal chars.
    parity("\\\"abc\"\"\"", "backslash_then_one_quote");
  }

  #[test]
  fn double_backslash_before_triple_is_content_plus_escape() {
    // `\\"""` — `\` (literal) + `\"""` (escape). Logos parses this as
    // Backslash + EscapedTripleQuote → escape count = 1.
    parity("\\\\\"\"\"\"\"\"", "double_backslash_then_triple");
  }

  #[test]
  fn triple_backslash_before_triple_is_two_content_plus_escape() {
    // `\\\"""` — two literal `\` + `\"""` escape. escape count = 1.
    parity("\\\\\\\"\"\"\"\"\"", "triple_backslash_then_triple");
  }

  #[test]
  fn quadruple_backslash_before_triple() {
    // `\\\\"""` — three literal `\` + `\"""` escape. escape count = 1.
    parity("\\\\\\\\\"\"\"\"\"\"", "quadruple_backslash_then_triple");
  }

  #[test]
  fn lone_backslashes_with_no_quotes() {
    parity("foo\\\\bar\\\\baz\"\"\"", "lone_backslashes");
  }

  #[test]
  fn unterminated_no_quotes() {
    parity("hello world", "unterminated_no_quotes");
  }

  #[test]
  fn unterminated_one_trailing_quote() {
    parity("hello\"", "unterminated_one_quote");
  }

  #[test]
  fn unterminated_two_trailing_quotes() {
    parity("hello\"\"", "unterminated_two_quotes");
  }

  #[test]
  fn unterminated_after_escape() {
    // `hello\""" ` then EOF — escape consumed, but no closing.
    parity("hello\\\"\"\"", "unterminated_after_escape");
  }

  #[test]
  fn unterminated_with_orphan_quotes() {
    parity("\"a\"b\"c", "unterminated_orphan_quotes");
  }

  #[test]
  fn utf8_payload() {
    parity("héllo wörld 世界 🦀\"\"\"", "utf8_payload");
  }

  #[test]
  fn newlines_and_tabs() {
    parity("line1\nline2\r\nline3\tindented\"\"\"", "newlines");
  }

  #[test]
  fn control_bytes_in_body() {
    // GraphQL spec allows any SourceCharacter; control bytes are
    // treated as content (validity is enforced elsewhere).
    parity("low\x01\x02high\"\"\"", "control_bytes");
  }

  #[test]
  fn many_singletons_then_close() {
    // Lots of single quotes scattered through the body — each forces
    // a memchr hop but no `"""` until the end.
    let body = "\"a\"b\"c\"d\"e\"f\"g\"h\"\"\"";
    parity(body, "many_singletons");
  }

  #[test]
  fn alternating_backslash_quote_then_close() {
    // `\"\"\"\"\"\"` — alternating backslashes and quotes (none is
    // a `\"""` because there are not three consecutive `"`).
    let body = "\\\"\\\"\\\"abc\"\"\"";
    parity(body, "alternating_bs_q");
  }

  #[test]
  fn very_long_body_no_quotes_then_close() {
    let body = "x".repeat(10_000) + "\"\"\"";
    parity(&body, "very_long_no_quotes");
  }

  #[test]
  fn very_long_body_with_quote_at_end() {
    let body = "x".repeat(10_000) + "y\"z\"\"\"";
    parity(&body, "very_long_one_quote");
  }

  #[test]
  fn parity_kitchen_sink_block_strings() {
    // Pull every block-string body out of the kitchen sink fixture
    // and run parity on each.
    const SRC: &str =
      include_str!("../../../../smear/tests/fixtures/executables/kitchen-sink_canonical.graphql");
    let bytes = SRC.as_bytes();
    let mut i = 0;
    let mut n = 0;
    while i + 3 <= bytes.len() {
      if &bytes[i..i + 3] == b"\"\"\"" {
        let body_start = i + 3;
        // Find any close (don't care about correctness here, just need
        // an end so we have a complete block-string body to test).
        let body = &SRC[body_start..];
        parity(body, &format!("kitchen_sink #{n}"));
        n += 1;
        // Skip past the close that the reference impl found.
        let (close, _) = find_block_close_logos_reference(body);
        match close {
          Some(off) => i = body_start + off + 3,
          None => break,
        }
      } else {
        i += 1;
      }
    }
  }
}
