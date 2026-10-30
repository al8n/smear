use tokora::{
  SimpleSpan,
  lexer::Lexable,
  logos::{Lexer, Logos, Source},
  utils::Lexeme,
};

use crate::{
  error::{StringError, StringErrors},
  utf8::IllFormedUtf8,
};

use super::{
  super::SealedWrapper, BlockLineExtras, LitBlockStr, LitComplexBlockStr, LitPlainBlockStr,
};

/// `&[u8]`-source variant of [`super::str::find_block_close_simd`]. The
/// algorithm is byte-for-byte identical: finding the close works on raw bytes,
/// and UTF-8 status is irrelevant to it because both `"` (0x22) and `\` (0x5C)
/// are below 0x80 and so can never appear inside a UTF-8 sequence. Whether the
/// content it delimits is UTF-8 is a separate question, answered afterwards by
/// [`IllFormedUtf8`] on both paths. See the str-side function for the full spec
/// citation and edge-case discussion.
#[inline]
fn find_block_close_simd(body: &[u8]) -> (usize, Option<usize>, usize) {
  let mut pos = 0usize;
  let mut escaped = 0usize;

  while pos < body.len() {
    let q_off = match memspan::skip::skip_until(&body[pos..], b'"') {
      Some(n) => pos + n,
      None => return (body.len(), None, escaped),
    };

    if q_off + 3 > body.len() {
      return (body.len(), None, escaped);
    }

    if body[q_off + 1] != b'"' || body[q_off + 2] != b'"' {
      pos = q_off + 1;
      continue;
    }

    if q_off > 0 && body[q_off - 1] == b'\\' {
      escaped += 1;
      pos = q_off + 3;
      continue;
    }

    return (pos, Some(q_off), escaped);
  }

  (pos, None, escaped)
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(crate = tokora::logos, utf8 = false, error(StringError<u8>))]
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

impl<'a, S, T> Lexable<&mut SealedWrapper<Lexer<'a, T>>, StringErrors<u8>>
  for LitBlockStr<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<[u8]>,
{
  #[inline]
  fn lex(lexer: &mut SealedWrapper<Lexer<'a, T>>) -> Result<Self, StringErrors<u8>>
  where
    Self: Sized,
  {
    lex_block_str_from_bytes(lexer)
  }
}

/// The Logos-path block-string lexer over a byte source.
///
/// Three callers reach it, all with a byte source: `LitStr::try_from(&[u8])`,
/// the lossless byte lexers through [`Lexable`], and a byte source's delegation,
/// which re-lexes a block literal the fast path refused. A text source reaches
/// the `str` twin instead.
///
/// A terminated literal whose content is not well-formed UTF-8 is refused with
/// one [`StringError::UnsupportedCharacter`] per ill-formed sequence, each at
/// its absolute span. The sequences are [`IllFormedUtf8`]'s, the same walk
/// `skip_block_str_from_bytes::<true>` refuses on, which is what keeps
/// delegation's `expect_err` true for a block literal.
#[inline]
pub(crate) fn lex_block_str_from_bytes<'a, S, T>(
  lexer: &mut SealedWrapper<Lexer<'a, T>>,
) -> Result<LitBlockStr<S::Slice<'a>>, StringErrors<u8>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_bytes = remainder.as_ref();

  // SIMD scan, identical shape to the str-side variant.
  let (_, close_off, num_escaped_triple_quotes) = find_block_close_simd(remainder_bytes);

  match close_off {
    Some(start) => {
      // Where the content starts, read before the bump moves the span's end: every caller has
      // consumed exactly the opening `"""` by now.
      let base = lexer.span().end;
      lexer.bump(start + 3);

      let content = &remainder_bytes[..start];

      let mut errs = StringErrors::default();
      for bad in IllFormedUtf8::new(content) {
        errs.push(StringError::UnsupportedCharacter(Lexeme::Range(
          SimpleSpan::new(base + bad.start, base + bad.end),
        )));
      }
      if !errs.is_empty() {
        return Err(errs);
      }

      let extras = line_facts(content);
      let plan = super::compute_block_normalization_plan(
        &extras,
        !content.is_empty(),
        num_escaped_triple_quotes,
      );

      if plan.is_clean {
        return Ok(LitPlainBlockStr::new(lexer.slice()).into());
      }

      Ok(
        LitComplexBlockStr::new(
          lexer.slice(),
          num_escaped_triple_quotes,
          extras.has_cr_terminators,
          plan.leading_blank_lines,
          plan.effective_trailing,
          plan.common_indent,
          plan.total_lines,
          plan.required_capacity,
        )
        .into(),
      )
    }
    None => {
      lexer.bump(remainder_bytes.len());
      let mut errs = StringErrors::default();
      errs.push(StringError::unterminated_block_string());
      Err(errs)
    }
  }
}

/// `&[u8]` block-string scanner for the SIMD syntactic fast path, which uses it
/// for every source type, text sources included.
///
/// `src` is the block-string body **after** the opening `"""`. Mirrors
/// [`skip_inline_str_simd`](crate::skip_inline_str_simd)'s convention: on a
/// valid literal the `usize` carried by the returned [`LitBlockStr`] is the
/// number of bytes consumed **after** the opening delimiter — the content plus
/// the always-3-byte closing `"""` — so the caller recovers the full token by
/// adding the 3 opening bytes. The facts (`num_escaped_triple_quotes`, CR
/// flag, blank-line counts, indent, line count, capacity) come from the exact
/// same `find_block_close_simd` + `BlockLineTok` + `compute_block_normalization_plan`
/// pipeline that [`lex_block_str_from_bytes`] uses, so the emitted token is
/// byte-identical to the Logos-delegated one.
///
/// `CHECK_UTF8` is `true` for a byte source, whose string literals must be
/// well-formed UTF-8 (smear#246), and `false` for a text source, which pays
/// nothing for it. When it is `true`, a terminated body with any ill-formed
/// sequence is refused at the first one [`IllFormedUtf8`] yields; that is the
/// same walk [`lex_block_str_from_bytes`] reports every sequence from, so the
/// delegation that follows refuses too.
///
/// An unterminated or ill-formed body returns `Err`, and the SIMD lexer
/// discards the payload and delegates the whole token to Logos rather than
/// constructing a source-typed error on the fast path. An unterminated body is
/// never checked for UTF-8 here, and the delegated lexer does not check it
/// either: its answer is the unterminated error alone.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
#[inline]
pub(crate) fn skip_block_str_from_bytes<const CHECK_UTF8: bool>(
  src: &[u8],
) -> Result<LitBlockStr<usize>, (usize, StringErrors<u8>)> {
  // SIMD scan, identical shape to the str-side variant.
  let (read, close_off, num_escaped_triple_quotes) = find_block_close_simd(src);

  match close_off {
    Some(start) => {
      let content = &src[..start];

      // The payload is dead, so nothing is built for the refusal.
      if CHECK_UTF8 && IllFormedUtf8::new(content).next().is_some() {
        return Err((start + 3, StringErrors::default()));
      }

      let extras = line_facts(content);
      let plan = super::compute_block_normalization_plan(
        &extras,
        !content.is_empty(),
        num_escaped_triple_quotes,
      );

      // Bytes consumed AFTER the opening `"""`: the content length (`start`, the
      // close offset) plus the always-3-byte closing `"""`. This mirrors the
      // Logos path's `lexer.bump(start + 3)` verbatim, so the token span covers
      // the full `"""…"""` including both delimiters.
      let consumed = start + 3;

      if plan.is_clean {
        return Ok(LitPlainBlockStr::new(consumed).into());
      }

      Ok(
        LitComplexBlockStr::new(
          consumed,
          num_escaped_triple_quotes,
          extras.has_cr_terminators,
          plan.leading_blank_lines,
          plan.effective_trailing,
          plan.common_indent,
          plan.total_lines,
          plan.required_capacity,
        )
        .into(),
      )
    }
    None => {
      let mut errs = StringErrors::default();
      errs.push(StringError::unterminated_block_string());
      Err((read, errs))
    }
  }
}

/// The line facts `BlockLineTok`'s callbacks record over a block string's
/// content, which the caller has already found to be UTF-8.
#[inline]
fn line_facts(content: &[u8]) -> BlockLineExtras {
  let mut lines = BlockLineTok::lexer_with_extras(content, BlockLineExtras::default());
  for line in lines.by_ref() {
    // `BlockLineTok`'s classes are Unicode, so content that is not UTF-8 used to produce error
    // tokens here, which this loop swallowed; their bytes never reached `on_line_body`, so
    // `nonblank_body_bytes` undercounted the line they sat in. Every caller now refuses such
    // content first or holds a text source, and in well-formed UTF-8 every byte belongs to a
    // scalar that `LineBody` or `Terminator` matches, so no byte is left for an error token.
    debug_assert!(
      line.is_ok(),
      "a block string's content reached the line scan without being UTF-8"
    );
  }
  lines.extras
}

// sub-lexer over inner block-string content
#[derive(Logos, Debug)]
#[logos(crate = tokora::logos, utf8 = false, extras = BlockLineExtras)]
enum BlockLineTok {
  /// Body of a line (one or more bytes, never includes a terminator).
  /// We process the whole line in the callback.
  #[regex(r#"[^\r\n]+"#, on_line_body, allow_greedy = true)]
  LineBody,

  /// One line terminator: \r\n | \r | \n
  #[regex(r#"\r\n|\r|\n"#, on_terminator)]
  Terminator,
}

// callbacks mutate `extras` to record state + capacity
#[inline]
fn on_line_body(lex: &mut Lexer<'_, BlockLineTok>) {
  let line = lex.slice();
  let len = line.len();
  let blank = super::is_blank_line(line);
  let line_idx = lex.extras.terminators; // 0 for first logical line

  if !lex.extras.saw_nonblank_any {
    if blank {
      // leading blank run
      lex.extras.leading_blank_lines += 1;
      lex.extras.pending_blank_body_bytes += len;
    } else {
      // first nonblank line
      lex.extras.saw_nonblank_any = true;

      if line_idx > 0 {
        let ind = super::leading_ws_indent(line);
        lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
        lex.extras.nonblank_after_first_count += 1;
      }
      lex.extras.nonblank_body_bytes += len;

      // drop the pending leading blanks (not kept)
      lex.extras.pending_blank_body_bytes = 0;
      lex.extras.trailing_blank_lines = 0;
    }
  } else if blank {
    // possible middle-or-trailing blank run
    lex.extras.pending_blank_body_bytes += len;
    lex.extras.trailing_blank_lines += 1;
  } else {
    // nonblank after some content
    if line_idx > 0 {
      let ind = super::leading_ws_indent(line);
      lex.extras.common_indent = Some(lex.extras.common_indent.map_or(ind, |m| m.min(ind)));
      lex.extras.nonblank_after_first_count += 1;
    }
    lex.extras.nonblank_body_bytes += len;

    // pending blanks are in the middle (kept); move to middle bucket
    lex.extras.middle_blank_body_bytes += lex.extras.pending_blank_body_bytes;
    lex.extras.pending_blank_body_bytes = 0;

    lex.extras.trailing_blank_lines = 0;
  }

  lex.extras.saw_body_this_line = true;
}

#[inline]
fn on_terminator(lex: &mut Lexer<'_, BlockLineTok>) {
  let t = lex.slice();
  if !t.is_empty() && t[0] == b'\r' {
    lex.extras.has_cr_terminators = true;
  }

  // empty line (no LineBody since last terminator) is blank
  if !lex.extras.saw_body_this_line {
    if !lex.extras.saw_nonblank_any {
      lex.extras.leading_blank_lines += 1;
      // body bytes for an empty line are 0
    } else {
      lex.extras.trailing_blank_lines += 1;
      // body bytes for this empty line are 0; pending stays as-is
    }
  }

  lex.extras.terminators += 1;
  lex.extras.saw_body_this_line = false;
}
