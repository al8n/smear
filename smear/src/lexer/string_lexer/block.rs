use std::{borrow::Cow, string::String};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::human_display::DisplayHuman;

pub(crate) use self::{
  str::{BlockStringToken, lex_block_str_from_str},
  u8_slice::{BlockStringToken as BytesBlockStringToken, lex_block_str_from_bytes},
};

mod str;
mod u8_slice;

use super::LitPlainStr;

variant_type!(
  /// A block string representation in GraphQL containing one or more escaped triple quotes,
  /// carriage returns, leading/trailing blank lines, or common indentation.
  #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  pub struct LitComplexBlockStr {
    /// The number of escaped triple quotes in the string.
    num_escaped_triple_quotes: usize,
    /// Whether there are any carriage return (`\r`) or carriage return + line feed (`\r\n`)
    /// line terminators in the string.
    has_cr_terminators: bool,
    /// The number of leading blank lines (spaces/tabs only) to trim.
    leading_blank_lines: usize,
    /// The number of trailing blank lines (spaces/tabs only) to trim.
    trailing_blank_lines: usize,
    /// The common indentation (spaces/tabs) across all non-blank lines
    /// *after the first* that should be stripped.
    common_indent: usize,
    /// The total number of lines in the block string (including blank lines).
    total_lines: usize,

    /// Required capacity to store the normalized string.
    required_capacity: usize,
  }
);

/// A block string representation in GraphQL.
#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitBlockStr<S> {
  /// A clean block string, no escaped triple quotes, no CR/CRLF,
  /// no leading/trailing blank lines, and no common indent.
  Plain(LitPlainStr<S>),

  /// A block string required some processing to unescape or normalize.
  /// This includes handling escaped triple quotes, line endings, and indentation.
  Complex(LitComplexBlockStr<S>),
}

impl<S: DisplayHuman> core::fmt::Display for LitBlockStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Plain(s) => DisplayHuman::fmt(s.source_ref(), f),
      Self::Complex(c) => DisplayHuman::fmt(c.source_ref(), f),
    }
  }
}

impl<'a> LitBlockStr<&'a str> {
  /// Returns the str representation of the block string.
  #[inline(always)]
  pub const fn as_str(&self) -> &'a str {
    match self {
      Self::Plain(s) => s.as_str(),
      Self::Complex(c) => c.as_str(),
    }
  }
}

impl<'a> LitBlockStr<&'a [u8]> {
  /// Returns the byte slice representation of the block string.
  #[inline(always)]
  pub const fn as_bytes(&self) -> &'a [u8] {
    match self {
      Self::Plain(s) => s.as_bytes(),
      Self::Complex(c) => c.as_bytes(),
    }
  }
}

impl<S> LitBlockStr<S> {
  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Plain(s) => s.source(),
      Self::Complex(c) => c.source(),
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Plain(s) => s.source_ref(),
      Self::Complex(c) => c.source_ref(),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn to_equivalent<T>(&self) -> LitBlockStr<T>
  where
    S: logosky::utils::ToEquivalent<T>,
  {
    match self {
      Self::Plain(s) => LitBlockStr::Plain(s.to_equivalent()),
      Self::Complex(c) => LitBlockStr::Complex(c.to_equivalent()),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn into_equivalent<T>(self) -> LitBlockStr<T>
  where
    S: logosky::utils::IntoEquivalent<T>,
  {
    match self {
      Self::Plain(s) => LitBlockStr::Plain(s.into_equivalent()),
      Self::Complex(c) => LitBlockStr::Complex(c.into_equivalent()),
    }
  }
}

impl_common_traits!(LitBlockStr::<&'a str>::as_str);
impl_common_traits!(LitBlockStr::<&'a [u8]>::as_bytes);
impl_common_traits!(LitComplexBlockStr::<&'a str>::as_str);
impl_common_traits!(LitComplexBlockStr::<&'a [u8]>::as_bytes);

#[inline(always)]
fn is_blank_line(s: &[u8]) -> bool {
  s.iter().all(|&b| b == b' ' || b == b'\t')
}

#[inline(always)]
fn leading_ws_indent(bytes: &[u8]) -> usize {
  bytes
    .iter()
    .take_while(|&&c| c == b' ' || c == b'\t')
    .count()
}

#[inline(always)]
fn chop_indent(s: &str, mut n: usize) -> &str {
  let bytes = s.as_bytes();
  let mut i = 0usize;
  while i < bytes.len() && n > 0 {
    match bytes[i] {
      b' ' | b'\t' => {
        i += 1;
        n -= 1;
      }
      _ => break,
    }
  }
  &s[i..]
}

impl<'a> From<LitBlockStr<&'a str>> for Cow<'a, str> {
  #[inline]
  fn from(value: LitBlockStr<&'a str>) -> Self {
    match value {
      LitBlockStr::Plain(s) => Cow::Borrowed(s.as_str()),
      LitBlockStr::Complex(s) => {
        let raw = s.as_str();

        // Inner content between the surrounding delimiters.
        let inner = &raw[3..raw.len() - 3];

        let total_lines = s.total_lines();
        let leading_blank_lines = s.leading_blank_lines();
        let trailing_blank_lines = s.trailing_blank_lines(); // already the "effective" trailing
        let common_indent = s.common_indent();
        let cap = s.required_capacity();

        let keep_start = leading_blank_lines;
        let keep_end = total_lines.saturating_sub(trailing_blank_lines);

        // Fast-return for empty result.
        if keep_start >= keep_end {
          return std::borrow::Cow::Owned(String::new());
        }

        // Write one logical line body:
        // - optionally dedent (only non-first, non-blank lines),
        // - unescape \"\"\" -> """
        #[inline(always)]
        fn write_line(out: &mut String, line: &str, dedent: usize, is_first_kept: bool) {
          let body = if is_first_kept || is_blank_line(line.as_bytes()) {
            line
          } else {
            chop_indent(line, dedent)
          };

          // Copy with `\"\"\"` → `"""` (drop the backslash)
          let b = body.as_bytes();
          let mut i = 0usize;
          let mut chunk_start = 0usize;
          while i < b.len() {
            if b[i] == b'\\'
              && i + 3 < b.len()
              && b[i + 1] == b'"'
              && b[i + 2] == b'"'
              && b[i + 3] == b'"'
            {
              // Flush up to the backslash
              // (safe to slice: split points are ASCII)
              unsafe {
                out
                  .as_mut_vec()
                  .extend_from_slice(&body.as_bytes()[chunk_start..i]);
              }
              out.push_str(r#"""""#); // three quotes
              i += 4; // skip backslash + 3 quotes
              chunk_start = i;
            } else {
              i += 1;
            }
          }
          // Flush tail
          unsafe {
            out
              .as_mut_vec()
              .extend_from_slice(&body.as_bytes()[chunk_start..]);
          }
        }

        // Iterate logical lines of `inner`, honoring CR, LF, or CRLF.
        let mut out = String::with_capacity(cap);
        let mut i = 0usize;
        let bytes = inner.as_bytes();
        let mut line_idx = 0usize;
        let mut wrote_any = false;

        while line_idx < total_lines {
          // find [line_start, line_end) + terminator length
          let line_start = i;
          let mut line_end = i;
          let mut term_len = 0usize;

          while line_end < bytes.len() {
            match bytes[line_end] {
              b'\n' => {
                term_len = 1;
                break;
              }
              b'\r' => {
                if line_end + 1 < bytes.len() && bytes[line_end + 1] == b'\n' {
                  term_len = 2;
                } else {
                  term_len = 1;
                }
                break;
              }
              _ => line_end += 1,
            }
          }
          // After the loop, line_end points to the first terminator byte (or end of slice).
          let body = &inner[line_start..line_end];

          // Keep line?
          if line_idx >= keep_start && line_idx < keep_end {
            let is_first_kept = !wrote_any;
            write_line(&mut out, body, common_indent, is_first_kept);
            wrote_any = true;

            // Emit normalized newline between kept lines
            if line_idx + 1 < keep_end {
              out.push('\n');
            }
          }

          // Advance past the terminator
          i = (line_end + term_len).min(inner.len());
          line_idx += 1;
        }

        Cow::Owned(out)
      }
    }
  }
}

// ---- extras that accumulate line-level facts during the sub-lex ----
#[derive(Default, Debug, Clone, Copy)]
struct BlockLineExtras {
  has_cr_terminators: bool,
  leading_blank_lines: usize,
  trailing_blank_lines: usize,
  common_indent: Option<usize>, // min indent across non-blank lines after the first
  saw_nonblank_any: bool,
  saw_body_this_line: bool, // whether we saw a LineBody since last Terminator
  terminators: usize,       // count of seen line terminators

  nonblank_after_first_count: usize, // lines with idx > 0 and non-blank
  nonblank_body_bytes: usize,        // sum of body bytes of all non-blank lines
  middle_blank_body_bytes: usize, // sum of body bytes of blank lines that are NOT leading/trailing
  pending_blank_body_bytes: usize, // body bytes of the current blank run since last non-blank
}

/// Result of computing the normalization plan/capacity.
#[derive(Debug, Clone, Copy)]
struct BlockNormalizationPlan {
  required_capacity: usize,  // exact UTF-8 bytes after normalization
  is_clean: bool,            // normalization would be a no-op
  total_lines: usize,        // extras.terminators + 1
  effective_trailing: usize, // trailing blanks actually trimmed
  common_indent: usize,      // extras.common_indent.unwrap_or(0)
}

/// Compute the required capacity (and related flags) for a block string.
///
/// `content_nonempty`: true iff the inner slice between the delimiters is non-empty  
/// `escaped_triple_count`: number of `\"\"\"` sequences seen by the outer lexer
#[inline]
fn compute_block_normalization_plan(
  extras: &BlockLineExtras,
  content_nonempty: bool,
  escaped_triple_count: usize,
) -> BlockNormalizationPlan {
  let total_lines = extras.terminators + 1;

  // Special case: all-blank block → treat trailing as leading to keep invariants stable.
  let effective_trailing = if !extras.saw_nonblank_any && content_nonempty {
    extras.leading_blank_lines
  } else {
    extras.trailing_blank_lines
  };

  // Lines kept after trimming leading/trailing blank lines.
  let kept_lines = total_lines
    .saturating_sub(extras.leading_blank_lines)
    .saturating_sub(effective_trailing);

  let common_indent = extras.common_indent.unwrap_or(0);

  // Bytes of kept bodies:
  //   = all nonblank bodies
  //   + bodies of blank lines that are in the middle (not leading/trailing)
  let kept_body_bytes = extras.nonblank_body_bytes + extras.middle_blank_body_bytes;

  // Indentation removed from *non-first* nonblank lines.
  let indent_removed = common_indent.saturating_mul(extras.nonblank_after_first_count);

  // Each `\"\"\"` escape becomes `"""` → drop exactly 1 byte (the backslash).
  let drops_from_escaped_triple = escaped_triple_count;

  // Normalized newlines: exactly one '\n' between kept lines.
  let newline_bytes = kept_lines.saturating_sub(1);

  let required_capacity = if kept_lines == 0 {
    0
  } else {
    kept_body_bytes
      .saturating_sub(indent_removed)
      .saturating_sub(drops_from_escaped_triple)
      .saturating_add(newline_bytes)
  };

  // Clean fast-path: nothing would change on normalization.
  let is_clean = escaped_triple_count == 0
    && !extras.has_cr_terminators
    && extras.leading_blank_lines == 0
    && effective_trailing == 0
    && common_indent == 0;

  BlockNormalizationPlan {
    required_capacity,
    is_clean,
    total_lines,
    effective_trailing,
    common_indent,
  }
}
