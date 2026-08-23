use std::{borrow::Cow, string::String};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::utils::human_display::DisplayHuman;

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use self::u8_slice::skip_block_str_from_bytes;
pub(crate) use self::{
  str::{BlockStringToken, lex_block_str_from_str},
  u8_slice::{BlockStringToken as BytesBlockStringToken, lex_block_str_from_bytes},
};

mod str;
mod u8_slice;

use super::LitPlainBlockStr;

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
///
/// # Neither a map key nor an ordering against `str`/`[u8]`
///
/// Both refusals, and the reasoning behind each, are written out on
/// [`LitInlineStr`](super::LitInlineStr) — this type is the other half of the same shape. It is an
/// enum, so its derived `Ord` ranks `Plain` ahead of `Complex` before it compares a byte, while a
/// cross-type impl would answer on the source alone; two relations, and [`PartialOrd`]'s
/// requirements hold across implementations. The pins are the same two, in this type's spelling:
///
/// ```compile_fail,E0308
/// use smear_lexer::LitStr;
///
/// let LitStr::Block(lit) = LitStr::try_from("\"\"\"z\"\"\"").unwrap() else { unreachable!() };
/// let _ = lit < *"\"\"\"m\"\"\"";
/// ```
///
/// ```compile_fail,E0308
/// use smear_lexer::LitStr;
///
/// let LitStr::Block(lit) = LitStr::try_from(b"\"\"\"z\"\"\"".as_slice()).unwrap() else {
///   unreachable!()
/// };
/// let _ = lit < *b"\"\"\"m\"\"\"".as_slice();
/// ```
///
/// Per this repository's convention the error codes are checked only under a nightly
/// `cargo test --doc`; on stable the assertion is that the snippets do not compile at all.
#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitBlockStr<S> {
  /// A clean block string, no escaped triple quotes, no CR/CRLF,
  /// no leading/trailing blank lines, and no common indent.
  Plain(LitPlainBlockStr<S>),

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

impl<S> LitBlockStr<Option<S>> {
  /// a
  #[inline(always)]
  pub fn transpose(self) -> Option<LitBlockStr<S>> {
    match self {
      Self::Plain(s) => s.transpose().map(LitBlockStr::Plain),
      Self::Complex(s) => s.transpose().map(LitBlockStr::Complex),
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
    S: tokora::utils::ToEquivalent<T>,
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
    S: tokora::utils::IntoEquivalent<T>,
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

// The complex carrier only. `LitBlockStr` is an enum, and its derived `Ord` ranks `Plain` ahead of
// `Complex` before it reads a byte, so a source-ordered impl beside it would not be part of the
// same order — see `impl_source_ordering`.
impl_source_ordering!(LitComplexBlockStr::<&'a str>::as_str);
impl_source_ordering!(LitComplexBlockStr::<&'a [u8]>::as_bytes);

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

/// Strips a block literal's `"""` delimiters.
///
/// Total on a slice that does not carry them, for the reason the inline conversion's `inline_body`
/// is: the carriers are `pub(crate)`-constructed and every construction in this crate spans a whole
/// `"""…"""` token, so re-checking the lexer with a panicking index would only add a way for this
/// conversion to abort.
#[inline]
fn block_body(raw: &str) -> &str {
  raw
    .strip_prefix(r#"""""#)
    .and_then(|rest| rest.strip_suffix(r#"""""#))
    .unwrap_or(raw)
}

/// Answers the literal's **value** — draft §2.9.4's `BlockStringValue` — borrowing it whenever the
/// spelling between the delimiters already *is* the value.
///
/// # Value, not spelling
///
/// The same split as [`LitInlineStr`](super::LitInlineStr)'s conversion: `as_str`,
/// [`Deref`](core::ops::Deref), [`AsRef`] and `From<LitBlockStr<&str>> for &str` answer the
/// **source spelling**, `"""` delimiters and all;
/// this one answers the cooked value. `Plain` used to answer the spelling too, which made the two
/// variants disagree about what the conversion was for.
///
/// # The algorithm is §2.9.4's, step for step
///
/// [`Plain`](LitBlockStr::Plain) is the case where the algorithm is the identity — the lexer's
/// `is_clean` — so it is a reslice. [`Complex`](LitBlockStr::Complex) replays the steps from the
/// line facts the lexer already collected: the common indent comes off every line **but the first
/// of the raw split** (step 4, which exempts neither a blank line nor the line that happens to
/// survive steps 5 and 6), leading and trailing blank lines go (steps 5 and 6), each surviving
/// line is joined by a single line feed whatever terminator the source spelled (step 8), and
/// `\"""` — §2.9.5's only escape — becomes `"""` on the way past.
///
/// # `Plain` means the lexer looked, and looked at *this* grammar
///
/// The identity case is a claim about *these* bytes read under *this* algorithm — §2.9.4's, whose
/// `is_clean` says it has nothing to do to them. Both halves have to be the lexer's.
///
/// **The bytes.** A source-replacing conversion falsified that half, which is why this type has no
/// `map` — a `Plain` `"""block"""` remapped to `"""a\n"""` stayed `Plain`, and this conversion
/// returned the trailing line feed that step 5 removes. [`into_equivalent`] is the only
/// representation change left, and its sealed bound keeps the bytes.
///
/// **The algorithm.** Removing `map` did not close the other half, because the carrier does not
/// name a grammar — the variant does, and one kind-agnostic carrier fitted both. An inline
/// literal's carrier re-labelled as a block one is an honest source under an algorithm that never
/// ran on it, and `block_body` finds no `"""` to strip, so the inline quotes stay in the value.
/// [`LitPlainStr`](super::LitPlainStr) carries its kind in its type now:
///
/// ```compile_fail,E0308
/// use smear_lexer::{LitBlockStr, LitInlineStr, LitStr};
///
/// let LitStr::Inline(inline) = LitStr::try_from("\"x\"").unwrap() else { unreachable!() };
/// let LitInlineStr::Plain(carrier) = inline else { unreachable!() };
/// // This used to typecheck, and `Cow::from` of it answered the spelling `"x"` — quotes included
/// // — where the value of the literal the carrier came from is `x`.
/// let forged = LitBlockStr::Plain(carrier);
/// ```
///
/// Per this repository's convention the error code above is checked only under a nightly
/// `cargo test --doc`; on stable the assertion is that the snippet does not compile at all.
///
/// [`into_equivalent`]: LitBlockStr::into_equivalent
impl<'a> From<LitBlockStr<&'a str>> for Cow<'a, str> {
  #[inline]
  fn from(value: LitBlockStr<&'a str>) -> Self {
    match value {
      LitBlockStr::Plain(s) => Cow::Borrowed(block_body(s.as_str())),
      LitBlockStr::Complex(s) => {
        // Inner content between the surrounding delimiters.
        let inner = block_body(s.as_str());

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
        // - dedent unless this is the first line of the RAW split (§2.9.4 step 4 exempts that one
        //   line and nothing else — not a blank line, and not the first line that survives steps 5
        //   and 6, which is a different line whenever the block opens with a terminator),
        // - unescape \"\"\" -> """
        #[inline(always)]
        fn write_line(out: &mut String, line: &str, dedent: usize, is_first_raw_line: bool) {
          let body = if is_first_raw_line {
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
            write_line(&mut out, body, common_indent, line_idx == 0);

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

// extras that accumulate line-level facts during the sub-lex
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
  required_capacity: usize, // UTF-8 bytes after normalization; see the note below
  is_clean: bool,           // normalization would be a no-op
  total_lines: usize,       // extras.terminators + 1
  leading_blank_lines: usize, // leading blanks actually trimmed
  effective_trailing: usize, // trailing blanks actually trimmed
  common_indent: usize,     // extras.common_indent.unwrap_or(0)
}

/// Compute the required capacity (and related flags) for a block string.
///
/// `content_nonempty`: true iff the inner slice between the delimiters is non-empty  
/// `escaped_triple_count`: number of `\"\"\"` sequences seen by the outer lexer
///
/// # The line the sub-lexer cannot announce
///
/// `BlockLineTok` emits a `LineBody` for a line with content and a `Terminator` for each line
/// terminator, so the **empty line a trailing terminator opens** is never announced: nothing
/// follows it to close it. Draft §2.9.4 splits the raw value on terminators, so `"""\na\n"""`
/// is three lines — ``, `a`, `` — and the third is a trailing blank step 6 removes. Counting it
/// here is what makes `total_lines` (`terminators + 1`) and the blank-line counts describe the
/// *same* split; without it `"""a\n"""` came back `is_clean` and cooked to `a\n`.
///
/// The one shape this deliberately leaves alone is empty content, where there is no terminator at
/// all: `content_nonempty` already carries that case, and counting its single blank line would
/// turn `""""""` from a borrow into an allocation of the empty string.
///
/// # `required_capacity` is an allocation hint, and an upper bound
///
/// It is exact except for a *kept blank* line that carries more whitespace than the common indent:
/// the dedent takes that whitespace off and this arithmetic does not model it, because doing so
/// needs each such line's length rather than their sum. Every other term is exact — and one of
/// them only became exact here, since `indent_removed` has always charged for dedenting every
/// non-blank line after the first of the raw split, which is what the writer now actually does.
#[inline]
fn compute_block_normalization_plan(
  extras: &BlockLineExtras,
  content_nonempty: bool,
  escaped_triple_count: usize,
) -> BlockNormalizationPlan {
  let mut extras = *extras;
  if !extras.saw_body_this_line && extras.terminators > 0 {
    if extras.saw_nonblank_any {
      extras.trailing_blank_lines += 1;
    } else {
      extras.leading_blank_lines += 1;
    }
  }
  let extras = &extras;

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
    leading_blank_lines: extras.leading_blank_lines,
    effective_trailing,
    common_indent,
  }
}
