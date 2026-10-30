//! The UTF-8 walk a byte source's source text is held to: a string literal's content (smear#246)
//! and a comment's body (smear#251).
//!
//! Draft §2.1 makes a document a sequence of `SourceCharacter`s, any Unicode scalar value. A text
//! source is UTF-8 by its type; a byte source is not validated wholesale, and is walked only where
//! the grammar reads characters rather than ASCII tokens.

use core::ops::Range;

/// The ill-formed UTF-8 sequences in a run of source text, in order, as byte ranges relative to
/// the run's start.
///
/// Two walks share it. [`new`](Self::new) walks its input whole. For a block string's content that
/// is the one decision both block paths make on UTF-8: `skip_block_str_from_bytes::<true>` refuses
/// when this yields anything, and `lex_block_str_from_bytes` reports everything it yields. They walk
/// the same content, because both find the close with the same `find_block_close_simd` over the
/// same bytes, so the fast path can refuse no block literal that the Logos path then accepts. The
/// lossless byte lexers walk a comment's body with it too, once the rule's regex has matched the
/// comment to its line end. `comment` is the syntactic lexers' walk: it finds the line end in the
/// same scan that checks the body. Both comment walks cover the same body and report everything
/// they yield, so the syntactic and the lossless lexer refuse the same comments with the same
/// errors.
///
/// The walk hops ASCII with `skip_until_or_non_ascii`, then checks each maximal non-ASCII run with
/// [`core::str::from_utf8`]. ASCII bytes are scalars on their own and no UTF-8 sequence contains
/// one, so only a non-ASCII run can be ill-formed, and a valid run ends on a sequence boundary.
/// Inside a run, [`Utf8Error::valid_up_to`](core::str::Utf8Error::valid_up_to) and
/// [`error_len`](core::str::Utf8Error::error_len) give each ill-formed sequence's maximal subpart —
/// the rest of the run when it ends inside one — and the walk resumes after it, so one run can
/// report several. These are the extents the inline grammar's error tokens have for the same bytes.
///
/// A block string's walk hops with no needles. A comment's (`COMMENT`) also stops at `\n` and `\r`
/// and ends at the first of them: both are ASCII, so where the line ends does not depend on what
/// the runs before it hold, and the one scan that checks the body is the scan that finds its end.
pub(crate) struct IllFormedUtf8<'a, const COMMENT: bool = false> {
  /// What the walk covers: all of the input, or, once a comment's walk meets its line terminator,
  /// the body before it.
  content: &'a [u8],
  /// Where the walk resumes.
  pos: usize,
  /// The end of the non-ASCII run being checked; equal to `pos` between runs.
  run_end: usize,
}

impl<'a> IllFormedUtf8<'a> {
  /// Walks all of `content`.
  #[inline]
  pub(crate) const fn new(content: &'a [u8]) -> Self {
    Self {
      content,
      pos: 0,
      run_end: 0,
    }
  }
}

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
impl<'a> IllFormedUtf8<'a, true> {
  /// Walks a comment's body for the syntactic lexers, which have not found its end yet. `rest` is
  /// everything after the `#`, and the body is the part of it before the first `\n` or `\r`, or all
  /// of it when there is neither. Ranges are body-relative.
  #[inline]
  pub(crate) const fn comment(rest: &'a [u8]) -> Self {
    Self {
      content: rest,
      pos: 0,
      run_end: 0,
    }
  }

  /// The body's length: where its line terminator is, or the length of `rest` when the comment
  /// runs to the end of the input.
  ///
  /// Read once the walk has returned `None`. The scan that ended the walk found the terminator, so
  /// reading the length scans nothing again.
  #[inline]
  pub(crate) fn body_len(&self) -> usize {
    debug_assert_eq!(
      self.pos,
      self.content.len(),
      "a comment's length was read before its walk ended"
    );
    self.content.len()
  }
}

impl<const COMMENT: bool> Iterator for IllFormedUtf8<'_, COMMENT> {
  type Item = Range<usize>;

  #[inline]
  fn next(&mut self) -> Option<Range<usize>> {
    loop {
      if self.pos == self.run_end {
        let rest = &self.content[self.pos..];
        let found = if COMMENT {
          memspan::skip::skip_until_or_non_ascii(rest, *b"\n\r")
        } else {
          memspan::skip::skip_until_or_non_ascii(rest, [])
        };
        let Some(skipped) = found else {
          // Only ASCII that ends nothing is left, so the walk covers the rest of the input. The
          // position moves there too, so a later call scans nothing.
          self.pos = self.content.len();
          self.run_end = self.pos;
          return None;
        };
        let start = self.pos + skipped;
        if COMMENT && self.content[start].is_ascii() {
          // The line terminator: the body ends before it.
          self.content = &self.content[..start];
          self.pos = start;
          self.run_end = start;
          return None;
        }
        self.pos = start;
        self.run_end = start + memspan::skip::skip_non_ascii(&self.content[start..]);
      }
      match core::str::from_utf8(&self.content[self.pos..self.run_end]) {
        Ok(_) => self.pos = self.run_end,
        Err(e) => {
          let start = self.pos + e.valid_up_to();
          let end = e.error_len().map_or(self.run_end, |len| start + len);
          self.pos = end;
          return Some(start..end);
        }
      }
    }
  }
}
