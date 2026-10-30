//! The UTF-8 walk a byte source's source text is held to (smear#246).
//!
//! Draft §2.1 makes a document a sequence of `SourceCharacter`s, any Unicode scalar value. A text
//! source is UTF-8 by its type; a byte source is not validated wholesale, and is walked only where
//! the grammar reads characters rather than ASCII tokens.

use core::ops::Range;

/// The ill-formed UTF-8 sequences in a block string's content, in order, as
/// content-relative byte ranges.
///
/// The one decision both block paths make on UTF-8:
/// `skip_block_str_from_bytes::<true>` refuses when this yields anything, and
/// `lex_block_str_from_bytes` reports everything it yields. They walk the same
/// content, because both find the close with the same `find_block_close_simd`
/// over the same bytes, so the fast path can refuse no block literal that the
/// Logos path then accepts.
///
/// The walk hops ASCII with `skip_until_or_non_ascii` given no needles, then
/// checks each maximal non-ASCII run with [`core::str::from_utf8`]. ASCII bytes
/// are scalars on their own and no UTF-8 sequence contains one, so only a
/// non-ASCII run can be ill-formed, and a valid run ends on a sequence boundary.
/// Inside a run, [`Utf8Error::valid_up_to`](core::str::Utf8Error::valid_up_to)
/// and [`error_len`](core::str::Utf8Error::error_len) give each ill-formed
/// sequence's maximal subpart — the rest of the run when it ends inside one —
/// and the walk resumes after it, so one run can report several. These are the
/// extents the inline grammar's error tokens have for the same bytes.
pub(crate) struct IllFormedUtf8<'a> {
  content: &'a [u8],
  /// Where the walk resumes.
  pos: usize,
  /// The end of the non-ASCII run being checked; equal to `pos` between runs.
  run_end: usize,
}

impl<'a> IllFormedUtf8<'a> {
  #[inline]
  pub(crate) const fn new(content: &'a [u8]) -> Self {
    Self {
      content,
      pos: 0,
      run_end: 0,
    }
  }
}

impl Iterator for IllFormedUtf8<'_> {
  type Item = Range<usize>;

  #[inline]
  fn next(&mut self) -> Option<Range<usize>> {
    loop {
      if self.pos == self.run_end {
        let start =
          self.pos + memspan::skip::skip_until_or_non_ascii(&self.content[self.pos..], [])?;
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
