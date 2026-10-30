//! The string-literal contents and comment bodies both dialects' UTF-8 tests drive (smear#246: a
//! string literal in a byte source must be well-formed UTF-8; smear#251: so must a comment).
//!
//! Each [`Case`] is the content of one literal or the body of one comment, and every test holds the
//! lexer's answer for it to what [`core::str::from_utf8`] says about the same bytes. The table is
//! exhaustive where that is cheap — every lone byte from `0x80`, every two-byte sequence — and
//! pointed where it is not: the three- and four-byte boundary classes, truncations, several
//! ill-formed runs in one literal, and stops and runs that land on a 16-, 32- or 64-byte vector
//! seam.
//!
//! Under Miri the exhaustive parts shrink to a fixed sample. The interpreter answers whether a path
//! has undefined behaviour, and the sample still takes every path.

use core::{fmt, ops::Range};

use std::{vec, vec::Vec};

/// Spaces after the closing delimiter of a case that needs a long scan window.
///
/// The fast path scans the rest of the document, not the literal, so this is what puts a stop
/// inside a full 64-byte vector rather than in the scalar tail of a short input.
const LONG_TAIL: usize = 80;

/// The tails of the exhaustive cases: a short input takes the scalar search, and the long tail
/// moves the same stops into a vector. Miri takes the first alone.
const TAILS: [usize; 2] = [0, LONG_TAIL];

/// Valid and ill-formed sequences to place at a seam. The first three are valid.
const PIECES: [&[u8]; 5] = [
  "\u{e9}".as_bytes(),
  "\u{4e2d}".as_bytes(),
  "\u{1f600}".as_bytes(),
  b"\xff",
  b"\xe4\xb8",
];

/// Body lengths around each vector width, and where in the body the first non-ASCII byte goes.
const WINDOWS: [usize; 9] = [15, 16, 17, 31, 32, 33, 63, 64, 65];
const OFFSETS: [usize; 7] = [0, 15, 16, 31, 32, 63, 64];
const MIRI_WINDOWS: [usize; 2] = [16, 33];
const MIRI_OFFSETS: [usize; 4] = [0, 15, 16, 31];

/// The vector widths a valid character is split across.
const SEAMS: [usize; 3] = [16, 32, 64];

/// Under Miri, the two-byte sequences are these firsts against these seconds: the edges of every
/// lead and continuation range, a delimiter, a backslash and a line terminator.
const MIRI_FIRST: [u8; 18] = [
  0x00, 0x22, 0x41, 0x5C, 0x7F, 0x80, 0xBF, 0xC0, 0xC1, 0xC2, 0xDF, 0xE0, 0xED, 0xEF, 0xF0, 0xF4,
  0xF5, 0xFF,
];
const MIRI_SECOND: [u8; 11] = [
  0x00, 0x0A, 0x22, 0x41, 0x5C, 0x80, 0x9F, 0xA0, 0xBF, 0xC0, 0xFF,
];

/// Truncated sequences, each a valid prefix that stops short.
pub(crate) const TRUNCATIONS: [&[u8]; 4] = [b"\xc3", b"\xe4\xb8", b"\xf0\x9f", b"\xf0\x9f\x98"];

/// One string literal's content or comment's body, and the room after it.
pub(crate) struct Case {
  kind: &'static str,
  /// The bytes between the delimiters, or after the `#`.
  pub(crate) body: Vec<u8>,
  /// Spaces after the closing delimiter, or after the line feed that ends a comment.
  tail: usize,
}

impl Case {
  fn new(kind: &'static str, body: Vec<u8>, tail: usize) -> Self {
    Self { kind, body, tail }
  }

  /// The case spelled as an inline literal, `"…"`, with its tail.
  pub(crate) fn inline(&self) -> Vec<u8> {
    self.spelled(b"\"")
  }

  /// The case spelled as a block literal, `"""…"""`, with its tail.
  ///
  /// No body in the table holds `"""` or ends in `\`, so the content of the block literal is the
  /// body, byte for byte.
  pub(crate) fn block(&self) -> Vec<u8> {
    self.spelled(b"\"\"\"")
  }

  /// Whether the body holds no `"`, `\`, CR or LF, so that an inline literal of it can have no
  /// error but a UTF-8 one.
  pub(crate) fn is_plain_text(&self) -> bool {
    !self
      .body
      .iter()
      .any(|&byte| matches!(byte, b'"' | b'\\' | b'\r' | b'\n'))
  }

  /// The case spelled as a comment, `#…`, that a line feed ends, with its tail after the line feed.
  pub(crate) fn comment(&self) -> Vec<u8> {
    let mut doc = self.comment_at_eof();
    doc.push(b'\n');
    doc.resize(doc.len() + self.tail, b' ');
    doc
  }

  /// The case spelled as a comment that the end of the input ends, so with no tail.
  pub(crate) fn comment_at_eof(&self) -> Vec<u8> {
    let mut doc = Vec::with_capacity(2 + self.body.len() + self.tail);
    doc.push(b'#');
    doc.extend_from_slice(&self.body);
    doc
  }

  /// Whether the body holds no CR or LF, so that all of it is one comment's body.
  pub(crate) fn is_comment_text(&self) -> bool {
    !self.body.iter().any(|&byte| matches!(byte, b'\r' | b'\n'))
  }

  fn spelled(&self, delimiter: &[u8]) -> Vec<u8> {
    let mut doc = Vec::with_capacity(2 * delimiter.len() + self.body.len() + self.tail);
    doc.extend_from_slice(delimiter);
    doc.extend_from_slice(&self.body);
    doc.extend_from_slice(delimiter);
    doc.resize(doc.len() + self.tail, b' ');
    doc
  }
}

impl fmt::Display for Case {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{} \"{}\" (tail {})",
      self.kind,
      self.body.escape_ascii(),
      self.tail
    )
  }
}

/// Where `from_utf8` finds each ill-formed sequence in `bytes`, in order: the maximal subpart
/// [`Utf8Error::error_len`](core::str::Utf8Error::error_len) measures, or the rest of `bytes` when
/// they end inside a sequence.
pub(crate) fn ill_formed(bytes: &[u8]) -> Vec<Range<usize>> {
  let mut spans = Vec::new();
  let mut at = 0;
  while let Err(e) = core::str::from_utf8(&bytes[at..]) {
    let start = at + e.valid_up_to();
    let end = e.error_len().map_or(bytes.len(), |len| start + len);
    spans.push(start..end);
    at = end;
  }
  spans
}

/// One `UnknownLexeme` error, projected to what both lexers must agree on for a comment: its span,
/// the byte it carries, and the position it names.
pub(crate) type Unknown = (Range<usize>, u8, usize);

/// What a comment that opens a document must be refused with: one [`Unknown`] per sequence
/// [`ill_formed`] finds in its `body`, each moved past the `#`. Empty when the body is UTF-8.
pub(crate) fn comment_refusals(body: &[u8]) -> Vec<Unknown> {
  ill_formed(body)
    .into_iter()
    .map(|span| {
      (
        1 + span.start..1 + span.end,
        body[span.start],
        1 + span.start,
      )
    })
    .collect()
}

/// The three- and four-byte boundary classes, each beside the valid sequence at its edge.
///
/// Overlong `E0 80..9F`; surrogates `ED A0..BF`; `EF BF BF`, which is U+FFFF and valid; overlong
/// `F0 80..8F`; `F4 90..`, past U+10FFFF; and the leads `F5..FF`, which begin nothing.
pub(crate) fn boundary_classes() -> Vec<Case> {
  fn case(kind: &'static str, sequence: &[u8]) -> Case {
    let mut body = vec![b'a'];
    body.extend_from_slice(sequence);
    body.push(b'b');
    Case::new(kind, body, 0)
  }

  let mut cases = Vec::new();
  for second in 0x80..=0x9Fu8 {
    cases.push(case("overlong E0", &[0xE0, second, 0x80]));
  }
  cases.push(case("lowest three-byte", &[0xE0, 0xA0, 0x80]));
  for second in 0xA0..=0xBFu8 {
    cases.push(case("surrogate", &[0xED, second, 0x80]));
  }
  cases.push(case("below the surrogates", &[0xED, 0x9F, 0xBF]));
  cases.push(case("U+FFFF", &[0xEF, 0xBF, 0xBF]));
  for second in 0x80..=0x8Fu8 {
    cases.push(case("overlong F0", &[0xF0, second, 0x80, 0x80]));
  }
  cases.push(case("lowest four-byte", &[0xF0, 0x90, 0x80, 0x80]));
  for second in 0x90..=0xBFu8 {
    cases.push(case("past U+10FFFF", &[0xF4, second, 0x80, 0x80]));
  }
  cases.push(case("U+10FFFF", &[0xF4, 0x8F, 0xBF, 0xBF]));
  for lead in 0xF5..=0xFFu8 {
    cases.push(case(
      "a lead that begins nothing",
      &[lead, 0x80, 0x80, 0x80],
    ));
  }
  cases
}

/// The whole table, or under Miri a fixed sample of it.
pub(crate) fn table() -> Vec<Case> {
  let miri = cfg!(miri);
  let tails = if miri { &TAILS[..1] } else { &TAILS[..] };
  let mut cases = Vec::new();

  for &tail in tails {
    for byte in 0x80..=0xFFu8 {
      cases.push(Case::new("one byte", vec![b'a', byte, b'b'], tail));
    }
  }

  // Every two-byte sequence, ASCII included: a delimiter, a backslash or a line terminator in
  // either place is still a case the two paths have to agree on, if not one `from_utf8` judges.
  for &tail in tails {
    if miri {
      for first in MIRI_FIRST {
        for second in MIRI_SECOND {
          cases.push(Case::new(
            "two bytes",
            vec![b'a', first, second, b'b'],
            tail,
          ));
        }
      }
    } else {
      for first in 0..=0xFFu8 {
        for second in 0..=0xFFu8 {
          cases.push(Case::new(
            "two bytes",
            vec![b'a', first, second, b'b'],
            tail,
          ));
        }
      }
    }
  }

  cases.extend(boundary_classes());

  for truncation in TRUNCATIONS {
    let mut before_close = vec![b'a'];
    before_close.extend_from_slice(truncation);
    let mut before_backslash = before_close.clone();
    before_backslash.extend_from_slice(b"\\nb");
    cases.push(Case::new("truncated before the close", before_close, 0));
    cases.push(Case::new(
      "truncated before a backslash",
      before_backslash,
      0,
    ));
  }

  cases.push(Case::new(
    "several ill-formed runs",
    b"a\xffb\xfe\xfec\xc3d\xe4\xb8".to_vec(),
    0,
  ));
  cases.push(Case::new(
    "an ill-formed byte inside a valid run",
    b"\xe4\xb8\xad\xff\xe4\xb8\xad".to_vec(),
    0,
  ));

  // The first non-ASCII byte at each seam offset, in each window around a vector width.
  let windows = if miri {
    &MIRI_WINDOWS[..]
  } else {
    &WINDOWS[..]
  };
  let offsets = if miri {
    &MIRI_OFFSETS[..]
  } else {
    &OFFSETS[..]
  };
  for &window in windows {
    for &offset in offsets {
      for piece in PIECES {
        if offset + piece.len() <= window {
          let mut body = vec![b'x'; offset];
          body.extend_from_slice(piece);
          body.resize(window, b'x');
          cases.push(Case::new("at a seam", body, LONG_TAIL));
        }
      }
    }
  }

  // A valid character whose bytes straddle a seam.
  let seams = if miri { &SEAMS[..1] } else { &SEAMS[..] };
  for &seam in seams {
    for piece in &PIECES[..3] {
      for inside in 1..piece.len() {
        let mut body = vec![b'x'; seam - inside];
        body.extend_from_slice(piece);
        body.extend_from_slice(b"xxxx");
        cases.push(Case::new("straddling a seam", body, LONG_TAIL));
      }
    }
  }

  // A non-ASCII run longer than a vector, once valid and once with one byte in it broken.
  let run = "\u{e9}\u{4e2d}\u{1f600}".repeat(8).into_bytes();
  let around = |run: &[u8]| {
    let mut body = b"ab".to_vec();
    body.extend_from_slice(run);
    body.extend_from_slice(b"cd");
    body
  };
  cases.push(Case::new("a long valid run", around(&run), LONG_TAIL));
  for broken in [0usize, 31, 32, 33, 63, 64, 65] {
    let mut run = run.clone();
    run[broken] = 0xFF;
    cases.push(Case::new(
      "a long run with one byte broken",
      around(&run),
      LONG_TAIL,
    ));
  }

  cases
}
