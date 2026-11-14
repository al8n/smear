//! A spelling copied as UTF-8: as it is when it is UTF-8, and each sequence of it that is not one
//! U+FFFD otherwise, as `String::from_utf8_lossy` writes it, each offset at a token's edge moved to
//! where that byte is in the copy.

use std::{string::String, vec::Vec};

use super::{copy, offset};

/// The copy of `written`, and what it grew by.
fn copied(written: &[u8]) -> (String, Vec<(u32, u32)>) {
  let mut out = String::new();
  let mut growths: Vec<(u32, u32)> = Vec::new();
  copy(written, &mut out, &mut growths);
  (out, growths)
}

/// `at` as the dense offset the spelling's functions take.
fn dense(at: usize) -> u32 {
  u32::try_from(at).expect("a short spelling")
}

/// Each offset of `written` a node's span can start or end at: one with no byte before it or an
/// ASCII byte, and no byte at it or an ASCII byte, so that no sequence of several bytes crosses it.
fn edges(written: &[u8]) -> Vec<usize> {
  (0..=written.len())
    .filter(|&at: &usize| {
      (at == 0 || written[at - 1].is_ascii()) && written.get(at).is_none_or(u8::is_ascii)
    })
    .collect()
}

/// Asserts that `written`'s copy is what `String::from_utf8_lossy` writes, and that each run of
/// `written` between two of its edges is, in the copy, between where [`offset`] moves its ends, and
/// reads as that run copied alone.
fn runs_agree(written: &[u8]) {
  let (out, growths) = copied(written);
  assert_eq!(out, String::from_utf8_lossy(written));
  let edges = edges(written);
  for &from in &edges {
    for &to in edges.iter().filter(|&&to: &&usize| to >= from) {
      let start = offset(&growths, dense(from)) as usize;
      let end = offset(&growths, dense(to)) as usize;
      assert_eq!(
        &out[start..end],
        &*String::from_utf8_lossy(&written[from..to]),
        "the run {from}..{to}"
      );
    }
  }
}

/// A spelling that is UTF-8, non-ASCII characters among it, is copied as it is, and every offset
/// is where it was. Refuses a copy that grows where nothing is replaced.
#[test]
fn a_spelling_that_is_utf8_is_copied_as_it_is() {
  let written = "[\"caf\u{e9}\", \"\u{1f600}\", 1]";
  let (out, growths) = copied(written.as_bytes());
  assert_eq!(out, written);
  assert!(growths.is_empty());
  for at in 0..=dense(written.len()) {
    assert_eq!(offset(&growths, at), at);
  }
  runs_agree(written.as_bytes());
}

/// Each sequence that is not UTF-8 is one U+FFFD, three bytes, and the copy grows there by three
/// less the sequence's length. In `["a\xFFb", "\xE2\x82c", "\xC0\xAFd", "\xF0\x9F\x98"]`:
/// `\xFF` begins nothing, one byte, so the copy grows by 2 up to offset 4; `\xE2\x82` is a
/// three-byte sequence cut short at two, so by 1 more, 3, up to 11; `\xC0` is no lead byte, and
/// `\xAF` a continuation with none, each one byte, so by 2 up to 17 and by 2 more, 7, up to 18;
/// and `\xF0\x9F\x98` is a four-byte sequence cut short at three, so by nothing, 7 still, up to
/// 26. Every run between two token edges reads as that run copied alone. Refuses offsets left
/// where they were in a copy that grew, and a growth counted by bytes rather than by sequences.
#[test]
fn each_sequence_that_is_not_utf8_moves_what_follows_it() {
  let written: &[u8] = b"[\"a\xffb\", \"\xe2\x82c\", \"\xc0\xafd\", \"\xf0\x9f\x98\"]";
  let (out, growths) = copied(written);
  assert_eq!(out, String::from_utf8_lossy(written));
  assert_eq!(growths, [(4, 2), (11, 3), (17, 5), (18, 7), (26, 7)]);
  // The last string's quotes, at 22 and 26, are each 7 bytes later in the copy: the four sequences
  // before the first grew it by 2 + 1 + 2 + 2, and the one between them by nothing.
  assert_eq!(offset(&growths, 22), 29);
  assert_eq!(offset(&growths, 26), 33);
  assert_eq!(&out[29..34], "\"\u{fffd}\"");
  runs_agree(written);
}
