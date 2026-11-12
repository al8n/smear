//! The excerpt through which smear's diagnostics quote a spelling: where it cuts, what it writes
//! for bytes that are not UTF-8, and the most it writes.

use std::{
  format,
  string::{String, ToString},
  vec::Vec,
};

use super::{EXCERPT_END, Excerpt};

/// The most bytes one excerpt writes, as `Excerpt` documents it: 64 of the spelling's bytes before
/// the `…` and 67 after it, each written as at most the three bytes of a U+FFFD, and the `…`'s
/// three.
const BOUND: usize = 396;

/// What an excerpt of `bytes` writes.
fn written(bytes: &[u8]) -> String {
  Excerpt(bytes).to_string()
}

/// Up to `2 * EXCERPT_END` bytes, nothing is cut, so a spelling is written whole, even where a cut
/// would have fallen inside a character.
#[test]
fn a_spelling_of_at_most_128_bytes_is_written_whole() {
  for len in [0, 1, 2 * EXCERPT_END - 1, 2 * EXCERPT_END] {
    let spelling = "x".repeat(len);
    assert_eq!(written(spelling.as_bytes()), spelling, "{len} bytes");
  }
  let accented = "é".repeat(EXCERPT_END);
  assert_eq!(accented.len(), 2 * EXCERPT_END);
  assert_eq!(written(accented.as_bytes()), accented);
}

/// Past `2 * EXCERPT_END` bytes, a spelling is written as its first and its last `EXCERPT_END`
/// bytes around `…`, whatever its length.
#[test]
fn a_longer_spelling_is_written_as_its_two_ends() {
  let head = "h".repeat(EXCERPT_END);
  let tail = "t".repeat(EXCERPT_END);
  for middle in [1, 2, 1 << 12] {
    let spelling = format!("{head}{}{tail}", "m".repeat(middle));
    assert_eq!(
      written(spelling.as_bytes()),
      format!("{head}…{tail}"),
      "{middle} bytes elided"
    );
  }
}

/// A cut that falls inside a character moves back to the character's first byte: the head leaves
/// the character out, and the tail keeps it whole, so neither splits one.
#[test]
fn a_cut_inside_a_character_moves_back_to_its_first_byte() {
  for character in ["é", "€", "😀"] {
    let width = character.len();
    for into in 1..width {
      // The head's cut falls `into` bytes into the character.
      let kept = "a".repeat(EXCERPT_END - into);
      let tail = "z".repeat(EXCERPT_END);
      let spelling = format!("{kept}{character}{}{tail}", "m".repeat(100));
      assert_eq!(
        written(spelling.as_bytes()),
        format!("{kept}…{tail}"),
        "{character:?}, {into} bytes in, at the head"
      );

      // The tail's cut falls `into` bytes into the character.
      let head = "a".repeat(EXCERPT_END);
      let rest = "z".repeat(EXCERPT_END + into - width);
      let spelling = format!("{head}{}{character}{rest}", "m".repeat(100));
      assert_eq!(
        written(spelling.as_bytes()),
        format!("{head}…{character}{rest}"),
        "{character:?}, {into} bytes in, at the tail"
      );
    }
  }
}

/// Continuation bytes alone move each cut back three bytes and no further, and each is written as
/// a U+FFFD of its own.
#[test]
fn continuation_bytes_alone_move_each_cut_back_three_bytes_and_no_further() {
  let spelling = [0x80_u8; 300];
  assert_eq!(
    written(&spelling),
    format!(
      "{}…{}",
      "\u{FFFD}".repeat(EXCERPT_END - 3),
      "\u{FFFD}".repeat(EXCERPT_END + 3)
    )
  );
}

/// Bytes that are not UTF-8 are written as `String::from_utf8_lossy` writes them, and past
/// `2 * EXCERPT_END` bytes each end is decoded on its own.
#[test]
fn bytes_that_are_not_utf8_are_written_as_from_utf8_lossy_writes_them() {
  let short: [&[u8]; 8] = [
    b"\xff",
    b"\xff\xfe",
    b"a\xc3",
    b"\xe2\x82 euro",
    b"\xed\xa0\x80",
    b"\xc0\x80",
    b"\xf4\x90\x80\x80",
    b"caf\xc3\xa9 \xff ok",
  ];
  for bytes in short {
    assert_eq!(written(bytes), String::from_utf8_lossy(bytes), "{bytes:?}");
  }

  // A character cut short where the head ends is one U+FFFD, as at the end of a whole spelling.
  let mut long: Vec<u8> = Vec::new();
  long.extend_from_slice(&[b'a'; EXCERPT_END - 2]);
  long.extend_from_slice(b"\xe2\x82");
  long.extend_from_slice(&[b'm'; 100]);
  long.extend_from_slice(b"\xff");
  long.extend_from_slice(&[b'z'; EXCERPT_END - 1]);
  let tail = long.len() - EXCERPT_END;
  assert_eq!(
    written(&long),
    format!(
      "{}…{}",
      String::from_utf8_lossy(&long[..EXCERPT_END]),
      String::from_utf8_lossy(&long[tail..])
    )
  );
}

/// The bound `Excerpt` documents is reached and never passed. It is reached by a head of 64 bytes
/// kept whole and a tail that moves back over three continuation bytes to keep 67, each of the 131
/// written as a U+FFFD.
#[test]
fn an_excerpt_writes_at_most_396_bytes_and_can_write_that_many() {
  let mut worst = [0xFF_u8; 300];
  let cut = worst.len() - EXCERPT_END;
  worst[cut - 2..=cut].fill(0x80);
  assert_eq!(written(&worst).len(), BOUND);

  for fill in [b'a', 0x80, 0xC3, 0xE2, 0xF0, 0xFF] {
    let buffer = [fill; 3 * EXCERPT_END];
    for len in 0..=buffer.len() {
      let excerpt = written(&buffer[..len]);
      assert!(
        excerpt.len() <= BOUND,
        "{len} bytes of {fill:#04x} write {}",
        excerpt.len()
      );
    }
  }
}
