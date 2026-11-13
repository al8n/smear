//! The excerpt through which smear's diagnostics quote a spelling: where it cuts, what it writes
//! for bytes that are not UTF-8, and the most it writes. And the reporters: which of them notes
//! where errors point, and how they count the syntax errors a parser did not lend.

use std::{
  format,
  string::{String, ToString},
  vec::Vec,
};

use tokora::{
  SimpleSpan,
  diagnostic::{Code, Location, Severity},
};

use super::{Diagnostic, EXCERPT_END, Excerpt, Reporter};

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

/// Past `2 * EXCERPT_END` bytes, a spelling of one-byte characters is written as its first and its
/// last `EXCERPT_END` bytes around `…`, whatever its length.
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

/// `before` bytes of `a`, then `character`, then `after` bytes of `z`.
fn around(before: usize, character: &str, after: usize) -> String {
  format!("{}{character}{}", "a".repeat(before), "z".repeat(after))
}

/// Asserts that each spelling of `character` between `a`s and `z`s that `places` lists, as
/// `(before, after)`, is written whole and with no `…`: one character holds both cuts there, so
/// nothing lies between them to elide. And that one byte past the longest of them, with
/// `EXCERPT_END` bytes on each side of the character, the cuts no longer meet and the character is
/// elided.
fn written_whole_where_the_cuts_meet(character: &str, places: &[(usize, usize)]) {
  for &(before, after) in places {
    let spelling = around(before, character, after);
    assert_eq!(
      written(spelling.as_bytes()),
      spelling,
      "{} bytes, {before} of them before {character:?}",
      spelling.len()
    );
  }
  let past = around(EXCERPT_END, character, EXCERPT_END);
  assert_eq!(
    written(past.as_bytes()),
    format!("{}…{}", "a".repeat(EXCERPT_END), "z".repeat(EXCERPT_END)),
    "{} bytes",
    past.len()
  );
}

/// A four-byte character holds both cuts in six places, in spellings of 129, 130 and 131 bytes;
/// the first is 62 `a`s, the character and 63 `z`s.
#[test]
fn a_four_byte_character_holding_both_cuts_is_written_whole() {
  written_whole_where_the_cuts_meet(
    "😀",
    &[(62, 63), (63, 62), (64, 61), (63, 63), (64, 62), (64, 63)],
  );
}

/// A three-byte character holds both cuts in three places, in spellings of 129 and 130 bytes.
#[test]
fn a_three_byte_character_holding_both_cuts_is_written_whole() {
  written_whole_where_the_cuts_meet("€", &[(63, 63), (64, 62), (64, 63)]);
}

/// A two-byte character holds both cuts in one place, in a spelling of 129 bytes.
#[test]
fn a_two_byte_character_holding_both_cuts_is_written_whole() {
  written_whole_where_the_cuts_meet("é", &[(64, 63)]);
}

/// Where the two cuts meet in bytes that are not UTF-8, the spelling is written whole too, each
/// byte as a U+FFFD: 131 of them, 393 bytes, the most a spelling written whole can take.
#[test]
fn where_the_cuts_meet_in_bytes_that_are_not_utf8_the_spelling_is_written_whole() {
  let mut spelling = [0xFF_u8; 2 * EXCERPT_END + 3];
  spelling[EXCERPT_END + 1..=EXCERPT_END + 3].fill(0x80);
  let whole = written(&spelling);
  assert_eq!(whole, "\u{FFFD}".repeat(2 * EXCERPT_END + 3));
  assert_eq!(whole.len(), 393);
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

/// The code the reporters' test reports under.
const CODE: Code = Code::new("smear::graphqlx::test-mark");

/// An error at `at`.
fn error(at: Location) -> Diagnostic {
  Diagnostic::builder(CODE, at).build()
}

/// A reporter made for `analyze`'s own run notes where each error points, in the order they were
/// reported, whether the ceiling keeps them or not, and never where a warning points. One made for
/// a consumer's walk notes none, however many errors it is handed.
#[test]
fn the_analysis_reporter_notes_each_error_and_a_consumer_walk_reporter_notes_none() {
  let first = Location::new(4, SimpleSpan::new(2, 5));
  let second = Location::entire(6);
  let warning = Diagnostic::builder(CODE, Location::entire(5))
    .severity(Severity::Warning)
    .build();

  let mut marking = Reporter::new(1);
  let mut consumer = Reporter::without_marks(1);
  for reporter in [&mut marking, &mut consumer] {
    reporter.report(error(first));
    reporter.report(warning.clone());
    reporter.report(error(second));
  }

  assert_eq!(marking.take_marks(), [first, second]);
  assert!(marking.take_marks().is_empty(), "the marks are taken once");
  marking.report(error(first));
  let (diagnostics, marks) = marking.finish();
  assert_eq!(marks, [first]);
  assert_eq!(diagnostics.error_count(), 3);

  assert!(consumer.take_marks().is_empty());
  let (diagnostics, marks) = consumer.finish();
  assert!(marks.is_empty());
  assert_eq!(diagnostics.error_count(), 2);
  assert_eq!(diagnostics.len(), 1);
  assert!(diagnostics.truncated());
}

/// A parser's syntax error is handed to the list as any error is, counted and kept, and notes no
/// mark, whichever reporter it goes through; an error a phase reports beside it is still noted.
#[test]
fn a_syntax_error_is_counted_and_notes_no_mark() {
  let syntax = Location::new(4, SimpleSpan::new(2, 5));
  let phase = Location::entire(6);

  let mut marking = Reporter::new(8);
  marking.report_syntax(error(syntax));
  assert!(
    marking.take_marks().is_empty(),
    "a syntax error marks nothing"
  );
  marking.report_syntax(error(syntax));
  marking.report(error(phase));
  let (diagnostics, marks) = marking.finish();
  assert_eq!(marks, [phase]);
  assert_eq!(diagnostics.error_count(), 3);
  assert_eq!(diagnostics.len(), 3);
  assert!(!diagnostics.truncated());

  let mut consumer = Reporter::without_marks(8);
  consumer.report_syntax(error(syntax));
  let (diagnostics, marks) = consumer.finish();
  assert!(marks.is_empty());
  assert_eq!(diagnostics.error_count(), 1);
}

/// The syntax errors a parser found and did not lend are counted, have no diagnostic, so they
/// truncate the list, and note no mark, whichever reporter counts them; counting none changes
/// nothing.
#[test]
fn unlent_errors_are_counted_and_truncate_the_list() {
  let syntax = Location::new(4, SimpleSpan::new(2, 5));

  let mut marking = Reporter::new(8);
  marking.report_syntax(error(syntax));
  marking.count_unlent(0);
  let (diagnostics, marks) = marking.finish();
  assert!(marks.is_empty());
  assert_eq!(diagnostics.error_count(), 1);
  assert_eq!(diagnostics.len(), 1);
  assert!(
    !diagnostics.truncated(),
    "counting no unlent error truncated the list"
  );

  let mut marking = Reporter::new(8);
  marking.report_syntax(error(syntax));
  marking.count_unlent(3);
  assert!(
    marking.take_marks().is_empty(),
    "an unlent error noted a mark"
  );
  let (diagnostics, marks) = marking.finish();
  assert!(marks.is_empty());
  assert_eq!(diagnostics.error_count(), 4);
  assert_eq!(diagnostics.len(), 1);
  assert!(diagnostics.truncated());
  assert!(diagnostics.has_errors());

  let mut consumer = Reporter::without_marks(8);
  consumer.count_unlent(2);
  let (diagnostics, marks) = consumer.finish();
  assert!(marks.is_empty());
  assert!(diagnostics.is_empty());
  assert!(diagnostics.truncated());
  assert_eq!(diagnostics.error_count(), 2);
  assert!(diagnostics.has_errors());
}
