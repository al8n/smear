//! Draft §2.9's static semantics, read off a spelling's bytes.
//!
//! # Where the expectations come from
//!
//! Two places, and neither is this file's own reading of the specification. The block-string cases
//! are draft §2.9.4's worked example and graphql-js's `blockString-test` cases, which an
//! independent Python port of `BlockStringValue()` reproduced byte for byte before any of this was
//! written. And the `Cow` conversions in the modules beside this one run these same walks over a
//! lexed literal, which `string_lexer/tests.rs` holds to `apollo-parser`, a reading that shares no
//! code with this one. [`the_byte_decoder_agrees_with_the_cow_conversions`] then pins that the two
//! doors into the walks, the spelling here and the lexed carrier there, reach the same value.

use core::fmt;
use std::{borrow::Cow, format, string::String, vec::Vec};

use super::{
  super::LitStr, StringValueError, decode_string_value, decode_string_value_to_string,
  string_value_len, string_value_pieces, string_value_pieces_from_str,
};

/// Decodes a spelling, or panics with the refusal.
///
/// Through both decoding doors, which must agree: the generic one, and the `String` one, whose room
/// must be the value's length exactly.
fn decoded(spelling: &str) -> String {
  let mut out = String::new();
  decode_string_value(spelling.as_bytes(), &mut out)
    .unwrap_or_else(|error| panic!("`{spelling}` denotes a value: {error}"));
  let mut reserved = String::new();
  decode_string_value_to_string(spelling.as_bytes(), &mut reserved)
    .unwrap_or_else(|error| panic!("`{spelling}` denotes a value: {error}"));
  assert_eq!(reserved, out, "the two doors disagree on `{spelling}`");
  assert_eq!(
    reserved.capacity(),
    reserved.len(),
    "the `String` door reserved past the value of `{spelling}`"
  );
  out
}

/// The refusal a spelling produces, the same from every door, which writes and reserves nothing.
fn refused(spelling: &[u8]) -> StringValueError {
  let mut out = String::from("kept");
  let error = decode_string_value(spelling, &mut out).expect_err("the spelling denotes no value");
  assert_eq!(out, "kept", "a refusal appended something");
  assert_eq!(string_value_len(spelling), Err(error));
  let mut reserved = String::from("kept");
  assert_eq!(
    decode_string_value_to_string(spelling, &mut reserved),
    Err(error)
  );
  assert_eq!(
    reserved, "kept",
    "a refusal appended something to a `String`"
  );
  assert_eq!(reserved.capacity(), "kept".len(), "a refusal reserved room");
  error
}

/// The cooked value of a literal through the lexer and its `Cow` conversion.
fn cooked(literal: &str) -> Cow<'_, str> {
  match LitStr::try_from(literal) {
    Ok(LitStr::Inline(inline)) => inline.into(),
    Ok(LitStr::Block(block)) => block.into(),
    Err(errors) => panic!("`{literal}` is a literal this lexer accepts: {errors:?}"),
  }
}

#[test]
fn every_inline_escape_is_its_character() {
  assert_eq!(decoded(r#""""#), "");
  assert_eq!(decoded(r#""plain""#), "plain");
  assert_eq!(
    decoded(r#""\" \\ \/ \b \f \n \r \t""#),
    "\" \\ / \u{8} \u{c} \n \r \t"
  );
  // `\u{5c}` is the backslash, so these are the fixed-width `\u` escapes the decoder reads rather
  // than characters the Rust lexer already read.
  assert_eq!(
    decoded("\"\u{5c}u0041\u{5c}u00e9\u{5c}u20AC\""),
    "A\u{e9}\u{20ac}"
  );
  // A surrogate pair is two escapes naming one character.
  assert_eq!(decoded("\"\u{5c}uD83D\u{5c}uDE00\""), "\u{1f600}");
  assert_eq!(decoded("\"\u{5c}ud83d\u{5c}ude00\""), "\u{1f600}");
  // The braced form, at both ends of its range.
  assert_eq!(decoded(r#""\u{1F600}""#), "\u{1f600}");
  assert_eq!(decoded(r#""\u{41}\u{0}""#), "A\u{0}");
  assert_eq!(decoded(r#""\u{10FFFF}""#), "\u{10ffff}");
  // Characters the spelling carries as themselves pass through untouched.
  assert_eq!(decoded("\"caf\u{e9} \u{2603}\""), "caf\u{e9} \u{2603}");
}

/// Draft §2.9.4's worked example, and graphql-js's dedent cases.
#[test]
fn a_block_string_is_block_string_value() {
  const CASES: &[(&str, &str)] = &[
    (
      "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n  \"\"\"",
      "Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    (
      "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\"\"\"",
      "Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    // Trailing spaces on a line are content.
    (
      "\"\"\"\n    Hello,     \n      World!   \n\n    Yours,     \n      GraphQL.   \n\"\"\"",
      "Hello,     \n  World!   \n\nYours,     \n  GraphQL.   ",
    ),
    // Leading and trailing blank lines go, however many.
    (
      "\"\"\"\n\n\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\n\n\"\"\"",
      "Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    // The first line of the raw split keeps its indentation and sets none.
    (
      "\"\"\"    Hello,\n      World!\n\n    Yours,\n      GraphQL.\"\"\"",
      "    Hello,\n  World!\n\nYours,\n  GraphQL.",
    ),
    ("\"\"\"\"\"\"", ""),
    ("\"\"\"hello\"\"\"", "hello"),
    // Nothing but blank lines is the empty string.
    ("\"\"\"   \n\t\n  \"\"\"", ""),
    // A tab is one indentation character, as a space is.
    ("\"\"\"\n\tline1\n\t\tline2\n\"\"\"", "line1\n\tline2"),
    // A blank line shorter than the common indent loses all of it; a longer one keeps the excess.
    ("\"\"\"\n    a\n  \n    b\n\"\"\"", "a\n\nb"),
    ("\"\"\"\n  a\n      \n  b\n\"\"\"", "a\n    \nb"),
    // Every terminator becomes one line feed.
    ("\"\"\"a\r\nb\rc\nd\"\"\"", "a\nb\nc\nd"),
    // `\"""` is a block string's only escape, and a backslash before anything else is a backslash.
    ("\"\"\"x \\\"\"\" y\"\"\"", "x \"\"\" y"),
    (
      "\"\"\"\n  \\\"\"\"quoted\\\"\"\"\n\"\"\"",
      "\"\"\"quoted\"\"\"",
    ),
    ("\"\"\"a \\n b\"\"\"", "a \\n b"),
  ];

  for (spelling, value) in CASES {
    assert_eq!(decoded(spelling), *value, "`{spelling}`");
    assert_eq!(
      string_value_len(spelling.as_bytes()),
      Ok(value.len()),
      "`{spelling}`"
    );
  }
}

/// Every literal the lexer accepts decodes the way its `Cow` conversion cooks it.
///
/// The two run the same walks, so what this pins is the two doors into them. The decoder takes the
/// delimiters off the spelling and refuses a backslash that begins no escape; the conversion takes
/// them off a lexed carrier and copies such a backslash through. On a literal the lexer accepted
/// the two must not differ.
#[test]
fn the_byte_decoder_agrees_with_the_cow_conversions() {
  for literal in CORPUS {
    assert_eq!(decoded(literal), cooked(literal), "`{literal}`");
  }
}

/// Literals the lexer accepts, spelling every escape and every block-string shape: the corpus the
/// doors into the walks are compared over.
const CORPUS: &[&str] = &[
  r#""""#,
  r#""A""#,
  r#""a\"b""#,
  r#""a\\b""#,
  r#""a\/b""#,
  r#""\b\f\n\r\t""#,
  // The fixed-width `\u` escapes, with `\u{5c}` spelling their backslash.
  "\"\u{5c}u0041\"",
  "\"\u{5c}u00e9\"",
  "\"\u{5c}ud83d\u{5c}ude00\"",
  "\"\u{e9}\u{1f600}\"",
  r#""\u{1F600}""#,
  r#""caf\u{e9} \u{1f600}""#,
  "\"caf\u{e9}\"",
  r#""""""""#,
  r#""""plain""""#,
  "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n    \"\"\"",
  "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\"\"\"",
  "\"\"\"\n  a\n    \n  b\n\"\"\"",
  "\"\"\"\n    a\n  \n    b\n\"\"\"",
  "\"\"\"a\n\"\"\"",
  "\"\"\"\na\n\"\"\"",
  "\"\"\"\ncontent\n\n\"\"\"",
  "\"\"\"a\n  b\n    c\n\"\"\"",
  "\"\"\"\n\t\ta\n\t\t\tb\n\"\"\"",
  "\"\"\"  keeps first line indent\n  and dedents this\n\"\"\"",
  "\"\"\"\r\n  a\r\n  b\r\n\"\"\"",
  "\"\"\"a\r\nb\rc\n\"\"\"",
  "\"\"\"\n   \n  \n\"\"\"",
  r#""""a\"""b""""#,
  r#""""a\nb""""#,
];

/// Every piece a door hands out for a spelling, in order, and the door's answer.
type Handed = (Vec<String>, Result<(), StringValueError>);

/// What [`string_value_pieces`] hands out for `spelling`.
fn through_bytes(spelling: &str) -> Handed {
  let mut pieces = Vec::new();
  let answer = string_value_pieces(spelling.as_bytes(), |piece| {
    pieces.push(String::from(piece));
    Ok(())
  });
  (pieces, answer)
}

/// What [`string_value_pieces_from_str`] hands out for `spelling`.
fn through_text(spelling: &str) -> Handed {
  let mut pieces = Vec::new();
  let answer = string_value_pieces_from_str(spelling, |piece| {
    pieces.push(String::from(piece));
    Ok(())
  });
  (pieces, answer)
}

/// The text door hands out what the byte door does: the same pieces in the same order, and the
/// same answer.
///
/// The two doors run one walk and differ only in the UTF-8 check the text door leaves to `&str`.
/// What each does on its own is cut the delimiters off and pick the reading, and a cut one byte
/// out, or the other reading, would hand out other pieces. The refused spellings are compared
/// pieces and all, because a refusal of the spelling comes after the pieces before it.
#[test]
fn the_text_door_hands_out_what_the_byte_door_does() {
  // No delimiters, or a backslash that begins no escape, after the pieces before it.
  const REFUSED: &[&str] = &[
    "",
    "abc",
    "\"",
    "\"\"\"",
    "\"\"\"abc\"",
    r#""ab\q""#,
    r#""a\nb\qc""#,
  ];

  for spelling in CORPUS {
    let (pieces, answer) = through_bytes(spelling);
    assert_eq!(answer, Ok(()), "`{spelling}` denotes a value");
    assert_eq!(through_text(spelling), (pieces, answer), "`{spelling}`");
  }
  for spelling in REFUSED {
    let (pieces, answer) = through_bytes(spelling);
    assert!(answer.is_err(), "`{spelling}` denotes no value");
    assert_eq!(through_text(spelling), (pieces, answer), "`{spelling}`");
  }
}

/// The pieces concatenate to the value, and none of them splits a character.
#[test]
fn a_piece_is_whole_text() {
  for spelling in [
    "\"caf\u{e9}\\n\u{1f600}\\u{1F600}x\"",
    "\"\"\"\n  \u{e9}\u{e9}\n  \\\"\"\"\u{2603}\n\"\"\"",
  ] {
    let mut pieces: Vec<String> = Vec::new();
    string_value_pieces(spelling.as_bytes(), |piece| {
      pieces.push(piece.into());
      Ok(())
    })
    .expect("the spelling denotes a value");
    assert!(pieces.len() > 1, "`{spelling}` came back as one piece");
    assert_eq!(pieces.concat(), decoded(spelling));
  }
}

/// A refused piece stops the walk: the answer is `Sink`, and no piece after the refused one is
/// handed out.
///
/// Counted at the callback, because that is the only place the difference shows. A walk that went
/// on past the refusal, into a callback that ignored the rest, would write nothing more either, and
/// would still decode every escape left in the literal.
#[test]
fn a_refused_piece_stops_the_walk() {
  // A spelling, and how many pieces its value is handed out as.
  const CASES: &[(&str, usize)] = &[
    // `a`, a line feed, `b`, a line feed and `c`, two of them escapes.
    (r#""a\nb\nc""#, 5),
    // `x`, a line feed and `y`, from a block string.
    ("\"\"\"\n  x\n  y\n\"\"\"", 3),
  ];

  for &(spelling, pieces) in CASES {
    let mut handed = 0;
    string_value_pieces(spelling.as_bytes(), |_| {
      handed += 1;
      Ok(())
    })
    .expect("the spelling denotes a value");
    assert_eq!(handed, pieces, "`{spelling}` taken whole");

    for refused in 1..=pieces {
      let mut handed = 0;
      let walked = string_value_pieces(spelling.as_bytes(), |_| {
        handed += 1;
        if handed == refused {
          Err(fmt::Error)
        } else {
          Ok(())
        }
      });
      assert_eq!(
        walked,
        Err(StringValueError::Sink),
        "`{spelling}`, refusing piece {refused}"
      );
      assert_eq!(
        handed, refused,
        "`{spelling}` handed out a piece after piece {refused} was refused"
      );
    }
  }
}

/// A refusal says which of the three it is, and where an escape went wrong.
#[test]
fn a_spelling_with_no_value_is_refused_and_appends_nothing() {
  // `\x5c` is the backslash, spelled so that no escape in the table is read before the decoder
  // reads it.
  const INVALID_ESCAPES: &[(&[u8], usize)] = &[
    (b"\"\x5cx\"", 1),
    (b"\"ab\x5cq\"", 3),
    // Lone surrogates, either half, in either spelling.
    (b"\"\x5cuD800\"", 1),
    (b"\"\x5cuDC00\"", 1),
    (b"\"\x5cuD800\x5cu0041\"", 1),
    (b"\"\x5cu{D800}\"", 1),
    // Past U+10FFFF, no digits, too many digits, too few, and no closing brace.
    (b"\"\x5cu{110000}\"", 1),
    (b"\"\x5cu{}\"", 1),
    (b"\"\x5cu{1234567}\"", 1),
    (b"\"\x5cu12\"", 1),
    (b"\"\x5cu{12\"", 1),
    // A backslash with nothing after it.
    (b"\"\x5c\"", 1),
  ];

  for &(spelling, offset) in INVALID_ESCAPES {
    assert_eq!(
      refused(spelling),
      StringValueError::InvalidEscape(offset),
      "{spelling:?}"
    );
  }

  for spelling in [&b"abc"[..], b"\"", b"\"\"\"", b"\"\"\"abc\"", b""] {
    assert_eq!(
      refused(spelling),
      StringValueError::Unquoted,
      "{spelling:?}"
    );
  }
}

/// Bytes that are no text are refused, whatever lexed them.
///
/// The spellings go to the decoder as bytes, with no lexer in front. The parsers' byte lexer refuses
/// a string literal that is not UTF-8 (smear#246), so no spelling it lexed reaches this refusal; the
/// decoder checks for itself anyway, because it reads bytes rather than relying on any lexer to
/// have.
#[test]
fn a_value_that_is_not_utf8_is_refused() {
  for spelling in [&b"\"\xff\""[..], b"\"\"\"\n  \xc3\x28\n\"\"\""] {
    assert_eq!(refused(spelling), StringValueError::NotUtf8, "{spelling:?}");
  }
}

/// `decode_string_value` appends to what the writer already holds.
#[test]
fn decoding_appends_to_what_is_there() {
  let mut out = String::from("> ");
  decode_string_value(br#""a\u{1F600}b""#, &mut out).expect("a value");
  decode_string_value(b"\"\"\"\n  c\n\"\"\"", &mut out).expect("a value");
  assert_eq!(out, "> a\u{1f600}bc");
}

/// The `String` door reserves the value's exact length before it writes, so a long run and the
/// escape after it land in one allocation.
///
/// This is the shape that doubles a `String` handed to the generic door: the run is written as one
/// piece, the `String` grows to fit it, and the escape's one byte then outgrows that room. Through
/// the `String` door a fresh `String` holds room for the value alone, a scratch `String` with room
/// for the run and not the escape grows to the value's length rather than to twice its room, and a
/// `String` that already holds text grows by the value's length past it.
#[test]
fn the_string_door_reserves_the_value_once_and_exactly() {
  const RUN: usize = 4096;
  let spelling = format!("\"{}\\n\"", "a".repeat(RUN));
  let value = format!("{}\n", "a".repeat(RUN));

  let mut fresh = String::new();
  decode_string_value_to_string(spelling.as_bytes(), &mut fresh).expect("a value");
  assert_eq!(fresh, value);
  assert_eq!(fresh.capacity(), value.len(), "a fresh `String`");

  let mut scratch = String::with_capacity(RUN);
  decode_string_value_to_string(spelling.as_bytes(), &mut scratch).expect("a value");
  assert_eq!(scratch, value);
  assert_eq!(
    scratch.capacity(),
    value.len(),
    "a scratch `String` with room for the run alone"
  );

  let mut held = String::from("> ");
  decode_string_value_to_string(spelling.as_bytes(), &mut held).expect("a value");
  assert_eq!(held, format!("> {value}"));
  assert_eq!(
    held.capacity(),
    held.len(),
    "a `String` that already held text"
  );
}

/// A writer with room for `room` bytes: it refuses the first write that would pass them, and
/// counts every write it is asked for after that one.
struct Cramped {
  written: String,
  room: usize,
  refused: bool,
  asked_after: usize,
}

impl Cramped {
  fn new(room: usize) -> Self {
    Self {
      written: String::new(),
      room,
      refused: false,
      asked_after: 0,
    }
  }
}

impl fmt::Write for Cramped {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    if self.refused {
      self.asked_after += 1;
      return Err(fmt::Error);
    }
    if self.written.len() + s.len() > self.room {
      self.refused = true;
      return Err(fmt::Error);
    }
    self.written.push_str(s);
    Ok(())
  }
}

/// A writer that refuses midway holds a prefix of the value and the answer is `Sink`, and the walk
/// stops there rather than writing on into a writer that failed.
#[test]
fn a_writer_that_refuses_midway_holds_a_prefix_and_answers_sink() {
  // The spelling, the writer's room in bytes, and the prefix it holds after the refusal.
  const CASES: &[(&str, usize, &str)] = &[
    // An inline value's pieces are `ab`, a line feed and `cd`, and the third does not fit.
    (r#""ab\ncd""#, 3, "ab\n"),
    // An escape's character is a piece of its own, and this one needs four bytes.
    (r#""ab\u{1F600}cd""#, 5, "ab"),
    // A block value's pieces are `x`, a line feed and `y`, and the second does not fit.
    ("\"\"\"\n  x\n  y\n\"\"\"", 1, "x"),
    // With no room the first piece is refused, and nothing is written.
    (r#""ab""#, 0, ""),
  ];

  for &(spelling, room, prefix) in CASES {
    let mut out = Cramped::new(room);
    assert_eq!(
      decode_string_value(spelling.as_bytes(), &mut out),
      Err(StringValueError::Sink),
      "`{spelling}` into {room} bytes"
    );
    assert_eq!(out.written, prefix, "`{spelling}` into {room} bytes");
    assert_eq!(
      out.asked_after, 0,
      "`{spelling}` was written on after the refusal"
    );
  }

  // A refusal of the spelling is met before a byte is written, so a writer with no room never
  // hears of it.
  let mut out = Cramped::new(0);
  assert_eq!(
    decode_string_value(b"\"ab\\q\"", &mut out),
    Err(StringValueError::InvalidEscape(3))
  );
  assert!(!out.refused, "a refused spelling was written");
}

/// A spelling that displays as its value, through the `Formatter` it is handed.
struct Displayed(&'static [u8]);

impl fmt::Display for Displayed {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    decode_string_value(self.0, f).map_err(|_| fmt::Error)
  }
}

/// Any `fmt::Write` takes the value: a `String`, a `&mut dyn fmt::Write`, and the `Formatter` a
/// `Display` implementation is handed.
#[test]
fn every_kind_of_writer_takes_the_value() {
  const SPELLING: &[u8] = br#""a\u{1F600}\tb""#;
  const VALUE: &str = "a\u{1f600}\tb";

  let mut string = String::new();
  decode_string_value(SPELLING, &mut string).expect("a value");
  assert_eq!(string, VALUE);

  let mut behind = String::new();
  let dynamic: &mut dyn fmt::Write = &mut behind;
  decode_string_value(SPELLING, dynamic).expect("a value");
  assert_eq!(behind, VALUE);

  assert_eq!(format!("{}", Displayed(SPELLING)), VALUE);
}
