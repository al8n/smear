use std::{string::String, vec::Vec};

use super::{MAX_DEPTH, Scanner, Text};
use crate::introspection::ResponseErrorKind;

/// A key comes back decoded, and borrowed when the decoding did not have to change anything.
///
/// The two halves are one property: [`Scanner::next_key`] answers *which member is this*, so the
/// escaped spelling and the plain one have to be the same answer — and the plain one, which is
/// every key a real response writes, has to still be the response's own bytes.
#[test]
fn a_key_is_decoded_and_still_borrowed_when_it_can_be() {
  // Assembled rather than written, so nothing between this source and the fixture can normalise
  // the escape into the character it stands for.
  let escaped = std::format!("\\u{}ata", "0064");
  let document = std::format!(r#"{{"{escaped}":1,"plain":2}}"#);
  assert!(document.contains("\\u0064"), "the fixture lost its escape");

  let mut scanner = Scanner::new(&document);
  let mut members = scanner.enter_object("a fixture").expect("an object");
  let mut keys = Vec::new();
  while let Some(key) = scanner.next_key(&mut members).expect("a key") {
    keys.push((String::from(key.as_str()), key.is_borrowed()));
    scanner.skip_value().expect("a value");
  }
  assert_eq!(
    keys,
    [(String::from("data"), false), (String::from("plain"), true)]
  );
}

/// Reads one string literal out of a document that is nothing else.
fn text(document: &str) -> Result<Text<'_>, ResponseErrorKind> {
  let mut scanner = Scanner::new(document);
  let read = scanner.text("a fixture").map_err(|error| error.kind())?;
  scanner.finish().map_err(|error| error.kind())?;
  Ok(read)
}

/// Walks a whole document as an uninterpreted value.
fn walk(document: &str) -> Result<(), ResponseErrorKind> {
  let mut scanner = Scanner::new(document);
  scanner.skip_value().map_err(|error| error.kind())?;
  scanner.finish().map_err(|error| error.kind())
}

#[test]
fn a_literal_with_no_backslash_is_the_response_itself() {
  let document = r#""PUBLIC""#;
  let read = text(document).expect("a string");
  assert!(read.is_borrowed(), "an escape-free literal was copied");
  assert_eq!(read.as_str(), "PUBLIC");

  // Borrowed means borrowed *from this document*, not merely equal to a slice of it.
  let base = document.as_ptr() as usize;
  let at = read.as_str().as_ptr() as usize;
  assert!(
    at > base && at + read.as_str().len() <= base + document.len(),
    "the literal does not point into the document it came from"
  );
}

#[test]
fn a_literal_with_a_backslash_is_expanded_once() {
  let read = text(r#""\"world\"""#).expect("a string");
  assert!(!read.is_borrowed(), "an escaped literal cannot be borrowed");
  assert_eq!(read.as_str(), "\"world\"");
}

#[test]
fn every_escape_the_grammar_defines_is_expanded() {
  for (literal, expected) in [
    (r#""\"""#, "\""),
    (r#""\\""#, "\\"),
    (r#""\/""#, "/"),
    (r#""\b""#, "\u{8}"),
    (r#""\f""#, "\u{c}"),
    (r#""\n""#, "\n"),
    (r#""\r""#, "\r"),
    (r#""\t""#, "\t"),
    (r#""A""#, "A"),
    (r#""aéb""#, "aéb"),
    (r#""head\ttail""#, "head\ttail"),
  ] {
    assert_eq!(
      text(literal).expect("a string").as_str(),
      expected,
      "{literal}"
    );
  }

  // The surrogate pair, which is the one escape that reads four hex digits twice and combines
  // them. Both the fixture and the expectation are assembled rather than written, so that nothing
  // between this source and the test can quietly turn either into the character it stands for —
  // which would leave a row that reads like a pair and exercises none of the pairing.
  let pair = std::format!("\"\\u{}\\u{}\"", "d83d", "de00");
  assert!(
    pair.contains("\\ud83d\\ude00"),
    "the fixture lost its escapes"
  );
  assert_eq!(
    text(&pair).expect("a string").as_str(),
    char::from_u32(0x1_F600)
      .expect("a scalar value")
      .to_string(),
  );
}

/// The raw path validates the same escapes the expanding one does.
///
/// A name never calls the expander — it borrows the literal's own bytes — and neither does the
/// walk over a member the door ignores. So an escape that only the *expander* refuses would be an
/// escape a `description` could carry unchallenged, and "the whole document is proved to be JSON"
/// would be a promise about the members that happen to be read.
#[test]
fn the_raw_path_validates_every_escape_the_expanding_one_does() {
  for literal in [
    r#""\q""#,
    r#""\u12""#,
    r#""\u12g4""#,
    r#""\ud83d""#,
    r#""\ude00""#,
    r#""\ud83dA""#,
    "\"a\nb\"",
  ] {
    assert_eq!(
      walk(literal).expect_err(literal),
      ResponseErrorKind::MalformedJson,
      "{literal}"
    );
  }
}

#[test]
fn a_literal_the_grammar_refuses_is_not_json() {
  for literal in [
    // An unknown escape.
    r#""\q""#,
    // A `\u` with too few digits, and one with a non-hex digit.
    r#""\u12""#,
    r#""\u12g4""#,
    // A raw control character, which JSON forbids in a string.
    "\"a\nb\"",
    "\"a\u{1}b\"",
    // Surrogates that are not a pair.
    r#""\ud83d""#,
    r#""\ud83dx""#,
    r#""\ude00""#,
    r#""\ud83dA""#,
    // No closing quote.
    r#""abc"#,
  ] {
    assert_eq!(
      text(literal).expect_err(literal),
      ResponseErrorKind::MalformedJson,
      "{literal}"
    );
  }
}

#[test]
fn the_number_grammar_is_the_one_json_defines() {
  for accepted in [
    "0", "-0", "1", "-1", "42", "1.5", "-1.5", "1e9", "1E9", "1e+9", "1e-9", "1.5e10", "0.0",
  ] {
    walk(accepted).unwrap_or_else(|kind| panic!("`{accepted}` was refused as {kind:?}"));
  }
  for refused in [
    "01", "-", "1.", ".5", "1e", "1e+", "+1", "1.2.3", "--1", "1..2",
  ] {
    assert_eq!(
      walk(refused).expect_err(refused),
      ResponseErrorKind::MalformedJson,
      "{refused}"
    );
  }
}

/// No number in a draft §4 response is read, only walked — so the reader needs a grammar and never
/// a conversion, and the widest literal a server can print costs it nothing.
#[test]
fn a_number_is_walked_and_never_converted() {
  let enormous = std::format!("{{\"count\":{}e{}}}", "9".repeat(400), "9".repeat(3));
  walk(&enormous).expect("a number the reader never has to represent");
}

#[test]
fn a_document_may_carry_exactly_one_value() {
  walk(r#"{"a":1}"#).expect("one value");
  walk("  [1, 2]  ").expect("whitespace is not a second value");
  assert_eq!(
    walk(r#"{"a":1} {"b":2}"#).expect_err("two values"),
    ResponseErrorKind::MalformedJson
  );
  assert_eq!(
    walk(r#"{"a":1}x"#).expect_err("trailing text"),
    ResponseErrorKind::MalformedJson
  );
}

#[test]
fn a_container_the_grammar_refuses_is_not_json() {
  for refused in [
    "{",
    "[",
    r#"{"a"}"#,
    r#"{"a":}"#,
    r#"{"a":1,}"#,
    "[1,]",
    "[1 2]",
    r#"{"a":1 "b":2}"#,
    // A key that is not a string.
    "{1:2}",
    // The three keywords, misspelled.
    "nul",
    "tru",
    "fals",
  ] {
    assert_eq!(
      walk(refused).expect_err(refused),
      ResponseErrorKind::MalformedJson,
      "{refused}"
    );
  }
}

/// Nesting is bounded, and the bound is what keeps every recursive walk downstream in range.
#[test]
fn nesting_is_bounded() {
  fn nested(depth: usize) -> String {
    let mut document = String::with_capacity(depth * 2 + 1);
    document.extend(std::iter::repeat_n('[', depth));
    document.push('1');
    document.extend(std::iter::repeat_n(']', depth));
    document
  }

  let limit = MAX_DEPTH as usize;
  walk(&nested(limit)).expect("the limit itself is admitted");
  assert_eq!(
    walk(&nested(limit + 1)).expect_err("one deeper is refused"),
    ResponseErrorKind::MalformedJson
  );

  // The refusal is the bound and not the stack: a document deep enough to overflow a recursive
  // walk is refused rather than run.
  assert_eq!(
    walk(&nested(100_000)).expect_err("far deeper is refused"),
    ResponseErrorKind::MalformedJson
  );
}

/// Whichever member of an object the door does not read, it still proves is JSON.
#[test]
fn an_unread_member_is_skipped_and_not_ignored() {
  let mut scanner = Scanner::new(r#"{"errors":[{"message":"boom"}],"data":null}"#);
  let mut members = scanner.enter_object("a fixture").expect("an object");
  let mut keys = Vec::new();
  while let Some(key) = scanner.next_key(&mut members).expect("a key") {
    keys.push(String::from(key.as_str()));
    scanner.skip_value().expect("a value");
  }
  assert_eq!(keys, ["errors", "data"]);
  scanner.finish().expect("nothing after the object");

  // The same document with a defect buried in the member nobody reads: an unterminated string,
  // and an escape no grammar defines.
  assert_eq!(
    walk(r#"{"errors":[{"message":"boom}],"data":null}"#).expect_err("an unterminated string"),
    ResponseErrorKind::MalformedJson
  );
  assert_eq!(
    walk(r#"{"errors":[{"message":"bo\qm"}],"data":null}"#).expect_err("an unknown escape"),
    ResponseErrorKind::MalformedJson
  );
}
