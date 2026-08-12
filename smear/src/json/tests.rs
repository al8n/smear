//! Unit gates over the parts of the writer that are not reachable from `tests/json_writer.rs`:
//! the escaping map, the two string-cooking walks, and the line/column arithmetic.
//!
//! The round-trip gate is the integration target, because its whole point is to read the output
//! back with a parser that shares no code with this module. What is here is the other half — the
//! properties a round trip cannot see, because a round trip through *any* correct reader agrees
//! about `"\u0041"` and `"A"`.

use super::{Error, Json, response::line_column};

/// Writes one value through a closure and returns the bytes.
fn write(f: impl FnOnce(&mut Json<String>) -> Result<(), Error>) -> String {
  let mut json = Json::new(String::new());
  f(&mut json).expect("the sink is a String and the value is writable");
  json.into_inner()
}

/// Cooks one GraphQL string literal — delimiters included — into a JSON string.
fn cook(literal: &str) -> Result<String, Error> {
  let mut json = Json::new(String::new());
  json.graphql_string(literal)?;
  Ok(json.into_inner())
}

#[test]
fn the_escaping_map_is_the_minimum_rfc_8259_requires() {
  assert_eq!(write(|json| json.string("plain")), r#""plain""#);
  assert_eq!(write(|json| json.string("a\"b")), r#""a\"b""#);
  assert_eq!(write(|json| json.string("a\\b")), r#""a\\b""#);
  assert_eq!(
    write(|json| json.string("\u{8}\u{c}\n\r\t")),
    r#""\b\f\n\r\t""#
  );
  assert_eq!(
    write(|json| json.string("\u{0}\u{1}\u{1f}")),
    r#""\u0000\u0001\u001f""#
  );

  // Permitted and deliberately not taken: `\/` is for whatever embeds the response in HTML, and
  // U+2028/U+2029 are legal JSON string content that only a pre-ES2019 JavaScript parser refused.
  assert_eq!(write(|json| json.string("a/b")), r#""a/b""#);
  assert_eq!(
    write(|json| json.string("\u{2028}\u{2029}")),
    "\"\u{2028}\u{2029}\""
  );
}

/// The surrogate answer, as a property rather than as a sentence in a doc comment.
///
/// No `\u` escape is emitted for anything above U+001F, so no surrogate escape — paired or lone —
/// can appear in this writer's output at all. That is what makes the class *absent* rather than
/// *handled*, and it is the writing-side dual of the defect filed against tokora's JSON example as
/// `al8n/tokora#272`, which accepts a lone `"\uD800"` on the reading side.
#[test]
fn nothing_above_the_control_range_is_written_as_an_escape() {
  let sample = (0u32..=0x7f)
    .chain([
      0x80, 0xff, 0x7ff, 0x800, 0x2028, 0x2029, 0xfffd, 0xffff, 0x1_0000, 0x1_f600, 0x10_ffff,
    ])
    .filter_map(char::from_u32);

  for ch in sample {
    let mut buffer = [0u8; 4];
    let written = write(|json| json.string(ch.encode_utf8(&mut buffer)));
    if written.contains("\\u") {
      assert!(
        (ch as u32) < 0x20,
        "U+{:04X} was written with a \\u escape: {written}",
        ch as u32
      );
    }
  }
}

/// A `Display` rendering is escaped as it streams, and correctly across fragment boundaries — the
/// property that a per-fragment escaper has to keep and a buffer-then-escape one gets for free.
#[test]
fn a_display_rendering_is_escaped_fragment_by_fragment() {
  struct Fragments;
  impl core::fmt::Display for Fragments {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      f.write_str("a\"")?;
      f.write_str("\\b")?;
      f.write_str("\n")
    }
  }

  assert_eq!(write(|json| json.display(&Fragments)), r#""a\"\\b\n""#);
}

#[test]
fn an_int_leaf_is_a_number_inside_draft_3_5_1_and_a_string_outside_it() {
  assert_eq!(write(|json| json.int_leaf(0)), "0");
  assert_eq!(write(|json| json.int_leaf(-1)), "-1");
  assert_eq!(
    write(|json| json.int_leaf(i64::from(i32::MAX))),
    "2147483647"
  );
  assert_eq!(
    write(|json| json.int_leaf(i64::from(i32::MIN))),
    "-2147483648"
  );

  // One past each end of draft §3.5.1's `Int`, which is the whole of the rule.
  assert_eq!(
    write(|json| json.int_leaf(i64::from(i32::MAX) + 1)),
    r#""2147483648""#
  );
  assert_eq!(
    write(|json| json.int_leaf(i64::from(i32::MIN) - 1)),
    r#""-2147483649""#
  );
  assert_eq!(
    write(|json| json.int_leaf(i64::MAX)),
    r#""9223372036854775807""#
  );
  assert_eq!(
    write(|json| json.int_leaf(i64::MIN)),
    r#""-9223372036854775808""#
  );

  // `number` is the other door and never applies the rule: a `line`, a `column` and a response
  // path index are counts and are always JSON numbers.
  assert_eq!(
    write(|json| json.number(i64::from(i32::MAX) + 1)),
    "2147483648"
  );
}

#[test]
fn a_non_finite_double_is_refused_rather_than_spelled() {
  let mut json = Json::new(String::new());
  assert_eq!(json.double(f64::NAN), Err(Error::NonFiniteFloat));
  assert_eq!(json.double(f64::INFINITY), Err(Error::NonFiniteFloat));
  assert_eq!(json.double(f64::NEG_INFINITY), Err(Error::NonFiniteFloat));
  assert!(json.double(0.0).is_ok());
}

#[test]
fn an_inline_literal_is_cooked_before_it_is_escaped() {
  assert_eq!(cook(r#""""#).unwrap(), r#""""#);
  assert_eq!(cook(r#""plain""#).unwrap(), r#""plain""#);

  // Draft §2.9.1's simple escapes, each becoming a character and then whatever JSON spells it as.
  assert_eq!(cook(r#""a\"b""#).unwrap(), r#""a\"b""#);
  assert_eq!(cook(r#""a\\b""#).unwrap(), r#""a\\b""#);
  assert_eq!(cook(r#""a\/b""#).unwrap(), r#""a/b""#);
  assert_eq!(cook(r#""\b\f\n\r\t""#).unwrap(), r#""\b\f\n\r\t""#);

  // A fixed-width escape, a surrogate PAIR, and the braced form: all three name one character and
  // all three come out as that character's UTF-8, never as an escape.
  assert_eq!(cook(r#""\u0041""#).unwrap(), r#""A""#);
  assert_eq!(cook(r#""\u00e9""#).unwrap(), "\"\u{e9}\"");
  assert_eq!(cook(r#""\ud83d\ude00""#).unwrap(), "\"\u{1f600}\"");
  assert_eq!(cook(r#""\u{1F600}""#).unwrap(), "\"\u{1f600}\"");
  assert_eq!(cook(r#""\u{41}""#).unwrap(), r#""A""#);

  // An escape whose result needs escaping again on the way out.
  assert_eq!(cook(r#""\u0022""#).unwrap(), r#""\"""#);
  assert_eq!(cook(r#""\u0000""#).unwrap(), r#""\u0000""#);
}

/// The deliberate answer for a lone surrogate, at the one place one can be spelled.
///
/// A `&str` cannot hold a surrogate, so the only way one reaches this writer is as an *escape* in
/// a GraphQL literal — and it is refused rather than emitted or replaced. Emitting `\uD800` would
/// produce exactly what this workspace's own introspection decoder refuses to read
/// (`smear-schema/src/introspection/json.rs`), so the writer would be writing what its sibling
/// reader rejects; substituting U+FFFD would change the response and say nothing.
///
/// Unreachable from a literal this workspace lexed — `smear-lexer` raises `UnpairedSurrogate`
/// before a `StringValue` exists — so this is a total function's honest branch and not a live path.
#[test]
fn a_lone_surrogate_escape_is_refused() {
  assert_eq!(cook(r#""\ud800""#), Err(Error::SurrogateEscape));
  assert_eq!(cook(r#""\udc00""#), Err(Error::SurrogateEscape));
  assert_eq!(cook(r#""\ud800\u0041""#), Err(Error::SurrogateEscape));
  assert_eq!(cook(r#""\ud800\udbff""#), Err(Error::SurrogateEscape));
  assert_eq!(cook(r#""\u{d800}""#), Err(Error::SurrogateEscape));
  assert_eq!(cook(r#""\u{dfff}""#), Err(Error::SurrogateEscape));
}

#[test]
fn a_malformed_escape_is_refused_rather_than_panicked_on() {
  assert_eq!(cook(r#""\q""#), Err(Error::MalformedEscape));
  assert_eq!(cook(r#""\u12""#), Err(Error::MalformedEscape));
  assert_eq!(cook(r#""\u12g4""#), Err(Error::MalformedEscape));
  assert_eq!(cook(r#""\u{}""#), Err(Error::MalformedEscape));
  assert_eq!(cook(r#""\u{110000}""#), Err(Error::MalformedEscape));
  assert_eq!(cook(r#""\u{1234567}""#), Err(Error::MalformedEscape));
  assert_eq!(cook("\"\\\""), Err(Error::MalformedEscape));
}

#[test]
fn a_block_literal_gets_draft_2_9_4() {
  assert_eq!(cook(r#""""""""#).unwrap(), r#""""#);
  assert_eq!(cook(r#""""plain""""#).unwrap(), r#""plain""#);

  // The specification's own worked shape: the first line is not dedented, the common indent of
  // every later non-blank line goes, and leading and trailing blank lines are dropped.
  assert_eq!(
    cook("\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n    \"\"\"").unwrap(),
    r#""Hello,\n  World!\n\nYours,\n  GraphQL.""#
  );

  // A tab counts as one indentation character, exactly as a space does.
  assert_eq!(
    cook("\"\"\"\n\t\ta\n\t\t\tb\n\"\"\"").unwrap(),
    r#""a\n\tb""#
  );

  // Carriage returns are line terminators too, and CRLF is one of them rather than two.
  assert_eq!(cook("\"\"\"a\r\nb\rc\n\"\"\"").unwrap(), r#""a\nb\nc""#);

  // A block string with nothing but blank lines is the empty string.
  assert_eq!(cook("\"\"\"\n   \n  \n\"\"\"").unwrap(), r#""""#);

  // `\"""` is the only escape a block string has, and a lone backslash is a backslash.
  assert_eq!(cook(r#""""a\"""b""""#).unwrap(), r#""a\"\"\"b""#);
  assert_eq!(cook(r#""""a\nb""""#).unwrap(), r#""a\\nb""#);
}

/// The second implementation, and it belongs to somebody else.
///
/// `smear-lexer` cooks its own literals through `Cow` conversions on its literal types, written
/// long before this module and with no knowledge of it. Agreeing with it on a corpus is a real
/// check on this walk, so this is a differential and not a restatement.
///
/// **Inline literals only**, and the reason is three measured findings about the oracle rather
/// than a narrower ambition. None of them is repaired here: they are `smear-lexer`'s, no caller
/// anywhere in this workspace reaches either conversion, and a writer is not where another crate's
/// public API moves. Together they are why
/// [`Json::graphql_string`](super::Json::graphql_string) cooks the literal itself instead of
/// calling the conversion — routing through it would have inherited all three.
///
/// 1. **It panics on a braced escape.** `normalize_str_to_string` has no arm for `\u{...}` and its
///    `read_hex4` panics on the `{`, while `handle_braced_escape_unicode` in the lexer beside it
///    accepts the escape. Measured: `Cow::from` on the literal `"\u{1F600}"` panics with
///    `invalid hex digit in \u escape`, so the oracle cannot be asked about a legal GraphQL string.
/// 2. **`Plain` and `Complex` disagree about what they return.** `Complex` returns the cooked
///    value; `Plain` returns `Cow::Borrowed(s.as_str())`, and a `LitPlainStr` holds the literal
///    *with its delimiters* — so `Cow::from` of the literal `""` is the two-character string `""`.
///    The delimiters come off below, which is what the other branch already does inside itself.
/// 3. **Its block-string dedent is not draft §2.9.4's.** `write_line` skips the dedent for the
///    first *kept* line, where the specification's step 4 skips only the first line of the raw
///    split and drops blank leading lines afterwards, and it does not dedent a blank line at all.
///    Measured on the specification's own worked example, it returns a first line of
///    `    Hello,` where §2.9.4 and `graphql-js`'s `dedentBlockStringLines` both return `Hello,`.
///    Block strings are checked against the specification's examples in the test above and against
///    a third-party parser in `tests/json_writer.rs` instead of against this.
#[test]
fn the_lexers_own_inline_cooking_agrees() {
  use smear_lexer::{LitInlineStr, LitStr};
  use std::borrow::Cow;

  const CORPUS: &[&str] = &[
    r#""""#,
    r#""plain""#,
    r#""a\"b""#,
    r#""a\\b""#,
    r#""a\/b""#,
    r#""\b\f\n\r\t""#,
    r#""\u0041""#,
    r#""\u00e9""#,
    r#""\ud83d\ude00""#,
    r#""tab\there""#,
    r#"" ""#,
    "\"caf\u{e9} \u{1f600}\"",
  ];

  for literal in CORPUS {
    let lit: LitStr<&str> = (*literal)
      .try_into()
      .unwrap_or_else(|_| panic!("`{literal}` is a literal the lexer accepts"));
    let theirs: Cow<'_, str> = match lit {
      LitStr::Inline(LitInlineStr::Plain(plain)) => {
        let raw = plain.as_str();
        Cow::Borrowed(&raw[1..raw.len() - 1])
      }
      LitStr::Inline(inline) => inline.into(),
      LitStr::Block(_) => unreachable!("the corpus is inline literals"),
    };

    // Theirs is the cooked value; ours is that value as a JSON string. Comparing them means
    // writing theirs as a JSON string too — through `Json::string`, which does no cooking at all,
    // so the only thing under test is the cooking.
    let expected = write(|json| json.string(&theirs));
    assert_eq!(
      cook(literal).unwrap(),
      expected,
      "disagreed with the lexer's own cooking of `{literal}`"
    );
  }
}

#[test]
fn a_position_is_one_based_and_counts_draft_2_1_1s_three_terminators() {
  assert_eq!(line_column("abc", 0), (1, 1));
  assert_eq!(line_column("abc", 2), (1, 3));

  assert_eq!(line_column("a\nb", 2), (2, 1));
  assert_eq!(line_column("a\rb", 2), (2, 1));
  // CRLF is ONE terminator, so `b` is line 2 and not line 3.
  assert_eq!(line_column("a\r\nb", 3), (2, 1));

  // The column counts characters, so a two-byte character advances it by one.
  assert_eq!(line_column("\u{e9}x", 3), (1, 3));

  // Total on a clamped or interior offset rather than panicking.
  assert_eq!(line_column("abc", 99), (1, 4));
  assert_eq!(line_column("\u{e9}", 1), (1, 1));
}

/// The existing `graphql-js` differential in `tests/proto_nonnull_oracle.rs` carries its own
/// line/column, validated against the reference implementation's numbers. It counts bytes and
/// knows only `\n`, which is exactly right for its corpus, and this walk must agree with it
/// everywhere that corpus lives — ASCII text with line-feed terminators.
#[test]
fn it_agrees_with_the_graphql_js_differentials_own_counter_on_ascii() {
  fn reference(source: &str, offset: usize) -> (i64, i64) {
    let mut line = 1;
    let mut column = 1;
    for (index, byte) in source.bytes().enumerate() {
      if index == offset {
        break;
      }
      if byte == b'\n' {
        line += 1;
        column = 1;
      } else {
        column += 1;
      }
    }
    (line, column)
  }

  const DOCUMENT: &str = "query Q {\n  a {\n    b\n  }\n  c\n}\n";
  for offset in 0..=DOCUMENT.len() {
    assert_eq!(
      line_column(DOCUMENT, offset),
      reference(DOCUMENT, offset),
      "disagreed at offset {offset}"
    );
  }
}
