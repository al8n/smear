//! Unit gates over the parts of the writer that are not reachable from `tests/json_writer.rs`:
//! the escaping map, the two string-cooking walks, the line/column arithmetic, and the three costs
//! a remote client chooses.
//!
//! **The cost gates are here rather than in the integration target for one reason**: the
//! instrument is the library's own. `response::visited` and `response::allocations` are
//! `#[cfg(test)]` and so are invisible across a crate boundary, and the gate has to drive
//! `write_response` itself — one that called the resolver directly could not see a writer that
//! built a fresh index per error, which is the defect one edit away from the one being fixed.
//!
//! The round-trip gate is the integration target, because its whole point is to read the output
//! back with a parser that shares no code with this module. What is here is the other half — the
//! properties a round trip cannot see, because a round trip through *any* correct reader agrees
//! about `"\u0041"` and `"A"`.

use super::{Error, Json, response::Locations};

/// Draft §7.1.2's line and column for one offset, through the door the writer uses.
fn line_column(document: &str, offset: usize) -> (i64, i64) {
  let mut locations = Locations::new(document);
  locations
    .resolve(offset)
    .expect("the index for a fixture-sized document fits")
}

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

/// The index answers what the prefix walk answered, at every offset of every shape it was written
/// for.
///
/// **The oracle is the implementation this replaced**, verbatim, and that is the point rather than
/// a shortcut: the change it is gating is about *cost* and is supposed to change no position
/// anywhere, so the property is equality with the old walk and the strongest oracle for it is the
/// old walk. What the numbers *should* be is settled independently, one test up, against the
/// reference implementation's own counter.
///
/// The corpus is chosen over the axes the two implementations could disagree on rather than over
/// cases: each of the three terminators; a CRLF pair a caller could point *between*; **a CRLF pair
/// the checkpoint stride falls inside**, which is the one state the index cannot recover from a
/// checkpoint alone and so the sharpest case in the file; a `\r` on a checkpoint; multi-byte
/// characters before and after a break; a line longer than the stride, so the column is counted
/// across one; a document exactly a stride long; and a document of nothing but terminators.
#[test]
fn the_index_agrees_with_the_prefix_walk_it_replaced() {
  /// Draft §7.1.2's line and column, derived by walking the prefix — the pre-index implementation.
  fn reference(document: &str, offset: usize) -> (i64, i64) {
    let mut end = offset.min(document.len());
    while !document.is_char_boundary(end) {
      end -= 1;
    }

    let mut line = 1i64;
    let mut column = 1i64;
    let mut chars = document[..end].chars();
    while let Some(ch) = chars.next() {
      match ch {
        '\n' => {
          line += 1;
          column = 1;
        }
        '\r' => {
          if chars.clone().next() == Some('\n') {
            chars.next();
          }
          line += 1;
          column = 1;
        }
        _ => column += 1,
      }
    }
    (line, column)
  }

  let long_line = "x".repeat(700);
  let stride_exact = "y".repeat(256);
  let mixed = format!(
    "{}\u{e9}{}\n\u{1f600}tail",
    "a".repeat(300),
    "b".repeat(300)
  );
  // The line feed of a CRLF lands exactly on the second checkpoint, and then exactly after it.
  let split_crlf = format!("{}\r\n{}", "a".repeat(255), "b".repeat(300));
  let crlf_on_checkpoint = format!("{}\r\n{}", "a".repeat(256), "b".repeat(300));
  let corpus: &[&str] = &[
    "",
    "a",
    "\n",
    "\r",
    "\r\n",
    "a\r\nb",
    "a\rb\nc\r\nd",
    "\r\r\n\n\r",
    "\u{e9}\n\u{1f600}\r\nx",
    "query Q {\n  a { b }\n}\n",
    "one\r\ntwo\r\nthree\r\n",
    &long_line,
    &stride_exact,
    &mixed,
    &split_crlf,
    &crlf_on_checkpoint,
    &"\n".repeat(600),
    &"\r\n".repeat(400),
  ];

  for document in corpus {
    let mut locations = Locations::new(document);
    // One `Locations` for the whole document, walked forwards and then backwards, because the
    // index is reused across a response's errors and nothing orders their offsets.
    let offsets = (0..=document.len() + 2).chain((0..=document.len() + 2).rev());
    for offset in offsets {
      assert_eq!(
        locations.resolve(offset).expect("the corpus is small"),
        reference(document, offset),
        "disagreed at offset {offset} of {document:?}"
      );
    }
  }
}

// ---------------------------------------------------------------------------------------------
// what the writer COSTS, on the three axes a remote client chooses
// ---------------------------------------------------------------------------------------------
//
// ── WHY THREE INSTRUMENTS, AND WHY THE FIRST TWO ARE NOT ALLOCATION PINS ──────────────────────
//
// The first two properties are cost claims about work a hostile input can multiply, and neither of
// them allocates the thing it spends. Deriving a line and column by walking the prefix allocates
// nothing at all — it spends CPU over the document — and recursing over the response spends the
// NATIVE STACK, which no allocator sees and which does not fail: it aborts the process. So the
// instruments are the quantities the properties are stated in, one each: document bytes looked
// at, and native stack bytes consumed.
//
// The byte counter is `smear-schema`'s introspection reader's, reached for rather than reinvented
// — that module carries the argument at length and learned it by writing an allocation gate first
// and discovering it was structurally blind to the walk it was written for. The shape here is its
// MARGINAL form for the same reason: a bound on the total would pass a writer that walked the
// document a fixed forty times, and what has to be bounded is what one more error costs.
//
// ── AND THEN THE THIRD, WHICH IS THE ONE THE SECOND CREATED ───────────────────────────────────
//
// Taking the depth off the native stack put it on the heap, and the native-stack probe cannot see
// what happens to it there: that gate passes, unchanged and at the same numbers, on a frame stack
// that doubles its way to 64 MiB and calls the allocation-error handler on refusal. A repair is
// where a cost MOVED to, so an instrument written for the old resource is by construction the one
// that cannot price the new one. That is why the allocation counter is here, why it is beside the
// other two rather than instead of either, and why `json::response`'s header answers "which is the
// next buffer" with a table instead of leaving it to a fifth round.

use core::fmt;

use graphql_proto::{Executor, Leaf, Response, Values};
use smear_compiler::Schema;
use smear_lexer::{
  limits::SyntacticLimits,
  tokora::{Parse as _, Parser},
};
use smear_parser::graphql::{
  GraphQL,
  ast::{ExecutableDocument, TypeSystemDocument},
  error::GraphqlErrors,
  syntactic::{GraphqlLexer, executable_document, type_system_document},
};

use super::{
  response::{allocations, allow_allocations, refuse_allocations_of_at_least, visited},
  write_response_with,
};

/// The smallest value space an execution can have: one object, and one integer.
///
/// Deliberately not the materialised value tree the integration target drives. These gates are
/// about how much work the WRITER does per error and per level, so the driver is the one that adds
/// nothing to either — and this way the two cost gates need no feature the module itself does not.
#[derive(Clone)]
enum Cell {
  Object,
  Int(i64),
}

/// A driver that answers every structural question the shallowest way it can.
struct Space;

impl Values for Space {
  type Value = Cell;

  fn is_null(&self, _: &Cell) -> bool {
    false
  }

  fn as_bool(&self, _: &Cell) -> Option<bool> {
    None
  }

  fn list_len(&self, _: &Cell) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &Cell, _: usize) -> Cell {
    Cell::Object
  }

  fn type_name<'a>(&'a self, _: &'a Cell) -> Option<&'a str> {
    None
  }

  /// The value is already its serialised form, so coercion is the identity.
  fn coerce_leaf(&mut self, value: Cell, _: Leaf<'_>) -> Option<Cell> {
    Some(value)
  }

  fn variable(&mut self, _: &str) -> Option<Cell> {
    None
  }
}

/// Parses one document with an explicit nesting ceiling.
///
/// The ceiling is the lexer's, and it is passed rather than defaulted because a chain of response
/// objects is a chain of `{` in the document: it is the limit that decides how deep a response the
/// rest of the stack will admit, so a gate about depth has to be the thing that sets it.
fn parse_executable(query: &str, nesting: usize) -> Result<ExecutableDocument<&str>, ()> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str_with_state(query, SyntacticLimits::with_max_nesting_depth(nesting))
  .map_err(|_| ())
}

/// Runs one operation to completion and hands the finished response to `take`.
///
/// `resolve` answers one field by name: `Some` is a value, and `None` is a resolver that failed —
/// which is what puts a draft §7.1.2 error, and so a location, in the response.
fn run<T>(
  sdl: &str,
  query: &str,
  nesting: usize,
  mut resolve: impl FnMut(&str) -> Option<Cell>,
  take: impl FnOnce(&Response<'_, Cell>) -> T,
) -> T {
  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str_with_state(sdl, SyntacticLimits::with_max_nesting_depth(nesting))
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("the SDL is a schema");
  let document = parse_executable(query, nesting).expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Cell::Object)
    .expect("the operation resolves");

  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = resolve(request.name());
    match answer {
      Some(value) => executor.handle_resolved(&mut space, id, value),
      None => executor.handle_field_error(id, "the resolver is degraded"),
    }
    while executor.poll_abandoned().is_some() {}
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  take(&response)
}

// ── axis one: the document, once per response rather than once per error ─────────────────────

/// The schema the location gates execute against: one nullable leaf, selectable as often as a
/// client likes.
const FLAT_SDL: &str = "type Query { f: Int }";

/// `k` uniquely aliased selections of one field, behind `pad` bytes of comment.
///
/// **Accepted, and it is the shape a hostile client would send**: every alias is a legal response
/// key, so nothing about the query is refusable, and every one of them can be failed by a
/// dependency the service does not control. The offsets increase, which is the ordering the
/// quadratic walk was worst on.
fn aliased(k: usize, pad: usize) -> String {
  let mut query = String::new();
  if pad > 0 {
    query.push('#');
    query.extend(core::iter::repeat_n('p', pad));
    query.push('\n');
  }
  query.push('{');
  for alias in 0..k {
    query.push_str(&format!(" a{alias}: f"));
  }
  query.push('}');
  query
}

/// Document bytes the writer looked at while writing one response, with every field failed.
fn visits(query: &str) -> u64 {
  run(
    FLAT_SDL,
    query,
    64,
    |_| None,
    |response| {
      assert_eq!(
        response.error_count(),
        response.errors().count(),
        "the fixture is not the shape these gates assume"
      );
      let before = visited();
      let mut out = String::new();
      write_response_with(&mut out, response, query, |_, json| json.null()).expect("writable");
      visited() - before
    },
  )
}

/// How many document bytes one more failed field may cost the writer.
///
/// A constant, and that is the whole claim: the cost of one more error is a function of the
/// index's stride and not of the document the error is in. Measured on the tree that introduced
/// this gate: 136, 132, 138, 138 across the four intervals below — flat. Measured for the defect
/// it was written against, the same walk restored: 129, 553, 2 449, 10 486. That plant *passes*
/// the first interval and fails the second, which is why the assertion is per interval and not on
/// a total: growing with k is the quadratic, and no single threshold on a sum can say so.
const BYTES_PER_LOCATION: u64 = 512;

/// Each additional error costs the writer a bounded look at the document, not a walk of it.
#[test]
fn each_location_costs_a_constant_number_of_bytes() {
  let mut previous = (8usize, visits(&aliased(8, 0)));
  for k in [32usize, 128, 512, 2048] {
    let cost = visits(&aliased(k, 0));
    let looks = (cost - previous.1) / (k - previous.0) as u64;
    assert!(
      looks <= BYTES_PER_LOCATION,
      "up to k={k}, each added error cost {looks} document bytes; the writer is deriving a \
       position by walking the document"
    );
    previous = (k, cost);
  }
}

/// The document is indexed once for the whole response, not once per error.
///
/// The sharper half of the same property, and the one an index rebuilt inside the loop would fail
/// while still passing the marginal gate above: with the error count held fixed, growing the
/// document must cost what reading it once costs, and nothing per error.
#[test]
fn the_document_is_read_once_however_many_locations_are_resolved() {
  const K: usize = 512;
  const PAD: usize = 16 * 1024;

  let plain = visits(&aliased(K, 0));
  let padded = visits(&aliased(K, PAD));
  let added = padded - plain;
  assert!(
    added <= 4 * PAD as u64,
    "{PAD} bytes of comment added {added} byte visits across {K} errors; a document read once \
     costs its own length and a document read per error costs {}",
    PAD * K
  );
}

/// The counter is wired to the writer, and it moves by what the writer reads.
///
/// Without this, both gates above could pass because nothing was being counted.
#[test]
fn the_byte_gate_counts() {
  // Every position in the response is derived from the document, so writing one cannot look at
  // fewer bytes than the document has.
  let query = aliased(64, 0);
  let looked = visits(&query);
  assert!(
    looked >= query.len() as u64,
    "writing {} bytes of document registered as {looked} byte visits",
    query.len()
  );

  // And a response with nothing to locate does not read the document at all, which is the other
  // half of "built on the first location": a successful response pays nothing for the index.
  let ok = run(
    FLAT_SDL,
    "{ f }",
    64,
    |_| Some(Cell::Int(1)),
    |response| {
      assert_eq!(response.error_count(), 0);
      let before = visited();
      let mut out = String::new();
      write_response_with(&mut out, response, "{ f }", |value, json| match value {
        Cell::Int(number) => json.number(*number),
        Cell::Object => json.null(),
      })
      .expect("writable");
      assert_eq!(out, r#"{"data":{"f":1}}"#);
      visited() - before
    },
  );
  assert_eq!(ok, 0, "a response with no locations read the document");
}

// ── axis two: the response's depth, on the heap rather than on the native stack ───────────────

/// The nesting ceiling these gates configure, which is what decides how deep a response can be.
///
/// A chain of response objects is a chain of `{` in the document, so the lexer's nesting limit is
/// the binding one — `max_response_slots` admits 2²⁰ positions and the merge budget is set
/// independently. 2048 is a *raised* ceiling: `smear-lexer`'s own default is 500, and the finding
/// this gate exists for is about a deployment that raises it, so the gate raises it.
const NESTING: usize = 2048;

/// The deepest chain of objects [`NESTING`] admits, and one level more than it admits.
///
/// Derived rather than picked. `DEEPEST` is asserted below to be exactly the ceiling — one level
/// further is refused before execution — so the gate is about a bound and not about a number that
/// happens to work today.
const DEEPEST: usize = NESTING - 1;

/// A schema whose one object type refers to itself, and the query that walks it `depth` deep.
fn chain(depth: usize) -> (String, String) {
  let sdl = "type Query { n: Node } type Node { n: Node x: Int }".to_string();
  let mut query = String::from("{");
  for _ in 0..depth {
    query.push_str(" n {");
  }
  query.push_str(" x ");
  for _ in 0..depth {
    query.push('}');
  }
  query.push('}');
  (sdl, query)
}

/// Writes a response `depth` objects deep and returns the native stack bytes the writer used.
///
/// The probe is a local in the leaf callback, which is the deepest point of the walk, measured
/// against a local in this frame. A walk that recurses puts one frame per level between them; a
/// walk on an explicit stack puts the same constant there whatever the depth is, which is the
/// property and is why the gate compares two depths rather than checking one against a threshold.
fn stack_used(depth: usize) -> usize {
  let (sdl, query) = chain(depth);
  run(
    &sdl,
    &query,
    NESTING,
    |name| {
      Some(if name == "x" {
        Cell::Int(1)
      } else {
        Cell::Object
      })
    },
    |response| {
      let anchor = 0u8;
      let base = core::ptr::addr_of!(anchor) as usize;
      let mut deepest = base;
      let mut out = String::new();

      write_response_with(&mut out, response, &query, |value, json| {
        let probe = 0u8;
        deepest = deepest.min(core::ptr::addr_of!(probe) as usize);
        match value {
          Cell::Int(number) => json.number(*number),
          Cell::Object => json.null(),
        }
      })
      .expect("a response the limits admit is writable");

      // The response really is as deep as the fixture claims: the envelope, the root of `data`,
      // and one object per level. Without this the measurement could be of a walk that never
      // descended.
      assert_eq!(
        out.matches('{').count(),
        depth + 2,
        "the response is not {depth} objects deep, so the stack measurement is of something else"
      );
      assert_eq!(response.error_count(), 0, "{out}");

      base.saturating_sub(deepest)
    },
  )
}

/// Runs `f` on a thread with room for the PARSER's own recursion.
///
/// Not part of the property. Reading a document 2048 selection sets deep is itself a recursive
/// descent — that is the parser's bound, enforced by the very limiter this gate configures — and a
/// gate about the writer must not be a gate about whichever stack the harness happened to give the
/// parse. The writer's own use is measured inside, and is a difference of two addresses, so the
/// size of this stack cannot flatter it.
fn with_room<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
  std::thread::Builder::new()
    .stack_size(256 * 1024 * 1024)
    .spawn(f)
    .expect("a thread")
    .join()
    .expect("the deep fixture did not abort")
}

/// A response as deep as the configured limits admit serialises, and the native stack it costs
/// does not grow with the depth.
///
/// **The depth is the ceiling, not a number that works.** One level beyond it is refused before
/// execution, which is asserted here so that "as deep as the limits admit" is a fact about the
/// fixture rather than a claim about it.
#[test]
fn the_response_depth_does_not_reach_the_native_stack() {
  // The measurement is a difference, so what is left in it is the constant part of the writer's
  // own frame — which cancels — plus whatever the platform adds between two calls at the same
  // depth. A walk that recursed would put thousands of times this in it.
  const SLACK: usize = 1024;

  let (shallow, deep) = with_room(|| (stack_used(DEEPEST / 2), stack_used(DEEPEST)));

  // The probe measured something, which is what stops the comparison below from being two zeroes
  // agreeing. Measured on the tree that introduced this gate: 3 152 bytes in debug and 1 848 in
  // release, the same at both depths; with the recursion restored, 1 559 120 and 3 115 600.
  assert!(
    shallow > 0,
    "the writer used no measurable native stack at all, so the growth below is not a measurement"
  );

  let growth = deep.saturating_sub(shallow);
  let levels = DEEPEST - DEEPEST / 2;
  assert!(
    growth <= SLACK,
    "{levels} more levels of response cost {growth} more bytes of native stack ({} per level); \
     the walk is on the native stack again",
    growth / levels.max(1)
  );

  // And the fixture is at the ceiling: one level further is not a deeper response, it is a
  // refused document.
  let (_, past) = chain(DEEPEST + 1);
  assert!(
    with_room(move || parse_executable(&past, NESTING).is_err()),
    "a document one level deeper than the ceiling parsed, so {DEEPEST} is not the deepest \
     response the configured limits admit"
  );
}

// ── axis three: the frames themselves, one allocation rather than a doubling sequence ─────────

/// A sink that allocates nothing, so every allocating event the counter sees is the writer's.
///
/// A `String` would double its way up as the response streamed into it and would be most of the
/// reading — the *caller's* buffer measured under the writer's name. What this keeps instead is the
/// two facts the fixture has to be checked against before its cost means anything: how many bytes
/// came out, and how many containers were opened.
#[derive(Default)]
struct Void {
  bytes: usize,
  braces: usize,
}

impl fmt::Write for Void {
  fn write_str(&mut self, text: &str) -> fmt::Result {
    self.bytes += text.len();
    self.braces += text.bytes().filter(|&byte| byte == b'{').count();
    Ok(())
  }
}

/// What one write of a `depth`-deep response cost, and what it was a write of.
struct Written {
  /// Allocating events the write caused on this thread.
  allocated: u64,
  /// The depth the writer sized its frame stack from.
  recorded: usize,
  /// Containers the output actually opened.
  containers: usize,
  /// Bytes the output actually had.
  bytes: usize,
}

/// Writes a response `depth` objects deep, with nothing failed, and counts what it allocated.
///
/// Nothing failed **on purpose**: an error would build the checkpoint index and the path buffer
/// too, and the reading would then be the sum of three buffers rather than the one this gate is
/// about. `a_whole_response_is_one_allocation_per_buffer` is the other fixture, where all three are
/// live at once and the total is still a constant.
fn written(depth: usize) -> Written {
  let (sdl, query) = chain(depth);
  run(
    &sdl,
    &query,
    NESTING,
    |name| {
      Some(if name == "x" {
        Cell::Int(1)
      } else {
        Cell::Object
      })
    },
    |response| {
      let mut sink = Void::default();
      let recorded = response.depth();
      let before = allocations();
      write_response_with(&mut sink, response, &query, |value, json| match value {
        Cell::Int(number) => json.number(*number),
        Cell::Object => json.null(),
      })
      .expect("a response the limits admit is writable");
      let allocated = allocations() - before;
      assert_eq!(response.error_count(), 0, "the fixture failed a resolver");
      Written {
        allocated,
        recorded,
        containers: sink.braces,
        bytes: sink.bytes,
      }
    },
  )
}

/// The shallower of the two depths the allocation curve is read at.
const SHALLOW: usize = 64;

/// The deeper one, sixteen times further down.
///
/// Sixteen times, deliberately, and the number to separate is a **logarithm**: an unhinted `Vec`
/// of 64-byte frames doubles from a capacity of four, so 65 frames cost it six allocations and
/// 1 025 cost it ten. Two readings four apart cannot be mistaken for one constant, and no slack a
/// logarithmic bound could carry would separate them. It is `graphql-proto`'s own pair for its own
/// buffer, kept the same so that the two gates' numbers can be read side by side.
const DEEP: usize = 1024;

/// How many times writing a response's `data` may go to the allocator.
///
/// **A constant, and it has to be a constant**, for the reason `graphql-proto`'s
/// `ALLOCATIONS_PER_DERIVATION` gives about the buffer one layer down: the natural bound here is
/// `log₂(depth)`, that is exactly what the defect costs, and a gate shaped like the defect's own
/// curve is a gate the defect passes.
///
/// Measured on this tree: **1** at 65 frames and **1** at 1 025. Measured with the reservation
/// removed and the `push` left exactly as it was: **6** and **10**, while the native-stack reading
/// stayed at its constant and the document-byte readings did not move at all — three axes, and only
/// this one can see it.
///
/// `<=` rather than `==` for the reason the layout assertions in `graphql-proto` use it: a future
/// `Vec` that satisfied the request without going to the allocator would be an improvement, and a
/// gate that failed a green tree for one is a gate nobody keeps. [`the_allocation_gate_counts`] is
/// what stops `0` from being an instrument that is not attached.
const ALLOCATIONS_PER_WRITE: u64 = 1;

/// Writing `data` costs one allocation, however deep the response is.
///
/// # The axis the other two gates in this file are blind to
///
/// `the_response_depth_does_not_reach_the_native_stack` was written for this exact walk and passes,
/// at the same numbers, on a frame stack that reallocates and copies its way to 64 MiB with two
/// buffers live at the peak — because taking a cost off the native stack is what makes an allocator
/// the only instrument that can still see it. The byte gates never look at the frames at all. So
/// the reading below is the third axis, and the plant for it was checked against both of the
/// others: they do not move.
#[test]
fn the_frame_stack_is_one_allocation_however_deep_the_response() {
  let (shallow, deep) = with_room(|| (written(SHALLOW), written(DEEP)));

  for (levels, run) in [(SHALLOW, shallow), (DEEP, deep)] {
    // The fixture really is as deep as it claims, and the reservation really is sized from that.
    // Without the first the cost below is a measurement of a walk that stopped early; without the
    // second it is a measurement of a constant that happens to be big enough for both readings.
    assert_eq!(
      run.containers,
      levels + 2,
      "a {levels}-level chain wrote {} containers in {} bytes, so the measurement is of a \
       different response",
      run.containers,
      run.bytes
    );
    assert_eq!(
      run.recorded,
      levels + 1,
      "a {levels}-level chain reported a response depth of {}, so the frame stack was not sized \
       from this response",
      run.recorded
    );

    assert!(
      run.allocated <= ALLOCATIONS_PER_WRITE,
      "writing a response {levels} objects deep cost {} allocating events; the frame stack is \
       growing into the walk instead of being reserved before it",
      run.allocated
    );
  }
}

/// How many times writing a whole response — `data`, `errors`, `locations` and `path` — may go to
/// the allocator.
///
/// **One per buffer, and the number IS the census.** `json::response`'s header enumerates three
/// growable buffers on this path and claims there is no fourth; this is that claim as an integer.
/// A buffer added later, or one of these three reserved inside a loop instead of before it, moves
/// this number whether or not anybody remembers to update the table.
///
/// Measured on this tree: **3** on every fixture below — the checkpoint index, the path buffer's
/// first fill, and `data`'s frames. Measured with each of the three reservations removed in turn,
/// on the fixture that can see it: **12** on the deep one for the frames, **12** on the deep one
/// for the path buffer, **8** at 2 048 errors for the index. Each plant leaves at least one of
/// these fixtures at 3, which is why there is more than one of them.
const ALLOCATIONS_PER_RESPONSE: u64 = 3;

/// A whole response with errors costs one allocation per buffer, and there are three of them.
///
/// # Two fixtures, because one of them can only see one buffer
///
/// The flat one fails *k* aliases of one field, so it is the reading that moves if a buffer is
/// grown **per error** — and it is blind to two of the three, because its response is one level
/// deep and every path in it is one segment long, so neither the frames nor the path buffer ever
/// has to grow. Measured: removing either reservation leaves this fixture at 3.
///
/// The deep one fails the innermost leaf of a chain, so it is one error at the far end of a long
/// path in a deeply nested `data` — the reading that moves if a buffer is grown **per level**, on
/// either of the two that are sized by a depth. Neither fixture subsumes the other, which is the
/// same lesson as the three counters and is why both are here.
#[test]
fn a_whole_response_is_one_allocation_per_buffer() {
  for k in [8usize, 2048] {
    let query = aliased(k, 0);
    let allocated = run(
      FLAT_SDL,
      &query,
      64,
      |_| None,
      |response| {
        assert_eq!(
          response.error_count(),
          k,
          "the fixture failed a different number of fields"
        );
        let mut sink = Void::default();
        let before = allocations();
        write_response_with(&mut sink, response, &query, |_, json| json.null()).expect("writable");
        allocations() - before
      },
    );
    assert!(
      allocated <= ALLOCATIONS_PER_RESPONSE,
      "a response with {k} errors cost {allocated} allocating events; one of the writer's buffers \
       is being grown per error rather than reserved per response"
    );
  }

  let (levels, allocated) = with_room(|| (DEEP, one_deep_error(DEEP)));
  assert!(
    allocated <= ALLOCATIONS_PER_RESPONSE,
    "a response {levels} objects deep with one error at the bottom of it cost {allocated} \
     allocating events; one of the writer's buffers is being grown per level rather than reserved \
     per response"
  );
}

/// Allocating events one write costs on a `depth`-deep response whose innermost leaf failed.
///
/// The one error is at the bottom, so its path is as long as the response is deep and every one of
/// the three buffers is live at once: the index over the document, the path buffer, and the frames.
/// `x: Int` is nullable, so draft §6.4.4 nulls the leaf and propagates no further — the response
/// keeps its full depth, which is what makes this a reading about levels.
fn one_deep_error(depth: usize) -> u64 {
  let (sdl, query) = chain(depth);
  run(
    &sdl,
    &query,
    NESTING,
    |name| (name != "x").then_some(Cell::Object),
    |response| {
      assert_eq!(
        response.error_count(),
        1,
        "the deep fixture did not fail exactly its innermost leaf"
      );
      assert_eq!(
        response.depth(),
        depth + 1,
        "the deep fixture is not {depth} levels deep"
      );
      let mut sink = Void::default();
      let before = allocations();
      write_response_with(&mut sink, response, &query, |value, json| match value {
        Cell::Int(number) => json.number(*number),
        Cell::Object => json.null(),
      })
      .expect("writable");
      allocations() - before
    },
  )
}

/// The allocation counter is wired to the writer, and it moves by what the writer does.
///
/// Without this, both gates above could pass because nothing was being counted — and a zero from an
/// allocation pin looks exactly like the property holding, which is the failure mode an
/// allocation-shaped gate has and a byte-shaped one does not.
#[test]
fn the_allocation_gate_counts() {
  let (shallow, into_string) = with_room(|| (written(4), into_a_string(SHALLOW)));

  // A stack of frames cannot be filled from an empty `Vec` without the allocator being asked once,
  // so a reading of zero here is the counting allocator not being the one this binary runs on.
  assert!(
    shallow.allocated >= 1,
    "writing a response {} containers deep registered no allocating event",
    shallow.containers
  );

  // And the sink is not the thing being measured: the same response into a `String`, which doubles
  // as it fills, costs strictly more. Without this the reading above could be a counter that sees
  // one allocation whatever happens.
  assert!(
    into_string > ALLOCATIONS_PER_WRITE,
    "writing into a growing `String` cost {into_string} allocating events, the same as writing \
     into a sink that allocates nothing; the counter is not seeing the allocator"
  );
}

/// Allocating events the same write costs into a `String`, which grows as it fills.
fn into_a_string(depth: usize) -> u64 {
  let (sdl, query) = chain(depth);
  run(
    &sdl,
    &query,
    NESTING,
    |name| {
      Some(if name == "x" {
        Cell::Int(1)
      } else {
        Cell::Object
      })
    },
    |response| {
      let mut out = String::new();
      let before = allocations();
      write_response_with(&mut out, response, &query, |value, json| match value {
        Cell::Int(number) => json.number(*number),
        Cell::Object => json.null(),
      })
      .expect("writable");
      allocations() - before
    },
  )
}

// ---------------------------------------------------------------------------------------------
// axis four: a VALUE's depth, which no ceiling in this stack has ever counted
// ---------------------------------------------------------------------------------------------
//
// ── WHY A FOURTH SECTION, WHEN THE THIRD SAID THE LIST WAS CLOSED ─────────────────────────────
//
// It was closed over the wrong extent. The table in `json::response`'s header enumerated every
// buffer THAT FILE grows and called the enumeration exhaustive; the defect that survived it grew
// the NATIVE STACK, in a SIBLING MODULE, on a subject the response's own depth does not describe.
// An axis list is only as good as the set of functions it is crossed with, and the census was
// {heap} × {json::response} while the defect was at {native stack} × {json::materialized}.
//
// So the enumeration this file's gates now stand for is the product, and it is in `super`'s header
// where both modules can be named. What is added here is the pair of readings the product's fourth
// cell needs: a value's depth on the native stack, and a value's depth on the heap.
//
// ── AND WHY THE ALLOCATION READING IS NOT THE ONE THAT SEES THE DEFECT ────────────────────────
//
// Said plainly so that nobody reads it as the gate for the recursion: the recursion allocated
// NOTHING. Its whole cost was native frames. The allocation reading below is a gate on the
// structure that REPLACED it — the frames must grow geometrically and not once per level — which is
// the same relationship the third axis has to the second, one subject over. The reading that fails
// on the recursion is the stack probe; the gate that fails LOUDLY is the small-stack one below,
// which does not assert anything at all — it aborts, the way the defect did.
#[cfg(feature = "materialized-numbers")]
mod value_depth {
  use core::fmt;

  use smear_lexer::tokora::{Parse as _, Parser, span::AsSpan};
  use smear_parser::graphql::{
    GraphQL,
    ast::materialized as tree,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, value::materialized::const_value},
  };

  use super::{
    super::{
      Error, Json, WriteJson,
      response::{allocations, allow_allocations, refuse_allocations_of_at_least},
    },
    Void,
  };

  /// The value tree these fixtures are built at.
  ///
  /// **The width is arbitrary here and named once so that it reads that way.** What this module
  /// measures is the *walk* — native frames, heap frames, what happens when the allocator says no
  /// — and a walk over a list of lists never reaches an `Int` leaf at all. Either width would give
  /// the same numbers; `i64` is the one the response writer's string branch is reachable at, so it
  /// is the less trivial of the two to have chosen.
  type ConstInputValue<S> = tree::ConstInputValue<S, i64>;
  type ConstList<S> = tree::ConstList<S, i64>;

  /// One value, parsed from the GraphQL literal that spells it.
  fn leaf(literal: &'static str) -> ConstInputValue<&'static str> {
    Parser::with_parser::<
      GraphqlLexer<'_, str>,
      ConstInputValue<&'static str>,
      GraphqlErrors<&'static str>,
      _,
      GraphQL,
    >(const_value::<_, _, i64>)
    .parse_str(literal)
    .expect("the fixture is a constant GraphQL value")
  }

  /// A value `depth` lists deep with a `null` at the bottom.
  ///
  /// **Built by wrapping and not by parsing**, and that is the finding rather than a convenience:
  /// the parser has a nesting ceiling and is itself a recursive descent, so a literal this deep
  /// would be refused before it could be built. `ConstList::new` is public, the variant is public,
  /// and a driver's value does not come from this crate's parser at all — it comes from whatever
  /// the driver resolved. Nothing between that constructor and the sink has ever looked at how
  /// deep the result is.
  pub(super) fn deep(depth: usize) -> ConstInputValue<&'static str> {
    let mut value = leaf("null");
    let span = *value.as_span();
    for _ in 0..depth {
      value = ConstInputValue::List(ConstList::new(span, std::vec![value]));
    }
    value
  }

  /// Takes a value apart one level at a time and drops the pieces.
  ///
  /// # This is a workaround for a defect this change does not fix, and it is here on purpose
  ///
  /// `ConstInputValue`'s derived `Drop` glue recurses, so letting one of these fixtures fall out of
  /// scope aborts the process at a depth well below the ones measured here — **whether or not the
  /// writer was ever called**. Measured on this tree: a value 7 734 lists deep dropped and 7 773
  /// aborted, with nothing in `json` on the stack.
  ///
  /// So the repair below bounds the WRITER and does not make a deep value safe to hold. That is a
  /// property of the AST type rather than of this module, it wants its own change and its own
  /// review, and until it has one every fixture here has to dismantle what it built.
  pub(super) fn dismantle(mut value: ConstInputValue<&'static str>) {
    while let ConstInputValue::List(list) = value {
      match list.into_values().pop() {
        Some(inner) => value = inner,
        None => return,
      }
    }
  }

  /// A sink that records how far down the native stack the writer was when it wrote.
  ///
  /// The probe is a local in the sink, which is reached at every level of the walk, measured
  /// against a local in the caller's frame — the same difference-of-two-addresses shape as the
  /// response walk's probe, with the leaf callback replaced by the one thing a value walk always
  /// calls.
  #[derive(Default)]
  struct Deepest {
    /// The lowest address a `write_str` has seen, or `None` before the first one.
    deepest: Option<usize>,
    bytes: usize,
  }

  impl fmt::Write for Deepest {
    fn write_str(&mut self, text: &str) -> fmt::Result {
      let probe = 0u8;
      let here = core::ptr::addr_of!(probe) as usize;
      self.deepest = Some(self.deepest.map_or(here, |lowest| lowest.min(here)));
      self.bytes += text.len();
      Ok(())
    }
  }

  /// Writes a value `depth` lists deep and returns the native stack bytes the writer used.
  fn stack_used(depth: usize) -> usize {
    let value = deep(depth);
    let anchor = 0u8;
    let base = core::ptr::addr_of!(anchor) as usize;
    let mut sink = Deepest::default();

    value
      .write_json(&mut Json::new(&mut sink))
      .expect("a value the allocator has room for is writable");

    // The fixture really is as deep as it claims: one bracket pair per level and a `null`.
    assert_eq!(
      sink.bytes,
      2 * depth + 4,
      "a {depth}-deep value wrote {} bytes, so the measurement is of a different value",
      sink.bytes
    );

    dismantle(value);
    base.saturating_sub(sink.deepest.expect("the walk wrote something"))
  }

  /// The shallower of the two depths the readings are taken at.
  const SHALLOW: usize = 64;

  /// The deeper one, sixteen times further down — `super`'s pair, kept the same so the two
  /// subjects' numbers can be read side by side.
  const DEEP: usize = 1024;

  /// What a difference of two depths may still contain: the constant part of the writer's own
  /// frame, which cancels, plus whatever the platform puts between two calls at the same depth. A
  /// walk that recursed would put five hundred times this in it per level.
  const SLACK: usize = 1024;

  /// A value's nesting does not reach the native stack, however deep it is.
  ///
  /// **The reading that sees the recursion**, and the only one of the four axes that can: the
  /// recursive walk allocated nothing, looked at no document byte, and traversed no extra response
  /// slot. Measured on this tree in debug: **1 384** bytes at both depths. Measured with the
  /// recursion restored: **35 704** and **557 944**, which is 544 bytes per level.
  #[test]
  fn a_values_depth_does_not_reach_the_native_stack() {
    let shallow = stack_used(SHALLOW);
    let deep = stack_used(DEEP);

    assert!(
      shallow > 0,
      "the writer used no measurable native stack at all, so the growth below is not a measurement"
    );

    let growth = deep.saturating_sub(shallow);
    let levels = DEEP - SHALLOW;
    assert!(
      growth <= SLACK,
      "{levels} more levels of value cost {growth} more bytes of native stack ({} per level); the \
       value walk is on the native stack",
      growth / levels.max(1)
    );
  }

  /// Allocating events one write of a `depth`-deep value costs.
  fn allocated(depth: usize) -> u64 {
    let value = deep(depth);
    let mut sink = Void::default();
    let before = allocations();
    value
      .write_json(&mut Json::new(&mut sink))
      .expect("a value the allocator has room for is writable");
    let cost = allocations() - before;
    dismantle(value);
    cost
  }

  /// The frames a value walk grows are its only allocation, and they double rather than crawl.
  ///
  /// # What this can and cannot see
  ///
  /// It cannot see the defect it was written beside — a recursive walk allocates **zero** and
  /// passes every bound below. What it pins is the structure that replaced the recursion, on the
  /// axis the replacement moved the cost onto, which is exactly the relationship
  /// `the_frame_stack_is_one_allocation_however_deep_the_response` has to the response walk.
  ///
  /// The bound is logarithmic and not the constant the response walk gets, because the reservation
  /// here cannot be exact: nothing recorded a value's depth and measuring one costs a walk with the
  /// same stack. `json::materialized`'s header argues that at length. Measured on this tree: **5**
  /// at depth 64 and **9** at depth 1 024. Measured with `try_reserve` replaced by
  /// `try_reserve_exact(1)` — the reservation that is exact and therefore per level: **64** and
  /// **1 024**.
  #[test]
  fn a_value_walks_frames_are_the_only_thing_it_allocates() {
    // A scalar opens no container, and `Vec::new` does not allocate — so the overwhelmingly common
    // leaf, the one a schema without a composite custom scalar has, costs nothing at all. Without
    // this the bound below could be met by a walk that allocated once for every value.
    for scalar in ["null", "true", r#""text""#, "1", "1.5", "ENUM"] {
      let value = leaf(scalar);
      let mut sink = Void::default();
      let before = allocations();
      value
        .write_json(&mut Json::new(&mut sink))
        .expect("writable");
      assert_eq!(
        allocations() - before,
        0,
        "writing the scalar {scalar} went to the allocator"
      );
    }

    for depth in [SHALLOW, DEEP] {
      // A doubling sequence from an empty `Vec`, plus room for the first request being for more
      // than one frame. Linear growth is `depth`, which no slack on a logarithm reaches.
      let bound = u64::from(depth.ilog2()) + 2;
      let cost = allocated(depth);
      assert!(
        cost <= bound,
        "writing a value {depth} lists deep cost {cost} allocating events against a bound of \
         {bound}; the frames are being grown once per level rather than doubled"
      );
    }
  }

  /// Runs `f` on a thread with a *small* stack, so that using the native one is fatal.
  ///
  /// The opposite of `super::with_room`, and deliberately: that helper exists so a gate about the
  /// writer is not a gate about the harness's stack, and this one exists so the writer cannot hide
  /// in a generous one. At the 544 bytes per level the recursion was measured at, the depth below
  /// wants about ten megabytes and this thread has half of one.
  fn on_a_small_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    std::thread::Builder::new()
      .stack_size(512 * 1024)
      .spawn(f)
      .expect("a thread")
      .join()
      .expect("the deep fixture did not abort")
  }

  /// How deep the loud gate's fixture is.
  ///
  /// Five times the depth the recursive walk died at on the harness's own eight-megabyte stack, on
  /// a thread with a sixteenth of it.
  const PAST_THE_OLD_ABORT: usize = 20_000;

  /// A value far past the depth the recursion died at writes, on a stack far smaller than the one
  /// it died on.
  ///
  /// **The gate that fails the way the defect did.** The two readings above are assertions about
  /// numbers; this one is the process surviving, and a walk that recursed does not fail it — it
  /// aborts the whole test binary with `SIGABRT`, which is the same signal the finding is about and
  /// is not a failure any assertion can be written to be kinder about.
  #[test]
  fn a_value_deeper_than_the_old_abort_writes_on_a_small_stack() {
    let written = on_a_small_stack(|| {
      let value = deep(PAST_THE_OLD_ABORT);
      let mut out = String::new();
      value
        .write_json(&mut Json::new(&mut out))
        .expect("writable");
      dismantle(value);
      out
    });

    // And it is the right response and not merely a response: the surviving process is the
    // finding, but a walk that survived by stopping early would be a different defect at the same
    // signal.
    assert_eq!(written.len(), 2 * PAST_THE_OLD_ABORT + 4);
    let (open, rest) = written.split_at(PAST_THE_OLD_ABORT);
    let (leaf, close) = rest.split_at(4);
    assert!(open.bytes().all(|byte| byte == b'['), "{}", &open[..8]);
    assert_eq!(leaf, "null");
    assert!(close.bytes().all(|byte| byte == b']'), "{}", &close[..8]);
  }

  /// Writes a 4 096-deep value with the allocator refusing any single request of `refuse_at` bytes
  /// or more.
  ///
  /// The value walk's own row of
  /// `super::every_buffer_the_writer_grows_refuses_rather_than_aborts`, which is where the argument
  /// for the instrument is. Here because `deep` is. Straight through the trait, so the frames are
  /// the only request there is, and the refusal is not a coincidence of one platform's layout: a
  /// frame holds a slice cursor, so it cannot be under two words, and 4 096 of them are at least
  /// sixteen times the threshold. Nothing else in the window asks the allocator for anything.
  pub(super) fn a_value_the_allocator_may_refuse(refuse_at: usize) -> Result<(), Error> {
    let value = deep(4096);
    let mut sink = Void::default();
    refuse_allocations_of_at_least(refuse_at);
    let outcome = value.write_json(&mut Json::new(&mut sink));
    allow_allocations();
    dismantle(value);
    outcome
  }
}

// ── the door itself: what happens when the allocator says no ─────────────────────────────────

/// The size, in bytes, above which the armed allocator answers no.
///
/// Small enough that each fixture's own buffer is a request over it, and large enough that nothing
/// incidental is: the fixtures below write into [`Void`], which allocates nothing, and everything
/// else in the window is a stack buffer.
const REFUSE_AT: usize = 4096;

/// The threshold that refuses nothing, which is how each fixture is read a second time.
const NEVER: usize = usize::MAX;

/// Every buffer the writer grows answers a refusal with [`Error::Allocation`], and the process
/// lives.
///
/// # The claim this measures, and why no other instrument could
///
/// [`Error::Allocation`]'s documentation says that **every** allocation this writer makes is behind
/// that variant, so "the allocator said no" is something a server can say mid-response rather than
/// something it dies of. Until this gate that sentence was prose. The allocation counter cannot
/// check it: it sees how many requests were made, and `Vec::push` registers on it exactly as
/// `try_reserve` does — right up to the moment one returns an error and the other calls the
/// allocation-error handler and aborts. So the instrument is an allocator that says no, and the
/// reading is that the call *returned at all*.
///
/// # Four fixtures, because a refusal has to be attributable
///
/// One armed threshold and four shapes, each arranged so that exactly one of the writer's four
/// buffers is a request above it. A single fixture with all four live would report "one of them
/// refused" and would pass with the other three nailed shut, which is this file's recurring
/// failure in miniature.
///
/// - **the value walk's frames** — the trait method alone, so nothing else is even asked for;
/// - **`data`'s frame stack** — a 1 024-level response with no errors, so neither the index nor the
///   path buffer is built;
/// - **the checkpoint index** — one error in a document padded to a megabyte, where the response is
///   one level deep and its one path is one segment long;
/// - **the path buffer** — one error at the bottom of a 1 024-level chain, whose document is small
///   enough that the index is well under the threshold and whose frames are never reached, because
///   §7.1 writes `errors` before `data`.
#[test]
fn every_buffer_the_writer_grows_refuses_rather_than_aborts() {
  #[cfg(feature = "materialized-numbers")]
  door(
    "the value walk's frames",
    value_depth::a_value_the_allocator_may_refuse,
  );

  let (frames, path) = with_room(|| {
    (
      (refused_frame_stack(REFUSE_AT), refused_frame_stack(NEVER)),
      (refused_path_buffer(REFUSE_AT), refused_path_buffer(NEVER)),
    )
  });
  outcomes("`data`'s frame stack", frames);
  outcomes("an error's path buffer", path);
  door("the checkpoint index", refused_checkpoint_index);
}

/// Reads one buffer's door both ways: armed, and with the same fixture unarmed.
///
/// **Both directions, because one of them is not a reading.** A fixture that returned
/// [`Error::Allocation`] for some reason of its own would satisfy the armed half alone, and a gate
/// that only ever arms the allocator cannot tell that apart from the door working.
fn door(buffer: &str, fixture: impl Fn(usize) -> Result<(), Error>) {
  outcomes(buffer, (fixture(REFUSE_AT), fixture(NEVER)));
}

/// The pair [`door`] checks, for the two fixtures that have to be run inside one `with_room`.
fn outcomes(buffer: &str, (armed, unarmed): (Result<(), Error>, Result<(), Error>)) {
  assert_eq!(
    armed,
    Err(Error::Allocation),
    "with the allocator refusing, {buffer} did not come back as a refusal"
  );
  assert_eq!(
    unarmed,
    Ok(()),
    "with the allocator answering, the same fixture for {buffer} is still not writable, so the \
     refusal beside it is not about the allocator"
  );
}

/// Writes a 1 024-level response with no errors, so `data`'s frames are the only buffer built.
fn refused_frame_stack(refuse_at: usize) -> Result<(), Error> {
  let (sdl, query) = chain(DEEP);
  run(
    &sdl,
    &query,
    NESTING,
    |name| {
      Some(if name == "x" {
        Cell::Int(1)
      } else {
        Cell::Object
      })
    },
    |response| {
      assert_eq!(response.error_count(), 0, "the fixture failed a resolver");
      let mut sink = Void::default();
      refuse_allocations_of_at_least(refuse_at);
      let outcome = write_response_with(&mut sink, response, &query, |value, json| match value {
        Cell::Int(number) => json.number(*number),
        Cell::Object => json.null(),
      });
      allow_allocations();
      outcome
    },
  )
}

/// Writes one error at the bottom of a 1 024-level chain, whose path is the only large buffer.
///
/// The document is the query, about five kilobytes, so the index is a couple of dozen checkpoints
/// and well under the threshold; the frames are never reached, because §7.1 writes `errors` first.
fn refused_path_buffer(refuse_at: usize) -> Result<(), Error> {
  let (sdl, query) = chain(DEEP);
  run(
    &sdl,
    &query,
    NESTING,
    |name| (name != "x").then_some(Cell::Object),
    |response| {
      assert_eq!(
        response.error_count(),
        1,
        "the fixture failed the wrong set"
      );
      let mut sink = Void::default();
      refuse_allocations_of_at_least(refuse_at);
      let outcome = write_response_with(&mut sink, response, &query, |value, json| match value {
        Cell::Int(number) => json.number(*number),
        Cell::Object => json.null(),
      });
      allow_allocations();
      outcome
    },
  )
}

/// Writes one error against a megabyte of document, where the index is the only large buffer.
///
/// The response is one level deep and its one path is one segment long, so neither the frames nor
/// the path buffer is anywhere near the threshold.
fn refused_checkpoint_index(refuse_at: usize) -> Result<(), Error> {
  let query = aliased(1, 1024 * 1024);
  run(
    FLAT_SDL,
    &query,
    64,
    |_| None,
    |response| {
      assert_eq!(
        response.error_count(),
        1,
        "the fixture failed the wrong set"
      );
      let mut sink = Void::default();
      refuse_allocations_of_at_least(refuse_at);
      let outcome = write_response_with(&mut sink, response, &query, |_, json| json.null());
      allow_allocations();
      outcome
    },
  )
}
