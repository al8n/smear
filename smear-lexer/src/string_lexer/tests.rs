//! Draft §2.9's cooking, through the `Cow` conversions on the literal types.
//!
//! # Where the expectations come from
//!
//! Not from this crate. The block-string values below were read off `apollo-parser` 0.8.6 — a
//! different organisation's GraphQL implementation, already a dev-dependency of `smear`'s response
//! writer for exactly this purpose — driven over the same literals. Re-deriving them from draft
//! §2.9.4's prose would have reproduced whatever misreading wrote the algorithm in the first
//! place, which is how the worked example below came to be the case this crate got wrong.
//!
//! `apollo-parser` cannot be the oracle for the braced `\u{X…}` escape: it predates that spelling
//! and rejects the literal outright. That one is settled by the character it names.
//!
//! That provenance stays live rather than being a note about how these numbers were once obtained.
//! `smear`'s `tests/json_writer.rs` holds its response writer to `apollo-parser` on block strings,
//! and its `src/json/tests.rs` holds these conversions to that writer over a corpus that overlaps
//! this one — so a drift here has two gates to get past, and neither of them is this file.

use std::borrow::Cow;

use super::{LitBlockStr, LitComplexInlineStr, LitInlineStr, LitStr};

/// Cooks a literal the way a consumer would: lex it, then convert.
fn cook(literal: &str) -> Cow<'_, str> {
  match LitStr::try_from(literal) {
    Ok(LitStr::Inline(inline)) => inline.into(),
    Ok(LitStr::Block(block)) => block.into(),
    Err(errors) => panic!("`{literal}` is a literal this lexer accepts: {errors:?}"),
  }
}

/// Whether the literal took the variant that costs nothing.
fn is_plain(literal: &str) -> bool {
  match LitStr::try_from(literal) {
    Ok(LitStr::Inline(inline)) => matches!(inline, LitInlineStr::Plain(_)),
    Ok(LitStr::Block(block)) => matches!(block, LitBlockStr::Plain(_)),
    Err(errors) => panic!("`{literal}` is a literal this lexer accepts: {errors:?}"),
  }
}

/// A braced escape is a character, and reaching it is not a panic.
///
/// `normalize_str_to_string`'s `read_hex4` used to read the `{` as a hex digit and abort, so this
/// conversion could not be asked about a literal `handle_braced_escape_unicode` — in the module
/// beside it — accepts. An astral one was the shortest way to say it.
#[test]
fn a_braced_unicode_escape_is_a_character() {
  assert_eq!(cook(r#""\u{1F600}""#), "\u{1f600}");
  assert_eq!(cook(r#""\u{41}""#), "A");
  assert_eq!(cook(r#""\u{a}""#), "\n");
  assert_eq!(cook(r#""\u{10FFFF}""#), "\u{10ffff}");
  assert_eq!(cook(r#""caf\u{e9} \u{1f600}""#), "caf\u{e9} \u{1f600}");

  // The braced form and the fixed-width forms name the same characters, so they cook the same.
  assert_eq!(cook(r#""\u{41}""#), cook(r#""A""#));
  assert_eq!(cook(r#""\u{1F600}""#), cook(r#""😀""#));
}

/// The rest of draft §2.9.1, so the arm the braced escape was added beside stays covered.
#[test]
fn every_other_escape_cooks_to_its_character() {
  assert_eq!(cook(r#""a\"b""#), "a\"b");
  assert_eq!(cook(r#""a\\b""#), "a\\b");
  assert_eq!(cook(r#""a\/b""#), "a/b");
  assert_eq!(cook(r#""\b\f\n\r\t""#), "\u{8}\u{c}\n\r\t");
  assert_eq!(cook(r#""A""#), "A");
  assert_eq!(cook(r#""é""#), "\u{e9}");
  // A surrogate pair is two escapes naming one character.
  assert_eq!(cook(r#""😀""#), "\u{1f600}");
  assert_eq!(cook(r#""aABb""#), "aABb");
  // The fixed-width spellings, one of them a surrogate PAIR naming a single character.
  assert_eq!(cook(r#""\u0041""#), "A");
  assert_eq!(cook(r#""\u00e9""#), "\u{e9}");
  assert_eq!(cook(r#""\ud83d\ude00""#), "\u{1f600}");
}

/// The two variants answer the same question, which is the only way this door means anything.
///
/// `Plain` used to hand back the source *with* its delimiters while `Complex` handed back a cooked
/// value, so one literal had two readings and nothing said which was intended. It is the value:
/// `as_str`, `Deref`, `AsRef`, `Borrow` and `From<…> for &str` already answer the spelling, and a
/// `Cow` return type only earns its keep if `Plain` is the *borrowed* case of the same answer.
///
/// Every pair below spells one value two ways — once so the lexer calls it `Plain`, once so it
/// calls it `Complex` — and the discriminants are asserted so that a future change which routed
/// everything through one arm could not quietly leave the other untested.
#[test]
fn plain_and_complex_answer_the_same_question() {
  const PAIRS: &[(&str, &str, &str)] = &[
    // value        plain spelling             complex spelling
    ("A", r#""A""#, r#""\u0041""#),
    ("a/b", r#""a/b""#, r#""a\/b""#),
    ("\u{1f600}", "\"\u{1f600}\"", r#""\u{1F600}""#),
    ("a", r#""""a""""#, "\"\"\"\na\n\"\"\""),
    ("a\nb", "\"\"\"a\nb\"\"\"", "\"\"\"\na\nb\n\"\"\""),
  ];

  for (value, plain, complex) in PAIRS {
    assert!(is_plain(plain), "`{plain}` was expected to lex as `Plain`");
    assert!(
      !is_plain(complex),
      "`{complex}` was expected to lex as `Complex`"
    );
    assert_eq!(cook(plain), *value, "plain spelling `{plain}`");
    assert_eq!(cook(complex), *value, "complex spelling `{complex}`");
  }

  // The empty literals have no escaped twin, and they are where the old disagreement was loudest:
  // `Cow::from` of `""` was the two-character string `""`.
  assert_eq!(cook(r#""""#), "");
  assert_eq!(cook(r#""""""""#), "");
}

/// `Plain` borrows, which is the whole reason this conversion returns a [`Cow`].
#[test]
fn a_plain_literal_is_borrowed_and_not_rebuilt() {
  assert!(matches!(cook(r#""plain""#), Cow::Borrowed("plain")));
  assert!(matches!(cook(r#""""plain""""#), Cow::Borrowed("plain")));
}

/// Draft §2.9.4's own worked example, which is the case the old dedent got wrong.
///
/// Two separate mistakes met on it. The dedent skipped the first *kept* line, where step 4 exempts
/// only the first line of the raw split — a different line whenever the block opens with a
/// terminator, which this example does — so `Hello,` kept the four spaces the algorithm removes.
/// And the empty line that the *last* terminator opens was never counted as a line at all, so
/// step 6 could not drop it and the value ended in a stray line feed.
#[test]
fn the_worked_example_from_2_9_4() {
  const EXPECTED: &str = "Hello,\n  World!\n\nYours,\n  GraphQL.";

  // As the specification prints it, and again with the closing delimiter at column 0 — the second
  // shape is the one that makes the last line a blank the split has to produce before step 6 can
  // remove it.
  assert_eq!(
    cook("\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n    \"\"\""),
    EXPECTED
  );
  assert_eq!(
    cook("\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\"\"\""),
    EXPECTED
  );
}

/// Step 4 dedents a *blank* line too, which is visible only when one carries more whitespace than
/// the common indent.
#[test]
fn a_blank_line_is_dedented_like_any_other() {
  // Common indent 2; the middle line's four spaces become two rather than staying four.
  assert_eq!(cook("\"\"\"\n  a\n    \n  b\n\"\"\""), "a\n  \nb");
  // Its own indent is all it can lose.
  assert_eq!(cook("\"\"\"\n    a\n  \n    b\n\"\"\""), "a\n\nb");
}

/// The last line of the raw split exists even though nothing terminates it.
///
/// `"""a\n"""` is two lines — `a` and `` — and the second is a trailing blank. Missing it made
/// this literal `is_clean`, so it took the borrowing arm and cooked to its own spelling, newline
/// and all.
#[test]
fn a_trailing_terminator_opens_a_line_that_gets_trimmed() {
  assert_eq!(cook("\"\"\"a\n\"\"\""), "a");
  assert_eq!(cook("\"\"\"\na\n\"\"\""), "a");
  assert_eq!(cook("\"\"\"\ncontent\n\n\"\"\""), "content");
  assert_eq!(cook("\"\"\"a\n  b\n    c\n\"\"\""), "a\nb\n  c");
  assert!(
    !is_plain("\"\"\"a\n\"\"\""),
    "a literal whose value is not its inner spelling cannot take the borrowing arm"
  );
}

/// The remaining §2.9.4 rules, so the two repairs above are not the only thing this file pins.
#[test]
fn the_rest_of_2_9_4() {
  assert_eq!(cook(r#""""plain""""#), "plain");
  // A tab is one indentation character, exactly as a space is.
  assert_eq!(cook("\"\"\"\n\t\ta\n\t\t\tb\n\"\"\""), "a\n\tb");
  // The first line of the raw split keeps its indentation.
  assert_eq!(
    cook("\"\"\"  keeps first line indent\n  and dedents this\n\"\"\""),
    "  keeps first line indent\nand dedents this"
  );
  // CR and CRLF are terminators, and every kept line is rejoined with one line feed.
  assert_eq!(cook("\"\"\"\r\n  a\r\n  b\r\n\"\"\""), "a\nb");
  assert_eq!(cook("\"\"\"a\r\nb\rc\n\"\"\""), "a\nb\nc");
  // Nothing but blank lines is the empty string.
  assert_eq!(cook("\"\"\"\n   \n  \n\"\"\""), "");
  // `\"""` is a block string's only escape; a backslash in front of anything else is a backslash.
  assert_eq!(cook(r#""""a\"""b""""#), "a\"\"\"b");
  assert_eq!(cook(r#""""a\nb""""#), "a\\nb");
}

/// The allocation the `Complex` arm asks for covers what it writes.
///
/// `required_capacity` is what the conversion hands `String::with_capacity`, so a value longer
/// than it would mean the one number this carrier exists to precompute cannot be trusted. It is an
/// upper bound rather than an equality: a kept blank line deeper than the common indent loses that
/// whitespace to the dedent, and the plan's arithmetic does not model it.
#[test]
fn required_capacity_covers_the_cooked_value() {
  const CORPUS: &[&str] = &[
    r#""a\"b""#,
    r#""\b\f\n\r\t""#,
    r#""\u0041""#,
    r#""\ud83d\ude00""#,
    r#""\u{1F600}""#,
    r#""caf\u{e9} \u{1f600}""#,
    "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n\"\"\"",
    "\"\"\"\na\n\"\"\"",
    "\"\"\"a\n\"\"\"",
    "\"\"\"\ncontent\n\n\"\"\"",
    "\"\"\"\n  a\n    \n  b\n\"\"\"",
    "\"\"\"\r\n  a\r\n  b\r\n\"\"\"",
    r#""""a\"""b""""#,
    "\"\"\"\n   \n  \n\"\"\"",
  ];

  for literal in CORPUS {
    let capacity = match LitStr::try_from(*literal) {
      Ok(LitStr::Inline(LitInlineStr::Complex(c))) => c.required_capacity(),
      Ok(LitStr::Block(LitBlockStr::Complex(c))) => c.required_capacity(),
      Ok(_) => panic!("`{literal}` was expected to lex as `Complex`"),
      Err(errors) => panic!("`{literal}` is a literal this lexer accepts: {errors:?}"),
    };
    let cooked = cook(literal);
    assert!(
      cooked.len() <= capacity,
      "`{literal}` cooked to {} bytes against a capacity of {capacity}",
      cooked.len()
    );
  }
}

/// Reaches `Cow::from`'s `Complex` arm directly, bypassing the lexer entirely.
///
/// The real lexer never hands this conversion an escape draft §2.9.1 does not name — the test
/// below this one checks exactly that — which is why an external caller could never observe
/// [`normalize_str_to_string`](super::normalize_str_to_string)'s copy-through claim failing to
/// hold. [`LitComplexInlineStr::new`] is `pub(crate)`, so a test in this crate can still ask the
/// conversion the question the lexer never gets to ask: what does it do with a sequence only
/// `u32::from_str_radix` — not the grammar — accepts.
fn force_complex_cook(source: &str) -> Cow<'_, str> {
  LitInlineStr::Complex(LitComplexInlineStr::new(source, source.len())).into()
}

/// `u32::from_str_radix` takes a leading `+` and, for the braced form, does not bound the digit
/// count — draft §2.9.1 names neither. `hex4` and the braced arm of `read_unicode_escape` used to
/// trust the parse to fail on everything the grammar excludes; it does not, so each sequence below
/// used to decode instead of taking the copy-through path its doc comment promises.
#[test]
fn an_escape_only_from_str_radix_accepts_is_copied_through_not_decoded() {
  // Fixed-width `\uXXXX`: a leading `+` ate one of the four hex slots and still parsed — `+123`
  // read as the 3-digit `123`, decoding U+0123 instead of being rejected.
  assert_eq!(force_complex_cook(r#""\u+123""#), r#"\u+123"#);

  // Braced `\u{X…}`: the same leading-`+` leniency, decoding U+0041 (`A`).
  assert_eq!(force_complex_cook(r#""\u{+41}""#), r#"\u{+41}"#);

  // Braced `\u{X…}`: `from_str_radix` has no digit-count limit, so an eleven-digit zero-padded
  // run past draft §2.9.1's maximum of six still parsed, decoding U+1F600 (an emoji).
  assert_eq!(
    force_complex_cook(r#""\u{0000001F600}""#),
    r#"\u{0000001F600}"#
  );

  // The same length leniency isolated from any particular value: seven digits is one past the
  // grammar's maximum even though `0x41` is a perfectly ordinary codepoint.
  assert_eq!(force_complex_cook(r#""\u{0000041}""#), r#"\u{0000041}"#);

  // Both leniencies at once.
  assert_eq!(
    force_complex_cook(r#""\u{+0000001F600}""#),
    r#"\u{+0000001F600}"#
  );
}

/// The premise behind reaching for `force_complex_cook` at all: every source above is a lex
/// error, not a `Complex` literal, so no caller outside this crate could ever have exercised the
/// bug the previous test pins.
#[test]
fn the_lexer_already_rejects_what_only_from_str_radix_would_accept() {
  for literal in [
    r#""\u+123""#,
    r#""\u{+41}""#,
    r#""\u{0000001F600}""#,
    r#""\u{0000041}""#,
    r#""\u{+0000001F600}""#,
  ] {
    assert!(
      LitStr::try_from(literal).is_err(),
      "`{literal}` was expected to be a lex error, not a literal this lexer accepts"
    );
  }
}
