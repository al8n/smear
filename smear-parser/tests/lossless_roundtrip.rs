#![cfg(feature = "rowan")]

//! Gate 3: round-trip. For every corpus entry, `tree.text()` is the source, byte for byte.
//!
//! This is the invariant the whole lossless suite exists for — a formatter, a refactoring tool or
//! an IDE that reprints a document must get the file back — and it is the one gate that holds over
//! **rejected** parses as strongly as over accepted ones. A parse that gave up halfway and dropped
//! the rest of the file would still satisfy gate 1 (the verdict is right) and gate 2 (that sweep
//! only pads valid entries). It fails here.
//!
//! # What the equality actually measures — which is less than it looks, measured rather than
//! assumed
//!
//! `Sink::finish` refuses any source byte that no committed token covers and no lexer-error
//! diagnostic explains, and it tiles the explained ones with the profile's `gap_kind`. So a
//! materialized tree is *structurally* total over its source: `text() == src` cannot fail through
//! "the grammar forgot a token".
//!
//! The obvious remaining candidate is the wiring — a sink bound to a different buffer than the
//! one that was parsed — and **that is not this gate's to catch either.** `Input`'s
//! source-identity handshake asserts both halves of the pairing, unconditionally and in every
//! build profile: the origin must be the same value, and the bound extent must be at least the
//! parsed one. Both were driven here. Binding the sink to `src` minus its trailing newline
//! panics on the extent assert; binding it to a same-length foreign buffer panics on the origin
//! one. Neither ever reaches a tree, let alone a comparison — and tokora's own message says why
//! the check has to live there rather than here: *"a same-length foreign source produces a tree
//! whose text is the sink's and whose structure is the parse's, and no later check can see it."*
//!
//! Eight mutations were run against this file (see the task record); **not one of them made
//! `text()` differ from `src`.** What reds instead is a *panic*: an uncovered gap, a source with
//! no lexable token in it, a misbound sink. So the honest reading of the sweep below is that it is
//! a **totality-contract regression detector plus a panic sweep over the whole corpus** — it is
//! the only gate that materializes every invalid entry — and that the file's measuring content is
//! in its neighbours:
//!
//! - [`every_byte_is_carried_by_a_token`] re-derives the source from the **tokens** rather than
//!   from `SyntaxText`, and counts the gap-tiled regions — because "every byte survived" and "the
//!   grammar accounted for every byte" are different claims, and over this corpus they have
//!   different answers. Exactly one entry, `invalid_unterminated_string.graphql`, round-trips
//!   through a `Gap` token rather than through committed ones: its unterminated string is a lexer
//!   error, so the lexer hands back no token for that region at all and the sink's fallback is
//!   what keeps the bytes. Mis-wiring the profile's gap kind to an ordinary trivia kind leaves the
//!   equality above green and reds this census, which is the measurement that earns it its place.
//! - [`all_three_roots_round_trip_every_corpus_entry`] is the only test here that reds when a
//!   driver stops draining what its production left behind.
//!
//! # What this gate cannot see — the part worth writing down
//!
//! `text()` is the tree with its *shape projected away*. Two trees that disagree about every node
//! boundary, every parent and every kind produce the same string as long as they carry the same
//! tokens in the same order. So the entire class of **structural** loss is invisible here, and
//! this gate is correct to pass over it:
//!
//! - [`a_round_trip_gate_is_blind_to_a_lost_definition_node`] exhibits an input whose whole
//!   `ObjectTypeDefinition` is missing from the tree while every byte survives. Gate 1 is blind to
//!   it too — both suites reject — so it lives in the space between two correct greens. Only
//!   gate 5's golden trees can see it.
//! - [`the_same_bytes_round_trip_through_two_different_trees`] makes the general form measurable
//!   rather than asserted: the same source through two different roots gives two genuinely
//!   different node-kind sequences and one identical string.
//!
//! The practical consequence for anyone mutating this suite: **a mutation that only moves node
//! kinds around will not red this gate**, and that is a property of the gate rather than a gap in
//! these tests. Two were run to confirm it — pointing `named_type` at `K::ListType`, and deleting
//! `try_eat`'s trivia skip so the *verdict* changes — and both leave all seven tests green. The
//! second is gate 2's own witness; the first is gate 5's.
//!
//! # The alphabet the corpus does not have
//!
//! Gate 2 measured that no corpus entry contains a carriage return, a tab or a BOM, and none
//! contains a non-ASCII byte either. A round-trip gate that only ever sees ASCII-with-newlines
//! would say nothing about multi-byte text or about the forms a real editor produces, so
//! [`bytes_the_corpus_does_not_contain_still_round_trip`] carries them as its own fixtures.
//!
//! Those fixtures each contain at least one lexable token, and the sources that contain **none**
//! are now corpus entries instead: `"unterminated`, `"""unterminated` and `%` are the
//! `invalid_lex_*` class, added once tokora `2bbca21` landed. Before it, such a source left the
//! sink holding a `Document` node over no tokens at all, which `finish` reported as
//! `StructureWithoutTokens` and the runner turned into a panic; the wall now also requires an
//! uncovered gap, and a fully explained source passes. They are the entries this gate's
//! [`GAP_TILED_ENTRIES`] census exists for, and the only ones whose round-trip is carried by
//! tiling alone.

use std::path::PathBuf;

use smear_parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{
    Parse, SyntaxNode, parse_document, parse_executable_document, parse_type_system_document,
  },
};

/// The corpus entries whose bytes are kept by the sink's gap tiling rather than by the grammar.
///
/// Each carries a **lexer** error, so no token is produced for the refused region at all and
/// `Sink::finish` covers it with the profile's `gap_kind`. Named rather than tolerated by a
/// blanket allowance: an entry showing up here means a region nothing in the grammar accounts for,
/// and whoever adds it should have to say so.
///
/// The three `invalid_lex_*` entries are the strongest form of the claim this gate makes. Their
/// whole source is refused, so the tree carries **no committed token at all** and the round-trip
/// rests entirely on the tiling — the state tokora's zero-token wall refused outright until
/// `2bbca21`. `invalid_unterminated_string.graphql` is the mixed case: nine tokens, then a tiled
/// tail. Gate 1's `both_suites_agree_across_the_lexer_grammar_error_boundary` is where that
/// distinction is asserted; here they are simply four regions the grammar did not cover.
const GAP_TILED_ENTRIES: &[&str] = &[
  "invalid_lex_illegal_character.graphql",
  "invalid_lex_unterminated_block_string.graphql",
  "invalid_lex_unterminated_string.graphql",
  "invalid_unterminated_string.graphql",
];

/// Every `.graphql` file in the shared corpus, in a deterministic order.
///
/// The corpus is gate 1's fixture set, read unchanged — this gate adds no entry of its own, so a
/// green here is a statement about the same material the other gates hold.
fn corpus_files() -> Vec<PathBuf> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| panic!("the shared corpus at {} is unreadable: {e}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .collect();
  files.sort();
  files
}

/// Every corpus entry as `(file name, contents)`.
fn corpus() -> Vec<(String, String)> {
  corpus_files()
    .into_iter()
    .map(|p| {
      let name = p.file_name().unwrap().to_string_lossy().to_string();
      let src = std::fs::read_to_string(&p)
        .unwrap_or_else(|e| panic!("{} is unreadable: {e}", p.display()));
      (name, src)
    })
    .collect()
}

/// The tree's text, which is the claim under test.
fn round_trip(src: &str) -> String {
  parse_document(src).syntax().text().to_string()
}

/// The tree's text re-derived from its **tokens**, in document order.
///
/// `SyntaxText` is rowan's own concatenation of the same tokens, so this is a second route to one
/// answer rather than a check of rowan. What it buys is a comparison neither half of which can be
/// the source itself: if [`round_trip`] ever became a passthrough — a `Parse` that retained `src`
/// and handed it back — the two derivations would disagree.
fn tokens_text(node: &SyntaxNode) -> String {
  node
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .map(|token| token.text().to_string())
    .collect()
}

/// How many of a tree's tokens are the sink's gap tiling rather than committed grammar tokens.
fn gap_tokens(node: &SyntaxNode) -> usize {
  node
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .filter(|token| token.kind() == K::Gap)
    .count()
}

/// Gate 3 proper: every corpus entry, valid and invalid alike, comes back byte for byte.
///
/// # Why the counters are on the *verdict* rather than on the file-name prefix
///
/// Gate 1 already holds the prefix against both suites, so re-checking it here would only measure
/// gate 1. What this gate needs a control for is different: the round-trip must be exercised over
/// **failing** parses, because that is where a production could bail out and leave bytes behind,
/// and an all-accepted corpus would leave the recovery path untested while still reading green.
#[test]
fn every_corpus_entry_round_trips_byte_for_byte() {
  let entries = corpus();
  assert!(!entries.is_empty(), "the corpus is empty");

  let mut accepted = 0usize;
  let mut rejected = 0usize;
  for (name, src) in &entries {
    let parse = parse_document(src);
    assert_eq!(
      &parse.syntax().text().to_string(),
      src,
      "{name}: the tree's text is not the source — a lossless parse lost, moved or invented bytes"
    );
    if parse.has_errors() {
      rejected += 1;
    } else {
      accepted += 1;
    }
  }

  // The positive controls. Without both, this loop can be green over a corpus that never reaches
  // the recovery path at all, or one that never reaches a complete parse.
  assert!(
    accepted >= 20,
    "only {accepted} corpus entries parse cleanly; the gate is not measuring the ordinary path"
  );
  assert!(
    rejected >= 5,
    "only {rejected} corpus entries fail to parse; the round-trip is untested over recovery, \
     which is the one path that can drop bytes"
  );
}

/// The other two roots materialize their own trees, and each must round-trip too.
///
/// [`parse_document`] is one entry point of three, and the other two are the reason this test
/// exists rather than being folded into the loop above. [`parse_type_system_document`] and
/// [`parse_executable_document`] run the corpus through a production set that **rejects most of
/// it**, so unlike `document` they routinely stop before the end of the source — which makes the
/// drain in `type_system_document_entry` and `executable_document_entry` load-bearing where
/// `document_entry`'s is not. Measured both ways: deleting `document_entry`'s drain leaves the
/// entire suite green, while deleting either of the other two's reds exactly this test, on
/// `finish` refusing the uncovered tail.
///
/// It is also the only sweep that materializes the **invalid** half of the corpus through the two
/// alternate roots at all.
#[test]
fn all_three_roots_round_trip_every_corpus_entry() {
  for (name, src) in corpus() {
    assert_eq!(
      parse_type_system_document(&src).syntax().text().to_string(),
      src,
      "{name}: the SDL-only root lost bytes"
    );
    assert_eq!(
      parse_executable_document(&src).syntax().text().to_string(),
      src,
      "{name}: the executable-only root lost bytes"
    );
  }
}

/// Where the bytes actually are: in the tokens, and — for one entry — in a gap.
///
/// The gate above proves the string comes back. This proves *what carries it*, which is the
/// difference between "tokora tiles the source for us" and "the grammar accounted for the source".
/// Both are true here, and they are true of different entries:
///
/// - every accepted parse in the corpus is covered by committed tokens, with no gap at all;
/// - exactly one rejected entry, [`GAP_TILED_ENTRIES`], keeps its tail through the sink's fallback,
///   because a lexer error yields no token to commit.
///
/// That single entry is also this test's positive control. Asserting "no gaps anywhere" over a
/// corpus that has one would be false; asserting it over a corpus that has none would be a zero
/// with nothing behind it.
///
/// **This is the assertion that catches a defect the equality cannot.** Pointing the profile's
/// `gap_kind` at an ordinary trivia kind — after which a consumer can no longer tell a region the
/// sink invented from one the lexer produced — leaves every round-trip in this file green and reds
/// here, with `left: []`.
#[test]
fn every_byte_is_carried_by_a_token() {
  let mut gap_tiled: Vec<String> = Vec::new();
  let mut total_gaps = 0usize;

  for (name, src) in corpus() {
    let parse = parse_document(&src);
    let syntax = parse.syntax();

    assert_eq!(
      tokens_text(&syntax),
      src,
      "{name}: concatenating the tree's tokens does not reproduce the source, so `text()` and \
       the tokens disagree about what the tree contains"
    );

    let gaps = gap_tokens(&syntax);
    if gaps > 0 {
      total_gaps += gaps;
      gap_tiled.push(name.clone());
      assert!(
        parse.has_errors(),
        "{name}: a cleanly accepted parse left a region for the sink to tile — the grammar \
         skipped bytes it should have committed"
      );
    }
  }

  assert_eq!(
    gap_tiled, GAP_TILED_ENTRIES,
    "the set of entries whose round-trip is carried by gap tiling has changed; a new one means a \
     new source region that nothing in the grammar accounts for"
  );
  assert!(
    total_gaps > 0,
    "no entry produced a gap token at all, so the census above measured nothing"
  );
}

/// The same bytes, two different trees, one identical string.
///
/// This is [the module docs'](self) claim made out of live values instead of prose: `text()`
/// projects the shape away, so a gate built on it is structurally unable to see a tree whose nodes
/// moved. The mixed root reads `query Q { f }` as an `OperationDefinition`; the SDL-only root has
/// no executable production and recovers the same bytes into `Error` nodes. Neither loses a byte.
///
/// A mutation that relocates a token to a different parent, or drops an intermediate wrapper node,
/// lands in exactly this blind spot — which is why gate 5 exists and why this test asserts the
/// blindness rather than leaving a reader to infer it.
#[test]
fn the_same_bytes_round_trip_through_two_different_trees() {
  const SRC: &str = "query Q { f }";

  fn shape(parse: &Parse) -> Vec<K> {
    parse.syntax().descendants().map(|n| n.kind()).collect()
  }

  let mixed = parse_document(SRC);
  let sdl = parse_type_system_document(SRC);

  assert_ne!(
    shape(&mixed),
    shape(&sdl),
    "the two roots were supposed to disagree about this source's shape; if they now agree, this \
     test has stopped exhibiting anything"
  );
  assert!(
    shape(&mixed).contains(&K::OperationDefinition),
    "the mixed root must read {SRC:?} as an operation"
  );
  assert!(
    !shape(&sdl).contains(&K::OperationDefinition),
    "the SDL-only root must not have an executable production"
  );

  assert_eq!(mixed.syntax().text().to_string(), SRC);
  assert_eq!(
    sdl.syntax().text().to_string(),
    SRC,
    "both trees keep every byte — which is the point: this gate cannot tell them apart"
  );
}

/// What a round-trip gate structurally cannot measure.
///
/// A definition that fails partway through its body unwinds before `node_at` spends its mark, so
/// the node is never opened and the tree keeps the bytes as bare children of `Document`. Recorded
/// by Task 10, pinned from gate 1's side by
/// `lossless_parity.rs::a_verdict_gate_is_blind_to_a_lost_definition_node`, and deliberately left
/// unfixed.
///
/// **This gate is blind to it in the same way gate 1 is, and for an unrelated reason.** Gate 1
/// compares one bit and both suites reject; this gate compares the bytes and every byte is there.
/// A whole class of structural loss lives between those two correct greens, and only gate 5 sees
/// it. Written as assertions rather than as a comment so that the day the behaviour changes is a
/// day somebody is told.
///
/// # The loss is not "any failure in a body"
///
/// The [control set](KEEPS_THE_NODE) is what makes that precise, and it exists because a mutation
/// found the first version could not tell the two apart: `type T { x: }` **keeps** its
/// `ObjectTypeDefinition`, because a missing type recovers in place and no `Err` unwinds past the
/// mark. Without that half, `!contains` would be satisfied by any input that simply is not an
/// object type.
#[test]
fn a_round_trip_gate_is_blind_to_a_lost_definition_node() {
  const LOST: K = K::ObjectTypeDefinition;

  /// Bodies that fail by **unwinding**: the mark is never spent and the node never opens.
  const LOSES_THE_NODE: &[&str] = &[
    "type T { \"\"\"b\"\"\" \"a\" }",
    "type { x: Int }",
    "type T { : Int }",
    "type T { x Int }",
    // Padded on both sides, so the round-trip assertion has something to be wrong about: over
    // whitespace-free witnesses alone, comparing against `src.trim()` is indistinguishable from
    // comparing against `src`.
    "\n  type T { x Int }\n",
  ];

  /// The control: these fail too, and each **keeps** its definition node, because each recovers in
  /// place instead of unwinding.
  const KEEPS_THE_NODE: &[&str] = &["type T { x: }", "type T { x: Int", "type T {}"];

  fn kinds_of(src: &str) -> Vec<K> {
    parse_document(src)
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect()
  }

  for src in LOSES_THE_NODE.iter().copied() {
    // Every byte is there, so this gate passes the input without comment…
    assert_eq!(round_trip(src), src, "{src:?}: the text did not round-trip");
    // …and the parse is a failure, so gate 1 passes it without comment too.
    assert!(
      parse_document(src).has_errors(),
      "{src:?} was supposed to be a failing parse"
    );

    // And yet the node is gone.
    let kinds = kinds_of(src);
    assert!(
      !kinds.contains(&LOST),
      "{src:?}: expected no {LOST:?} in {kinds:?} — if the node is back, this blind spot has been \
       closed and the test should become the assertion that it stays closed"
    );
  }

  for src in KEEPS_THE_NODE.iter().copied() {
    assert_eq!(round_trip(src), src, "control {src:?} did not round-trip");
    assert!(
      parse_document(src).has_errors(),
      "control {src:?} was supposed to be a failing parse"
    );
    let kinds = kinds_of(src);
    assert!(
      kinds.contains(&LOST),
      "control {src:?}: {LOST:?} is missing from {kinds:?} — a failing parse that recovers in \
       place must still carry its definition node, or the assertion above measures nothing"
    );
  }
}

/// Byte forms the corpus does not contain, round-tripped here instead.
///
/// Gate 2 measured that no corpus entry carries a carriage return, a tab or a BOM; none carries a
/// non-ASCII byte either, and 83 of the 87 end in a newline — the three `invalid_lex_*` entries
/// must not, and the empty one has nothing to end with. So the corpus on its own would leave
/// a whole set of real-editor and real-document byte shapes untested by the one gate that is about
/// bytes. Every case below is a source the parser must reproduce exactly, including the ones it
/// rejects.
///
/// **Every case contains at least one lexable token.** That is now a division of labour rather
/// than a workaround: wholly unlexable sources are the corpus's `invalid_lex_*` class and are
/// round-tripped by the sweep above, so what is left for this set is byte *forms* — encodings,
/// line endings, escapes — over sources the grammar can actually reach.
#[test]
fn bytes_the_corpus_does_not_contain_still_round_trip() {
  const CASES: &[(&str, &str)] = &[
    ("empty", ""),
    ("whitespace only", "  \n\t "),
    ("comment with no line terminator", "# c"),
    ("BOM at offset zero", "\u{FEFF}{ a }"),
    ("BOM in the interior", "{ a\u{FEFF} }"),
    ("bare carriage return", "{ a }\r"),
    ("CRLF line endings", "query Q {\r\n  f\r\n}\r\n"),
    ("tabs as separators", "type\tT\t{\tf:\tInt\t}"),
    (
      "multi-byte text inside a string",
      "{ f(a: \"h\u{e9}llo \u{2192} \u{1f600}\") }",
    ),
    (
      "multi-byte text inside a comment",
      "# caf\u{e9} \u{2192}\n{ a }",
    ),
    (
      "escape sequences inside a string",
      "{ f(a: \"\\u0041\\n\\t\\\\\") }",
    ),
    (
      "a block string with its own newlines",
      "\"\"\"\n  doc\n\"\"\"\ntype T { f: Int }",
    ),
    ("no trailing newline", "type T { f: Int }"),
    ("trailing spaces and no newline", "type T { f: Int }   "),
    // Rejected shapes. The recovery path is the one that can drop bytes, so it is where a
    // round-trip gate earns its keep.
    ("unterminated brace", "type T { f: Int"),
    (
      "a lexer error after a lexable prefix",
      "{ a } \"unterminated",
    ),
    (
      "top-level junk between definitions",
      "type A { f: Int } ??? type B { g: Int }",
    ),
  ];

  let mut accepted = 0usize;
  let mut rejected = 0usize;
  for (name, src) in CASES {
    let parse = parse_document(src);
    assert_eq!(
      &parse.syntax().text().to_string(),
      src,
      "{name}: {src:?} did not round-trip"
    );
    assert_eq!(
      &tokens_text(&parse.syntax()),
      src,
      "{name}: {src:?} round-tripped through `text()` but its tokens do not reproduce it"
    );
    if parse.has_errors() {
      rejected += 1;
    } else {
      accepted += 1;
    }
  }

  assert!(
    accepted >= 8,
    "only {accepted} of these fixtures parse cleanly; they were meant to be ordinary documents \
     written with unusual bytes, not a second invalid corpus"
  );
  assert!(
    rejected >= 3,
    "only {rejected} of these fixtures fail; the recovery path is what this set is for"
  );
}

/// The comparison this gate is built on can distinguish two sources.
///
/// Every assertion above is an equality against `src`. Two independent ways for that to become
/// vacuous: a `text()` that answered the same thing for every tree, and a `Parse` that retained
/// the source and handed it straight back. The first is refuted by driving the projection until it
/// disagrees with itself; the second by [`every_byte_is_carried_by_a_token`], which re-derives the
/// same bytes from the tokens.
#[test]
fn the_round_trip_comparison_is_not_vacuous() {
  const A: &str = "query A { f }";
  const B: &str = "query B { g }";

  assert_eq!(round_trip(A), A);
  assert_eq!(round_trip(B), B);
  assert_ne!(
    round_trip(A),
    B,
    "the text projection answers the same thing for two different sources"
  );
  assert_ne!(
    round_trip(A),
    round_trip(B),
    "two different sources produced the same text"
  );

  // The bytes are the tree's, not the argument's: a passthrough would agree with `src` here and
  // disagree with the tokens, which is the pairing `every_byte_is_carried_by_a_token` holds over
  // the whole corpus. Repeated on one source so this test states the property it depends on.
  assert_eq!(tokens_text(&parse_document(A).syntax()), A);
}
