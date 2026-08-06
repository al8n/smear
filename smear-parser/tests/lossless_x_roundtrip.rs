#![cfg(all(feature = "rowan", feature = "graphqlx"))]

//! Gate 3: `tree.text() == source`, byte-exact, over every corpus entry and all three GraphQLx
//! roots — **including the invalid ones**, which are the point. A parser that drops a byte it
//! could not understand round-trips every valid input perfectly.
//!
//! # The byte equality cannot be made to fail by an in-tree defect, so it is not this file's
//! measuring content
//!
//! Phase A ran eight mutations against the GraphQL version of this gate and **not one** made
//! `text()` differ from `src`. That is not a weakness in the mutations: the sink tiles every byte
//! the lexer reported and every byte it refused, so the text is a function of the *input* and the
//! tree's shape barely enters into it. What the equality reds on is a **panic**, not an inequality.
//!
//! So the assertions that measure something are written deliberately and are four:
//!
//! 1. [`every_byte_is_carried_by_a_token`] — the token lengths sum to `src.len()`, so a byte
//!    carried only by a [`Gap`](K::Gap) tile is visible *as* a gap rather than merely as present;
//! 2. [`only_the_unlexable_entries_are_tiled_with_gaps`] — the gap census is pinned per entry, so a
//!    parse that started tiling gaps over ordinary input reds, which the text comparison cannot
//!    see;
//! 3. [`two_different_trees_over_one_source_carry_the_same_bytes`] — the same source through two
//!    roots gives equal texts and **unequal trees**, which is what proves the comparison is not
//!    comparing a tree with itself;
//! 4. [`a_round_trip_gate_is_blind_to_a_lost_definition_node`] — a witness that round-trips
//!    perfectly while a whole definition has vanished from the tree. Recording the blindness is
//!    what stops a green gate 3 being read later as evidence of structural fidelity; only gate 5's
//!    golden trees can see that class.

use std::path::PathBuf;

use smear_parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{Parse, SyntaxNode, parse_executable_document, parse_str, parse_type_system_document},
};

/// Every `.graphqlx` file in the GraphQLx corpus, in a deterministic order.
///
/// Restated rather than imported: every gate here is a self-contained integration binary.
fn corpus_files() -> Vec<PathBuf> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| {
      panic!(
        "the GraphQLx corpus at {} is unreadable: {e}",
        dir.display()
      )
    })
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphqlx"))
    .collect();
  files.sort();
  files
}

/// A named root entry point over a `&str`.
type Root = (&'static str, fn(&str) -> Parse);

/// The three roots, by name, so a failure says which one lost the bytes.
///
/// All three are shipped `fn(&str) -> Parse` entry points on `graphqlx::lossless`, and all three
/// are run over every entry, valid or not: a root that rejects an entry still has to keep its
/// bytes, and the rejecting path is the one where a byte is most likely to be dropped.
const ROOTS: &[Root] = &[
  ("document", parse_str),
  ("type_system_document", parse_type_system_document),
  ("executable_document", parse_executable_document),
];

/// The text a tree re-prints.
fn text_of(node: &SyntaxNode) -> String {
  node.text().to_string()
}

/// The node kinds of `node` in pre-order, ignoring tokens.
fn kinds_of(node: &SyntaxNode) -> Vec<K> {
  node.descendants().map(|n| n.kind()).collect()
}

/// The number of bytes the tree's **tokens** cover, and how many of those are gap tiles.
fn token_bytes(node: &SyntaxNode) -> (usize, usize) {
  let mut total = 0usize;
  let mut gaps = 0usize;
  for token in node
    .descendants_with_tokens()
    .filter_map(|e| e.into_token())
  {
    total += token.text().len();
    if token.kind() == K::Gap {
      gaps += 1;
    }
  }
  (total, gaps)
}

/// `tree.text() == source`, for every corpus entry through every root.
#[test]
fn every_corpus_entry_round_trips_through_every_root() {
  let mut checked = 0usize;
  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    for (root, parse) in ROOTS {
      let tree = parse(&src);
      assert_eq!(
        text_of(&tree.syntax()),
        src,
        "{} through {root}: the tree does not re-print its source",
        entry.display()
      );
      checked += 1;
    }
  }
  assert_eq!(
    checked,
    corpus_files().len() * ROOTS.len(),
    "the sweep did not run every entry through every root"
  );
}

/// Every byte of the source is carried by a token, in every root.
///
/// The text comparison cannot distinguish a byte carried by a real token from one carried by a
/// [`Gap`](K::Gap) tile, because both re-print. This sums the token lengths instead, which is the
/// same claim stated where the two are separable — and it is the assertion that would red if the
/// sink ever started synthesising text rather than tiling it.
#[test]
fn every_byte_is_carried_by_a_token() {
  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    for (root, parse) in ROOTS {
      let tree = parse(&src);
      let (bytes, _) = token_bytes(&tree.syntax());
      assert_eq!(
        bytes,
        src.len(),
        "{} through {root}: the tokens cover {bytes} of {} bytes",
        entry.display(),
        src.len()
      );
    }
  }
}

/// The corpus entries whose mixed-root parse tiles a [`Gap`](K::Gap), and how many tiles each has.
///
/// A gap is a byte run **no committed token covered** — the sink's answer to a source the lexer
/// refused. Exactly four entries are in that state and each has exactly one tile, which is the
/// whole of the corpus's unlexable class.
///
/// Pinned per entry rather than counted in aggregate. A parse that started tiling gaps over
/// ordinary input keeps every byte and re-prints perfectly, so the text comparison is blind to it;
/// this census is what sees it, and pinning the *count* as well as the entry is what sees a single
/// gap becoming three.
const GAP_ENTRIES: &[(&str, usize)] = &[
  ("invalid_lex_illegal_character.graphqlx", 1),
  ("invalid_lex_unterminated_block_string.graphqlx", 1),
  ("invalid_lex_unterminated_string.graphqlx", 1),
  ("invalid_unterminated_string.graphqlx", 1),
];

/// Only the unlexable entries are tiled with gaps, and each with the pinned number of tiles.
#[test]
fn only_the_unlexable_entries_are_tiled_with_gaps() {
  let mut seen: Vec<(String, usize)> = Vec::new();
  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let (_, gaps) = token_bytes(&parse_str(&src).syntax());
    if gaps > 0 {
      seen.push((
        entry.file_name().unwrap().to_string_lossy().to_string(),
        gaps,
      ));
    }
  }
  let expected: Vec<(String, usize)> = GAP_ENTRIES
    .iter()
    .map(|(name, n)| ((*name).to_string(), *n))
    .collect();
  assert_eq!(
    seen, expected,
    "the gap census moved; pin the change on purpose"
  );
}

/// The same bytes round-trip through two **different** trees.
///
/// Without the inequality half, this whole gate could be comparing a tree with itself: a
/// `roundtrip(x) == roundtrip(x)` reads exactly like the assertion above and holds for any
/// implementation whatsoever. The witness is a source the two roots genuinely disagree about — an
/// executable definition, which the SDL-only root has no production for — so the two trees differ
/// in shape while carrying the same bytes.
#[test]
fn two_different_trees_over_one_source_carry_the_same_bytes() {
  const SRC: &str = "query Q<T = Int>($v: T) { f(a: $v) }\n";

  let mixed = parse_str(SRC);
  let sdl_only = parse_type_system_document(SRC);

  assert_eq!(text_of(&mixed.syntax()), SRC);
  assert_eq!(text_of(&sdl_only.syntax()), SRC);
  assert_ne!(
    kinds_of(&mixed.syntax()),
    kinds_of(&sdl_only.syntax()),
    "the two roots built the same tree, so the equality above compares a tree with itself"
  );
  assert!(
    !mixed.has_errors(),
    "the mixed root must accept the witness, or the two trees differ for the wrong reason"
  );
  assert!(
    sdl_only.has_errors(),
    "the SDL-only root must reject the witness, which is what makes the trees differ"
  );
}

/// **This gate is blind to a lost definition node**, and here is the witness.
///
/// `type T { f: Int } type` re-prints byte for byte through the mixed root, and the trailing
/// `type` — a keyword that opens a definition — produces **no node at all**: not an
/// [`ObjectTypeDefinition`](K::ObjectTypeDefinition), not even an [`Error`](K::Error). The
/// production returns `Err` before its retro-wrap mark is spent, an unspent mark materializes into
/// nothing, and the resync commits the keyword as a bare token child of the
/// [`Document`](K::Document).
///
/// Every byte survives, so gate 3 is green. The tree is missing a region a reader would expect,
/// and only gate 5's golden trees can say so. Recorded here rather than in a comment because a
/// claim nobody runs is a claim that stops being true.
#[test]
fn a_round_trip_gate_is_blind_to_a_lost_definition_node() {
  const SRC: &str = "type T { f: Int } type";

  let tree = parse_str(SRC);
  assert_eq!(text_of(&tree.syntax()), SRC, "the round-trip is perfect");
  let (bytes, gaps) = token_bytes(&tree.syntax());
  assert_eq!(bytes, SRC.len(), "and every byte is carried by a token");
  assert_eq!(gaps, 0, "and not one of them by a gap tile");

  let kinds = kinds_of(&tree.syntax());
  assert_eq!(
    kinds
      .iter()
      .filter(|k| **k == K::ObjectTypeDefinition)
      .count(),
    1,
    "the second `type` opened no definition node — which is the blindness this test records"
  );
  assert!(
    !kinds.contains(&K::Error),
    "and it opened no error node either, so nothing in the tree marks the region at all"
  );
  assert!(tree.has_errors(), "the diagnostic is the only witness left");
}

/// Lexical edges the corpus does not contain, and which the lexer **accepts**.
///
/// Every number radix GraphQLx admits — `LitInt` is `Decimal | Hex | Binary | Octal` and `LitFloat`
/// is `Decimal | Hex` — plus block strings with embedded quotes, escape sequences, a BOM at offset
/// 0, CRLF line endings and tabs. Each is asserted to round-trip, to be accepted, **and** to be
/// covered entirely by real tokens, which is the half that says the lexer understood the bytes
/// rather than that the sink tiled over them.
#[test]
fn the_lexical_edges_the_corpus_does_not_contain() {
  const EDGES: &[(&str, &str)] = &[
    ("decimal int", "query { f(a: 42, b: -7, c: 0) }"),
    ("hex int", "query { f(a: 0x1F, b: 0xdeadBEEF) }"),
    ("binary int", "query { f(a: 0b1010, b: 0b1) }"),
    ("octal int", "query { f(a: 0o17, b: 0o7) }"),
    ("decimal float", "query { f(a: 1.5, b: -0.25e10, c: 3E-2) }"),
    ("hex float", "query { f(a: 0x1.8p3) }"),
    ("digit separators", "query { f(a: 1_000) }"),
    (
      "block string with quotes",
      "query { f(a: \"\"\"a \"b\" c\"\"\") }",
    ),
    ("escapes", "query { f(a: \"\\u0041\\n\\t\\\\ \\\"\") }"),
    ("bom at offset 0", "\u{FEFF}type T { f: Int }"),
    ("crlf", "type T {\r\n  f: Int\r\n}\r\n"),
    ("tabs", "type T {\n\tf: Int\n}\n"),
  ];

  for (label, src) in EDGES {
    let tree = parse_str(src);
    assert_eq!(text_of(&tree.syntax()), *src, "{label}: lost bytes");
    assert!(
      !tree.has_errors(),
      "{label}: the fixture claims the lexer admits this spelling"
    );
    let (bytes, gaps) = token_bytes(&tree.syntax());
    assert_eq!(bytes, src.len(), "{label}: the tokens do not cover it");
    assert_eq!(
      gaps, 0,
      "{label}: the lexer refused a byte the fixture claims it understands"
    );
  }
}

/// A radix prefix is **lowercase only**, and the rejected spelling still round-trips.
///
/// Found by writing the fixture above with `0X`, `0B` and `0O` and watching each tile a gap. It is
/// not a divergence — the syntactic suite rejects all three too, which
/// `tests/lossless_x_parity.rs` would have caught had the sweep included a number radix; it did
/// not, and this is where the omission surfaced. The asymmetry is worth stating because it is
/// unusual: the *prefix* is case-sensitive while the hex **digits** (`0xdeadBEEF`) and the decimal
/// **exponent** marker (`3E-2`) are not.
///
/// Both halves are asserted. The rejection is the language fact; the round-trip is this gate's,
/// and it is the half that matters here — a refused numeric literal is exactly the kind of byte
/// run a parser is tempted to drop.
#[test]
fn an_uppercase_radix_prefix_is_refused_and_still_kept() {
  for src in [
    "query { f(a: 0XdeadBEEF) }",
    "query { f(a: 0B1) }",
    "query { f(a: 0O7) }",
  ] {
    let tree = parse_str(src);
    assert_eq!(text_of(&tree.syntax()), src, "{src:?}: lost bytes");
    assert!(
      tree.has_errors(),
      "{src:?}: an uppercase radix prefix is not GraphQLx"
    );
    let (bytes, _) = token_bytes(&tree.syntax());
    assert_eq!(bytes, src.len(), "{src:?}: the tokens do not cover it");
  }
}

/// A source that is nothing but a lexer error does not panic, and still round-trips.
///
/// **This was a panic in Phase A** and was fixed upstream (tokora `2bbca21`: the sink's zero-token
/// wall additionally requires an *uncovered* gap, so a fully explained source passes). The GraphQLx
/// path is a different lexer and a different kind space; that it inherits the fix is verified here
/// rather than assumed.
///
/// Both fixtures carry no trailing newline and no comment — either would lex, and a fixture that
/// lexes is not in this class.
#[test]
fn a_source_that_is_nothing_but_a_lexer_error_does_not_panic() {
  for src in ["\"unterminated", "%", "\"\"\"unterminated block"] {
    let tree = parse_str(src);
    assert_eq!(text_of(&tree.syntax()), src, "{src:?}: lost bytes");
    assert!(
      tree.has_errors(),
      "{src:?}: the lexer error must be reported"
    );
    let (bytes, gaps) = token_bytes(&tree.syntax());
    assert_eq!(bytes, src.len(), "{src:?}: the tokens do not cover it");
    assert!(
      gaps > 0,
      "{src:?}: a refused byte must reach the tree as a gap"
    );
  }
}

/// The comparison is not vacuous.
///
/// Phase A found that a `src.trim()` on one side of the round-trip **silently passed**, because no
/// witness in the corpus had surrounding whitespace to lose. Two halves close that: a padded
/// witness, whose leading and trailing trivia a trim would eat, and a negative control showing the
/// round-trip of one source is not the text of another.
#[test]
fn the_round_trip_comparison_is_not_vacuous() {
  // Leading and trailing whitespace, and a leading comment, all of which a `trim()` on either side
  // would silently drop while every existing corpus entry survived it.
  const PADDED: &str = "\n\n  # leading\n  type T { f: Int }  \n\n";
  let tree = parse_str(PADDED);
  assert_eq!(text_of(&tree.syntax()), PADDED);
  assert_ne!(
    text_of(&tree.syntax()),
    PADDED.trim(),
    "the witness has no padding to lose, so a trim on one side would pass"
  );

  // And the round-trip of one source is not the text of another: without this the comparison could
  // be against a constant that happens to equal the fixture.
  const OTHER: &str = "type U { g: String }\n";
  assert_ne!(text_of(&parse_str(OTHER).syntax()), PADDED);
  assert_eq!(text_of(&parse_str(OTHER).syntax()), OTHER);
}
