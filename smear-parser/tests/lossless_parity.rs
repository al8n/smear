#![cfg(feature = "rowan")]

//! Gate 1: acceptance parity between the lossless suite and the shipped `syntactic` one.
//!
//! The cross-suite invariant is a **verdict**: for every corpus entry, both suites must agree on
//! *whether* the input is valid GraphQL. Diagnostic sets are deliberately not compared — the
//! lossless suite recovers and is therefore more verbose by construction, and its holes report
//! `Severity::Warning` rather than an error precisely so a recovered parse still reads as
//! accepted.
//!
//! # Why this gate could not exist before Task 8
//!
//! `Parse::has_errors()` is the lossless verdict. Until Task 8 it lied: `trivia::expect` returned
//! `Err` without emitting, so an outright parse failure recorded nothing and read as a success.
//! That is fixed and mutation-proved in `lossless_document.rs`; this file consumes the result.
//!
//! # The failure mode this gate is most exposed to
//!
//! A parity suite where both sides accept everything passes forever and proves nothing. The
//! corpus alone cannot rule that out, so the defences are tests rather than comments:
//!
//! 1. [`both_verdicts_answer_in_both_directions`] — neither verdict function is a constant.
//! 2. [`the_two_suites_do_not_accept_the_same_language`] — eleven real constructs the two suites
//!    answer *differently*, held deliberately outside the corpus. This is what proves the
//!    equality assertion is not vacuous: without a witness, "they always agree" is
//!    indistinguishable from "the comparison is broken".
//! 3. The `checked_valid` / `checked_invalid` counters inside the parity test itself, which fail
//!    an all-valid or an all-invalid corpus.
//! 4. [`every_corpus_entry_declares_its_expected_verdict`] — a misspelled prefix cannot smuggle
//!    an entry into the wrong class.
//!
//! # What this gate cannot see
//!
//! A verdict is one bit. [`a_verdict_gate_is_blind_to_a_lost_definition_node`] exhibits an input
//! on which both suites agree — correctly — while the lossless tree has silently dropped a whole
//! `ObjectTypeDefinition`. Gate 3 (round-trip) is equally blind to it, because every byte
//! survives. Only gate 5's golden trees can see it.

use std::path::PathBuf;

use smear_parser::graphql::{
  GraphQL,
  ast::Document,
  error::GraphqlErrors,
  kinds::SyntaxKind as K,
  lossless::parse_str,
  syntactic::{GraphqlLexer, document},
};
use tokora::{Parse as _, Parser};

/// The lossless verdict: did the parse report a grammar **error**?
///
/// Recovery holes and warnings do not count — see `Parse::has_errors`.
fn lossless_has_errors(src: &str) -> bool {
  parse_str(src).has_errors()
}

/// The syntactic verdict: did the shipped `document` production reject the source?
///
/// # This is a whole-input verdict, not a prefix one
///
/// `tokora`'s `parse_str` does **not** check for end-of-input; it hands back whatever the
/// production returned. `document` is nonetheless a whole-input production, because its
/// `repeated_while` decider (`decide_definition_or_extension_head`) answers `Stop` only on
/// `None` — so trailing junk re-enters `definition_or_extension` and fails there rather than
/// being silently left behind. `syntactic/document/tests.rs`'s `reject_all!(document, "{ id } )")`
/// is that behaviour pinned upstream. Without it this helper would need an explicit exhaustion
/// check and `invalid_top_level_junk.graphql` would read as accepted.
fn syntactic_has_errors<'inp>(src: &'inp str) -> bool {
  Parser::with_parser::<
    'inp,
    GraphqlLexer<'inp, str>,
    Document<&'inp str>,
    GraphqlErrors<&'inp str>,
    _,
    GraphQL,
  >(document)
  .parse_str(src)
  .is_err()
}

/// Every `.graphql` file in the shared corpus, in a deterministic order.
///
/// The corpus lives beside this file because gates 2, 3 and 5 all read it; it is not this gate's
/// private fixture set.
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

/// The cross-suite invariant: both suites must agree on *whether* the input is valid GraphQL.
/// Diagnostic sets are deliberately NOT compared — the lossless suite may be more verbose.
#[test]
fn both_suites_agree_on_every_corpus_entry() {
  let mut checked_valid = 0usize;
  let mut checked_invalid = 0usize;

  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let lossless_errs = lossless_has_errors(&src);
    let syntactic_errs = syntactic_has_errors(&src);
    assert_eq!(
      lossless_errs,
      syntactic_errs,
      "{}: lossless says errors={lossless_errs}, syntactic says errors={syntactic_errs}",
      entry.display()
    );

    // Each class is asserted against **both** suites, not only against `lossless_errs`. The
    // equality above already implies the syntactic half — but it is the one assertion in this
    // loop that no corpus entry can ever make fail, precisely because the corpus is an agreeing
    // set (see `the_two_suites_do_not_accept_the_same_language`). Neutering it would therefore
    // be a silent mutation, and these two lines are what keep that mutation harmless: without
    // them, dropping the comparison would drop the syntactic suite out of the gate entirely and
    // leave a lossless-only smoke test wearing a parity gate's name.
    let name = entry.file_name().unwrap().to_string_lossy().to_string();
    if name.starts_with("invalid_") {
      assert!(
        lossless_errs,
        "{name} is named invalid but the lossless suite accepted it"
      );
      assert!(
        syntactic_errs,
        "{name} is named invalid but the syntactic suite accepted it"
      );
      checked_invalid += 1;
    } else {
      assert!(
        !lossless_errs,
        "{name} is named valid but the lossless suite rejected it"
      );
      assert!(
        !syntactic_errs,
        "{name} is named valid but the syntactic suite rejected it"
      );
      checked_valid += 1;
    }
  }

  // The positive controls. Without both, the equality assertion above can pass vacuously
  // over a corpus that is all-valid or all-invalid.
  assert!(
    checked_valid >= 20,
    "only {checked_valid} valid entries; the corpus is too thin"
  );
  assert!(
    checked_invalid >= 5,
    "only {checked_invalid} invalid entries; no negative control"
  );
}

/// Every corpus entry names the class it belongs to.
///
/// [`both_suites_agree_on_every_corpus_entry`] routes on the `invalid_` prefix and treats
/// *everything else* as valid, so a file called `invald_foo.graphql` would be silently asserted
/// to parse — and would then fail with a message pointing at the wrong thing entirely. The
/// prefix is a declaration; this is the check that it was actually made.
#[test]
fn every_corpus_entry_declares_its_expected_verdict() {
  let files = corpus_files();
  assert!(!files.is_empty(), "the corpus is empty");
  for entry in files {
    let name = entry.file_name().unwrap().to_string_lossy().to_string();
    assert!(
      name.starts_with("valid_") || name.starts_with("invalid_"),
      "{name}: a corpus entry must be named valid_* or invalid_*"
    );
  }
}

/// Neither verdict function is a constant.
///
/// The parity assertion compares two booleans. If either side always answered the same way the
/// comparison would still be *green over an agreeing corpus* while measuring nothing at all, so
/// each function is driven in both directions here, over inputs that are not corpus entries.
#[test]
fn both_verdicts_answer_in_both_directions() {
  const ACCEPTED: &str = "type T { f: Int }";
  const REJECTED: &str = "type T { f: }";

  assert!(
    !lossless_has_errors(ACCEPTED),
    "lossless rejected {ACCEPTED:?}"
  );
  assert!(
    lossless_has_errors(REJECTED),
    "lossless accepted {REJECTED:?}"
  );
  assert!(
    !syntactic_has_errors(ACCEPTED),
    "syntactic rejected {ACCEPTED:?}"
  );
  assert!(
    syntactic_has_errors(REJECTED),
    "syntactic accepted {REJECTED:?}"
  );
}

/// Constructs the lossless suite accepts and `syntactic/` rejects.
///
/// **This is the test that makes [`both_suites_agree_on_every_corpus_entry`] mean something.** A
/// parity gate over a corpus both suites happen to agree on is indistinguishable from a parity
/// gate whose comparison never runs; the difference is whether an entry *could* have separated
/// them. Each source below is such an entry, verified to divide the two suites, and deliberately
/// kept out of `tests/corpus/` so the gate stays green while the divergence stays recorded.
///
/// # The two rules involved, and why the divergence exists
///
/// Both are cases where `syntactic/` enforces a spec rule *at parse time* and the lossless suite
/// defers it to a validation pass over the finished tree:
///
/// - **Const positions** (ten witnesses). `DefaultValue` and every SDL `Directives` context
///   require a `Value[Const]`, in which a `Variable` is not a production at all. `syntactic/`
///   has a const flavour and rejects; the lossless `value` production has one shape for both
///   positions and accepts, on the recorded ground that "constness is a validation rule over the
///   tree" (`lossless/directive.rs`, `lossless/value.rs`).
/// - **`on` as a fragment name** (one witness). `FragmentName: Name but not "on"`. `syntactic/`
///   spends a whole production on the exclusion (`fragment_name`); `lossless/executable.rs`
///   consumes a plain `Identifier` and records the same deferral in a comment.
///
/// # This is a defect against gate 1's contract, not a settled ruling
///
/// Task 8 met the identical question twice and answered it the other way: `enum_value_definition`
/// and `directive_location` both **report** a reserved spelling *and* still build the node,
/// explicitly "because gate 1 compares verdicts". Reporting never prevented building — so the
/// stated rationale for deferring (keeping a node for the diagnostic to point at) does not
/// actually justify staying silent. Task 7's productions predate that ruling and never got it.
///
/// **If a later task closes the gap, this test goes red.** That is the intended failure: move
/// each witness into `tests/corpus/` as an `invalid_*.graphql` entry, where the parity gate will
/// then hold it, and shrink this test to whatever still diverges — but do not simply delete it
/// without leaving the comparison another witness, or gate 1 loses its only proof of life.
#[test]
fn the_two_suites_do_not_accept_the_same_language() {
  /// A `Variable` in each position the spec marks const. Ten distinct call sites, because the
  /// const contexts are reached through six different productions and a single witness would
  /// only pin one of them.
  const CONST_POSITION: &[&str] = &[
    "type T { f(a: Int = $v): Int }",
    "directive @d(a: Int = $v) on FIELD",
    "input I { a: Int = $v }",
    "type T @d(a: $v) { f: Int }",
    "enum E { A @d(a: $v) }",
    "schema @d(a: $v) { query: Q }",
    "scalar S @d(a: $v)",
    "type T { f: Int @d(a: $v) }",
    "query Q($v: Int = $w) { f }",
    "extend type T @d(a: $v)",
  ];

  /// `FragmentName: Name but not "on"` — `fragment on on T { f }` names the fragment `on`.
  const RESERVED_FRAGMENT_NAME: &str = "fragment on on T { f }";

  let mut witnesses = 0usize;
  for src in CONST_POSITION
    .iter()
    .copied()
    .chain([RESERVED_FRAGMENT_NAME])
  {
    let lossless_errs = lossless_has_errors(src);
    let syntactic_errs = syntactic_has_errors(src);
    assert!(
      !lossless_errs && syntactic_errs,
      "{src:?}: expected lossless to accept and syntactic to reject, got \
       lossless errors={lossless_errs}, syntactic errors={syntactic_errs}. If the lossless suite \
       has since learned this rule, move this source into tests/corpus/ as an invalid_* entry."
    );
    witnesses += 1;
  }
  assert_eq!(witnesses, 11, "a witness went missing from the loop");
}

/// The compatibility extension both suites share, pinned as a *non*-divergence.
///
/// A leading `Description` on an **executable** definition is not GraphQL — `apollo-parser`
/// rejects it — but `syntactic/`'s `described_definition` accepts it for frozen-parser
/// compatibility, and `lossless/document.rs` matches that deliberately. It is therefore a
/// divergence from the *spec*, not between the two suites, and the parity gate must stay silent
/// about it: `valid_executable_described_operation.graphql` carries it as an ordinary valid
/// entry. This test states the reason that entry is filed where it is.
///
/// The neighbouring case is the opposite ruling and is pinned the same way: a description before
/// an **extension** is rejected by both (`invalid_described_extension.graphql`), because
/// `syntactic/`'s described path commits to a definition and can never reach an extension.
#[test]
fn a_description_before_an_executable_definition_is_accepted_by_both() {
  for src in [
    "\"doc\" query Q { f }",
    "\"doc\" mutation M { f }",
    "\"doc\" fragment F on T { f }",
    "\"doc\" { f }",
  ] {
    assert!(!lossless_has_errors(src), "lossless rejected {src:?}");
    assert!(!syntactic_has_errors(src), "syntactic rejected {src:?}");
  }

  for src in [
    "\"doc\" extend type T { f: Int }",
    "\"doc\" extend scalar S @d",
  ] {
    assert!(lossless_has_errors(src), "lossless accepted {src:?}");
    assert!(syntactic_has_errors(src), "syntactic accepted {src:?}");
  }
}

/// What a verdict gate structurally cannot measure.
///
/// A definition that fails partway through its body unwinds before `node_at` spends its mark, so
/// the node is never opened and the tree keeps the bytes as bare children of `Document`.
/// `apollo-parser` opens the node first and keeps it. Recorded by Task 10 and deliberately left
/// unfixed — it is Task 8 production behaviour, and moving it would move shapes that Task 8's
/// tests and Task 13b's goldens pin.
///
/// **Gate 1 is blind to it, and so is gate 3.** Both suites reject these inputs, so the parity
/// comparison is satisfied and correct; every byte survives, so a round-trip gate is satisfied
/// and correct. A whole class of structural loss lives in the space between those two greens.
/// This test is the record of that blind spot, written as an assertion so the day the behaviour
/// changes is a day someone is told.
///
/// # The loss is not "any failure in a body"
///
/// The control set below is what makes that precise, and it was found by mutation: replacing the
/// control source with `type T { x: }` changed nothing, because that input **keeps** its
/// `ObjectTypeDefinition` — the missing type recovers in place as an `Error` child and no `Err`
/// ever unwinds. The node is lost only when a production returns `Err` that propagates past the
/// `node_at` holding the mark. Asserting both halves is what stops this test degenerating into
/// "some erroring inputs lack the node", which would be true of `{ f }` as well.
#[test]
fn a_verdict_gate_is_blind_to_a_lost_definition_node() {
  const LOST: K = K::ObjectTypeDefinition;

  /// Bodies that fail by **unwinding**: the mark is never spent and the node never opens.
  const LOSES_THE_NODE: &[&str] = &[
    "type T { \"\"\"b\"\"\" \"a\" }",
    "type { x: Int }",
    "type T { : Int }",
    "type T { x Int }",
    // Padded, so the round-trip assertion below has something to be wrong about: over
    // whitespace-free witnesses alone, comparing against `src.trim()` is indistinguishable from
    // comparing against `src` (mutation, in-space silent pass).
    "\n  type T { x Int }\n",
  ];

  /// The control, and the sharp edge of the finding: these fail too — the parity gate rejects
  /// every one of them — yet each keeps its `ObjectTypeDefinition`, because each recovers in
  /// place instead of unwinding. Without this half, `!contains` would be satisfiable by any
  /// input at all that happens not to be an object type.
  const KEEPS_THE_NODE: &[&str] = &["type T { x: }", "type T { x: Int", "type T {}"];

  fn kinds_of(src: &str) -> Vec<K> {
    parse_str(src)
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect()
  }

  for src in LOSES_THE_NODE.iter().copied() {
    // Both suites reject — so the parity loop above would pass this entry without comment.
    assert!(lossless_has_errors(src), "lossless accepted {src:?}");
    assert!(syntactic_has_errors(src), "syntactic accepted {src:?}");

    // Every byte is still there — so a round-trip gate would pass it without comment too.
    assert_eq!(
      parse_str(src).syntax().text().to_string(),
      src,
      "{src:?}: the text did not round-trip"
    );

    // And yet the node is gone.
    let kinds = kinds_of(src);
    assert!(
      !kinds.contains(&LOST),
      "{src:?}: expected no {LOST:?} in {kinds:?} — if the node is back, this blind spot has \
       been closed and the test should become the assertion that it stays closed"
    );
  }

  for src in KEEPS_THE_NODE.iter().copied() {
    assert!(
      lossless_has_errors(src),
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
