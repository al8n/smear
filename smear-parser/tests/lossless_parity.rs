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
//! 2. [`the_corpus_can_tell_the_two_suites_apart`] — the corpus entries are *discriminating
//!    material*: twenty-eight of them separate a lossless production from a syntactic one. This
//!    is what proves the equality assertion is not vacuous, and it replaces the eleven divergence
//!    witnesses that used to carry that burden — see below.
//! 3. The `checked_valid` / `checked_invalid` counters inside the parity test itself, which fail
//!    an all-valid or an all-invalid corpus.
//! 4. [`every_corpus_entry_declares_its_expected_verdict`] — a misspelled prefix cannot smuggle
//!    an entry into the wrong class.
//!
//! # The eleven divergence witnesses are gone, and where the proof of life went
//!
//! Task 11 measured eleven constructs the lossless suite accepted and `syntactic/` rejected — a
//! `Variable` in ten `Value[Const]` positions, and `on` as a `FragmentName` — and pinned them in
//! a test *because they were also the only evidence this gate's comparison could ever fail*.
//! Both rules are grammar rules rather than validation rules, so the lossless suite learned them:
//! `value.rs`'s `Constness` parameter and `executable.rs`'s fragment-name check, each reporting
//! the error **and still building the node**, which is the ruling Task 8 had already made twice.
//! All eleven now live in `tests/corpus/` as ordinary `invalid_*` entries that this gate holds.
//!
//! That left the gate with nothing separating the two suites — over 1,518 probed sources, none
//! remains — so the proof of life had to stop being a *language* divergence. It is now a
//! **production** divergence, which is permanent by construction: the lossless suite's SDL-only
//! root rejects every executable definition, while `syntactic/`'s mixed root accepts them, so the
//! corpus's twenty-eight executable entries split the two suites on demand. The gate is green
//! because `parse_str`'s root and `document` describe the same language — not because the corpus
//! is inert. Closing *that* divergence would be a bug, so unlike the eleven it cannot quietly go
//! away.
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
  lossless::{document::test_support::parse_type_system_document, parse_str},
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

/// The corpus is discriminating material, and the syntactic verdict is a parse of its own.
///
/// **This is the test that makes [`both_suites_agree_on_every_corpus_entry`] mean something**, and
/// it is the replacement for the eleven divergence witnesses Task 11 pinned — see the module docs
/// for why they are gone. A parity gate over a corpus both suites happen to agree on is
/// indistinguishable from a parity gate whose comparison never runs; the difference is whether an
/// entry *could* separate them. Two independent halves establish that it could.
///
/// # Half one: the entries themselves split the two suites
///
/// The lossless suite has two roots. `parse_str` drives the **mixed** one, which is the language
/// `syntactic/`'s `document` also describes — that agreement is what the gate measures. Its
/// sibling `type_system_document` is the **SDL-only** root, and it rejects every executable
/// definition. So each corpus entry that carries one is an input on which a lossless production
/// and a syntactic production answer *differently*: the material discriminates, and the gate is
/// green because the two roots under comparison describe the same language rather than because
/// the corpus is inert.
///
/// **This divergence is permanent, which the eleven were not.** Closing it would mean an SDL-only
/// root that accepts `query Q { f }`, which is a bug rather than a fix — so unlike a grammar gap
/// waiting to be filled, this proof of life cannot be closed out from under the gate.
///
/// The floor is asserted against the corpus's own executable content rather than fixed at
/// twenty-eight, so adding an SDL entry cannot red this test and deleting every executable one
/// must.
///
/// # Half two: the syntactic verdict is not the lossless one wearing a hat
///
/// The most complete way for this gate to die is for [`syntactic_has_errors`] to stop consulting
/// the syntactic suite at all — after which every assertion in the file still passes, half one
/// included, because half one never calls it on a source it does not also parse losslessly. So
/// the syntactic side is exercised through its **output** rather than its `is_err()` bit: a
/// `Document` whose definitions can be counted cannot be produced by a `rowan` parse, and an
/// alias would fail to typecheck long before it failed an assertion.
#[test]
fn the_corpus_can_tell_the_two_suites_apart() {
  // Half one. `parse_type_system_document` is the SDL-only root's driver — the same production
  // set, entered one level up from `document`.
  let mut split = 0usize;
  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let sdl_rejects = parse_type_system_document(&src).has_errors();
    let syntactic_accepts = !syntactic_has_errors(&src);
    if sdl_rejects && syntactic_accepts {
      // The two suites answer differently about this very byte string.
      split += 1;
      // …and the mixed lossless root agrees with `syntactic/` about it, which is the whole
      // point: the disagreement is between the *roots*, not between the suites.
      assert!(
        !lossless_has_errors(&src),
        "{}: the SDL root rejects it and syntactic accepts it, but so does the mixed lossless \
         root — this entry is not the clean discriminator it is being counted as",
        entry.display()
      );
    }
  }
  assert!(
    split >= 20,
    "only {split} corpus entries separate the SDL-only lossless root from the syntactic one; \
     the corpus has lost its executable half and this gate can no longer show its comparison is \
     live"
  );

  // Half two. Not `is_err()`: the value on the `Ok` side is what a lossless parse cannot forge.
  const TWO_DEFINITIONS: &str = "query Q { f } fragment F on T { g }";
  let parsed = Parser::with_parser::<
    '_,
    GraphqlLexer<'_, str>,
    Document<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(document)
  .parse_str(TWO_DEFINITIONS)
  .expect("the syntactic suite must accept two ordinary executable definitions");

  // The count is checked against the **lossless** tree's own, not against a literal. Both sides
  // are then live values drawn from different suites, so the assertion cannot be neutered into
  // a self-comparison the way a literal one can — the mutation the parity loop above records as
  // its own structural blind spot. The literal anchor follows, so an agreement at zero on both
  // sides is still a failure.
  let lossless_definitions = parse_str(TWO_DEFINITIONS)
    .syntax()
    .descendants()
    .filter(|n| matches!(n.kind(), K::OperationDefinition | K::FragmentDefinition))
    .count();
  assert_eq!(
    parsed.definitions().len(),
    lossless_definitions,
    "the syntactic verdict must come from a real syntactic parse of the same bytes"
  );
  assert_eq!(
    lossless_definitions, 2,
    "both suites must have found the two definitions that are there"
  );
}

/// The eleven constructs Task 11 measured as divergences, now agreed **and still whole**.
///
/// Each is a corpus entry, so [`both_suites_agree_on_every_corpus_entry`] already holds the
/// verdict. What a corpus entry cannot hold is the other half of the ruling: the rejection is a
/// *diagnostic*, and the tree is built anyway. Asserting the verdict alone would pass just as
/// well if the lossless suite had learned to reject these by bailing out — which would cost the
/// nodes and the bytes a lossless consumer exists for. So the text is asserted too, byte for
/// byte, against the file the corpus keeps.
#[test]
fn the_eleven_former_divergences_are_rejected_without_losing_their_text() {
  const WITNESSES: &[&str] = &[
    "invalid_const_default_in_arguments_definition.graphql",
    "invalid_const_default_in_directive_definition.graphql",
    "invalid_const_default_in_input_object.graphql",
    "invalid_const_default_in_variable_definition.graphql",
    "invalid_const_directive_on_object_type.graphql",
    "invalid_const_directive_on_enum_value.graphql",
    "invalid_const_directive_on_schema.graphql",
    "invalid_const_directive_on_scalar.graphql",
    "invalid_const_directive_on_field_definition.graphql",
    "invalid_const_directive_on_extension.graphql",
    "invalid_fragment_named_on.graphql",
  ];

  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  for name in WITNESSES {
    let path = dir.join(name);
    let src = std::fs::read_to_string(&path)
      .unwrap_or_else(|e| panic!("{name} must exist in the shared corpus: {e}"));

    assert!(
      lossless_has_errors(&src),
      "{name}: the lossless suite accepted it — the const or fragment-name rule has regressed"
    );
    assert!(
      syntactic_has_errors(&src),
      "{name}: the syntactic suite accepted it — this was never a divergence"
    );
    assert_eq!(
      parse_str(&src).syntax().text().to_string(),
      src,
      "{name}: a rejected parse still keeps every byte"
    );
  }
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
