#![cfg(all(feature = "validator", feature = "rowan"))]

//! The differential gate for the validator's lossless **schema** door.
//!
//! # The claim, and the instrument
//!
//! `validate_schema_lossless(parse_type_system_document(sdl), sdl) ==
//! Schema::build(type_system_document(sdl))` — **the same answer**, as a value. Refused, it is the
//! same `Vec<SchemaError>`: [`SchemaError`] derives [`PartialEq`] over its kind, its subject, its
//! owner, its span, its related span and its document index, and the comparison is `assert_eq!`
//! over the whole vector, so order is in it too. Built, it is the same `Schema`, compared through
//! its `Debug` rendering because that is the whole of what the type publishes about itself.
//!
//! The two doors are one builder by construction — the lossless one projects the CST to the
//! `TypeSystemDocument` and calls `Schema::build` — so what this gate actually measures is the
//! **projection**: that the AST reached through the tree is the AST reached through the parser, at
//! every byte a refusal can point at. That is the single most likely place for a silent defect,
//! because both paths produce *a* span and only a comparison shows they differ.
//!
//! `tests/validator_lossless.rs` is this file's twin one section of the specification over, and
//! everything below is deliberately shaped like it: same corpus module, same trivia alphabet, same
//! four ways to pass without meaning anything.
//!
//! # The corpus is the refusal corpus
//!
//! `tests/support/validator_corpus.rs`'s `SCHEMA_FIXTURES` — one entry per draft §3 refusal kind,
//! each an SDL that makes it fire with the complete set of kinds that SDL produces.
//! `validator_schema.rs`'s `refusal_floor` is what keeps it complete; this file reads it so the
//! differential inherits that completeness instead of being a second, narrower corpus that happens
//! to agree with itself. The **accepted** direction comes from the same module's §5 material — the
//! specification's menagerie and the two per-fixture overrides — which are valid schemas by
//! construction, because every §5 fixture is validated against one.
//!
//! Each document is run **nine times**: compact, and padded at every token boundary with each of
//! `tests/support/span_extent.rs`'s eight ignorable forms.
//!
//! # What the padding is worth, measured rather than claimed
//!
//! Trivia is the only material on which a `text_range` span differs from a token-extent one, so
//! the usual statement is that the compact half of a corpus cannot catch that defect at all. It is
//! **not true here**, for `validator_lossless.rs`'s reason: the fixtures are hand-written GraphQL
//! with spaces in it, so "compact" already means "padded wherever the author typed a space".
//!
//! The measurement, taken by making the projection's `extent` return the node's own range: 1 of
//! the 58 compact documents disagrees, and 8 of the 464 padded ones. The padding is worth roughly
//! eight times the catch rate, not the whole of it — the same ratio the executable gate measured —
//! and [`the_padding_is_load_bearing`] pins the mechanism behind it: after injection, **every**
//! refusal the corpus produces sits next to trivia.
//!
//! Both numbers are small because a §3 refusal usually blames a *name token*, whose span no
//! composite-extent rule touches. Composite spans are compared exhaustively somewhere else and on
//! purpose: `lossless_project.rs`'s `the_type_system_projection_equals_the_parse_over_the_shared_corpus`
//! asserts the projected AST equals the parsed one as a **value**, every span of every node, over
//! the parser's own corpus. This gate is the refusal-level statement on top of that, not a
//! replacement for it.
//!
//! # The four ways a gate like this passes without meaning anything
//!
//! 1. **A comparison that ignores what matters.** [`the_comparison_can_answer_no`] feeds it the
//!    same SDL one byte later, an SDL with a different name in it, and the same refusals in the
//!    wrong order, and requires all three to compare unequal.
//! 2. **A defect at a site no fixture reaches.** [`a_one_byte_shift_is_caught_for_every_fixture`]
//!    plants the span perturbation at **each** fixture's own refusal site rather than at whichever
//!    one the author picked, and requires the comparison to notice every time. It is this file's
//!    stand-in for `validator_lossless.rs`'s per-rule drop: draft §3 has no `RuleSet` to switch a
//!    rule off with, because both doors reach the rules through one `Schema::build`, so the only
//!    thing that *can* differ between the doors is the document — which is what is perturbed.
//! 3. **A corpus that produces nothing to compare.** [`the_agreement_is_not_vacuous`] counts the
//!    refusals and the distinct kinds each door actually produced, separately, and floors both.
//! 4. **A door nobody can reach with a broken document.** [`a_half_typed_schema_still_gets_an_answer`]
//!    truncates every corpus entry at every token boundary and requires the door to keep
//!    answering — which is the entire reason the lossless leg exists.
//!
//! # One refusal an editor cannot underline
//!
//! [`POSITIONLESS`] records it: `MissingQueryRootOperationType` is reported at `0..0` with the
//! subject `query`, because "this document has no query root" is a statement about the document
//! and not about any span in it. Both doors report it identically — it is `Schema::build`'s
//! answer, and that is the only thing this file has an opinion about — but a consumer rendering
//! §3 into an editor's gutter has nothing to attach it to. Named here rather than left to be
//! rediscovered, and checked rather than described: 57 of the 58 fixtures blame real bytes, and
//! the census reds if a second one stops doing so.
//!
//! # What is deliberately not compared
//!
//! An SDL the **syntactic** parser rejects. It has no AST, so `Schema::build` has no input, so
//! there is no second value for a differential to hold against. That case is the lossless door's
//! own — see the recovery tests at the bottom of this file, which pin it against stated
//! expectations rather than against an oracle that does not exist.

#![allow(missing_docs)]

use std::{collections::BTreeSet, string::String, vec::Vec};

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::TypeSystemDocument,
    error::GraphqlErrors,
    lossless::parse_type_system_document,
    syntactic::{GraphqlLexer, type_system_document},
  },
  validator::{Recovery, Refusal, Schema, SchemaError, SchemaErrorKind, validate_schema_lossless},
};

// The two corpora, shared with `validator_schema.rs` (which owns the census that keeps the §3 half
// complete) and `validator_rules.rs` (which owns the §5 half's). Only three of this module's
// exports are wanted here; the rest would be `dead_code` denials under CI's `-Dwarnings`.
#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

// The trivia alphabet and injector, shared with `syntactic_span_extent.rs`,
// `lossless_project.rs` and `validator_lossless.rs`.
#[allow(dead_code)]
#[path = "support/span_extent.rs"]
mod extent;

use corpus::{FIXTURES, QUERY_ONLY, SCHEMA, SCHEMA_FIXTURES};
use extent::{ALPHABET, inject};

// ---------------------------------------------------------------------------------------------
// floors
// ---------------------------------------------------------------------------------------------

// Every number below is the measurement on the day this was written, taken as a floor. They are
// what stops the sweep going quiet: a corpus that stopped producing refusals, or stopped reaching
// a kind, agrees with itself perfectly and says nothing at all.
//
// They are stated against the rule set **this branch's base has**, which is 60 kinds, 58 of them
// fireable. Draft §3 is still growing — smear#102 adds seven more on a sibling branch — and each
// new kind arrives with its own fixture, so every number here moves up when it lands. That is the
// point of a floor.

/// The smallest number of `(document, trivia form)` pairs the refusal sweep is allowed to compare.
///
/// 58 fixtures x 9 forms.
const COMPARISON_FLOOR: usize = 522;

/// The smallest number of **refusals** the sweep is allowed to have compared.
const REFUSAL_FLOOR: usize = 522;

/// The smallest number of distinct refusal kinds the compared errors are allowed to reach.
///
/// The corpus holds one entry per fireable kind and `validator_schema.rs` asserts that, so
/// anything below it means the differential lost coverage the refusal floor still claims.
const KIND_FLOOR: usize = 58;

/// The smallest number of `(schema, trivia form)` pairs the **accepted** sweep is allowed to
/// compare.
///
/// Two distinct schemas x 9 forms, which is every valid SDL the §5 corpus holds: the
/// specification's whole menagerie — six type kinds, extensions of none, `@oneOf`, list and
/// non-null wrappers, arguments with defaults, a repeatable directive — and the one-field root the
/// two schema-override fixtures share. Small because the corpus is, and it is not the only
/// statement about the accepted direction: [`the_two_doors_agree_over_the_parser_corpus`] runs
/// every SDL shape the parser's own corpus has through the same comparison, and
/// `lossless_project.rs` compares the projected AST against the parsed one as a **value** over
/// that corpus, which is the stronger claim and the one that would catch a dropped field.
const ACCEPTED_FLOOR: usize = 18;

/// The smallest number of `(entry, trivia form)` pairs the parser corpus contributes.
///
/// Measured at 28 of the corpus's 56 `valid_` entries reaching the SDL root — the rest are
/// executable, which that root reports rather than accepts — times nine forms, none of which the
/// padding cost.
const PARSER_CORPUS_FLOOR: usize = 252;

/// The smallest number of compared refusals whose span sits at a **discriminating** site — one
/// where trivia is adjacent, so a token-extent span and a `text_range` span differ.
///
/// Measured at 464 of 464: after padding at every token boundary, *every* refusal the corpus
/// produces is at a site where the two span rules disagree.
const DISCRIMINATING_FLOOR: usize = 464;

/// The refusals draft §3 reports about the **document as a whole**, which have no artifact in it
/// to point at.
///
/// An excuse list, checked rather than asserted, so a *new* positionless refusal is a failure
/// instead of a silent skip — [`a_one_byte_shift_is_caught_for_every_fixture`] requires the set it
/// finds to be exactly this one.
///
/// One entry, and it is a real limit of what an editor can render: "this document has no query
/// root" is not a claim about any span in it, and `Schema::build` reports it at `0..0` with the
/// subject `query`. That is trunk's behaviour, reproduced here rather than corrected — the door
/// under test hands the same document to the same builder, so a span invented on this side would
/// be a divergence between the two doors, which is the one thing this file exists to refuse.
const POSITIONLESS: &[SchemaErrorKind] = &[SchemaErrorKind::MissingQueryRootOperationType];

// ---------------------------------------------------------------------------------------------
// the two doors
// ---------------------------------------------------------------------------------------------

/// What a door answered about one SDL.
///
/// One value rather than a `Result`, so a door that *built* a schema the other door *refused* is
/// caught by the same `assert_eq!` as a door that refused it differently, instead of by a second
/// assertion somebody has to remember to write.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Answer {
  /// The SDL is a schema, rendered as everything the type publishes about itself.
  Built(String),
  /// The SDL is not a schema, and these are the reasons in the builder's own order.
  Refused(Vec<SchemaError>),
}

impl Answer {
  fn of(result: Result<Schema, Vec<SchemaError>>) -> Self {
    match result {
      Ok(schema) => Self::Built(std::format!("{schema:#?}")),
      Err(errors) => Self::Refused(errors),
    }
  }

  fn refusals(&self) -> &[SchemaError] {
    match self {
      Self::Built(_) => &[],
      Self::Refused(errors) => errors,
    }
  }
}

/// The syntactic door: parse to an AST, build the schema from the AST.
fn syntactic(sdl: &str) -> Answer {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .unwrap_or_else(|errors| panic!("the syntactic parser rejects it: {errors:?}\n---\n{sdl}"));

  Answer::of(Schema::build(&document).map_err(|errors| errors.errors().to_vec()))
}

/// The lossless door: parse to a CST, hand the CST and its text to the validator.
fn lossless(sdl: &str) -> (Answer, Recovery) {
  let parse = parse_type_system_document(sdl);
  match validate_schema_lossless(&parse, sdl) {
    Ok((schema, recovery)) => (Answer::of(Ok(schema)), recovery),
    Err(refused) => {
      let recovery = refused
        .recovery()
        .expect("the pair matches, so the build was asked");
      let errors = refused
        .errors()
        .expect("the pair matches, so the build was asked")
        .errors()
        .to_vec();
      assert!(
        !errors.is_empty(),
        "the lossless door returned `Err` with no refusals in it\n---\n{sdl}"
      );
      // The `Err` arm's own accessors agree with each other, so a caller reading either one is
      // reading the same verdict.
      assert_eq!(
        refused
          .errors()
          .expect("the pair matches, so the build was asked")
          .len(),
        errors.len(),
        "the refusal list and its length disagree\n---\n{sdl}"
      );
      (Answer::of(Err(errors)), recovery)
    }
  }
}

/// `assert_eq!` over two answers, with a message a human can read when the `Built` arm differs.
fn assert_same(expected: &Answer, actual: &Answer, context: &str, sdl: &str) {
  match (expected, actual) {
    (Answer::Built(left), Answer::Built(right)) => {
      if left != right {
        let at = left
          .lines()
          .zip(right.lines())
          .position(|(a, b)| a != b)
          .unwrap_or_else(|| left.lines().count().min(right.lines().count()));
        let show = |text: &String| text.lines().nth(at).unwrap_or("<end>").to_string();
        panic!(
          "{context}: the two doors built different schemas; first difference at line {at}\n  \
           syntactic: {}\n  lossless:  {}\n---\n{sdl}",
          show(left),
          show(right)
        );
      }
    }
    _ => assert_eq!(
      expected, actual,
      "{context}: the two doors disagree\n---\n{sdl}"
    ),
  }
}

/// Every token boundary in `sdl`, read off the lossless tree rather than from a second lexer.
fn boundaries(sdl: &str) -> Vec<usize> {
  let parse = parse_type_system_document(sdl);
  let mut out = vec![0usize];
  for element in parse.syntax().descendants_with_tokens() {
    if let Some(token) = element.into_token() {
      out.push(usize::from(token.text_range().end()));
    }
  }
  out.sort_unstable();
  out.dedup();
  out
}

/// Every `(document, trivia form)` pair a fixture contributes: compact, then one per form.
fn forms(sdl: &str) -> Vec<(&'static str, String)> {
  let marks = boundaries(sdl);
  std::iter::once(("compact", sdl.to_string()))
    .chain(
      ALPHABET
        .iter()
        .map(|(form, pad)| (*form, inject(sdl, &marks, pad))),
    )
    .collect()
}

/// Every SDL in the corpus that is **not** a schema, with the kind it exists for.
fn refused_documents() -> &'static [(SchemaErrorKind, &'static str, &'static [SchemaErrorKind])] {
  SCHEMA_FIXTURES
}

/// Every SDL in the corpus that **is** a schema.
///
/// The §5 corpus's own material: each fixture is validated against a schema, so each fixture's
/// schema is one by construction. Deduplicated, because most fixtures share the menagerie.
fn accepted_documents() -> Vec<&'static str> {
  let mut out = vec![SCHEMA, QUERY_ONLY];
  for fixture in FIXTURES {
    if let Some(sdl) = fixture.schema
      && !out.contains(&sdl)
    {
      out.push(sdl);
    }
  }
  out
}

// ---------------------------------------------------------------------------------------------
// the core assertion
// ---------------------------------------------------------------------------------------------

/// The two doors produce the same answer, as values, over the whole refusal corpus.
#[test]
fn the_two_doors_agree() {
  let mut compared = 0usize;
  let mut refusals = 0usize;
  let mut kinds: BTreeSet<SchemaErrorKind> = BTreeSet::new();

  for (kind, sdl, _) in refused_documents() {
    for (form, padded) in forms(sdl) {
      let expected = syntactic(&padded);
      let (actual, recovery) = lossless(&padded);

      assert!(
        recovery.is_complete(),
        "{kind:?} ({form}): the projection dropped {} top-level element(s) of an SDL the \
         syntactic parser accepts\n---\n{padded}",
        recovery.skipped()
      );
      assert_same(
        &expected,
        &actual,
        &std::format!("{kind:?} ({form})"),
        &padded,
      );

      refusals += expected.refusals().len();
      kinds.extend(expected.refusals().iter().map(SchemaError::kind));
      compared += 1;
    }
  }

  assert_eq!(
    compared,
    SCHEMA_FIXTURES.len() * (ALPHABET.len() + 1),
    "the sweep did not run every form over every fixture"
  );
  assert!(
    compared >= COMPARISON_FLOOR,
    "only {compared} comparisons, floor is {COMPARISON_FLOOR}"
  );
  assert!(
    refusals >= REFUSAL_FLOOR,
    "only {refusals} refusals compared, floor is {REFUSAL_FLOOR}"
  );
  assert!(
    kinds.len() >= KIND_FLOOR,
    "the compared refusals reach only {} kinds, floor is {KIND_FLOOR}: {kinds:?}",
    kinds.len()
  );
}

/// The two doors build the **same schema** out of every SDL that is one.
///
/// The direction a refusal-only differential cannot see, and the one an editor spends most of its
/// time in: an SDL that builds. A projection that quietly dropped a field, an argument default or
/// a directive location would produce no refusal at all on either side and pass every assertion
/// above.
#[test]
fn the_two_doors_build_the_same_schema() {
  let mut compared = 0usize;

  for sdl in accepted_documents() {
    for (form, padded) in forms(sdl) {
      let expected = syntactic(&padded);
      assert!(
        matches!(expected, Answer::Built(_)),
        "({form}) an SDL the §5 corpus validates against is not a schema\n---\n{padded}"
      );
      let (actual, recovery) = lossless(&padded);
      assert!(
        recovery.is_complete(),
        "({form}): the projection dropped {} top-level element(s) of a clean SDL\n---\n{padded}",
        recovery.skipped()
      );
      assert_same(
        &expected,
        &actual,
        &std::format!("accepted ({form})"),
        &padded,
      );
      compared += 1;
    }
  }

  assert!(
    compared >= ACCEPTED_FLOOR,
    "only {compared} accepted comparisons, floor is {ACCEPTED_FLOOR}"
  );
}

/// The two doors agree over every SDL shape the **parser's** own corpus has.
///
/// The refusal corpus is one small SDL per §3 kind, so it is broad in rules and narrow in syntax:
/// no descriptions, no block strings, barely an extension, and no value shape a default can hold.
/// `tests/corpus/valid_*.graphql` is the opposite — it is the corpus `lossless_project.rs`,
/// `lossless_trivia.rs` and `syntactic_span_extent.rs` all read, and it exists to cover the
/// grammar — so running it through this comparison is what puts a directive on an enum value, a
/// const object in an input field's default and a description on an argument in front of the two
/// doors.
///
/// Every entry is a **fragment** of a schema rather than a schema — none of them defines a query
/// root, and without one every answer would be the same positionless
/// [`MissingQueryRootOperationType`](SchemaErrorKind::MissingQueryRootOperationType) and the sweep
/// would compare 252 copies of a span at `0..0`. So each entry is prefixed with a root, once, and
/// what is compared is the answer about the whole. The prefix is a constant, so every offset in
/// the entry is shifted by the same amount through both doors and nothing about the comparison
/// changes.
///
/// The entries the SDL root rejects are filtered out, not listed, so an entry added to the corpus
/// later joins this sweep without anybody editing a table.
#[test]
fn the_two_doors_agree_over_the_parser_corpus() {
  let mut compared = 0usize;
  let mut refusals = 0usize;
  let mut built = 0usize;
  let mut kinds: BTreeSet<SchemaErrorKind> = BTreeSet::new();

  for (name, sdl) in parser_corpus() {
    for (form, padded) in forms(&sdl) {
      // Padding can turn an entry the SDL root took into one it does not; skip those rather than
      // assert, because which trivia a production tolerates is `lossless_trivia.rs`'s contract.
      let parse = parse_type_system_document(&padded);
      if parse.has_errors() {
        continue;
      }
      let expected = syntactic(&padded);
      let (actual, recovery) = lossless(&padded);
      assert!(
        recovery.is_complete(),
        "{name} ({form}): the projection dropped {} element(s) of a clean SDL\n---\n{padded}",
        recovery.skipped()
      );
      assert_same(
        &expected,
        &actual,
        &std::format!("{name} ({form})"),
        &padded,
      );

      match &expected {
        Answer::Built(_) => built += 1,
        Answer::Refused(errors) => {
          refusals += errors.len();
          kinds.extend(errors.iter().map(SchemaError::kind));
        }
      }
      compared += 1;
    }
  }

  assert!(
    compared >= PARSER_CORPUS_FLOOR,
    "only {compared} parser-corpus comparisons, floor is {PARSER_CORPUS_FLOOR}"
  );
  // Both directions have to be present, or the sweep is measuring one of them twice — and the
  // refusals have to reach something other than the positionless kind, or the widened syntax
  // bought no widened spans.
  assert!(built > 0, "no parser-corpus entry is a schema");
  assert!(
    refusals > 0,
    "the parser corpus produced no refusals at all"
  );
  assert!(
    kinds.iter().any(|kind| !POSITIONLESS.contains(kind)),
    "every parser-corpus refusal is positionless, so this sweep compares no spans: {kinds:?}"
  );
}

/// Every `valid_` entry of the parser's shared corpus that the SDL root accepts, prefixed with a
/// query root so the result is a whole document rather than a fragment.
///
/// Discovered rather than listed, exactly as `lossless_project.rs`'s executable sweep discovers
/// its own subset.
fn parser_corpus() -> Vec<(String, String)> {
  let dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<std::path::PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| panic!("the shared corpus at {} is unreadable: {e}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .filter(|path| {
      path
        .file_name()
        .is_some_and(|name| name.to_string_lossy().starts_with("valid_"))
    })
    .collect();
  files.sort();

  files
    .into_iter()
    .map(|path| {
      let name = path
        .file_name()
        .expect("a corpus entry has a file name")
        .to_string_lossy()
        .to_string();
      let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{} is unreadable: {e}", path.display()));
      (name, std::format!("{ROOTED}{src}"))
    })
    .filter(|(_, src)| !parse_type_system_document(src).has_errors())
    .collect()
}

/// The query root prefixed to each parser-corpus entry. A constant, so every offset in the entry
/// moves by the same amount through both doors.
const ROOTED: &str = "type Query { ok: Int }\n";

// ---------------------------------------------------------------------------------------------
// non-vacuity
// ---------------------------------------------------------------------------------------------

/// The corpus produces refusals on **both** sides, and reaches every kind through both doors.
///
/// [`the_two_doors_agree`] counts what the syntactic door produced, which is the right thing to
/// floor when the two are equal. This one counts them separately, so a lossless door that silently
/// refused nothing could not hide behind an equality that happened to hold because both sides were
/// empty.
#[test]
fn the_agreement_is_not_vacuous() {
  let mut syntactic_total = 0usize;
  let mut lossless_total = 0usize;
  let mut syntactic_kinds: BTreeSet<SchemaErrorKind> = BTreeSet::new();
  let mut lossless_kinds: BTreeSet<SchemaErrorKind> = BTreeSet::new();
  let mut documents_with_refusals = 0usize;

  for (_, sdl, _) in refused_documents() {
    for (_, padded) in forms(sdl) {
      let expected = syntactic(&padded);
      let (actual, _) = lossless(&padded);
      syntactic_total += expected.refusals().len();
      lossless_total += actual.refusals().len();
      syntactic_kinds.extend(expected.refusals().iter().map(SchemaError::kind));
      lossless_kinds.extend(actual.refusals().iter().map(SchemaError::kind));
      if !actual.refusals().is_empty() {
        documents_with_refusals += 1;
      }
    }
  }

  assert!(
    syntactic_total >= REFUSAL_FLOOR,
    "the syntactic door produced only {syntactic_total} refusals over the corpus"
  );
  assert!(
    lossless_total >= REFUSAL_FLOOR,
    "the lossless door produced only {lossless_total} refusals over the corpus"
  );
  assert_eq!(syntactic_total, lossless_total);
  assert_eq!(syntactic_kinds, lossless_kinds);
  assert!(
    syntactic_kinds.len() >= KIND_FLOOR,
    "the corpus reaches only {} kinds through the doors, floor is {KIND_FLOOR}",
    syntactic_kinds.len()
  );
  assert_eq!(
    documents_with_refusals,
    SCHEMA_FIXTURES.len() * (ALPHABET.len() + 1),
    "every fixture in the refusal corpus should refuse, in every form"
  );
}

// ---------------------------------------------------------------------------------------------
// control 1 — the comparison can answer no
// ---------------------------------------------------------------------------------------------

/// The `Vec<SchemaError>` comparison notices a moved span, a different subject and a different
/// order.
///
/// Without this, every assertion in this file would be discounted by however much of a
/// [`SchemaError`] `PartialEq` does not read.
#[test]
fn the_comparison_can_answer_no() {
  let compact = "type Query { ok: Nope }";
  let shifted = " type Query { ok: Nope }";
  let renamed = "type Query { ok: Missing }";

  let a = syntactic(compact);
  assert!(!a.refusals().is_empty(), "the control SDL is a schema");

  // The positive leg first, so "everything is unequal" cannot be why the rest passes.
  let (again, recovery) = lossless(compact);
  assert_eq!(a, again);
  assert!(recovery.is_complete());

  let (b, _) = lossless(shifted);
  assert_ne!(
    a, b,
    "the same SDL one byte later compares equal, so spans are not being compared and this gate \
     cannot see a span defect at all"
  );

  let (c, _) = lossless(renamed);
  assert_ne!(
    a, c,
    "two SDL differing only in a type name compare equal, so subjects are not being compared"
  );

  // Order. A fixture with two refusals is what makes this reachable.
  let multi = "type Query { ok: Nope other: AlsoNope }";
  let ordered = syntactic(multi);
  assert!(
    ordered.refusals().len() >= 2,
    "the order control needs an SDL with at least two refusals, got {}",
    ordered.refusals().len()
  );
  let mut reversed = ordered.refusals().to_vec();
  reversed.reverse();
  assert_ne!(
    ordered,
    Answer::Refused(reversed),
    "the same refusals in the opposite order compare equal, so the doors could disagree on order \
     and this gate would not say so"
  );

  // And the `Built`/`Refused` seam, so a door that built what the other refused is caught by the
  // same comparison rather than slipping between two of them.
  assert_ne!(
    a,
    syntactic("type Query { ok: Int }"),
    "a refused answer compares equal to a built one"
  );
}

// ---------------------------------------------------------------------------------------------
// control 2 — the perturbation is planted at every fixture, not at one
// ---------------------------------------------------------------------------------------------

/// For **every** fixture in the corpus, shifting the lossless side one byte reds the comparison.
///
/// `validator_lossless.rs`'s per-rule control switches a rule off on one side; draft §3 has no
/// `RuleSet` to do that with, because both doors reach every rule through one `Schema::build`. The
/// only thing that can differ between these two doors is the **document** the builder is handed,
/// so the perturbation is a document one — and it is planted at each fixture's own refusal site
/// rather than at whichever one an author picked, which is what makes it a census instead of an
/// anecdote.
#[test]
fn a_one_byte_shift_is_caught_for_every_fixture() {
  let mut caught = 0usize;
  let mut excused: BTreeSet<SchemaErrorKind> = BTreeSet::new();

  for (kind, sdl, _) in refused_documents() {
    let expected = syntactic(sdl);

    // A fixture whose every refusal is a zero-width whole-document one has nothing for a shift to
    // move, so it cannot be perturbed this way. Excused, and only if the excuse was written down.
    if expected
      .refusals()
      .iter()
      .all(|error| error.span().start() == error.span().end())
    {
      for error in expected.refusals() {
        assert!(
          POSITIONLESS.contains(&error.kind()),
          "{kind:?}: {:?} carries no position and is not on the POSITIONLESS list — either it \
           gained a span (delete the excuse) or a new refusal lost one (write the reason \
           down)\n---\n{sdl}",
          error.kind()
        );
        excused.insert(error.kind());
      }
      continue;
    }

    let shifted = std::format!(" {sdl}");
    let (perturbed, _) = lossless(&shifted);
    assert_ne!(
      expected, perturbed,
      "{kind:?}: the lossless door read an SDL one byte later and the comparison still \
       agreed\n---\n{sdl}"
    );
    caught += 1;
  }

  assert_eq!(
    excused.iter().copied().collect::<Vec<_>>(),
    POSITIONLESS,
    "the positionless refusals are not the ones the excuse list names"
  );
  assert_eq!(caught + excused.len(), SCHEMA_FIXTURES.len());
}

// ---------------------------------------------------------------------------------------------
// spans
// ---------------------------------------------------------------------------------------------

/// A refusal through the lossless door points at the same **bytes**, and the padding moves it.
///
/// [`the_two_doors_agree`] already compares spans, because they are fields of the value it
/// compares. This adds the two things an `assert_eq!` cannot say on its own: that the offsets
/// address the text they claim to (the slices are read out and compared), and that the padded
/// corpus actually moved them — a sweep in which every form produced identical offsets would be
/// eight copies of the compact run wearing eight names.
///
/// The related span is read out too. It is the second half of a duplicate-and-original refusal and
/// nothing else in this file would notice if it pointed somewhere else.
#[test]
fn a_refusal_points_at_the_same_bytes_through_both_doors() {
  let mut checked = 0usize;
  let mut related_checked = 0usize;
  let mut moved = 0usize;

  for (kind, sdl, _) in refused_documents() {
    let compact: Vec<_> = syntactic(sdl).refusals().to_vec();
    assert!(!compact.is_empty(), "{kind:?}: the fixture is a schema");

    for (form, padded) in forms(sdl) {
      let expected = syntactic(&padded);
      let (actual, _) = lossless(&padded);
      let (expected, actual) = (expected.refusals(), actual.refusals());
      assert_eq!(expected.len(), actual.len(), "{kind:?} ({form})");

      for (index, (left, right)) in expected.iter().zip(actual.iter()).enumerate() {
        let text = |error: &SchemaError| {
          padded
            .get(error.span().start()..error.span().end())
            .map(String::from)
        };
        assert_eq!(
          text(left),
          text(right),
          "{kind:?} ({form}) #{index}: the two doors blame different bytes"
        );
        assert!(
          text(left).is_some(),
          "{kind:?} ({form}) #{index}: the span is not a valid range of the SDL it came from"
        );
        checked += 1;

        if let (Some(a), Some(b)) = (left.related(), right.related()) {
          let related = |span: smear::lexer::tokora::SimpleSpan| {
            padded.get(span.start()..span.end()).map(String::from)
          };
          assert_eq!(
            related(a),
            related(b),
            "{kind:?} ({form}) #{index}: the two doors' related spans blame different bytes"
          );
          assert!(related(a).is_some());
          related_checked += 1;
        } else {
          assert_eq!(
            left.related().is_some(),
            right.related().is_some(),
            "{kind:?} ({form}) #{index}: one door carries a related span and the other does not"
          );
        }
      }

      if form != "compact"
        && expected
          .iter()
          .map(SchemaError::span)
          .ne(compact.iter().map(SchemaError::span))
      {
        moved += 1;
      }
    }
  }

  assert!(
    checked >= REFUSAL_FLOOR,
    "only {checked} spans read out, floor is {REFUSAL_FLOOR}"
  );
  assert!(
    related_checked > 0,
    "no fixture produced a related span, so the second half of a duplicate refusal is unchecked"
  );
  // Every fixture but the positionless one: a refusal reported at `0..0` about the document as a
  // whole is the one thing padding cannot move, and [`POSITIONLESS`] is where that is written down.
  assert!(
    moved >= (SCHEMA_FIXTURES.len() - POSITIONLESS.len()) * ALPHABET.len(),
    "the padding moved the refusals in only {moved} of the padded runs; a corpus the padding does \
     not move cannot discriminate a token-extent span from a `text_range` one"
  );
}

/// The padded corpus puts refusals next to trivia, which is the only material on which a
/// `text_range` span differs from a token-extent one.
#[test]
fn the_padding_is_load_bearing() {
  let mut discriminating = 0usize;
  let mut total = 0usize;

  for (_, sdl, _) in refused_documents() {
    for (form, padded) in forms(sdl) {
      if form == "compact" {
        continue;
      }
      for error in syntactic(&padded).refusals() {
        total += 1;
        let start = error.span().start();
        let end = error.span().end();
        let before = padded[..start].chars().next_back();
        let after = padded[end..].chars().next();
        if before.is_some_and(is_ignorable) || after.is_some_and(is_ignorable) {
          discriminating += 1;
        }
      }
    }
  }

  assert!(
    discriminating >= DISCRIMINATING_FLOOR,
    "only {discriminating} of {total} padded refusals sit next to trivia, floor is \
     {DISCRIMINATING_FLOOR}"
  );
}

/// The six ignorable forms, as characters — enough for the adjacency question above.
fn is_ignorable(c: char) -> bool {
  matches!(c, ' ' | '\t' | '\n' | '\r' | ',' | '\u{FEFF}') || c == '#'
}

// ---------------------------------------------------------------------------------------------
// the recovering half — what the syntactic door has no answer for
// ---------------------------------------------------------------------------------------------

/// An SDL somebody is still typing gets §3 run over the part that is well-formed.
///
/// The reason the lossless leg exists, measured rather than asserted: every corpus SDL is
/// truncated at every token boundary and pushed through the door, and the run records how many of
/// those broken documents still produced a refusal. A door that refused them all would compile,
/// pass every other test in this file, and be useless.
#[test]
fn a_half_typed_schema_still_gets_an_answer() {
  let mut truncations = 0usize;
  let mut answered = 0usize;
  let mut recovered_partially = 0usize;

  for (_, sdl, _) in refused_documents() {
    for cut in boundaries(sdl) {
      if cut == 0 || cut == sdl.len() {
        continue;
      }
      let half = &sdl[..cut];
      let (answer, recovery) = lossless(half);
      truncations += 1;
      if !answer.refusals().is_empty() {
        answered += 1;
      }
      if recovery.projected() > 0 && !recovery.is_complete() {
        recovered_partially += 1;
      }
    }
  }

  // 1825 truncations, 1204 of which still produced a refusal, and 442 of which kept one
  // definition while dropping another. Floors, not equalities: the parser's recovery decides
  // where a truncated document breaks, and that is not this file's contract to pin.
  assert!(truncations >= 1_300, "only {truncations} truncations");
  assert!(
    answered >= 850,
    "only {answered} of {truncations} half-typed SDL produced a refusal; the door is refusing the \
     case it exists for"
  );
  assert!(
    recovered_partially >= 300,
    "only {recovered_partially} of {truncations} truncations exercised a **partial** recovery — \
     one definition kept and another dropped — so the recovery path is barely reached"
  );
}

/// A hole inside one definition costs that definition and no other.
#[test]
fn a_broken_definition_does_not_cost_the_good_ones() {
  // The first type is complete and wrong; the second has no field type yet.
  let sdl = "type Query { ok: Nope }\ntype Half { f: }";
  let (answer, recovery) = lossless(sdl);

  assert_eq!(recovery.projected(), 1);
  assert!(!recovery.is_complete());

  let refusals = answer.refusals();
  assert_eq!(refusals.len(), 1);
  assert_eq!(refusals[0].kind(), SchemaErrorKind::UndefinedType);
  let span = refusals[0].span();
  assert_eq!(&sdl[span.start()..span.end()], "Nope");
}

/// A dropped definition can invent a refusal, and the recovery is what says so.
///
/// The cost of recovering, pinned rather than left to be discovered. Draft §3 resolves every type
/// reference against the whole document, so it cannot tell a type that was never written from one
/// the projection dropped. `Recovery::is_complete()` false is the caller's signal that a refusal
/// may be an artifact.
#[test]
fn a_dropped_definition_can_invent_a_refusal() {
  let sdl = "type Query { ok: Nope }\ntype Nope { a: }";
  let (answer, recovery) = lossless(sdl);

  assert_eq!(recovery.projected(), 1, "only `Query` survives");
  assert!(!recovery.is_complete());
  assert!(
    answer
      .refusals()
      .iter()
      .any(|error| error.kind() == SchemaErrorKind::UndefinedType),
    "the artifact this test documents did not appear: {:#?}",
    answer.refusals()
  );

  // And the same SDL with the second type intact builds, so the refusal above is the recovery's
  // and not the document's.
  let whole = "type Query { ok: Nope }\ntype Nope { a: Int }";
  let (clean, recovery) = lossless(whole);
  assert!(recovery.is_complete());
  assert!(matches!(clean, Answer::Built(_)), "{:#?}", clean.refusals());
}

/// An `Ok` over a partial projection is a schema built from **less SDL than the author wrote**.
///
/// The §3 twin of `validator_lossless.rs`'s "nothing projected is visible in the recovery", and
/// the more dangerous half: this one is a real, internally consistent `Schema` that an editor can
/// go on to validate operations against, and the only thing that says it is missing a type is the
/// [`Recovery`].
#[test]
fn a_clean_verdict_over_a_partial_projection_is_a_smaller_schema() {
  let sdl = "type Query { ok: Int }\ntype Half { f: }";
  let (answer, recovery) = lossless(sdl);

  assert!(
    matches!(answer, Answer::Built(_)),
    "{:#?}",
    answer.refusals()
  );
  assert_eq!(recovery.projected(), 1);
  assert_eq!(recovery.skipped(), 1);
  assert!(!recovery.is_complete());

  // `Half` is not in it, and nothing in the verdict said so.
  let (schema, _) = validate_schema_lossless(&parse_type_system_document(sdl), sdl)
    .expect("the projected half is a schema");
  assert!(schema.type_by_name(b"Half").is_none());
  assert!(schema.type_by_name(b"Query").is_some());
}

/// Nothing projected is **not** reported as a clean document.
///
/// Where the executable door answers `Ok` with an empty sink over a document it never saw, this
/// one refuses: an empty type-system document has no query root, so `Schema::build` says
/// [`MissingQueryRootOperationType`](SchemaErrorKind::MissingQueryRootOperationType). That is the
/// better failure direction and it is not this door's doing — it falls out of §3 being a
/// whole-document pass — so it is pinned here rather than claimed.
#[test]
fn nothing_projected_is_visible_in_the_recovery() {
  let sdl = "type Query { ok: }";
  let parse = parse_type_system_document(sdl);
  assert!(parse.has_errors());

  let refused = validate_schema_lossless(&parse, sdl).expect_err("an empty document is no schema");
  assert_eq!(
    refused
      .recovery()
      .expect("the pair matches, so the build was asked")
      .projected(),
    0
  );
  assert!(
    !refused
      .recovery()
      .expect("the pair matches, so the build was asked")
      .is_complete()
  );
  assert_eq!(
    refused
      .errors()
      .expect("the pair matches, so the build was asked")
      .kinds(),
    [SchemaErrorKind::MissingQueryRootOperationType]
  );
}

/// Executable syntax in an SDL parse is rubble the recovery steps over.
#[test]
fn executable_syntax_is_skipped_not_fatal() {
  let sdl = "type Query { ok: Int } query Q { ok }";
  let (answer, recovery) = lossless(sdl);

  assert_eq!(recovery.projected(), 1, "the type should survive");
  assert!(
    recovery.skipped() > 0,
    "the operation should be dropped rather than projected"
  );
  assert!(
    matches!(answer, Answer::Built(_)),
    "{:#?}",
    answer.refusals()
  );
}

/// The `(parse, source)` pair is verified as a **whole root**, and the SDL door verifies it too.
///
/// The executable door got this check first, and this one was cleared out of that sweep — "no
/// `Budget`, so out of scope". True of the ledger question and silent about this one: a `source`
/// that is the parse's text **plus trailing SDL** projects every stale definition, reports a
/// complete recovery, and lets `Schema::build` answer `Ok` for the prefix while the appended
/// definitions are silently absent. A schema built from part of its own SDL.
///
/// Both doors now make the same whole-root check, and so does the recovering projector itself, so a
/// consumer using it directly cannot be handed a stale prefix either. al8n/smear#198.
#[test]
fn a_mismatched_pair_builds_nothing_and_says_so() {
  let check = |parsed: &str, given: &str| {
    let parse = parse_type_system_document(parsed);
    assert!(!parse.has_errors());
    let refused = validate_schema_lossless(&parse, given)
      .err()
      .unwrap_or_else(|| panic!("a parse of {parsed:?} against {given:?} built a schema"));
    assert_eq!(
      refused.refusal(),
      Some(Refusal::SourceMismatch),
      "{refused}"
    );
    // The build was never asked, so there are no §3 refusals to read — and no empty list standing
    // in for them either.
    assert_eq!(refused.errors(), None);
    assert_eq!(refused.recovery(), None);
    assert_eq!(
      refused.to_string(),
      "the parse and the source are not the same document, so nothing was built"
    );
  };

  // Same length, different bytes: caught before this change too, by each definition's own check.
  check("type Query { ok: Int }", "type Query { no: Int }");

  // **The prefix.** Every definition of the parse matches the source at its own range, so the
  // per-definition check sees nothing wrong — and `type Extra` is never built from.
  check(
    "type Query { ok: Int }",
    "type Query { ok: Int } type Extra { n: Int }",
  );

  // And the pair that matches still builds, so the check has not simply refused everything.
  let sdl = "type Query { ok: Int }";
  let (_schema, recovery) = validate_schema_lossless(&parse_type_system_document(sdl), sdl)
    .expect("a matching pair builds");
  assert!(recovery.is_complete());
}

/// The fail-fast projection refuses the SDL root's other half, and the recovering one skips it.
///
/// A mixed parse holds a `Document` node, not a `TypeSystemDocument` one, so the SDL root's
/// projection has nothing to read — which is the refusal `project_type_system_document` documents
/// and the reason the door takes a parse from `parse_type_system_document` rather than any parse.
#[test]
fn a_mixed_parse_is_not_an_sdl_parse() {
  use smear::parser::graphql::lossless::{parse_document, project_type_system_document};

  let sdl = "type Query { ok: Int }";
  let mixed = parse_document(sdl);
  assert!(!mixed.has_errors());
  assert!(
    project_type_system_document(&mixed, sdl).is_err(),
    "the SDL projection read a mixed root"
  );

  // And through the door, the recovering walk falls back to the tree's own root, where the one
  // child is a `Document` node with no SDL image — so it is skipped rather than mis-read.
  let refused = validate_schema_lossless(&mixed, sdl).expect_err("nothing projected is no schema");
  assert_eq!(
    refused
      .recovery()
      .expect("the pair matches, so the build was asked")
      .projected(),
    0
  );
  assert!(
    refused
      .recovery()
      .expect("the pair matches, so the build was asked")
      .skipped()
      > 0
  );
}
