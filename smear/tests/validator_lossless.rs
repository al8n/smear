#![cfg(all(feature = "validator", feature = "rowan"))]

//! The differential gate for the validator's lossless door.
//!
//! # The claim, and the instrument
//!
//! `validate_executable_lossless(schema, parse_executable_document(src), src) ==
//! validate_executable(schema, executable_document(src))` — **the same diagnostics**, as values.
//! Not "the same rules", not "the same count": [`Diagnostic`] derives [`PartialEq`] over its rule,
//! its span, its related span, its subject slice and its schema-side context, and the comparison
//! is `assert_eq!` over the whole `Vec`, so order is in it too.
//!
//! The two doors are one validator by construction — the lossless one projects the CST to the AST
//! and calls the syntactic one — so what this gate actually measures is the **projection**: that
//! the AST reached through the tree is the AST reached through the parser, at every byte a
//! diagnostic can point at. That is the single most likely place for a silent defect, because
//! both paths produce *a* span and only a comparison shows they differ.
//!
//! # The corpus is the rule corpus
//!
//! `tests/support/validator_corpus.rs` — one entry per draft §5 rule, each with a document that
//! fires it and a valid twin that fires nothing. `validator_rules.rs`'s `liveness_floor` is what
//! keeps it complete; this file reads it so the differential oracle inherits that completeness
//! instead of being a second, narrower corpus that happens to agree with itself.
//!
//! Each document is run **nine times**: compact, and padded at every token boundary with each of
//! `tests/support/span_extent.rs`'s eight ignorable forms — the same alphabet
//! `syntactic_span_extent.rs` and `lossless_project.rs` read.
//!
//! # What the padding is worth, measured rather than claimed
//!
//! Trivia is the only material on which a `text_range` span differs from a token-extent one, so
//! the usual statement is that the compact half of a corpus cannot catch that defect at all. That
//! statement is **false here**, and saying it would be a claim this file disproves: the fixtures
//! are hand-written GraphQL with spaces in it, so "compact" already means "padded wherever the
//! author typed a space".
//!
//! The measurement, taken by making the projection's `extent` return the node's own range: 2 of
//! the 62 compact documents disagree, and 16 of the 496 padded pairs. The padding is worth
//! roughly eight times the catch rate, not the whole of it, and
//! [`the_padding_is_load_bearing`] pins the mechanism behind that — after injection, **every**
//! diagnostic the corpus produces sits next to trivia.
//!
//! Both numbers are small because a §5 diagnostic usually blames a *name token*, whose span no
//! composite-extent rule touches. Composite spans are compared exhaustively somewhere else and on
//! purpose: `lossless_project.rs` asserts the projected AST equals the parsed one as a **value**,
//! every span of every node, at both document roots. This gate is the diagnostic-level statement
//! on top of that, not a replacement for it.
//!
//! # The four ways a gate like this passes without meaning anything
//!
//! 1. **A comparison that ignores what matters.** [`the_comparison_can_answer_no`] feeds it the
//!    same document one byte later, a document with a different name in it, and the same
//!    diagnostics in the wrong order, and requires all three to compare unequal.
//! 2. **A rule that runs on one side only.** [`dropping_one_rule_from_one_door_is_caught`] runs
//!    the lossless door with one rule switched off and requires the comparison to notice, for
//!    every rule in the corpus.
//! 3. **A corpus that produces nothing to compare.** [`the_agreement_is_not_vacuous`] counts the
//!    diagnostics and the distinct rules the sweep actually observed and floors both.
//! 4. **A door nobody can reach with a broken document.** [`a_half_typed_document_still_gets_an_answer`]
//!    truncates every corpus entry at every token boundary and requires the door to keep
//!    answering — which is the entire reason the lossless leg exists.
//!
//! # What is deliberately not compared
//!
//! A document the **syntactic** parser rejects. It has no AST, so the syntactic door has no
//! answer, so there is no second value for a differential to hold against. That case is the
//! lossless door's own — see the recovery tests at the bottom of this file, which pin it against
//! stated expectations rather than against an oracle that does not exist.

#![allow(missing_docs)]

use std::{collections::BTreeSet, string::String, vec::Vec};

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    lossless::parse_executable_document,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{
    Budget, Collect, Diagnostic, Recovery, Rule, RuleSet, Schema, Scratch, validate_executable,
    validate_executable_lossless, validate_executable_lossless_with, validate_executable_with,
  },
};

// The rule corpus, shared with `validator_rules.rs`, which owns the census that keeps it
// complete. `Fixture::rule` is that census's field and is read here only in failure messages, so
// the allow is at the include rather than on a field of a table this file does not own.
#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

// The trivia alphabet and injector, shared with `syntactic_span_extent.rs` and
// `lossless_project.rs`. Only two of its five exports are wanted here; the rest would be
// `dead_code` denials under CI's `-Dwarnings`.
#[allow(dead_code)]
#[path = "support/span_extent.rs"]
mod extent;

use corpus::{FIXTURES, Fixture, SCHEMA};
use extent::{ALPHABET, inject};

// ---------------------------------------------------------------------------------------------
// floors
// ---------------------------------------------------------------------------------------------

// Every number below is the measurement on the day this was written, taken as a floor. They are
// what stops the sweep going quiet: a corpus that stopped producing diagnostics, or stopped
// reaching a rule, agrees with itself perfectly and says nothing at all.

/// The smallest number of `(document, trivia form)` pairs the sweep is allowed to compare.
///
/// 31 fixtures x 2 documents x 9 forms.
const COMPARISON_FLOOR: usize = 558;

/// The smallest number of **diagnostics** the sweep is allowed to have compared.
const DIAGNOSTIC_FLOOR: usize = 297;

/// The smallest number of distinct rules the compared diagnostics are allowed to reach.
///
/// The corpus holds one entry per rule and `validator_rules.rs` asserts that, so anything below
/// `Rule::ALL.len()` means the differential lost coverage the liveness floor still claims.
const RULE_FLOOR: usize = 31;

/// The smallest number of compared diagnostics whose span sits at a **discriminating** site —
/// one where trivia is adjacent, so a token-extent span and a `text_range` span differ.
///
/// Measured at 264 of 264: after padding at every token boundary, *every* diagnostic the corpus
/// produces is at a site where the two span rules disagree.
const DISCRIMINATING_FLOOR: usize = 264;

// ---------------------------------------------------------------------------------------------
// the two doors
// ---------------------------------------------------------------------------------------------

fn build(sdl: &str) -> Schema {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .unwrap_or_else(|errors| panic!("fixture SDL does not parse: {errors:?}\n---\n{sdl}"));
  Schema::build(&document)
    .unwrap_or_else(|errors| panic!("fixture SDL is not a schema:\n{errors}\n---\n{sdl}"))
}

/// The syntactic door: parse to an AST, validate the AST.
fn syntactic<'a>(
  schema: &Schema,
  source: &'a str,
  budget: &Budget,
  rules: RuleSet,
) -> Vec<Diagnostic<&'a str>> {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .unwrap_or_else(|errors| panic!("the syntactic parser rejects it: {errors:?}\n---\n{source}"));

  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let verdict = validate_executable_with(schema, &document, &mut scratch, budget, rules, &mut sink);
  assert_eq!(
    verdict.is_err(),
    !collected.is_empty(),
    "the syntactic verdict and its diagnostics disagree\n---\n{source}"
  );
  collected
}

/// The lossless door: parse to a CST, hand the CST and its text to the validator.
fn lossless<'a>(
  schema: &Schema,
  source: &'a str,
  budget: &Budget,
  rules: RuleSet,
) -> (Vec<Diagnostic<&'a str>>, Recovery) {
  let parse = parse_executable_document(source);
  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let verdict = validate_executable_lossless_with(
    schema,
    &parse,
    source,
    &mut scratch,
    budget,
    rules,
    &mut sink,
  );
  let recovery = match verdict {
    Ok(recovery) => {
      assert!(
        collected.is_empty(),
        "the lossless door reported diagnostics under an `Ok` verdict\n---\n{source}"
      );
      recovery
    }
    Err(refused) => {
      assert!(
        !collected.is_empty(),
        "the lossless door returned `Err` with nothing in the sink\n---\n{source}"
      );
      assert_eq!(
        refused.invalid().emitted() as usize,
        collected.len(),
        "the lossless verdict's count is not what the sink received\n---\n{source}"
      );
      refused.recovery()
    }
  };
  (collected, recovery)
}

/// Every token boundary in `src`, read off the lossless tree rather than from a second lexer.
fn boundaries(src: &str) -> Vec<usize> {
  let parse = parse_executable_document(src);
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
fn forms(src: &str) -> Vec<(&'static str, String)> {
  let marks = boundaries(src);
  std::iter::once(("compact", src.to_string()))
    .chain(
      ALPHABET
        .iter()
        .map(|(form, pad)| (*form, inject(src, &marks, pad))),
    )
    .collect()
}

/// Every document in the corpus: both halves of every fixture, with the schema and budget that
/// fixture is measured under.
///
/// The schema is built once per fixture and both halves borrow it, so the sweep pays thirty
/// builds rather than sixty for a value that is by construction the same on both sides.
fn documents() -> Vec<(&'static Fixture, Schema, Budget)> {
  FIXTURES
    .iter()
    .map(|fixture| {
      (
        fixture,
        build(fixture.schema.unwrap_or(SCHEMA)),
        fixture.budget.unwrap_or_default(),
      )
    })
    .collect()
}

/// The two documents a fixture contributes: the one that fires its rule, and its valid twin.
fn halves(fixture: &Fixture) -> [&'static str; 2] {
  [fixture.invalid, fixture.valid]
}

// ---------------------------------------------------------------------------------------------
// the core assertion
// ---------------------------------------------------------------------------------------------

/// The two doors produce the same diagnostics, as values, over the whole rule corpus.
#[test]
fn the_two_doors_agree() {
  let mut compared = 0usize;
  let mut diagnostics = 0usize;
  let mut rules: BTreeSet<Rule> = BTreeSet::new();

  for (fixture, schema, budget) in documents() {
    for source in halves(fixture) {
      for (form, padded) in forms(source) {
        let expected = syntactic(&schema, &padded, &budget, RuleSet::ALL);
        let (actual, recovery) = lossless(&schema, &padded, &budget, RuleSet::ALL);

        assert!(
          recovery.is_complete(),
          "{:?} ({form}): the projection dropped {} top-level element(s) of a document the \
         syntactic parser accepts\n---\n{padded}",
          fixture.rule,
          recovery.skipped()
        );
        assert_eq!(
          actual, expected,
          "{:?} ({form}): the two doors disagree\n---\n{padded}",
          fixture.rule
        );

        diagnostics += expected.len();
        rules.extend(expected.iter().map(Diagnostic::rule));
        compared += 1;
      }
    }
  }

  assert_eq!(
    compared,
    FIXTURES.len() * 2 * (ALPHABET.len() + 1),
    "the sweep did not run every form over every document"
  );
  assert!(
    compared >= COMPARISON_FLOOR,
    "only {compared} comparisons, floor is {COMPARISON_FLOOR}"
  );
  assert!(
    diagnostics >= DIAGNOSTIC_FLOOR,
    "only {diagnostics} diagnostics compared, floor is {DIAGNOSTIC_FLOOR}"
  );
  assert!(
    rules.len() >= RULE_FLOOR,
    "the compared diagnostics reach only {} rules, floor is {RULE_FLOOR}: {rules:?}",
    rules.len()
  );
}

// ---------------------------------------------------------------------------------------------
// non-vacuity
// ---------------------------------------------------------------------------------------------

/// The corpus produces diagnostics on **both** sides, and reaches every rule through both doors.
///
/// [`the_two_doors_agree`] counts what the syntactic door produced, which is the right thing to
/// floor when the two are equal. This one counts them separately, so a lossless door that
/// silently reported nothing could not hide behind an equality that happened to hold because both
/// sides were empty.
#[test]
fn the_agreement_is_not_vacuous() {
  let mut syntactic_total = 0usize;
  let mut lossless_total = 0usize;
  let mut syntactic_rules: BTreeSet<Rule> = BTreeSet::new();
  let mut lossless_rules: BTreeSet<Rule> = BTreeSet::new();
  let mut invalid_documents_with_findings = 0usize;

  for (fixture, schema, budget) in documents() {
    for source in halves(fixture) {
      for (_, padded) in forms(source) {
        let expected = syntactic(&schema, &padded, &budget, RuleSet::ALL);
        let (actual, _) = lossless(&schema, &padded, &budget, RuleSet::ALL);
        syntactic_total += expected.len();
        lossless_total += actual.len();
        syntactic_rules.extend(expected.iter().map(Diagnostic::rule));
        lossless_rules.extend(actual.iter().map(Diagnostic::rule));
        if !actual.is_empty() {
          invalid_documents_with_findings += 1;
        }
      }
    }
  }

  assert!(
    syntactic_total >= DIAGNOSTIC_FLOOR,
    "the syntactic door produced only {syntactic_total} diagnostics over the corpus"
  );
  assert!(
    lossless_total >= DIAGNOSTIC_FLOOR,
    "the lossless door produced only {lossless_total} diagnostics over the corpus"
  );
  assert_eq!(syntactic_total, lossless_total);
  assert_eq!(syntactic_rules, lossless_rules);
  assert!(
    syntactic_rules.len() >= RULE_FLOOR,
    "the corpus reaches only {} rules through the doors, floor is {RULE_FLOOR}",
    syntactic_rules.len()
  );
  assert_eq!(
    invalid_documents_with_findings,
    FIXTURES.len() * (ALPHABET.len() + 1),
    "exactly the invalid half of the corpus should produce findings, in every form"
  );
}

// ---------------------------------------------------------------------------------------------
// control 1 — the comparison can answer no
// ---------------------------------------------------------------------------------------------

/// The `Vec<Diagnostic>` comparison notices a moved span, a different subject and a different
/// order.
///
/// Without this, every assertion in this file would be discounted by however much of a diagnostic
/// `PartialEq` does not read.
#[test]
fn the_comparison_can_answer_no() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let compact = "{ dog { meowVolume nickname } }";
  let shifted = " { dog { meowVolume nickname } }";
  let renamed = "{ dog { purrVolume nickname } }";

  let a = syntactic(&schema, compact, &budget, RuleSet::ALL);
  assert!(
    !a.is_empty(),
    "the control document produces no diagnostics"
  );

  // The positive leg first, so "everything is unequal" cannot be why the rest passes.
  let (again, recovery) = lossless(&schema, compact, &budget, RuleSet::ALL);
  assert_eq!(a, again);
  assert!(recovery.is_complete());

  let (b, _) = lossless(&schema, shifted, &budget, RuleSet::ALL);
  assert_ne!(
    a, b,
    "the same document one byte later compares equal, so spans are not being compared and this \
     gate cannot see a span defect at all"
  );

  let (c, _) = lossless(&schema, renamed, &budget, RuleSet::ALL);
  assert_ne!(
    a, c,
    "two documents differing only in a field name compare equal, so subject slices are not being \
     compared"
  );

  // Order. The corpus's multi-diagnostic documents are what makes this reachable.
  let multi = "{ dog { meowVolume } } fragment unused on Dog { purrVolume }";
  let ordered = syntactic(&schema, multi, &budget, RuleSet::ALL);
  assert!(
    ordered.len() >= 2,
    "the order control needs a document with at least two diagnostics, got {}",
    ordered.len()
  );
  let mut reversed = ordered.clone();
  reversed.reverse();
  assert_ne!(
    ordered, reversed,
    "the same diagnostics in the opposite order compare equal, so the doors could disagree on \
     order and this gate would not say so"
  );
}

// ---------------------------------------------------------------------------------------------
// control 2 — a rule missing from one door is caught
// ---------------------------------------------------------------------------------------------

/// For **every** rule in the corpus, switching it off on the lossless side alone reds the
/// comparison.
///
/// The perturbation the differential exists to catch, planted mechanically rather than once by
/// hand: it proves the gate can see a rule that stopped running through one door, for each rule
/// individually, instead of proving it for whichever rule the author happened to pick.
#[test]
fn dropping_one_rule_from_one_door_is_caught() {
  let mut caught = 0usize;
  for fixture in FIXTURES {
    let schema = build(fixture.schema.unwrap_or(SCHEMA));
    let budget = fixture.budget.unwrap_or_default();

    let expected = syntactic(&schema, fixture.invalid, &budget, RuleSet::ALL);
    let (perturbed, _) = lossless(
      &schema,
      fixture.invalid,
      &budget,
      RuleSet::ALL.without(fixture.rule),
    );
    assert_ne!(
      expected, perturbed,
      "{:?}: the lossless door ran without it and the comparison still agreed\n---\n{}",
      fixture.rule, fixture.invalid
    );
    caught += 1;
  }
  assert_eq!(caught, FIXTURES.len());
}

// ---------------------------------------------------------------------------------------------
// spans
// ---------------------------------------------------------------------------------------------

/// A diagnostic through the lossless door points at the same **bytes**, and the padding moves it.
///
/// [`the_two_doors_agree`] already compares spans, because they are fields of the value it
/// compares. This adds the two things an `assert_eq!` cannot say on its own: that the offsets
/// address the text they claim to (the slices are read out and compared), and that the padded
/// corpus actually moved them — a sweep in which every form produced identical offsets would be
/// eight copies of the compact run wearing eight names.
#[test]
fn a_diagnostic_points_at_the_same_bytes_through_both_doors() {
  let mut checked = 0usize;
  let mut moved = 0usize;

  for (fixture, schema, budget) in documents() {
    for source in halves(fixture) {
      let compact = syntactic(&schema, source, &budget, RuleSet::ALL);
      if compact.is_empty() {
        continue;
      }

      for (form, padded) in forms(source) {
        let expected = syntactic(&schema, &padded, &budget, RuleSet::ALL);
        let (actual, _) = lossless(&schema, &padded, &budget, RuleSet::ALL);
        assert_eq!(expected.len(), actual.len());

        for (index, (left, right)) in expected.iter().zip(actual.iter()).enumerate() {
          let text = |source: &str, diagnostic: &Diagnostic<&str>| {
            source
              .get(diagnostic.span().start()..diagnostic.span().end())
              .map(String::from)
          };
          assert_eq!(
            text(&padded, left),
            text(&padded, right),
            "{:?} ({form}) #{index}: the two doors blame different bytes",
            fixture.rule
          );
          assert!(
            text(&padded, left).is_some(),
            "{:?} ({form}) #{index}: the span is not a valid range of the source it came from",
            fixture.rule
          );
          checked += 1;
        }

        if form != "compact"
          && expected
            .iter()
            .map(Diagnostic::span)
            .ne(compact.iter().map(Diagnostic::span))
        {
          moved += 1;
        }
      }
    }
  }

  assert!(
    checked >= DIAGNOSTIC_FLOOR,
    "only {checked} spans read out, floor is {DIAGNOSTIC_FLOOR}"
  );
  assert!(
    moved >= FIXTURES.len() * ALPHABET.len(),
    "the padding moved the diagnostics in only {moved} of the padded runs; a corpus the padding \
     does not move cannot discriminate a token-extent span from a `text_range` one"
  );
}

/// The padded corpus puts diagnostics next to trivia, which is the only material on which a
/// `text_range` span differs from a token-extent one.
#[test]
fn the_padding_is_load_bearing() {
  let mut discriminating = 0usize;
  let mut total = 0usize;

  for (fixture, schema, budget) in documents() {
    for source in halves(fixture) {
      for (form, padded) in forms(source) {
        if form == "compact" {
          continue;
        }
        for diagnostic in syntactic(&schema, &padded, &budget, RuleSet::ALL) {
          total += 1;
          let start = diagnostic.span().start();
          let end = diagnostic.span().end();
          let before = padded[..start].chars().next_back();
          let after = padded[end..].chars().next();
          if before.is_some_and(is_ignorable) || after.is_some_and(is_ignorable) {
            discriminating += 1;
          }
        }
      }
    }
  }

  assert!(
    discriminating >= DISCRIMINATING_FLOOR,
    "only {discriminating} of {total} padded diagnostics sit next to trivia, floor is \
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

/// A document somebody is still typing gets the rules run over the part that is well-formed.
///
/// The reason the lossless leg exists, measured rather than asserted: every corpus document is
/// truncated at every token boundary and pushed through the door, and the run records how many of
/// those broken documents still produced a finding. A door that refused them all would compile,
/// pass every other test in this file, and be useless.
#[test]
fn a_half_typed_document_still_gets_an_answer() {
  let mut truncations = 0usize;
  let mut answered = 0usize;
  let mut recovered_partially = 0usize;

  for (fixture, schema, budget) in documents() {
    for source in halves(fixture) {
      for cut in boundaries(source) {
        if cut == 0 || cut == source.len() {
          continue;
        }
        let half = &source[..cut];
        let (diagnostics, recovery) = lossless(&schema, half, &budget, RuleSet::ALL);
        truncations += 1;
        if !diagnostics.is_empty() {
          answered += 1;
        }
        if recovery.projected() > 0 && !recovery.is_complete() {
          recovered_partially += 1;
        }
      }
    }
  }

  // 1354 truncations, 598 of which still produced a finding, and 148 of which kept one
  // definition while dropping another. Floors, not equalities: the parser's recovery decides
  // where a truncated document breaks, and that is not this file's contract to pin.
  assert!(truncations >= 1_000, "only {truncations} truncations");
  assert!(
    answered >= 400,
    "only {answered} of {truncations} half-typed documents produced a finding; the door is \
     refusing the case it exists for"
  );
  assert!(
    recovered_partially >= 100,
    "only {recovered_partially} of {truncations} truncations exercised a **partial** recovery — \
     one definition kept and another dropped — so the recovery path is barely reached"
  );
}

/// A hole inside one definition costs that definition and no other.
#[test]
fn a_broken_definition_does_not_cost_the_good_ones() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  // The first operation is complete and wrong; the second is three keystrokes in.
  let source = "{ dog { meowVolume } }\nquery Half(";
  let (diagnostics, recovery) = lossless(&schema, source, &budget, RuleSet::ALL);

  assert_eq!(recovery.projected(), 1);
  assert!(!recovery.is_complete());
  assert_eq!(diagnostics.len(), 1);
  assert_eq!(diagnostics[0].rule(), Rule::FieldSelections);
  assert_eq!(
    &source[diagnostics[0].span().start()..diagnostics[0].span().end()],
    "meowVolume"
  );
}

/// Type-system syntax in an executable parse is rubble the recovery steps over.
///
/// And the second half is the honest one: the recovery keeps whatever the **parser** shaped into
/// a definition, not whatever the author meant. `type T { f: Int }` leaves its head as two error
/// tiles and its body as a well-formed shorthand operation, so three definitions are projected
/// from a document that has two. That is not correctable here — the shape is the parser's answer
/// and re-deciding it would be a second grammar — and it is exactly the reason
/// `Recovery::is_complete()` false has to be read as "this verdict is about a document that is
/// not the one on screen".
#[test]
fn rubble_between_two_definitions_is_skipped_not_fatal() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let source = "{ dog { meowVolume } } extend type T @onQuery { dog { name } }";
  let (diagnostics, recovery) = lossless(&schema, source, &budget, RuleSet::ALL);

  assert_eq!(recovery.projected(), 2, "both operations should survive");
  assert!(
    recovery.skipped() > 0,
    "the extension should be dropped rather than projected"
  );
  // 5.3.1 for `meowVolume`, and 5.2.3.1 twice because two anonymous operations survived.
  let rules: BTreeSet<Rule> = diagnostics.iter().map(Diagnostic::rule).collect();
  assert!(rules.contains(&Rule::FieldSelections), "{rules:?}");
  assert!(rules.contains(&Rule::LoneAnonymousOperation), "{rules:?}");

  // The braced-body case, pinned rather than described.
  let braced = "{ dog { meowVolume } } type T { f: Int } { dog { name } }";
  let (_, recovery) = lossless(&schema, braced, &budget, RuleSet::ALL);
  assert_eq!(
    (recovery.projected(), recovery.skipped()),
    (3, 2),
    "the SDL body no longer becomes a third, accidental operation; if the parser's recovery \
     changed, this is the line that says so"
  );
}

/// Nothing projected is reported as nothing projected, and **not** as a clean document.
///
/// The trap this door has and the syntactic one does not: `Ok` with an empty sink over a document
/// the validator never saw. It is `Ok` because no rule fired, which is true and useless, and
/// `Recovery::projected() == 0` is the only thing that says so. Pinned here so a caller reading
/// the verdict alone is a documented mistake rather than a surprise.
#[test]
fn nothing_projected_is_visible_in_the_recovery() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let source = "{ dog { name @ } }";
  let parse = parse_executable_document(source);
  assert!(parse.has_errors());

  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let recovery =
    validate_executable_lossless(&schema, &parse, source, &mut scratch, &budget, &mut sink)
      .expect("no rule can fire over a document with no definitions in it");

  assert!(collected.is_empty());
  assert_eq!(recovery.projected(), 0);
  assert!(!recovery.is_complete());
}

/// A dropped fragment can invent a diagnostic, and the recovery is what says so.
///
/// The cost of recovering, pinned rather than left to be discovered. Draft 5.5.2.1 cannot tell a
/// fragment that was never written from one the projection dropped, so a spread of the dropped
/// fragment is reported. `Recovery::is_complete()` false is the caller's signal that a finding
/// may be an artifact.
#[test]
fn a_dropped_fragment_can_invent_a_finding() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let source = "{ dog { ...named } } fragment named on Dog { name @ }";
  let (diagnostics, recovery) = lossless(&schema, source, &budget, RuleSet::ALL);

  assert_eq!(recovery.projected(), 1, "only the operation survives");
  assert!(!recovery.is_complete());
  assert!(
    diagnostics
      .iter()
      .any(|d| d.rule() == Rule::FragmentSpreadTargetDefined),
    "the artifact this test documents did not appear: {diagnostics:#?}"
  );

  // And the same document with the fragment intact reports nothing, so the finding above is the
  // recovery's and not the document's.
  let whole = "{ dog { ...named } } fragment named on Dog { name }";
  let (clean, recovery) = lossless(&schema, whole, &budget, RuleSet::ALL);
  assert!(recovery.is_complete());
  assert!(clean.is_empty(), "{clean:#?}");
}

/// The `(parse, source)` pair is verified, not trusted.
#[test]
fn a_mismatched_source_projects_nothing() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let source = "{ dog { meowVolume } }";
  let other = "{ dog { barkVolume } }";
  let parse = parse_executable_document(source);
  assert!(!parse.has_errors());

  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let recovery =
    validate_executable_lossless(&schema, &parse, other, &mut scratch, &budget, &mut sink)
      .expect("a document with no projected definitions fires no rule");

  assert_eq!(
    recovery.projected(),
    0,
    "the tree was projected against text it was not parsed from"
  );
  assert!(collected.is_empty());
}

/// The lossless door reuses one `Scratch` across both doors, and does not poison it.
#[test]
fn the_two_doors_share_a_scratch() {
  let schema = build(SCHEMA);
  let budget = Budget::default();
  let mut scratch = Scratch::new();

  let bad = "{ dog { meowVolume } }";
  let parse = parse_executable_document(bad);

  for _ in 0..3 {
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    assert!(
      validate_executable_lossless(&schema, &parse, bad, &mut scratch, &budget, &mut sink).is_err()
    );
    assert_eq!(collected.len(), 1);

    let document = Parser::with_parser::<
      GraphqlLexer<'_, str>,
      ExecutableDocument<&str>,
      GraphqlErrors<&str>,
      _,
      GraphQL,
    >(executable_document)
    .parse_str("{ dog { name } }")
    .expect("parses");
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    assert!(
      validate_executable(&schema, &document, &mut scratch, &budget, &mut sink).is_ok(),
      "the syntactic door saw the lossless door's leftovers"
    );
  }
}
