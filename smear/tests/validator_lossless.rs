#![cfg(all(feature = "validator", feature = "rowan_0_17", feature = "std"))]

//! The validator's differential, written once over the GraphQL parser layer.
//!
//! # The claim, and the instrument
//!
//! [`verdict`] reads a text with a parser layer and validates the document the layer answers.
//! Over the same text, `verdict(&Syntactic::new(), …)` and `verdict(&Lossless::new(), …)` answer
//! **the same diagnostics**, as values. Not "the same rules", not "the same count": [`Diagnostic`]
//! derives [`PartialEq`] over its rule, its span, its related span, its subject slice and its
//! schema-side context, and the comparison is `assert_eq!` over the whole `Vec`, so order is in it
//! too. Beside them the two layers answer the same verdicts: the text is a document to both,
//! neither lends an error, both recoveries are complete and equal, and the validator's own
//! `Result` is the same.
//!
//! There is one validator, and nothing in it knows which layer made its document, so what this
//! gate actually measures is the lossless layer's **projection**: that the AST reached through the
//! tree is the AST the syntactic layer answers, at every byte a diagnostic can point at. That is
//! the single most likely place for a silent defect, because both paths produce *a* span and only
//! a comparison shows they differ.
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
//! 2. **A rule that runs on one side only.** [`dropping_one_rule_from_one_layer_is_caught`]
//!    validates the lossless layer's document with one rule switched off and requires the
//!    comparison to notice, for every rule in the corpus.
//! 3. **A corpus that produces nothing to compare.** [`the_agreement_is_not_vacuous`] counts the
//!    diagnostics and the distinct rules the sweep actually observed and floors both.
//! 4. **A layer nobody can reach with a broken document.**
//!    [`a_half_typed_document_still_gets_an_answer`] truncates every corpus entry at every token
//!    boundary and requires the lossless layer to keep answering — which is the entire reason it
//!    exists.
//!
//! # Two exits
//!
//! A syntax error leaves by the layer's sink, and a finding by the validator's: nothing passes the
//! first into the second, and a caller reads two verdicts, the layer's `is_valid()` and the
//! validator's `Result`. [`syntax_errors_leave_by_the_layer_and_findings_by_the_validator`] holds
//! each exit to what may leave by it.
//!
//! # What is deliberately not compared
//!
//! A text the **syntactic** layer rejects. It answers no document, so there is nothing of its to
//! validate and no second value for a differential to hold against. That case is the lossless
//! layer's own — see the recovery tests at the bottom of this file, which pin it against stated
//! expectations rather than against an oracle that does not exist.

#![allow(missing_docs)]

use core::ops::ControlFlow;
use std::{collections::BTreeSet, string::String, vec::Vec};

use smear::{
  diagnostic::Diagnose,
  lexer::tokora::{Parse as _, Parser},
  parser::{
    graphql::{
      GraphQL, Layer, Lossless, Recovery, Syntactic,
      ast::TypeSystemDocument,
      error::GraphqlErrors,
      lossless::{GraphqlLosslessErrorValue, parse_executable_document},
      syntactic::{GraphqlLexer, type_system_document},
    },
    layer::from_fn,
  },
  validator::{
    Budget, Collect, Diagnostic, Invalid, Rule, RuleSet, Schema, Scratch, validate_executable,
    validate_executable_with,
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
/// 32 fixtures x 2 documents x 9 forms: the sweep's own count is exactly `FIXTURES.len()` x 2 x 9,
/// so this floor is what fails when the corpus loses a fixture.
const COMPARISON_FLOOR: usize = 576;

/// The smallest number of **diagnostics** the sweep is allowed to have compared.
const DIAGNOSTIC_FLOOR: usize = 297;

/// The smallest number of distinct rules the compared diagnostics are allowed to reach.
///
/// The corpus holds one entry per rule that HAS one, and `validator_rules.rs` asserts that against
/// its `UNFIREABLE` excuse list, so anything below `Rule::ALL.len() - UNFIREABLE.len()` means the
/// differential lost coverage the liveness floor still claims. The one excused rule is
/// `ValidationWorkBudget`, which the corpus holds no entry for: `validator_work.rs` fires it at
/// many budgets, and `UNFIREABLE` gives the reason.
const RULE_FLOOR: usize = 31;

/// The smallest number of compared diagnostics whose span sits at a **discriminating** site —
/// one where trivia is adjacent, so a token-extent span and a `text_range` span differ.
///
/// Measured at 264 of 264: after padding at every token boundary, *every* diagnostic the corpus
/// produces is at a site where the two span rules disagree.
const DISCRIMINATING_FLOOR: usize = 264;

/// How many of the truncations [`a_half_typed_document_still_gets_an_answer`] reads are whole
/// documents, at most.
///
/// A truncation is a whole document only when its cut falls at the end of a top-level definition
/// or after a trivia token that follows one, and only then does the lossless layer lend it no
/// error. Read off the corpus: its 64 documents hold 84 top-level definitions, and counting each
/// one with every trivia character after it, a bound on the trivia tokens there, then leaving out
/// each text's last cut, which no truncation takes, gives 40.
const WHOLE_TRUNCATIONS: usize = 40;

// ---------------------------------------------------------------------------------------------
// the one harness, over the layer
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

/// What a parser layer and the validator answer for one text: the layer's syntax verdict, how many
/// errors it lent, how much of the text its document covers, and the validator's findings, in the
/// order it reported them, and its own verdict.
struct Verdict<'s> {
  valid: bool,
  lent: usize,
  recovery: Recovery,
  findings: Vec<Diagnostic<&'s str>>,
  validated: Result<(), Invalid>,
}

/// `text` read by `parser`, and the document it answers validated against `schema` under `budget`
/// and `rules`: the layer's verdict and the validator's, side by side.
///
/// Two checks every caller relies on are made here. `Ok` goes with an empty sink, and `Err` with
/// an empty sink only when a budget refused the document with its own rule outside `rules`, which
/// is the one way to report nothing and refuse; and the count the verdict carries is what the sink
/// received. al8n/smear#196.
///
/// The layer's own `Result` is answered as it is: a caller knows its layer.
fn verdict<'s, P>(
  parser: &P,
  schema: &Schema,
  text: &'s str,
  budget: &Budget,
  rules: RuleSet,
) -> Result<Verdict<'s>, P::Failure>
where
  P: Layer<'s>,
{
  let mut lent = 0usize;
  let mut sink = from_fn(|_: &P::Error| {
    lent += 1;
    ControlFlow::Continue(())
  });
  let parsed = parser.executable_document(text, &mut sink)?;
  let mut scratch = Scratch::new();
  let mut findings: Vec<Diagnostic<&'s str>> = Vec::new();
  let mut collect = Collect::new(&mut findings);
  let validated = validate_executable_with(
    schema,
    parsed.document(),
    &mut scratch,
    budget,
    rules,
    &mut collect,
  );
  match validated {
    Ok(()) => assert!(
      findings.is_empty(),
      "the validator reported findings under an `Ok` verdict\n---\n{text}"
    ),
    Err(invalid) => {
      assert!(
        !findings.is_empty() || invalid.budget_tripped(),
        "the validator returned `Err` with an empty sink and no budget refusal\n---\n{text}"
      );
      assert_eq!(
        invalid.emitted() as usize,
        findings.len(),
        "the verdict's count is not what the sink received\n---\n{text}"
      );
    }
  }
  Ok(Verdict {
    valid: parsed.is_valid(),
    lent,
    recovery: parsed.recovery(),
    findings,
    validated,
  })
}

/// The verdict a layer answered for `text`, which the caller knows is a document to that layer.
fn answered<'s, F>(read: Result<Verdict<'s>, F>, text: &str) -> Verdict<'s> {
  read.unwrap_or_else(|_| panic!("the layer answers no document\n---\n{text}"))
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

/// Every document in the corpus: both halves of every fixture, with the schema, budget and rule
/// set that fixture is measured under.
///
/// The schema is built once per fixture and both halves borrow it, so the sweep pays thirty
/// builds rather than sixty for a value that is by construction the same on both sides.
fn documents() -> Vec<(&'static Fixture, Schema, Budget, RuleSet)> {
  FIXTURES
    .iter()
    .map(|fixture| {
      (
        fixture,
        build(fixture.schema.unwrap_or(SCHEMA)),
        fixture.budget.unwrap_or_default(),
        fixture.rules.unwrap_or(RuleSet::ALL),
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

/// The two layers answer the same verdicts and the validator the same diagnostics, as values, over
/// the whole rule corpus.
#[test]
fn the_two_layers_agree() {
  let mut compared = 0usize;
  let mut diagnostics = 0usize;
  let mut rules: BTreeSet<Rule> = BTreeSet::new();

  for (fixture, schema, budget, rule_set) in documents() {
    for source in halves(fixture) {
      for (form, padded) in forms(source) {
        let expected = answered(
          verdict(&Syntactic::new(), &schema, &padded, &budget, rule_set),
          &padded,
        );
        let actual = answered(
          verdict(&Lossless::new(), &schema, &padded, &budget, rule_set),
          &padded,
        );

        assert!(
          expected.valid && actual.valid,
          "{:?} ({form}): a layer calls a corpus document invalid\n---\n{padded}",
          fixture.rule
        );
        assert_eq!(
          (expected.lent, actual.lent),
          (0, 0),
          "{:?} ({form}): a layer lent a syntax error for a corpus document\n---\n{padded}",
          fixture.rule
        );
        assert!(
          expected.recovery.is_complete() && actual.recovery.is_complete(),
          "{:?} ({form}): a layer's document leaves out part of a text both read whole\n---\n\
           {padded}",
          fixture.rule
        );
        assert_eq!(
          actual.recovery, expected.recovery,
          "{:?} ({form}): the two layers count two recoveries\n---\n{padded}",
          fixture.rule
        );
        assert_eq!(
          actual.findings, expected.findings,
          "{:?} ({form}): the two layers disagree\n---\n{padded}",
          fixture.rule
        );
        assert_eq!(
          actual.validated, expected.validated,
          "{:?} ({form}): the validator answers two verdicts\n---\n{padded}",
          fixture.rule
        );

        diagnostics += expected.findings.len();
        rules.extend(expected.findings.iter().map(Diagnostic::rule));
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

/// The corpus produces diagnostics through **both** layers, and reaches every rule through both.
///
/// [`the_two_layers_agree`] counts what the syntactic layer's document produced, which is the
/// right thing to floor when the two are equal. This one counts them separately, so a lossless
/// layer whose document silently reported nothing could not hide behind an equality that happened
/// to hold because both sides were empty.
#[test]
fn the_agreement_is_not_vacuous() {
  let mut syntactic_total = 0usize;
  let mut lossless_total = 0usize;
  let mut syntactic_rules: BTreeSet<Rule> = BTreeSet::new();
  let mut lossless_rules: BTreeSet<Rule> = BTreeSet::new();
  let mut invalid_documents_with_findings = 0usize;

  for (fixture, schema, budget, rules) in documents() {
    for source in halves(fixture) {
      for (_, padded) in forms(source) {
        let expected = answered(
          verdict(&Syntactic::new(), &schema, &padded, &budget, rules),
          &padded,
        )
        .findings;
        let actual = answered(
          verdict(&Lossless::new(), &schema, &padded, &budget, rules),
          &padded,
        )
        .findings;
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
    "the syntactic layer's documents produced only {syntactic_total} diagnostics over the corpus"
  );
  assert!(
    lossless_total >= DIAGNOSTIC_FLOOR,
    "the lossless layer's documents produced only {lossless_total} diagnostics over the corpus"
  );
  assert_eq!(syntactic_total, lossless_total);
  assert_eq!(syntactic_rules, lossless_rules);
  assert!(
    syntactic_rules.len() >= RULE_FLOOR,
    "the corpus reaches only {} rules through the layers, floor is {RULE_FLOOR}",
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

  let a = answered(
    verdict(&Syntactic::new(), &schema, compact, &budget, RuleSet::ALL),
    compact,
  )
  .findings;
  assert!(
    !a.is_empty(),
    "the control document produces no diagnostics"
  );

  // The positive leg first, so "everything is unequal" cannot be why the rest passes.
  let again = answered(
    verdict(&Lossless::new(), &schema, compact, &budget, RuleSet::ALL),
    compact,
  );
  assert_eq!(a, again.findings);
  assert!(again.recovery.is_complete());

  let b = answered(
    verdict(&Lossless::new(), &schema, shifted, &budget, RuleSet::ALL),
    shifted,
  )
  .findings;
  assert_ne!(
    a, b,
    "the same document one byte later compares equal, so spans are not being compared and this \
     gate cannot see a span defect at all"
  );

  let c = answered(
    verdict(&Lossless::new(), &schema, renamed, &budget, RuleSet::ALL),
    renamed,
  )
  .findings;
  assert_ne!(
    a, c,
    "two documents differing only in a field name compare equal, so subject slices are not being \
     compared"
  );

  // Order. The corpus's multi-diagnostic documents are what makes this reachable.
  let multi = "{ dog { meowVolume } } fragment unused on Dog { purrVolume }";
  let ordered = answered(
    verdict(&Syntactic::new(), &schema, multi, &budget, RuleSet::ALL),
    multi,
  )
  .findings;
  assert!(
    ordered.len() >= 2,
    "the order control needs a document with at least two diagnostics, got {}",
    ordered.len()
  );
  let mut reversed = ordered.clone();
  reversed.reverse();
  assert_ne!(
    ordered, reversed,
    "the same diagnostics in the opposite order compare equal, so the layers could disagree on \
     order and this gate would not say so"
  );
}

// ---------------------------------------------------------------------------------------------
// control 2 — a rule missing from one layer's run is caught
// ---------------------------------------------------------------------------------------------

/// For **every** rule in the corpus, switching it off where the lossless layer's document is
/// validated alone reds the comparison.
///
/// The perturbation the differential exists to catch, planted mechanically rather than once by
/// hand: it proves the gate can see a rule that stopped running for one layer's document, for each
/// rule individually, instead of proving it for whichever rule the author happened to pick.
#[test]
fn dropping_one_rule_from_one_layer_is_caught() {
  let mut caught = 0usize;
  for fixture in FIXTURES {
    let schema = build(fixture.schema.unwrap_or(SCHEMA));
    let budget = fixture.budget.unwrap_or_default();
    let rules = fixture.rules.unwrap_or(RuleSet::ALL);

    let expected = answered(
      verdict(&Syntactic::new(), &schema, fixture.invalid, &budget, rules),
      fixture.invalid,
    )
    .findings;
    let perturbed = answered(
      verdict(
        &Lossless::new(),
        &schema,
        fixture.invalid,
        &budget,
        rules.without(fixture.rule),
      ),
      fixture.invalid,
    )
    .findings;
    assert_ne!(
      expected, perturbed,
      "{:?}: the lossless layer's document was validated without it and the comparison still \
       agreed\n---\n{}",
      fixture.rule, fixture.invalid
    );
    caught += 1;
  }
  assert_eq!(caught, FIXTURES.len());
}

// ---------------------------------------------------------------------------------------------
// spans
// ---------------------------------------------------------------------------------------------

/// A diagnostic of the lossless layer's document points at the same **bytes**, and the padding
/// moves it.
///
/// [`the_two_layers_agree`] already compares spans, because they are fields of the value it
/// compares. This adds the two things an `assert_eq!` cannot say on its own: that the offsets
/// address the text they claim to (the slices are read out and compared), and that the padded
/// corpus actually moved them — a sweep in which every form produced identical offsets would be
/// eight copies of the compact run wearing eight names.
#[test]
fn a_diagnostic_points_at_the_same_bytes_through_both_layers() {
  let mut checked = 0usize;
  let mut moved = 0usize;

  for (fixture, schema, budget, rules) in documents() {
    for source in halves(fixture) {
      let compact = answered(
        verdict(&Syntactic::new(), &schema, source, &budget, rules),
        source,
      )
      .findings;
      if compact.is_empty() {
        continue;
      }

      for (form, padded) in forms(source) {
        let expected = answered(
          verdict(&Syntactic::new(), &schema, &padded, &budget, rules),
          &padded,
        )
        .findings;
        let actual = answered(
          verdict(&Lossless::new(), &schema, &padded, &budget, rules),
          &padded,
        )
        .findings;
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
            "{:?} ({form}) #{index}: the two layers' documents are blamed at different bytes",
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

  for (fixture, schema, budget, rules) in documents() {
    for source in halves(fixture) {
      for (form, padded) in forms(source) {
        if form == "compact" {
          continue;
        }
        let found = answered(
          verdict(&Syntactic::new(), &schema, &padded, &budget, rules),
          &padded,
        )
        .findings;
        for diagnostic in found {
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
// the recovering half — what the syntactic layer has no answer for
// ---------------------------------------------------------------------------------------------

/// A document somebody is still typing gets the rules run over the part that is well-formed.
///
/// The reason the lossless layer exists, measured rather than asserted: every corpus document is
/// truncated at every token boundary and read through it, and the run records how many of those
/// broken documents still produced a finding. A layer that answered none of them would compile,
/// pass every other test in this file, and be useless.
#[test]
fn a_half_typed_document_still_gets_an_answer() {
  let mut truncations = 0usize;
  let mut answered_with_a_finding = 0usize;
  let mut recovered_partially = 0usize;

  for (fixture, schema, budget, rules) in documents() {
    for source in halves(fixture) {
      for cut in boundaries(source) {
        if cut == 0 || cut == source.len() {
          continue;
        }
        let half = &source[..cut];
        let read = answered(
          verdict(&Lossless::new(), &schema, half, &budget, rules),
          half,
        );
        truncations += 1;
        if !read.findings.is_empty() {
          answered_with_a_finding += 1;
        }
        if read.recovery.projected() > 0 && !read.recovery.is_complete() {
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
    answered_with_a_finding >= 400,
    "only {answered_with_a_finding} of {truncations} half-typed documents produced a finding; \
     the layer is refusing the case it exists for"
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
  let read = answered(
    verdict(&Lossless::new(), &schema, source, &budget, RuleSet::ALL),
    source,
  );

  assert!(!read.valid && read.lent >= 1, "the text is not a document");
  assert_eq!(read.recovery.projected(), 1);
  assert!(!read.recovery.is_complete());
  assert_eq!(read.findings.len(), 1);
  assert_eq!(read.findings[0].rule(), Rule::FieldSelections);
  assert_eq!(
    &source[read.findings[0].span().start()..read.findings[0].span().end()],
    "meowVolume"
  );
}

/// Type-system syntax in an executable text is rubble the recovery steps over.
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
  let read = answered(
    verdict(&Lossless::new(), &schema, source, &budget, RuleSet::ALL),
    source,
  );

  assert!(!read.valid && read.lent >= 1, "the text is not a document");
  assert_eq!(
    read.recovery.projected(),
    2,
    "both operations should survive"
  );
  assert!(
    read.recovery.skipped() > 0,
    "the extension should be dropped rather than projected"
  );
  // 5.3.1 for `meowVolume`, and 5.2.3.1 twice because two anonymous operations survived.
  let rules: BTreeSet<Rule> = read.findings.iter().map(Diagnostic::rule).collect();
  assert!(rules.contains(&Rule::FieldSelections), "{rules:?}");
  assert!(rules.contains(&Rule::LoneAnonymousOperation), "{rules:?}");

  // The braced-body case, pinned rather than described.
  let braced = "{ dog { meowVolume } } type T { f: Int } { dog { name } }";
  let read = answered(
    verdict(&Lossless::new(), &schema, braced, &budget, RuleSet::ALL),
    braced,
  );
  assert!(!read.valid && read.lent >= 1, "the text is not a document");
  assert_eq!(
    (read.recovery.projected(), read.recovery.skipped()),
    (3, 2),
    "the SDL body no longer becomes a third, accidental operation; if the parser's recovery \
     changed, this is the line that says so"
  );
}

/// Nothing projected is reported as nothing projected, and **not** as a clean document.
///
/// A document from the lossless layer that validates with an empty sink is not thereby a valid
/// text. The validator answers `Ok` here because no rule fired, which is true and useless: the
/// layer projected no definition, and its own two answers say so, `is_valid()` false and
/// `Recovery::projected() == 0`. Pinned here so a caller reading the validator's verdict alone is
/// a documented mistake rather than a surprise.
#[test]
fn nothing_projected_is_visible_in_the_recovery() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let source = "{ dog { name @ } }";
  let read = answered(
    verdict(&Lossless::new(), &schema, source, &budget, RuleSet::ALL),
    source,
  );

  assert_eq!(
    read.validated,
    Ok(()),
    "no rule can fire over a document with no definitions in it"
  );
  assert!(read.findings.is_empty());
  assert!(
    !read.valid && read.lent >= 1,
    "the validator's `Ok` is not the text's verdict"
  );
  assert_eq!(read.recovery.projected(), 0);
  assert!(!read.recovery.is_complete());
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
  let read = answered(
    verdict(&Lossless::new(), &schema, source, &budget, RuleSet::ALL),
    source,
  );

  assert!(!read.valid && read.lent >= 1, "the text is not a document");
  assert_eq!(read.recovery.projected(), 1, "only the operation survives");
  assert!(!read.recovery.is_complete());
  assert!(
    read
      .findings
      .iter()
      .any(|d| d.rule() == Rule::FragmentSpreadTargetDefined),
    "the artifact this test documents did not appear: {:#?}",
    read.findings
  );

  // And the same document with the fragment intact reports nothing, so the finding above is the
  // recovery's and not the document's.
  let whole = "{ dog { ...named } } fragment named on Dog { name }";
  let read = answered(
    verdict(&Lossless::new(), &schema, whole, &budget, RuleSet::ALL),
    whole,
  );
  assert!(read.valid && read.lent == 0, "the whole text is a document");
  assert!(read.recovery.is_complete());
  assert!(read.findings.is_empty(), "{:#?}", read.findings);
}

// ---------------------------------------------------------------------------------------------
// two exits
// ---------------------------------------------------------------------------------------------

/// One error the lossless layer lends.
type Lent<'s> = GraphqlLosslessErrorValue<&'s str>;

/// `text` through the lossless layer and then the validator, each exit held to what may leave by
/// it: the errors the layer lends number `error_count()`, and each has a code of the parser's; each
/// finding in the validator's sink has a rule's code; and when the layer projected nothing, the
/// validator's sink holds nothing, since no rule fires over a document with no definition in it.
/// Answers how many errors were lent, how many findings the validator reported, and whether the
/// layer projected nothing.
fn exits(schema: &Schema, text: &str, budget: &Budget, rules: RuleSet) -> (usize, usize, bool) {
  let mut lent: Vec<Lent<'_>> = Vec::new();
  let parsed = Lossless::new()
    .executable_document(text, &mut lent)
    .unwrap_or_else(|_| panic!("the lossless layer answers no document\n---\n{text}"));
  let nothing = parsed.recovery().projected() == 0;
  assert_eq!(
    lent.len(),
    parsed.error_count(),
    "the errors lent are not the errors counted\n---\n{text}"
  );
  for error in &lent {
    assert!(
      error.code().as_str().starts_with("smear::syntax::"),
      "the layer lent `{}`, which is no syntax error\n---\n{text}",
      error.code().as_str()
    );
  }
  let mut findings: Vec<Diagnostic<&str>> = Vec::new();
  let _ = validate_executable_with(
    schema,
    parsed.document(),
    &mut Scratch::new(),
    budget,
    rules,
    &mut Collect::new(&mut findings),
  );
  for finding in &findings {
    assert!(
      finding
        .rule()
        .code()
        .as_str()
        .starts_with("smear::validation::"),
      "the validator's sink holds `{}`, which is no finding\n---\n{text}",
      finding.rule().code().as_str()
    );
  }
  assert!(
    !nothing || findings.is_empty(),
    "the layer projected nothing, and the validator's sink holds {findings:#?}\n---\n{text}"
  );
  (lent.len(), findings.len(), nothing)
}

/// T15: a syntax error leaves by the layer's sink, and a finding by the validator's.
///
/// Through the lossless layer, over each corpus document that fires a rule, which is a document
/// and lends nothing, over every truncation [`a_half_typed_document_still_gets_an_answer`] reads,
/// and over the text [`nothing_projected_is_visible_in_the_recovery`] reads, so that both exits
/// are seen to carry something: every error lent is counted by `error_count()` and has a code of
/// the parser's, `smear::syntax::…`, and every diagnostic in the validator's sink has a rule's
/// code, `smear::validation::…`. A rule's code is all a finding can carry, so a syntax error that
/// reached the validator's sink would wear one; what tells it apart is a text the layer projects
/// nothing of, which lends errors while the validator's sink holds nothing.
#[test]
fn syntax_errors_leave_by_the_layer_and_findings_by_the_validator() {
  let (mut truncations, mut lending, mut finding, mut both) = (0usize, 0usize, 0usize, 0usize);
  let mut projected_nothing = 0usize;
  for (fixture, schema, budget, rules) in documents() {
    let (lent, _, _) = exits(&schema, fixture.invalid, &budget, rules);
    assert_eq!(
      lent, 0,
      "{:?}: a document of the rule corpus lent a syntax error\n---\n{}",
      fixture.rule, fixture.invalid
    );
    for source in halves(fixture) {
      for cut in boundaries(source) {
        if cut == 0 || cut == source.len() {
          continue;
        }
        let (lent, found, nothing) = exits(&schema, &source[..cut], &budget, rules);
        truncations += 1;
        if lent > 0 {
          lending += 1;
        }
        if found > 0 {
          finding += 1;
        }
        if lent > 0 && found > 0 {
          both += 1;
        }
        if nothing {
          projected_nothing += 1;
        }
      }
    }
  }
  let source = "{ dog { name @ } }";
  let (lent, _, nothing) = exits(&build(SCHEMA), source, &Budget::default(), RuleSet::ALL);
  assert!(lent >= 1, "`{source}` lent no error");
  if nothing {
    projected_nothing += 1;
  }
  println!(
    "T15: {truncations} truncations; {lending} lent an error, {finding} held a finding, {both} \
     did both; {projected_nothing} texts projected nothing"
  );
  // `nothing_projected_is_visible_in_the_recovery` holds that the layer projects nothing of
  // `{ dog { name @ } }`.
  assert!(
    projected_nothing >= 1,
    "no text projected nothing, so the validator's sink was never held to be empty"
  );
  // `a_half_typed_document_still_gets_an_answer` holds these truncations to at least 1 000, and
  // at least 400 of them to a finding. A truncation lends no error only when it is a whole
  // document, which at most `WHOLE_TRUNCATIONS` of them are.
  assert!(
    lending >= 1_000 - WHOLE_TRUNCATIONS,
    "only {lending} of {truncations} truncations lent an error"
  );
  assert!(
    finding >= 400,
    "only {finding} of {truncations} truncations held a finding"
  );
  assert!(
    both >= 400 - WHOLE_TRUNCATIONS,
    "only {both} of {truncations} truncations lent an error and held a finding"
  );
}

// ---------------------------------------------------------------------------------------------
// one scratch
// ---------------------------------------------------------------------------------------------

/// One `Scratch` serves a document from each layer in turn, and neither leaves anything in it for
/// the next.
#[test]
fn the_two_layers_share_a_scratch() {
  let schema = build(SCHEMA);
  let budget = Budget::default();
  let mut scratch = Scratch::new();

  let bad = "{ dog { meowVolume } }";
  let good = "{ dog { name } }";

  for _ in 0..3 {
    let lossless = Lossless::new()
      .executable_document(bad, &mut ())
      .expect("short enough for a tree");
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    assert!(
      validate_executable(
        &schema,
        lossless.document(),
        &mut scratch,
        &budget,
        &mut sink
      )
      .is_err()
    );
    assert_eq!(collected.len(), 1);

    let syntactic = Syntactic::new()
      .executable_document(good, &mut ())
      .expect("the syntactic layer reads it");
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    assert!(
      validate_executable(
        &schema,
        syntactic.document(),
        &mut scratch,
        &budget,
        &mut sink
      )
      .is_ok(),
      "the validator saw what the lossless layer's document left in the scratch"
    );
  }
}
