//! **G2 — the differential oracle.** smear's draft §5 validator against `apollo-compiler` 1.32.0.
//!
//! Four gates ride one corpus:
//!
//! * [`the_oracle_agrees`] — every case, verdict against verdict, with the directional whitelist.
//! * [`the_corpus_reaches_every_rule`] — a corpus that never exercises half the rule set would
//!   agree with anything, so the census is mechanical.
//! * [`every_whitelist_class_is_exercised`] — an excuse nothing hits is an excuse nobody has
//!   checked.
//! * [`the_harness_reds_when_a_rule_goes_lax`] — the discrimination proof. Runs the whole corpus
//!   once per rule with that rule switched off and requires a red, so "the oracle agrees" cannot
//!   be true merely because the oracle cannot tell.
//!
//! Run with `--nocapture` to see the report; it is printed on the way past whether or not the
//! gate passes.

#![allow(missing_docs)]

use std::collections::BTreeMap;

use smear::validator::Rule;
use smear_apollo_bench::oracle::{
  Class, Divergence, Oracle, Report, SchemaOutcome, build_schemas, corpus, gaps,
  whitelist::Expectation,
};

fn run() -> Report {
  Oracle::new().run(&corpus::all())
}

// ---------------------------------------------------------------------------------------------
// the gate
// ---------------------------------------------------------------------------------------------

#[test]
fn the_oracle_agrees() {
  let report = run();
  println!("{report}");
  assert!(
    report.passed(),
    "the differential oracle found a disagreement\n\n{report}"
  );
}

/// The corpus must reach every rule, or "we agree" is a statement about the corpus.
///
/// A rule added to `Rule::ALL` — draft 5.3.2 is the one still outstanding — fails here until a
/// case exists for it. That is deliberately a one-line addition to `corpus::PAIRS` or
/// `whitelist_probes`, not a restructuring: the harness has no per-rule machinery to extend.
#[test]
fn the_corpus_reaches_every_rule() {
  let exercised = corpus::exercised_rules();
  let missing: Vec<_> = Rule::ALL
    .iter()
    .copied()
    .filter(|rule| !exercised.contains(rule))
    .collect();
  assert!(
    missing.is_empty(),
    "the differential corpus exercises no case for {missing:?} — add one to `corpus::PAIRS` or \
     `corpus::whitelist_probes`, next to the rule's neighbours"
  );
}

/// Every whitelist class must be hit, and hit by a case that reached a comparison.
#[test]
fn every_whitelist_class_is_exercised() {
  let report = run();
  let unexercised = report.unexercised_classes();
  assert!(
    unexercised.is_empty(),
    "whitelist class(es) {unexercised:?} were never exercised. A class nothing hits is either \
     dead — apollo grew the rule and the exception should be deleted — or the corpus is too thin \
     to reach it. Both need saying out loud.\n\n{report}"
  );
  for class in Class::ALL {
    let hits = report.class_hits.get(class).copied().unwrap_or(0);
    println!("{class:?}: {hits} hit(s)  — {class}");
  }
}

/// The other direction's exception list, held to the same two conditions.
///
/// A [`Gap`](smear_apollo_bench::oracle::Gap) says smear is laxer than the oracle on a named draft
/// section because that rule is openly unimplemented. Both halves have to keep being true:
///
/// * **still open** — no `Rule` may claim the section. Phase 3 landing draft 5.3.2 flips this and
///   the message says to delete the entry.
/// * **still exercised** — the corpus's merge cases still have to *reach* it. If they stop, the
///   rule has quietly started passing them, which is the same signal by a different route.
///
/// Together they are why phase 3 needs a one-line deletion here and nothing else in the harness.
#[test]
fn every_open_gap_is_still_open_and_still_exercised() {
  let stale = gaps::stale();
  assert!(
    stale.is_empty(),
    "a Rule now claims the section of {} declared gap(s) — the rule landed, so delete the entry \
     from `oracle::gaps::GAPS`:\n{}",
    stale.len(),
    stale
      .iter()
      .map(|gap| format!("  draft {} {} ({})\n", gap.section, gap.title, gap.tracking))
      .collect::<String>()
  );

  let report = run();
  let closed = report.closed_gaps();
  assert!(
    closed.is_empty(),
    "declared gap(s) that nothing hit: {:?}. The corpus still contains the documents, so either \
     smear now implements the rule — delete the entry — or the cases that reached it were \
     removed.\n\n{report}",
    closed.iter().map(|gap| gap.section).collect::<Vec<_>>()
  );

  for gap in gaps::GAPS {
    println!(
      "gap draft {} {}: {} hit(s) — {}",
      gap.section,
      gap.title,
      report.gap_hits.get(gap.section).copied().unwrap_or(0),
      gap.tracking
    );
  }
}

/// An SDL only one implementation accepts is a §3 divergence, and there is no case-level door for
/// it: the only admissible explanation is a declared schema-stage gap, matched from apollo's own
/// error names.
#[test]
fn no_schema_is_accepted_by_only_one_side() {
  let report = run();
  assert!(
    report.schema_failures.is_empty(),
    "{} suite(s) built on one side and not the other\n\n{report}",
    report.schema_failures.len()
  );
}

/// The schema-stage gap, checked one apollo error name at a time.
///
/// An aggregate hit count would stay healthy while the gap half-closed. This asserts the width:
/// every probe must still be smear-accepts / apollo-rejects, and the control must still be
/// accepted by both, so a fix that lands `UnsupportedLocation` alone reds here naming exactly the
/// probe that moved.
#[test]
fn the_sdl_directive_gap_is_exactly_as_wide_as_declared() {
  let mut closed = Vec::new();
  for (name, sdl) in corpus::SDL_DIRECTIVE_PROBES {
    match build_schemas(sdl) {
      Err(SchemaOutcome::GapExplained(gap)) if gap.section == "3.13-usages" => {
        println!("sdl gap still open: {name}");
      }
      Ok(_) => closed.push(format!("  {name}: both sides now accept the SDL")),
      Err(other) => closed.push(format!("  {name}: {other:?}")),
    }
  }
  assert!(
    closed.is_empty(),
    "the SDL directive-usage gap has narrowed. Re-scope `oracle::gaps::GAPS`'s `3.13-usages` \
     entry — or delete it, if all six probes now agree.\n{}",
    closed.join("\n")
  );

  // The control proves the probes measure the *usage* check and not something incidental about
  // the SDL shape they share.
  assert!(
    build_schemas(corpus::SDL_DIRECTIVE_CONTROL).is_ok(),
    "the control SDL — a correct directive usage — is not accepted by both sides, so the probes \
     above prove nothing"
  );
}

// ---------------------------------------------------------------------------------------------
// re-verification of the four classes
// ---------------------------------------------------------------------------------------------

/// Re-measures each whitelist class against the `apollo-compiler` that actually resolved.
///
/// The design spec measured all four on 1.32.0 and `Cargo.toml` pins that release with `=`. This
/// test is what makes the pin a checkable claim: it asserts the *observed* behaviour, class by
/// class, so a bump that moves a gap fails here with the class named rather than silently
/// widening or narrowing what the whitelist excuses.
///
/// It is separate from [`the_oracle_agrees`] on purpose. That gate would also fail — a class whose
/// expectation stops holding is a `WhitelistViolated` divergence — but it would fail among
/// whatever else the corpus turned up. This one fails alone, saying which class moved.
#[test]
fn the_whitelist_still_describes_apollo() {
  let report = run();
  let violated: BTreeMap<Class, Vec<String>> = report
    .failures
    .iter()
    .filter_map(|failure| match &failure.divergence {
      Divergence::WhitelistViolated {
        class,
        smear,
        apollo,
        ..
      } => Some((
        *class,
        format!("{} (smear={smear}, apollo={apollo})", failure.case),
      )),
      _ => None,
    })
    .fold(BTreeMap::new(), |mut map, (class, case)| {
      map.entry(class).or_default().push(case);
      map
    });

  assert!(
    violated.is_empty(),
    "the whitelist no longer describes apollo-compiler {}. Re-run the feasibility measurement \
     before touching the pin in Cargo.toml.\n{violated:#?}\n\nexpectations:\n{}",
    smear_apollo_bench::oracle::APOLLO_VERSION,
    Class::ALL
      .iter()
      .map(|class| format!("  {class:?} requires {:?}: {class}\n", class.expectation()))
      .collect::<String>()
  );

  // The two directions are not interchangeable, and the type system is what keeps them apart.
  // Asserted here so a future edit that adds a "smear may be laxer" expectation is caught by a
  // test rather than by an incident.
  for class in Class::ALL {
    assert!(
      matches!(
        class.expectation(),
        Expectation::SmearStricter | Expectation::AgreeValid
      ),
      "{class:?} declares an expectation that lets smear be laxer than the oracle"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// the discrimination proof
// ---------------------------------------------------------------------------------------------

/// Rules whose loss the corpus cannot currently detect, each with the reason written down.
///
/// A rule lands here when switching it off leaves every document it governs *still* invalid for a
/// second reason, so no verdict moves and the oracle sees nothing. That is a real limit of a
/// verdict-level differential and the honest thing to do with it is name it, not widen the
/// tolerance until the test passes.
///
/// The list is asserted **exactly**: a rule that drops off it (the corpus grew a case that
/// isolates it) fails just as loudly as a rule that joins it.
const UNDETECTABLE_LOSSES: &[(Rule, &str)] = &[];

#[test]
fn the_harness_reds_when_a_rule_goes_lax() {
  let suites = corpus::all();
  let mut undetected = Vec::new();

  for rule in Rule::ALL {
    let report = Oracle::without(*rule).run(&suites);
    // `passed()` also requires every whitelist class to be hit, which is itself part of the
    // property: switching off 5.6.3 must break W1's expectation, because W1 exists precisely to
    // keep that rule honest while apollo cannot.
    if report.passed() {
      undetected.push(*rule);
    }
  }

  let expected: Vec<Rule> = UNDETECTABLE_LOSSES.iter().map(|(rule, _)| *rule).collect();
  assert_eq!(
    undetected, expected,
    "the set of rules whose loss the differential corpus cannot see has moved.\n\
     Detected-loss rules are the point of the gate; undetectable ones must be listed in \
     UNDETECTABLE_LOSSES with a reason.\n  observed undetectable: {undetected:?}\n  \
     declared undetectable: {expected:?}"
  );
}

/// One worked example of the red, kept separate so its message can be read.
///
/// 5.3.1 is the rule chosen because both implementations have it, apollo's copy lives in its build
/// stage (so this also proves the harness reaches that stage), and the fixture that exercises it
/// fires nothing else.
#[test]
fn a_lax_field_selections_rule_is_named_in_the_red() {
  let report = Oracle::without(Rule::FieldSelections).run(&corpus::curated());
  assert!(
    !report.passed(),
    "switching off {} produced no divergence at all",
    Rule::FieldSelections
  );

  let laxer: Vec<_> = report
    .failures
    .iter()
    .filter(|failure| matches!(failure.divergence, Divergence::SmearLaxer { .. }))
    .collect();
  assert!(
    !laxer.is_empty(),
    "switching off {} produced failures, but none of them was `SmearLaxer` — the harness noticed \
     something other than smear having gone lax\n\n{report}",
    Rule::FieldSelections
  );
  assert!(
    laxer
      .iter()
      .any(|failure| failure.exercises == Some(Rule::FieldSelections)),
    "the red does not name {}\n{}",
    Rule::FieldSelections,
    laxer
      .iter()
      .map(|failure| format!("{failure}\n"))
      .collect::<String>()
  );

  println!(
    "discrimination red ({} disabled), first three of {}:\n{}",
    Rule::FieldSelections,
    laxer.len(),
    laxer
      .iter()
      .take(3)
      .map(|failure| format!("{failure}\n"))
      .collect::<String>()
  );
}

// ---------------------------------------------------------------------------------------------
// corpus health
// ---------------------------------------------------------------------------------------------

/// A corpus whose cases mostly never reach a comparison is a corpus that proves nothing.
///
/// The threshold is deliberately not 100%: the mutation family generates syntax breakage on
/// purpose, and a document neither parser accepts is agreement at the syntactic level rather than
/// a defect. What it rules out is the failure this harness was written to avoid — a run that looks
/// busy because it has thousands of cases and compares a handful of them.
#[test]
fn most_of_the_corpus_reaches_a_comparison() {
  let report = run();
  assert!(report.cases > 0, "the corpus is empty");
  let ratio = report.compared as f64 / report.cases as f64;
  assert!(
    ratio > 0.5,
    "only {} of {} cases reached a disagreement-capable state ({:.1}%)\n\n{report}",
    report.compared,
    report.cases,
    ratio * 100.0
  );

  // Every curated family must be fully comparable: those documents are written to be read by both
  // implementations, so one that is not is a finding, not noise.
  for provenance in [
    corpus::Provenance::SpecFixture,
    corpus::Provenance::SpecExample,
    corpus::Provenance::WhitelistProbe,
    corpus::Provenance::Realistic,
  ] {
    let Some((cases, compared)) = report.by_provenance.get(&provenance).copied() else {
      panic!("no {provenance} cases ran at all");
    };
    assert_eq!(
      cases,
      compared,
      "{} of {cases} {provenance} cases did not reach a comparison\n\n{report}",
      cases - compared
    );
  }
}
