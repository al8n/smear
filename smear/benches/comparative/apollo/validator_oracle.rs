//! **G2 — the differential oracle.** smear's draft §5 validator against `apollo-compiler` 1.32.0.
//!
//! Five gates ride one corpus:
//!
//! * [`the_oracle_agrees`] — every case, verdict against verdict, with the directional whitelist.
//! * [`the_corpus_reaches_every_rule`] — a corpus that never exercises half the rule set would
//!   agree with anything, so the census is mechanical.
//! * [`every_whitelist_class_is_exercised`] — an excuse nothing hits is an excuse nobody has
//!   checked.
//! * [`every_sdl_constant_position_probe_is_refused_by_both`] — the schema comparison's own
//!   census, one apollo error name at a time. The census exists because its absence is what smear
//!   issue #95 was about: three checks nothing in the corpus reached, so the oracle was green
//!   about them without ever having asked.
//! * [`the_harness_reds_when_a_rule_goes_lax`] — the discrimination proof. Runs the whole corpus
//!   once per rule with that rule switched off and requires a red, so "the oracle agrees" cannot
//!   be true merely because the oracle cannot tell.
//!
//! Run with `--nocapture` to see the report; it is printed on the way past whether or not the
//! gate passes.

#![allow(missing_docs)]

// The shared harness. It is a module compiled into this target rather than a library
// linked into it; `support/mod.rs` carries why, and why this file sits under `benches/`.
mod support;

use std::collections::BTreeMap;

use crate::support::oracle::{
  Class, Divergence, Oracle, Report, SchemaOutcome, apollo_schema_error_names, build_schemas,
  corpus, gaps, whitelist::Expectation,
};
use smear::validator::Rule;

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

/// Rules with no case, each with the reason written down.
///
/// A [`Rule`] belongs here only when the *oracle* cannot express it — not when writing the case is
/// merely awkward. Both entries are this crate's resource policy over draft 5.3.2, which the
/// specification leaves unbounded: a document that trips one is refused by smear and accepted by
/// apollo, which has no such bound, so any case reaching one would be an
/// [`UndeclaredStricter`](Divergence::UndeclaredStricter) divergence rather than coverage. The
/// bounds are gated where they can be: `smear/tests/validator_merge.rs` drives them directly.
///
/// The list is asserted **exactly**, so a rule that acquires a case must be deleted from here.
const UNREACHABLE_BY_DIFFERENTIAL: &[(Rule, &str)] = &[
  (
    Rule::MergeDepthBudget,
    "a resource bound, not a draft rule: apollo has no depth limit, so tripping it is smear \
     being stricter with nothing to compare against",
  ),
  (
    Rule::MergeWorkBudget,
    "likewise a resource bound; the same argument as MergeDepthBudget",
  ),
];

/// The corpus must reach every rule, or "we agree" is a statement about the corpus.
///
/// A rule added to `Rule::ALL` fails here until a case exists for it. That is deliberately a
/// one-line addition to `corpus::PAIRS` or `whitelist_probes`, not a restructuring: the harness has
/// no per-rule machinery to extend.
#[test]
fn the_corpus_reaches_every_rule() {
  let exercised = corpus::exercised_rules();
  let missing: Vec<_> = Rule::ALL
    .iter()
    .copied()
    .filter(|rule| !exercised.contains(rule))
    .collect();
  let excused: Vec<Rule> = UNREACHABLE_BY_DIFFERENTIAL
    .iter()
    .map(|(rule, _)| *rule)
    .collect();
  assert_eq!(
    missing, excused,
    "the set of rules the differential corpus does not reach has moved. A rule that gained a \
     case must leave UNREACHABLE_BY_DIFFERENTIAL; a rule that lost one needs a case added to \
     `corpus::PAIRS` or `corpus::whitelist_probes`, next to its neighbours.\n  observed \
     unreached: {missing:?}\n  declared unreachable: {excused:?}"
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
/// A [`Gap`](crate::support::oracle::Gap) says smear is laxer than the oracle on a named draft
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

/// The SDL constant-position checks, one apollo error name at a time.
///
/// This gate has run in both directions. While the checks were missing it asserted the *gap's*
/// width — every probe smear-accepts / apollo-rejects — so a partial fix reddened naming the probe
/// that moved. Now that all nine have landed it asserts the same thing from the other side: every
/// probe must be refused by **both** implementations, so removing any one check reddens naming the
/// probe that stopped being caught. An aggregate would stay healthy through either half-move; this
/// cannot.
///
/// What it does *not* pin is smear refusing for the *right* reason — `BothRejected` only says both
/// said no. `smear/tests/validator_schema.rs::refusal_floor` is what pins the exact
/// `SchemaErrorKind` set each of these SDLs produces, and the two together are the property.
#[test]
fn every_sdl_constant_position_probe_is_refused_by_both() {
  let mut moved = Vec::new();
  for (apollo_error, sdl) in corpus::SDL_DIRECTIVE_PROBES {
    let outcome = build_schemas(sdl);
    match &outcome {
      Err(SchemaOutcome::BothRejected) => {}
      Ok(_) => moved.push(format!("  {apollo_error}: both sides now accept the SDL")),
      Err(other) => moved.push(format!("  {apollo_error}: {other:?}")),
    }
    // Naming apollo's own typed variant is what keeps a probe honest when its SDL is edited: an
    // SDL that grew a second defect would still be `BothRejected` while measuring something else.
    let names = apollo_schema_error_names(sdl);
    if !names.contains(apollo_error) {
      moved.push(format!(
        "  {apollo_error}: apollo now reports {names:?} for this SDL, so the probe no longer \
         measures the check it is named for"
      ));
    }
  }
  assert!(
    moved.is_empty(),
    "an SDL constant-position probe changed verdict. Either a check in \
     `validator::schema::builder` was lost — the usual cause — or apollo moved under the \
     `=1.32.0` pin.\n{}",
    moved.join("\n")
  );

  // One control per probed shape. A control that does not share the probe's shape proves nothing
  // about it, so `@onObject(a: { x: 1 })` is here to show that the two input-object probes measure
  // the literal's contents rather than the presence of an input object in a directive argument.
  for (name, sdl) in corpus::SDL_DIRECTIVE_CONTROLS {
    assert!(
      build_schemas(sdl).is_ok(),
      "the control SDL `{name}` — a correct usage — is not accepted by both sides, so the probes \
       that share its shape prove nothing"
    );
  }
}

/// The §3 rules smear has and the oracle does not, held to the same two conditions from the other
/// side.
///
/// Seven of them landed at once, and the fact worth gating is that **the oracle can see none of
/// them**: `apollo-compiler` 1.32.0 accepts six outright and rejects the seventh only because it
/// has no `@oneOf` built-in. A green differential therefore said nothing about whether those checks
/// existed, and would say nothing if they were deleted — which is exactly the shape of defect the
/// audit behind this table found three of.
///
/// So the measurement is asserted rather than written down, in both directions:
///
/// * **smear still refuses.** A check lost is a red here, with the row named, while
///   [`the_oracle_agrees`] stays green.
/// * **apollo still behaves as declared.** Each row carries apollo's typed error names for its SDL,
///   so a bump under the `=1.32.0` pin that grows one of these rules reds here instead of quietly
///   turning a hand-expected rule into an oracle-visible one nobody re-measured.
///
/// The controls are what stop a row proving only that smear refuses *something*: each is the
/// smallest edit its rule permits, and smear must accept it.
#[test]
fn every_smear_only_schema_check_is_still_smear_only() {
  let mut moved = Vec::new();

  for (label, apollo_errors, sdl) in corpus::SDL_SMEAR_ONLY_PROBES {
    match build_schemas(sdl) {
      Err(SchemaOutcome::SmearRejected(_)) => {}
      Err(SchemaOutcome::BothRejected) if !apollo_errors.is_empty() => {}
      Ok(_) => moved.push(format!(
        "  {label}: smear now ACCEPTS the SDL — the §3 check it is named for is gone"
      )),
      Err(other) => moved.push(format!("  {label}: {other:?}")),
    }
    let names = apollo_schema_error_names(sdl);
    if names != *apollo_errors {
      moved.push(format!(
        "  {label}: apollo reports {names:?} for this SDL, not the declared {apollo_errors:?} — \
         either the oracle grew the rule (delete the row and add a corpus case) or the SDL \
         acquired a second defect"
      ));
    }
  }

  for (label, apollo_errors, sdl) in corpus::SDL_SMEAR_ONLY_CONTROLS {
    match build_schemas(sdl) {
      Ok(_) => {}
      Err(SchemaOutcome::ApolloRejected(_) | SchemaOutcome::BothRejected)
        if !apollo_errors.is_empty() => {}
      Err(other) => moved.push(format!(
        "  {label} (control): smear does not accept it — {other:?}. The probe that shares its \
         shape now proves only that smear refuses something"
      )),
    }
    let names = apollo_schema_error_names(sdl);
    if names != *apollo_errors {
      moved.push(format!(
        "  {label} (control): apollo reports {names:?}, not the declared {apollo_errors:?}"
      ));
    }
  }

  assert!(
    moved.is_empty(),
    "a schema-stage check that only smear performs changed verdict.\n{}",
    moved.join("\n")
  );

  // These SDLs must never join `corpus::all()`: an SDL only smear refuses is a
  // `SchemaOutcome::SmearRejected`, which the runner counts as a failure with no door, so a suite
  // carrying one would turn this measurement into a permanent red.
  let corpus_sdls: Vec<String> = corpus::all().into_iter().map(|suite| suite.sdl).collect();
  for (label, _, sdl) in corpus::SDL_SMEAR_ONLY_PROBES {
    assert!(
      !corpus_sdls.iter().any(|corpus_sdl| corpus_sdl == sdl),
      "{label}'s SDL is in the runner's corpus, where it can only ever be a one-sided schema"
    );
  }
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
    crate::support::oracle::APOLLO_VERSION,
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
///
/// Its two entries today are there for the *other* reason a rule can be invisible — no case
/// reaches it at all — and both are the resource bounds
/// [`UNREACHABLE_BY_DIFFERENTIAL`] explains. That the two lists agree is a coincidence of the
/// current corpus, not a derivation: a rule with a case can still be undetectable, which is what
/// the paragraph above describes, so the two are asserted separately.
const UNDETECTABLE_LOSSES: &[(Rule, &str)] = &[
  (
    Rule::MergeDepthBudget,
    "no case reaches it: a document that trips a resource bound apollo does not have would be a \
     divergence rather than coverage — see UNREACHABLE_BY_DIFFERENTIAL",
  ),
  (
    Rule::MergeWorkBudget,
    "likewise unreached; the same argument as MergeDepthBudget",
  ),
];

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
