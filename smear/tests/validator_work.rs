//! The validation-wide work ceiling, the two constructions that made it necessary, and the
//! evidence that indexing them changed no diagnostic.
//!
//! # What this file is about
//!
//! [`Budget`]'s two knobs bound draft 5.3.2's merge engine, and the merge engine runs **last**.
//! Everything before it — the prep sweep, the fragment graph, the subscription root collection and
//! the one selection walk per operation that carries every per-node rule — ran with no ledger at
//! all. Two constructions turned `O(n)` of syntax into `O(n²)` of work there, and neither knob saw
//! it: not because a rule set had switched the merge bounds off, but because by the time the merge
//! engine is consulted the walk has already finished spending. al8n/smear#198.
//!
//! - **Variable usages.** Every usage scanned every definition of the operation, comparing names
//!   byte for byte. 4,000 of each, off 250 KB, measured 60 ms against 0.33 ms for the same
//!   declarations with one usage.
//! - **Shared fragments.** The operation loop restarts the selection walk for each operation, and
//!   `Frame::CHECK` suppresses a definition's repeated *diagnostics*, not its traversal. 3,200
//!   operations spreading one 3,200-field fragment — 129 KB — spent 189 ms in the walk.
//!
//! # The two halves, and why the halves are different kinds of test
//!
//! The **ceiling** is a refusal, so it is asserted exactly: rule, count, verdict, wall clock. The
//! **index** is a cost, so it is asserted as one — a scaling ratio wide enough that a quadratic
//! cannot pass it and noise cannot fail it, plus an absolute wall clock, both measured with the
//! merge rules out of the way. The merge engine has its own quadratic on these documents and its
//! own branch; leaving it in would make this file's numbers a measurement of that.
//!
//! # And the third half, which is the one that could go wrong quietly
//!
//! Replacing a scan with an index cannot change a diagnostic. [`the_index_marks_what_the_scan_did`]
//! and [`repeated_spreads_report_once_and_only_once`] are that claim, written so that they compile
//! and pass on the commit *before* the index as well as after it. A duplicated variable name is
//! where the risk is: every definition of it is marked used, and a usage resolves against the
//! **first**, so an index whose ties broke the other way would silently change which declaration
//! draft 5.8.5 reads.

// The same gate every other validator test carries: `smear::validator` does not exist in the
// crate's API surface with the feature off.
#![cfg(feature = "validator")]
#![allow(missing_docs)]

use std::{string::String, time::Instant, vec::Vec};

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{
    Budget, Collect, Diagnostic, Rule, RuleSet, Schema, Scratch, validate_executable_with,
  },
};

// ---------------------------------------------------------------------------------------------
// harness
// ---------------------------------------------------------------------------------------------

const SCHEMA: &str = r#"
type Query {
  dog: Dog
  nest: Nest
}

type Nest { nest: Nest leaf: Int name: String }

type Dog {
  name: String
  nickname: String
  barkVolume: Int
  isHouseTrained(atOtherHomes: Boolean): Boolean
  counted(times: Int): Boolean
}
"#;

fn build(sdl: &str) -> Schema {
  Schema::build(
    &Parser::with_parser::<
      GraphqlLexer<'_, str>,
      TypeSystemDocument<&str>,
      GraphqlErrors<&str>,
      _,
      GraphQL,
    >(type_system_document)
    .parse_str(sdl)
    .expect("the SDL parses"),
  )
  .expect("the SDL is a schema")
}

fn parse(source: &str) -> ExecutableDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .expect("the query parses")
}

/// Everything one run produces: whether a bound refused, what was emitted, and the wall clock.
struct Run {
  refused: bool,
  emitted: u32,
  diagnostics: Vec<(Rule, usize, usize, Option<String>)>,
  ms: f64,
}

impl Run {
  /// The rules that fired, sorted and deduplicated.
  fn rules(&self) -> Vec<Rule> {
    let mut rules: Vec<_> = self.diagnostics.iter().map(|(rule, ..)| *rule).collect();
    rules.sort_unstable();
    rules.dedup();
    rules
  }
}

fn run(schema: &Schema, source: &str, budget: &Budget, rules: RuleSet) -> Run {
  let document = parse(source);
  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let start = Instant::now();
  let verdict = validate_executable_with(schema, &document, &mut scratch, budget, rules, &mut sink);
  let ms = start.elapsed().as_secs_f64() * 1e3;
  let invalid = verdict.err();
  Run {
    refused: invalid.is_some_and(|invalid| invalid.budget_tripped()),
    emitted: invalid.map_or(0, |invalid| invalid.emitted()),
    // Rule, span, related span and the source spelling: everything about a diagnostic that an
    // index could have moved, in a form a test can print.
    diagnostics: collected
      .iter()
      .map(|diagnostic: &Diagnostic<&str>| {
        (
          diagnostic.rule(),
          diagnostic.span().start(),
          diagnostic
            .related_span()
            .map_or(usize::MAX, |span| span.start()),
          diagnostic.subject_source().map(|s| String::from(*s)),
        )
      })
      .collect(),
    ms,
  }
}

/// How long a refusal is allowed to take.
///
/// The ceiling is four million units and an unoptimised build spends roughly sixteen times what an
/// optimised one does on each — measured at 42 ms release against 663 ms debug for the same
/// document — so a single number here would either be vacuous in release or red in debug. Both
/// halves are assertions about the same property: reaching the ceiling costs a bounded amount of
/// time, and doubling the document does not change it.
///
/// `validator_merge.rs` gets away with one number because its budget is 65,536 units, sixty-four
/// times smaller, so its debug cost lands under the same threshold its release cost does.
fn refusal_budget_ms() -> f64 {
  if cfg!(debug_assertions) {
    4_000.0
  } else {
    500.0
  }
}

/// The fastest of `rounds` runs, kept whole so its diagnostics can be asserted on too.
fn best_of(rounds: usize, mut once: impl FnMut() -> Run) -> Run {
  let mut best = once();
  for _ in 1..rounds {
    let next = once();
    if next.ms < best.ms {
      best = next;
    }
  }
  best
}

/// [`RuleSet::ALL`] without draft 5.3.2 and its two bounds.
///
/// Not because the merge rules are off in production — they are not — but because the merge engine
/// has its own cost on exactly these documents, it is being repaired on its own branch, and a cost
/// assertion that included it would be measuring that repair instead of this one.
fn without_merge() -> RuleSet {
  RuleSet::ALL
    .without(Rule::FieldSelectionMerging)
    .without(Rule::MergeWorkBudget)
    .without(Rule::MergeDepthBudget)
}

// ---------------------------------------------------------------------------------------------
// the constructions
// ---------------------------------------------------------------------------------------------

/// `n` variable definitions, and `n` usages of the **last** one.
///
/// The last one, so a scan over the definitions runs to the end at every usage. Anything else
/// measures how lucky the fixture was.
fn variable_bomb(n: usize) -> String {
  let mut source = String::new();
  source.push_str("query q(");
  for i in 0..n {
    source.push_str(&std::format!("$v{i}: Boolean, "));
  }
  source.pop();
  source.pop();
  source.push_str(") { dog {");
  let last = n - 1;
  for i in 0..n {
    source.push_str(&std::format!(
      " a{i}: isHouseTrained(atOtherHomes: $v{last})"
    ));
  }
  source.push_str(" } }");
  source
}

/// The control: the same `n` declarations, one usage. Linear in the document, by construction.
fn variable_control(n: usize) -> String {
  let mut source = String::new();
  source.push_str("query q(");
  for i in 0..n {
    source.push_str(&std::format!("$v{i}: Boolean, "));
  }
  source.pop();
  source.pop();
  let last = n - 1;
  source.push_str(&std::format!(
    ") {{ dog {{ a: isHouseTrained(atOtherHomes: $v{last}) }} }}"
  ));
  source
}

/// `ops` operations, each spreading one fragment of `fields` ordinary fields.
///
/// `O + S` of syntax, `O · S` of walk: the fragment is re-entered, re-resolved and re-walked once
/// per operation, because the variable rules are properties of an operation and the walk that
/// carries them carries everything else with it.
fn fragment_bomb(ops: usize, fields: usize) -> String {
  let mut source = String::new();
  for i in 0..ops {
    source.push_str(&std::format!("query q{i} {{ dog {{ ...F }} }} "));
  }
  source.push_str("fragment F on Dog {");
  for i in 0..fields {
    source.push_str(&std::format!(" f{i}: name"));
  }
  source.push_str(" }");
  source
}

// ---------------------------------------------------------------------------------------------
// 1. the index: a cost claim
// ---------------------------------------------------------------------------------------------

/// Variable-use validation is linear in the operation's size, not quadratic in it.
///
/// Quadrupling `n` quadruples a linear cost and multiplies a quadratic one by sixteen. The
/// threshold is eight — twice the linear expectation and half the quadratic one — so neither a
/// slow machine nor a fast one decides the verdict.
///
/// Measured on this machine, release, with the rule set below. At a46ab95, the commit before the
/// index: 2,000 usages 7.13 ms, 8,000 usages 159.26 ms, **ratio 22.34** — this assertion is the one
/// that fails there. With the index: 0.74 ms and 3.06 ms, ratio 3.93. The 22.34 is above sixteen
/// rather than at it because the scan's inner comparison gets cache-colder as the definition list
/// grows, which is the same reason the byte length of a name is charged and not just its identity.
#[test]
fn a_variable_index_replaces_the_scan_at_every_usage() {
  let schema = build(SCHEMA);
  let budget = Budget::default();
  let rules = without_merge();

  // Best of five. A single sample of a millisecond-scale run is mostly scheduler noise, and a
  // ratio of two noisy samples is twice that; the minimum is the one statistic here that a busy
  // machine can only move in one direction.
  let small = best_of(5, || run(&schema, &variable_bomb(2_000), &budget, rules));
  let large = best_of(5, || run(&schema, &variable_bomb(8_000), &budget, rules));
  let control = best_of(5, || run(&schema, &variable_control(8_000), &budget, rules));

  let ratio = large.ms / small.ms;
  println!(
    "variable bomb: n=2000 {:.3} ms, n=8000 {:.3} ms (ratio {ratio:.2}), control {:.3} ms",
    small.ms, large.ms, control.ms
  );

  // The usages are the term. Neither document is refused: they are honest in size, and what was
  // wrong with them was the work, not the size.
  assert!(!large.refused, "the linear document was refused");
  assert_eq!(large.rules(), [Rule::AllVariablesUsed]);
  assert!(
    ratio < 8.0,
    "quadrupling the operation's size multiplied the cost by {ratio:.2}; a scan per usage would"
  );
  // The ratio above is the real claim; this is the backstop that catches a regression too small
  // to move a ratio. Scaled for an unoptimised build the same way the refusal budget is.
  let ceiling = if cfg!(debug_assertions) {
    1_600.0
  } else {
    100.0
  };
  assert!(
    large.ms < ceiling,
    "8,000 usages against 8,000 definitions took {:.3} ms",
    large.ms
  );
}

// ---------------------------------------------------------------------------------------------
// 2. the index: a claim about diagnostics, which is the one that could go wrong quietly
// ---------------------------------------------------------------------------------------------

/// The index marks exactly what the scan marked, at a duplicated name.
///
/// Three things have to hold at once, and the third is the one an index gets wrong:
///
/// - **every** definition of the name is marked used, so 5.8.4 does not report the copies;
/// - a usage resolves against the **first** definition of the name, so 5.8.5 reads the type the
///   scan read;
/// - and 5.8.1 still reports each copy against that first one.
#[test]
fn the_index_marks_what_the_scan_did() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  // Three declarations of one name, all used by one usage. 5.8.1 twice, and 5.8.4 never: the
  // copies are marked used as well, because the duplication is 5.8.1's mistake and reporting it
  // twice would be reporting one mistake twice.
  let used = run(
    &schema,
    "query q($a: Boolean, $a: Boolean, $a: Boolean) { dog { isHouseTrained(atOtherHomes: $a) } }",
    &budget,
    RuleSet::ALL,
  );
  assert_eq!(used.rules(), [Rule::VariableUniqueness]);
  assert_eq!(used.emitted, 2);
  // Both copies blamed, each against the declaration before it: 5.8.1 walks adjacent pairs of the
  // sorted index, so the second copy's related span is the first copy's and not the original's.
  // That is what the scan produced and it is what the index has to keep producing.
  assert_eq!(
    used.diagnostics,
    [
      (Rule::VariableUniqueness, 21, 8, Some(String::from("a"))),
      (Rule::VariableUniqueness, 34, 21, Some(String::from("a"))),
    ]
  );

  // The same document with the usage removed: now every copy is unused, and all three are
  // reported. A mark that reached only the first would report two here, not three.
  let unused = run(
    &schema,
    "query q($a: Boolean, $a: Boolean, $a: Boolean) { dog { name } }",
    &budget,
    RuleSet::ALL,
  );
  assert_eq!(
    unused.rules(),
    [Rule::VariableUniqueness, Rule::AllVariablesUsed]
      .into_iter()
      .collect::<Vec<_>>()
      .tap_sorted()
  );
  assert_eq!(unused.emitted, 5, "{:?}", unused.diagnostics);

  // Which declaration 5.8.5 reads, in both directions. The usage sits in a `Boolean` position.
  // Resolving against the FIRST declaration is what the scan did, so `Boolean` first is silent and
  // `Int` first is not — and an index that landed on an arbitrary member of the run, or on the
  // last, would swap these two verdicts without changing anything else.
  let boolean_first = run(
    &schema,
    "query q($a: Boolean, $a: Int) { dog { isHouseTrained(atOtherHomes: $a) } }",
    &budget,
    RuleSet::ALL,
  );
  assert_eq!(boolean_first.rules(), [Rule::VariableUniqueness]);

  let int_first = run(
    &schema,
    "query q($a: Int, $a: Boolean) { dog { isHouseTrained(atOtherHomes: $a) } }",
    &budget,
    RuleSet::ALL,
  );
  assert_eq!(
    int_first.rules(),
    {
      let mut rules = std::vec![Rule::VariableUniqueness, Rule::AllVariableUsagesAreAllowed];
      rules.sort_unstable();
      rules
    },
    "{:?}",
    int_first.diagnostics
  );
}

/// A fragment three operations share reports its own mistakes once, and its variable rules per
/// operation — which is the whole reason the walk repeats.
#[test]
fn repeated_spreads_report_once_and_only_once() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  // One misspelled field in a fragment three operations spread: one diagnostic, not three.
  let shared = run(
    &schema,
    "query a { dog { ...F } } query b { dog { ...F } } query c { dog { ...F } } \
     fragment F on Dog { nickname wrong }",
    &budget,
    RuleSet::ALL,
  );
  assert_eq!(shared.rules(), [Rule::FieldSelections]);
  assert_eq!(shared.emitted, 1, "{:?}", shared.diagnostics);

  // The same fragment spread twice inside one operation: still once. The visited bitset is the
  // specification's own transitive inclusion read as the set it is.
  let twice = run(
    &schema,
    "{ dog { ...F nickname } } fragment F on Dog { wrong }",
    &budget,
    RuleSet::ALL,
  );
  assert_eq!(twice.emitted, 1, "{:?}", twice.diagnostics);

  // And the variable rules are *not* deduplicated: the same fragment is invalid under one
  // operation's variables and valid under another's, and both verdicts are reported.
  let per_operation = run(
    &schema,
    "query a($v: Boolean) { dog { ...F } } query b($v: Int) { dog { ...F } } \
     fragment F on Dog { isHouseTrained(atOtherHomes: $v) }",
    &budget,
    RuleSet::ALL,
  );
  assert_eq!(
    per_operation.rules(),
    [Rule::AllVariableUsagesAreAllowed],
    "{:?}",
    per_operation.diagnostics
  );
  assert_eq!(per_operation.emitted, 1, "{:?}", per_operation.diagnostics);
}

/// Sorting a helper without a second `let`.
trait TapSorted {
  fn tap_sorted(self) -> Self;
}

impl TapSorted for Vec<Rule> {
  fn tap_sorted(mut self) -> Self {
    self.sort_unstable();
    self
  }
}

// ---------------------------------------------------------------------------------------------
// 3. the ceiling
// ---------------------------------------------------------------------------------------------
// CEILING-SECTION-BEGIN

/// A shared fragment walked once per operation is refused rather than walked, and the refusal is
/// bounded in wall clock rather than in document size.
///
/// The document doubles between the two halves and the refusal does not get slower, which is the
/// whole property: before the ceiling the same pair measured 48 ms and 189 ms.
#[test]
fn the_ceiling_refuses_a_shared_fragment_walked_once_per_operation() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let source = fragment_bomb(1_600, 1_600);
  let small = run(&schema, &source, &budget, RuleSet::ALL);
  println!(
    "fragment bomb: {} bytes, 1600 x 1600, {:.3} ms",
    source.len(),
    small.ms
  );
  assert!(small.refused, "the verdict does not report the bound");
  assert_eq!(small.rules(), [Rule::ValidationWorkBudget]);
  assert!(
    small.ms < refusal_budget_ms(),
    "refusing took {:.3} ms",
    small.ms
  );

  // Exactly one. A collecting sink does not stop the unwinding, and one refusal per remaining unit
  // of work would bury whatever else the document got wrong.
  assert_eq!(small.emitted, 1, "the refusal was reported more than once");

  let source = fragment_bomb(3_200, 3_200);
  let large = run(&schema, &source, &budget, RuleSet::ALL);
  println!(
    "fragment bomb: {} bytes, 3200 x 3200, {:.3} ms",
    source.len(),
    large.ms
  );
  assert!(large.refused);
  assert_eq!(large.rules(), [Rule::ValidationWorkBudget]);
  assert!(
    large.ms < refusal_budget_ms(),
    "four times the work refused in {:.3} ms",
    large.ms
  );
}

/// Filtering the bound's rule out switches off its **diagnostic**, not its refusal.
///
/// This is the shape that has to be got right: a validator that abandoned a pass and then answered
/// `Ok` would be spelling giving up exactly the way it spells finishing. So the verdict is `Err`,
/// [`Invalid::budget_tripped`] is true, and [`Invalid::emitted`] is zero — a combination that means
/// one thing and only one thing.
#[test]
fn giving_up_is_not_spelled_like_finishing() {
  let schema = build(SCHEMA);
  let budget = Budget::default();
  let rules = RuleSet::ALL.without(Rule::ValidationWorkBudget);

  let source = fragment_bomb(1_600, 1_600);
  let quiet = run(&schema, &source, &budget, rules);
  assert!(
    quiet.refused,
    "the bound stopped the walk and the verdict does not say so"
  );
  assert_eq!(quiet.emitted, 0, "{:?}", quiet.diagnostics);
  assert_eq!(quiet.diagnostics, []);
  assert!(
    quiet.ms < refusal_budget_ms(),
    "refusing took {:.3} ms",
    quiet.ms
  );

  // And the refusal is still a refusal to the type system: `Err`, whatever the count.
  let document = parse(&source);
  let mut scratch = Scratch::new();
  let mut sink = smear::validator::Ignore;
  let invalid =
    validate_executable_with(&schema, &document, &mut scratch, &budget, rules, &mut sink)
      .expect_err("a document the ceiling refused came back Ok");
  assert!(invalid.budget_tripped());
  assert_eq!(invalid.emitted(), 0);
  assert_eq!(
    invalid.to_string(),
    "resource budget exceeded before the document was fully examined"
  );
}

/// The documents this crate ships as fixtures are orders below the ceiling, and so are the shapes
/// a fragment-reusing client actually sends.
///
/// A bound nothing honest reaches is the only kind worth having, and "nothing honest" is a claim
/// about a population rather than about one document — so this walks the four executable fixtures
/// the repository already keeps plus a fragment-reusing shape at a realistic scale.
#[test]
fn an_honest_document_is_nowhere_near_the_ceiling() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  // The repository's own corpus, against its own schema. These parse against a different schema
  // than `SCHEMA`, so field rules fire; what is under test is that the *ceiling* does not.
  for (label, source) in [
    (
      "bench_01_tiny",
      include_str!("fixtures/executables/bench_01_tiny_simple.graphql"),
    ),
    (
      "bench_05_fragments",
      include_str!("fixtures/executables/bench_05_medium_fragments.graphql"),
    ),
    (
      "bench_10_huge",
      include_str!("fixtures/executables/bench_10_huge_comprehensive.graphql"),
    ),
    (
      "kitchen_sink",
      include_str!("fixtures/executables/kitchen-sink_canonical.graphql"),
    ),
  ] {
    let outcome = run(&schema, source, &budget, RuleSet::ALL);
    assert!(
      !outcome.rules().contains(&Rule::ValidationWorkBudget),
      "{label} ({} bytes) reached the validation ceiling",
      source.len()
    );
  }

  // Fifty operations sharing one two-hundred-field fragment: the shape the ceiling is about, at a
  // size a real client sends. It spends about half a percent of the ceiling.
  let honest = run(&schema, &fragment_bomb(50, 200), &budget, RuleSet::ALL);
  assert!(
    !honest.rules().contains(&Rule::ValidationWorkBudget),
    "{:?}",
    honest.rules()
  );
}
// CEILING-SECTION-END
