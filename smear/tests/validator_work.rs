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

use std::{string::String, thread, time::Instant, vec::Vec};

use smear::{
  lexer::{
    limits::SyntacticLimits,
    tokora::{Parse as _, Parser},
  },
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
  withOpts(opts: Opts): Boolean
  withLoose(opts: Loose): Boolean
  withJson(payload: Json): Boolean
  withNeedy(opts: Needy): Boolean
}

scalar Json

type Subscription { newMessage: Message }
type Message { body: String }

input Opts { need: Int! a: Int }
input Loose { a: Int b: Int }
input Needy { need: Int! a: Int }

directive @onField repeatable on FIELD
directive @onFragDef(if: Boolean) on FRAGMENT_DEFINITION
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

/// Parses past the lexer's default nesting ceiling of 24.
///
/// The ceiling is the **caller's**, not a fact about documents: `SyntacticLimits` is public and so
/// are the AST constructors, so a document with any nesting at all can reach
/// [`validate_executable`]. Nothing the validator does may assume otherwise, which is why the
/// coordinate descent is charged rather than bounded by a number somebody else chose.
fn parse_deep(source: &str) -> ExecutableDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str_with_state(source, SyntacticLimits::with_max_nesting_depth(1_024))
  .expect("the deep query parses")
}

/// Runs `body` on a thread with room for the parser's own recursion.
///
/// The parser is a recursive descent and drops its tree recursively; neither is what these tests
/// are about. `validator_merge.rs` carries the same runner for the same reason.
fn on_a_deep_stack<T: Send + 'static>(body: impl FnOnce() -> T + Send + 'static) -> T {
  thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(body)
    .expect("the thread spawns")
    .join()
    .expect("the body does not panic")
}

/// The smallest [`Budget::validation_work`] at which a pre-parsed document is not refused.
///
/// [`min_budget`]'s twin for a document the shared parse door cannot produce.
fn min_budget_of<S>(schema: &Schema, document: &ExecutableDocument<S>, rules: RuleSet) -> u32
where
  S: AsRef<[u8]> + Clone,
{
  let refused = |work: u32| {
    let budget = Budget::default().with_validation_work(work);
    let mut scratch = Scratch::new();
    let mut sink = smear::validator::Ignore;
    validate_executable_with(schema, document, &mut scratch, &budget, rules, &mut sink)
      .err()
      .is_some_and(|invalid| invalid.budget_tripped())
  };
  assert!(!refused(u32::MAX), "refused at every budget");
  let (mut lo, mut hi) = (0u32, u32::MAX);
  while lo < hi {
    let mid = lo + (hi - lo) / 2;
    if refused(mid) {
      lo = mid + 1;
    } else {
      hi = mid;
    }
  }
  lo
}

/// The smallest [`Budget::validation_work`] at which `source` is not refused.
///
/// The instrument for the question the first round of al8n/smear#198 got wrong. A charge that sits
/// *behind* the work it prices still charges, so "does this pass charge?" cannot see it; a step the
/// ledger never charges at all raises this number by nothing, and a step charged in front of its
/// work raises it by exactly what that step costs. Differences in this number are therefore
/// statements about charges, not about clocks.
///
/// A binary search over the knob — thirty-two validations of a document these tests keep small.
/// Monotone by construction: more budget cannot cause a refusal.
fn min_budget(schema: &Schema, source: &str, rules: RuleSet) -> u32 {
  let refused = |work: u32| {
    let budget = Budget::default().with_validation_work(work);
    run(schema, source, &budget, rules).refused
  };
  assert!(
    !refused(u32::MAX),
    "the document is refused at every budget, so there is no minimum to find"
  );
  let (mut lo, mut hi) = (0u32, u32::MAX);
  while lo < hi {
    let mid = lo + (hi - lo) / 2;
    if refused(mid) {
      lo = mid + 1;
    } else {
      hi = mid;
    }
  }
  lo
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

  // Best of five, **interleaved**. A single sample of a millisecond-scale run is mostly scheduler
  // noise and a ratio of two noisy samples is twice that, so the minimum is the one statistic a
  // busy machine can only move in one direction — but measuring all five smalls and then all five
  // larges lets a burst of load land on one side of the ratio and not the other. This suite runs
  // eighty-five test binaries in parallel, and that is exactly what happened: 4.2 in isolation,
  // 8.7 under the full run, against a threshold of 8. Alternating puts any burst on both sides.
  let (mut small_ms, mut large_ms, mut control_ms) = (f64::MAX, f64::MAX, f64::MAX);
  let mut large = run(&schema, &variable_bomb(8_000), &budget, rules);
  for _ in 0..5 {
    small_ms = small_ms.min(run(&schema, &variable_bomb(2_000), &budget, rules).ms);
    let round = run(&schema, &variable_bomb(8_000), &budget, rules);
    large_ms = large_ms.min(round.ms);
    large = round;
    control_ms = control_ms.min(run(&schema, &variable_control(8_000), &budget, rules).ms);
  }
  let ratio = large_ms / small_ms;
  println!(
    "variable bomb: n=2000 {small_ms:.3} ms, n=8000 {large_ms:.3} ms (ratio {ratio:.2}), \
     control {control_ms:.3} ms"
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
    large_ms < ceiling,
    "8,000 usages against 8,000 definitions took {large_ms:.3} ms"
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

// ---------------------------------------------------------------------------------------------
// 4. the knob, and the contract that could not survive without one
// ---------------------------------------------------------------------------------------------

/// The document the default refuses is a **valid** document, and a caller can validate it.
///
/// This is the half the first round of al8n/smear#198 got wrong. An absolute ceiling with no public
/// surface is not a conservative choice: 1,600 operations sharing a 1,600-selection fragment is
/// valid GraphQL that fits in 63 KB, and with no knob there was no argument to
/// [`validate_executable`] that would accept it. A bound a caller cannot raise is a document a
/// caller cannot send.
#[test]
fn the_ceiling_is_a_knob_a_caller_can_raise() {
  let schema = build(SCHEMA);
  let source = fragment_bomb(1_600, 1_600);

  let refused = run(&schema, &source, &Budget::default(), RuleSet::ALL);
  assert!(refused.refused);
  assert_eq!(refused.rules(), [Rule::ValidationWorkBudget]);

  // Raised, and the same bytes are valid: no diagnostic, no refusal. The merge rules are out
  // because draft 5.3.2 has its own knob and this test is about this one.
  let raised = Budget::default().with_validation_work(u32::MAX);
  let accepted = run(&schema, &source, &raised, without_merge());
  assert!(!accepted.refused, "{:?}", accepted.rules());
  assert_eq!(accepted.diagnostics, [], "the document is valid");
}

/// `RuleSet::EMPTY` evaluates no rule — and that was never the same as "always `Ok`".
///
/// A resource bound is a rule *and* a bound, and an empty set reaches only the rule. What makes a
/// validator that cannot refuse is the **budget**, and this pins both halves of that sentence so
/// the documentation cannot drift back.
#[test]
fn an_empty_rule_set_is_not_a_promise_of_ok() {
  let schema = build(SCHEMA);
  let source = fragment_bomb(1_600, 1_600);

  // No rule is evaluated, nothing is emitted — and the document is still refused, because the
  // bound is not a rule the set can remove.
  let quiet = run(&schema, &source, &Budget::default(), RuleSet::EMPTY);
  assert!(quiet.refused);
  assert_eq!(quiet.emitted, 0);
  assert_eq!(quiet.diagnostics, []);

  // The supported spelling of "never refuse for this resource" is the knob, on all three.
  let unbounded = Budget::new(u32::MAX, u32::MAX).with_validation_work(u32::MAX);
  let ok = run(&schema, &source, &unbounded, RuleSet::EMPTY);
  assert!(
    !ok.refused,
    "an empty rule set with every bound off still refused"
  );
  assert_eq!(ok.emitted, 0);
}

// ---------------------------------------------------------------------------------------------
// 5. prepayment: the charge in front of the work, not merely present
// ---------------------------------------------------------------------------------------------

/// A spread pays for the **type condition** it resolves, not only for its own name.
///
/// `check_fragment_spread` charges the spread's spelling and then resolves the fragment's type
/// condition through `Schema::sym`, which hashes every byte of a second, different spelling. `O`
/// spreads of one fragment therefore read `O · L` bytes off `O + L` of syntax, and that read was
/// charged nothing.
///
/// Asserted as a **difference** between two documents rather than against an absolute, so the
/// number does not depend on what the rest of validation happens to cost.
#[test]
fn a_spread_pays_for_the_condition_it_resolves() {
  // A type whose name is two kilobytes: the condition every spread resolves.
  let long = "D".repeat(2_000);
  let sdl = std::format!("type Query {{ d: {long} }} type {long} {{ name: String }}");
  let schema = build(&sdl);

  let document = |spreads: usize| {
    let mut source = String::from("{ d {");
    for _ in 0..spreads {
      source.push_str(" ...F");
    }
    source.push_str(&std::format!(" }} }} fragment F on {long} {{ name }}"));
    source
  };

  let rules = without_merge();
  let one = min_budget(&schema, &document(1), rules);
  let many = min_budget(&schema, &document(201), rules);

  // 2,000 bytes is 251 units, and 200 more spreads must cost 200 of them. Only the condition
  // charge can produce this difference: the spread's own name is two bytes.
  let floor = 200 * (2_000 / 8 + 1);
  println!("spread conditions: 1 spread {one} units, 201 spreads {many} units (floor {floor})");
  assert!(
    many - one >= floor,
    "200 extra spreads of a 2,000-byte condition cost {} units; the condition is not charged",
    many - one
  );
}

/// A subscription's root collection pays for the directives it scans.
///
/// Draft 5.2.4.1 walks the root selections again, on its own, and asks each one whether it carries
/// `@skip` or `@include` — which scans **every** directive on it. The pass charged one flat unit
/// per selection.
///
/// The comparison is a subscription against the same selection set as a query: both pay the
/// selection walk, and only the subscription pays the extra scan.
#[test]
fn a_subscription_pays_for_the_directives_its_root_collection_scans() {
  let schema = build(SCHEMA);
  let directives = "@onField ".repeat(4_000);
  let rules = without_merge();

  // On the ROOT selection: draft 5.2.4.1's collection visits the root selections, and it is each
  // of those it asks about `@skip`/`@include`.
  let as_query = std::format!("query q {{ dog {directives} {{ name }} }}");
  let as_subscription = std::format!("subscription s {{ newMessage {directives} {{ body }} }}");

  let query = min_budget(&schema, &as_query, rules);
  let subscription = min_budget(&schema, &as_subscription, rules);
  println!("root collection: query {query} units, subscription {subscription} units");

  // The subscription pass reads all 4,000 again. Half of them is a floor generous enough that the
  // two documents' unrelated differences cannot account for it.
  assert!(
    subscription >= query + 2_000,
    "the subscription paid {subscription} against the query's {query}; the root collection's \
     directive scan is not charged"
  );
}

/// The value walk pays for the descent `resolve` makes, which is the frame depth.
///
/// `resolve` rebuilds the path from the root on **every** iteration of the loop, including the ones
/// that only pop, so a literal nested `D` deep does `O(D²)` of coordinate walking. One unit per
/// literal priced `O(D)` of it.
///
/// Two choices here are the point rather than convenience. The nesting goes in an **undeclared**
/// argument, because an unknown position is the one the walk descends with no type to stop it —
/// exactly the position an adversary picks. And it is parsed past the lexer's default nesting
/// ceiling, because that ceiling belongs to the caller: `SyntacticLimits` is public, the AST
/// constructors are public, and a validator that leaned on either would be bounded by a number
/// somebody else chose.
#[test]
fn a_nested_literal_pays_for_the_coordinates_it_rebuilds() {
  on_a_deep_stack(|| {
    let schema = build(SCHEMA);
    let rules = without_merge();

    let document = |depth: usize| {
      let mut source = String::from("{ dog { isHouseTrained(bogus: ");
      source.push_str(&"[".repeat(depth));
      source.push_str(&"]".repeat(depth));
      source.push_str(") } }");
      source
    };

    let shallow_source = document(1);
    let deep_source = document(200);
    let shallow = min_budget_of(&schema, &parse_deep(&shallow_source), rules);
    let deep = min_budget_of(&schema, &parse_deep(&deep_source), rules);
    println!("value coordinates: depth 1 {shallow} units, depth 200 {deep} units");

    // The descent is triangular in the depth: about `D²/2` over the pushes and as much again over
    // the pops. A quarter of one triangle is a floor no per-literal charge can reach — 200 levels
    // is 200 literals, and this asks for ten thousand units.
    let floor = 200 * 200 / 4;
    assert!(
      deep - shallow >= floor,
      "200 levels of nesting cost {} units; `resolve` walks the whole path and is not charged \
       for it",
      deep - shallow
    );
  });
}

/// A uniqueness sort over attacker-chosen names cannot run before a refusal is possible.
///
/// This one is not about the *amount*: 5.4.2, 5.6.3 and 5.7.3 charge the same total wherever the
/// charge sits. It is about the **position**. The charge used to be per-argument inside the loop
/// that follows the sort, so `O(N log N)` comparisons over bytes the client chose all happened
/// before the ledger could refuse anything — which is a clock, so this is measured with one.
#[test]
fn a_uniqueness_sort_cannot_outrun_the_ledger() {
  let schema = build(SCHEMA);
  // 60,000 distinct argument names on one field. Every one of them is a name the sort compares.
  let mut source = String::from("{ dog { isHouseTrained(");
  for i in 0..60_000 {
    source.push_str(&std::format!("argument{i}: true "));
  }
  source.push_str(") } }");

  // Sixteen units: enough to reach `check_arguments` — the selection and the field name cost about
  // four — and nothing like enough for sixty thousand argument names.
  let budget = Budget::default().with_validation_work(16);
  let outcome = best_of(3, || run(&schema, &source, &budget, RuleSet::ALL));
  println!(
    "uniqueness sort: {} bytes refused in {:.3} ms",
    source.len(),
    outcome.ms
  );
  assert!(outcome.refused);
  // Measured, this machine, release: 0.003 ms with the charge in front of the sort, and 4.9 ms
  // with it behind — the same total, spent after `O(N log N)` comparisons instead of before the
  // first one. One millisecond separates them by two orders of magnitude in one direction and
  // five times in the other.
  let ceiling = if cfg!(debug_assertions) { 8.0 } else { 1.0 };
  assert!(
    outcome.ms < ceiling,
    "refusing at a budget of sixteen units took {:.3} ms, which is a sort that ran first",
    outcome.ms
  );
}

// ---------------------------------------------------------------------------------------------
// 6. the pass that runs before the validator exists
// ---------------------------------------------------------------------------------------------

/// The lossless door's projection is charged, and charged before it runs.
///
/// It builds the **entire AST** out of the CST before any rule exists to refuse anything, so a
/// ledger that opened inside the validator opened too late. "Bounded by the document" was the
/// answer al8n/smear#198's first pass table gave and it was the wrong one: the parser's limits
/// bound the CST's *shape* — nesting — and leave its *size* alone, and the size is what an
/// adversary picks.
///
/// So the ledger opens in `validate_executable_lossless_with`, the projection spends from it
/// first, and the validator continues on what is left.
#[cfg(feature = "rowan")]
#[test]
fn the_projection_is_charged_before_it_runs() {
  use smear::{
    parser::graphql::lossless::parse_executable_document,
    validator::validate_executable_lossless_with,
  };

  let schema = build(SCHEMA);
  // Broad rather than deep: the nesting ceiling is the one limit the lossless parser does apply,
  // and it is not the one this is about.
  let source = fragment_bomb(4_000, 4_000);
  let parse = parse_executable_document(&source);

  let refuse_at = |work: u32, rules: RuleSet| {
    let budget = Budget::default().with_validation_work(work);
    let mut scratch = Scratch::new();
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    let start = Instant::now();
    let verdict = validate_executable_lossless_with(
      &schema,
      &parse,
      &source,
      &mut scratch,
      &budget,
      rules,
      &mut sink,
    );
    let ms = start.elapsed().as_secs_f64() * 1e3;
    (verdict, collected, ms)
  };

  // Sixteen units against a document whose projection alone is priced in the tens of thousands.
  let (verdict, collected, ms) = refuse_at(16, RuleSet::ALL);
  let invalid = verdict.expect_err("a projection the budget cannot pay for came back Ok");
  println!("projection: {} bytes refused in {ms:.3} ms", source.len());
  assert!(invalid.invalid().budget_tripped());
  assert_eq!(invalid.invalid().emitted(), 1);
  assert_eq!(
    collected.iter().map(Diagnostic::rule).collect::<Vec<_>>(),
    [Rule::ValidationWorkBudget]
  );
  // Nothing was projected, so there is no recovery — not a recovery that reads as a clean parse,
  // and not one carrying a number nobody counted.
  assert_eq!(invalid.recovery(), None);
  // And it did not project first: the whole point is that this is not a walk of the CST.
  let ceiling = if cfg!(debug_assertions) { 20.0 } else { 2.0 };
  assert!(
    ms < ceiling,
    "refusing before the projection took {ms:.3} ms, which is a projection that ran"
  );

  // Giving up is not spelled like finishing, on this path either: with the rule filtered out there
  // is nothing to emit and the verdict is still `Err` with the flag set.
  let (verdict, collected, _) = refuse_at(16, RuleSet::ALL.without(Rule::ValidationWorkBudget));
  let invalid = verdict.expect_err("a refused projection came back Ok with the rule off");
  assert!(invalid.invalid().budget_tripped());
  assert_eq!(invalid.invalid().emitted(), 0);
  assert!(collected.is_empty());
  assert_eq!(invalid.recovery(), None);

  // Raised, and the same parse projects and validates: the charge is a bound, not a wall.
  let budget = Budget::default().with_validation_work(u32::MAX);
  let mut scratch = Scratch::new();
  let mut sink = smear::validator::Ignore;
  let recovery = validate_executable_lossless_with(
    &schema,
    &parse,
    &source,
    &mut scratch,
    &budget,
    without_merge(),
    &mut sink,
  )
  .expect("the document is valid once the budget admits it");
  assert!(recovery.is_complete());
  let _ = &recovery;
}

// ---------------------------------------------------------------------------------------------
// 7. the siblings: the same reasoning, one site over
// ---------------------------------------------------------------------------------------------

/// The **selection** coordinate resolver pays for its descent, exactly as the value one does.
///
/// `selections::resolve` scans the frame stack for the definition root and then descends it again,
/// and `walk_selections`, `collect_definition_edges` and the subscription collection all call it
/// after **every** pop. One unit per selection priced `O(1)` of a walk that costs the depth, so a
/// definition nested `D` deep did `Θ(D²)` frame and selection lookups on `Θ(D)` of units.
///
/// Round one of al8n/smear#198 named this resolver in the same sentence as the value one and only
/// the value one was repaired. It is the reason this branch now sweeps siblings rather than sites.
#[test]
fn the_selection_resolver_pays_for_its_descent() {
  on_a_deep_stack(|| {
    let schema = build(SCHEMA);
    let rules = without_merge();

    // `Nest` is self-referential, so the only thing that changes between the two is the depth.
    let document = |depth: usize| {
      let mut source = String::from("{");
      for _ in 0..depth {
        source.push_str(" nest {");
      }
      source.push_str(" leaf ");
      for _ in 0..depth {
        source.push_str("} ");
      }
      source.push('}');
      source
    };

    let shallow = min_budget_of(&schema, &parse_deep(&document(1)), rules);
    let deep = min_budget_of(&schema, &parse_deep(&document(200)), rules);
    println!("selection coordinates: depth 1 {shallow} units, depth 200 {deep} units");

    // Triangular in the depth over the pushes and again over the pops. A quarter of one triangle
    // is a floor a per-selection charge cannot reach: 200 levels is 200 selections.
    let floor = 200 * 200 / 4;
    assert!(
      deep - shallow >= floor,
      "200 levels of selection nesting cost {} units; `resolve` walks the whole path and is not \
       charged for it",
      deep - shallow
    );
  });
}

/// `RuleSet::EMPTY` spends a bounded handful of units on a document built to be expensive.
///
/// The class-level plant, not a site-level one. Every pass that reads caller-sized data before
/// asking whether its rule is enabled is the same defect, and the contract this branch rewrote —
/// *a rule that is off is not evaluated* — is exactly the claim that no such pass exists. Since the
/// passes are now charged, a violation is also a **refusal a caller did not ask for**, which is
/// what makes this measurable rather than a matter of taste.
///
/// The document carries one of each: a megabyte-long integer literal, a long enum spelling, a deep
/// value literal, a wide argument list, a fragment nobody spreads, and a variable list to sort.
#[test]
fn an_empty_rule_set_pays_for_nothing() {
  let schema = build(SCHEMA);
  let digits = "9".repeat(1_000_000);
  let arguments = (0..2_000)
    .map(|i| std::format!("a{i}: 1 "))
    .collect::<String>();
  let variables = (0..2_000)
    .map(|i| std::format!("$v{i}: Boolean, "))
    .collect::<String>();
  let source = std::format!(
    "query q({variables}$last: Boolean) {{ dog {{ ...F counted(times: {digits}) \
     isHouseTrained({arguments}) }} }} \
     fragment F on Dog {{ name }} fragment Unused on Dog {{ nickname }}"
  );

  let spent = min_budget(&schema, &source, RuleSet::EMPTY);
  println!("empty rule set: {} bytes, {spent} units", source.len());

  // What an empty set may still pay for is the structure it cannot know is unwanted: the prep
  // sweep and the selection walk, both linear in the document. What it may not pay for is a
  // literal parsed, an enum hashed, a name list sorted or a value descended on behalf of a rule
  // that is off — and the megabyte of digits alone is 125,001 units of exactly that.
  assert!(
    spent < 100_000,
    "an empty rule set spent {spent} units; something is working for a rule that is off"
  );

  // And it is still `Ok`, which is the other half of the contract.
  let budget = Budget::default();
  let outcome = run(&schema, &source, &budget, RuleSet::EMPTY);
  assert!(!outcome.refused);
  assert_eq!(outcome.emitted, 0);
}

/// A rule that is off does not read the literal, at the one site the review named.
///
/// Narrower than the sweep above and pinned separately because it is the site: draft 5.6.1's two
/// leaf arms ran `has_enum_value` and `scalar_accepts` **before** asking whether 5.6.1 was on, so a
/// consumer that wanted only the fragment rules hashed enum spellings and parsed arbitrarily long
/// digit strings — and, once the read was charged, could be refused for it.
#[test]
fn a_disabled_coercion_rule_does_not_read_the_literal() {
  let schema = build(SCHEMA);
  let digits = "9".repeat(1_000_000);
  let source = std::format!("{{ dog {{ counted(times: {digits}) }} }}");

  let without = min_budget(
    &schema,
    &source,
    without_merge().without(Rule::ValuesOfCorrectType),
  );
  let with = min_budget(&schema, &source, without_merge());
  println!("literal: 5.6.1 off {without} units, 5.6.1 on {with} units");

  // A megabyte of digits is 125,001 units. With the rule on they are read and charged; with it off
  // neither happens, and the difference is the whole literal.
  assert!(
    with - without >= 125_000,
    "the literal cost {} units more with 5.6.1 on; with it off it is still being read",
    with - without
  );
  assert!(
    without < 1_000,
    "5.6.1 off still spent {without} units on a one-megabyte literal"
  );
}

/// The projection prepayment is priced over **both** inputs, because they are two parameters.
///
/// `parse` and `source` are separate arguments and nothing pairs them. Pricing from `source.len()`
/// alone meant a tree of `N` top-level definitions handed in beside an empty source paid **one
/// unit** — and the recovering projector then visited all `N` CST children, rejected each on a
/// source mismatch, and returned `Ok`. The bound was priced from one input and spent on the other.
#[cfg(feature = "rowan")]
#[test]
fn the_projection_is_priced_over_both_inputs() {
  use smear::{
    parser::graphql::lossless::parse_executable_document,
    validator::validate_executable_lossless_with,
  };

  let schema = build(SCHEMA);
  let parsed = fragment_bomb(4_000, 4_000);
  let parse = parse_executable_document(&parsed);

  // The mismatch: a large parse, an empty source. `units(0)` is one.
  let budget = Budget::default().with_validation_work(16);
  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let verdict = validate_executable_lossless_with(
    &schema,
    &parse,
    "",
    &mut scratch,
    &budget,
    RuleSet::ALL,
    &mut sink,
  );

  let invalid = verdict.expect_err("a 160 KB parse beside an empty source came back Ok");
  assert!(invalid.invalid().budget_tripped());
  assert_eq!(invalid.recovery(), None);
  assert_eq!(
    collected.iter().map(Diagnostic::rule).collect::<Vec<_>>(),
    [Rule::ValidationWorkBudget]
  );
}

/// A refused projection reports a count of what it did not look at, and the sink's answer.
///
/// Two disclosures that were wrong rather than approximate. `Recovery::skipped` is documented as
/// evidence that something was dropped **and a bound on how much**, so reporting `1` for a parse
/// with hundreds of top-level elements is not a floor, it is an under-count wearing one. And the
/// sink's `ControlFlow::Break` was discarded while the verdict hardcoded `stopped: false`, so a
/// `First` sink could be told to stop by a verdict that then said it had not been.
#[cfg(feature = "rowan")]
#[test]
fn a_refused_projection_reports_what_it_did_not_look_at() {
  use smear::{
    parser::graphql::lossless::parse_executable_document,
    validator::{First, validate_executable_lossless_with},
  };

  let schema = build(SCHEMA);
  // Five hundred top-level definitions, so a `skipped` of `1` is off by more than a rounding.
  let source = fragment_bomb(500, 4);
  let parse = parse_executable_document(&source);
  let budget = Budget::default().with_validation_work(16);

  let mut scratch = Scratch::new();
  let mut sink = First::new();
  let invalid = validate_executable_lossless_with(
    &schema,
    &parse,
    &source,
    &mut scratch,
    &budget,
    RuleSet::ALL,
    &mut sink,
  )
  .expect_err("the projection was not paid for and the door said Ok");

  // The count does not exist, which is the only representation that cannot be misread. It was `1`
  // with a prose disclosure, then `1` with a flag beside it saying how to read the `1`; both still
  // constructed a number, and for an empty or trivia-only parse the true count is zero, so the `1`
  // was not even the floor it claimed to be.
  assert_eq!(
    invalid.recovery(),
    None,
    "a refusal taken before the projection invented a recovery"
  );
  // And it renders as the state rather than as a count.
  assert!(
    invalid.to_string().ends_with("(nothing was projected)"),
    "{invalid}"
  );

  // And the ordinary refusal — inside the walk, after a projection that did run — says so too, so
  // the flag separates the two rather than being always false on an error.
  let generous = Budget::default().with_validation_work(4_000);
  let mut scratch = Scratch::new();
  let mut quiet = smear::validator::Ignore;
  let inside = validate_executable_lossless_with(
    &schema,
    &parse,
    &source,
    &mut scratch,
    &generous,
    RuleSet::ALL,
    &mut quiet,
  )
  .expect_err("a budget of four thousand does not validate this document");
  assert!(
    inside.recovery().is_some(),
    "a refusal taken inside the walk reports no recovery"
  );
  assert!(inside.invalid().budget_tripped());

  // `First` keeps one diagnostic and breaks. The verdict has to say so.
  assert_eq!(invalid.invalid().emitted(), 1);
  assert!(
    invalid.invalid().stopped(),
    "the sink asked to stop and the verdict reports that it did not"
  );
  assert!(sink.get().is_some());
}

// ---------------------------------------------------------------------------------------------
// 8. states that cannot be half-read
// ---------------------------------------------------------------------------------------------

/// The smallest `merge_work` at which `source` is not refused.
///
/// [`min_budget`]'s twin over the other knob, and the instrument for a producer that failed to run:
/// a consumer reading a bitset nobody filled does more work, and "more work" is exactly what this
/// number measures.
fn min_merge_work(schema: &Schema, source: &str, rules: RuleSet) -> u32 {
  // Keyed on the rule firing rather than on `Invalid::budget_tripped`, deliberately: the verdict
  // flag is the subject of another finding in this same round, and an instrument that depended on
  // it would stop measuring the moment that one regressed.
  let refused = |work: u32| {
    let budget = Budget::default().with_merge_work(work);
    run(schema, source, &budget, rules)
      .rules()
      .contains(&Rule::MergeWorkBudget)
  };
  assert!(!refused(u32::MAX - 1), "refused at every merge budget");
  let (mut lo, mut hi) = (0u32, u32::MAX - 1);
  while lo < hi {
    let mid = lo + (hi - lo) / 2;
    if refused(mid) {
      lo = mid + 1;
    } else {
      hi = mid;
    }
  }
  lo
}

/// A merge budget that stops the engine refuses the document, whether or not it had a rule to emit.
///
/// The seam between two branches' repairs. `probe/interner-charge` unified the merge engine's
/// "stopped" and "reported" flags onto one field; this branch added a third for its own ledger; and
/// the verdict tail — which both branches write — then read one of the two. With draft 5.3.2
/// enabled and its two budget rules filtered out, the engine stopped, emitted nothing, and the
/// document came back `Ok` on a merge it never finished.
#[test]
fn a_merge_bound_refuses_even_with_both_of_its_rules_off() {
  let schema = build(SCHEMA);
  // Enough same-named selections to exhaust the default merge budget several times over.
  let mut source = String::from("{ ");
  for _ in 0..20_000 {
    source.push_str("a: nest { leaf } ");
  }
  source.push('}');

  let quiet = RuleSet::ALL
    .without(Rule::MergeWorkBudget)
    .without(Rule::MergeDepthBudget);
  let outcome = run(&schema, &source, &Budget::default(), quiet);
  println!(
    "merge bound, both rules off: {} bytes, refused={} emitted={}",
    source.len(),
    outcome.refused,
    outcome.emitted
  );
  assert!(
    outcome.refused,
    "the merge engine stopped and the verdict says the document passed"
  );
  assert_eq!(outcome.emitted, 0, "{:?}", outcome.diagnostics);
  assert_eq!(outcome.diagnostics, []);

  // And with the rule on, the same document is refused *and* told about — so the flag is not just
  // always true on an error.
  let loud = run(&schema, &source, &Budget::default(), RuleSet::ALL);
  assert!(loud.refused);
  assert_eq!(loud.rules(), [Rule::MergeWorkBudget]);
}

/// Every rule that starts draft 5.3.2's engine also starts the pass that fills what it reads.
///
/// `check_fragments_used` produces `Scratch::reachable` and the engine reads it to skip the
/// fragments an operation's own merge already covered. The producer's guard named two of the
/// engine's three activating rules, so under `RuleSet::only(MergeWorkBudget)` the engine read a
/// cleared bitset: every fragment looked unreached, a chain already expanded from the operation was
/// merged again from every suffix, and the extra work showed up as a **false budget refusal**.
///
/// Measured as the merge budget the document needs, which is the quantity that was inflated.
#[test]
fn every_rule_that_starts_the_merge_engine_starts_its_producer() {
  let schema = build(SCHEMA);
  // A chain: the operation reaches f0, which reaches f1, and so on. Every one of them is reachable,
  // so a correct `reachable` bitset makes the engine merge the chain once.
  const LINKS: usize = 40;
  let mut source = String::from("{ dog { ...f0 } } ");
  for i in 0..LINKS {
    source.push_str(&std::format!("fragment f{i} on Dog {{ ...f{} }} ", i + 1));
  }
  source.push_str(&std::format!("fragment f{LINKS} on Dog {{ name }}"));

  let alone = min_merge_work(&schema, &source, RuleSet::only(Rule::MergeWorkBudget));
  let with_rule = min_merge_work(
    &schema,
    &source,
    RuleSet::only(Rule::MergeWorkBudget).with(Rule::FieldSelectionMerging),
  );
  println!("merge activation: only(MergeWorkBudget) {alone}, with 5.3.2 {with_rule}");

  // The reachability is a fact about the document, not about which rule asked for it, so the two
  // rule sets must need the same budget. Without the shared predicate the first is `LINKS` times
  // the second, because every suffix of the chain gets merged again from its own root.
  assert_eq!(
    alone, with_rule,
    "a rule set that starts the engine without 5.3.2 needs a different merge budget, which is the \
     engine reading a bitset nobody filled"
  );
}

/// A definition-local rule does not pay per operation for a fragment it checks once.
///
/// `walks_values` answered two different questions with one boolean. Draft 5.6's rules are a
/// property of a **definition** and fire under `Frame::CHECK`, which a definition carries exactly
/// once; the variable rules are a property of an **operation**. With only 5.6.1 enabled, the second
/// and later operations to reach a shared fragment still descended and charged its literals to
/// produce nothing — `O(operations × literal size)` off `O(operations + literal size)` of input.
#[test]
fn a_definition_local_rule_does_not_pay_per_operation() {
  let schema = build(SCHEMA);
  // The size that matters is the literal's **node count**, not its byte length: what a repeat visit
  // redoes is the descent, and a scalar however long is one node. A wide list in an *undeclared*
  // argument is the shape with no type to stop the walk, which is the one an adversary writes.
  let entries = "1 ".repeat(5_000);
  let document = |operations: usize| {
    let mut source = String::new();
    for i in 0..operations {
      source.push_str(&std::format!("query q{i} {{ dog {{ ...F }} }} "));
    }
    source.push_str(&std::format!(
      "fragment F on Dog {{ counted(bogus: [{entries}]) }}"
    ));
    source
  };

  let rules = RuleSet::only(Rule::ValuesOfCorrectType);
  let one = min_budget(&schema, &document(1), rules);
  let many = min_budget(&schema, &document(200), rules);
  println!("definition-local: 1 operation {one} units, 200 operations {many} units");

  // Five thousand entries is about ten thousand units of descent, spent once when the definition is
  // checked. The other 199 operations add their own selection walks — a handful of units each —
  // and must not add that descent again.
  assert!(
    many - one < 10_000,
    "199 more operations over one already-checked fragment cost {} units",
    many - one
  );
}

/// An empty parse the budget cannot pay for reports no recovery at all.
///
/// The reviewer's own counterexample to the `1`: a parse with nothing in it has a true skipped
/// count of **zero**, so `1` was not the floor it was documented as. The repair is that there is no
/// number — the state carries no [`Recovery`], and the rendering branches on the state.
#[cfg(feature = "rowan")]
#[test]
fn a_refused_projection_of_an_empty_parse_reports_no_recovery() {
  use smear::{
    parser::graphql::lossless::parse_executable_document,
    validator::validate_executable_lossless_with,
  };

  let schema = build(SCHEMA);
  for source in ["", "   \n# just a comment\n"] {
    let parse = parse_executable_document(source);
    let budget = Budget::default().with_validation_work(0);
    let mut scratch = Scratch::new();
    let mut sink = smear::validator::Ignore;
    let invalid = validate_executable_lossless_with(
      &schema,
      &parse,
      source,
      &mut scratch,
      &budget,
      RuleSet::ALL,
      &mut sink,
    )
    .expect_err("a zero budget cannot pay for a projection");
    assert_eq!(
      invalid.recovery(),
      None,
      "an empty parse was given a recovery it did not earn"
    );
    assert!(invalid.to_string().ends_with("(nothing was projected)"));
  }
}

// ---------------------------------------------------------------------------------------------
// 9. the dimension, and the gate on the call path
// ---------------------------------------------------------------------------------------------

/// The subscription pass charges the response names it compares, in bytes.
///
/// Draft 5.2.4.1's collection asks whether two selections share a response key, which is a **byte**
/// comparison over spellings the document chose and a GraphQL alias has no length bound. The only
/// charge on that path was the frame depth at the top of the loop — a coordinate walk that costs no
/// bytes at all — so a handful of very long aliases were compared end to end for `O(fields)` units.
///
/// A charge in front of the work is not a charge *for* the work unless it is in the work's own
/// dimension, which is the fact the second version of the pass table could not see.
#[test]
fn the_subscription_pass_charges_the_names_it_compares() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::SingleRootField);

  // The same number of selections either way; only the spelling grows. Sharing a prefix is what
  // makes the comparison read to the end rather than stopping at the first byte.
  let document = |pad: usize| {
    let alias = "a".repeat(pad);
    std::format!(
      "subscription s {{ {alias}x: newMessage {{ body }} {alias}y: newMessage {{ body }} }}"
    )
  };

  let short = min_budget(&schema, &document(1), rules);
  let long = min_budget(&schema, &document(20_000), rules);
  println!("subscription aliases: 1-byte {short} units, 20,000-byte {long} units");

  // **One** alias of twenty thousand bytes is read, not two: the first root field is *stored* into
  // `first_response` and the second is compared against it, so 2,501 units is the whole of what
  // this document owes. This assertion said 5,000 until al8n/smear#198's eleventh round, which is
  // the over-charge it was written to catch showing up in the plant that caught it — the charge
  // sat above the match and billed the storing arm as well. A charge counting selections still
  // cannot see any of it.
  assert!(
    long - short >= 2_000,
    "20,000-byte aliases cost {} units more than one-byte ones; the comparison is unpriced",
    long - short
  );
}

/// Each reader of an input object's field list pays for its own walk of it.
///
/// Round five of al8n/smear#198 widened one head prepayment to cover both 5.6.3's sort and 5.6.4's
/// sizing fold, because that fold walked the list unpaid. Round eleven deleted the fold — 5.6.4's
/// scan now charges each spelling as it resolves it — which left the prepayment covering a reader
/// that no longer needed it. So the gate narrowed back to 5.6.3, and this pins **both** halves
/// separately: neither rule pays for the other's walk, and each still pays for its own.
///
/// The type has a **required** field and the literal does not supply it, so 5.6.4's presence scan
/// runs to the end of the written list instead of stopping at a match. Round five's version used a
/// type with no required fields at all, where 5.6.4 walks nothing — which is why that version could
/// not survive the narrowing.
#[test]
fn each_reader_of_an_input_object_field_list_pays_for_its_own_walk() {
  let schema = build(SCHEMA);
  const FIELDS: usize = 5_000;
  let literal = "a: 1 ".repeat(FIELDS);
  let source = std::format!("{{ dog {{ withNeedy(opts: {{ {literal} }}) }} }}");

  // 5.6.2 is the baseline: it descends the literal and charges each field name once at the object
  // arm, reads no scalar literal, and walks the field list itself not at all.
  let names = min_budget(&schema, &source, RuleSet::only(Rule::InputObjectFieldNames));
  let unique = min_budget(
    &schema,
    &source,
    RuleSet::only(Rule::InputObjectFieldUniqueness),
  );
  let required = min_budget(
    &schema,
    &source,
    RuleSet::only(Rule::InputObjectRequiredFields),
  );
  println!("input object: 5.6.2 {names} units, 5.6.3 {unique} units, 5.6.4 {required} units");

  let one_walk = FIELDS as u32 * 9 / 10;
  assert!(
    unique >= names + one_walk,
    "5.6.3 sorts a {FIELDS}-field list and paid {} units more than a rule that does not",
    unique.saturating_sub(names)
  );
  assert!(
    required >= names + one_walk,
    "5.6.4 scans a {FIELDS}-field list and paid {} units more than a rule that does not",
    required.saturating_sub(names)
  );

  // And **one** walk each, not two. This is what the narrowed gate buys: before it, 5.6.4 paid the
  // head prepayment for a sort it does not run *and* its own scan, and this bound catches that.
  assert!(
    required < names + FIELDS as u32 * 3 / 2,
    "5.6.4 paid {} units for one walk of a {FIELDS}-field list",
    required.saturating_sub(names)
  );
}

/// A rule set that reads no directive, argument, value or variable usage pays for none of them.
///
/// The gate-per-charge-site sweep, as one document. `RuleSet::only(FieldSelections)` needs the
/// selection walk and nothing under it, so every charge below it must be behind a predicate that
/// names its consumers — and four were not: the directive list's prepayment, the argument list's,
/// the packed type behind every variable definition, and the search a variable leaf runs against
/// an index no enabled rule reads.
///
/// Each is a **false refusal**: work nobody asked for, charged to a ledger that can refuse.
#[test]
fn a_selection_only_rule_set_pays_for_selections() {
  let schema = build(SCHEMA);
  let long = "z".repeat(100_000);
  // One of each: a directive whose name is a hundred kilobytes, an argument likewise, a variable
  // whose declared type is likewise, and a usage of it.
  let source = std::format!(
    "query q($v{long}: Boolean{long}) {{ \
       dog @d{long}(arg{long}: 1) {{ \
         isHouseTrained(atOtherHomes: $v{long}) \
       }} \
     }}"
  );

  let spent = min_budget(&schema, &source, RuleSet::only(Rule::FieldSelections));
  println!("selection-only: {} bytes, {spent} units", source.len());

  // Four hundred kilobytes of spelling is 50,000 units if any of it is read. 5.3.1 reads the
  // field names and nothing else, so what it may pay for is the walk.
  assert!(
    spent < 5_000,
    "a selection-only rule set spent {spent} units; something below the walk is charging"
  );

  // And it is still `Ok`: the document is valid for the one rule that is on.
  let outcome = run(
    &schema,
    &source,
    &Budget::default(),
    RuleSet::only(Rule::FieldSelections),
  );
  assert!(!outcome.refused);
  assert_eq!(outcome.emitted, 0);
}

// ---------------------------------------------------------------------------------------------
// 10. gates named after readers, not families
// ---------------------------------------------------------------------------------------------

/// A usage-only rule set does not resolve declared types or walk constant defaults.
///
/// The gate on a variable definition's body was the **family** `walks_values(true)`, which ORs in
/// `collects_usages`. A default value and a definition's directives are `ConstInputValue` trees and
/// a constant value has no variable arm to return, so no usage rule can ever conclude anything
/// about one. The gate admitted three readers that cannot act, and it was run-wide besides — so it
/// hashed the declared type of every definition, including the ones with no default at all.
#[test]
fn a_usage_only_rule_set_does_not_walk_constant_defaults() {
  let schema = build(SCHEMA);
  const VARS: usize = 400;
  let long = "T".repeat(2_000);

  // Short variable names, very long declared types, and a constant default on each. What a
  // usage-only rule set needs from this list is the names; everything else has no reader.
  let mut source = String::from("query q(");
  for i in 0..VARS {
    source.push_str(&std::format!("$v{i}: Boolean{long} = true, "));
  }
  source.pop();
  source.pop();
  source.push_str(") { dog { name } }");

  let usages = min_budget(
    &schema,
    &source,
    RuleSet::only(Rule::AllVariableUsesDefined),
  );
  let types = min_budget(
    &schema,
    &source,
    RuleSet::only(Rule::VariablesAreInputTypes),
  );
  println!("constant defaults: 5.8.3 alone {usages} units, 5.8.2 alone {types} units");

  // 5.8.2 is a real reader of the declared type and pays for all 400 of them — 251 units each.
  // 5.8.3 reads none of them.
  assert!(
    types >= usages + (VARS as u32 * 250),
    "5.8.2 paid only {} units more than a rule that reads no declared type",
    types.saturating_sub(usages)
  );
  assert!(
    usages < 10_000,
    "a usage-only rule set spent {usages} units on a list of constant defaults"
  );
}

/// Only the rule that reads the `used` bitset pays to fill it.
///
/// Draft 5.8.4 is its only reader. 5.8.3 asks whether a name exists and 5.8.5 wants the *first*
/// declaration of it — neither looks at a mark. Marking was gated on `collects_usages`, the family,
/// so with `V` duplicate declarations against `U` usages either of them alone performed and charged
/// `O(U · V)` of marking that nothing consumed.
#[test]
fn only_the_rule_that_reads_the_bitset_pays_to_fill_it() {
  let schema = build(SCHEMA);
  const DUPES: usize = 300;
  const USAGES: usize = 300;

  let mut source = String::from("query q(");
  for _ in 0..DUPES {
    source.push_str("$a: Boolean, ");
  }
  source.pop();
  source.pop();
  source.push_str(") { dog {");
  for i in 0..USAGES {
    source.push_str(&std::format!(" u{i}: isHouseTrained(atOtherHomes: $a)"));
  }
  source.push_str(" } }");

  let exists = min_budget(
    &schema,
    &source,
    RuleSet::only(Rule::AllVariableUsesDefined),
  );
  let marks = min_budget(&schema, &source, RuleSet::only(Rule::AllVariablesUsed));
  println!("duplicate marking: 5.8.3 alone {exists} units, 5.8.4 alone {marks} units");

  // 5.8.4 walks the run of 300 declarations at each of 300 usages and pays for it; 5.8.3 walks no
  // run at all.
  let product = (DUPES * USAGES) as u32;
  assert!(
    marks >= exists + product / 2,
    "5.8.4 paid {} units more than a rule that marks nothing",
    marks.saturating_sub(exists)
  );
  assert!(
    exists < product / 4,
    "5.8.3 spent {exists} units on a bitset it never reads"
  );
}

/// A diagnostic's subject is charged in bytes before it is cloned.
///
/// The clone is the one piece of caller-sized work the validator does that is not a comparison, and
/// two sites reached it ahead of any byte charge: draft 5.2.1.1 copies the operation's name under a
/// charge of one unit from prep — the byte charge lived under 5.2.2.1, a different rule — and
/// 5.5.2.2 copies the cycle target's name once per **edge** against a charge taken once per
/// fragment.
///
/// What this pins is the charge, not the clone: see
/// [`Budget::validation_work`] for what `S: Clone` does and does not promise.
///
/// # Only 5.2.1.1 is pinned, and the other two are named here instead
///
/// The audit found three sites reaching a clone ahead of a byte charge, and two of them **cannot**
/// be told apart by a budget measurement, because a neighbouring charge already scales with the
/// same quantity. Measured: reverting 5.5.2.2's repair alone still moved the minimum budget from
/// 1,627 to 102,377 units, because `find_fragment` charges the same fragment name once per spread
/// on the way in. 5.6.1's OneOf field is the same one node over — the value walk charges that name
/// one level down, immediately after the clone. Both repairs are real and neither has a witness
/// that would fail without it, so asserting them here would be asserting the neighbour.
#[test]
fn a_diagnostic_subject_is_charged_before_it_is_cloned() {
  let schema = build(SCHEMA);
  let long = "n".repeat(20_000);

  // 5.2.1.1: the schema has no mutation root, so the name is cloned into the diagnostic — and no
  // other charge on this rule set reads an operation's spelling at all.
  let short_op = "mutation m { x }".to_owned();
  let long_op = std::format!("mutation m{long} {{ x }}");
  let rules = RuleSet::only(Rule::OperationTypeExistence);
  let short = min_budget(&schema, &short_op, rules);
  let long_cost = min_budget(&schema, &long_op, rules);
  println!("5.2.1.1 subject: short {short} units, 20,000-byte name {long_cost} units");
  assert!(
    long_cost - short >= 2_500,
    "a 20,000-byte operation name was cloned for {} units",
    long_cost - short
  );
}

// ---------------------------------------------------------------------------------------------
// 11. where the charge lives
// ---------------------------------------------------------------------------------------------

/// The subject a diagnostic clones is charged by the code that clones it.
///
/// `report_name` copies the spelling it is handed, and every caller charged *something* first —
/// twice a different string. `RuleSet::only(VariablesAreInputTypes)` is the case with no
/// neighbour: it charges the declared **type**, the name index is not built because no rule reads
/// it, and the variable's own spelling is therefore charged nowhere else at all. So a caller could
/// hand an arbitrarily long declaration name to a copy priced by three bytes of `Dog`.
///
/// The repair is not the charge but its **address**: the callee pays, so a caller cannot pass the
/// wrong subject and no new caller has to remember.
#[test]
fn a_cloned_subject_is_charged_by_the_code_that_clones_it() {
  let schema = build(SCHEMA);
  const VARS: usize = 200;
  // `Dog` is an object type, so 5.8.2 fires on every one of these and clones its name.
  let document = |pad: usize| {
    let long = "v".repeat(pad);
    let mut source = String::from("query q(");
    for i in 0..VARS {
      source.push_str(&std::format!("${long}{i}: Dog, "));
    }
    source.pop();
    source.pop();
    source.push_str(") { dog { name } }");
    source
  };

  let rules = RuleSet::only(Rule::VariablesAreInputTypes);
  let short = min_budget(&schema, &document(1), rules);
  let long = min_budget(&schema, &document(2_000), rules);
  println!("cloned subject: 1-byte names {short} units, 2,000-byte names {long} units");

  assert!(
    long - short >= (VARS as u32) * 250,
    "{VARS} names of 2,000 bytes were cloned for {} units",
    long - short
  );
}

/// Finding a variable leaf is not resolving the names above it, and neither happens outside an
/// operation.
///
/// Draft 5.8.3 asks whether a name was declared and 5.8.4 whether it was used; both are answered at
/// the leaf. Only 5.8.5 reads anything above one, because it needs the position's expected type.
/// One predicate gated descent and resolution together, so a `AllVariableUsesDefined`-only rule set
/// charged and schema-resolved every ancestor spelling on the way down.
///
/// And the same predicate never mentioned scope: `check_variable_usage` discards every leaf outside
/// an operation, so an unreached fragment was descended for nobody.
#[test]
fn descending_for_a_leaf_does_not_resolve_the_names_above_it() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::AllVariableUsesDefined);

  // The argument name is undeclared either way, so nothing above the leaf resolves to anything —
  // and the leaf is reached identically. Only the charge for the spelling changes.
  let ancestor = |pad: usize| {
    let long = "a".repeat(pad);
    std::format!("query q($v: Boolean) {{ dog {{ isHouseTrained({long}x: $v) }} }}")
  };
  let short = min_budget(&schema, &ancestor(1), rules);
  let long = min_budget(&schema, &ancestor(40_000), rules);
  println!("ancestor names: 1-byte {short} units, 40,000-byte {long} units");
  assert!(
    long - short < 1_000,
    "a 40,000-byte argument name cost {} units for a rule that reads no name above a leaf",
    long - short
  );

  // An unreached fragment: `check_variable_usage` returns before doing anything outside an
  // operation, so descending its value tree cannot produce a finding.
  let orphan = |entries: usize| {
    let list = "1 ".repeat(entries);
    std::format!(
      "{{ dog {{ name }} }} fragment Orphan on Dog {{ isHouseTrained(bogus: [{list}]) }}"
    )
  };
  let small = min_budget(&schema, &orphan(1), rules);
  let big = min_budget(&schema, &orphan(20_000), rules);
  println!("unreached fragment: 1 entry {small} units, 20,000 entries {big} units");
  assert!(
    big - small < 1_000,
    "a 20,000-entry literal in an unreached fragment cost {} units for a rule scoped to operations",
    big - small
  );
}

/// The variable-definition loop charges per definition, not per decision to enter it.
///
/// The gate says whether to loop; nothing was charging for going round it. A value-only rule set
/// opens this loop — a default value is a value — and then finds nothing to do on a declaration
/// with no default and no directives, so an arbitrarily long public-AST declaration list ran
/// through it for a constant budget.
#[test]
fn the_variable_definition_loop_charges_per_definition() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::ValuesOfCorrectType);

  // No defaults, no directives: every branch inside the loop declines, and the loop itself is the
  // only work left.
  let document = |vars: usize| {
    let mut source = String::from("query q(");
    for i in 0..vars {
      source.push_str(&std::format!("$v{i}: Boolean, "));
    }
    source.pop();
    source.pop();
    source.push_str(") { dog { name } }");
    source
  };

  let few = min_budget(&schema, &document(10), rules);
  let many = min_budget(&schema, &document(10_000), rules);
  println!("definition loop: 10 declarations {few} units, 10,000 declarations {many} units");
  assert!(
    many - few >= 9_000,
    "9,990 more declarations cost {} units",
    many - few
  );
}

// ---------------------------------------------------------------------------------------------
// 12. a gate that skips is a wrong answer
// ---------------------------------------------------------------------------------------------

/// The verdict does not depend on the order the operations were written in.
///
/// A fragment definition's own directives are the **non-constant** family, so they can carry a
/// variable usage — and a usage is operation-local while the directive *rules* over them are
/// definition-local. `begin_fragment` ran behind the bit that deduplicates the reporting, so only
/// the **first** operation to reach a fragment ever visited those directives.
///
/// The consequence is not a number moving. With `$v` declared by one operation and not the other,
/// putting the declaring one first hides draft 5.8.3 entirely; putting it second makes 5.8.4 report
/// the declaration unused. Two orderings of the same three definitions, two different verdicts —
/// and a budget test cannot see either of them.
#[test]
fn the_verdict_does_not_depend_on_operation_order() {
  let schema = build(SCHEMA);
  let budget = Budget::default();

  let declares = "query a($v: Boolean) { dog { ...F } }";
  let does_not = "query b { dog { ...F } }";
  let fragment = "fragment F on Dog @onFragDef(if: $v) { name }";

  let first = run(
    &schema,
    &std::format!("{declares} {does_not} {fragment}"),
    &budget,
    RuleSet::ALL,
  );
  let second = run(
    &schema,
    &std::format!("{does_not} {declares} {fragment}"),
    &budget,
    RuleSet::ALL,
  );
  println!(
    "operation order: declaring first {:?}, declaring second {:?}",
    first.rules(),
    second.rules()
  );

  assert_eq!(
    first.rules(),
    second.rules(),
    "the same definitions in a different order produced a different verdict"
  );
  // And the verdict is the right one: `b` uses `$v` without declaring it, `a` declares and uses it.
  assert_eq!(first.rules(), [Rule::AllVariableUsesDefined]);
  assert_eq!(first.emitted, 1, "{:?}", first.diagnostics);
  assert_eq!(second.emitted, 1, "{:?}", second.diagnostics);
}

/// A scan over a schema-sized group is charged for every position that reaches it.
///
/// The group is the schema's and its size is not an input — which is what the pass table said, and
/// it was silent about the other factor. A request chooses **how many times** to reach it, and the
/// per-required charge inside the loop sees none of the scan: an optional entry `continue`s before
/// spending, and with no written arguments a required one spends zero.
#[test]
fn a_schema_sized_scan_is_charged_per_position_that_reaches_it() {
  const DECLARED: usize = 200;
  const POSITIONS: usize = 200;

  // Every argument optional and every input field optional, so the loops scan the whole group and
  // reach no `spend` inside it.
  let args = (0..DECLARED)
    .map(|i| std::format!("a{i}: Int, "))
    .collect::<String>();
  let fields = (0..DECLARED)
    .map(|i| std::format!("f{i}: Int "))
    .collect::<String>();
  let sdl = std::format!(
    "type Query {{ dog: Dog }} \
     type Dog {{ name: String manyArgs({args}): Boolean withWide(opts: Wide): Boolean }} \
     input Wide {{ {fields} }}"
  );
  let schema = build(&sdl);

  let positions = |body: &str, n: usize| {
    let mut source = String::from("{ dog {");
    for i in 0..n {
      source.push_str(&std::format!(" x{i}: {body}"));
    }
    source.push_str(" } }");
    source
  };

  // 5.4.3's presence half: the argument group.
  let rules = RuleSet::only(Rule::RequiredArguments);
  let one = min_budget(&schema, &positions("manyArgs", 1), rules);
  let many = min_budget(&schema, &positions("manyArgs", POSITIONS), rules);
  println!("argument group: 1 position {one} units, {POSITIONS} positions {many} units");
  assert!(
    many - one >= ((POSITIONS - 1) * DECLARED) as u32,
    "{POSITIONS} positions over a {DECLARED}-argument group cost {} units",
    many - one
  );

  // 5.6.4's presence half: the input-field group, reached through an empty literal.
  let rules = RuleSet::only(Rule::InputObjectRequiredFields);
  let one = min_budget(&schema, &positions("withWide(opts: {})", 1), rules);
  let many = min_budget(&schema, &positions("withWide(opts: {})", POSITIONS), rules);
  println!("input-field group: 1 position {one} units, {POSITIONS} positions {many} units");
  assert!(
    many - one >= ((POSITIONS - 1) * DECLARED) as u32,
    "{POSITIONS} literals over a {DECLARED}-field group cost {} units",
    many - one
  );
}

// ---------------------------------------------------------------------------------------------
// 13. setup is work
// ---------------------------------------------------------------------------------------------

/// Opening a walk costs the same whatever the document's fragment population is.
///
/// # This is a guard, not a witness, and the measurement says so
///
/// The "already entered" set was a bitset cleared at the top of every operation's walk and every
/// subscription's root collection: `Θ(O · F / 64)` writes that the ledger never saw, because it
/// charges what a pass *examines* and this is what a pass *prepares*. Generation stamps remove the
/// clear rather than pricing it.
///
/// **The repair is not measurable at any size this suite can carry.** Measured on this machine,
/// release, `O = F = n`, bitset against stamps: 1.32/1.44 ms at 2,000, 8.85/9.64 at 10,000,
/// 33.64/32.30 at 30,000, 85.35/78.78 at 60,000 — indistinguishable, and the sign flips with
/// ordering noise. The arithmetic says why: at `n = 60,000` the clear is 56 million `u64` writes
/// over a buffer that stays in cache, about 7 ms of an 85 ms run, while a per-operation walk costs
/// microseconds against that buffer's nanoseconds. The quadratic term overtakes the linear one at
/// roughly two million fragments — a sixty-megabyte document, which is a legitimate input and not a
/// test fixture.
///
/// So this asserts the **property** — opening a walk is `O(1)` in the fragment population — and
/// would fail if a later change put document-sized setup back at the top of a per-operation walk
/// with a constant big enough to see. It does not distinguish `ad7fed3` from its successor, and
/// nothing in this suite does. Fourth time on this branch that a repair has arrived without a
/// witness; recording the failed measurement is what stops it reading as one.
#[test]
fn opening_a_walk_does_not_scale_with_the_fragment_population() {
  let schema = build(SCHEMA);
  let rules = without_merge();
  let budget = Budget::default();

  // `n` operations and `n` distinct fragments, each operation spreading exactly one. The walk is
  // tiny; the population is not.
  let queries = |n: usize| {
    let mut source = String::new();
    for i in 0..n {
      source.push_str(&std::format!("query q{i} {{ dog {{ ...f{i} }} }} "));
    }
    for i in 0..n {
      source.push_str(&std::format!("fragment f{i} on Dog {{ name }} "));
    }
    source
  };
  // The same shape through draft 5.2.4.1's root collection, which carried the identical clear.
  let subscriptions = |n: usize| {
    let mut source = String::new();
    for i in 0..n {
      source.push_str(&std::format!("subscription s{i} {{ ...f{i} }} "));
    }
    for i in 0..n {
      source.push_str(&std::format!(
        "fragment f{i} on Subscription {{ newMessage {{ body }} }} "
      ));
    }
    source
  };

  for (label, build_source) in [
    ("operation walk", &queries as &dyn Fn(usize) -> String),
    ("subscription roots", &subscriptions),
  ] {
    let small = best_of(3, || run(&schema, &build_source(2_000), &budget, rules));
    let large = best_of(3, || run(&schema, &build_source(20_000), &budget, rules));
    let ratio = large.ms / small.ms;
    println!(
      "{label}: 2,000 pairs {:.3} ms, 20,000 pairs {:.3} ms (ratio {ratio:.2})",
      small.ms, large.ms
    );
    assert!(!large.refused, "the document is valid and was refused");

    // Ten times the document. Linear-plus-sorting measures about sixteen; the threshold is set
    // where a document-sized clear with a *visible* constant would land and honest growth does not.
    assert!(
      ratio < 30.0,
      "{label}: ten times the document cost {ratio:.2} times the work"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// 14. over-charges: a false refusal is still a wrong answer
// ---------------------------------------------------------------------------------------------

/// 5.2.1.1 pays for a name only when it names one.
///
/// The operation-name prepayment existed for two readers: 5.2.2.1's sort, and 5.2.1.1's *clone* of
/// a name into a diagnostic's subject. Centralising the clone charge in `Validator::subject` left
/// the second reader paid for twice — and, on the path where 5.2.1.1 emits nothing at all, paid for
/// once by a rule that performs one `O(1)` root lookup and never reads a name. A long enough
/// operation name could exhaust `validation_work` and refuse a document that is **valid** under the
/// only rule the caller asked for.
///
/// Both halves are pinned: the silent path must not scale with the spelling, and the reporting path
/// must still scale, because that clone is real.
#[test]
fn operation_type_existence_pays_for_a_name_only_when_it_names_one() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::OperationTypeExistence);
  let pad = "q".repeat(40_000);

  // The schema has a query root, so the rule looks it up and says nothing.
  let quiet_short = min_budget(&schema, "query q { dog { name } }", rules);
  let quiet_long = min_budget(
    &schema,
    &std::format!("query q{pad} {{ dog {{ name }} }}"),
    rules,
  );
  println!("5.2.1.1 silent: short {quiet_short} units, 40,000-byte name {quiet_long} units");
  assert!(
    quiet_long - quiet_short < 1_000,
    "a 40,000-byte operation name cost {} units for a rule that reads no name",
    quiet_long - quiet_short
  );

  // The schema has no mutation root, so the rule emits and clones the name into it.
  let loud_short = min_budget(&schema, "mutation m { x }", rules);
  let loud_long = min_budget(&schema, &std::format!("mutation m{pad} {{ x }}"), rules);
  println!("5.2.1.1 reporting: short {loud_short} units, 40,000-byte name {loud_long} units");
  assert!(
    loud_long - loud_short >= 5_000,
    "the reporting path cloned a 40,000-byte name for {} units",
    loud_long - loud_short
  );
}

/// 5.5.2.3 pays for the bitset scan it runs, not the one it skips.
///
/// `possible` is `target == parent || intersect(..)`. An equal pair — the ecosystem's self-spread
/// exception — answers on the first operand and never touches a bitset, and the charge added from
/// this branch's own count audit one round earlier was taken above that test. A charge sized to the
/// work's worst path rather than its taken one is a false refusal.
#[test]
fn an_equal_type_spread_pays_for_no_bitset_scan() {
  // A wide possible-object set: the charge is one unit per word, so the interface needs enough
  // implementors for a word count to be visible.
  const IMPLEMENTORS: usize = 3_200;
  const SPREADS: usize = 200;
  let types = (0..IMPLEMENTORS)
    .map(|i| std::format!("type Impl{i} implements Wide {{ name: String }} "))
    .collect::<String>();
  let sdl = std::format!(
    "type Query {{ w: Wide }} interface Wide {{ name: String }} \
     type Other {{ name: String }} {types}"
  );
  let schema = build(&sdl);

  // Same spread count, same spelling lengths; only the target differs. `Other` implements nothing,
  // so its set is disjoint from `Wide`'s and the scan runs to the end.
  let document = |target: &str| {
    let spreads = std::format!("...{target} ").repeat(SPREADS);
    std::format!(
      "{{ w {{ ...F }} }} fragment F on Wide {{ {spreads} name }} fragment G on Other {{ name }}"
    )
  };

  let rules = RuleSet::only(Rule::FragmentSpreadIsPossible);
  let equal = min_budget(&schema, &document("F"), rules);
  let scanned = min_budget(&schema, &document("G"), rules);
  println!(
    "5.5.2.3: {SPREADS} equal spreads {equal} units, {SPREADS} scanned spreads {scanned} units"
  );

  // `IMPLEMENTORS / 64` words per scan. The equal spreads must pay for none of it.
  let words = (IMPLEMENTORS / 64) as u32;
  assert!(
    scanned >= equal + (SPREADS as u32) * words / 2,
    "the scanning document paid only {} units more than the one that scans nothing",
    scanned.saturating_sub(equal)
  );
}

// ---------------------------------------------------------------------------------------------
// 15. the branch a document takes is the branch that decides what it owes
// ---------------------------------------------------------------------------------------------

/// 5.5.2.3 pays for the words the intersection reads, not the words it could have.
///
/// `possible_objects_intersect` stops at the first overlapping word, and for a spread that *can*
/// apply the overlap is usually immediate. Prepaying the bitset's width billed every legal spread
/// for a scan that ended on its first comparison.
#[test]
fn a_short_circuited_intersection_pays_for_the_words_it_reads() {
  const IMPLEMENTORS: usize = 3_200;
  const SPREADS: usize = 200;
  let types = (0..IMPLEMENTORS)
    .map(|i| std::format!("type Impl{i} implements Wide {{ name: String }} "))
    .collect::<String>();
  let schema = build(&std::format!(
    "type Query {{ w: Wide }} interface Wide {{ name: String }} {types}"
  ));

  // Both spreads are possible, so both report nothing and differ only in *where* the overlap is:
  // the first implementor sits in word zero, the last in word forty-nine.
  let document = |target: &str| {
    let spreads = "...G ".repeat(SPREADS);
    std::format!(
      "{{ w {{ ...F }} }} fragment F on Wide {{ {spreads} name }} \
       fragment G on {target} {{ name }}"
    )
  };

  let rules = RuleSet::only(Rule::FragmentSpreadIsPossible);
  let first = min_budget(&schema, &document("Impl0"), rules);
  let last = min_budget(
    &schema,
    &document(&std::format!("Impl{}", IMPLEMENTORS - 1)),
    rules,
  );
  println!("5.5.2.3 short circuit: first-word overlap {first} units, last-word {last} units");

  // Forty-nine words of difference at each of two hundred spreads. Prepaying the width makes the
  // two identical.
  assert!(
    last >= first + (SPREADS as u32) * 40,
    "an overlap in word zero cost {} units less than one in word forty-nine",
    last.saturating_sub(first)
  );
}

/// The frame depth is paid by the arm that descends, not by every sibling.
///
/// `resolve` rebuilds the path from the definition root and runs **only after a pop**; examining
/// the next sibling is `O(1)`. Charging the depth at the top of the loop billed a level of `W`
/// siblings `W · D` for `W` examinations and one resolution.
#[test]
fn the_resolve_charge_is_paid_by_the_arm_that_resolves() {
  on_a_deep_stack(|| {
    let schema = build(SCHEMA);
    let rules = without_merge();

    // Two hundred levels deep, then `width` siblings at the bottom. The depth is fixed; only the
    // number of `O(1)` sibling examinations changes.
    let document = |width: usize| {
      let mut source = String::from("{");
      for _ in 0..200 {
        source.push_str(" nest {");
      }
      for i in 0..width {
        source.push_str(&std::format!(" a{i}: leaf"));
      }
      for _ in 0..200 {
        source.push_str(" } ");
      }
      source.push('}');
      source
    };

    let narrow = min_budget_of(&schema, &parse_deep(&document(1)), rules);
    let wide = min_budget_of(&schema, &parse_deep(&document(200)), rules);
    println!("resolve placement: 1 sibling {narrow} units, 200 siblings {wide} units");

    // 199 more siblings at depth 200 is about 39,800 units of depth charge if every iteration pays
    // it, and a few hundred if only the resolutions do.
    assert!(
      wide - narrow < 10_000,
      "199 more siblings at depth 200 cost {} units",
      wide - narrow
    );
  });
}

/// A usage reads no name when there is no index to search.
///
/// With no variable declarations the search runs zero comparisons and the existence test never
/// reaches its second operand, so not one byte of the spelling is read. The charge sat above both.
#[test]
fn an_empty_variable_index_reads_no_name() {
  let schema = build(SCHEMA);
  // A shorthand operation declares no variables, and 5.8.4 reports nothing about declarations that
  // do not exist — so nothing on this path reads or clones the spelling.
  let document = |pad: usize| {
    let long = "v".repeat(pad);
    let mut source = String::from("{ dog {");
    for i in 0..200 {
      source.push_str(&std::format!(
        " a{i}: isHouseTrained(atOtherHomes: ${long})"
      ));
    }
    source.push_str(" } }");
    source
  };

  let rules = RuleSet::only(Rule::AllVariablesUsed);
  let short = min_budget(&schema, &document(1), rules);
  let long = min_budget(&schema, &document(20_000), rules);
  println!("empty index: 1-byte name {short} units, 20,000-byte name {long} units");
  assert!(
    long - short < 1_000,
    "a 20,000-byte variable name cost {} units against an index with nothing in it",
    long - short
  );
}

/// A custom scalar pays for no literal, because it reads none.
///
/// `scalar_accepts` returns `true` for a custom scalar without inspecting anything — only the
/// service knows how to read one — while `Int` hands the digits to `fits_i32`. The charge sat above
/// the call and billed both the same.
#[test]
fn a_custom_scalar_pays_for_no_literal() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::ValuesOfCorrectType);
  let digits = "9".repeat(200_000);

  let custom = min_budget(
    &schema,
    &std::format!("{{ dog {{ withJson(payload: {digits}) }} }}"),
    rules,
  );
  let integer = min_budget(
    &schema,
    &std::format!("{{ dog {{ counted(times: {digits}) }} }}"),
    rules,
  );
  println!("literal arms: custom scalar {custom} units, Int {integer} units");

  // 200,000 digits is 25,001 units. `Int` reads them; `Json` does not.
  assert!(
    integer >= custom + 20_000,
    "the Int arm paid only {} units more than a custom scalar that reads nothing",
    integer.saturating_sub(custom)
  );
  assert!(
    custom < 1_000,
    "a custom scalar spent {custom} units on a literal it never looks at"
  );
}

/// A subscription's first root field is stored, not compared.
///
/// Draft 5.2.4.1 keeps the first response name and compares every later one against it. The `None`
/// arm moves two references and reads nothing; the charge sat above the match.
#[test]
fn a_stored_response_name_is_not_compared() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::SingleRootField);

  // Exactly one root field, so the storing arm is the only arm taken.
  let document = |pad: usize| {
    let alias = "a".repeat(pad);
    std::format!("subscription s {{ {alias}x: newMessage {{ body }} }}")
  };
  let short = min_budget(&schema, &document(1), rules);
  let long = min_budget(&schema, &document(40_000), rules);
  println!("stored response name: 1-byte {short} units, 40,000-byte {long} units");
  assert!(
    long - short < 1_000,
    "a 40,000-byte alias that is stored and never compared cost {} units",
    long - short
  );
}

/// The `@skip`/`@include` scan pays one unit per directive, and stops when it finds one.
///
/// Draft 5.2.4.1 asks each root selection whether it carries a conditional directive. The test is
/// `matches!(bytes, b"skip" | b"include")` — a length check and at most seven bytes, however long
/// the spelling is — and the scan stops at the first match. Charging every name's full length up
/// front was the wrong dimension and the wrong count at once. Found by the taken-branch audit.
#[test]
fn the_conditional_directive_scan_pays_one_unit_per_directive() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::SingleRootField);

  // Two hundred directives that are not `@skip` or `@include`, so the scan runs to the end and
  // reads at most seven bytes of each.
  let document = |pad: usize| {
    let long = "z".repeat(pad);
    let directives = std::format!("@d{long} ").repeat(200);
    std::format!("subscription s {{ newMessage {directives} {{ body }} }}")
  };

  let short = min_budget(&schema, &document(1), rules);
  let long = min_budget(&schema, &document(1_000), rules);
  println!("conditional scan: 1-byte names {short} units, 1,000-byte names {long} units");
  assert!(
    long - short < 1_000,
    "200 directives of 1,000 bytes cost {} units for a scan that reads seven of each",
    long - short
  );
}

/// 5.4.3's presence scan stops at the argument it finds.
///
/// `any` returns on the first match, and a required argument that *is* supplied — the ordinary case
/// — usually matches early. Charging the whole written list per declared entry billed the common
/// case for the worst one. Found by the taken-branch audit.
#[test]
fn the_required_argument_scan_stops_where_it_matches() {
  const OTHERS: usize = 200;
  let pad = "a".repeat(200);
  let declared = (0..OTHERS)
    .map(|i| std::format!("{pad}{i}: Int, "))
    .collect::<String>();
  let schema = build(&std::format!(
    "type Query {{ dog: Dog }} type Dog {{ manyArgs(need: Int!, {declared}): Boolean }}"
  ));

  let written = (0..OTHERS)
    .map(|i| std::format!("{pad}{i}: 1, "))
    .collect::<String>();
  let rules = RuleSet::only(Rule::RequiredArguments);
  // The same arguments, the required one written first or last. `any` walks the written order.
  let first = min_budget(
    &schema,
    &std::format!("{{ dog {{ manyArgs(need: 1, {written}) }} }}"),
    rules,
  );
  let last = min_budget(
    &schema,
    &std::format!("{{ dog {{ manyArgs({written} need: 1) }} }}"),
    rules,
  );
  println!("presence scan: required first {first} units, required last {last} units");

  // Two hundred names of two hundred bytes is 26 units each. Finding the match immediately must
  // cost none of them.
  assert!(
    last >= first + (OTHERS as u32) * 20,
    "writing the required argument last cost only {} units more than writing it first",
    last.saturating_sub(first)
  );
}

// ---------------------------------------------------------------------------------------------
// 16. n = 0, n = 1, and the short circuit — three questions, not one
// ---------------------------------------------------------------------------------------------

/// A spread's name is charged when there is an index to search for it.
///
/// `find_fragment` binary-searches the fragment index, and on an **empty** index it invokes its
/// comparator zero times and reads nothing. The charge in front of it was cleared as "always runs
/// to completion when charged", which is the `n = 1` answer given to the `n = 0` question.
#[test]
fn an_empty_fragment_index_reads_no_spread_name() {
  let schema = build(SCHEMA);
  // 5.3.1 only: `FragmentSpreadTargetDefined` is off, so the undefined spread is not reported and
  // its spelling is not cloned either.
  let rules = RuleSet::only(Rule::FieldSelections);
  let document = |pad: usize| {
    let long = "f".repeat(pad);
    let mut source = String::from("{ dog {");
    for _ in 0..200 {
      source.push_str(&std::format!(" ...{long}"));
    }
    source.push_str(" } }");
    source
  };

  let short = min_budget(&schema, &document(1), rules);
  let long = min_budget(&schema, &document(20_000), rules);
  println!("empty fragment index: 1-byte {short} units, 20,000-byte {long} units");
  assert!(
    long - short < 1_000,
    "200 spreads of a 20,000-byte name cost {} units against an index with nothing in it",
    long - short
  );
}

/// A one-element sort is charged for the comparisons it does not make.
///
/// The variable index's per-declaration charge pays for the **sort**, and a sort of one compares
/// nothing — as the duplicate scan reading its output starts at `base + 1` and compares nothing
/// either. The same crossing catches 5.2.2.1's operation-name prepayment, which was not named by
/// review.
#[test]
fn a_singleton_sort_is_not_charged_for_comparing() {
  let schema = build(SCHEMA);
  let long = "n".repeat(20_000);

  // One declaration, no duplicate, no usage: nothing reports, so no clone charge masks the build.
  let vars = RuleSet::only(Rule::VariableUniqueness);
  let one = min_budget(&schema, "query q($v: Boolean) { dog { name } }", vars);
  let one_long = min_budget(
    &schema,
    &std::format!("query q(${long}: Boolean) {{ dog {{ name }} }}"),
    vars,
  );
  println!("singleton variable index: short {one} units, 20,000-byte {one_long} units");
  assert!(
    one_long - one < 1_000,
    "one declaration of 20,000 bytes cost {} units for a sort that compares nothing",
    one_long - one
  );

  // Two declarations *do* sort, so the charge is not simply gone.
  let two = min_budget(
    &schema,
    "query q($a: Boolean, $b: Boolean) { dog { name } }",
    vars,
  );
  let two_long = min_budget(
    &schema,
    &std::format!("query q(${long}a: Boolean, ${long}b: Boolean) {{ dog {{ name }} }}"),
    vars,
  );
  println!("two declarations: short {two} units, 20,000-byte {two_long} units");
  assert!(
    two_long - two >= 4_000,
    "two declarations of 20,000 bytes cost {} units for a sort that does compare",
    two_long - two
  );

  // 5.2.2.1's operation names, the same crossing one rule over.
  let ops = RuleSet::only(Rule::OperationNameUniqueness);
  let single = min_budget(&schema, "query q { dog { name } }", ops);
  let single_long = min_budget(
    &schema,
    &std::format!("query q{long} {{ dog {{ name }} }}"),
    ops,
  );
  println!("singleton operation index: short {single} units, 20,000-byte {single_long} units");
  assert!(
    single_long - single < 1_000,
    "one named operation of 20,000 bytes cost {} units for a sort that compares nothing",
    single_long - single
  );
}

/// Draft 5.3.2's conflict subject is charged before it is cloned — **to draft 5.3.2's ledger**.
///
/// The last direct `source().clone()` in the crate, and centralising it through
/// `Validator::subject` got the charge right and the ledger wrong. 5.3.2's contract assigns its
/// work to `merge_work` and reserves `validation_work` for every other pass, so debiting the
/// validation ledger here meant a tight `validation_work` could replace a real merge-conflict
/// diagnostic with a resource refusal while `merge_work` still had room — and, with the validation
/// bound switched off, that the copy was accounted nowhere.
///
/// Both directions are pinned: the merge ledger pays for the alias, and the validation ledger does
/// not.
#[test]
fn a_merge_conflict_subject_is_charged_to_the_merge_ledger() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::FieldSelectionMerging).with(Rule::MergeWorkBudget);
  let document = |pad: usize| {
    let alias = "a".repeat(pad);
    std::format!("{{ dog {{ {alias}x: name {alias}x: nickname }} }}")
  };

  // The merge ledger pays. `min_merge_work` keys on 5.3.2's own bound firing.
  let short = min_merge_work(&schema, &document(1), rules);
  let long = min_merge_work(&schema, &document(20_000), rules);
  println!("merge ledger: 1-byte alias {short} units, 20,000-byte {long} units");
  assert!(
    long - short >= 2_000,
    "a 20,000-byte alias was cloned into a conflict diagnostic for {} merge units",
    long - short
  );

  // And the validation ledger does not. Merge work raised so 5.3.2 finishes and the only question
  // is what `validation_work` was asked to pay for.
  let validation = |source: &str| {
    let refused = |work: u32| {
      let budget = Budget::default()
        .with_merge_work(u32::MAX - 1)
        .with_validation_work(work);
      run(&schema, source, &budget, rules).refused
    };
    assert!(!refused(u32::MAX - 1));
    let (mut lo, mut hi) = (0u32, u32::MAX - 1);
    while lo < hi {
      let mid = lo + (hi - lo) / 2;
      if refused(mid) {
        lo = mid + 1;
      } else {
        hi = mid;
      }
    }
    lo
  };
  let short = validation(&document(1));
  let long = validation(&document(20_000));
  println!("validation ledger: 1-byte alias {short} units, 20,000-byte {long} units");
  assert!(
    long - short < 500,
    "the validation ledger paid {} units for a copy that belongs to draft 5.3.2",
    long - short
  );
}

/// Draft 5.3.2's own setup is charged **before** it runs, and a `merge_work` of zero buys nothing.
///
/// The one bypass the campaign found in the merge ledger rather than the validation one, and it was
/// on trunk the whole time: `build_merge_index` resized two document-sized tables, created a
/// `MergeSet` and a todo row per operation and per fragment, and hashed every fragment's type
/// condition through `Schema::sym` — all of it above the first `charge(1)`. So the smallest budget
/// a caller can name still bought `O(definitions + fragments + condition bytes)` of work and
/// allocation before the refusal. al8n/smear#198.
///
/// # The instrument
///
/// `Scratch::capacity` is the working set's own count of the rows it is holding, and the two runs
/// below differ in exactly one thing: whether draft 5.3.2's engine is activated. Everything else —
/// the prep sweep, the fragment graph, `check_fragments_used`'s reachability bitset, which is gated
/// on `FragmentsMustBeUsed` and stays on in both — fills identically. So a difference in the
/// working set *is* the merge engine's setup, and equality is the claim that a refused engine
/// allocated nothing.
#[test]
fn a_zero_merge_budget_refuses_before_the_first_allocation() {
  let schema = build(SCHEMA);
  let mut source = String::from("{ dog { name } }\n");
  for index in 0..400 {
    source.push_str(&std::format!("fragment f{index} on Dog {{ name }}\n"));
  }
  let document = parse(&source);

  let measure = |rules: RuleSet, merge_work: u32| {
    let mut scratch = Scratch::new();
    let mut sink = smear::validator::Ignore;
    let budget = Budget::default().with_merge_work(merge_work);
    let verdict =
      validate_executable_with(&schema, &document, &mut scratch, &budget, rules, &mut sink);
    (verdict.is_err(), scratch.capacity())
  };

  // The baseline: the same document with the engine switched off at all three of its activators,
  // so every table but the merge engine's is filled exactly as it is in the run below.
  let off = RuleSet::ALL
    .without(Rule::FieldSelectionMerging)
    .without(Rule::MergeDepthBudget)
    .without(Rule::MergeWorkBudget);
  let (_, baseline) = measure(off, u32::MAX - 1);

  // And the subject: the engine on, with nothing to spend.
  let (refused, at_zero) = measure(RuleSet::ALL, 0);
  assert!(refused, "a zero merge budget must refuse the document");
  assert_eq!(
    at_zero,
    baseline,
    "a refused merge engine grew the working set by {} rows",
    at_zero.saturating_sub(baseline)
  );

  // The premise: the setup this is measuring the absence of is worth measuring. With room to run,
  // the same engine over the same document builds a table hundreds of rows wide.
  let (_, ran) = measure(RuleSet::ALL, u32::MAX - 1);
  assert!(
    ran > baseline + 400,
    "the engine that ran grew the working set by only {}, so the zero-budget equality above is \
     not evidence of anything",
    ran - baseline
  );
}

/// A fragment's type condition is charged **in its own bytes**, to the merge ledger.
///
/// The third of `build_merge_index`'s three charges, and the one that is not a count.
/// `composite_of` resolves the condition through `Schema::sym`, which hashes every byte of a
/// spelling the *document* chose, while a row charge prices a name of any length at one unit. The
/// same repair reached `check_fragment_spread` on the validation side in an earlier round of
/// al8n/smear#198 and stopped at the ledger boundary, because the merge engine was somebody else's
/// file.
///
/// The condition names a type the schema does not define, which is what keeps this measuring the
/// *hash* rather than a lookup that succeeded: an unknown name is hashed end to end and then found
/// to be absent.
#[test]
fn a_fragment_type_condition_is_charged_in_bytes_to_the_merge_ledger() {
  let schema = build(SCHEMA);
  // 5.5.1.2 is off, so an unresolvable condition emits nothing and the only thing under test is
  // what reading it cost.
  let rules = RuleSet::only(Rule::FieldSelectionMerging).with(Rule::MergeWorkBudget);
  let document = |pad: usize| {
    let condition = "T".repeat(pad);
    std::format!("{{ dog {{ name }} }}\nfragment f on {condition} {{ name }}")
  };

  let short = min_merge_work(&schema, &document(1), rules);
  let long = min_merge_work(&schema, &document(20_000), rules);
  println!("merge ledger: 1-byte condition {short} units, 20,000-byte {long} units");
  assert!(
    long - short >= 2_000,
    "a 20,000-byte type condition was hashed for {} merge units",
    long - short
  );
}

/// The two spellings `fill_merge_set` reads are charged in bytes too — the sibling sweep of the
/// charge above.
///
/// `build_merge_index` is where the type condition of a fragment *definition* is resolved.
/// `fill_merge_set` is where the other two document-chosen spellings on the merge path are read:
/// an **inline fragment's** condition, through the same `composite_of`, and a **spread's** name,
/// through `find_fragment`'s comparator, which compares whole names. Both sat behind
/// `charge(selections().len())` — one unit for the selection, nothing for the spelling — which is
/// the same defect the finding named, at the two places the same round's sweep reaches.
///
/// The spread charge is gated on a non-empty fragment table, because a binary search over an empty
/// index returns without a comparison; that is the `n = 0` half of the same audit, and the gate is
/// the one `check_fragment_spread` already takes on the validation side.
#[test]
fn the_spellings_fill_merge_set_reads_are_charged_in_bytes() {
  let schema = build(SCHEMA);
  let rules = RuleSet::only(Rule::FieldSelectionMerging).with(Rule::MergeWorkBudget);

  // An inline fragment's condition, resolved through `composite_of` exactly as a definition's is.
  let inline = |pad: usize| {
    let condition = "T".repeat(pad);
    std::format!("{{ dog {{ ... on {condition} {{ name }} }} }}")
  };
  let short = min_merge_work(&schema, &inline(1), rules);
  let long = min_merge_work(&schema, &inline(20_000), rules);
  println!("merge ledger: 1-byte inline condition {short} units, 20,000-byte {long} units");
  assert!(
    long - short >= 2_000,
    "a 20,000-byte inline condition was hashed for {} merge units",
    long - short
  );

  // A spread's name, compared whole by `find_fragment`'s comparator.
  let spread = |pad: usize| {
    let name = std::format!("f{}", "x".repeat(pad));
    std::format!("{{ dog {{ ...{name} }} }}\nfragment {name} on Dog {{ name }}")
  };
  let short = min_merge_work(&schema, &spread(1), rules);
  let long = min_merge_work(&schema, &spread(20_000), rules);
  println!("merge ledger: 1-byte spread name {short} units, 20,000-byte {long} units");
  assert!(
    long - short >= 2_000,
    "a 20,000-byte spread name was searched for {} merge units",
    long - short
  );
}

/// Draft 5.8.4's marks bitset is sized behind its charge, so a starved ledger allocates nothing
/// for it.
///
/// Round 14's finding on the validation side, one file over, and the reviewer's recommendation was
/// the merge-side repair verbatim: clear, saturating count, charge, resize. The two sites are the
/// same four moves for the same reason.
///
/// # Why the other three bitsets in the module were not this
///
/// `on_path`, `done` and `reachable` are sized to the document's **fragment** count, and the prep
/// sweep charges at least one unit per fragment before any of them is reset — so `count / 64` words
/// of zeroing sit behind `count` units already taken, over-prepaid by a factor of 64. Prep charges
/// per **definition**. One operation declaring `V` variables is one definition, so `used`'s `V / 64`
/// words sat behind a single unit.
///
/// # The instrument
///
/// At a `validation_work` too small to reach the end of either document, the working set must not
/// depend on `V`: the ledger died at the same number of units in both runs, so everything charged
/// grew by the same amount, and a difference is something that was *not* charged. The two counts
/// are ten times apart, so the old sizing shows up as roughly nine hundred words.
#[test]
fn the_variable_marks_bitset_is_sized_behind_its_charge() {
  let schema = build(SCHEMA);
  // 5.8.4 alone: `marks_usage` is on, which is what reaches the bitset at all.
  let rules = RuleSet::only(Rule::AllVariablesUsed);
  let document = |count: usize| {
    let mut source = String::from("query q(");
    for index in 0..count {
      source.push_str(&std::format!("$v{index}: Int "));
    }
    source.push_str(") { dog { name } }");
    source
  };
  let small = document(6_400);
  let large = document(64_000);

  let capacity = |source: &str, work: u32| {
    let parsed = parse(source);
    let mut scratch = Scratch::new();
    let mut sink = smear::validator::Ignore;
    let budget = Budget::default().with_validation_work(work);
    let verdict =
      validate_executable_with(&schema, &parsed, &mut scratch, &budget, rules, &mut sink);
    (verdict.is_err(), scratch.capacity())
  };

  // Starved: both runs refuse, and the one that refuses must not have sized a table on the way.
  let (small_refused, starved_small) = capacity(&small, 100);
  let (large_refused, starved_large) = capacity(&large, 100);
  assert!(
    small_refused && large_refused,
    "both starved runs must refuse for this to be measuring a refusal"
  );
  assert_eq!(
    starved_small, starved_large,
    "a refused run sized a table by its variable count: {starved_small} against {starved_large}"
  );

  // The premise: `V` is a dimension the working set does grow in when there is budget for it, so
  // the equality above is a statement about the refusal and not about the documents.
  let (_, ran_small) = capacity(&small, u32::MAX - 1);
  let (_, ran_large) = capacity(&large, u32::MAX - 1);
  assert!(
    ran_large > ran_small + 40_000,
    "the two documents differ by only {} rows when both are fully validated, so the starved \
     comparison above discriminates nothing",
    ran_large - ran_small
  );
}
