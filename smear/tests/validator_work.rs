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
}

type Subscription { newMessage: Message }
type Message { body: String }

input Opts { need: Int! a: Int }
input Loose { a: Int b: Int }

directive @onField repeatable on FIELD
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

  // Two aliases of twenty thousand bytes are 5,002 units of spelling. A charge counting selections
  // cannot see any of it.
  assert!(
    long - short >= 5_000,
    "20,000-byte aliases cost {} units more than one-byte ones; the comparison is unpriced",
    long - short
  );
}

/// The input object's field list is prepaid for **both** of its readers.
///
/// 5.6.3 sorts the list and 5.6.4 walks it to size its per-scan charge and then rescans it once per
/// required field. The prepayment sat inside 5.6.3's guard, so with only 5.6.4 enabled the sizing
/// fold ran over an arbitrarily wide literal before the `spend` it feeds — and the rescan's charge
/// was the only one taken.
#[test]
fn an_input_object_field_list_is_prepaid_for_both_readers() {
  let schema = build(SCHEMA);
  const FIELDS: usize = 5_000;
  let literal = "a: 1 ".repeat(FIELDS);
  // `Loose` declares **no required fields**, which is the sharp case: 5.6.4 still walks the whole
  // written list to size its per-scan charge, and then finds nothing to spend that charge on. The
  // walk happened and nothing paid for it.
  let source = std::format!("{{ dog {{ withLoose(opts: {{ {literal} }}) }} }}");

  // 5.6.2 is the baseline: it descends the same literal and pays for each field name one level
  // down exactly as 5.6.4 does, reads no scalar literal, and never walks the field list. So the
  // descent cancels and what is left is the list walk itself.
  let required = min_budget(
    &schema,
    &source,
    RuleSet::only(Rule::InputObjectRequiredFields),
  );
  let names = min_budget(&schema, &source, RuleSet::only(Rule::InputObjectFieldNames));
  println!("input object: 5.6.4 alone {required} units, 5.6.2 alone {names} units");

  assert!(
    required >= names + FIELDS as u32,
    "5.6.4 walked a {FIELDS}-field list and paid {} units more than a rule that does not walk it",
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
