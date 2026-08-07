//! Draft 5.3.2 `FieldsInSetCanMerge`, and the resource bound that makes it safe on a request path.
//!
//! # Two things are under test here
//!
//! The **rule**: the specification's own examples and counter-examples, each asserted to merge or
//! to conflict, plus the shapes only a fragment graph produces.
//!
//! The **bound**: hostile documents under the **default** [`Budget`] — not a lowered one. A
//! lowered knob shows the mechanism fires and is what `validator_rules.rs`'s liveness fixtures
//! use; it says nothing about whether the shipped defaults are the right size. These tests say
//! that, and record the wall clock at the refusal so a regression in the engine's cost shows up as
//! a number rather than as a hang.

// Every fixture and assertion below calls into `smear::validator`, which does not exist in the
// crate's API surface with the feature off. This file did not gate itself to that dependency —
// the same defect `validator_schema.rs` carried until #100 — so it was a hard `E0433` compile
// error under any feature selection that excludes `validator`, including the crate's own default
// features. `cargo test -p smear --no-run` in CI now compiles exactly that selection.
#![cfg(feature = "validator")]
#![allow(missing_docs)]

use std::{string::String, thread, time::Instant, vec::Vec};

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{
    Budget, Collect, Context, Count, Diagnostic, First, Ignore, MergeConflict, Rule, Schema,
    Scratch, validate_executable,
  },
};

// ---------------------------------------------------------------------------------------------
// harness
// ---------------------------------------------------------------------------------------------

/// The specification's §5 example type system, plus the two self-referential shapes a depth or
/// breadth fixture needs and the abstract types the common-parent split is about.
const SCHEMA: &str = r#"
type Query {
  dog: Dog
  cat: Cat
  pet: Pet
  catOrDog: CatOrDog
  human: Human
  nest: Nest
  note(payload: Json): String
}

type Nest {
  nest: Nest
  leaf: Int
  name: String
}

interface Pet {
  name: String!
}

type Human {
  name: String!
  pets: [Pet]
}

enum DogCommand { SIT DOWN HEEL }
enum CatCommand { JUMP }

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHouseTrained(atOtherHomes: Boolean): Boolean!
  owner: Human
  friend: Dog
}

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
  friend: Cat
}

union CatOrDog = Cat | Dog

scalar Json
"#;

fn build() -> Schema {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SCHEMA)
  .expect("the SDL parses");
  Schema::build(&document).expect("the SDL is a schema")
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
  .unwrap_or_else(|errors| panic!("fixture query does not parse: {errors:?}"))
}

/// Runs `body` on a thread with room for the parser's own recursion.
///
/// The parser is a recursive descent and, in a debug build, overflows a test thread's default
/// stack somewhere between 25 and 50 levels of selection nesting — long before the depths these
/// tests are about. Dropping the resulting tree recurses too. Neither is what is under test here,
/// so both are given room rather than being worked around by assembling the tree by hand: a
/// budget test wants the spans and the shape a real document has.
fn on_a_deep_stack<T: Send + 'static>(body: impl FnOnce() -> T + Send + 'static) -> T {
  thread::Builder::new()
    .stack_size(64 << 20)
    .spawn(body)
    .expect("the worker starts")
    .join()
    .expect("the worker finishes")
}

/// Every diagnostic `source` produces under `budget`.
fn diagnose<'a>(schema: &Schema, source: &'a str, budget: &Budget) -> Vec<Diagnostic<&'a str>> {
  let document = parse(source);
  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let _ = validate_executable(schema, &document, &mut scratch, budget, &mut sink);
  collected
}

/// The rules `source` fires under the default budget, sorted and deduplicated.
fn fired(schema: &Schema, source: &str) -> Vec<Rule> {
  let mut rules: Vec<_> = diagnose(schema, source, &Budget::default())
    .iter()
    .map(Diagnostic::rule)
    .collect();
  rules.sort_unstable();
  rules.dedup();
  rules
}

/// Which of draft 5.3.2's three requirements a document breaks, in report order.
fn conflicts(schema: &Schema, source: &str) -> Vec<MergeConflict> {
  diagnose(schema, source, &Budget::default())
    .iter()
    .filter(|d| d.rule() == Rule::FieldSelectionMerging)
    .filter_map(|d| match d.context_of() {
      Context::Merge(conflict) => Some(conflict),
      _ => None,
    })
    .collect()
}

// ---------------------------------------------------------------------------------------------
// the rule
// ---------------------------------------------------------------------------------------------

/// The specification's own `mergeIdenticalFields` family, verbatim in behaviour.
#[test]
fn the_specifications_examples_merge() {
  let schema = build();
  for source in [
    // 5.3.2 examples, as operations over the same shapes the specification writes as fragments.
    "{ dog { name name } }",
    "{ dog { otherName: name otherName: name } }",
    "{ dog { doesKnowCommand(dogCommand: SIT) doesKnowCommand(dogCommand: SIT) } }",
    "query withVariable($dogCommand: DogCommand!) \
     { dog { doesKnowCommand(dogCommand: $dogCommand) doesKnowCommand(dogCommand: $dogCommand) } }",
    // `safeDifferingFields` and `safeDifferingArgs`: two concrete parents that cannot both be
    // encountered, so the pair never has to agree.
    "{ pet { ... on Dog { volume: barkVolume } ... on Cat { volume: meowVolume } } }",
    "{ pet { ... on Dog { doesKnowCommand(dogCommand: SIT) } \
             ... on Cat { doesKnowCommand(catCommand: JUMP) } } }",
    // Merging happens one level down too, and the merged set is checked in turn.
    "{ dog { owner { name } owner { pets { name } } } }",
    // A fragment spread twice contributes its fields once.
    "{ dog { ...fields ...fields } } fragment fields on Dog { name barkVolume }",
    // Argument order does not matter, and object literals compare as maps.
    "{ dog { isHouseTrained(atOtherHomes: true) isHouseTrained(atOtherHomes: true) } }",
  ] {
    assert_eq!(fired(&schema, source), [], "\n---\n{source}");
  }
}

/// The specification's own counter-examples, each with the requirement it breaks.
#[test]
fn the_specifications_counter_examples_conflict() {
  let schema = build();
  for (source, expected) in [
    // `conflictingBecauseAlias`: different fields behind one response name — and, here, different
    // shapes as well, because `nickname` is nullable where `name` is not.
    (
      "{ dog { name: nickname name } }",
      &[MergeConflict::Shapes, MergeConflict::Fields][..],
    ),
    // `conflictingArgsOnValues`, `conflictingArgsValueAndVar`, `conflictingArgsWithVars`,
    // `differingArgs` — the same field, four ways of disagreeing about its arguments.
    (
      "{ dog { doesKnowCommand(dogCommand: SIT) doesKnowCommand(dogCommand: HEEL) } }",
      &[MergeConflict::Arguments],
    ),
    (
      "query q($dogCommand: DogCommand!) \
       { dog { doesKnowCommand(dogCommand: SIT) doesKnowCommand(dogCommand: $dogCommand) } }",
      &[MergeConflict::Arguments],
    ),
    (
      "query q($varOne: DogCommand!, $varTwo: DogCommand!) \
       { dog { doesKnowCommand(dogCommand: $varOne) doesKnowCommand(dogCommand: $varTwo) } }",
      &[MergeConflict::Arguments],
    ),
    // `differingArgs`. Draft 5.4.3 also has something to say about the second selection, which is
    // why this one is asserted through `conflicts` rather than through the whole fired set.
    (
      "{ dog { doesKnowCommand(dogCommand: SIT) doesKnowCommand } }",
      &[MergeConflict::Arguments],
    ),
    // `conflictingDifferingResponses`: two parents that cannot overlap, so the field names need
    // not agree — but the *shapes* must, and `String` is not `Int`.
    (
      "{ pet { ... on Dog { someValue: nickname } ... on Cat { someValue: meowVolume } } }",
      &[MergeConflict::Shapes],
    ),
  ] {
    assert_eq!(conflicts(&schema, source), expected, "\n---\n{source}");
    assert!(
      fired(&schema, source).contains(&Rule::FieldSelectionMerging),
      "\n---\n{source}"
    );
  }

  // Only `differingArgs` brings a second rule with it, and it is draft 5.4.3 answering for the
  // missing argument rather than 5.3.2 firing twice.
  assert_eq!(
    fired(
      &schema,
      "{ dog { doesKnowCommand(dogCommand: SIT) doesKnowCommand } }"
    ),
    [Rule::FieldSelectionMerging, Rule::RequiredArguments]
  );
}

/// The rule sees through fragments, at any depth, and blames the second of the two selections.
#[test]
fn conflicts_are_found_through_fragments() {
  let schema = build();
  let source = "{ dog { ...left ...right } } \
                fragment left on Dog { x: name } \
                fragment right on Dog { x: nickname }";
  let diagnostics = diagnose(&schema, source, &Budget::default());
  assert!(!diagnostics.is_empty(), "the conflict was not found");
  for diagnostic in &diagnostics {
    assert_eq!(diagnostic.rule(), Rule::FieldSelectionMerging);
    assert_eq!(diagnostic.subject_source(), Some(&"x"));
    let span = diagnostic.span();
    assert_eq!(&source[span.start()..span.end()], "x: nickname");
    let related = diagnostic.related_span().expect("the first of the two");
    assert_eq!(&source[related.start()..related.end()], "x: name");
  }

  // And one level further down, where the merged set is itself a merge of two fragments.
  assert_eq!(
    fired(
      &schema,
      "{ dog { owner { ...a } owner { ...b } } } \
       fragment a on Human { y: name } \
       fragment b on Human { y: pets { name } }"
    ),
    [Rule::FieldSelectionMerging]
  );
}

/// A common-parent part carries every abstract-parent selection, because a schema change can make
/// any abstract type overlap any other.
#[test]
fn abstract_parents_must_agree_with_every_concrete_one() {
  let schema = build();
  // `name` on the interface and `name` on `Dog` are the same field: fine.
  assert_eq!(fired(&schema, "{ pet { name ... on Dog { name } } }"), []);
  // `x` on the interface is `Pet.name`; `x` on `Dog` is `Dog.nickname`. Nothing says a future
  // `Dog` cannot be the runtime type of that `Pet`, so the two must already agree, and they do
  // not.
  assert_eq!(
    fired(&schema, "{ pet { x: name ... on Dog { x: nickname } } }"),
    [Rule::FieldSelectionMerging]
  );
}

/// A cyclic fragment graph does not make the expansion loop.
///
/// Draft 5.5.2.2 has already refused the document by the time the engine runs, but the engine does
/// not lean on that: an expansion enters each named fragment once, which is the specification's
/// own "including visiting fragments" read as the set it is.
#[test]
fn a_cyclic_fragment_graph_terminates() {
  let schema = build();
  let rules = fired(
    &schema,
    "{ dog { ...a } } fragment a on Dog { name ...b } fragment b on Dog { barkVolume ...a }",
  );
  assert!(rules.contains(&Rule::FragmentSpreadsMustNotFormCycles));
  assert!(!rules.contains(&Rule::MergeWorkBudget), "{rules:?}");
  assert!(!rules.contains(&Rule::MergeDepthBudget), "{rules:?}");
}

/// A fragment no operation reaches still has its own selection set checked.
#[test]
fn unreached_fragments_are_merged_too() {
  let schema = build();
  let rules = fired(
    &schema,
    "{ dog { name } } fragment orphan on Dog { x: name x: nickname }",
  );
  assert!(rules.contains(&Rule::FieldSelectionMerging), "{rules:?}");
}

/// Two operations that reach the *same* field occurrences share one memo entry, and the second
/// still gets both passes over the right rows.
///
/// The memo is keyed on field-occurrence identity, so two operations spreading one fragment do
/// collide — and the row range a claim hands back is then the canonical one, not the range the
/// expansion just built. Carrying the stale range into the second pass reads whatever the first
/// pass's own expansions wrote over it.
#[test]
fn two_operations_sharing_a_fragment_are_both_checked() {
  let schema = build();
  let source = "query one { ...shared } query two { ...shared } \
                fragment shared on Query { \
                  dog { doesKnowCommand(dogCommand: SIT) doesKnowCommand(dogCommand: HEEL) \
                        owner { name } } }";
  assert_eq!(fired(&schema, source), [Rule::FieldSelectionMerging]);

  // The conflict is a property of the fragment, and the fragment is one definition, so it is
  // reported once however many operations reach it.
  let diagnostics = diagnose(&schema, source, &Budget::default());
  assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
  assert_eq!(
    diagnostics[0].context_of(),
    Context::Merge(MergeConflict::Arguments)
  );

  // The same shape with a conflict only the response-shape pass can see.
  assert_eq!(
    fired(
      &schema,
      "query one { ...shared } query two { ...shared } \
       fragment shared on Query { pet { ... on Dog { v: nickname } ... on Cat { v: meowVolume } } }"
    ),
    [Rule::FieldSelectionMerging]
  );
}

// ---------------------------------------------------------------------------------------------
// the bound
// ---------------------------------------------------------------------------------------------

/// A selection nested `depth` levels through the self-referential `Nest` type.
fn deep_query(depth: usize) -> String {
  let mut source = String::with_capacity(depth * 8 + 16);
  source.push('{');
  for _ in 0..depth {
    source.push_str(" nest {");
  }
  source.push_str(" leaf ");
  for _ in 0..depth {
    source.push_str("} ");
  }
  source.push('}');
  source
}

/// `breadth` selections of the same response name, each with its own subselection.
///
/// One response name collecting thousands of members is the shape that makes every later step —
/// grouping, partitioning, comparing, merging the subselections — proportional to the whole
/// document rather than to one selection set.
fn wide_query(breadth: usize) -> String {
  let mut source = String::with_capacity(breadth * 18 + 16);
  source.push_str("{ ");
  for _ in 0..breadth {
    source.push_str("a: nest { leaf } ");
  }
  source.push('}');
  source
}

/// Runs `source` under `budget` and returns the verdict, the rules, and the wall clock.
fn timed(schema: &Schema, source: &str, budget: &Budget) -> (bool, Vec<Rule>, f64) {
  let document = parse(source);
  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let start = Instant::now();
  let verdict = validate_executable(schema, &document, &mut scratch, budget, &mut sink);
  let elapsed = start.elapsed().as_secs_f64() * 1e3;
  let budget_tripped = verdict.as_ref().err().is_some_and(|i| i.budget_tripped());
  let mut rules: Vec<_> = collected.iter().map(Diagnostic::rule).collect();
  rules.sort_unstable();
  rules.dedup();
  (budget_tripped, rules, elapsed)
}

/// A response shape nested past the default depth is refused, in bounded time, by name.
#[test]
fn the_default_depth_budget_refuses_a_deep_document() {
  on_a_deep_stack(|| {
    let schema = build();
    let budget = Budget::default();
    assert_eq!(budget.merge_depth(), 128);

    // apollo-compiler's own measured behaviour at this shape: 140 levels fails closed.
    let source = deep_query(200);
    let (tripped, rules, ms) = timed(&schema, &source, &budget);
    println!(
      "merge_depth trip: {} bytes, 200 levels, {ms:.3} ms",
      source.len()
    );
    assert!(tripped, "the verdict does not report the budget");
    assert_eq!(rules, [Rule::MergeDepthBudget], "{rules:?}");
    assert!(ms < 500.0, "refusing took {ms} ms");

    // And the level below the bound is validated rather than refused.
    let (tripped, rules, _) = timed(&schema, &deep_query(120), &budget);
    assert!(!tripped);
    assert_eq!(rules, []);
  });
}

/// A hostile breadth grid is refused by the default work budget, in bounded time, by name.
#[test]
fn the_default_work_budget_refuses_a_wide_document() {
  let schema = build();
  let budget = Budget::default();
  assert_eq!(budget.merge_work(), 65_536);

  let source = wide_query(20_000);
  let (tripped, rules, ms) = timed(&schema, &source, &budget);
  println!(
    "merge_work trip: {} bytes, 20000 same-name selections, {ms:.3} ms",
    source.len()
  );
  assert!(tripped, "the verdict does not report the budget");
  assert_eq!(rules, [Rule::MergeWorkBudget], "{rules:?}");
  assert!(ms < 500.0, "refusing took {ms} ms");

  // Exactly one. A collecting sink does not stop the unwinding, so a bound that reported itself
  // once per remaining unit of work would bury the document's real problems under thousands of
  // copies of the same sentence.
  let diagnostics = diagnose(&schema, &source, &budget);
  assert_eq!(
    diagnostics.len(),
    1,
    "the refusal was reported more than once"
  );
}

/// The engine is memoised, and the difference is the difference between finishing and not.
///
/// Each level spreads the same fragment from two different fields, so a merge that re-derived
/// every branch would do `2^depth` of them. The memo recognises that both branches merge the same
/// field occurrences and checks it once, which is why thirty levels finish at all.
#[test]
fn shared_subtrees_are_merged_once() {
  let schema = build();
  const LEVELS: usize = 30;

  let mut source = String::from("{ nest { ...l0 } }");
  for level in 0..LEVELS {
    source.push_str(&std::format!(
      " fragment l{level} on Nest {{ a: nest {{ ...l{next} }} b: nest {{ ...l{next} }} }}",
      next = level + 1
    ));
  }
  source.push_str(&std::format!(" fragment l{LEVELS} on Nest {{ leaf }}"));

  let (tripped, rules, ms) = timed(&schema, &source, &Budget::default());
  println!(
    "memoised diamond: {} bytes, {LEVELS} levels, {ms:.3} ms",
    source.len()
  );
  assert!(!tripped, "a memoised merge should not need the budget");
  assert_eq!(rules, [], "{rules:?}");
  assert!(ms < 500.0, "the memo is not working: {ms} ms");
}

/// The bound is a refusal, not a truncation: the verdict says so, and an ordinary document does
/// not say it.
#[test]
fn the_budget_flag_pins_both_states() {
  on_a_deep_stack(|| {
    let schema = build();
    let budget = Budget::default();

    let deep = deep_query(200);
    let refused = parse(&deep);
    let mut scratch = Scratch::new();
    let mut sink = First::new();
    let invalid = validate_executable(&schema, &refused, &mut scratch, &budget, &mut sink)
      .expect_err("a document past the depth bound is invalid");
    assert!(invalid.budget_tripped());
    assert_eq!(
      sink.get().expect("a diagnostic").rule(),
      Rule::MergeDepthBudget
    );
    let rendered = sink
      .get()
      .expect("a diagnostic")
      .display(&schema)
      .to_string();
    assert!(
      rendered.starts_with("5.3.2/depth Merge Depth Budget Exceeded: <here>, found 128 ("),
      "{rendered}"
    );
    assert!(
      invalid.to_string().contains("resource budget exceeded"),
      "{invalid}"
    );

    // An ordinary invalid document is invalid for its own reasons and says nothing about a budget.
    let ordinary = parse("{ dog { name: nickname name } }");
    let mut sink = Count::new();
    let invalid = validate_executable(&schema, &ordinary, &mut scratch, &budget, &mut sink)
      .expect_err("the aliases conflict");
    assert!(!invalid.budget_tripped());
    assert!(!invalid.to_string().contains("budget"), "{invalid}");
  });
}

/// Switching the bound's own rule off stops the *refusal*, not the engine.
///
/// The bound still holds — the work is not done — but with no diagnostic to emit there is nothing
/// to make the document invalid, so it comes back `Ok` on what was examined. That is the honest
/// reading of `RuleSet`: a rule that is off is not evaluated, and this rule's evaluation is the
/// refusal. What a caller keeps by turning it off is the resource protection; what they give up is
/// being told.
#[test]
fn the_bound_holds_even_with_its_rule_disabled() {
  use smear::validator::{RuleSet, validate_executable_with};

  on_a_deep_stack(|| {
    let schema = build();
    let source = deep_query(200);
    let document = parse(&source);
    let mut scratch = Scratch::new();
    let rules = RuleSet::ALL.without(Rule::MergeDepthBudget);

    let start = Instant::now();
    let verdict = validate_executable_with(
      &schema,
      &document,
      &mut scratch,
      &Budget::default(),
      rules,
      &mut Ignore,
    );
    let ms = start.elapsed().as_secs_f64() * 1e3;
    println!("bound with the rule disabled: {ms:.3} ms");
    assert!(
      verdict.is_ok(),
      "with nothing to emit there is nothing to refuse with"
    );
    assert!(ms < 500.0, "the bound stopped holding: {ms} ms");
  });
}

/// Rule order is part of the resource posture: the classic all-pairs fragment bomb never reaches
/// the merge engine.
///
/// Draft 5.5.2.2 refuses it first, exactly as apollo-compiler does — which is why the engine runs
/// last in this validator rather than wherever 5.3.2 falls in the specification's numbering.
#[test]
fn the_all_pairs_fragment_bomb_dies_at_5_5_2_2() {
  let schema = build();
  const FRAGMENTS: usize = 60;

  let mut source = String::from("{ dog { ...f0 } }");
  for index in 0..FRAGMENTS {
    source.push_str(&std::format!(" fragment f{index} on Dog {{ name"));
    for other in 0..FRAGMENTS {
      source.push_str(&std::format!(" ...f{other}"));
    }
    source.push_str(" }");
  }

  // A `First` sink is the server default, and it is what makes the ordering observable: the
  // document is refused before anything expands a fragment.
  let document = parse(&source);
  let mut scratch = Scratch::new();
  let mut sink = First::new();
  let start = Instant::now();
  let invalid = validate_executable(
    &schema,
    &document,
    &mut scratch,
    &Budget::default(),
    &mut sink,
  )
  .expect_err("the spreads are cyclic");
  let ms = start.elapsed().as_secs_f64() * 1e3;
  println!(
    "all-pairs bomb: {} bytes, {FRAGMENTS} fragments, {ms:.3} ms",
    source.len()
  );
  assert_eq!(
    sink.get().expect("a diagnostic").rule(),
    Rule::FragmentSpreadsMustNotFormCycles,
    "the merge engine was reached before the cycle rule refused the document"
  );
  assert!(!invalid.budget_tripped());
  assert!(ms < 500.0, "{ms} ms");

  // And with a collecting sink, which does not stop, the engine does run over the cyclic graph —
  // and terminates, under budget.
  let (tripped, rules, ms) = timed(&schema, &source, &Budget::default());
  println!("all-pairs bomb, collecting sink: {ms:.3} ms, {rules:?}");
  assert!(rules.contains(&Rule::FragmentSpreadsMustNotFormCycles));
  assert!(!tripped, "the engine needed the budget on a cyclic graph");
  assert!(ms < 2_000.0, "{ms} ms");
}

/// Pairing two wide object literals by field name is quadratic, and the budget sees it.
///
/// Matching arguments and object fields by name is a scan, so a field written with a thousand of
/// either is a quadratic comparison. Charging the pair once rather than each scan would leave that
/// quadratic running inside a budgeted engine, which is precisely the hole the budget exists to
/// close — so each scan is charged for its own length, and a hostile literal trips.
#[test]
fn a_wide_literal_comparison_is_charged_for_its_scans() {
  let schema = build();
  const WIDTH: usize = 400;

  let mut payload = String::from("{");
  for index in 0..WIDTH {
    payload.push_str(&std::format!(" f{index}: {index}"));
  }
  payload.push_str(" }");
  // The same field twice behind one response name, so the two literals have to be compared.
  let source = std::format!("{{ x: note(payload: {payload}) x: note(payload: {payload}) }}");

  let (tripped, rules, ms) = timed(&schema, &source, &Budget::default());
  println!(
    "wide literal: {} bytes, {WIDTH} fields each side, {ms:.3} ms",
    source.len()
  );
  assert!(tripped, "the quadratic ran uncharged");
  assert_eq!(rules, [Rule::MergeWorkBudget], "{rules:?}");
  assert!(ms < 500.0, "{ms} ms");

  // Narrow enough to stay inside the bound, the same shape merges.
  let mut payload = String::from("{");
  for index in 0..8 {
    payload.push_str(&std::format!(" f{index}: {index}"));
  }
  payload.push_str(" }");
  let source = std::format!("{{ x: note(payload: {payload}) x: note(payload: {payload}) }}");
  assert_eq!(fired(&schema, &source), []);
}

/// How much legitimate document the shipped default actually clears.
///
/// The design's estimate was "the measured 87 KB hostile grid", taken from apollo's row counting.
/// This crate's work unit is not apollo's — it also charges comparisons, the rows a common-parent
/// partition duplicates, and tree-resolution steps — so the number is measured here rather than
/// inherited, and it is printed so that a change to the accounting shows up as a moved number.
#[test]
fn the_default_work_budget_clears_a_large_ordinary_document() {
  let schema = build();

  // A realistic shape: distinct response names, so every group is a singleton and the engine pays
  // for the document once rather than for every pair in it.
  let mut source = String::from("{ dog {");
  for index in 0..2_000 {
    source.push_str(&std::format!(" f{index}: name"));
  }
  source.push_str(" } }");

  let (tripped, rules, ms) = timed(&schema, &source, &Budget::default());
  println!(
    "ordinary document: {} bytes, 2000 distinct selections, {ms:.3} ms, tripped = {tripped}",
    source.len()
  );
  assert!(
    !tripped,
    "the default work budget refused an ordinary {}-byte document",
    source.len()
  );
  assert_eq!(rules, []);
}
