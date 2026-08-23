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
    Budget, Collect, Context, Count, Diagnostic, First, Ignore, MergeConflict, Rule, RuleSet,
    Schema, Scratch, validate_executable, validate_executable_with,
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

/// The nesting ceiling these fixtures parse under, and it is **not** the default.
///
/// `smear_lexer::limits::MAX_NESTING_DEPTH` is sized against a 2 MiB thread (smear issue #61),
/// which is what a caller who has not thought about it gets. These tests have thought about it:
/// they run on [`on_a_deep_stack`]'s 64 MiB worker precisely so that a 200-level document is a
/// document rather than a stack overflow, and raising the ceiling to match is the API's own answer
/// for a caller who has arranged the stack.
///
/// It is also the ordering that makes the merge budget below testable at all. The parser's ceiling
/// is a **native-stack** bound and the validator's `merge_depth` is a **work** bound, and the two
/// are now ordered the other way round from how they read: at the shipped defaults the parser
/// refuses at 24, long before the validator's 128 has anything to refuse. So `merge_depth` defends
/// a document only a caller who raised this ceiling can produce — which is exactly the caller
/// these tests impersonate.
const FIXTURE_NESTING_CEILING: usize = 512;

fn parse(source: &str) -> ExecutableDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str_with_state(
    source,
    SyntacticLimits::with_max_nesting_depth(FIXTURE_NESTING_CEILING),
  )
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

/// The smallest `merge_work` at which `subject` comes back clean under `rules`, with `warm_with`
/// validated on the same `Scratch` first when there is one.
///
/// A boundary rather than a hand-picked limit: what a document costs is the quantity these fixtures
/// are about, so bisecting for it names the axis instead of one of its consequences. The two
/// assertions at the end pin the boundary the search landed on, so a non-monotone engine cannot
/// hide behind a bisection that happened to agree.
fn least_work_that_clears(
  schema: &Schema,
  subject: &str,
  warm_with: Option<&str>,
  rules: RuleSet,
) -> u32 {
  let subject = parse(subject);
  let prelude = warm_with.map(parse);
  let clears = |limit: u32| {
    let mut scratch = Scratch::new();
    if let Some(prelude) = prelude.as_ref() {
      assert!(
        validate_executable(
          schema,
          prelude,
          &mut scratch,
          &Budget::default(),
          &mut Ignore
        )
        .is_ok(),
        "the prelude must finish, or it is not the state a real reuse leaves"
      );
    }
    let budget = Budget::default().with_merge_work(limit);
    validate_executable_with(schema, &subject, &mut scratch, &budget, rules, &mut Ignore).is_ok()
  };

  let (mut lo, mut hi) = (0u32, Budget::default().merge_work());
  assert!(clears(hi), "the subject does not clear the default budget");
  while lo < hi {
    let mid = lo + (hi - lo) / 2;
    if clears(mid) {
      hi = mid;
    } else {
      lo = mid + 1;
    }
  }
  assert!(clears(lo));
  assert!(lo > 0 && !clears(lo - 1), "{lo} is not a boundary");
  lo
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

/// Switching the bound's own rule off stops the *diagnostic*, not the engine and not the refusal.
///
/// The bound still holds — the work is not done — and the document is still refused. What a caller
/// gives up by filtering the rule out is being told *which* bound stopped the engine; what they
/// must not be given is an `Ok`, because draft 5.3.2 was abandoned partway through and every
/// subtree past the trip is unexamined. `Invalid::budget_tripped` carries the refusal with no
/// diagnostic attached, and `Invalid::emitted` is zero.
///
/// # The plant
///
/// Make `validate_executable_with` return `Ok` whenever nothing was emitted — which is what it did
/// until al8n/smear#196 — and both halves below fail on their `expect_err`: a validator reporting a
/// clean result for a check it gave up on.
#[test]
fn a_refusal_with_its_rule_filtered_out_is_still_a_refusal() {
  on_a_deep_stack(|| {
    let schema = build();
    let budget = Budget::default();

    // The depth bound, with `FieldSelectionMerging` itself switched on.
    let source = deep_query(200);
    let document = parse(&source);
    let mut scratch = Scratch::new();
    let rules = RuleSet::ALL.without(Rule::MergeDepthBudget);
    assert!(rules.contains(Rule::FieldSelectionMerging));

    let start = Instant::now();
    let invalid = validate_executable_with(
      &schema,
      &document,
      &mut scratch,
      &budget,
      rules,
      &mut Ignore,
    )
    .expect_err("the engine abandoned 5.3.2, so the document cannot be reported clean");
    let ms = start.elapsed().as_secs_f64() * 1e3;
    println!("depth bound with the rule disabled: {ms:.3} ms");
    assert!(invalid.budget_tripped(), "{invalid}");
    assert_eq!(
      invalid.emitted(),
      0,
      "the rule was filtered out; there was nothing to emit"
    );
    assert!(
      invalid.to_string().contains("resource budget exceeded"),
      "{invalid}"
    );
    assert!(
      !invalid.stopped(),
      "`stopped` is the *sink's* answer, and no diagnostic reached the sink here. It reading \
       `true` would mean the two flags had been folded into one, and a caller asking `stopped` \
       whether the whole document was looked at would be right by accident on this verdict and \
       wrong on every other refusal. al8n/smear#196"
    );
    assert!(ms < 500.0, "the bound stopped holding: {ms} ms");

    // The work bound, the same way, with a collecting sink so the empty sink is observed rather
    // than assumed.
    let source = wide_query(20_000);
    let document = parse(&source);
    let rules = RuleSet::ALL.without(Rule::MergeWorkBudget);
    assert!(rules.contains(Rule::FieldSelectionMerging));
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    let invalid =
      validate_executable_with(&schema, &document, &mut scratch, &budget, rules, &mut sink)
        .expect_err("the work bound refused; the merge rule was never finished");
    assert!(invalid.budget_tripped(), "{invalid}");
    assert_eq!(invalid.emitted(), 0);
    assert!(
      collected.is_empty(),
      "{:?}",
      collected.first().map(Diagnostic::rule)
    );

    // The control, and the reason this is not simply "any missing rule refuses": with draft 5.3.2
    // and both of its bounds out of the set the engine never runs, so there is nothing to abandon
    // and the same document under the same budget is clean.
    let quiet = RuleSet::ALL
      .without(Rule::FieldSelectionMerging)
      .without(Rule::MergeWorkBudget)
      .without(Rule::MergeDepthBudget);
    assert!(
      validate_executable_with(
        &schema,
        &document,
        &mut scratch,
        &budget,
        quiet,
        &mut Ignore
      )
      .is_ok(),
      "a rule nobody asked to run cannot refuse a document"
    );
  });
}

/// The same document, budget and rule set must get the same verdict on a reused working set as on
/// a fresh one.
///
/// [`Scratch`] is the caller's, reused across requests by design. Its bucket tables used to be
/// emptied with a `fill` that kept their *length*, and the length is what the interner and the
/// merge memo charge their growth against — so a cold run paid relinks at 1, 65, 129 … and the
/// identical run behind a larger request paid none. With a `merge_work` between the two totals the
/// verdict flipped on history alone, which is a server answering one client differently because of
/// what the last one sent.
///
/// # How this is measured
///
/// Not at a hand-picked limit, which would be a number to re-tune rather than a property. The
/// smallest `merge_work` at which the subject comes back clean is binary-searched on each side,
/// and the two are required to be equal — so the test names the defect's axis instead of one of
/// its consequences.
///
/// **The plant.** Put the `fill(NONE)` back in `Names::reset` or `Scratch::reset` and the warm
/// side settles lower than the cold one by exactly the relinks the warm run skipped.
#[test]
fn the_verdict_does_not_depend_on_what_the_last_request_left_behind() {
  on_a_deep_stack(the_verdict_does_not_depend_on_history);
}

/// The body of [`the_verdict_does_not_depend_on_what_the_last_request_left_behind`], on a stack the
/// nested fixtures fit: the merge memo only doubles its bucket table past 64 distinct sets, and 64
/// distinct sets means 64 levels of response shape.
fn the_verdict_does_not_depend_on_history() {
  let schema = build();

  // Wide enough to grow the interner's bucket table past two doublings, deep enough to grow the
  // merge memo's past one, and clean under the default budget.
  fn document_of(names: usize, depth: usize) -> String {
    let mut source = String::from("{ dog {");
    for index in 0..names {
      source.push_str(&std::format!(" f{index}: name"));
    }
    source.push_str(" } ");
    for _ in 0..depth {
      source.push_str(" nest {");
    }
    source.push_str(" leaf ");
    for _ in 0..depth {
      source.push_str(" } ");
    }
    source.push('}');
    source
  }

  let prelude = document_of(500, 120);
  let subject = document_of(100, 70);
  assert_eq!(fired(&schema, &prelude), [], "the prelude must be clean");
  assert_eq!(fired(&schema, &subject), [], "the subject must be clean");

  // The prelude really does leave a larger working set behind, so "warm" is not a word for
  // "identical".
  let mut scratch = Scratch::new();
  let empty = scratch.capacity();
  assert!(
    validate_executable(
      &schema,
      &parse(&prelude),
      &mut scratch,
      &Budget::default(),
      &mut Ignore
    )
    .is_ok()
  );
  assert!(
    scratch.capacity() > empty,
    "the prelude grew nothing: {empty} -> {}",
    scratch.capacity()
  );

  let cold = least_work_that_clears(&schema, &subject, None, RuleSet::ALL);
  let warm = least_work_that_clears(&schema, &subject, Some(&prelude), RuleSet::ALL);
  println!("least merge_work that clears the subject: cold {cold}, warm {warm}");
  assert_eq!(
    cold, warm,
    "the same document needs {cold} units on a fresh working set and {warm} on a reused one"
  );
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

/// Pairing two wide object literals by field name is linear in their width, and the width is still
/// charged.
///
/// # What this fixture used to say, and why it stopped saying it
///
/// It pinned the pairing as *quadratic and charged*: four hundred fields a side cost
/// `400 · 401 / 2` scan steps, and the fixture's whole assertion was that the default budget
/// tripped on those steps. Charging a quadratic is not the same as being entitled to run one. The
/// steps are work whose count the client chooses, four hundred fields is a seven-kilobyte literal,
/// and a budget spent on the *shape* of the comparison is a budget an honest document cannot spend
/// on its own size. al8n/smear#196 indexes the right-hand side once for the node, so the pairing is
/// `w` lookups of about one probe apiece and the same literal merges.
///
/// So the property moved with the code, and what is pinned now is the **class**. Doubling the
/// width roughly doubles the cost where the old pairing quadrupled it, and a literal wide enough is
/// still refused — the bound did not go away, it stopped being quadratic. A gate whose subject a
/// repair dissolves has to say so out loud; leaving the old assertion to be deleted quietly is how
/// a suite ends up green over a property nobody is testing any more.
#[test]
fn a_wide_literal_comparison_is_linear_in_its_width() {
  /// Two identical object literals of `width` distinct fields behind one response name, so the two
  /// have to be paired.
  fn document(width: usize) -> String {
    let mut payload = String::from("{");
    for index in 0..width {
      payload.push_str(&std::format!(" f{index}: {index}"));
    }
    payload.push_str(" }");
    std::format!("{{ x: note(payload: {payload}) x: note(payload: {payload}) }}")
  }

  let schema = build();
  let rules = RuleSet::EMPTY
    .with(Rule::FieldSelectionMerging)
    .with(Rule::MergeWorkBudget)
    .with(Rule::MergeDepthBudget);

  // The old fixture's own subject, which now merges.
  let source = document(400);
  let (tripped, fired, ms) = timed(&schema, &source, &Budget::default());
  println!(
    "wide literal: {} bytes, 400 fields each side, {ms:.3} ms, tripped = {tripped}",
    source.len()
  );
  assert!(
    !tripped,
    "a seven-kilobyte literal of four hundred distinct fields is refused: {fired:?}"
  );
  assert_eq!(fired, [], "{fired:?}");
  assert!(ms < 500.0, "{ms} ms");

  // Doubling the width. A quadratic pairing would take about four times the units; an indexed one
  // takes about two, and the separator is three because the fixture also carries a fixed cost that
  // does not double.
  let narrow = least_work_that_clears(&schema, &document(64), None, rules);
  let wide = least_work_that_clears(&schema, &document(128), None, rules);
  println!("object literal: {narrow} units at 64 fields a side, {wide} at 128");
  assert!(
    wide > narrow,
    "the width is not charged at all: {narrow} against {wide}"
  );
  assert!(
    wide < narrow * 3,
    "doubling the width cost {narrow} units then {wide}, which is the quadratic this fixture used \
     to pin rather than the linear pairing that replaced it"
  );

  // And the bound is still a bound: linear in the width is not free in it.
  let source = document(20_000);
  let (tripped, fired, ms) = timed(&schema, &source, &Budget::default());
  println!(
    "very wide literal: {} bytes, 20000 fields each side, {ms:.3} ms",
    source.len()
  );
  assert!(tripped, "{fired:?}");
  assert_eq!(fired, [Rule::MergeWorkBudget], "{fired:?}");
  assert!(ms < 500.0, "{ms} ms");
}

/// A part of identical selections is linear in how many arguments they carry, not quadratic.
///
/// # Hashing the names was half the repair, and the half that is invisible on long ones
///
/// Round three of al8n/smear#196 stopped a lookup being charged for bytes no candidate reads. It
/// left the lookup **walking every candidate from the front**, so two lists of `n` distinct names
/// in the same order cost `1 + 2 + … + n` steps a direction and a pair cost `n(n + 1)`. On the
/// thirty-two five-hundred-byte names that round measured, the byte term dominated and the step
/// term was invisible; on many *short* names it is the only term there is.
///
/// Five identical selections of a hundred and twenty-eight short arguments is
/// `4 · 128 · 129 = 66,048` steps against a default budget of 65,536 — spent before a byte is
/// hashed or a value compared, on a few kilobytes of the sort of document a query generator emits
/// without thinking about it. Two rounds, two shapes, one lesson: a repair written against the
/// last defect's proportions is a repair against one point of a surface.
///
/// **The plants.** Give `Validator::index` one bucket for any width — `buckets_for(count).min(1)`
/// — and every name lands in the same chain, so the lookups walk the list from the front again and
/// the hundred-and-twenty-eight-argument row is refused. Drop `Scratch::merge_indexed` and the
/// left side is rebuilt for every pair instead of once for the part, which is three more builds
/// here: the boundary below rises from 5,684 units to 6,836.
#[test]
fn a_part_of_identical_selections_is_not_quadratic_in_its_arguments() {
  /// Arguments a selection, and the reviewer's own number.
  const WIDTH: usize = 128;

  /// `members` identical selections behind one response name, each with `width` short arguments.
  fn document(members: usize, width: usize) -> String {
    let mut args = String::new();
    for index in 0..width {
      args.push_str(&std::format!(" a{index}: 1"));
    }
    let mut body = String::new();
    for _ in 0..members {
      body.push_str(&std::format!(" x: name({args} )"));
    }
    std::format!("{{ dog {{{body} }} }}")
  }

  on_a_deep_stack(|| {
    let schema = build();
    let rules = RuleSet::EMPTY
      .with(Rule::FieldSelectionMerging)
      .with(Rule::MergeWorkBudget)
      .with(Rule::MergeDepthBudget);
    let default = Budget::default().merge_work();

    let source = document(5, WIDTH);
    let charged = least_work_that_clears(&schema, &source, None, rules);
    println!(
      "five identical selections: {} bytes, {WIDTH} arguments each, {charged} units against a \
       default of {default}",
      source.len()
    );
    assert!(
      charged <= default,
      "a valid {}-byte document costs {charged} units against a default merge_work of {default}: \
       four pairings of two identical {WIDTH}-argument lists is {} lookup steps when each lookup \
       starts at the front of the list",
      source.len(),
      4 * WIDTH * (WIDTH + 1)
    );

    // Doubling the arguments doubles the cost, rather than quadrupling it.
    let narrow = least_work_that_clears(&schema, &document(5, 64), None, rules);
    let wide = least_work_that_clears(&schema, &document(5, 128), None, rules);
    println!("arguments a side: {narrow} units at 64, {wide} at 128");
    assert!(wide > narrow, "the width is not charged at all: {narrow}");
    assert!(
      wide < narrow * 3,
      "doubling the argument count cost {narrow} units then {wide}, which is a scan from the front \
       of the list rather than a lookup into it"
    );
  });
}

/// Every comparison draft 5.3.2 makes over a **spelling** is charged for that spelling's length.
///
/// # Why a second gate, when the width one above is already one
///
/// `a_wide_literal_comparison_is_charged_for_its_scans` prices the *entries*: pairing `n` object
/// fields by name is `n²` comparisons and the ledger records `n²`. It says nothing about `L`, the
/// length of what each of those comparisons reads, and `L` is a number the client writes with no
/// ceiling in draft §2.1.9 or §2.9. Charging entries while running bytes recorded `Θ(n²)` and ran
/// `Θ(n² · L)` — the defect al8n/smear#196 opened against in the interner, at three more sites
/// inside the engine that consumes it.
///
/// Three sites, and one axis each, so a repair that reaches one and not the others cannot be green:
///
/// - **argument names**, paired by `same_arguments`' two scans;
/// - **scalar values**, compared by `same_value`'s `shallow_equal` on their source spellings;
/// - **object-literal field names**, paired by `same_value`'s inner scan.
///
/// Each row is the *same structure* at two lengths — same widths, same nesting, same number of
/// comparisons — so the only thing that moves between the two documents is how many bytes each
/// comparison reads. A ledger over entries reads the same number twice.
///
/// # Only 5.3.2 is switched on, which is the point about reachability
///
/// The argument names below are ones the schema does not define, and 5.4.1 would ordinarily refuse
/// the document before the merge engine ever compared them. It does not have to: `same_arguments`
/// runs on what the *document* wrote, so a caller running `FieldSelectionMerging` alone — or one
/// whose sink does not stop — reaches this scan with no other rule pre-empting it. The rule set
/// here is that caller.
///
/// **The plants.** Replace `shallow_units` with the entry count it replaced and the scalar row's
/// two boundaries become equal. Delete *both* of the `byte_units` charges a looked-up name passes
/// through — the one `Validator::index` takes in front of hashing it, and the one
/// `Validator::find` takes in front of the `memcmp` a probe that agrees on hash and length goes on
/// to make — and the other two rows stop doubling, at 269 units against 277 and 221 against 225.
/// Both charges, because either alone still leaves a term in `L`: the two are one repair, and the
/// second is what makes the first affordable.
#[test]
fn a_comparison_over_a_spelling_is_charged_for_its_length() {
  /// Selections per side. Small enough that the padded documents still clear the **default**
  /// budget, which is what keeps the boundary search's upper end honest.
  const WIDTH: usize = 24;
  /// The two spelling lengths every row is measured at; the second is `PAD` bytes longer.
  const PAD: usize = 200;

  /// `WIDTH` names of one length, sharing a `pad`-byte prefix and differing only at the end — so a
  /// `memcmp` between any two of them runs to the last byte rather than stopping at the first.
  fn names(pad: usize) -> Vec<String> {
    (0..WIDTH)
      .map(|index| std::format!("{}{index:04}", "z".repeat(pad + 1)))
      .collect()
  }

  /// The same arguments on both sides, in opposite orders, so every scan runs the whole width.
  fn arguments(pad: usize) -> String {
    let names = names(pad);
    let mut left = String::new();
    let mut right = String::new();
    for name in &names {
      left.push_str(&std::format!(" {name}: 1"));
    }
    for name in names.iter().rev() {
      right.push_str(&std::format!(" {name}: 1"));
    }
    std::format!("{{ dog {{ x: name({left} ) x: name({right} ) }} }}")
  }

  /// The same object literal twice, its fields in opposite orders.
  fn object_fields(pad: usize) -> String {
    let names = names(pad);
    let mut left = String::new();
    let mut right = String::new();
    for name in &names {
      left.push_str(&std::format!(" {name}: 1"));
    }
    for name in names.iter().rev() {
      right.push_str(&std::format!(" {name}: 1"));
    }
    std::format!("{{ x: note(payload: {{{left} }}) x: note(payload: {{{right} }}) }}")
  }

  /// `WIDTH` list elements of one string literal, compared element by element.
  fn scalar_values(pad: usize) -> String {
    let body = "z".repeat(pad + 1);
    let mut list = String::new();
    for _ in 0..WIDTH {
      list.push_str(&std::format!(" \"{body}\""));
    }
    std::format!("{{ x: note(payload: [{list} ]) x: note(payload: [{list} ]) }}")
  }

  on_a_deep_stack(|| {
    let schema = build();
    // Draft 5.3.2 and the two bounds that stop it, and nothing else: the point is that this scan is
    // reached without 5.4.1 or 5.6.x having a say.
    let rules = RuleSet::EMPTY
      .with(Rule::FieldSelectionMerging)
      .with(Rule::MergeWorkBudget)
      .with(Rule::MergeDepthBudget);

    for (label, build_row) in [
      ("argument names", arguments as fn(usize) -> String),
      ("object-literal field names", object_fields),
      ("scalar values", scalar_values),
    ] {
      let short = build_row(0);
      let long = build_row(PAD);

      // Both documents merge; what separates them is only what merging costs.
      for source in [&short, &long] {
        let mut scratch = Scratch::new();
        assert!(
          validate_executable_with(
            &schema,
            &parse(source),
            &mut scratch,
            &Budget::default(),
            rules,
            &mut Ignore
          )
          .is_ok(),
          "{label}: the fixture must merge under the default budget, or the boundary search below \
           has nothing to search for"
        );
      }

      let least_short = least_work_that_clears(&schema, &short, None, rules);
      let least_long = least_work_that_clears(&schema, &long, None, rules);
      println!(
        "{label}: {least_short} units at {} bytes a name, {least_long} at {} bytes",
        1 + 4,
        PAD + 1 + 4
      );
      // Twice, and not merely more. `PAD` adds twenty-five `byte_units` to every name, so a
      // ledger that reads the spellings moves by thousands of units here — while a bucket table
      // hands two different name sets slightly different collision counts, so "more" can be eight
      // units of probe noise with no term in `L` at all. Asking for a doubling is asking whether
      // the bytes are in the total, rather than whether anything is.
      assert!(
        least_long >= least_short * 2,
        "{label}: the same {WIDTH} comparisons cost {least_short} units over short spellings and \
         {least_long} over spellings {PAD} bytes longer. A total that does not even double has no \
         term in the spelling: what separates the two is how the names happened to land in their \
         buckets, not the bytes the comparisons read"
      );
    }
  });
}

/// A scan's charge bounds the bytes the scan reads, and does not bound a worst case it never runs.
///
/// # One direction is half a repair, and half a repair is the other denial of service
///
/// [`a_comparison_over_a_spelling_is_charged_for_its_length`] pins the first direction: recorded
/// units cannot understate performed byte work, or a caller buys `Θ(n² · L)` of `memcmp` with
/// `Θ(n²)` of ledger. Taken alone it also admits a charge of `entries × wanted`, taken before the
/// scan — and that is not what the scan does. Matching by name returns at the first hit and a
/// byte-slice comparison settles at the first difference, so a whole-worst-case charge prices
/// bytes no candidate ever reads, and a ledger that overcharges refuses honest documents rather
/// than hostile ones.
///
/// The honest half below is what that cost. Thirty-two arguments of five hundred and twelve valid
/// bytes, written twice with the same spelling and **distinct first bytes**, settle every one of
/// the thirty-one non-matching candidates of every lookup without reading a byte — and the
/// pre-scan product charged `2 × 32 × 32 × 65 = 133,120` units against a default `merge_work` of
/// 65,536. Forty kilobytes of valid document, refused for work nobody performs.
///
/// The repair is the pair [`Names::intern`](smear::validator::Scratch) already takes: a stored
/// hash beside a length makes a non-matching candidate a two-integer rejection that reads no
/// bytes, so the byte charge is reached only where the bytes are about to be. al8n/smear#196.
///
/// **The plants.** Charge either of `same_arguments`' lookups `entries × wanted` in front of
/// itself — the product this replaced — and the honest half is refused. Delete both of the
/// `byte_units` charges a looked-up name passes through, the one `Validator::index` takes in front
/// of hashing it and the one `Validator::find` takes in front of a matching probe's `memcmp`, and
/// the hostile half is served.
#[test]
fn a_scan_is_not_charged_for_bytes_no_candidate_reads() {
  /// Arguments a side in the honest half.
  const WIDTH: usize = 32;
  /// Bytes a name in the honest half.
  const LEN: usize = 512;
  /// Arguments a side in the hostile half.
  const HOSTILE_WIDTH: usize = 200;
  /// Bytes a name in the hostile half.
  const HOSTILE_LEN: usize = 4096;

  /// `width` valid names of `len` bytes, no two sharing a first byte.
  fn distinct(width: usize, len: usize) -> Vec<String> {
    const LEAD: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEF";
    assert!(width <= LEAD.len() && len > 0);
    (0..width)
      .map(|index| {
        let mut name = String::from(LEAD[index] as char);
        name.push_str(&"z".repeat(len - 1));
        name
      })
      .collect()
  }

  /// `width` valid names of `len` bytes, sharing every byte but the last four.
  fn prefixed(width: usize, len: usize) -> Vec<String> {
    assert!(width <= 10_000 && len > 4);
    (0..width)
      .map(|index| std::format!("{}{index:04}", "z".repeat(len - 4)))
      .collect()
  }

  /// Two selections behind one response name, holding `names` as arguments on both sides.
  fn document(names: &[String], reversed: bool) -> String {
    let mut left = String::new();
    for name in names {
      left.push_str(&std::format!(" {name}: 1"));
    }
    let mut right = String::new();
    if reversed {
      for name in names.iter().rev() {
        right.push_str(&std::format!(" {name}: 1"));
      }
    } else {
      right.push_str(&left);
    }
    std::format!("{{ dog {{ x: name({left} ) x: name({right} ) }} }}")
  }

  /// The least `merge_work` at which `source` comes back clean, searched past the shipped default.
  ///
  /// [`least_work_that_clears`] cannot measure this one: its upper end *is* the default budget, and
  /// what is under test here is a document that the defect puts above it.
  fn least(schema: &Schema, source: &str, rules: RuleSet) -> u32 {
    let document = parse(source);
    let clears = |limit: u32| {
      let mut scratch = Scratch::new();
      let budget = Budget::default().with_merge_work(limit);
      validate_executable_with(schema, &document, &mut scratch, &budget, rules, &mut Ignore).is_ok()
    };
    let (mut lo, mut hi) = (0u32, 1u32 << 24);
    assert!(clears(hi), "the subject does not clear {hi} units");
    while lo < hi {
      let mid = lo + (hi - lo) / 2;
      if clears(mid) {
        hi = mid;
      } else {
        lo = mid + 1;
      }
    }
    assert!(clears(lo));
    assert!(lo > 0 && !clears(lo - 1), "{lo} is not a boundary");
    lo
  }

  on_a_deep_stack(|| {
    let schema = build();
    // Draft 5.3.2 and its two bounds alone, for the reason
    // `a_comparison_over_a_spelling_is_charged_for_its_length` gives: this scan runs on what the
    // document wrote, so no rule about whether the schema knows these names pre-empts it.
    let rules = RuleSet::EMPTY
      .with(Rule::FieldSelectionMerging)
      .with(Rule::MergeWorkBudget)
      .with(Rule::MergeDepthBudget);
    let default = Budget::default().merge_work();

    let honest = document(&distinct(WIDTH, LEN), false);
    let charged = least(&schema, &honest, rules);
    println!(
      "honest: {} bytes, {WIDTH} arguments of {LEN} bytes with distinct first bytes, {charged} \
       units against a default of {default}",
      honest.len()
    );
    assert!(
      charged <= default,
      "a valid {}-byte document costs {charged} units against a default merge_work of {default}: \
       the ledger charged {WIDTH} whole-name comparisons for every lookup and every lookup settles \
       {} of them on the first byte",
      honest.len(),
      WIDTH - 1
    );
    let mut scratch = Scratch::new();
    assert!(
      validate_executable_with(
        &schema,
        &parse(&honest),
        &mut scratch,
        &Budget::default(),
        rules,
        &mut Ignore
      )
      .is_ok(),
      "the honest half must serve under the shipped default, not merely under {charged}"
    );

    // And the shape the charge exists for is still refused: the same names in reverse order, all
    // sharing every byte but the last four, so no lookup settles early and the comparison the
    // ledger has to see is the whole of `Θ(n² · L)`.
    let hostile = document(&prefixed(HOSTILE_WIDTH, HOSTILE_LEN), true);
    let mut scratch = Scratch::new();
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    let start = Instant::now();
    let verdict = validate_executable_with(
      &schema,
      &parse(&hostile),
      &mut scratch,
      &Budget::default(),
      rules,
      &mut sink,
    );
    let ms = start.elapsed().as_secs_f64() * 1e3;
    let mut fired: Vec<_> = collected.iter().map(Diagnostic::rule).collect();
    fired.sort_unstable();
    fired.dedup();
    println!(
      "hostile: {} bytes, {HOSTILE_WIDTH} arguments of {HOSTILE_LEN} bytes in reverse order, \
       {ms:.3} ms",
      hostile.len()
    );
    assert!(
      verdict.as_ref().err().is_some_and(|i| i.budget_tripped()),
      "the reverse-order long-prefix comparison served under the default budget"
    );
    assert_eq!(fired, [Rule::MergeWorkBudget], "{fired:?}");
    assert!(ms < 500.0, "{ms} ms");
  });
}

/// A comparison that two lengths settle is not charged for bytes neither side reads.
///
/// # An over-charge does not merely refuse; it answers the wrong question
///
/// `shallow_units` charged `byte_units` of the *shorter* spelling, on the reasoning that a slice
/// comparison cannot read past it. True, and it prices a comparison this code does not make:
/// `[u8] == [u8]` settles unequal lengths before it reads a byte of either side. So the two string
/// literals below — 524,288 bytes against 524,289 — cost one integer compare and were charged
/// 65,537 units, which is the whole shipped `merge_work` and one more.
///
/// What that bought was not safety, and this is the half that makes it worth a fixture of its own.
/// The document has a **real** draft 5.3.2 conflict: the two literals differ, so `x` cannot merge.
/// The over-charge spent the budget before the comparison could say so, and the caller was handed
/// a resource refusal in place of the finding — told to raise a knob when what they have is a
/// document that will not merge. With `MergeWorkBudget` outside the rule set it degrades further
/// still, to an `Err` carrying nothing at all. al8n/smear#196.
///
/// **The plant.** Charge `byte_units(min(left, right))` again and this reads `[MergeWorkBudget]`
/// where the conflict should be.
#[test]
fn a_comparison_two_lengths_settle_is_not_charged_for_either_spelling() {
  /// Long enough that one `byte_units` of it exceeds the shipped `merge_work` on its own.
  const LEN: usize = 524_288;

  on_a_deep_stack(|| {
    let schema = build();
    let left = "a".repeat(LEN);
    let right = "a".repeat(LEN + 1);
    let source = std::format!("{{ x: note(payload: \"{left}\") x: note(payload: \"{right}\") }}");

    let (tripped, fired, ms) = timed(&schema, &source, &Budget::default());
    println!(
      "length-decided comparison: {} bytes, spellings of {LEN} and {}, {ms:.3} ms",
      source.len(),
      LEN + 1
    );
    assert!(
      !tripped,
      "an `O(1)` decision exhausted the merge budget: {fired:?}"
    );
    assert_eq!(
      fired,
      [Rule::FieldSelectionMerging],
      "the document's finding is that the two literals differ, and that is what the caller must be \
       told: {fired:?}"
    );
    assert_eq!(
      conflicts(&schema, &source),
      [MergeConflict::Arguments],
      "and it is the arguments that conflict, not the shapes"
    );
    assert!(ms < 500.0, "{ms} ms");
  });
}

/// A merge bound governs the merge engine's work, and with no rule to start the engine there is
/// none.
///
/// # The contract sentence this replaces, and why it was wrong
///
/// `validate_executable_with` said `budget` is enforced whatever the rule set contains. That is
/// true of a bound whose passes run unconditionally. It is not true of these two:
/// [`Budget::merge_work`] and [`Budget::merge_depth`] are spent by draft 5.3.2's engine, and
/// `check_field_merging` returns before it builds anything when
/// [`Rule::FieldSelectionMerging`], [`Rule::MergeDepthBudget`] and [`Rule::MergeWorkBudget`] are
/// all absent. Nothing is expanded, interned or compared — `build_merge_index` has one caller and
/// it is past that gate, and `Scratch::names` has two touchers and both are inside it.
///
/// **So the bound is vacuous rather than disabled, and the difference is the whole point.** No
/// expensive thing was let through; no expensive thing happened. A caller using `budget` as an
/// admission policy — "refuse anything that would cost more than this" — is holding the wrong
/// instrument, because the cost it prices is the cost of a rule they switched off.
///
/// # The assertion that decides it
///
/// The old sentence requires the middle case below to be `Err`. It is `Ok`, so the sentence was
/// the wrong half. And the last case is what separates vacuity from suppression: a bound that
/// governed work that happens would have to change *something* between zero and the shipped
/// default. Nothing moves — not the verdict, not the working set — because there is no work in
/// between for it to govern. al8n/smear#196.
#[test]
fn a_merge_bound_governs_the_engine_and_is_vacuous_without_it() {
  on_a_deep_stack(|| {
    let schema = build();
    // Wide enough that the engine cannot finish it under any small budget.
    let source = wide_query(20_000);
    let document = parse(&source);
    let none = Budget::default().with_merge_work(0);

    /// The verdict and the working set the run leaves behind.
    fn run(
      schema: &Schema,
      document: &ExecutableDocument<&str>,
      budget: &Budget,
      rules: RuleSet,
    ) -> (bool, bool, usize) {
      let mut scratch = Scratch::new();
      let verdict =
        validate_executable_with(schema, document, &mut scratch, budget, rules, &mut Ignore);
      let tripped = verdict
        .as_ref()
        .err()
        .is_some_and(smear::validator::Invalid::budget_tripped);
      (verdict.is_ok(), tripped, scratch.capacity())
    }

    // The bound is real where the engine runs: draft 5.3.2 started, and a `merge_work` of zero
    // cannot pay for the first thing it does.
    let started = RuleSet::EMPTY
      .with(Rule::FieldSelectionMerging)
      .with(Rule::MergeWorkBudget);
    let (ok, tripped, _) = run(&schema, &document, &none, started);
    assert!(!ok && tripped, "a zero merge budget refused nothing");

    // And with every merge rule absent it is served. **This is the assertion the old contract
    // sentence forbade**: it promised `budget` was enforced whatever the set contained, and the
    // document below is one the same budget refuses two lines up.
    let (ok, tripped, cold) = run(&schema, &document, &none, RuleSet::EMPTY);
    assert!(
      ok && !tripped,
      "an empty rule set was refused by a bound over an engine it never started; the bound is not \
       an admission policy and cannot be made into one by narrowing"
    );

    // Vacuous and not suppressed. A bound over work that happens moves something when it moves
    // from nothing to the shipped default; this one moves neither the verdict nor the working set,
    // because between the two there is no work for it to price.
    let (ok, tripped, warm) = run(&schema, &document, &Budget::default(), RuleSet::EMPTY);
    assert!(ok && !tripped);
    assert_eq!(
      cold, warm,
      "the working set differs between a zero merge budget and the shipped one under a rule set \
       that starts no merge engine, so something in between was being priced after all and the \
       sentence above is a rationalisation"
    );
  });
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
