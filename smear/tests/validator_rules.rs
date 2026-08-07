//! Draft §5 executable-document validation: the per-rule liveness floor, and what it does not
//! cover.
//!
//! # The floor
//!
//! [`liveness_floor`] iterates `Rule::ALL` and asserts that every rule has an entry in
//! [`FIXTURES`] holding a document that makes it **fire**, together with the **exact** set of
//! rules that document produces, and a valid twin that produces none. A rule added without a
//! document that exercises it fails; a document that starts tripping a second rule fails; a rule
//! that stops firing fails. Thirty rules and a corpus that never reaches twenty of them is the
//! failure this suite exists to make impossible, so the census is mechanical rather than
//! reviewed-in.
//!
//! Draft 5.3.2's own cases, and the two resource bounds under a hostile document rather than a
//! lowered knob, live in `validator_merge.rs`.
//!
//! Two further gates ride the same table: [`every_rule_is_independently_addressable`] runs each
//! fixture with only its own rule enabled, proving the `RuleSet` door is real and that no rule
//! depends on another having run; and [`every_valid_twin_is_valid_under_every_rule`] runs the
//! twins with the full set, which is what makes "produces none" mean something.
//!
//! # Where the table lives
//!
//! `tests/support/validator_corpus.rs`, since `validator_lossless.rs` reads the same thirty
//! documents to compare the syntactic and lossless doors over them. The floor below is still what
//! says a rule has an entry there, so the corpus and the census did not come apart in the move.

// Every fixture and assertion below calls into `smear::validator`, which does not exist in the
// crate's API surface with the feature off. This file did not gate itself to that dependency —
// the same defect `validator_schema.rs` carried until #100 — so it was a hard `E0433` compile
// error under any feature selection that excludes `validator`, including the crate's own default
// features. `cargo test -p smear --no-run` in CI now compiles exactly that selection.
#![cfg(feature = "validator")]
#![allow(missing_docs)]

use std::vec::Vec;

use smear::{
  lexer::tokora::{Parse as _, Parser, SimpleSpan},
  parser::graphql::{
    GraphQL,
    ast::{
      Argument, ArgumentList, Described, ExecutableDefinition, ExecutableDocument, Field,
      InputValue, Name, Object, ObjectField, OperationDefinition, Selection, SelectionSet,
      TypeSystemDocument,
    },
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{
    Budget, Collect, Context, Count, Diagnostic, First, Ignore, Rule, RuleSet, Schema, Scratch,
    validate_executable, validate_executable_with,
  },
};

// The rule corpus, shared with `validator_lossless.rs`. See that module's header for what the
// second reader does with it.
//
// The module also carries the draft §3 refusal corpus, which this file has no use for and which
// would be a `dead_code` denial under CI's `-Dwarnings`. Allowed at the include, as the module's
// three other readers do, rather than on an item of a table this file does not own.
#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

use corpus::{FIXTURES, SCHEMA};

// ---------------------------------------------------------------------------------------------
// harness
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

fn parse(source: &str) -> ExecutableDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .unwrap_or_else(|errors| panic!("fixture query does not parse: {errors:?}\n---\n{source}"))
}

/// Validates `source` against `schema` with every rule on, returning the diagnostics.
fn diagnose<'a>(schema: &Schema, source: &'a str) -> Vec<Diagnostic<&'a str>> {
  diagnose_with(schema, source, RuleSet::ALL)
}

fn diagnose_with<'a>(schema: &Schema, source: &'a str, rules: RuleSet) -> Vec<Diagnostic<&'a str>> {
  diagnose_full(schema, source, rules, &Budget::default())
}

fn diagnose_full<'a>(
  schema: &Schema,
  source: &'a str,
  rules: RuleSet,
  budget: &Budget,
) -> Vec<Diagnostic<&'a str>> {
  let document = parse(source);
  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let result = validate_executable_with(schema, &document, &mut scratch, budget, rules, &mut sink);
  assert_eq!(
    result.is_err(),
    !collected.is_empty(),
    "the verdict and the diagnostics disagree\n---\n{source}"
  );
  collected
}

/// The set of rules a document fires, sorted and deduplicated.
fn fired(schema: &Schema, source: &str) -> Vec<Rule> {
  fired_under(schema, source, &Budget::default())
}

fn fired_under(schema: &Schema, source: &str, budget: &Budget) -> Vec<Rule> {
  let mut rules: Vec<_> = diagnose_full(schema, source, RuleSet::ALL, budget)
    .iter()
    .map(Diagnostic::rule)
    .collect();
  rules.sort_unstable();
  rules.dedup();
  rules
}

fn sorted(rules: &[Rule]) -> Vec<Rule> {
  let mut rules = rules.to_vec();
  rules.sort_unstable();
  rules.dedup();
  rules
}

// ---------------------------------------------------------------------------------------------
// the floor
// ---------------------------------------------------------------------------------------------

/// Rules with no fixture, each excused in writing.
///
/// The list is empty and is expected to stay that way. It exists so that an excuse has to be
/// written down rather than implied by a rule quietly missing from the table.
const UNFIREABLE: &[(Rule, &str)] = &[];

#[test]
fn liveness_floor() {
  let schema = build(SCHEMA);

  let mut missing = Vec::new();
  for rule in Rule::ALL {
    let excused = UNFIREABLE.iter().any(|(excused, _)| excused == rule);
    let fixtured = FIXTURES.iter().any(|fixture| fixture.rule == *rule);
    if !fixtured && !excused {
      missing.push(rule);
    }
    assert!(
      !(fixtured && excused),
      "{rule:?} is both excused and fixtured; delete the excuse"
    );
  }
  assert!(
    missing.is_empty(),
    "these rules have no document that makes them fire: {missing:?} — add one to FIXTURES, or \
     record a written reason in UNFIREABLE"
  );

  // The census must not pass vacuously.
  assert!(
    Rule::ALL.len() >= 31,
    "read only {} rules; the enumeration is wrong, not the fixtures",
    Rule::ALL.len()
  );

  for fixture in FIXTURES {
    let schema = match fixture.schema {
      Some(sdl) => build(sdl),
      None => build(SCHEMA),
    };
    let _ = &schema;

    let budget = fixture.budget.unwrap_or_default();
    let actual = fired_under(&schema, fixture.invalid, &budget);
    assert_eq!(
      actual,
      sorted(fixture.fires),
      "the fixture for {:?} fired {actual:?}\n---\n{}",
      fixture.rule,
      fixture.invalid
    );
    assert!(
      actual.contains(&fixture.rule),
      "the fixture for {:?} did not fire it\n---\n{}",
      fixture.rule,
      fixture.invalid
    );

    let twin = fired_under(&schema, fixture.valid, &budget);
    assert!(
      twin.is_empty(),
      "the valid twin for {:?} fired {twin:?}\n---\n{}",
      fixture.rule,
      fixture.valid
    );
  }
  let _ = schema;
}

/// Every rule can be selected on its own, and firing it needs no other rule to have run.
///
/// This is the property that makes `RuleSet` a real door rather than a filter: a consumer that
/// asks for one rule gets exactly that rule's diagnostics, from a document that also violates
/// others.
#[test]
fn every_rule_is_independently_addressable() {
  for fixture in FIXTURES {
    let schema = match fixture.schema {
      Some(sdl) => build(sdl),
      None => build(SCHEMA),
    };
    let budget = fixture.budget.unwrap_or_default();
    let alone = RuleSet::only(fixture.rule);
    let diagnostics = diagnose_full(&schema, fixture.invalid, alone, &budget);
    assert!(
      !diagnostics.is_empty(),
      "{:?} fired nothing when selected alone\n---\n{}",
      fixture.rule,
      fixture.invalid
    );
    for diagnostic in &diagnostics {
      assert_eq!(
        diagnostic.rule(),
        fixture.rule,
        "selecting {:?} alone produced {:?}",
        fixture.rule,
        diagnostic.rule()
      );
    }

    // And the complement: with the rule switched off, the document never reports it.
    let without = RuleSet::ALL.without(fixture.rule);
    for diagnostic in diagnose_full(&schema, fixture.invalid, without, &budget) {
      assert_ne!(
        diagnostic.rule(),
        fixture.rule,
        "{:?} fired although it was excluded",
        fixture.rule
      );
    }
  }
}

/// The empty rule set validates nothing, so every fixture passes.
///
/// The complement of the floor: it shows the gate above measures rule *evaluation* and not merely
/// diagnostic filtering, because an empty set makes even the worst document valid.
#[test]
fn the_empty_rule_set_accepts_everything() {
  let schema = build(SCHEMA);
  for fixture in FIXTURES.iter().filter(|fixture| fixture.schema.is_none()) {
    let diagnostics = diagnose_with(&schema, fixture.invalid, RuleSet::EMPTY);
    assert!(
      diagnostics.is_empty(),
      "the empty rule set reported {:?}",
      diagnostics.first().map(Diagnostic::rule)
    );
  }
}

/// Every valid twin is valid under every rule, over every sink.
#[test]
fn every_valid_twin_is_valid_under_every_rule() {
  for fixture in FIXTURES {
    let schema = match fixture.schema {
      Some(sdl) => build(sdl),
      None => build(SCHEMA),
    };
    let document = parse(fixture.valid);
    let mut scratch = Scratch::new();
    let budget = fixture.budget.unwrap_or_default();

    let mut first = First::new();
    assert!(
      validate_executable(&schema, &document, &mut scratch, &budget, &mut first).is_ok(),
      "the twin for {:?} is invalid: {:?}",
      fixture.rule,
      first.get().map(|d| d.rule())
    );

    let mut count = Count::new();
    assert!(validate_executable(&schema, &document, &mut scratch, &budget, &mut count).is_ok());
    assert_eq!(count.get(), 0);
  }
}

// ---------------------------------------------------------------------------------------------
// the sinks
// ---------------------------------------------------------------------------------------------

/// `First` keeps one diagnostic and stops; the others see the whole document.
#[test]
fn sinks_mirror_their_tokora_counterparts() {
  let schema = build(SCHEMA);
  // Three independent mistakes, so "stopped early" is observable rather than inferred.
  let source = "{ dog { meowVolume purrVolume } cat }";
  let document = parse(source);
  let mut scratch = Scratch::new();
  let budget = Budget::default();

  let mut first = First::new();
  let invalid = validate_executable(&schema, &document, &mut scratch, &budget, &mut first)
    .expect_err("the document is invalid");
  assert_eq!(invalid.emitted(), 1, "First must stop at the first");
  assert!(invalid.stopped());
  assert_eq!(
    first.get().expect("a diagnostic").rule(),
    Rule::FieldSelections
  );

  let mut collected = Vec::new();
  let mut collect = Collect::new(&mut collected);
  let invalid = validate_executable(&schema, &document, &mut scratch, &budget, &mut collect)
    .expect_err("the document is invalid");
  assert!(!invalid.stopped());
  assert_eq!(invalid.emitted() as usize, collected.len());
  assert_eq!(collected.len(), 3, "{collected:#?}");

  let mut count = Count::new();
  let invalid = validate_executable(&schema, &document, &mut scratch, &budget, &mut count)
    .expect_err("the document is invalid");
  assert_eq!(count.get(), 3);
  assert_eq!(invalid.emitted(), 3);

  // `Ignore` keeps nothing and the verdict still stands: the count is the validator's, not the
  // sink's.
  let invalid = validate_executable(&schema, &document, &mut scratch, &budget, &mut Ignore)
    .expect_err("the document is invalid");
  assert_eq!(invalid.emitted(), 3);
  assert!(!invalid.stopped());
}

/// A sink handed by mutable reference is a sink, so a caller can keep theirs.
#[test]
fn a_borrowed_sink_is_a_sink() {
  let schema = build(SCHEMA);
  let document = parse("{ dog { meowVolume } }");
  let mut scratch = Scratch::new();
  let mut sink = Count::new();
  let mut borrowed = &mut sink;
  assert!(
    validate_executable(
      &schema,
      &document,
      &mut scratch,
      &Budget::default(),
      &mut borrowed
    )
    .is_err()
  );
  assert_eq!(sink.get(), 1);
}

// ---------------------------------------------------------------------------------------------
// diagnostics
// ---------------------------------------------------------------------------------------------

/// A diagnostic is a value: it names its rule, points at the source, and renders only on demand.
#[test]
fn a_diagnostic_points_at_what_it_refused() {
  let schema = build(SCHEMA);
  let source = "{ dog { meowVolume } }";
  let diagnostics = diagnose(&schema, source);
  let diagnostic = &diagnostics[0];

  assert_eq!(diagnostic.rule(), Rule::FieldSelections);
  assert_eq!(diagnostic.subject_source(), Some(&"meowVolume"));
  let span = diagnostic.span();
  assert_eq!(&source[span.start()..span.end()], "meowVolume");
  assert!(matches!(diagnostic.context_of(), Context::Type(_)));
  assert_eq!(
    diagnostic.display(&schema).to_string(),
    "5.3.1 Field Selections: `meowVolume` on type `Dog` (8..18)"
  );
}

/// A duplicate points at both occurrences.
#[test]
fn a_duplicate_points_back_at_the_first() {
  let schema = build(SCHEMA);
  let source = "query a { dog { name } } query a { dog { nickname } }";
  let diagnostics = diagnose(&schema, source);
  assert_eq!(diagnostics.len(), 1);
  let related = diagnostics[0].related_span().expect("a related span");
  assert!(related.start() < diagnostics[0].span().start());
  assert_eq!(&source[related.start()..related.end()], "a");
}

/// Every diagnostic every fixture produces renders, and names something.
#[test]
fn every_diagnostic_renders() {
  for fixture in FIXTURES {
    let schema = match fixture.schema {
      Some(sdl) => build(sdl),
      None => build(SCHEMA),
    };
    let budget = fixture.budget.unwrap_or_default();
    for diagnostic in diagnose_full(&schema, fixture.invalid, RuleSet::ALL, &budget) {
      let rendered = diagnostic.display(&schema).to_string();
      assert!(
        rendered.starts_with(diagnostic.rule().section()),
        "{rendered} does not open with its section"
      );
      assert!(
        rendered.len() > diagnostic.rule().section().len() + diagnostic.rule().title().len(),
        "{rendered} says nothing beyond the rule's name"
      );
    }
  }
}

// ---------------------------------------------------------------------------------------------
// acceptance: the shapes the rules must NOT refuse
// ---------------------------------------------------------------------------------------------

/// The specification's own valid examples, and the shapes that look wrong and are not.
#[test]
fn valid_documents_stay_valid() {
  let schema = build(SCHEMA);
  for source in [
    // Introspection is an ordinary document against the injected meta-schema.
    "{ __schema { types { name } } }",
    "{ __type(name: \"Dog\") { name kind } }",
    "{ dog { __typename name } }",
    // `__typename` is the one field selectable directly on a union.
    "{ catOrDog { __typename ... on Dog { barkVolume } ... on Cat { meowVolume } } }",
    // Interface field selection is on the interface's own fields.
    "{ pet { name } }",
    // An Int literal coerces into a Float position; an Int and a String both into an ID.
    "{ arguments { floatArgField(floatArg: 123) } }",
    "{ arguments { idArgField(idArg: 123) } }",
    "{ arguments { idArgField(idArg: \"123\") } }",
    // A custom scalar takes whatever the service can read.
    "{ arguments { customScalarArgField(customArg: { anything: [1, 2] }) } }",
    // A non-list value in a list position is the one-element list containing it.
    "{ arguments { booleanListArgField(booleanListArg: true) } }",
    "{ arguments { booleanListArgField(booleanListArg: [true, false, null]) } }",
    // An argument with a schema default may be omitted.
    "{ arguments { optionalNonNullBooleanArgField } }",
    // Order does not matter in arguments.
    "{ arguments { multipleRequirements(y: 2, x: 1) } }",
    "{ arguments { multipleRequirements(x: 1, y: 2) } }",
    // A fragment may be spread by several operations, and each supplies its own variables.
    "query one($atOtherHomes: Boolean) { dog { ...trained } } \
     query two($atOtherHomes: Boolean) { dog { ...trained } } \
     fragment trained on Dog { isHouseTrained(atOtherHomes: $atOtherHomes) }",
    // A subscription may collect one root field through a fragment.
    "subscription sub { ...newMessageFields } \
     fragment newMessageFields on Subscription { newMessage { body sender } }",
    // Two selections of the same response name are one entry in the subscription's map.
    "subscription sub { newMessage { body } newMessage { sender } }",
    // A nullable variable reaches a non-null location when either side has a default.
    "query withLocationDefault($booleanArg: Boolean) \
     { arguments { optionalNonNullBooleanArgField(optionalBooleanArg: $booleanArg) } }",
    "query withVariableDefault($booleanArg: Boolean = true) \
     { arguments { nonNullBooleanArgField(nonNullBooleanArg: $booleanArg) } }",
    // A non-null list variable reaches a nullable list location.
    "query nonNullListToList($list: [Boolean]!) \
     { arguments { booleanListArgField(booleanListArg: $list) } }",
    // OneOf: exactly one field, and a non-null variable in it.
    "mutation addCat { addPet(pet: { cat: { name: \"Brontie\" } }) { name } }",
    "mutation addCat($cat: CatInput!) { addPet(pet: { cat: $cat }) { name } }",
    "mutation addCatWithDefault($cat: CatInput! = { name: \"Brontie\" }) \
     { addPet(pet: { cat: $cat }) { name } }",
    // Repeatable directives repeat; non-repeatable ones may appear once per location.
    "{ dog { name @skip(if: true) } owner: dog { name @skip(if: false) } }",
    // A variable definition may carry its own directives and a default value.
    "query defaults($search: FindDogInput = { name: \"Fido\" }) \
     { findDog(searchBy: $search) { name } }",
  ] {
    let diagnostics = diagnose(&schema, source);
    assert!(
      diagnostics.is_empty(),
      "a valid document was refused: {:?}\n---\n{source}",
      diagnostics
        .iter()
        .map(|d| (d.rule(), d.display(&schema).to_string()))
        .collect::<Vec<_>>()
    );
  }
}

/// Draft 5.5.2.3's one adopted deviation: a spread whose condition *is* the scope is valid even
/// where the possible-object set is empty.
///
/// `Empty` is an interface nothing implements, so both possible-object sets are empty and the
/// intersection is too. graphql-js, apollo-compiler and graphql-spec#1109 all accept it.
#[test]
fn a_condition_that_is_the_scope_is_always_possible() {
  let schema = build(SCHEMA);
  assert!(
    schema
      .possible_objects(schema.type_by_name(b"Empty").expect("Empty is defined").0)
      .expect("an interface carries a bitset")
      .iter()
      .all(|word| *word == 0),
    "the fixture stopped being an interface with no implementors"
  );
  assert!(diagnose(&schema, "{ empty { ... on Empty { nothing } } }").is_empty());
  assert!(
    diagnose(
      &schema,
      "{ empty { ...emptyFragment } } fragment emptyFragment on Empty { nothing }"
    )
    .is_empty()
  );
}

// ---------------------------------------------------------------------------------------------
// the rules the ecosystem gets wrong, and the corners the table does not reach
// ---------------------------------------------------------------------------------------------

/// Draft 5.6.3, which apollo-compiler 1.32 does not implement at all, at every nesting depth.
#[test]
fn input_object_field_uniqueness_holds_at_depth() {
  let schema = build(SCHEMA);
  assert_eq!(
    fired(
      &schema,
      "{ arguments { customScalarArgField(customArg: 1) } }"
    ),
    []
  );
  // Nested inside a list inside an object, which is where a scan that reused one buffer for every
  // depth would lose the outer level's state.
  assert_eq!(
    fired(
      &schema,
      r#"{ findDog(searchBy: { name: "a", name: "b", owner: "x" }) { name } }"#
    ),
    [Rule::InputObjectFieldUniqueness]
  );
}

/// Draft 5.8.5 in every position, including inside a list literal — where apollo-compiler compares
/// only named types and accepts a nullable variable in a non-null item position.
#[test]
fn variable_usage_is_checked_inside_list_literals() {
  let schema = build(SCHEMA);
  // `booleanList(booleanListArg: [Boolean!])`: the item position is non-null.
  assert_eq!(
    fired(
      &schema,
      "query nested($v: Boolean) { booleanList(booleanListArg: [$v]) }"
    ),
    [Rule::AllVariableUsagesAreAllowed]
  );
  assert_eq!(
    fired(
      &schema,
      "query nested($v: Boolean!) { booleanList(booleanListArg: [$v]) }"
    ),
    []
  );
}

/// Draft 5.6.1 and 5.8.5's OneOf clauses, which apollo-compiler 1.32 does not implement at all.
#[test]
fn one_of_input_objects_are_checked() {
  let schema = build(SCHEMA);
  for (source, expected) in [
    (
      "mutation empty { addPet(pet: {}) { name } }",
      &[Rule::ValuesOfCorrectType][..],
    ),
    (
      "mutation two($dog: DogInput) { addPet(pet: { cat: { name: \"a\" }, dog: $dog }) { name } }",
      &[Rule::ValuesOfCorrectType, Rule::AllVariableUsagesAreAllowed],
    ),
    (
      "mutation nulled { addPet(pet: { cat: null }) { name } }",
      &[Rule::ValuesOfCorrectType],
    ),
    // A nullable variable in a OneOf field position, one list level down.
    (
      "mutation nested($dog: DogInput) { addPets(pets: [{ dog: $dog }]) { name } }",
      &[Rule::AllVariableUsagesAreAllowed],
    ),
  ] {
    assert_eq!(fired(&schema, source), sorted(expected), "---\n{source}");
  }
}

/// Draft 5.2.4.1 forbids `@skip` and `@include` on a subscription's root selection set, and
/// forbids an introspection field there.
#[test]
fn subscription_roots_are_collected_without_runtime_values() {
  let schema = build(SCHEMA);
  for source in [
    "subscription requiresRuntime($bool: Boolean!) \
     { newMessage @include(if: $bool) { body } }",
    "subscription introspects { __typename }",
    // The rule is about the *field*, not the response key, so an alias does not launder it.
    "subscription aliased { alias: __typename }",
    "subscription throughFragment { ...two } \
     fragment two on Subscription { newMessage { body } disallowedSecondRootField }",
  ] {
    let rules = fired(&schema, source);
    assert!(
      rules.contains(&Rule::SingleRootField),
      "5.2.4.1 did not fire for\n---\n{source}\n--- got {rules:?}"
    );
  }

  // And the aliased case names the field it refused, not the key the alias gave it — which is
  // what distinguishes reading the rule as written from reading it off the response name.
  let aliased = diagnose(&schema, "subscription aliased { alias: __typename }");
  assert_eq!(aliased.len(), 1);
  assert_eq!(aliased[0].subject_source(), Some(&"__typename"));
}

/// Draft 5.5.2.2 runs over the whole fragment graph, including fragments no operation reaches —
/// unlike apollo-compiler, which skips unused fragments.
///
/// A rule that later expands fragments must never meet an unestablished graph, so the cycle is
/// established whether or not anybody spreads it.
#[test]
fn cycles_are_found_in_unused_fragments_too() {
  let schema = build(SCHEMA);
  assert_eq!(
    fired(
      &schema,
      "{ dog { name } } fragment a on Dog { ...b } fragment b on Dog { ...a }"
    ),
    sorted(&[
      Rule::FragmentSpreadsMustNotFormCycles,
      Rule::FragmentsMustBeUsed
    ])
  );
  // A fragment that spreads itself is the degenerate case.
  assert!(
    fired(
      &schema,
      "{ dog { ...self } } fragment self on Dog { name ...self }"
    )
    .contains(&Rule::FragmentSpreadsMustNotFormCycles)
  );
}

/// A fragment several operations share reports its own mistakes once, and its variable usages once
/// per operation.
///
/// This is the split the walk is built around: definition-local rules fire on first entry, and the
/// operation-scoped rules fire every time.
#[test]
fn shared_fragments_report_structure_once_and_variables_per_operation() {
  let schema = build(SCHEMA);

  let structural = diagnose(
    &schema,
    "query one { dog { ...bad } } query two { dog { ...bad } } fragment bad on Dog { meowVolume }",
  );
  assert_eq!(
    structural.len(),
    1,
    "a shared fragment reported its own mistake more than once: {structural:#?}"
  );
  assert_eq!(structural[0].rule(), Rule::FieldSelections);

  let variables = diagnose(
    &schema,
    "query defines($atOtherHomes: Boolean) { dog { ...trained } } \
     query omits { dog { ...trained } } \
     fragment trained on Dog { isHouseTrained(atOtherHomes: $atOtherHomes) }",
  );
  assert_eq!(variables.len(), 1, "{variables:#?}");
  assert_eq!(variables[0].rule(), Rule::AllVariableUsesDefined);
}

/// An unreached fragment still has its structure validated, and has no variable scope to check
/// against.
#[test]
fn unreached_fragments_are_still_structurally_validated() {
  let schema = build(SCHEMA);
  assert_eq!(
    fired(
      &schema,
      "{ dog { name } } fragment orphan on Dog { meowVolume }"
    ),
    sorted(&[Rule::FieldSelections, Rule::FragmentsMustBeUsed])
  );
  // A variable usage inside one is not reported: the rule is scoped to an operation, and no
  // operation includes this fragment.
  assert_eq!(
    fired(
      &schema,
      "{ dog { name } } fragment orphan on Dog { isHouseTrained(atOtherHomes: $nope) }"
    ),
    [Rule::FragmentsMustBeUsed]
  );
}

/// Draft 5.4.3 and 5.6.4 own the explicit-`null` case, so one mistake produces one diagnostic.
#[test]
fn an_explicit_null_in_a_required_position_reports_once() {
  let schema = build(SCHEMA);
  assert_eq!(
    fired(
      &schema,
      "{ arguments { nonNullBooleanArgField(nonNullBooleanArg: null) } }"
    ),
    [Rule::RequiredArguments]
  );
  assert_eq!(
    fired(&schema, "{ requiredInput(input: { req: null }) }"),
    [Rule::InputObjectRequiredFields]
  );
}

/// A mistake at an unresolved position does not cascade: an unknown field's subselections are left
/// alone, and a variable used in an unknown argument is still "used".
#[test]
fn unresolved_positions_do_not_cascade() {
  let schema = build(SCHEMA);
  assert_eq!(
    fired(&schema, "{ dog { meowVolume { andAnother { andMore } } } }"),
    [Rule::FieldSelections]
  );
  assert_eq!(
    fired(
      &schema,
      "query q($v: Boolean) { dog { isHouseTrained(nope: $v) } }"
    ),
    [Rule::ArgumentNames]
  );
  assert_eq!(
    fired(
      &schema,
      "query q($v: Boolean) { dog { name @nope(arg: $v) } }"
    ),
    [Rule::DirectivesAreDefined]
  );
}

/// Deep nesting is walked without recursing on it, in both trees.
///
/// # Why the AST is assembled by hand
///
/// The parser is a recursive descent, and in a debug build its own recursion overflows a test
/// thread's stack somewhere between 25 and 50 levels of selection nesting — long before anything
/// the validator would notice. Parsing a deep document would therefore measure the parser, not
/// this claim. Building the tree directly, which the public AST types allow, puts the validator
/// alone under test: a thousand levels in each tree, with no parser in the picture.
///
/// The finished documents are deliberately leaked. `Vec`'s own `Drop` is recursive, so dropping a
/// thousand nested selection sets would overflow the stack on the way out and the test would fail
/// for a reason that has nothing to do with what it measures.
#[test]
fn deep_documents_are_walked_iteratively() {
  const DEPTH: usize = 1_000;
  let schema = build(SCHEMA);
  let mut scratch = Scratch::new();
  // The claim under test is that the walks do not recurse, which is about the stack and not about
  // policy — so the budget is lifted out of the way. Under the *default* budget a thousand nested
  // levels is refused, deliberately and by design: apollo-compiler refuses 140 for the same
  // reason, and `validator_merge.rs` pins that refusal.
  let budget = Budget::new(4_096, 10_000_000);

  let selections = nested_selection_document(DEPTH);
  assert!(
    validate_executable(&schema, &selections, &mut scratch, &budget, &mut Ignore).is_ok(),
    "a thousand nested selection sets"
  );
  core::mem::forget(selections);

  let values = nested_value_document(DEPTH);
  assert!(
    validate_executable(&schema, &values, &mut scratch, &budget, &mut Ignore).is_ok(),
    "a thousand nested object literals"
  );
  core::mem::forget(values);

  // And the same shapes with a mistake at the bottom, so the walk is shown to have arrived.
  let selections = nested_selection_document_ending_in(DEPTH, "notAField");
  assert_eq!(
    {
      let mut sink = First::new();
      let _ = validate_executable(&schema, &selections, &mut scratch, &budget, &mut sink);
      sink.get().map(Diagnostic::rule)
    },
    Some(Rule::FieldSelections)
  );
  core::mem::forget(selections);
}

fn nested_selection_document(depth: usize) -> ExecutableDocument<&'static str> {
  nested_selection_document_ending_in(depth, "leaf")
}

fn nested_selection_document_ending_in(
  depth: usize,
  innermost: &'static str,
) -> ExecutableDocument<&'static str> {
  let span = SimpleSpan::const_new(0, 0);
  let mut set = SelectionSet::new(
    span,
    vec![Selection::Field(Field::new(
      span,
      None,
      Name::new(span, innermost),
      None,
      None,
      None,
    ))],
  );
  for _ in 0..depth {
    set = SelectionSet::new(
      span,
      vec![Selection::Field(Field::new(
        span,
        None,
        Name::new(span, "nest"),
        None,
        None,
        Some(set),
      ))],
    );
  }
  document(SelectionSet::new(
    span,
    vec![Selection::Field(Field::new(
      span,
      None,
      Name::new(span, "nest"),
      None,
      None,
      Some(set),
    ))],
  ))
}

fn nested_value_document(depth: usize) -> ExecutableDocument<&'static str> {
  let span = SimpleSpan::const_new(0, 0);
  // `Recursive` has no required fields, so the innermost literal is the empty object.
  let mut value = InputValue::Object(Object::new(span, vec![]));
  for _ in 0..depth {
    value = InputValue::Object(Object::new(
      span,
      vec![ObjectField::new(span, Name::new(span, "rec"), value)],
    ));
  }
  document(SelectionSet::new(
    span,
    vec![Selection::Field(Field::new(
      span,
      None,
      Name::new(span, "recursive"),
      Some(ArgumentList::new(
        span,
        vec![Argument::new(span, Name::new(span, "input"), value)],
      )),
      None,
      None,
    ))],
  ))
}

fn document(set: SelectionSet<&'static str>) -> ExecutableDocument<&'static str> {
  let span = SimpleSpan::const_new(0, 0);
  ExecutableDocument::new(
    span,
    vec![Described::new(
      span,
      None,
      ExecutableDefinition::Operation(OperationDefinition::Shorthand(set)),
    )],
  )
}

// ---------------------------------------------------------------------------------------------
// the scratch
// ---------------------------------------------------------------------------------------------

/// One `Scratch` serves an unbounded number of validations, and stops growing.
#[test]
fn the_scratch_is_reused_not_regrown() {
  let schema = build(SCHEMA);
  let source = "query one($atOtherHomes: Boolean) { dog { ...trained ...named } } \
                fragment trained on Dog { isHouseTrained(atOtherHomes: $atOtherHomes) } \
                fragment named on Dog { name owner { name pets { name } } }";
  let document = parse(source);
  let mut scratch = Scratch::new();
  let budget = Budget::default();

  for _ in 0..3 {
    assert!(
      validate_executable(&schema, &document, &mut scratch, &budget, &mut Ignore).is_ok(),
      "{source}"
    );
  }
  let settled = scratch.capacity();
  for _ in 0..20 {
    assert!(validate_executable(&schema, &document, &mut scratch, &budget, &mut Ignore).is_ok());
  }
  assert_eq!(
    scratch.capacity(),
    settled,
    "the working set kept growing after it had seen the document"
  );

  // And a reset leaves it usable, with its capacity intact.
  scratch.reset();
  assert_eq!(scratch.capacity(), settled);
  assert!(validate_executable(&schema, &document, &mut scratch, &budget, &mut Ignore).is_ok());
}

/// A `Scratch` left over from an invalid document does not poison the next one.
#[test]
fn a_scratch_does_not_carry_state_between_documents() {
  let schema = build(SCHEMA);
  let mut scratch = Scratch::new();
  let budget = Budget::default();

  let bad = parse("{ dog { meowVolume } } fragment unused on Dog { name }");
  let mut first = First::new();
  assert!(validate_executable(&schema, &bad, &mut scratch, &budget, &mut first).is_err());

  let good = parse("{ dog { ...used } } fragment used on Dog { name }");
  let mut sink = Count::new();
  assert!(
    validate_executable(&schema, &good, &mut scratch, &budget, &mut sink).is_ok(),
    "the second document saw the first one's leftovers"
  );
  assert_eq!(sink.get(), 0);
}
