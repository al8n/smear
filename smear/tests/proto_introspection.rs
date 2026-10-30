#![cfg(feature = "proto")]

//! Draft §4 introspection, answered by the executor.
//!
//! # What is here and what is not
//!
//! `validator_introspection.rs` holds the oracle: graphql-js's full introspection query executed
//! against three schemas, written by `smear::json`, read back through `Schema::from_introspection`
//! and compared with the schema it was answered for. That comparison reads every fact a schema
//! keeps, and it reads them **name-sorted** wherever an order is not itself the fact — so it says
//! nothing about the order an answer lists things in, or about the shape of a field that answers
//! `null`. This file is those two halves:
//!
//! - **One golden answer, byte for byte**, for a schema that carries every datum draft §4 returns.
//!   It is the only gate on order: declaration order for fields, arguments, enum values, input
//!   fields, interfaces and directives; a union's members as declared and an interface's possible
//!   types in object order; a directive's locations in bit order; `__Schema.types` in the type
//!   table's order. `the_golden_fixture_has_orders_to_get_wrong` is what keeps it from passing on
//!   a schema whose stored order happens to be its declared one.
//! - **Behaviour, case by case**: `includeDeprecated`, the kind-dependent nulls, an unknown type,
//!   both arguments through variables and through literals, aliases and fragments over the meta
//!   types, `__typename` on a meta object, the meta-schema introspecting itself, where `__schema`
//!   and `__type` are answered and where not, and the two budget refusals.
//!
//! And two records. `the_full_introspection_query_fits_the_default_limits` measures what
//! graphql-js's query costs over GitHub's and GitLab's schemas and prints it, so the
//! `proto-oracle` job's log carries the numbers; and `the_introspection_query_is_graphql_jss_own`
//! re-derives the committed query from upstream's source when that job has upstream to hand.

use core::num::NonZeroU32;

use smear::{
  json::write_response_with,
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  proto::{Children, Executor, Kind, Leaf, Limits, Node, Resource, Response, Values},
  validator::{Budget, First, Schema, Scratch, validate_executable},
};

use serde_json::{Value as JsonValue, json};

// ------------------------------------------------------------------------------------------
// a driver, and the plumbing every case shares
// ------------------------------------------------------------------------------------------

/// The driver's values. Introspection asks a driver for nothing but a variable's value, so the
/// rest is the handful of fields the cases resolve beside it.
#[derive(Clone, Debug)]
enum J {
  Null,
  Bool(bool),
  Int(i64),
  Str(String),
  Obj,
}

#[derive(Default)]
struct Space {
  variables: Vec<(&'static str, J)>,
}

impl Values for Space {
  type Value = J;

  fn is_null(&self, value: &J) -> bool {
    matches!(value, J::Null)
  }

  fn as_bool(&self, value: &J) -> Option<bool> {
    match value {
      J::Bool(flag) => Some(*flag),
      _ => None,
    }
  }

  fn as_str<'a>(&'a self, value: &'a J) -> Option<&'a str> {
    match value {
      J::Str(text) => Some(text),
      _ => None,
    }
  }

  fn list_len(&self, _: &J) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &J, _: usize) -> J {
    J::Null
  }

  fn type_name<'a>(&'a self, _: &'a J) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: J, _: Leaf<'_>) -> Option<J> {
    Some(value)
  }

  fn variable(&mut self, name: &str) -> Option<J> {
    self
      .variables
      .iter()
      .find(|(declared, _)| *declared == name)
      .map(|(_, value)| value.clone())
  }
}

/// Parses an SDL and a query into the two borrows an [`Executor`] is built from.
fn compile<'q>(sdl: &str, query: &'q str) -> (Schema, ExecutableDocument<&'q str>) {
  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("the SDL is a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(query)
  .expect("the query parses");
  (schema, document)
}

/// Asserts that `query` passes every draft §5 rule against `sdl`.
fn assert_valid(sdl: &str, query: &str) {
  let (schema, document) = compile(sdl, query);
  let mut scratch = Scratch::new();
  let budget = Budget::default();
  let mut sink = First::new();
  if validate_executable(&schema, &document, &mut scratch, &budget, &mut sink).is_err() {
    panic!(
      "expected a valid document: {}",
      sink
        .get()
        .expect("a rule fired, so there is a diagnostic")
        .display(&schema)
    );
  }
}

/// Answers every field the driver is offered, the way the cases need them answered.
fn drive(executor: &mut Executor<'_, &str, Space>, space: &mut Space) {
  while let Some(request) = executor.poll_resolve(space) {
    let value = match request.name() {
      "me" => J::Obj,
      "poke" => J::Bool(true),
      "greeting" => J::Str("hello".to_owned()),
      _ => J::Null,
    };
    let id = request.id();
    executor.handle_resolved(space, id, value);
  }
}

/// Writes a response with `smear::json`, the writer a service would use.
fn written(response: &Response<'_, J>, query: &str) -> String {
  let mut out = String::new();
  write_response_with(&mut out, response, query, |value, json| match value {
    J::Null | J::Obj => json.null(),
    J::Bool(flag) => json.bool(*flag),
    J::Int(number) => json.number(*number),
    J::Str(text) => json.string(text),
  })
  .expect("a String takes everything");
  out
}

/// What one execution came to.
struct Run {
  /// The response's `data`, read back from what the writer wrote.
  data: JsonValue,
  /// Each error's kind, ceiling, path and message, in the order they were raised.
  errors: Vec<(Kind, Option<Resource>, String, String)>,
}

fn run(sdl: &str, query: &str) -> Run {
  run_with(sdl, query, &[], Limits::default())
}

fn run_with(sdl: &str, query: &str, variables: &[(&'static str, J)], limits: Limits) -> Run {
  let (schema, document) = compile(sdl, query);
  let mut space = Space {
    variables: variables.to_vec(),
  };
  let mut executor = Executor::with_limits(&schema, &document, limits);
  executor
    .start(&mut space, None, J::Obj)
    .expect("the operation resolves");
  drive(&mut executor, &mut space);
  let response = executor.poll_response().expect("nothing is outstanding");
  let errors = response
    .errors()
    .map(|error| {
      (
        error.kind(),
        error.resource(),
        error.path().to_string(),
        error.to_string(),
      )
    })
    .collect();
  let mut parsed: JsonValue =
    serde_json::from_str(&written(&response, query)).expect("the writer writes JSON");
  Run {
    data: parsed["data"].take(),
    errors,
  }
}

/// Every position in `data` — the root included — and how many of them are list elements.
///
/// On a stack of its own rather than the native one, for the reason the writer walks that way: an
/// introspection answer is as deep as the query asks.
fn census(data: Node<'_, J>) -> (usize, usize) {
  let mut positions = 1;
  let mut elements = 0;
  let mut stack: Vec<(Children<'_, J>, bool)> = Vec::new();
  match data {
    Node::Object(children) => stack.push((children, false)),
    Node::List(children) => stack.push((children, true)),
    _ => {}
  }
  while let Some((children, list)) = stack.last_mut() {
    let list = *list;
    let Some((_, child)) = children.next() else {
      stack.pop();
      continue;
    };
    positions += 1;
    if list {
      elements += 1;
    }
    match child {
      Node::Object(children) => stack.push((children, false)),
      Node::List(children) => stack.push((children, true)),
      _ => {}
    }
  }
  (positions, elements)
}

/// The least [`Limits::max_selection_visits`] under which `document` executes with no error.
///
/// Searched for rather than read: the executor keeps its ledger to itself, and a run under one
/// unit too few raises where the last unit would have been spent, so the least ceiling that
/// succeeds is exactly what a successful run spends.
fn least_visits(schema: &Schema, document: &ExecutableDocument<&str>) -> u32 {
  let fits = |visits: u32| -> bool {
    let limits = Limits {
      max_selection_visits: NonZeroU32::new(visits).expect("not zero"),
      ..Limits::default()
    };
    let mut space = Space::default();
    let mut executor = Executor::with_limits(schema, document, limits);
    executor.start(&mut space, None, J::Obj).is_ok()
      && executor
        .poll_response()
        .is_some_and(|response| response.is_ok())
  };
  let (mut low, mut high) = (1u32, Limits::default().max_selection_visits.get());
  assert!(fits(high), "the document does not fit the default ceiling");
  while low < high {
    let middle = low + (high - low) / 2;
    if fits(middle) {
      high = middle;
    } else {
      low = middle + 1;
    }
  }
  low
}

// ------------------------------------------------------------------------------------------
// the fixtures
// ------------------------------------------------------------------------------------------

/// graphql-js 16.11.0's `getIntrospectionQuery({ descriptions, specifiedByUrl,
/// directiveIsRepeatable, schemaDescription, inputValueDeprecation, oneOf })`, all `true`, byte for
/// byte — see `the_introspection_query_is_graphql_jss_own`.
const INTROSPECTION_QUERY: &str = include_str!("fixtures/executables/introspection_query.graphql");

/// The schema the behaviour cases run against: one of every kind, every list a deprecation can
/// shorten, a second root, and a query root that returns itself.
const SPEC_SDL: &str = r#"
schema {
  query: Query
  mutation: Mutation
}

type Query {
  greeting: String
  me: Query
  thing: Thing
}

type Mutation {
  poke: Boolean
}

"Anything with an id."
interface Thing {
  id: ID!
}

type Widget implements Thing {
  id: ID!
  size(unit: Unit = METRES, legacy: Boolean @deprecated): Int
  old: String @deprecated(reason: "gone")
  tags: [String!]!
}

type Lone {
  alone: Int
}

union Any = Widget | Lone

enum Unit {
  METRES
  FEET @deprecated(reason: "imperial")
}

input Spec {
  size: Int
  colour: String @deprecated
}

input Pick @oneOf {
  a: Int
  b: String
}

scalar Instant @specifiedBy(url: "https://example.com/instant")

directive @mark(why: String @deprecated, how: Int) on FIELD_DEFINITION
"#;

/// A schema carrying every datum draft §4 returns, declared out of the order it is stored in.
///
/// A block-string schema description that dedents, descriptions with escapes, all three
/// deprecations — with a reason, bare, and `reason: null` — on fields, arguments, enum values,
/// input fields and a directive's argument, a `@specifiedBy`, a `@oneOf`, a repeatable directive
/// declaring its locations out of bit order, defaults whose canonical text is not their spelling,
/// a four-wrapper type, an interface implementing an interface, an object listing its interfaces
/// out of type order, and a union listing its members out of object order.
const GOLDEN_SDL: &str = r#"
"""
  Every datum introspection returns.
    Indented past the rest.
"""
schema {
  query: Root
}

"An instant."
scalar Instant @specifiedBy(url: "https://example.com/instant")

"A label."
directive @tag(
  "What it says."
  name: String! = "none"
  old: Int @deprecated(reason: "gone")
) repeatable on OBJECT | FIELD_DEFINITION | QUERY

interface Node {
  id: ID!
}

"Something with a name."
interface Named implements Node {
  id: ID!
  name: String
}

"A person, \"quoted\"."
type Person implements Named & Node @tag {
  friends(first: Int = 10, after: [Int!] = [1, 2]): [[Person!]]!
  "Seconds since the epoch."
  born: Instant
  age(unit: Unit = YEARS, "Round down." floor: Boolean = true @deprecated): Int
    @deprecated(reason: "use born")
  name: String
  id: ID!
  legacy: String @deprecated(reason: null)
}

type Robot implements Node {
  id: ID!
}

union Thing = Robot | Person

enum Unit {
  "Twelve months."
  YEARS
  DAYS @deprecated
  HOURS @deprecated(reason: "too fine")
}

input Filter @oneOf {
  "By id."
  id: ID
  name: String @deprecated(reason: "use id")
  label: String
}

input Shape {
  sides: Int = 3
  label: String = "tri\tangle"
}

type Root {
  person(filter: Filter, shape: Shape): Person
  things: [Thing!]!
}
"#;

/// Every field of every meta type the golden answer has something to say about, with aliases and a
/// fragment so that the answer's keys are the query's and not the meta-schema's.
const GOLDEN_QUERY: &str = r#"
{
  __schema {
    description
    queryType { name }
    mutationType { name }
    types { name }
    directives {
      name
      isRepeatable
      locations
      args(includeDeprecated: true) { name defaultValue }
    }
  }
  person: __type(name: "Person") {
    kind
    name
    description
    interfaces { name }
    fields { name }
    all: fields(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
      args { name }
      every: args(includeDeprecated: true) {
        name
        description
        defaultValue
        isDeprecated
        deprecationReason
        type { ...Ref }
      }
      type { ...Ref }
    }
    specifiedByURL
    enumValues { name }
    inputFields { name }
    possibleTypes { name }
    isOneOf
    ofType { name }
  }
  node: __type(name: "Node") { kind possibleTypes { name } interfaces { name } }
  named: __type(name: "Named") { kind interfaces { name } possibleTypes { name } }
  thing: __type(name: "Thing") { kind possibleTypes { name } fields { name } interfaces { name } }
  unit: __type(name: "Unit") {
    kind
    enumValues { name }
    all: enumValues(includeDeprecated: true) { name description isDeprecated deprecationReason }
    fields { name }
  }
  filter: __type(name: "Filter") {
    kind
    isOneOf
    inputFields { name }
    all: inputFields(includeDeprecated: true) {
      name
      description
      defaultValue
      isDeprecated
      deprecationReason
    }
  }
  shape: __type(name: "Shape") { isOneOf inputFields { name defaultValue } }
  instant: __type(name: "Instant") { kind description specifiedByURL fields { name } isOneOf }
  missing: __type(name: "Missing") { name }
}

fragment Ref on __Type {
  kind
  name
  ofType { kind name ofType { kind name ofType { kind name ofType { kind name } } } }
}
"#;

/// [`GOLDEN_QUERY`]'s answer against [`GOLDEN_SDL`], as `smear::json` writes it.
const GOLDEN_ANSWER: &str = include_str!("golden/introspection_every_datum.json");

// ------------------------------------------------------------------------------------------
// the golden answer
// ------------------------------------------------------------------------------------------

/// The answer for a schema with every datum, byte for byte: content, key order, list order,
/// escaping and nulls at once.
#[test]
fn the_golden_answer_is_byte_for_byte() {
  assert_valid(GOLDEN_SDL, GOLDEN_QUERY);
  let (schema, document) = compile(GOLDEN_SDL, GOLDEN_QUERY);
  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, J::Obj)
    .expect("the operation resolves");
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "an introspection query offered the driver a field"
  );
  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 0);
  assert_eq!(written(&response, GOLDEN_QUERY), GOLDEN_ANSWER.trim_end());
}

/// The golden answer is an order gate only if the fixture's stored orders differ from its declared
/// ones, so that is asserted rather than assumed — for each order the answer pins.
#[test]
fn the_golden_fixture_has_orders_to_get_wrong() {
  let (schema, _) = compile(GOLDEN_SDL, "{ __typename }");
  let id = |name: &str| {
    schema
      .type_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("`{name}` is defined"))
      .0
  };
  let (person, thing) = (id("Person"), id("Thing"));
  assert!(
    schema
      .field_order_of(person)
      .iter()
      .enumerate()
      .any(|(at, &position)| at as u32 != position),
    "`Person` declares its fields in the order they are stored, so the field order is untested"
  );
  assert_ne!(
    schema.interfaces_of(person).to_vec(),
    schema.interfaces_in_order(person).collect::<Vec<_>>(),
    "`Person` declares its interfaces in type order, so the interface order is untested"
  );
  assert_ne!(
    schema.members_of(thing).to_vec(),
    schema.possible_object_ids(thing).collect::<Vec<_>>(),
    "`Thing` declares its members in object order, so a union's declared order is untested"
  );
}

// ------------------------------------------------------------------------------------------
// what reaches the driver
// ------------------------------------------------------------------------------------------

/// `__schema` and `__type` are answered whole by the call that collected them, and the driver is
/// offered nothing beneath them.
#[test]
fn introspection_never_reaches_the_driver() {
  let query = "{ __schema { queryType { name } } }";
  let (schema, document) = compile(SPEC_SDL, query);
  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, J::Obj)
    .expect("the operation resolves");
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "a selection of only `__schema` is complete when `start` returns"
  );
  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 0);

  // Beside a driver field, that field is the one offer.
  let query = r#"{ greeting __type(name: "Widget") { name } }"#;
  let (schema, document) = compile(SPEC_SDL, query);
  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, J::Obj)
    .expect("the operation resolves");
  let request = executor.poll_resolve(&mut space).expect("`greeting`");
  assert_eq!(request.name(), "greeting");
  let id = request.id();
  executor.handle_resolved(&mut space, id, J::Str("hello".to_owned()));
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "`greeting` was the only field the driver had to answer"
  );
  let response = executor.poll_response().expect("nothing is outstanding");
  let data: JsonValue =
    serde_json::from_str(&written(&response, query)).expect("the writer writes JSON");
  assert_eq!(
    data["data"],
    json!({ "greeting": "hello", "__type": { "name": "Widget" } })
  );
}

/// An object of the query root type answers `__schema` and `__type` wherever it stands — draft
/// §4.4's placement of them — and no other type has them to answer.
#[test]
fn only_the_query_root_type_answers() {
  let nested = run(
    SPEC_SDL,
    r#"{ me { __schema { queryType { name } } me { __type(name: "Widget") { name } } } }"#,
  );
  assert!(nested.errors.is_empty(), "{:?}", nested.errors);
  assert_eq!(
    nested.data,
    json!({
      "me": {
        "__schema": { "queryType": { "name": "Query" } },
        "me": { "__type": { "name": "Widget" } }
      }
    })
  );

  // The mutation root has no `__schema`, so the selection names nothing and adds no key.
  let mutation = run(
    SPEC_SDL,
    "mutation { poke __schema { queryType { name } } }",
  );
  assert!(mutation.errors.is_empty(), "{:?}", mutation.errors);
  assert_eq!(mutation.data, json!({ "poke": true }));
}

// ------------------------------------------------------------------------------------------
// what each field answers
// ------------------------------------------------------------------------------------------

/// `includeDeprecated` defaults to `false`, an explicit `null` means the same, and every one of
/// the four lists it governs is filtered by it.
#[test]
fn include_deprecated_defaults_to_false_and_filters_every_list() {
  let answer = run(
    SPEC_SDL,
    r#"{
      widget: __type(name: "Widget") {
        fields { name args { name } }
        all: fields(includeDeprecated: true) { name all: args(includeDeprecated: true) { name } }
        none: fields(includeDeprecated: null) { name }
        no: fields(includeDeprecated: false) { name }
      }
      unit: __type(name: "Unit") {
        enumValues { name }
        all: enumValues(includeDeprecated: true) { name }
      }
      spec: __type(name: "Spec") {
        inputFields { name }
        all: inputFields(includeDeprecated: true) { name }
      }
      __schema {
        directives { name args { name } all: args(includeDeprecated: true) { name } }
      }
    }"#,
  );
  assert!(answer.errors.is_empty(), "{:?}", answer.errors);
  let names =
    |list: &[&str]| -> JsonValue { list.iter().map(|name| json!({ "name": name })).collect() };
  assert_eq!(
    answer.data["widget"],
    json!({
      "fields": [
        { "name": "id", "args": [] },
        { "name": "size", "args": names(&["unit"]) },
        { "name": "tags", "args": [] }
      ],
      "all": [
        { "name": "id", "all": [] },
        { "name": "size", "all": names(&["unit", "legacy"]) },
        { "name": "old", "all": [] },
        { "name": "tags", "all": [] }
      ],
      "none": names(&["id", "size", "tags"]),
      "no": names(&["id", "size", "tags"])
    })
  );
  assert_eq!(
    answer.data["unit"],
    json!({ "enumValues": names(&["METRES"]), "all": names(&["METRES", "FEET"]) })
  );
  assert_eq!(
    answer.data["spec"],
    json!({ "inputFields": names(&["size"]), "all": names(&["size", "colour"]) })
  );
  let directive = |name: &str, shown: &[&str], all: &[&str]| -> JsonValue {
    json!({ "name": name, "args": names(shown), "all": names(all) })
  };
  assert_eq!(
    answer.data["__schema"]["directives"],
    json!([
      directive("mark", &["how"], &["why", "how"]),
      directive("skip", &["if"], &["if"]),
      directive("include", &["if"], &["if"]),
      directive("deprecated", &["reason"], &["reason"]),
      directive("specifiedBy", &["url"], &["url"]),
      directive("oneOf", &[], &[])
    ])
  );
}

/// Draft §4's kind-dependent nulls, for every kind a type can be, a reference's two wrappers
/// included.
#[test]
fn the_kind_decides_which_fields_answer_null() {
  let answer = run(
    SPEC_SDL,
    r#"{
      scalar: __type(name: "Instant") { ...K }
      object: __type(name: "Widget") { ...K }
      interface: __type(name: "Thing") { ...K }
      union: __type(name: "Any") { ...K }
      enum: __type(name: "Unit") { ...K }
      input: __type(name: "Pick") { ...K }
      lone: __type(name: "Lone") { interfaces { name } }
      wrapped: __type(name: "Widget") {
        fields(includeDeprecated: true) {
          name
          type { ...K ofType { kind name ofType { kind name ofType { kind name } } } }
        }
      }
    }
    fragment K on __Type {
      kind
      name
      specifiedByURL
      fields { name }
      interfaces { name }
      possibleTypes { name }
      enumValues { name }
      inputFields { name }
      isOneOf
    }"#,
  );
  assert!(answer.errors.is_empty(), "{:?}", answer.errors);
  let names =
    |list: &[&str]| -> JsonValue { list.iter().map(|name| json!({ "name": name })).collect() };
  // Everything `K` selects, `null` unless the kind answers it.
  let kind = |kind: &str, name: JsonValue, answers: JsonValue| -> JsonValue {
    let mut all = json!({
      "kind": kind,
      "name": name,
      "specifiedByURL": null,
      "fields": null,
      "interfaces": null,
      "possibleTypes": null,
      "enumValues": null,
      "inputFields": null,
      "isOneOf": null
    });
    for (key, value) in answers.as_object().expect("an object of answers") {
      all[key] = value.clone();
    }
    all
  };
  assert_eq!(
    answer.data["scalar"],
    kind(
      "SCALAR",
      json!("Instant"),
      json!({ "specifiedByURL": "https://example.com/instant" })
    )
  );
  assert_eq!(
    answer.data["object"],
    kind(
      "OBJECT",
      json!("Widget"),
      json!({ "fields": names(&["id", "size", "tags"]), "interfaces": names(&["Thing"]) })
    )
  );
  assert_eq!(
    answer.data["interface"],
    kind(
      "INTERFACE",
      json!("Thing"),
      json!({
        "fields": names(&["id"]),
        "interfaces": [],
        "possibleTypes": names(&["Widget"])
      })
    )
  );
  assert_eq!(
    answer.data["union"],
    kind(
      "UNION",
      json!("Any"),
      json!({ "possibleTypes": names(&["Widget", "Lone"]) })
    )
  );
  assert_eq!(
    answer.data["enum"],
    kind(
      "ENUM",
      json!("Unit"),
      json!({ "enumValues": names(&["METRES"]) })
    )
  );
  assert_eq!(
    answer.data["input"],
    kind(
      "INPUT_OBJECT",
      json!("Pick"),
      json!({ "inputFields": names(&["a", "b"]), "isOneOf": true })
    )
  );
  // An object implementing nothing answers `[]`, not `null`: the kind answers the field.
  assert_eq!(answer.data["lone"], json!({ "interfaces": [] }));

  // A reference: `[String!]!` is `NON_NULL` of `LIST` of `NON_NULL` of `String`, and each wrapper
  // answers `kind` and `ofType` and nothing else.
  let tags = &answer.data["wrapped"]["fields"][3];
  assert_eq!(tags["name"], "tags");
  let mut outer = kind("NON_NULL", JsonValue::Null, json!({}));
  outer["ofType"] = json!({
    "kind": "LIST",
    "name": null,
    "ofType": {
      "kind": "NON_NULL",
      "name": null,
      "ofType": { "kind": "SCALAR", "name": "String" }
    }
  });
  assert_eq!(tags["type"], outer);
  // And a named type answers `ofType` with `null`.
  let size = &answer.data["wrapped"]["fields"][1];
  assert_eq!(size["name"], "size");
  assert_eq!(size["type"]["kind"], "SCALAR");
  assert_eq!(size["type"]["ofType"], JsonValue::Null);
}

/// A name no type has is `null` and nothing else: draft §4.4's own answer, not a field error.
#[test]
fn an_unknown_type_name_answers_null() {
  let answer = run(SPEC_SDL, r#"{ __type(name: "Nope") { name } }"#);
  assert!(answer.errors.is_empty(), "{:?}", answer.errors);
  assert_eq!(answer.data, json!({ "__type": null }));
}

/// The meta-schema introspects like any other: `__type(name: "__Type")` answers, and its fields
/// are the meta type's own, in the order the meta-schema declares them and without the
/// `__typename` every composite type's table carries.
#[test]
fn the_meta_schema_introspects_itself() {
  let answer = run(
    SPEC_SDL,
    r#"{
      t: __type(name: "__Type") { kind name fields { name } interfaces { name } }
      d: __type(name: "__Directive") { fields { name } }
      k: __type(name: "__TypeKind") { kind enumValues { name } }
    }"#,
  );
  assert!(answer.errors.is_empty(), "{:?}", answer.errors);
  let names =
    |list: &[&str]| -> JsonValue { list.iter().map(|name| json!({ "name": name })).collect() };
  assert_eq!(
    answer.data,
    json!({
      "t": {
        "kind": "OBJECT",
        "name": "__Type",
        "fields": names(&[
          "kind",
          "name",
          "description",
          "specifiedByURL",
          "fields",
          "interfaces",
          "possibleTypes",
          "enumValues",
          "inputFields",
          "ofType",
          "isOneOf"
        ]),
        "interfaces": []
      },
      "d": {
        "fields": names(&["name", "description", "locations", "args", "isRepeatable"])
      },
      "k": {
        "kind": "ENUM",
        "enumValues": names(&[
          "SCALAR",
          "OBJECT",
          "INTERFACE",
          "UNION",
          "ENUM",
          "INPUT_OBJECT",
          "LIST",
          "NON_NULL"
        ])
      }
    })
  );
}

/// Aliases and fragments over the meta types are draft §6.3's ordinary collection: a spread on the
/// query root, an inline fragment and a named one on `__Type`, and every key an alias.
#[test]
fn aliases_and_fragments_work_over_the_meta_types() {
  let answer = run(
    SPEC_SDL,
    r#"{
      s: __schema { q: queryType { ...N } }
      ...OnRoot
    }
    fragment N on __Type { n: name k: kind }
    fragment OnRoot on Query {
      t: __type(name: "Widget") { ... on __Type { name } ...N }
    }"#,
  );
  assert!(answer.errors.is_empty(), "{:?}", answer.errors);
  assert_eq!(
    answer.data,
    json!({
      "s": { "q": { "n": "Query", "k": "OBJECT" } },
      "t": { "name": "Widget", "n": "Widget", "k": "OBJECT" }
    })
  );
}

/// `__typename` on a meta object names the meta type, through the path every object takes.
#[test]
fn typename_names_the_meta_type() {
  let answer = run(
    SPEC_SDL,
    r#"{
      __typename
      __schema { __typename queryType { __typename } directives { __typename } }
      __type(name: "Widget") {
        __typename
        fields { __typename type { __typename } args { __typename } }
      }
      unit: __type(name: "Unit") { enumValues { __typename } }
      pick: __type(name: "Pick") { inputFields { __typename } }
    }"#,
  );
  assert!(answer.errors.is_empty(), "{:?}", answer.errors);
  let directive = json!({ "__typename": "__Directive" });
  let field = |args: JsonValue| -> JsonValue {
    json!({ "__typename": "__Field", "type": { "__typename": "__Type" }, "args": args })
  };
  assert_eq!(
    answer.data,
    json!({
      "__typename": "Query",
      "__schema": {
        "__typename": "__Schema",
        "queryType": { "__typename": "__Type" },
        "directives": [directive, directive, directive, directive, directive, directive]
      },
      "__type": {
        "__typename": "__Type",
        "fields": [
          field(json!([])),
          field(json!([{ "__typename": "__InputValue" }])),
          field(json!([]))
        ]
      },
      "unit": { "enumValues": [{ "__typename": "__EnumValue" }] },
      "pick": {
        "inputFields": [{ "__typename": "__InputValue" }, { "__typename": "__InputValue" }]
      }
    })
  );
}

// ------------------------------------------------------------------------------------------
// the two arguments
// ------------------------------------------------------------------------------------------

/// Both arguments read through variables: `name` through [`Values::as_str`] and
/// `includeDeprecated` through [`Values::as_bool`], with draft §6.4.1's default and `null` both
/// meaning `false`.
#[test]
fn variables_reach_both_arguments() {
  const QUERY: &str = r#"query ($name: String!, $deprecated: Boolean) {
    __type(name: $name) { name fields(includeDeprecated: $deprecated) { name } }
  }"#;
  let names =
    |list: &[&str]| -> JsonValue { list.iter().map(|name| json!({ "name": name })).collect() };
  let widget = J::Str("Widget".to_owned());
  for (variables, fields) in [
    (
      vec![("name", widget.clone()), ("deprecated", J::Bool(true))],
      names(&["id", "size", "old", "tags"]),
    ),
    (
      vec![("name", widget.clone()), ("deprecated", J::Bool(false))],
      names(&["id", "size", "tags"]),
    ),
    (
      vec![("name", widget.clone())],
      names(&["id", "size", "tags"]),
    ),
    (
      vec![("name", widget.clone()), ("deprecated", J::Null)],
      names(&["id", "size", "tags"]),
    ),
  ] {
    let answer = run_with(SPEC_SDL, QUERY, &variables, Limits::default());
    assert!(answer.errors.is_empty(), "{:?}", answer.errors);
    assert_eq!(
      answer.data,
      json!({ "__type": { "name": "Widget", "fields": fields } })
    );
  }

  let unknown = run_with(
    SPEC_SDL,
    QUERY,
    &[("name", J::Str("Nope".to_owned()))],
    Limits::default(),
  );
  assert!(unknown.errors.is_empty(), "{:?}", unknown.errors);
  assert_eq!(unknown.data, json!({ "__type": null }));
}

/// A value of the wrong type for either argument is draft §6.4.1 step 5.j's field error, at the
/// field it was passed to — whether a variable carried it or the document wrote it.
#[test]
fn a_value_of_the_wrong_type_is_a_field_error() {
  const QUERY: &str = r#"query ($name: String!, $deprecated: Boolean) {
    __type(name: $name) { name fields(includeDeprecated: $deprecated) { name } }
  }"#;
  let name_error: (Kind, Option<Resource>, String, String) = (
    Kind::ArgumentCoercion,
    None,
    "__type".to_owned(),
    "Argument \"name\" of type \"String!\" must be a String.".to_owned(),
  );
  let flag_error: (Kind, Option<Resource>, String, String) = (
    Kind::ArgumentCoercion,
    None,
    "__type.fields".to_owned(),
    "Argument \"includeDeprecated\" of type \"Boolean\" must be a Boolean.".to_owned(),
  );

  let not_a_string = run_with(SPEC_SDL, QUERY, &[("name", J::Int(5))], Limits::default());
  assert_eq!(not_a_string.errors, vec![name_error.clone()]);
  assert_eq!(not_a_string.data, json!({ "__type": null }));

  let not_a_boolean = run_with(
    SPEC_SDL,
    QUERY,
    &[
      ("name", J::Str("Widget".to_owned())),
      ("deprecated", J::Str("yes".to_owned())),
    ],
    Limits::default(),
  );
  assert_eq!(not_a_boolean.errors, vec![flag_error.clone()]);
  assert_eq!(
    not_a_boolean.data,
    json!({ "__type": { "name": "Widget", "fields": null } })
  );

  // The same two faults written as literals, which only an unvalidated document can carry.
  let literal_name = run(SPEC_SDL, "{ __type(name: 5) { name } }");
  assert_eq!(literal_name.errors, vec![name_error]);
  assert_eq!(literal_name.data, json!({ "__type": null }));
  let literal_flag = run(
    SPEC_SDL,
    r#"{ __type(name: "Widget") { name fields(includeDeprecated: "yes") { name } } }"#,
  );
  assert_eq!(literal_flag.errors, vec![flag_error]);
  assert_eq!(
    literal_flag.data,
    json!({ "__type": { "name": "Widget", "fields": null } })
  );
}

/// `name` is `String!` with no default, so draft §6.4.1 raises for it like for any other argument:
/// absent, `null`, or a variable the request did not supply.
#[test]
fn a_missing_name_is_draft_6_4_1s_error() {
  const VARIABLE: &str = "query ($name: String!) { __type(name: $name) { name } }";
  let unsupplied = run(SPEC_SDL, VARIABLE);
  assert_eq!(unsupplied.data, json!({ "__type": null }));
  assert_eq!(
    unsupplied
      .errors
      .iter()
      .map(|(kind, ..)| *kind)
      .collect::<Vec<_>>(),
    vec![Kind::ArgumentVariableMissing]
  );

  let null = run_with(SPEC_SDL, VARIABLE, &[("name", J::Null)], Limits::default());
  assert_eq!(null.data, json!({ "__type": null }));
  assert_eq!(
    null
      .errors
      .iter()
      .map(|(kind, ..)| *kind)
      .collect::<Vec<_>>(),
    vec![Kind::ArgumentNull]
  );

  let absent = run(SPEC_SDL, "{ __type { name } }");
  assert_eq!(absent.data, json!({ "__type": null }));
  assert_eq!(
    absent
      .errors
      .iter()
      .map(|(kind, ..)| *kind)
      .collect::<Vec<_>>(),
    vec![Kind::ArgumentMissing]
  );

  let literal_null = run(SPEC_SDL, "{ __type(name: null) { name } }");
  assert_eq!(literal_null.data, json!({ "__type": null }));
  assert_eq!(
    literal_null
      .errors
      .iter()
      .map(|(kind, ..)| *kind)
      .collect::<Vec<_>>(),
    vec![Kind::ArgumentNull]
  );
}

/// A literal `name` is read as the string value it denotes — escapes applied, a block string
/// dedented — and not as its spelling.
#[test]
fn a_literal_name_is_read_as_its_value() {
  let answer = run(
    SPEC_SDL,
    r#"{
      escaped: __type(name: "\u0057idget") { name }
      braced: __type(name: "Wid\u{67}et") { name }
      block: __type(name: """Widget""") { name }
      dedented: __type(name: """
        Widget
      """) { name }
      empty: __type(name: "") { name }
    }"#,
  );
  assert!(answer.errors.is_empty(), "{:?}", answer.errors);
  let widget = json!({ "name": "Widget" });
  assert_eq!(
    answer.data,
    json!({
      "escaped": widget,
      "braced": widget,
      "block": widget,
      "dedented": widget,
      "empty": null
    })
  );
}

// ------------------------------------------------------------------------------------------
// the budgets
// ------------------------------------------------------------------------------------------

/// A position refusal inside the walk unwinds the list it was building and fails it there — and
/// `types: [__Type!]!` under `__schema: __Schema!` makes that the operation's whole `data`.
#[test]
fn a_position_refusal_under_types_nulls_data() {
  let limits = Limits {
    max_response_slots: NonZeroU32::new(8).expect("not zero"),
    ..Limits::default()
  };
  let answer = run_with(SPEC_SDL, "{ __schema { types { name } } }", &[], limits);
  assert_eq!(answer.data, JsonValue::Null, "`data` is null, not absent");
  assert_eq!(
    answer.errors,
    vec![(
      Kind::ExecutorBudget,
      Some(Resource::ResponseSlots),
      "__schema.types".to_owned(),
      "Cannot complete field __Schema.types: the response would exceed the executor's limit of 8 \
       positions."
        .to_owned(),
    )]
  );

  // And with room, the same query answers in full.
  let roomy = run(SPEC_SDL, "{ __schema { types { name } } }");
  assert!(roomy.errors.is_empty(), "{:?}", roomy.errors);
  assert_eq!(
    roomy.data["__schema"]["types"]
      .as_array()
      .expect("a list")
      .len(),
    23,
    "ten document types, eight meta types and five built-in scalars"
  );
}

/// The walk's own charge — one unit per member a list reads — refuses like any other ceiling, at
/// the list whose member it could not afford.
///
/// The query ends on a list on purpose: draft §6.3 collects every directive before any of them
/// answers its `locations`, so the last unit a successful run spends is the last directive's last
/// location, and one unit fewer is refused exactly there.
#[test]
fn the_walks_own_charge_refuses_at_the_list() {
  const QUERY: &str = "{ __schema { directives { locations } } }";
  let (schema, document) = compile(SPEC_SDL, QUERY);
  let least = least_visits(&schema, &document);
  let short = Limits {
    max_selection_visits: NonZeroU32::new(least - 1).expect("not zero"),
    ..Limits::default()
  };
  let answer = run_with(SPEC_SDL, QUERY, &[], short);
  assert_eq!(answer.data, JsonValue::Null);
  assert_eq!(
    answer.errors,
    vec![(
      Kind::ExecutorBudget,
      Some(Resource::SelectionVisits),
      "__schema.directives.5.locations".to_owned(),
      format!(
        "Cannot complete field __Directive.locations: answering it from the schema would exceed \
         the executor's limit of {} selection visits.",
        least - 1
      ),
    )]
  );
}

// ------------------------------------------------------------------------------------------
// the records
// ------------------------------------------------------------------------------------------

/// graphql-js's full introspection query over the two largest public schemas in the fixture tree
/// answers under [`Limits::default`], and what it costs is printed.
///
/// The positions are counted off the answer: a run with no error discards and restores nothing, so
/// every position the executor created is one `data` renders. The metadata follows from them —
/// two entries, a merged selection and its location, for each position that is an object's field,
/// since no field of this query is selected twice under one parent. The visits are searched for;
/// see [`least_visits`].
#[test]
fn the_full_introspection_query_fits_the_default_limits() {
  let defaults = Limits::default();
  for (name, sdl) in [
    (
      "github",
      include_str!("fixtures/schemas/github_schema.graphql"),
    ),
    (
      "gitlab",
      include_str!("fixtures/schemas/gitlab_schema.graphql"),
    ),
  ] {
    let (schema, document) = compile(sdl, INTROSPECTION_QUERY);
    let mut space = Space::default();
    let mut executor = Executor::new(&schema, &document);
    executor
      .start(&mut space, None, J::Obj)
      .expect("the operation resolves");
    assert!(
      executor.poll_resolve(&mut space).is_none(),
      "{name}: the introspection query offered the driver a field"
    );
    let response = executor.poll_response().expect("nothing is outstanding");
    let errors: Vec<String> = response.errors().map(|error| error.to_string()).collect();
    assert!(
      errors.is_empty(),
      "{name}: the default limits refused graphql-js's introspection query: {errors:?}"
    );
    let (slots, elements) = census(response.data());
    let metadata = 2 * (slots - 1 - elements);
    let visits = least_visits(&schema, &document);
    println!(
      "introspection budget, {name}: {slots} response slots of {}, {metadata} metadata entries of \
       {}, {visits} selection visits of {}",
      defaults.max_response_slots, defaults.max_response_metadata, defaults.max_selection_visits,
    );
  }
}

/// The committed query is upstream's own: re-derived from graphql-js's `getIntrospectionQuery.ts`
/// with every option on, whenever `SMEAR_GRAPHQL_JS` points at a checkout — which the
/// `proto-oracle` job's does.
#[test]
fn the_introspection_query_is_graphql_jss_own() {
  let Some(root) = std::env::var_os("SMEAR_GRAPHQL_JS") else {
    println!(
      "SMEAR_GRAPHQL_JS is unset, so the committed introspection query was not re-derived from \
       upstream"
    );
    return;
  };
  let path = std::path::Path::new(&root).join("src/utilities/getIntrospectionQuery.ts");
  let source = std::fs::read_to_string(&path)
    .unwrap_or_else(|error| panic!("{} is unreadable: {error}", path.display()));
  let start = source
    .find("return `")
    .expect("`getIntrospectionQuery` returns a template literal")
    + "return `".len();
  let end = start
    + source[start..]
      .find("`;")
      .expect("the template literal is closed");
  let mut query = source[start..end].to_owned();
  for (placeholder, value) in [
    ("${schemaDescription}", "description"),
    ("${descriptions}", "description"),
    ("${directiveIsRepeatable}", "isRepeatable"),
    (
      "${inputDeprecation('(includeDeprecated: true)')}",
      "(includeDeprecated: true)",
    ),
    ("${inputDeprecation('isDeprecated')}", "isDeprecated"),
    (
      "${inputDeprecation('deprecationReason')}",
      "deprecationReason",
    ),
    ("${specifiedByUrl}", "specifiedByURL"),
    ("${oneOf}", "isOneOf"),
  ] {
    query = query.replace(placeholder, value);
  }
  assert!(
    !query.contains("${"),
    "upstream's template has a placeholder this derivation does not know:\n{query}"
  );
  assert_eq!(
    query, INTROSPECTION_QUERY,
    "the committed introspection query has drifted from graphql-js's"
  );
}

/// graphql-js's query is a valid document against a schema built here: every field it selects is
/// in the meta-schema the build injects, `isOneOf` and `specifiedByURL` included.
#[test]
fn the_introspection_query_is_valid_here() {
  assert_valid(SPEC_SDL, INTROSPECTION_QUERY);
}
