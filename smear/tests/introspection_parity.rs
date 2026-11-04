#![cfg(feature = "proto")]

//! The executor's draft §4 answer, against graphql-js 16.11.0's answer for the same SDL.
//!
//! # What is compared, and against what
//!
//! For each fixture below, smear builds the [`Schema`] from the SDL, executes graphql-js's full
//! introspection query — the committed `introspection_query.graphql`, `getIntrospectionQuery` with
//! all six options on — with the `proto` executor, writes the response with `smear::json`, and
//! reads its `data` back with `serde_json`.
//!
//! graphql-js's side is a committed golden under `tests/oracle/graphql-js-introspection/`:
//! `introspectionFromSchema(buildSchema(sdl), options)` with the same six options, written by
//! `ci/introspection-parity/generate.mjs`. CI never runs node. Each golden records its provenance
//! — the release, the options, and the SHA-256 of the SDL and of the query it answered — and a case
//! holds all of it to the fixture and the query it is about to compare before it compares anything,
//! so a fixture edited without its golden being regenerated fails on the hash rather than on
//! whatever an answer for some other schema happens to say.
//!
//! The answers are compared value for value. **Every difference has to be one of the eight recorded
//! below**, each absorbed by exactly one rule and nothing broader; anything else fails, named by its
//! path — `__schema.types[Person].fields[friends].args[first].defaultValue` — and every one is
//! collected before the case fails, so one run reports them all.
//!
//! # The eight recorded differences
//!
//! Each was checked against graphql-js 16.11.0's real output, and against smear's answer where one
//! is committed: `tests/golden/introspection_every_datum.json`, whose graphql-js twin the generator
//! writes as `every_datum.json` beside the goldens, for a schema carrying every datum draft §4
//! returns.
//!
//! 1. **`__schema.types` order.** smear lists the document's types in document order, then the
//!    meta-schema, then the built-in scalars the document did not spell out; graphql-js lists the
//!    document's types with each built-in scalar where it is first referenced, then the meta-schema.
//!    Compared as a set keyed by name: the order is recorded, not compared.
//! 2. **Built-in scalars nothing references.** smear injects all five, and graphql-js leaves out of
//!    its type map one that no field, argument or input field names. smear's extra types must be
//!    exactly the built-in scalars its own answer never references — a subset of `Int`, `Float` and
//!    `ID`, since the meta-schema references `String` and `Boolean` — and are then set aside.
//! 3. **`defaultValue`.** smear prints the literal the document wrote, in graphql-js's print form;
//!    graphql-js prints the value after coercing it to the input value's type,
//!    `print(astFromValue(valueFromAST(literal, type), type))`. So smear's text is re-rendered the
//!    graphql-js way — directed by the `type` beside it and by graphql-js's own `__schema.types` —
//!    and compared with graphql-js's text as a string. The rules that makes:
//!    - a single item where a list is expected is wrapped, at every depth: `7` is `[7]` for `[Int]`;
//!    - an input object's fields come in the type's field order, with every omitted field that has
//!      a default filled in from it: `{max: 9, min: 1}` is `{min: 1, max: 9, step: 1}`;
//!    - a `Float` is a JavaScript number and prints as one: `1.0` is `1` and `1.5e3` is `1500`;
//!    - an `ID` whose value is a string of digits prints as an integer: `"4"` is `4`;
//!    - a custom scalar's literal is coerced untyped, so an enum value is its name as a string:
//!      `AS_WRITTEN` is `"AS_WRITTEN"`;
//!    - a string, block strings included, prints as `printString` quotes it, and a list and an
//!      object as `[1, 2]` and `{a: 1}`. That is the form smear's own text already has, and it is
//!      asserted rather than normalised: smear's text must parse and print back to itself before it
//!      is re-rendered, so a change to smear's printer fails here instead of being absorbed.
//! 4. **`__Directive`'s field order.** smear declares `isRepeatable` last and graphql-js third.
//!    `__Directive`'s fields are compared as a set; every other type's, meta types included, in
//!    order.
//! 5. **`@deprecated(reason: null)`.** smear answers `isDeprecated: true, deprecationReason: null`;
//!    graphql-js answers `isDeprecated: false, deprecationReason: null`, its `isDeprecated` being
//!    `deprecationReason != null`. Accepted exactly there, and pinned both ways: the places it is
//!    accepted must number the places the SDL spells `@deprecated(reason: null)`.
//! 6. **A directive's `locations` order.** smear lists them in `DirectiveLocation` bit order and
//!    graphql-js as declared. Compared as a set.
//! 7. **`__schema.directives` order.** Both list the document's directives first; graphql-js then
//!    appends the specified ones as `include`, `skip`, `deprecated`, `specifiedBy`, `oneOf`, and
//!    smear as `skip`, `include`, …. Compared as a set keyed by name.
//! 8. **An interface's `possibleTypes` order.** Compared as a set; a union's members keep their
//!    declared order, and that is compared. smear lists an interface's implementations in object
//!    order and graphql-js in its type map's order, and for a schema built from SDL both are the
//!    document's order — so this one has never fired: no fixture differs, and none can.
//!
//! Everything else must be equal: descriptions, `isDeprecated` and `deprecationReason`,
//! `specifiedByURL`, `isOneOf`, interfaces, a union's possible types, enum values, input fields,
//! directives and their arguments, every `ofType` chain, and the root types.
//!
//! # Which difference fires where
//!
//! Recorded per fixture and asserted, so the table is a checked claim rather than a note:
//!
//! | difference | `agreement` | `github` | `gitlab` | `defaults` |
//! |---|---|---|---|---|
//! | 1. types reordered | yes | yes | yes | yes |
//! | 2. extra built-in scalars | `Float` | `Float` | none | none |
//! | 3. defaults re-rendered | 0 | 0 | 0 | 9 |
//! | 4. `__Directive` reordered | yes | yes | yes | yes |
//! | 5. `reason: null` | 1 | 0 | 0 | 0 |
//! | 6. locations reordered | `@auth`, `@deprecated` | `@deprecated` | `@deprecated` | `@deprecated` |
//! | 7. directives reordered | yes | yes | yes | yes |
//! | 8. possible types reordered | none | none | none | none |
//!
//! The three B2 fixtures — the agreement fixture `validator_introspection.rs` holds every construct
//! in, and GitHub's and GitLab's schemas — show every difference but two. Not one of their 536
//! defaults prints differently once coerced, so `defaults` exists for this file alone: one default
//! per rule of difference 3, and nothing else in it differs any other way. Difference 8 has no such
//! fixture, because no schema built from SDL can show it.

use std::{
  collections::{BTreeMap, BTreeSet},
  fmt::Write as _,
  path::PathBuf,
};

use serde_json::Value;
use sha2::{Digest as _, Sha256};
use smear::{
  json::write_response_with,
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  proto::{Executor, Leaf, Values},
  validator::Schema,
};

// ------------------------------------------------------------------------------------------
// the fixtures, and the record each one has to produce
// ------------------------------------------------------------------------------------------

/// graphql-js 16.11.0's `getIntrospectionQuery` with all six options on, byte for byte.
const QUERY: &str = include_str!("fixtures/executables/introspection_query.graphql");

/// [`QUERY`]'s path from the workspace root, as a golden's provenance records it.
const QUERY_PATH: &str = "smear/tests/fixtures/executables/introspection_query.graphql";

/// The release every golden is graphql-js's answer from.
const RELEASE: &str = "16.11.0";

/// The five built-in scalars, which smear injects whether or not anything references them.
const BUILT_IN_SCALARS: [&str; 5] = ["Int", "Float", "String", "Boolean", "ID"];

/// The built-in scalars difference 2 may set aside: the meta-schema references the other two.
const UNREFERENCED_SCALARS: [&str; 3] = ["Int", "Float", "ID"];

/// One schema, the golden graphql-js answered it with, and what the comparison has to record.
struct Fixture {
  /// The golden's name under `tests/oracle/graphql-js-introspection/`.
  name: &'static str,
  /// The SDL's path from the workspace root, as the golden's provenance records it.
  path: &'static str,
  sdl: &'static str,
  /// Difference 2's set.
  extra_scalars: &'static [&'static str],
  /// How many defaults difference 3 re-rendered.
  defaults_coerced: usize,
  /// The directives difference 6 found listed out of bit order.
  locations_reordered: &'static [&'static str],
}

const AGREEMENT: Fixture = Fixture {
  name: "agreement",
  path: "smear/tests/fixtures/schemas/introspection_agreement.graphql",
  sdl: include_str!("fixtures/schemas/introspection_agreement.graphql"),
  extra_scalars: &["Float"],
  defaults_coerced: 0,
  locations_reordered: &["auth", "deprecated"],
};

const GITHUB: Fixture = Fixture {
  name: "github",
  path: "smear/tests/fixtures/schemas/github_schema.graphql",
  sdl: include_str!("fixtures/schemas/github_schema.graphql"),
  extra_scalars: &["Float"],
  defaults_coerced: 0,
  locations_reordered: &["deprecated"],
};

const GITLAB: Fixture = Fixture {
  name: "gitlab",
  path: "smear/tests/fixtures/schemas/gitlab_schema.graphql",
  sdl: include_str!("fixtures/schemas/gitlab_schema.graphql"),
  extra_scalars: &[],
  defaults_coerced: 0,
  locations_reordered: &["deprecated"],
};

const DEFAULTS: Fixture = Fixture {
  name: "defaults",
  path: "smear/tests/fixtures/schemas/introspection_defaults.graphql",
  sdl: include_str!("fixtures/schemas/introspection_defaults.graphql"),
  extra_scalars: &[],
  defaults_coerced: 9,
  locations_reordered: &["deprecated"],
};

/// How often each recorded difference was needed to explain a comparison.
#[derive(Debug, Default, PartialEq, Eq)]
struct Record {
  /// Difference 1.
  types_reordered: bool,
  /// Difference 2.
  extra_scalars: BTreeSet<String>,
  /// Difference 3.
  defaults_coerced: usize,
  /// Difference 4.
  directive_fields_reordered: bool,
  /// Difference 5.
  reason_null: usize,
  /// Difference 6.
  locations_reordered: BTreeSet<String>,
  /// Difference 7.
  directives_reordered: bool,
  /// Difference 8.
  possible_types_reordered: BTreeSet<String>,
}

impl Record {
  /// What `fixture` has to record: its own three rows of the table, the rows every fixture shares,
  /// and difference 5 counted off its SDL.
  fn expected(fixture: &Fixture) -> Self {
    Self {
      types_reordered: true,
      extra_scalars: names(fixture.extra_scalars),
      defaults_coerced: fixture.defaults_coerced,
      directive_fields_reordered: true,
      reason_null: fixture.sdl.matches("@deprecated(reason: null)").count(),
      locations_reordered: names(fixture.locations_reordered),
      directives_reordered: true,
      possible_types_reordered: BTreeSet::new(),
    }
  }
}

fn names(names: &[&str]) -> BTreeSet<String> {
  names.iter().map(|name| (*name).to_owned()).collect()
}

// ------------------------------------------------------------------------------------------
// the cases
// ------------------------------------------------------------------------------------------

/// The agreement fixture: every construct a schema can hold, described.
#[test]
fn the_agreement_fixture_differs_from_graphql_js_only_as_recorded() {
  differs_only_as_recorded(&AGREEMENT);
}

/// GitHub's public schema, for volume.
#[test]
fn githubs_schema_differs_from_graphql_js_only_as_recorded() {
  differs_only_as_recorded(&GITHUB);
}

/// GitLab's public schema, which also spells out two of the specified directives.
#[test]
fn gitlabs_schema_differs_from_graphql_js_only_as_recorded() {
  differs_only_as_recorded(&GITLAB);
}

/// One default per rule of difference 3, which no other fixture exercises.
#[test]
fn the_defaults_fixture_differs_from_graphql_js_only_as_recorded() {
  differs_only_as_recorded(&DEFAULTS);
}

/// Compares smear's answer for `fixture` with graphql-js's, and its record with the table's.
fn differs_only_as_recorded(fixture: &Fixture) {
  let golden = golden(fixture.name);
  provenance(fixture, &golden["provenance"]);
  let smear = smear_answer(fixture.sdl);
  let graphql_js = &golden["data"];

  let mut comparison = Comparison::new(&graphql_js["__schema"]);
  comparison.data(&smear, graphql_js);
  let Comparison {
    unexplained,
    record,
    ..
  } = comparison;

  println!(
    "introspection parity, {}: {} differences outside the eight; {record:?}",
    fixture.name,
    unexplained.len()
  );
  assert!(
    unexplained.is_empty(),
    "{}: {} differences from graphql-js {RELEASE} are not among the eight recorded:\n  {}",
    fixture.name,
    unexplained.len(),
    unexplained
      .iter()
      .take(40)
      .cloned()
      .collect::<Vec<_>>()
      .join("\n  ")
  );
  assert_eq!(
    record,
    Record::expected(fixture),
    "{}: the recorded differences fired other than the table in this file's header says, so \
     the table — and README's §4 row — no longer describe what differs",
    fixture.name
  );
}

/// Holds a golden to the fixture and the query it is compared with.
fn provenance(fixture: &Fixture, provenance: &Value) {
  let name = fixture.name;
  assert_eq!(
    provenance["graphql"], RELEASE,
    "{name}: the golden's release"
  );
  assert_eq!(
    provenance["options"],
    serde_json::json!({
      "descriptions": true,
      "specifiedByUrl": true,
      "directiveIsRepeatable": true,
      "schemaDescription": true,
      "inputValueDeprecation": true,
      "oneOf": true,
    }),
    "{name}: the golden's options are not the six the committed query was generated with"
  );
  assert_eq!(provenance["sdl"], fixture.path, "{name}: the golden's SDL");
  assert_eq!(
    provenance["sdlSha256"],
    sha256(fixture.sdl.as_bytes()),
    "{name}: the fixture is not the SDL the golden was generated from — regenerate it with \
     `ci/introspection-parity/generate.mjs`"
  );
  assert_eq!(
    provenance["query"], QUERY_PATH,
    "{name}: the golden's query"
  );
  assert_eq!(
    provenance["querySha256"],
    sha256(QUERY.as_bytes()),
    "{name}: the golden answered a different query than the one executed here"
  );
}

/// The lowercase hex SHA-256 of `bytes`.
fn sha256(bytes: &[u8]) -> String {
  let mut hasher = Sha256::new();
  hasher.update(bytes);
  let mut hex = String::with_capacity(64);
  for byte in hasher.finalize() {
    let _ = write!(hex, "{byte:02x}");
  }
  hex
}

/// A committed golden, read where `oracle.rs` reads its own.
fn golden(name: &str) -> Value {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/oracle/graphql-js-introspection")
    .join(format!("{name}.json"));
  let text = std::fs::read_to_string(&path).unwrap_or_else(|error| {
    panic!(
      "the golden at {} is unreadable: {error} — generate it with \
       `ci/introspection-parity/generate.mjs`",
      path.display()
    )
  });
  serde_json::from_str(&text)
    .unwrap_or_else(|error| panic!("the golden at {} is not JSON: {error}", path.display()))
}

// ------------------------------------------------------------------------------------------
// smear's side
// ------------------------------------------------------------------------------------------

/// A driver with nothing to say: no field of the query reaches one.
struct Nothing;

impl Values for Nothing {
  type Value = ();

  fn is_null(&self, _: &()) -> bool {
    false
  }

  fn as_bool(&self, _: &()) -> Option<bool> {
    None
  }

  fn as_str<'a>(&'a self, _: &'a ()) -> Option<&'a str> {
    None
  }

  fn list_len(&self, _: &()) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &(), _: usize) {}

  fn type_name<'a>(&'a self, _: &'a ()) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: (), _: Leaf<'_>) -> Option<()> {
    Some(value)
  }

  fn variable(&mut self, _: &str) -> Option<()> {
    None
  }
}

/// `sdl` built, [`QUERY`] executed and written, and the `data` it wrote read back.
fn smear_answer(sdl: &str) -> Value {
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
  .parse_str(QUERY)
  .expect("graphql-js's introspection query parses");

  let mut nothing = Nothing;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut nothing, None, ())
    .expect("the operation resolves");
  assert!(
    executor.poll_resolve(&mut nothing).is_none(),
    "an introspection query offered the driver a field"
  );
  let response = executor.poll_response().expect("nothing is outstanding");
  let errors: Vec<String> = response.errors().map(|error| error.to_string()).collect();
  assert!(errors.is_empty(), "the introspection raised {errors:?}");
  let mut written = String::new();
  write_response_with(&mut written, &response, QUERY, |_, _| {
    unreachable!("an introspection response holds no driver value")
  })
  .expect("a String takes everything");
  let mut parsed: Value = serde_json::from_str(&written).expect("the writer writes JSON");
  parsed["data"].take()
}

// ------------------------------------------------------------------------------------------
// the comparison
// ------------------------------------------------------------------------------------------

/// graphql-js's `__schema.types`, by name: what difference 3 reads an input type off.
type Types<'j> = BTreeMap<&'j str, &'j Value>;

/// One comparison of two answers: what no rule explains, and how often each rule was needed.
struct Comparison<'j> {
  types: Types<'j>,
  unexplained: Vec<String>,
  record: Record,
}

impl<'j> Comparison<'j> {
  fn new(graphql_js: &'j Value) -> Self {
    let types: Types<'j> = graphql_js["types"]
      .as_array()
      .map(|types| {
        types
          .iter()
          .filter_map(|ty| Some((ty["name"].as_str()?, ty)))
          .collect()
      })
      .unwrap_or_default();
    Self {
      types,
      unexplained: Vec::new(),
      record: Record::default(),
    }
  }

  fn differ(&mut self, path: &str, what: String) {
    self.unexplained.push(format!("{path}: {what}"));
  }

  /// Equal, or one unexplained difference at `path`.
  fn exact(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    if smear != graphql_js {
      self.differ(
        path,
        format!("smear {} | graphql-js {}", show(smear), show(graphql_js)),
      );
    }
  }

  /// Whether both are objects with the same keys; a difference at `path` when they are not.
  fn same_keys(&mut self, path: &str, smear: &Value, graphql_js: &Value) -> bool {
    let (Some(ours), Some(theirs)) = (smear.as_object(), graphql_js.as_object()) else {
      self.exact(path, smear, graphql_js);
      return false;
    };
    let ours: BTreeSet<&String> = ours.keys().collect();
    let theirs: BTreeSet<&String> = theirs.keys().collect();
    if ours != theirs {
      self.differ(
        path,
        format!("smear has the keys {ours:?} and graphql-js {theirs:?}"),
      );
    }
    true
  }

  /// The answer's `data`.
  fn data(&mut self, smear: &Value, graphql_js: &'j Value) {
    if !self.same_keys("data", smear, graphql_js) {
      return;
    }
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      match key.as_str() {
        "__schema" => self.schema(ours, theirs),
        _ => self.exact(key, ours, theirs),
      }
    }
  }

  fn schema(&mut self, smear: &Value, graphql_js: &'j Value) {
    let path = "__schema";
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    let referenced = referenced(smear);
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      let here = format!("{path}.{key}");
      match key.as_str() {
        "types" => self.types(&here, ours, theirs, &referenced),
        "directives" => {
          // Difference 7: compared as a set keyed by name.
          if self.list(&here, ours, theirs, false, Self::directive) {
            self.record.directives_reordered = true;
          }
        }
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  /// `__schema.types`: differences 1 and 2.
  fn types(
    &mut self,
    path: &str,
    smear: &Value,
    graphql_js: &'j Value,
    referenced: &BTreeSet<String>,
  ) {
    let (Some(ours), Some(theirs)) = (self.named(path, smear), self.named(path, graphql_js)) else {
      return self.exact(path, smear, graphql_js);
    };

    // Difference 2: smear's extra types are exactly the built-in scalars nothing references.
    let extra: BTreeSet<&str> = ours
      .order
      .iter()
      .copied()
      .filter(|name| !theirs.by_name.contains_key(*name))
      .collect();
    let unreferenced: BTreeSet<&str> = BUILT_IN_SCALARS
      .into_iter()
      .filter(|name| ours.by_name.contains_key(*name) && !referenced.contains(*name))
      .collect();
    if extra != unreferenced {
      self.differ(
        path,
        format!(
          "smear lists {extra:?} beyond graphql-js's types, and the built-in scalars nothing \
           references are {unreferenced:?}"
        ),
      );
    }
    if let Some(scalar) = unreferenced
      .iter()
      .find(|name| !UNREFERENCED_SCALARS.contains(*name))
    {
      self.differ(
        path,
        format!("`{scalar}` is unreferenced, and the meta-schema references it"),
      );
    }
    self.record.extra_scalars = extra.iter().map(|name| (*name).to_owned()).collect();
    for name in theirs
      .order
      .iter()
      .filter(|name| !ours.by_name.contains_key(**name))
    {
      self.differ(
        path,
        format!("graphql-js lists `{name}`, and smear does not"),
      );
    }

    // Difference 1: the order is recorded, not compared.
    let common: Vec<&str> = ours
      .order
      .iter()
      .copied()
      .filter(|name| theirs.by_name.contains_key(*name))
      .collect();
    self.record.types_reordered = common != theirs.order;

    for (&name, &theirs) in &theirs.by_name {
      if let Some(&ours) = ours.by_name.get(name) {
        self.full_type(&format!("{path}[{name}]"), ours, theirs);
      }
    }
  }

  /// One `__Type` of `__schema.types`, with every field the query selects.
  fn full_type(&mut self, path: &str, smear: &Value, graphql_js: &'j Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    let kind = graphql_js["kind"].as_str().unwrap_or_default();
    let name = graphql_js["name"].as_str().unwrap_or_default();
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      let here = format!("{path}.{key}");
      match key.as_str() {
        "fields" => {
          // Difference 4: `__Directive`'s fields are compared as a set, and only its.
          let ordered = name != "__Directive";
          if self.list(&here, ours, theirs, ordered, Self::field) && !ordered {
            self.record.directive_fields_reordered = true;
          }
        }
        "inputFields" => {
          self.list(&here, ours, theirs, true, Self::input_value);
        }
        "enumValues" => {
          self.list(&here, ours, theirs, true, Self::enum_value);
        }
        "interfaces" => {
          self.list(&here, ours, theirs, true, Self::member);
        }
        "possibleTypes" => {
          // Difference 8: an interface's are compared as a set, and a union's in order.
          let ordered = kind != "INTERFACE";
          if self.list(&here, ours, theirs, ordered, Self::member) && !ordered {
            self.record.possible_types_reordered.insert(name.to_owned());
          }
        }
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  fn field(&mut self, path: &str, smear: &Value, graphql_js: &'j Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    self.deprecation(path, smear, graphql_js);
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      let here = format!("{path}.{key}");
      match key.as_str() {
        "args" => {
          self.list(&here, ours, theirs, true, Self::input_value);
        }
        "isDeprecated" | "deprecationReason" => {}
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  fn input_value(&mut self, path: &str, smear: &Value, graphql_js: &'j Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    self.deprecation(path, smear, graphql_js);
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      let here = format!("{path}.{key}");
      match key.as_str() {
        "defaultValue" => self.default_value(&here, ours, theirs, &graphql_js["type"]),
        "isDeprecated" | "deprecationReason" => {}
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  fn enum_value(&mut self, path: &str, smear: &Value, graphql_js: &'j Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    self.deprecation(path, smear, graphql_js);
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      match key.as_str() {
        "isDeprecated" | "deprecationReason" => {}
        _ => self.exact(&format!("{path}.{key}"), ours, theirs),
      }
    }
  }

  /// An interface, a possible type or a list member compared whole: a named type reference.
  fn member(&mut self, path: &str, smear: &Value, graphql_js: &'j Value) {
    self.exact(path, smear, graphql_js);
  }

  fn directive(&mut self, path: &str, smear: &Value, graphql_js: &'j Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    let name = graphql_js["name"].as_str().unwrap_or_default();
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      let here = format!("{path}.{key}");
      match key.as_str() {
        "locations" => {
          if self.locations(&here, ours, theirs) {
            self.record.locations_reordered.insert(name.to_owned());
          }
        }
        "args" => {
          self.list(&here, ours, theirs, true, Self::input_value);
        }
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  /// Difference 6: a directive's locations, compared as a set. Answers whether the order differed.
  fn locations(&mut self, path: &str, smear: &Value, graphql_js: &Value) -> bool {
    let (Some(ours), Some(theirs)) = (smear.as_array(), graphql_js.as_array()) else {
      self.exact(path, smear, graphql_js);
      return false;
    };
    fn sorted(list: &[Value]) -> Vec<String> {
      let mut list: Vec<String> = list.iter().map(Value::to_string).collect();
      list.sort();
      list
    }
    if sorted(ours) != sorted(theirs) {
      self.exact(path, smear, graphql_js);
      return false;
    }
    ours != theirs
  }

  /// Difference 5, on any object that answers `isDeprecated` and `deprecationReason`.
  fn deprecation(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    let pair = |value: &Value| {
      (
        value.get("isDeprecated").cloned(),
        value.get("deprecationReason").cloned(),
      )
    };
    let (ours, theirs) = (pair(smear), pair(graphql_js));
    if ours == theirs {
      return;
    }
    let absent_reason = Some(Value::Null);
    if ours == (Some(Value::Bool(true)), absent_reason.clone())
      && theirs == (Some(Value::Bool(false)), absent_reason)
    {
      self.record.reason_null += 1;
      return;
    }
    self.differ(
      path,
      format!("deprecation: smear {ours:?} | graphql-js {theirs:?}"),
    );
  }

  /// Difference 3: smear's literal, re-rendered the way graphql-js prints a coerced default.
  fn default_value(&mut self, path: &str, smear: &Value, graphql_js: &Value, ty: &Value) {
    if smear == graphql_js {
      return;
    }
    let Some(text) = smear.as_str() else {
      return self.exact(path, smear, graphql_js);
    };
    let literal = match Const::parse(text) {
      Ok(literal) => literal,
      Err(error) => return self.differ(path, format!("smear's `{text}` does not parse: {error}")),
    };
    let printed = literal.print();
    if printed != text {
      return self.differ(
        path,
        format!("smear's `{text}` is not in print form, which prints it `{printed}`"),
      );
    }
    let rendered = coerce(&literal, ty, &self.types)
      .and_then(|value| ast_from_value(&value, ty, &self.types))
      .map(|ast| ast.print());
    if rendered.as_deref() == graphql_js.as_str() {
      self.record.defaults_coerced += 1;
    } else {
      self.differ(
        path,
        format!(
          "smear `{text}`, coerced `{rendered:?}` | graphql-js {}",
          show(graphql_js)
        ),
      );
    }
  }

  /// A list whose members carry a `name`, compared member by member under the name, and in order
  /// when `ordered`.
  ///
  /// Answers whether the order differed, which is a difference when `ordered` and is what the
  /// caller records when not.
  fn list(
    &mut self,
    path: &str,
    smear: &Value,
    graphql_js: &'j Value,
    ordered: bool,
    each: fn(&mut Self, &str, &Value, &'j Value),
  ) -> bool {
    if smear.is_null() && graphql_js.is_null() {
      return false;
    }
    let (Some(ours), Some(theirs)) = (self.named(path, smear), self.named(path, graphql_js)) else {
      self.exact(path, smear, graphql_js);
      return false;
    };
    for name in ours
      .order
      .iter()
      .filter(|name| !theirs.by_name.contains_key(**name))
    {
      self.differ(
        path,
        format!("smear lists `{name}`, and graphql-js does not"),
      );
    }
    for name in theirs
      .order
      .iter()
      .filter(|name| !ours.by_name.contains_key(**name))
    {
      self.differ(
        path,
        format!("graphql-js lists `{name}`, and smear does not"),
      );
    }
    for (&name, &theirs) in &theirs.by_name {
      if let Some(&ours) = ours.by_name.get(name) {
        each(self, &format!("{path}[{name}]"), ours, theirs);
      }
    }
    let reordered = ours.order != theirs.order;
    if reordered && ordered {
      self.differ(
        path,
        format!(
          "the order: smear {:?} | graphql-js {:?}",
          ours.order, theirs.order
        ),
      );
    }
    reordered
  }

  /// A list's members by `name`, and the names in order; `None` when it is not a list.
  fn named<'v>(&mut self, path: &str, list: &'v Value) -> Option<Named<'v>> {
    let mut named = Named {
      order: Vec::new(),
      by_name: BTreeMap::new(),
    };
    for member in list.as_array()? {
      let Some(name) = member["name"].as_str() else {
        self.differ(path, format!("a member with no name: {}", show(member)));
        continue;
      };
      named.order.push(name);
      if named.by_name.insert(name, member).is_some() {
        self.differ(path, format!("`{name}` is listed twice"));
      }
    }
    Some(named)
  }
}

/// A list's members under their names.
struct Named<'v> {
  order: Vec<&'v str>,
  by_name: BTreeMap<&'v str, &'v Value>,
}

/// A value for a message, cut short: a whole type is not a message.
fn show(value: &Value) -> String {
  let mut text = value.to_string();
  if text.len() > 160 {
    let mut cut = 160;
    while !text.is_char_boundary(cut) {
      cut -= 1;
    }
    text.truncate(cut);
    text.push('…');
  }
  text
}

/// Every type name an answer's fields, arguments, input fields and directive arguments reference.
fn referenced(schema: &Value) -> BTreeSet<String> {
  fn named(mut ty: &Value, into: &mut BTreeSet<String>) {
    while let Some(of) = ty.get("ofType").filter(|of| !of.is_null()) {
      ty = of;
    }
    if let Some(name) = ty["name"].as_str() {
      into.insert(name.to_owned());
    }
  }
  fn inputs(list: &Value, into: &mut BTreeSet<String>) {
    for input in list.as_array().into_iter().flatten() {
      named(&input["type"], into);
    }
  }
  let mut into = BTreeSet::new();
  for ty in schema["types"].as_array().into_iter().flatten() {
    for field in ty["fields"].as_array().into_iter().flatten() {
      named(&field["type"], &mut into);
      inputs(&field["args"], &mut into);
    }
    inputs(&ty["inputFields"], &mut into);
  }
  for directive in schema["directives"].as_array().into_iter().flatten() {
    inputs(&directive["args"], &mut into);
  }
  into
}

// ------------------------------------------------------------------------------------------
// difference 3: graphql-js's coercion and printing, over a parsed literal
// ------------------------------------------------------------------------------------------

/// A constant GraphQL value: a literal as parsed, or an AST as graphql-js's `astFromValue` builds
/// one — the two are one shape, which is what lets one printer serve both.
#[derive(Clone, Debug, PartialEq)]
enum Const {
  Null,
  Bool(bool),
  /// An integer, as spelled.
  Int(String),
  /// A float, as spelled.
  Float(String),
  /// A string's value, decoded.
  Str(String),
  Enum(String),
  List(Vec<Const>),
  Object(Vec<(String, Const)>),
}

impl Const {
  /// Parses a constant value: the inline strings, numbers, names, lists and objects a printed
  /// default is made of. A block string is refused, because neither printer writes one.
  fn parse(text: &str) -> Result<Self, String> {
    let mut reader = Reader { text, at: 0 };
    let value = reader.value()?;
    reader.skip();
    if reader.at == text.len() {
      Ok(value)
    } else {
      Err(format!("trailing text at byte {}", reader.at))
    }
  }

  /// graphql-js's `print` of a value node.
  fn print(&self) -> String {
    let mut out = String::new();
    self.write(&mut out);
    out
  }

  fn write(&self, out: &mut String) {
    match self {
      Self::Null => out.push_str("null"),
      Self::Bool(flag) => out.push_str(if *flag { "true" } else { "false" }),
      Self::Int(spelling) | Self::Float(spelling) | Self::Enum(spelling) => out.push_str(spelling),
      Self::Str(value) => print_string(value, out),
      Self::List(items) => {
        out.push('[');
        for (at, item) in items.iter().enumerate() {
          if at > 0 {
            out.push_str(", ");
          }
          item.write(out);
        }
        out.push(']');
      }
      Self::Object(fields) => {
        out.push('{');
        for (at, (name, value)) in fields.iter().enumerate() {
          if at > 0 {
            out.push_str(", ");
          }
          out.push_str(name);
          out.push_str(": ");
          value.write(out);
        }
        out.push('}');
      }
    }
  }
}

/// graphql-js's `printString`: the short escapes for `"` `\` `\b` `\t` `\n` `\f` `\r`, `\u00XX`
/// in upper case for every other code point in U+0000–U+001F and U+007F–U+009F, and everything
/// else as itself.
fn print_string(value: &str, out: &mut String) {
  out.push('"');
  for ch in value.chars() {
    match ch {
      '"' => out.push_str("\\\""),
      '\\' => out.push_str("\\\\"),
      '\u{8}' => out.push_str("\\b"),
      '\t' => out.push_str("\\t"),
      '\n' => out.push_str("\\n"),
      '\u{c}' => out.push_str("\\f"),
      '\r' => out.push_str("\\r"),
      '\u{0}'..='\u{1f}' | '\u{7f}'..='\u{9f}' => {
        let _ = write!(out, "\\u{:04X}", u32::from(ch));
      }
      _ => out.push(ch),
    }
  }
  out.push('"');
}

/// A cursor over a printed constant value.
struct Reader<'t> {
  text: &'t str,
  at: usize,
}

impl<'t> Reader<'t> {
  fn peek(&self) -> Option<char> {
    self.text[self.at..].chars().next()
  }

  /// Skips what draft §2.1 ignores between tokens: white space, line terminators and commas.
  fn skip(&mut self) {
    while let Some(ch) = self.peek() {
      if matches!(ch, ' ' | '\t' | '\n' | '\r' | ',' | '\u{feff}') {
        self.at += ch.len_utf8();
      } else {
        break;
      }
    }
  }

  fn expect(&mut self, ch: char) -> Result<(), String> {
    self.skip();
    if self.peek() == Some(ch) {
      self.at += ch.len_utf8();
      Ok(())
    } else {
      Err(format!("expected `{ch}` at byte {}", self.at))
    }
  }

  fn value(&mut self) -> Result<Const, String> {
    self.skip();
    match self.peek() {
      Some('[') => {
        self.at += 1;
        let mut items = Vec::new();
        loop {
          self.skip();
          if self.peek() == Some(']') {
            self.at += 1;
            return Ok(Const::List(items));
          }
          items.push(self.value()?);
        }
      }
      Some('{') => {
        self.at += 1;
        let mut fields = Vec::new();
        loop {
          self.skip();
          if self.peek() == Some('}') {
            self.at += 1;
            return Ok(Const::Object(fields));
          }
          let name = self.name()?;
          self.expect(':')?;
          fields.push((name.to_owned(), self.value()?));
        }
      }
      Some('"') => self.string(),
      Some(ch) if ch == '-' || ch.is_ascii_digit() => self.number(),
      Some(ch) if ch == '_' || ch.is_ascii_alphabetic() => Ok(match self.name()? {
        "null" => Const::Null,
        "true" => Const::Bool(true),
        "false" => Const::Bool(false),
        name => Const::Enum(name.to_owned()),
      }),
      _ => Err(format!("no value at byte {}", self.at)),
    }
  }

  fn name(&mut self) -> Result<&'t str, String> {
    let start = self.at;
    let bytes = self.text.as_bytes();
    while self.at < bytes.len()
      && (bytes[self.at] == b'_' || bytes[self.at].is_ascii_alphanumeric())
    {
      self.at += 1;
    }
    if self.at == start || bytes[start].is_ascii_digit() {
      return Err(format!("no name at byte {start}"));
    }
    Ok(&self.text[start..self.at])
  }

  /// Draft §2.9.1 and §2.9.2: `-?(0|[1-9][0-9]*)`, then a fraction, an exponent, or both for a
  /// float.
  fn number(&mut self) -> Result<Const, String> {
    let start = self.at;
    let bytes = self.text.as_bytes();
    let digits = |at: &mut usize| {
      let from = *at;
      while *at < bytes.len() && bytes[*at].is_ascii_digit() {
        *at += 1;
      }
      *at - from
    };
    let mut at = self.at;
    if bytes.get(at) == Some(&b'-') {
      at += 1;
    }
    let whole = digits(&mut at);
    if whole == 0 || (whole > 1 && bytes[at - whole] == b'0') {
      return Err(format!("a malformed number at byte {start}"));
    }
    let mut float = false;
    if bytes.get(at) == Some(&b'.') {
      at += 1;
      if digits(&mut at) == 0 {
        return Err(format!("a fraction with no digits at byte {start}"));
      }
      float = true;
    }
    if matches!(bytes.get(at), Some(b'e' | b'E')) {
      at += 1;
      if matches!(bytes.get(at), Some(b'+' | b'-')) {
        at += 1;
      }
      if digits(&mut at) == 0 {
        return Err(format!("an exponent with no digits at byte {start}"));
      }
      float = true;
    }
    self.at = at;
    let spelling = self.text[start..at].to_owned();
    Ok(if float {
      Const::Float(spelling)
    } else {
      Const::Int(spelling)
    })
  }

  /// An inline string, its escapes decoded.
  fn string(&mut self) -> Result<Const, String> {
    if self.text[self.at..].starts_with("\"\"\"") {
      return Err(format!("a block string at byte {}", self.at));
    }
    self.at += 1;
    let mut value = String::new();
    loop {
      let ch = self
        .peek()
        .ok_or_else(|| "an unterminated string".to_owned())?;
      self.at += ch.len_utf8();
      match ch {
        '"' => return Ok(Const::Str(value)),
        '\\' => {
          let escape = self
            .peek()
            .ok_or_else(|| "an unterminated escape".to_owned())?;
          self.at += escape.len_utf8();
          value.push(match escape {
            '"' => '"',
            '\\' => '\\',
            '/' => '/',
            'b' => '\u{8}',
            'f' => '\u{c}',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'u' => self.unicode()?,
            other => return Err(format!("an unknown escape `\\{other}`")),
          });
        }
        ch => value.push(ch),
      }
    }
  }

  /// `\uXXXX`, a surrogate pair of two of them, or `\u{X…}`, after the `\u`.
  fn unicode(&mut self) -> Result<char, String> {
    let hex = |digits: &str| u32::from_str_radix(digits, 16).map_err(|error| error.to_string());
    if self.peek() == Some('{') {
      let close = self.text[self.at..]
        .find('}')
        .ok_or_else(|| "an unclosed `\\u{`".to_owned())?;
      let code = hex(&self.text[self.at + 1..self.at + close])?;
      self.at += close + 1;
      return char::from_u32(code).ok_or_else(|| format!("U+{code:X} is not a character"));
    }
    let four = |reader: &mut Self| {
      let digits = reader
        .text
        .get(reader.at..reader.at + 4)
        .ok_or_else(|| "a short `\\u` escape".to_owned())?;
      reader.at += 4;
      hex(digits)
    };
    let code = four(self)?;
    if (0xd800..0xdc00).contains(&code) && self.text[self.at..].starts_with("\\u") {
      self.at += 2;
      let low = four(self)?;
      if !(0xdc00..0xe000).contains(&low) {
        return Err(format!("U+{code:X} is not followed by a low surrogate"));
      }
      let code = 0x10000 + ((code - 0xd800) << 10) + (low - 0xdc00);
      return char::from_u32(code).ok_or_else(|| format!("U+{code:X} is not a character"));
    }
    char::from_u32(code).ok_or_else(|| format!("U+{code:X} is not a character"))
  }
}

/// A value after graphql-js's input coercion: what `valueFromAST` returns, in JavaScript's terms.
#[derive(Clone, Debug, PartialEq)]
enum Js {
  Null,
  Bool(bool),
  Number(f64),
  Str(String),
  List(Vec<Js>),
  Object(Vec<(String, Js)>),
}

/// graphql-js 16.11.0's `valueFromAST(literal, type)`, with no variables: `None` is its
/// `undefined`, an invalid value.
///
/// `ty` is an introspection type reference and `types` graphql-js's own `__schema.types`, which is
/// where an input object's fields — their order, types and defaults — and an enum's values are read.
fn coerce(literal: &Const, ty: &Value, types: &Types<'_>) -> Option<Js> {
  match ty["kind"].as_str() {
    Some("NON_NULL") => {
      if matches!(literal, Const::Null) {
        return None;
      }
      coerce(literal, &ty["ofType"], types)
    }
    _ if matches!(literal, Const::Null) => Some(Js::Null),
    // A single item where a list is expected is the list of that item.
    Some("LIST") => match literal {
      Const::List(items) => items
        .iter()
        .map(|item| coerce(item, &ty["ofType"], types))
        .collect::<Option<Vec<_>>>()
        .map(Js::List),
      item => coerce(item, &ty["ofType"], types).map(|value| Js::List(vec![value])),
    },
    _ => {
      let name = ty["name"].as_str()?;
      let definition = types.get(name)?;
      match definition["kind"].as_str()? {
        "INPUT_OBJECT" => coerce_input_object(literal, definition, types),
        "ENUM" => {
          let members = definition["enumValues"].as_array()?;
          match literal {
            Const::Enum(value)
              if members
                .iter()
                .any(|member| member["name"] == value.as_str()) =>
            {
              Some(Js::Str(value.clone()))
            }
            _ => None,
          }
        }
        _ => coerce_scalar(literal, name),
      }
    }
  }
}

/// An input object: its fields in the type's order, a written field coerced, an omitted one filled
/// from its default, and `@oneOf` held to exactly one non-null field.
fn coerce_input_object(literal: &Const, definition: &Value, types: &Types<'_>) -> Option<Js> {
  let Const::Object(written) = literal else {
    return None;
  };
  let mut fields = Vec::new();
  for field in definition["inputFields"].as_array()? {
    let name = field["name"].as_str()?;
    // graphql-js keys the written fields by name, so a repeated one is read as its last.
    match written.iter().rev().find(|(key, _)| key.as_str() == name) {
      Some((_, value)) => fields.push((name.to_owned(), coerce(value, &field["type"], types)?)),
      None => match field["defaultValue"].as_str() {
        // graphql-js keeps the coerced default; its printing coerces back to the same value.
        Some(default) => fields.push((
          name.to_owned(),
          coerce(&Const::parse(default).ok()?, &field["type"], types)?,
        )),
        None if field["type"]["kind"] == "NON_NULL" => return None,
        None => {}
      },
    }
  }
  if definition["isOneOf"].as_bool() == Some(true)
    && (fields.len() != 1 || matches!(fields[0].1, Js::Null))
  {
    return None;
  }
  Some(Js::Object(fields))
}

/// A scalar's `parseLiteral`: the five built-in ones as graphql-js defines them, and a custom
/// scalar's as `valueFromASTUntyped`.
fn coerce_scalar(literal: &Const, name: &str) -> Option<Js> {
  match (name, literal) {
    ("Int", Const::Int(spelling)) => {
      let value: i64 = spelling.parse().ok()?;
      if i32::try_from(value).is_err() {
        return None;
      }
      Some(Js::Number(value as f64))
    }
    ("Float", Const::Int(spelling) | Const::Float(spelling)) => {
      spelling.parse::<f64>().ok().map(Js::Number)
    }
    ("String", Const::Str(value)) => Some(Js::Str(value.clone())),
    ("Boolean", Const::Bool(flag)) => Some(Js::Bool(*flag)),
    ("ID", Const::Str(value) | Const::Int(value)) => Some(Js::Str(value.clone())),
    ("Int" | "Float" | "String" | "Boolean" | "ID", _) => None,
    _ => Some(untyped(literal)),
  }
}

/// graphql-js's `valueFromASTUntyped`.
fn untyped(literal: &Const) -> Js {
  match literal {
    Const::Null => Js::Null,
    Const::Bool(flag) => Js::Bool(*flag),
    Const::Int(spelling) | Const::Float(spelling) => {
      Js::Number(spelling.parse().unwrap_or(f64::NAN))
    }
    Const::Str(value) | Const::Enum(value) => Js::Str(value.clone()),
    Const::List(items) => Js::List(items.iter().map(untyped).collect()),
    Const::Object(fields) => Js::Object(
      fields
        .iter()
        .map(|(name, value)| (name.clone(), untyped(value)))
        .collect(),
    ),
  }
}

/// graphql-js 16.11.0's `astFromValue(value, type)`: `None` is its `null`, no value to print.
fn ast_from_value(value: &Js, ty: &Value, types: &Types<'_>) -> Option<Const> {
  match ty["kind"].as_str() {
    Some("NON_NULL") => {
      ast_from_value(value, &ty["ofType"], types).filter(|ast| !matches!(ast, Const::Null))
    }
    _ if matches!(value, Js::Null) => Some(Const::Null),
    Some("LIST") => match value {
      Js::List(items) => Some(Const::List(
        items
          .iter()
          .filter_map(|item| ast_from_value(item, &ty["ofType"], types))
          .collect(),
      )),
      item => ast_from_value(item, &ty["ofType"], types),
    },
    _ => {
      let name = ty["name"].as_str()?;
      let definition = types.get(name)?;
      match definition["kind"].as_str()? {
        "INPUT_OBJECT" => {
          let Js::Object(present) = value else {
            return None;
          };
          let mut fields = Vec::new();
          for field in definition["inputFields"].as_array()? {
            let field_name = field["name"].as_str()?;
            let Some((_, value)) = present.iter().find(|(key, _)| key.as_str() == field_name)
            else {
              continue;
            };
            if let Some(ast) = ast_from_value(value, &field["type"], types) {
              fields.push((field_name.to_owned(), ast));
            }
          }
          Some(Const::Object(fields))
        }
        "ENUM" => match value {
          Js::Str(member) => Some(Const::Enum(member.clone())),
          _ => None,
        },
        _ => serialize(value, name),
      }
    }
  }
}

/// A scalar's `serialize`, and the node `astFromValue` makes of what it returns.
fn serialize(value: &Js, name: &str) -> Option<Const> {
  let number = |number: f64| {
    let text = js_number(number);
    Some(if is_integer(&text) {
      Const::Int(text)
    } else {
      Const::Float(text)
    })
  };
  match (name, value) {
    ("Int" | "Float", Js::Number(n)) if n.is_finite() => number(*n),
    ("String", Js::Str(text)) => Some(Const::Str(text.clone())),
    ("Boolean", Js::Bool(flag)) => Some(Const::Bool(*flag)),
    // An `ID` that is a string of digits is printed as an integer.
    ("ID", Js::Str(text)) if is_integer(text) => Some(Const::Int(text.clone())),
    ("ID", Js::Str(text)) => Some(Const::Str(text.clone())),
    ("Int" | "Float" | "String" | "Boolean" | "ID", _) => None,
    // A custom scalar serialises as itself.
    (_, Js::Bool(flag)) => Some(Const::Bool(*flag)),
    (_, Js::Number(n)) if n.is_finite() => number(*n),
    (_, Js::Str(text)) => Some(Const::Str(text.clone())),
    _ => None,
  }
}

/// graphql-js's `integerStringRegExp`, `^-?(?:0|[1-9][0-9]*)$`.
fn is_integer(text: &str) -> bool {
  match text.strip_prefix('-').unwrap_or(text).as_bytes() {
    [b'0'] => true,
    [b'1'..=b'9', rest @ ..] => rest.iter().all(u8::is_ascii_digit),
    _ => false,
  }
}

/// JavaScript's `String(number)` for a finite number: the shortest digits that round-trip, written
/// out in full between 10⁻⁷ and 10²¹ and in exponent form outside it.
fn js_number(number: f64) -> String {
  if number == 0.0 {
    return "0".to_owned();
  }
  if number < 0.0 {
    return format!("-{}", js_number(-number));
  }
  let scientific = format!("{number:e}");
  let (mantissa, exponent) = scientific
    .split_once('e')
    .expect("an exponent form has an exponent");
  let exponent: i32 = exponent.parse().expect("the exponent is an integer");
  let digits: String = mantissa.chars().filter(char::is_ascii_digit).collect();
  let count = digits.len() as i32;
  // The decimal point sits after this many of the digits.
  let point = exponent + 1;
  if (count..=21).contains(&point) {
    format!("{digits}{}", "0".repeat((point - count) as usize))
  } else if (1..=21).contains(&point) {
    let (whole, fraction) = digits.split_at(point as usize);
    format!("{whole}.{fraction}")
  } else if (-5..=0).contains(&point) {
    format!("0.{}{digits}", "0".repeat(-point as usize))
  } else {
    let shown = point - 1;
    let sign = if shown < 0 { '-' } else { '+' };
    let (first, rest) = digits.split_at(1);
    if rest.is_empty() {
      format!("{first}e{sign}{}", shown.abs())
    } else {
      format!("{first}.{rest}e{sign}{}", shown.abs())
    }
  }
}

// ------------------------------------------------------------------------------------------
// the normaliser's own pieces, against graphql-js's real output
// ------------------------------------------------------------------------------------------

/// The number formatting difference 3 rests on reproduces JavaScript's, read off node running
/// `String(n)`, at each of its four forms and both of their edges.
#[test]
fn javascripts_number_formatting_is_reproduced() {
  for (number, javascript) in [
    (1.0, "1"),
    (1500.0, "1500"),
    (-0.0025, "-0.0025"),
    (2.5, "2.5"),
    (100.0, "100"),
    (0.1, "0.1"),
    (123.456, "123.456"),
    (1e21, "1e+21"),
    (1.2345678901234568e20, "123456789012345680000"),
    (1e-6, "0.000001"),
    (1e-7, "1e-7"),
    (1.5e-7, "1.5e-7"),
    (5e-324, "5e-324"),
    (f64::MAX, "1.7976931348623157e+308"),
    (-0.0, "0"),
  ] {
    assert_eq!(js_number(number), javascript, "{number:e}");
  }
}

/// The string printer reproduces graphql-js's `printString`, read off node, and the parser reads
/// back what it wrote.
#[test]
fn graphql_jss_print_string_is_reproduced() {
  for (value, graphql_js) in [
    ("plain", r#""plain""#),
    ("a\"b\\c", r#""a\"b\\c""#),
    ("tab\there", r#""tab\there""#),
    ("nl\n", r#""nl\n""#),
    (
      "\u{1}\u{1f}\u{7f}\u{80}\u{9f}\u{a0}",
      "\"\\u0001\\u001F\\u007F\\u0080\\u009F\u{a0}\"",
    ),
    ("é—\u{2028}", "\"é—\u{2028}\""),
  ] {
    let mut printed = String::new();
    print_string(value, &mut printed);
    assert_eq!(printed, graphql_js, "{value:?}");
    assert_eq!(
      Const::parse(&printed),
      Ok(Const::Str(value.to_owned())),
      "{printed}"
    );
  }
}
