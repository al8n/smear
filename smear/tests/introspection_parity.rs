#![cfg(all(any(feature = "std", feature = "alloc"), feature = "proto"))]

//! The executor's draft §4 answer, against graphql-js 17.1.0's answer for the same SDL.
//!
//! # What is compared, and against what
//!
//! For each fixture below, smear builds the [`Schema`] from the SDL, executes graphql-js's full
//! introspection query — the committed `introspection_query.graphql`, `getIntrospectionQuery` with
//! seven of its eight options on and `typeDepth` at its default of 9 — with the `proto` executor,
//! writes the response with `smear::json`, and reads its `data` back with `serde_json`.
//!
//! graphql-js's side is a committed golden under `tests/oracle/graphql-js-introspection/`:
//! `introspectionFromSchema(buildSchema(sdl), options)` with the same eight options, written by
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
//! Each was checked against graphql-js 17.1.0's real output, and against smear's answer where one
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
//! 3. **`defaultValue`'s print form.** Both sides print the literal the document wrote, and draft
//!    §4.2.4 asks only for "a String encoding (using the GraphQL language)" of the default, naming
//!    no form. The two forms differ: smear's is graphql-js 16's, `{a: 1}` and `{}` with every
//!    string inline, and graphql-js 17 writes `{ a: 1 }` and `{  }`, keeps a block string a block
//!    string, and breaks a list or an object past eighty columns across lines. So two texts that
//!    differ are each parsed as a constant value and compared as values: an integer, a float and an
//!    enum value by kind and spelling, so `1500` is not `1.5e3` and `1` is not `1.0`; a string by
//!    its value, a block string's as draft §2.10.4's `BlockStringValue` gives it; a list item by
//!    item; and an object field by field, in order. smear's own text is held to its form rather
//!    than absorbed: it must parse and print back to itself, so a change to smear's printer fails
//!    here.
//! 4. **A directive's `locations` order.** smear lists them in `DirectiveLocation` bit order and
//!    graphql-js as declared. Compared as a set, which for `@deprecated` holds the draft's five
//!    locations on both sides.
//! 5. **`__schema.directives` order.** Both list the document's directives first; graphql-js then
//!    appends the specified ones as `include`, `skip`, `deprecated`, `specifiedBy`, `oneOf`, and
//!    smear as `skip`, `include`, …. Compared as a set keyed by name.
//! 6. **An interface's `possibleTypes` order.** Compared as a set; a union's members keep their
//!    declared order, and that is compared. smear lists an interface's implementations in object
//!    order and graphql-js in its type map's order, and for a schema built from SDL both are the
//!    document's order — so this one has never fired: no fixture differs, and none can.
//! 7. **`__DirectiveLocation`'s `FRAGMENT_VARIABLE_DEFINITION`.** graphql-js 17 lists it after
//!    `VARIABLE_DEFINITION`, for the fragment variables it parses. The draft's
//!    `__DirectiveLocation` (§4.2) has twenty values, ending `INPUT_FIELD_DEFINITION` and
//!    `DIRECTIVE_DEFINITION`, and this is not one of them. Exactly that value is set aside from
//!    graphql-js's listing, once, and the rest is compared in order.
//! 8. **`VARIABLE_DEFINITION`'s description.** graphql-js 17 describes it as "Location adjacent to
//!    an operation variable definition.", to tell it from 7, and smear keeps graphql-js 16's
//!    "Location adjacent to a variable definition.": the draft gives the meta-schema no
//!    descriptions. Accepted for exactly that pair of texts, on exactly that value.
//!
//! Everything else must be equal: descriptions, `isDeprecated` and `deprecationReason` — a
//! directive's included — `specifiedByURL`, `isOneOf`, interfaces, a union's possible types, enum
//! values, input fields, directives and their arguments, every `ofType` chain, the root types, and
//! every type's fields in order, `__Directive`'s among them.
//!
//! # Which difference fires where
//!
//! Recorded per fixture and asserted, so the table is a checked claim rather than a note:
//!
//! | difference | `agreement` | `github` | `gitlab` | `defaults` |
//! |---|---|---|---|---|
//! | 1. types reordered | yes | yes | yes | yes |
//! | 2. extra built-in scalars | `Float` | `Float` | none | none |
//! | 3. defaults printed in another form | 2 | 0 | 7 | 1 |
//! | 4. locations reordered | `@auth`, `@deprecated` | `@deprecated` | `@deprecated` | `@deprecated` |
//! | 5. directives reordered | yes | yes | yes | yes |
//! | 6. possible types reordered | none | none | none | none |
//! | 7. `FRAGMENT_VARIABLE_DEFINITION` set aside | yes | yes | yes | yes |
//! | 8. `VARIABLE_DEFINITION`'s description | yes | yes | yes | yes |
//!
//! The agreement fixture — the one `validator_introspection.rs` holds every construct a schema can
//! in, a deprecated directive and an `extend directive` among them — and GitHub's and GitLab's
//! schemas show every difference but 6 between them, and no schema built from SDL can show that
//! one. `defaults` exists for this file alone: numbers, enum values and strings spelled where two
//! print forms could part — both print them as written, so they must agree to the byte — and an
//! input object the two print each in its own form.

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

/// graphql-js 17.1.0's `getIntrospectionQuery` with seven of its eight options on and `typeDepth`
/// at 9, byte for byte.
const QUERY: &str = include_str!("fixtures/executables/introspection_query.graphql");

/// [`QUERY`]'s path from the workspace root, as a golden's provenance records it.
const QUERY_PATH: &str = "smear/tests/fixtures/executables/introspection_query.graphql";

/// The release every golden is graphql-js's answer from.
const RELEASE: &str = "17.1.0";

/// The five built-in scalars, which smear injects whether or not anything references them.
const BUILT_IN_SCALARS: [&str; 5] = ["Int", "Float", "String", "Boolean", "ID"];

/// The built-in scalars difference 2 may set aside: the meta-schema references the other two.
const UNREFERENCED_SCALARS: [&str; 3] = ["Int", "Float", "ID"];

/// The `__DirectiveLocation` value graphql-js 17 adds and the draft does not define: difference 7.
const FRAGMENT_VARIABLE_DEFINITION: &str = "FRAGMENT_VARIABLE_DEFINITION";

/// The `__DirectiveLocation` value whose description is difference 8.
const VARIABLE_DEFINITION: &str = "VARIABLE_DEFINITION";

/// [`VARIABLE_DEFINITION`]'s description in smear's meta-schema, which is graphql-js 16's.
const SMEARS_VARIABLE_DEFINITION: &str = "Location adjacent to a variable definition.";

/// [`VARIABLE_DEFINITION`]'s description in graphql-js 17's.
const GRAPHQL_JSS_VARIABLE_DEFINITION: &str =
  "Location adjacent to an operation variable definition.";

/// One schema, the golden graphql-js answered it with, and what the comparison has to record.
struct Fixture {
  /// The golden's name under `tests/oracle/graphql-js-introspection/`.
  name: &'static str,
  /// The SDL's path from the workspace root, as the golden's provenance records it.
  path: &'static str,
  sdl: &'static str,
  /// Difference 2's set.
  extra_scalars: &'static [&'static str],
  /// How many defaults difference 3 found printed in another form.
  defaults_reprinted: usize,
  /// The directives difference 4 found listed out of bit order.
  locations_reordered: &'static [&'static str],
}

const AGREEMENT: Fixture = Fixture {
  name: "agreement",
  path: "smear/tests/fixtures/schemas/introspection_agreement.graphql",
  sdl: include_str!("fixtures/schemas/introspection_agreement.graphql"),
  extra_scalars: &["Float"],
  defaults_reprinted: 2,
  locations_reordered: &["auth", "deprecated"],
};

const GITHUB: Fixture = Fixture {
  name: "github",
  path: "smear/tests/fixtures/schemas/github_schema.graphql",
  sdl: include_str!("fixtures/schemas/github_schema.graphql"),
  extra_scalars: &["Float"],
  defaults_reprinted: 0,
  locations_reordered: &["deprecated"],
};

const GITLAB: Fixture = Fixture {
  name: "gitlab",
  path: "smear/tests/fixtures/schemas/gitlab_schema.graphql",
  sdl: include_str!("fixtures/schemas/gitlab_schema.graphql"),
  extra_scalars: &[],
  defaults_reprinted: 7,
  locations_reordered: &["deprecated"],
};

const DEFAULTS: Fixture = Fixture {
  name: "defaults",
  path: "smear/tests/fixtures/schemas/introspection_defaults.graphql",
  sdl: include_str!("fixtures/schemas/introspection_defaults.graphql"),
  extra_scalars: &[],
  defaults_reprinted: 1,
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
  defaults_reprinted: usize,
  /// Difference 4.
  locations_reordered: BTreeSet<String>,
  /// Difference 5.
  directives_reordered: bool,
  /// Difference 6.
  possible_types_reordered: BTreeSet<String>,
  /// Difference 7.
  fragment_variable_definition: bool,
  /// Difference 8.
  variable_definition_described: bool,
}

impl Record {
  /// What `fixture` has to record: its own three rows of the table, and the rows every fixture
  /// shares.
  fn expected(fixture: &Fixture) -> Self {
    Self {
      types_reordered: true,
      extra_scalars: names(fixture.extra_scalars),
      defaults_reprinted: fixture.defaults_reprinted,
      locations_reordered: names(fixture.locations_reordered),
      directives_reordered: true,
      possible_types_reordered: BTreeSet::new(),
      fragment_variable_definition: true,
      variable_definition_described: true,
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

/// Defaults spelled where the two print forms could part, at spellings no other fixture holds.
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

  let mut comparison = Comparison::default();
  comparison.data(&smear, graphql_js);
  let Comparison {
    unexplained,
    record,
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
      "experimentalDirectiveDeprecation": true,
      "oneOf": true,
      "typeDepth": 9,
    }),
    "{name}: the golden's options are not the eight the committed query was generated with"
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

/// One comparison of two answers: what no rule explains, and how often each rule was needed.
#[derive(Default)]
struct Comparison {
  unexplained: Vec<String>,
  record: Record,
}

impl Comparison {
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
  fn data(&mut self, smear: &Value, graphql_js: &Value) {
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

  fn schema(&mut self, smear: &Value, graphql_js: &Value) {
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
          // Difference 5: compared as a set keyed by name.
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
    graphql_js: &Value,
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
  fn full_type(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
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
          self.list(&here, ours, theirs, true, Self::field);
        }
        "inputFields" => {
          self.list(&here, ours, theirs, true, Self::input_value);
        }
        "enumValues" if name == "__DirectiveLocation" => {
          // Differences 7 and 8.
          let theirs = self.without_fragment_variable_definition(&here, theirs);
          self.list(&here, ours, &theirs, true, Self::directive_location);
        }
        "enumValues" => {
          self.list(&here, ours, theirs, true, Self::enum_value);
        }
        "interfaces" => {
          self.list(&here, ours, theirs, true, Self::member);
        }
        "possibleTypes" => {
          // Difference 6: an interface's are compared as a set, and a union's in order.
          let ordered = kind != "INTERFACE";
          if self.list(&here, ours, theirs, ordered, Self::member) && !ordered {
            self.record.possible_types_reordered.insert(name.to_owned());
          }
        }
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  fn field(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      let here = format!("{path}.{key}");
      match key.as_str() {
        "args" => {
          self.list(&here, ours, theirs, true, Self::input_value);
        }
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  fn input_value(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      let here = format!("{path}.{key}");
      match key.as_str() {
        "defaultValue" => self.default_value(&here, ours, theirs),
        _ => self.exact(&here, ours, theirs),
      }
    }
  }

  fn enum_value(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    if !self.same_keys(path, smear, graphql_js) {
      return;
    }
    for (key, theirs) in graphql_js.as_object().into_iter().flatten() {
      let Some(ours) = smear.get(key) else { continue };
      self.exact(&format!("{path}.{key}"), ours, theirs);
    }
  }

  /// Difference 7: graphql-js's `__DirectiveLocation` values without
  /// [`FRAGMENT_VARIABLE_DEFINITION`], which has to be among them exactly once.
  fn without_fragment_variable_definition(&mut self, path: &str, graphql_js: &Value) -> Value {
    let Some(values) = graphql_js.as_array() else {
      return graphql_js.clone();
    };
    let kept: Vec<Value> = values
      .iter()
      .filter(|value| value["name"] != FRAGMENT_VARIABLE_DEFINITION)
      .cloned()
      .collect();
    match values.len() - kept.len() {
      1 => self.record.fragment_variable_definition = true,
      count => self.differ(
        path,
        format!("graphql-js lists `{FRAGMENT_VARIABLE_DEFINITION}` {count} times rather than once"),
      ),
    }
    Value::Array(kept)
  }

  /// One of `__DirectiveLocation`'s values, compared as any enum value is, save difference 8:
  /// [`VARIABLE_DEFINITION`]'s description, smear's text against graphql-js 17's and no other.
  fn directive_location(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    if graphql_js["name"] == VARIABLE_DEFINITION
      && smear["description"] == SMEARS_VARIABLE_DEFINITION
      && graphql_js["description"] == GRAPHQL_JSS_VARIABLE_DEFINITION
    {
      self.record.variable_definition_described = true;
      let mut theirs = graphql_js.clone();
      theirs["description"] = smear["description"].clone();
      return self.enum_value(path, smear, &theirs);
    }
    self.enum_value(path, smear, graphql_js);
  }

  /// An interface, a possible type or a list member compared whole: a named type reference.
  fn member(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    self.exact(path, smear, graphql_js);
  }

  fn directive(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
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

  /// Difference 4: a directive's locations, compared as a set. Answers whether the order differed.
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

  /// Difference 3: two texts of one default, compared as the constant values they spell — smear's
  /// held to its own print form first, so a change to its printer fails rather than being
  /// absorbed.
  fn default_value(&mut self, path: &str, smear: &Value, graphql_js: &Value) {
    if smear == graphql_js {
      return;
    }
    let (Some(ours), Some(theirs)) = (smear.as_str(), graphql_js.as_str()) else {
      return self.exact(path, smear, graphql_js);
    };
    let literal = match Const::parse(ours) {
      Ok(literal) => literal,
      Err(error) => return self.differ(path, format!("smear's `{ours}` does not parse: {error}")),
    };
    let printed = literal.print();
    if printed != ours {
      return self.differ(
        path,
        format!("smear's `{ours}` is not in print form, which prints it `{printed}`"),
      );
    }
    match Const::parse(theirs) {
      Ok(value) if value == literal => self.record.defaults_reprinted += 1,
      Ok(_) => self.differ(path, format!("smear `{ours}` | graphql-js `{theirs}`")),
      Err(error) => self.differ(
        path,
        format!("graphql-js's `{theirs}` does not parse: {error}"),
      ),
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
    graphql_js: &Value,
    ordered: bool,
    each: fn(&mut Self, &str, &Value, &Value),
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
// difference 3: a printed default, read back as the constant value it spells
// ------------------------------------------------------------------------------------------

/// A constant GraphQL value, as parsed out of a printed default.
///
/// Equality is difference 3's: an integer, a float and an enum value compare by kind and by
/// spelling, a string by its value, a list item by item, and an object field by field in order.
#[derive(Clone, Debug, PartialEq)]
enum Const {
  Null,
  Bool(bool),
  /// An integer, as spelled.
  Int(String),
  /// A float, as spelled.
  Float(String),
  /// A string's value: an inline string's escapes decoded, a block string's lines as draft
  /// §2.10.4's `BlockStringValue` gives them.
  Str(String),
  Enum(String),
  List(Vec<Const>),
  Object(Vec<(String, Const)>),
}

impl Const {
  /// Parses a constant value: the strings, block strings included, numbers, names, lists and
  /// objects either side's printed default is made of, in either side's spacing.
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

  /// smear's print form of the value, which is graphql-js 16's `print` of a value node.
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

  /// A string: an inline one with its escapes decoded, or a block string.
  fn string(&mut self) -> Result<Const, String> {
    if self.text[self.at..].starts_with("\"\"\"") {
      return self.block_string();
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

  /// A block string, after its opening `"""`: its characters as written, `\"""` standing for
  /// `"""`, up to the closing `"""`, and its value as draft §2.10.4's `BlockStringValue` gives it.
  fn block_string(&mut self) -> Result<Const, String> {
    self.at += 3;
    let mut raw = String::new();
    loop {
      let rest = &self.text[self.at..];
      if rest.starts_with("\\\"\"\"") {
        raw.push_str("\"\"\"");
        self.at += 4;
      } else if rest.starts_with("\"\"\"") {
        self.at += 3;
        return Ok(Const::Str(block_string_value(&raw)));
      } else {
        let ch = rest
          .chars()
          .next()
          .ok_or_else(|| "an unterminated block string".to_owned())?;
        raw.push(ch);
        self.at += ch.len_utf8();
      }
    }
  }
}

/// Draft §2.10.4's `BlockStringValue`: the raw value split at every line terminator, the common
/// indentation of every line but the first taken off, the leading and the trailing lines of
/// nothing but white space dropped, and what is left joined with line feeds.
fn block_string_value(raw: &str) -> String {
  let white = |ch: char| ch == ' ' || ch == '\t';
  let bytes = raw.as_bytes();
  let mut lines: Vec<&str> = Vec::new();
  let (mut start, mut at) = (0, 0);
  while at < bytes.len() {
    match bytes[at] {
      b'\n' => {
        lines.push(&raw[start..at]);
        at += 1;
        start = at;
      }
      b'\r' => {
        lines.push(&raw[start..at]);
        at += if bytes.get(at + 1) == Some(&b'\n') {
          2
        } else {
          1
        };
        start = at;
      }
      _ => at += 1,
    }
  }
  lines.push(&raw[start..]);
  let common = lines
    .iter()
    .skip(1)
    .filter_map(|line| {
      let indent = line.chars().take_while(|&ch| white(ch)).count();
      (indent < line.chars().count()).then_some(indent)
    })
    .min();
  if let Some(common) = common {
    for line in lines.iter_mut().skip(1) {
      // The indentation is spaces and tabs, a byte each, so a count of them is a byte offset — and
      // a line of white space shorter than it loses the whole of itself.
      let whole = *line;
      *line = &whole[common.min(whole.len())..];
    }
  }
  while lines.first().is_some_and(|line| line.chars().all(white)) {
    lines.remove(0);
  }
  while lines.last().is_some_and(|line| line.chars().all(white)) {
    lines.pop();
  }
  lines.join("\n")
}

// ------------------------------------------------------------------------------------------
// difference 3's own pieces
// ------------------------------------------------------------------------------------------

/// Two printings of one constant value read back as one value, whichever form printed them, and
/// two values stay two however alike their spellings: difference 3's rule, at its edges.
#[test]
fn printed_defaults_compare_as_values() {
  let same = |ours: &str, theirs: &str| {
    assert!(Const::parse(ours).is_ok(), "`{ours}` parses");
    assert_eq!(
      Const::parse(ours),
      Const::parse(theirs),
      "`{ours}` and `{theirs}`"
    );
  };
  // An object and an empty one in both forms, and a string inline and as a block string.
  same(
    "{published: false, ranked: [[1, null], []]}",
    "{ published: false, ranked: [[1, null], []] }",
  );
  same("{}", "{  }");
  same(r#""the \"newest\" first""#, r#""""the "newest" first""""#);
  // A block string's value: the common indentation and the blank first and last lines go, an
  // escaped triple quote is a triple quote, and a line terminator of either kind is a line feed.
  same(
    "\"multi\\n  line\"",
    "\"\"\"\n    multi\n      line\n  \"\"\"",
  );
  same("\"a\\\"\\\"\\\"b\"", "\"\"\"a\\\"\"\"b\"\"\"");
  same("\"x\\ny\"", "\"\"\"x\r\ny\"\"\"");
  // A list broken across lines, as graphql-js 17 prints one past eighty columns.
  same("[1, 2]", "[\n  1,\n  2\n]");

  let apart = |ours: &str, theirs: &str| {
    assert_ne!(
      Const::parse(ours),
      Const::parse(theirs),
      "`{ours}` and `{theirs}`"
    );
  };
  // A number and an enum value by kind and spelling, and an object's fields in order.
  apart("1500", "1.5e3");
  apart("1", "1.0");
  apart("RED", "\"RED\"");
  apart("{a: 1, b: 2}", "{b: 2, a: 1}");
}

/// The string printer of smear's print form reproduces graphql-js's `printString`, read off node,
/// and the parser reads back what it wrote.
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
