#![cfg(feature = "proto")]

//! The draft §6.2.2 oracle: `graphql-js`'s `mutations-test.ts`, hand-ported case for case.
//!
//! # Why this file and not another case in `proto_execute.rs`
//!
//! Serial execution is an *ordering* property, and an ordering property is the kind a test suite
//! can assert vacuously. A Sans-I/O executor is driven one request at a time, so a driver that
//! answers each request before asking for the next serialises everything it is handed — mutation or
//! query — and every assertion about the resulting `data` would pass with §6.2.2 deleted.
//! `proto_execute.rs` closes that by polling twice without answering: it asserts what the executor
//! is *willing to have outstanding*, which is where the rule lives.
//!
//! This file asserts the other half — that the rule produces the answers the reference
//! implementation produces — and it does so on upstream's own fixture, whose whole design is that
//! the answer depends on the order. Each mutation writes one number and each sub-selection reads it,
//! so parallel execution does not fail on a technicality; it returns `theNumber: 5` five times.
//!
//! # Hand-ported, and what that costs
//!
//! Same as `proto_nonnull_oracle.rs`, whose header argues the case at length: the values are
//! closures, the schema is programmatic, and mechanical extraction was measured and rejected. Each
//! [`Case`] records the upstream line and a verbatim fragment of it, and with `SMEAR_GRAPHQL_JS`
//! pointing at a checkout [`upstream_has_not_drifted`] re-reads the file and fails if an anchor has
//! moved, if a schema member has been renamed, or if upstream has *added* a case this port lacks.
//!
//! # Coverage, stated as a number
//!
//! All **3** of `mutations-test.ts`'s `it(...)` blocks.
//!
//! # Four deliberate divergences, each with a reason
//!
//! - **The schema is a translation, not a copy.** Upstream builds it with `new GraphQLObjectType`,
//!   so [`SDL`] is the SDL that schema denotes. The drift gate checks the member names instead of
//!   the text, which is the most a port can do against a programmatic schema.
//! - **`promise*` and `sync*` resolvers are the same resolver.** `proto` is Sans-I/O and has no
//!   notion of a value that is not yet available, so upstream's `promiseToChangeTheNumber` and
//!   `immediatelyChangeTheNumber` differ only in a name. That is not a hole in the port: the
//!   ordering the promise variants exist to perturb is *this executor's* to decide, and the cases
//!   below keep both spellings so the interleaving upstream wrote is the interleaving that runs.
//! - **One ported document is not a valid one.** `mutation { thisIsIllegalDoNotIncludeMe }` names a
//!   field the schema does not define, which draft §5.3.1 rejects — and `proto` documents itself as
//!   entered with a validated document. It is ported anyway because the executor's behaviour on it
//!   is defined and is upstream's: a key it cannot invent is a key it omits.
//!   [`the_illegal_case_is_the_one_invalid_document`] asserts the premise rather than asserting it
//!   in prose.
//! - **Errors are compared as a set, ordered by path**, as the nonnull port does and for the same
//!   reason: draft §7.1.2 does not fix the order of `errors`.

use std::collections::BTreeSet;

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, InputValue, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  proto::{Argument, ArgumentSource, Executor, Leaf, Node, Values},
  validator::{Budget, First, Schema, Scratch, validate_executable},
};

/// The `graphql-js` commit this port was read from: the `v16.11.0` tag.
const UPSTREAM_COMMIT: &str = "c18e9f6aada9ae086ddf836e4d822cf1426f3868";

/// The file every [`Case::line`] below indexes.
const UPSTREAM_FILE: &str = "src/execution/__tests__/mutations-test.ts";

/// `mutations-test.ts:41` and `:46`, the message both failing resolvers throw.
const CANNOT_CHANGE: &str = "Cannot change the number";

/// Upstream's `new Root(6)`.
const ORIGINAL_NUMBER: i64 = 6;

// ------------------------------------------------------------------------------------------
// the driver
// ------------------------------------------------------------------------------------------

/// The driver's value representation.
///
/// `Holder` is upstream's `NumberHolder`, and it carries no number: upstream's holder is a
/// *reference* into the one `Root` every mutation writes, so a value that copied the number would
/// make each field read the number as it stood when the field resolved rather than when its
/// sub-selection did — which is the difference the ordering case turns on.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Val {
  Null,
  Int(i64),
  Holder,
}

/// The mutable world, which is the whole point of a mutation.
struct Space {
  /// Upstream's `Root.numberHolder.theNumber`. Every top-level field writes it and every
  /// `theNumber` reads it, so `data` records the order the two interleaved in.
  number: i64,
}

impl Values for Space {
  type Value = Val;

  fn is_null(&self, value: &Val) -> bool {
    matches!(value, Val::Null)
  }

  fn as_bool(&self, _: &Val) -> Option<bool> {
    None
  }

  fn list_len(&self, _: &Val) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &Val, _: usize) -> Val {
    Val::Null
  }

  fn type_name<'a>(&'a self, _: &'a Val) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: Val, _: Leaf<'_>) -> Option<Val> {
    Some(value)
  }

  fn variable(&mut self, _: &str) -> Option<Val> {
    None
  }
}

/// What the driver does with one request, decided while the request is still borrowed.
enum Answer {
  /// One of the two changing resolvers: write the number, return the holder.
  Set(i64),
  /// One of the two failing resolvers.
  Fail(&'static str),
  /// `theNumber`, which reads the number as it stands now.
  Read,
}

fn plan(name: &str, arguments: &[Argument<'_, &str, Val>]) -> Answer {
  match name {
    "immediatelyChangeTheNumber" | "promiseToChangeTheNumber" => Answer::Set(new_number(arguments)),
    "failToChangeTheNumber" | "promiseAndFailToChangeTheNumber" => Answer::Fail(CANNOT_CHANGE),
    "theNumber" => Answer::Read,
    other => panic!("the ported corpus has no resolver for `{other}`"),
  }
}

fn new_number(arguments: &[Argument<'_, &str, Val>]) -> i64 {
  let argument = arguments
    .iter()
    .find(|argument| argument.name() == "newNumber")
    .expect("every changing field in the corpus is written with `newNumber`");
  match argument.source() {
    ArgumentSource::Literal(InputValue::Int(literal)) => (*literal.source())
      .parse()
      .expect("upstream writes `newNumber` as a small integer literal"),
    other => panic!("`newNumber` is an integer literal in every ported case, and got {other:?}"),
  }
}

// ------------------------------------------------------------------------------------------
// the corpus
// ------------------------------------------------------------------------------------------

/// `mutations-test.ts:50-97`, which upstream builds with `new GraphQLObjectType`. Rendered as the
/// SDL it denotes; [`upstream_has_not_drifted`] checks the member names against the file.
const SDL: &str = r#"
  type NumberHolder {
    theNumber: Int
  }

  type Query {
    numberHolder: NumberHolder
  }

  type Mutation {
    immediatelyChangeTheNumber(newNumber: Int): NumberHolder
    promiseToChangeTheNumber(newNumber: Int): NumberHolder
    failToChangeTheNumber(newNumber: Int): NumberHolder
    promiseAndFailToChangeTheNumber(newNumber: Int): NumberHolder
  }
"#;

/// Every schema member the port depends on, checked against the upstream file by name.
const MEMBERS: &[&str] = &[
  "NumberHolder",
  "theNumber",
  "numberHolder",
  "immediatelyChangeTheNumber",
  "promiseToChangeTheNumber",
  "failToChangeTheNumber",
  "promiseAndFailToChangeTheNumber",
  "newNumber",
];

/// One expected `errors` entry.
struct ExpectedError {
  message: &'static str,
  /// The draft §7.1.2 response path, rendered as `a.b.0.c`.
  path: &'static str,
  /// Upstream's sole `locations` entry, 1-based, recomputed here from the query text and the span.
  line: u32,
  column: u32,
}

/// One ported `it(...)` block.
struct Case {
  /// Upstream's `it(...)` title, verbatim.
  name: &'static str,
  /// The 1-based line of that `it(` in [`UPSTREAM_FILE`].
  line: u32,
  /// A verbatim fragment of that line, for [`upstream_has_not_drifted`].
  anchor: &'static str,
  /// The query, including the leading newline of upstream's template literal, so a recomputed line
  /// and column can be compared with upstream's.
  query: &'static str,
  /// `data`, rendered by [`render`].
  data: &'static str,
  errors: &'static [ExpectedError],
}

const Q_SERIAL: &str = r#"
      mutation M {
        first: immediatelyChangeTheNumber(newNumber: 1) {
          theNumber
        },
        second: promiseToChangeTheNumber(newNumber: 2) {
          theNumber
        },
        third: immediatelyChangeTheNumber(newNumber: 3) {
          theNumber
        }
        fourth: promiseToChangeTheNumber(newNumber: 4) {
          theNumber
        },
        fifth: immediatelyChangeTheNumber(newNumber: 5) {
          theNumber
        }
      }
    "#;

const Q_ILLEGAL: &str = "mutation { thisIsIllegalDoNotIncludeMe }";

const Q_FAILING: &str = r#"
      mutation M {
        first: immediatelyChangeTheNumber(newNumber: 1) {
          theNumber
        },
        second: promiseToChangeTheNumber(newNumber: 2) {
          theNumber
        },
        third: failToChangeTheNumber(newNumber: 3) {
          theNumber
        }
        fourth: promiseToChangeTheNumber(newNumber: 4) {
          theNumber
        },
        fifth: immediatelyChangeTheNumber(newNumber: 5) {
          theNumber
        }
        sixth: promiseAndFailToChangeTheNumber(newNumber: 6) {
          theNumber
        }
      }
    "#;

const CASES: &[Case] = &[
  Case {
    name: "evaluates mutations serially",
    line: 100,
    anchor: "it('evaluates mutations serially', async () => {",
    query: Q_SERIAL,
    data: r#"{"first":{"theNumber":1},"second":{"theNumber":2},"third":{"theNumber":3},"fourth":{"theNumber":4},"fifth":{"theNumber":5}}"#,
    errors: &[],
  },
  Case {
    name: "does not include illegal mutation fields in output",
    line: 135,
    anchor: "it('does not include illegal mutation fields in output', () => {",
    query: Q_ILLEGAL,
    data: "{}",
    errors: &[],
  },
  Case {
    name: "evaluates mutations correctly in the presence of a failed mutation",
    line: 144,
    anchor: "it('evaluates mutations correctly in the presence of a failed mutation', async () => {",
    query: Q_FAILING,
    data: r#"{"first":{"theNumber":1},"second":{"theNumber":2},"third":null,"fourth":{"theNumber":4},"fifth":{"theNumber":5},"sixth":null}"#,
    errors: &[
      ExpectedError {
        message: CANNOT_CHANGE,
        path: "third",
        line: 9,
        column: 9,
      },
      ExpectedError {
        message: CANNOT_CHANGE,
        path: "sixth",
        line: 18,
        column: 9,
      },
    ],
  },
];

// ------------------------------------------------------------------------------------------
// the harness
// ------------------------------------------------------------------------------------------

/// One run's `data` and every error, as `(message, path, line, column)`.
struct Outcome {
  data: String,
  errors: Vec<(String, String, u32, u32)>,
}

fn run(case: &Case) -> Outcome {
  let (schema, document) = compile(SDL, case.query);
  let mut space = Space {
    number: ORIGINAL_NUMBER,
  };
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Val::Holder)
    .expect("the operation resolves");

  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = plan(request.name(), request.arguments());
    match answer {
      Answer::Set(number) => {
        space.number = number;
        executor.handle_resolved(&mut space, id, Val::Holder);
      }
      Answer::Fail(message) => executor.handle_field_error(id, message),
      Answer::Read => {
        let number = space.number;
        executor.handle_resolved(&mut space, id, Val::Int(number));
      }
    }
    while executor.poll_abandoned().is_some() {}
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  Outcome {
    data: render(&response.data()),
    errors: response
      .errors()
      .map(|error| {
        let locations = error.locations();
        assert_eq!(
          locations.len(),
          1,
          "no ported query selects a response key twice, so upstream emits one location per error"
        );
        let (line, column) = line_column(case.query, locations[0].start());
        (error.to_string(), error.path().to_string(), line, column)
      })
      .collect(),
  }
}

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

fn is_valid(sdl: &str, query: &str) -> bool {
  let (schema, document) = compile(sdl, query);
  let mut scratch = Scratch::new();
  let budget = Budget::default();
  let mut sink = First::new();
  validate_executable(&schema, &document, &mut scratch, &budget, &mut sink).is_ok()
}

/// Renders a response value the way upstream's expectations are written.
fn render(node: &Node<'_, Val>) -> String {
  match node {
    Node::Null => "null".to_owned(),
    Node::Leaf(Val::Int(number)) => number.to_string(),
    // Neither can reach a leaf — a null leaf is draft §6.4.3 step 4's field error, and a holder is
    // an object position — so a marker rather than a panic, which keeps a divergence readable as a
    // diff instead of a backtrace.
    Node::Leaf(_) => "<not-a-number>".to_owned(),
    Node::TypeName(name) => format!("\"{name}\""),
    Node::List(children) => {
      let mut out = String::from("[");
      for (index, (_, child)) in children.clone().enumerate() {
        if index > 0 {
          out.push(',');
        }
        out.push_str(&render(&child));
      }
      out.push(']');
      out
    }
    Node::Object(children) => {
      let mut out = String::from("{");
      for (index, (key, child)) in children.clone().enumerate() {
        if index > 0 {
          out.push(',');
        }
        out.push_str(&format!("\"{key}\":"));
        out.push_str(&render(&child));
      }
      out.push('}');
      out
    }
  }
}

/// Turns a byte offset into upstream's 1-based `{ line, column }`.
fn line_column(source: &str, offset: usize) -> (u32, u32) {
  let mut line = 1;
  let mut column = 1;
  for (index, byte) in source.bytes().enumerate() {
    if index == offset {
      break;
    }
    if byte == b'\n' {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }
  }
  (line, column)
}

// ------------------------------------------------------------------------------------------
// the gates
// ------------------------------------------------------------------------------------------

/// The differential itself: every ported case, compared on `data`, messages, paths and locations.
#[test]
fn matches_graphql_js() {
  let mut compared_errors = 0usize;
  let mut failures = Vec::new();

  for case in CASES {
    let outcome = run(case);
    if outcome.data != case.data {
      failures.push(format!(
        "{}\n  {UPSTREAM_FILE}:{}\n  data expected: {}\n  data actual:   {}",
        case.name, case.line, case.data, outcome.data
      ));
    }

    let expected: BTreeSet<(String, String, u32, u32)> = case
      .errors
      .iter()
      .map(|error| {
        (
          error.path.to_owned(),
          error.message.to_owned(),
          error.line,
          error.column,
        )
      })
      .collect();
    let actual: BTreeSet<(String, String, u32, u32)> = outcome
      .errors
      .iter()
      .map(|(message, path, line, column)| (path.clone(), message.clone(), *line, *column))
      .collect();
    if expected != actual {
      failures.push(format!(
        "{}\n  {UPSTREAM_FILE}:{}\n  errors expected: {expected:#?}\n  errors actual:   {actual:#?}",
        case.name, case.line
      ));
    }
    compared_errors += case.errors.len();
  }

  // The non-vacuity companion: a differential that ran zero comparisons is green.
  println!(
    "graphql-js mutation differential: {} cases, {compared_errors} error entries compared \
     ({compared_errors} response paths, {compared_errors} locations), upstream {UPSTREAM_COMMIT}",
    CASES.len()
  );
  assert_eq!(
    CASES.len(),
    3,
    "all 3 of {UPSTREAM_FILE}'s cases are ported"
  );
  assert_eq!(
    compared_errors, 2,
    "the failing-mutation case must reach the error path on both sides"
  );
  assert!(
    failures.is_empty(),
    "{} of {} cases diverge from graphql-js:\n\n{}",
    failures.len(),
    CASES.len(),
    failures.join("\n\n")
  );
}

/// The ordering case, read as the property rather than as a string.
///
/// Upstream's fixture is designed so that the *answer* records the order: each mutation writes the
/// number and its own sub-selection reads it back. Executed in parallel, every `theNumber` reads
/// the last write and the response is five fives — which is what this asserts is not happening,
/// rather than asserting a literal that a reader would have to decode.
#[test]
fn each_mutation_sees_its_own_write() {
  let case = &CASES[0];
  let outcome = run(case);
  let numbers: Vec<&str> = outcome
    .data
    .split("\"theNumber\":")
    .skip(1)
    .map(|tail| {
      &tail[..tail
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(tail.len())]
    })
    .collect();
  assert_eq!(
    numbers,
    vec!["1", "2", "3", "4", "5"],
    "each top-level mutation's sub-selection read the number that mutation wrote; a parallel \
     executor answers every one of them with the last write, and none of them with {ORIGINAL_NUMBER}"
  );
}

/// A failed mutation nulls its own key and stops nothing after it.
///
/// The nullable half of draft §6.4.4 on the serial path: `NumberHolder` is nullable, so `third` and
/// `sixth` become `null` and `fourth` and `fifth` still run — and still run *in order*, which the
/// numbers say.
#[test]
fn a_failed_mutation_does_not_stop_the_ones_after_it() {
  let case = &CASES[2];
  let outcome = run(case);
  assert_eq!(outcome.data, case.data);
  assert_eq!(outcome.errors.len(), 2);
  let paths: BTreeSet<&str> = outcome
    .errors
    .iter()
    .map(|(_, path, _, _)| path.as_str())
    .collect();
  assert_eq!(paths, BTreeSet::from(["third", "sixth"]));
}

/// The premise of the illegal-field case, asserted rather than described.
///
/// Draft §5.3.1 rejects a field the type does not define, so this document is the one input in the
/// corpus `proto` documents itself as never receiving. It is ported because the executor is total
/// over it and total in the way upstream is; this is what says the divergence in the header is real
/// and would notice if §5.3.1 stopped firing.
#[test]
fn the_illegal_case_is_the_one_invalid_document() {
  for case in CASES {
    let valid = is_valid(SDL, case.query);
    assert_eq!(
      valid,
      case.query != Q_ILLEGAL,
      "`{}` is {} and the port expects the opposite",
      case.name,
      if valid { "valid" } else { "invalid" }
    );
  }
}

/// Every ported case still says where it came from.
///
/// Opt-in, like the nonnull port's and for the same reasons. Point `SMEAR_GRAPHQL_JS` at a
/// checkout — CI points it at [`UPSTREAM_COMMIT`].
///
/// Three things a stale hand-port would fail: that every recorded line still holds the `it(` it was
/// read from, that every schema member this port names is still in the file, and that upstream has
/// not *added* a case this corpus does not have. The schema is checked by member rather than by
/// text because upstream builds it programmatically, which is the most a port can do — and it is
/// stated here rather than assumed, so the weaker check is visible.
#[test]
fn upstream_has_not_drifted() {
  let Ok(root) = std::env::var("SMEAR_GRAPHQL_JS") else {
    println!(
      "SMEAR_GRAPHQL_JS is unset: the {} ported cases ran against no upstream. \
       Set it to a graphql-js checkout at {UPSTREAM_COMMIT} to verify the port.",
      CASES.len()
    );
    return;
  };
  let path = std::path::Path::new(&root).join(UPSTREAM_FILE);
  let text = std::fs::read_to_string(&path)
    .unwrap_or_else(|error| panic!("SMEAR_GRAPHQL_JS is set but {path:?} is unreadable: {error}"));
  let lines: Vec<&str> = text.lines().collect();

  let mut verified = 0usize;
  for case in CASES {
    let line = lines
      .get(case.line as usize - 1)
      .unwrap_or_else(|| panic!("{UPSTREAM_FILE} has no line {}", case.line));
    assert!(
      line.trim() == case.anchor.trim(),
      "the port of `{}` records {UPSTREAM_FILE}:{}, which now reads\n  {}\nand not\n  {}",
      case.name,
      case.line,
      line.trim(),
      case.anchor.trim()
    );
    verified += 1;
  }

  for member in MEMBERS {
    assert!(
      text.contains(member),
      "{UPSTREAM_FILE} no longer names `{member}`, which the ported SDL declares"
    );
  }

  let upstream_cases = text.matches("\n    it(").count() + text.matches("\n  it(").count();
  assert_eq!(
    upstream_cases,
    CASES.len(),
    "{UPSTREAM_FILE} now has {upstream_cases} cases and this corpus has {}; a case was added \
     upstream and has not been ported",
    CASES.len()
  );

  println!(
    "upstream drift gate: {verified} anchors and {} schema members verified against \
     {UPSTREAM_FILE} ({upstream_cases} upstream cases)",
    MEMBERS.len()
  );
}
