#![cfg(feature = "proto")]

//! Draft §6 execution over shapes `nonnull-test.ts` does not reach.
//!
//! # What is here and what is not
//!
//! `proto_nonnull_oracle.rs` is the differential: seventeen cases transcribed from the reference
//! implementation, compared on `data`, message, response path and location. It is the authority on
//! draft §6.4.4, and nothing here duplicates it.
//!
//! This file covers the parts of §6 that file's schema has no field for. `nonnull-test.ts`'s
//! `DataType` has no list, no interface, no union, no enum, no fragment and no `@skip`, so §6.4.4's
//! **list clause** — the half of null propagation that says a null element of `[T!]` nulls the
//! whole list — has no case in the oracle at all. It is upstream's `lists-test.ts` that covers it,
//! and that file interpolates its schema and builds its values out of `Set`s and generators, so it
//! is even less portable than the one that was ported. So the list clause is gated here, against
//! the specification rather than against `graphql-js`, and that difference is worth knowing when
//! reading a green run.

use core::{cell::Cell, num::NonZeroU32};
use std::rc::Rc;

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  proto::{ArgumentSource, Executor, Kind, Leaf, Limits, Node, Response, StartError, Values},
  validator::{Budget, First, Schema, Scratch, validate_executable},
};

// ------------------------------------------------------------------------------------------
// a general-purpose driver
// ------------------------------------------------------------------------------------------

/// The driver's values: a small JSON-like tree, plus the two things a service has that JSON does
/// not — a concrete type name, and a field that fails.
#[derive(Clone, Debug, PartialEq)]
enum J {
  Null,
  Bool(bool),
  Int(i64),
  Str(String),
  List(Vec<J>),
  /// An object, tagged with the concrete type name a driver's `resolveType` would report.
  Obj(&'static str, Vec<(&'static str, J)>),
  /// Resolving this field raises a field error carrying the message.
  Fail(&'static str),
  /// A list that *claims* `n` elements while holding none, every element read as `"x"`.
  ///
  /// Not an exotic driver. A resolver over a cursor, a row count, a `Content-Length` or a remote
  /// page total answers [`Values::list_len`] from metadata rather than from a materialised
  /// `Vec`, and that is the whole point: the length the executor is handed is the driver's claim
  /// about the driver's own value, so it is bounded by nothing in the schema and nothing in the
  /// document. Building the plant out of a `Vec` instead would make the *test* pay the memory the
  /// executor is being asked not to spend, which is exactly the shape of gate nobody keeps.
  Phantom(usize),
  /// Resolving this field succeeds, and the value's *serialiser* answers [`J::Null`].
  ///
  /// Both halves are ordinary. Draft §6.4.3 steps 1 and 2 ask [`Values::is_null`] about the value
  /// the resolver produced, and this one is not null, so neither step fires. Step 4 then hands it
  /// to [`Values::coerce_leaf`], and a serialiser that maps an out-of-domain input onto its
  /// representation's null — a timestamp that will not format, an enum discriminant with no member,
  /// a column that reads back `NULL` — reports that by *returning* rather than by declining.
  Vanishes,
}

impl J {
  fn get(&self, key: &str) -> Option<&J> {
    match self {
      J::Obj(_, fields) => fields
        .iter()
        .find(|(name, _)| *name == key)
        .map(|(_, value)| value),
      _ => None,
    }
  }
}

#[derive(Default)]
struct Space {
  variables: Vec<(&'static str, J)>,
  /// When set, [`Values::variable`] hands each value out once and then forgets it.
  ///
  /// A driver whose variable table is a cursor, a one-shot channel or an arena it reclaims is
  /// nothing draft §6 forbids — [`Values::variable`] takes `&mut self` precisely so a handle table
  /// may allocate on read. This is what turns "the value that passed draft §6.4.1 is the value the
  /// resolver receives" into a property a test can fail rather than a sentence in a doc comment.
  consume_variables: bool,
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

  fn list_len(&self, value: &J) -> Option<usize> {
    match value {
      J::List(items) => Some(items.len()),
      J::Phantom(len) => Some(*len),
      _ => None,
    }
  }

  fn list_item(&mut self, value: &J, index: usize) -> J {
    match value {
      J::List(items) => items[index].clone(),
      J::Phantom(_) => J::Str("x".to_owned()),
      _ => J::Null,
    }
  }

  fn type_name<'a>(&'a self, value: &'a J) -> Option<&'a str> {
    match value {
      J::Obj(name, _) => Some(name),
      _ => None,
    }
  }

  fn coerce_leaf(&mut self, value: J, leaf: Leaf<'_>) -> Option<J> {
    match (&value, leaf) {
      (J::Int(_), Leaf::Int | Leaf::Float) => Some(value),
      (J::Str(_), Leaf::String | Leaf::Id | Leaf::Enum(_) | Leaf::Scalar(_)) => Some(value),
      (J::Bool(_), Leaf::Boolean) => Some(value),
      (J::Null, _) => Some(value),
      (J::Vanishes, _) => Some(J::Null),
      _ => None,
    }
  }

  fn variable(&mut self, name: &str) -> Option<J> {
    let index = self
      .variables
      .iter()
      .position(|(declared, _)| *declared == name)?;
    if self.consume_variables {
      return Some(self.variables.remove(index).1);
    }
    Some(self.variables[index].1.clone())
  }
}

/// What the driver answers when it is asked to resolve `__typename`.
///
/// No schema in this file declares a type by this name, so it can reach a response only by the
/// executor having handed the meta-field out — which is the thing draft §4.4 forbids it to do.
const LIE: &str = "AskedTheDriver";

/// Runs a query to completion, resolving each field by looking its name up in the parent value.
///
/// Returns `data` rendered canonically, and every error as `(kind, message, path)`.
fn run(sdl: &str, query: &str, root: J) -> (String, Vec<(Kind, String, String)>) {
  run_with(sdl, query, root, Vec::new())
}

fn run_with(
  sdl: &str,
  query: &str,
  root: J,
  variables: Vec<(&'static str, J)>,
) -> (String, Vec<(Kind, String, String)>) {
  execute(sdl, query, root, variables, data_and_errors)
}

/// A finished response as `data` and every error, which is what most cases here compare.
///
/// Named rather than repeated inline because the fixture that runs an operation twice compares two
/// of them, and two answers are only comparable if they were read the same way.
fn data_and_errors(response: &Response<'_, J>) -> (String, Vec<(Kind, String, String)>) {
  let errors = response
    .errors()
    .map(|error| (error.kind(), error.to_string(), error.path().to_string()))
    .collect();
  (render(&response.data()), errors)
}

/// Runs a query to completion and hands the finished response to `take`.
///
/// A closure rather than a returned [`Response`], because a `Response` borrows the executor that
/// produced it and neither can outlive this function. Everything a test wants out of one is
/// therefore read here.
fn execute<T>(
  sdl: &str,
  query: &str,
  root: J,
  variables: Vec<(&'static str, J)>,
  take: impl FnOnce(&Response<'_, J>) -> T,
) -> T {
  execute_bounded(sdl, query, root, variables, Limits::default(), take)
}

/// [`execute`] under a caller-chosen [`Limits`].
fn execute_bounded<T>(
  sdl: &str,
  query: &str,
  root: J,
  variables: Vec<(&'static str, J)>,
  limits: Limits,
  take: impl FnOnce(&Response<'_, J>) -> T,
) -> T {
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

  let mut space = Space {
    variables,
    consume_variables: false,
  };
  let mut executor = Executor::with_limits(&schema, &document, limits);
  drive(&mut executor, &mut space, root, take)
}

/// Runs one operation on `executor` to completion and hands the finished response to `take`.
///
/// Split out of [`execute_bounded`] so that a fixture can run **two** operations on one executor,
/// which is what any property about state surviving `reset` needs and what a per-run executor
/// cannot express.
fn drive<T>(
  executor: &mut Executor<'_, &str, Space>,
  space: &mut Space,
  root: J,
  take: impl FnOnce(&Response<'_, J>) -> T,
) -> T {
  executor
    .start(space, None, root)
    .expect("the operation resolves");

  while let Some(request) = executor.poll_resolve(space) {
    let id = request.id();
    let name = request.name();
    // The driver lies about `__typename`, on purpose. Every assertion below that names a concrete
    // type is therefore also an assertion that this branch was never taken; a driver answering the
    // meta-field *correctly* would prove nothing about who answered it.
    let answer = if name == "__typename" {
      Ok(J::Str(LIE.to_owned()))
    } else {
      match request.parent_value().get(name).cloned() {
        Some(J::Fail(message)) => Err(message),
        Some(value) => Ok(value),
        None => Ok(J::Null),
      }
    };
    match answer {
      Ok(value) => executor.handle_resolved(space, id, value),
      Err(message) => executor.handle_field_error(id, message),
    }
    while executor.poll_abandoned().is_some() {}
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  take(&response)
}

/// One `errors` entry as `(path, message, locations)`, every location a `(start, end)` byte range
/// into the query.
type Located = (String, String, Vec<(usize, usize)>);

/// Runs a query and returns every error it raised, with all of draft §7.1.2's `locations`.
fn run_locations(sdl: &str, query: &str, root: J) -> Vec<Located> {
  execute(sdl, query, root, Vec::new(), |response| {
    response
      .errors()
      .map(|error| {
        (
          error.path().to_string(),
          error.to_string(),
          error
            .locations()
            .iter()
            .map(|span| (span.start(), span.end()))
            .collect(),
        )
      })
      .collect()
  })
}

/// Parses an SDL and a query into the two borrows an [`Executor`] is built from.
///
/// Every test that drives the poll surface by hand rather than through [`execute`] needs both, and
/// the parse is eleven lines of turbofish that says nothing about the case it opens.
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
///
/// The executor is entered with an already-validated document, so nothing else in this file needs
/// the validator. The directive-condition cases below do: their whole point is that the input they
/// exercise arrives through a **valid** document, and a case that only asserted the executor's
/// answer would leave that premise in prose where nothing can check it.
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

/// Whether `query` passes every draft §5 rule against `sdl`.
///
/// The cases that need this are the ones asserting a document is **in**valid, so that "the
/// executor is only total here, this input cannot arrive" is a checked claim rather than a
/// comment. A rule that stopped firing would turn them red.
fn is_valid(sdl: &str, query: &str) -> bool {
  let (schema, document) = compile(sdl, query);
  let mut scratch = Scratch::new();
  let budget = Budget::default();
  let mut sink = First::new();
  validate_executable(&schema, &document, &mut scratch, &budget, &mut sink).is_ok()
}

/// Returns the `(start, end)` range of every occurrence of `needle` in `haystack`.
///
/// The expected locations are computed from the query rather than written as literals so that a
/// reader can see the assertion is about *which* selections were reported and not about arithmetic
/// on a string nobody re-counts.
fn occurrences(haystack: &str, needle: &str) -> Vec<(usize, usize)> {
  let mut found = Vec::new();
  let mut from = 0usize;
  while let Some(offset) = haystack[from..].find(needle) {
    let start = from + offset;
    found.push((start, start + needle.len()));
    from = start + needle.len();
  }
  found
}

fn render(node: &Node<'_, J>) -> String {
  match node {
    Node::Null => "null".to_owned(),
    Node::Leaf(J::Str(text)) => format!("\"{text}\""),
    Node::Leaf(J::Int(number)) => number.to_string(),
    Node::Leaf(J::Bool(flag)) => flag.to_string(),
    // Deliberately not `"null"`. A leaf holding a value no serialiser should have produced is the
    // state draft §6.4.3 step 4 exists to prevent, and rendering it the way [`Node::Null`] renders
    // would make a response that carries one indistinguishable — in every `data` assertion in this
    // file — from a response that nulled the position and reported the field error.
    Node::Leaf(_) => "<unserialised>".to_owned(),
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
        out.push_str(&format!("\"{key}\":{}", render(&child)));
      }
      out.push('}');
      out
    }
  }
}

fn obj(fields: Vec<(&'static str, J)>) -> J {
  J::Obj("Query", fields)
}

// ------------------------------------------------------------------------------------------
// draft §6.4.4, the list clause
// ------------------------------------------------------------------------------------------

const LIST_SDL: &str = r#"
type Query {
  nullable: [String]
  items: [String!]
  matrix: [[String!]]
  required: [String!]!
}
"#;

/// A null element of `[String]` is a null element and nothing more.
#[test]
fn a_null_element_of_a_nullable_list_stays_an_element() {
  let (data, errors) = run(
    LIST_SDL,
    "{ nullable }",
    obj(vec![(
      "nullable",
      J::List(vec![J::Str("a".into()), J::Null, J::Str("c".into())]),
    )]),
  );
  assert_eq!(data, r#"{"nullable":["a",null,"c"]}"#);
  assert!(errors.is_empty(), "{errors:?}");
}

/// A null element of `[String!]` nulls the whole list, which is the clause of §6.4.4 the oracle's
/// schema has no field to reach.
#[test]
fn a_null_element_of_a_non_null_list_nulls_the_list() {
  let (data, errors) = run(
    LIST_SDL,
    "{ items }",
    obj(vec![(
      "items",
      J::List(vec![J::Str("a".into()), J::Null, J::Str("c".into())]),
    )]),
  );
  assert_eq!(data, r#"{"items":null}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::NullInNonNull);
  assert_eq!(
    errors[0].2, "items.1",
    "the error's path is the failing element's, not the nulled list's"
  );
}

/// One level in, so a wrong walk shows up as the list surviving or `data` disappearing.
#[test]
fn a_null_element_of_an_inner_non_null_list_nulls_only_the_inner_list() {
  let (data, errors) = run(
    LIST_SDL,
    "{ matrix }",
    obj(vec![(
      "matrix",
      J::List(vec![
        J::List(vec![J::Str("a".into())]),
        J::List(vec![J::Null]),
      ]),
    )]),
  );
  assert_eq!(data, r#"{"matrix":[["a"],null]}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].2, "matrix.1.0");
}

/// `[String!]!` has nothing nullable between the element and the root.
#[test]
fn a_null_element_of_a_non_null_list_of_non_null_nulls_data() {
  let (data, errors) = run(
    LIST_SDL,
    "{ required }",
    obj(vec![("required", J::List(vec![J::Null]))]),
  );
  assert_eq!(data, "null");
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].2, "required.0");
}

/// Draft §6.4.3 step 3's other branch.
#[test]
fn a_non_collection_in_a_list_position_is_a_field_error() {
  let (data, errors) = run(
    LIST_SDL,
    "{ nullable }",
    obj(vec![("nullable", J::Str("Singular".into()))]),
  );
  assert_eq!(data, r#"{"nullable":null}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::NotAList);
  assert_eq!(
    errors[0].1,
    "Expected Iterable, but did not find one for field \"Query.nullable\"."
  );
}

// ------------------------------------------------------------------------------------------
// draft §6.3 CollectFields
// ------------------------------------------------------------------------------------------

const NEST_SDL: &str = r#"
type Query {
  a: Leaf
  b: Leaf
  nest: Query
}
type Leaf {
  x: String
  y: String
  n: Int
}
"#;

#[test]
fn skip_and_include_are_read_from_literals_and_variables() {
  let query = r#"
    query ($no: Boolean!, $yes: Boolean!) {
      a @skip(if: true) { x }
      b @include(if: false) { x }
      nest @skip(if: $no) @include(if: $yes) { a { x } }
    }
  "#;
  let root = obj(vec![
    ("a", J::Obj("Leaf", vec![("x", J::Str("A".into()))])),
    ("b", J::Obj("Leaf", vec![("x", J::Str("B".into()))])),
    (
      "nest",
      J::Obj(
        "Query",
        vec![("a", J::Obj("Leaf", vec![("x", J::Str("N".into()))]))],
      ),
    ),
  ]);
  let (data, errors) = run_with(
    NEST_SDL,
    query,
    root,
    vec![("no", J::Bool(false)), ("yes", J::Bool(true))],
  );
  assert_eq!(data, r#"{"nest":{"a":{"x":"N"}}}"#);
  assert!(errors.is_empty(), "{errors:?}");
}

/// `@skip` wins over `@include`, which is the order draft §6.3 steps 3.a and 3.b are written in.
#[test]
fn skip_beats_include() {
  let (data, _) = run(
    NEST_SDL,
    "{ a @skip(if: true) @include(if: true) { x } b { x } }",
    obj(vec![
      ("a", J::Obj("Leaf", vec![("x", J::Str("A".into()))])),
      ("b", J::Obj("Leaf", vec![("x", J::Str("B".into()))])),
    ]),
  );
  assert_eq!(data, r#"{"b":{"x":"B"}}"#);
}

// ------------------------------------------------------------------------------------------
// draft §6.3, a condition that cannot be read as a boolean
// ------------------------------------------------------------------------------------------

/// The schema every case in this section runs against, and the one the reference implementation
/// was measured on.
///
/// `required` is the non-null position: a condition that fails under it has nowhere to stop, so
/// draft §6.4.4 takes the null all the way to `data`.
const COND_SDL: &str = r#"
type Query {
  secret: String
  keeper: String
  nest: Level
  required: Level!
}
type Level {
  secret: String
  keeper: String
}
"#;

/// The expectations below are `graphql-js` 16.11.0's own output, run at the commit
/// `proto_nonnull_oracle.rs` pins, on [`COND_SDL`] and on the queries written here.
///
/// It is recorded rather than a differential because it is not one: the oracle's corpus is
/// `nonnull-test.ts`, which has no directive case, and this file compares against the
/// specification. What upstream settles is the *shape* of the answer, and it settles it three
/// ways. A condition raises rather than resolving to a boolean. The error is
/// draft §7.1.1's "raised during execution": `data` is present and null, where a genuine request
/// error — §6.1 `CoerceVariableValues` failing — omits `data` altogether. And the position is the
/// object whose selection set was being collected, so at the root there is no `path` and `data` is
/// the whole null, while nested it is the enclosing field's path and that field's null.
const MEASURED_AGAINST: &str = "graphql-js 16.11.0 at c18e9f6aada9ae086ddf836e4d822cf1426f3868";

/// The message upstream produces for a condition supplied a runtime null, verbatim.
const CONDITION_NULL: &str = "Argument \"if\" of non-null type \"Boolean!\" must not be null.";

/// Upstream's message for a condition whose variable has no runtime value, verbatim.
const CONDITION_VARIABLE_MISSING: &str = "Argument \"if\" of required type \"Boolean!\" was \
                                          provided the variable \"$flag\" which was not provided \
                                          a runtime value.";

/// Upstream's message for a directive carrying no `if` argument, verbatim.
const CONDITION_ARGUMENT_MISSING: &str =
  "Argument \"if\" of required type \"Boolean!\" was not provided.";

/// The root object every case in this section resolves against.
fn guarded() -> J {
  let level = || {
    J::Obj(
      "Level",
      vec![
        ("secret", J::Str("SECRET".into())),
        ("keeper", J::Str("KEEP".into())),
      ],
    )
  };
  obj(vec![
    ("secret", J::Str("SECRET".into())),
    ("keeper", J::Str("KEEP".into())),
    ("nest", level()),
    ("required", level()),
  ])
}

/// Draft 5.8.5's `IsVariableUsageAllowed` lets a **nullable** variable stand at a non-null
/// location when it declares a non-null default, and `if` is `Boolean!` — so `$flag: Boolean =
/// true` is a valid `@skip`/`@include` condition, and so is the same shape at a field argument.
///
/// This is the premise the rest of the section rests on, and it is asserted rather than written
/// down. Draft §6.1 step 5.e.i then coerces an explicitly supplied `null` for such a variable to
/// `null`, because the *variable's* declared type is nullable — which is what puts a null in a
/// `Boolean!` position with no request error anywhere for execution to inherit.
///
/// Both defaults are here because the two directives read the answer with opposite sign, so a
/// client writing the harmless default writes `= false` for `@include` and `= true` for `@skip`.
#[test]
fn a_nullable_variable_with_a_non_null_default_is_a_valid_condition() {
  for default in ["true", "false"] {
    for directive in ["skip", "include"] {
      assert_valid(
        COND_SDL,
        &format!("query ($flag: Boolean = {default}) {{ secret @{directive}(if: $flag) keeper }}"),
      );
    }
  }
  assert_valid(LOC_SDL, "query ($n: Int = 1) { counted(count: $n) }");
}

/// A readable condition decides, and the two directives decide oppositely.
///
/// This is the pair that rules out every "collapse an unreadable condition to a boolean" design at
/// once: `false` keeps a `@skip`ped selection and drops an `@include`d one, `true` does the
/// reverse, so no single boolean is the safe answer for both. It is also the happy path, and a fix
/// that raised on conditions it could read would show up here.
#[test]
fn a_readable_condition_decides_with_opposite_sign_for_the_two_directives() {
  for (directive, flag, expected) in [
    ("skip", true, r#"{"keeper":"KEEP"}"#),
    ("skip", false, r#"{"secret":"SECRET","keeper":"KEEP"}"#),
    ("include", true, r#"{"secret":"SECRET","keeper":"KEEP"}"#),
    ("include", false, r#"{"keeper":"KEEP"}"#),
  ] {
    let query =
      format!("query ($flag: Boolean = true) {{ secret @{directive}(if: $flag) keeper }}");
    assert_valid(COND_SDL, &query);
    let (data, errors) = run_with(COND_SDL, &query, guarded(), vec![("flag", J::Bool(flag))]);
    assert_eq!(data, expected, "@{directive}(if: {flag})");
    assert!(errors.is_empty(), "@{directive}(if: {flag}): {errors:?}");
  }
}

/// A condition supplied a runtime `null` is a field error, for **both** directives, at the root.
///
/// `@skip(if: $flag)` with a null flag must not return the guarded selection: the guard was not
/// evaluated, so the selection it guards is not something the response may carry. The answer cannot
/// be "keep" and it cannot be "drop", because `@include` wants the opposite of whichever one
/// `@skip` gets — so it is neither, and the position is nulled with an entry in `errors` saying
/// why.
///
/// The root selection set has no enclosing field, so the path is empty and the null is the whole of
/// `data`. Draft §7.1.2 asks for a `path` only "if an error can be associated to a particular
/// field", and this one cannot; draft §7.1.1 puts `data: null` — present, and null — on an error
/// raised *during* execution, which this is, as against the absent `data` it reserves for one
/// raised before execution begins. [`MEASURED_AGAINST`] answers identically, including the absent
/// `path`.
#[test]
fn a_null_condition_is_a_field_error_at_the_root_for_both_directives() {
  for directive in ["skip", "include"] {
    let query =
      format!("query ($flag: Boolean = true) {{ secret @{directive}(if: $flag) keeper }}");
    assert_valid(COND_SDL, &query);
    let (data, errors) = run_with(COND_SDL, &query, guarded(), vec![("flag", J::Null)]);
    assert_eq!(data, "null", "@{directive}");
    assert_eq!(errors.len(), 1, "@{directive}: {errors:?}");
    assert_eq!(errors[0].0, Kind::DirectiveCondition, "@{directive}");
    assert_eq!(
      errors[0].1, CONDITION_NULL,
      "@{directive}: the message is {MEASURED_AGAINST}'s, verbatim"
    );
    assert_eq!(errors[0].2, "", "@{directive}: the root has no path");
  }
}

/// The same condition several levels in nulls the object it was collected on, and nothing else.
///
/// The sibling `keeper` at the root is untouched: one selection's unreadable guard says nothing
/// about another object's fields. [`MEASURED_AGAINST`] produces the same `data` and the same
/// `["nest"]` path.
#[test]
fn a_null_condition_nulls_the_object_whose_selection_set_it_guards() {
  for directive in ["skip", "include"] {
    let query = format!(
      "query ($flag: Boolean = true) {{ nest {{ secret @{directive}(if: $flag) keeper }} keeper }}"
    );
    assert_valid(COND_SDL, &query);
    let (data, errors) = run_with(COND_SDL, &query, guarded(), vec![("flag", J::Null)]);
    assert_eq!(data, r#"{"nest":null,"keeper":"KEEP"}"#, "@{directive}");
    assert_eq!(errors.len(), 1, "@{directive}: {errors:?}");
    assert_eq!(errors[0].0, Kind::DirectiveCondition, "@{directive}");
    assert_eq!(errors[0].1, CONDITION_NULL, "@{directive}");
    assert_eq!(errors[0].2, "nest", "@{directive}");
  }
}

/// Under a non-null position the null has nowhere to stop, so draft §6.4.4 takes it to `data`.
///
/// The error's path stays the field that raised it while the null lands at the root, which is the
/// separation §6.4.4 depends on and the one thing an implementation that reports the *nulled*
/// position would get wrong here. [`MEASURED_AGAINST`] agrees on both.
#[test]
fn a_null_condition_under_a_non_null_position_nulls_data() {
  for directive in ["skip", "include"] {
    let query = format!(
      "query ($flag: Boolean = true) {{ required {{ secret @{directive}(if: $flag) }} keeper }}"
    );
    assert_valid(COND_SDL, &query);
    let (data, errors) = run_with(COND_SDL, &query, guarded(), vec![("flag", J::Null)]);
    assert_eq!(data, "null", "@{directive}");
    assert_eq!(errors.len(), 1, "@{directive}: {errors:?}");
    assert_eq!(errors[0].1, CONDITION_NULL, "@{directive}");
    assert_eq!(
      errors[0].2, "required",
      "@{directive}: the path is the field that raised it, not the position that was nulled"
    );
  }
}

/// Draft §6.3 applies the condition to all three kinds of selection, so all three raise.
///
/// A fragment spread is the one a fix written at the field arm alone would miss: its selections
/// reach the response through a different branch of the walk.
#[test]
fn an_unreadable_condition_is_read_on_every_kind_of_selection() {
  for query in [
    "query ($flag: Boolean = true) { secret @skip(if: $flag) keeper }",
    "query ($flag: Boolean = true) { ...F @skip(if: $flag) keeper }\n\
     fragment F on Query { secret }",
    "query ($flag: Boolean = true) { ... on Query @skip(if: $flag) { secret } keeper }",
  ] {
    assert_valid(COND_SDL, query);
    let (data, errors) = run_with(COND_SDL, query, guarded(), vec![("flag", J::Null)]);
    assert_eq!(data, "null", "{query}");
    assert_eq!(errors.len(), 1, "{query}: {errors:?}");
    assert_eq!(errors[0].0, Kind::DirectiveCondition, "{query}");
    assert_eq!(errors[0].1, CONDITION_NULL, "{query}");
  }
}

/// Draft §6.3 step 3.a runs before step 3.b whatever order the document writes them in.
///
/// `@skip(if: true)` removes the selection, and a removed selection's `@include` is never read — so
/// an unreadable `@include` written first raises nothing. Reading the directives in document order
/// would raise, and [`MEASURED_AGAINST`] does not: it answers `{"keeper":"KEEP"}` with no errors.
/// The companion below is what keeps this from being mistaken for "`@skip` suppresses `@include`":
/// with `@skip(if: false)` the selection survives step 3.a, step 3.b reads its condition, and the
/// error comes back.
#[test]
fn step_3a_runs_before_step_3b_whatever_the_document_order() {
  let skipped =
    "query ($flag: Boolean = true) { secret @include(if: $flag) @skip(if: true) keeper }";
  assert_valid(COND_SDL, skipped);
  let (data, errors) = run_with(COND_SDL, skipped, guarded(), vec![("flag", J::Null)]);
  assert_eq!(data, r#"{"keeper":"KEEP"}"#);
  assert!(errors.is_empty(), "{errors:?}");

  let kept = "query ($flag: Boolean = true) { secret @include(if: $flag) @skip(if: false) keeper }";
  assert_valid(COND_SDL, kept);
  let (data, errors) = run_with(COND_SDL, kept, guarded(), vec![("flag", J::Null)]);
  assert_eq!(data, "null");
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].1, CONDITION_NULL);
}

/// Collection stops at the first unreadable condition, so a selection set with two reports one.
///
/// [`MEASURED_AGAINST`] reports one as well — it throws out of `collectFields` — and an
/// implementation that collected the rest of the set anyway would report a second error about a
/// position that no longer exists.
#[test]
fn two_unreadable_conditions_in_one_selection_set_raise_once() {
  let query = "query ($flag: Boolean = true) { secret @skip(if: $flag) keeper @skip(if: $flag) }";
  assert_valid(COND_SDL, query);
  let (data, errors) = run_with(COND_SDL, query, guarded(), vec![("flag", J::Null)]);
  assert_eq!(data, "null");
  assert_eq!(errors.len(), 1, "{errors:?}");
}

/// A condition naming a variable the driver has no value for, which in `proto` is reachable from a
/// **valid** document.
///
/// Draft §6.1 `CoerceVariableValues` is the driver's here, so `$flag: Boolean = true` with nothing
/// supplied reaches [`Values::variable`] as `None` when the driver did not apply the declared
/// default. Upstream cannot be made to do that — it runs §6.1 itself — so the shape it produces
/// this message for is a different one, `$flag: Boolean` with no default at all, which its own
/// validator rejects. The message is upstream's word for word either way, and the answer is the
/// one the polarity argument requires: not a boolean.
#[test]
fn a_condition_whose_variable_has_no_value_is_a_field_error() {
  for directive in ["skip", "include"] {
    let query =
      format!("query ($flag: Boolean = true) {{ secret @{directive}(if: $flag) keeper }}");
    assert_valid(COND_SDL, &query);
    let (data, errors) = run_with(COND_SDL, &query, guarded(), Vec::new());
    assert_eq!(data, "null", "@{directive}");
    assert_eq!(errors.len(), 1, "@{directive}: {errors:?}");
    assert_eq!(errors[0].0, Kind::DirectiveCondition, "@{directive}");
    assert_eq!(errors[0].1, CONDITION_VARIABLE_MISSING, "@{directive}");
  }
}

/// A condition whose value is neither null nor a boolean — the one deliberate divergence.
///
/// [`MEASURED_AGAINST`] does **not** raise here. `shouldIncludeNode` compares `skip?.if === true`
/// and `include?.if === false`, so a string condition satisfies neither test, both directives go
/// inert, and upstream answers `{"secret":"SECRET","keeper":"KEEP"}` — it returns the guarded
/// selection. That is the same disclosure this section exists to close, reached by a different
/// route, and it is the one place matching upstream would be worse than diverging.
///
/// Diverging is safe because the input is unreachable from a conforming request: draft 5.8.5
/// admits only a `Boolean` or `Boolean!` variable at the condition, and §6.1 rejects a runtime
/// value for one of those that is neither a boolean nor null. So it takes an invalid document or a
/// driver that skipped §6.1 — and under either, failing the guard closed is the answer that is
/// safe for both senses.
#[test]
fn a_condition_that_is_not_a_boolean_is_a_field_error() {
  for directive in ["skip", "include"] {
    let query =
      format!("query ($flag: Boolean = true) {{ secret @{directive}(if: $flag) keeper }}");
    let (data, errors) = run_with(
      COND_SDL,
      &query,
      guarded(),
      vec![("flag", J::Str("yes".into()))],
    );
    assert_eq!(data, "null", "@{directive}");
    assert_eq!(errors.len(), 1, "@{directive}: {errors:?}");
    assert_eq!(errors[0].0, Kind::DirectiveCondition, "@{directive}");
    assert_eq!(
      errors[0].1, "Argument \"if\" of type \"Boolean!\" must be a Boolean.",
      "@{directive}"
    );
  }
}

/// A directive carrying no `if` argument at all.
///
/// Draft 5.4.2.1 makes it a validation failure, which this asserts rather than assumes, so no
/// validated document reaches it — but the rule has to be total, and the answer `false` would give
/// is `@skip`'s disclosure again. The message is [`MEASURED_AGAINST`]'s.
#[test]
fn a_directive_with_no_condition_is_a_field_error() {
  for directive in ["skip", "include"] {
    let query = format!("{{ secret @{directive} keeper }}");
    assert!(
      !is_valid(COND_SDL, &query),
      "@{directive} without `if` is draft 5.4.2.1's failure, so this case is unreachable from a \
       validated document and only the executor's totality is at stake"
    );
    let (data, errors) = run_with(COND_SDL, &query, guarded(), Vec::new());
    assert_eq!(data, "null", "@{directive}");
    assert_eq!(errors.len(), 1, "@{directive}: {errors:?}");
    assert_eq!(errors[0].0, Kind::DirectiveCondition, "@{directive}");
    assert_eq!(errors[0].1, CONDITION_ARGUMENT_MISSING, "@{directive}");
  }
}

/// The error points at the offending value, not at the whole selection set.
///
/// Draft §7.1.2's `locations` is what tells a client *which* guard failed, and it has to, because
/// the path names only the enclosing object: two guarded selections in one set produce the same
/// path. [`MEASURED_AGAINST`] reports the variable node for the same reason.
#[test]
fn a_condition_error_is_located_at_the_condition() {
  let query = "query ($flag: Boolean = true) { secret @skip(if: $flag) keeper }";
  let errors = execute(
    COND_SDL,
    query,
    guarded(),
    vec![("flag", J::Null)],
    |response| {
      response
        .errors()
        .map(|error| {
          error
            .locations()
            .iter()
            .map(|span| (span.start(), span.end()))
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    },
  );
  assert_eq!(errors, vec![occurrences(query, "$flag")[1..].to_vec()]);
}

/// The same null at a **field** argument reports too, and the difference is where.
///
/// Both positions read a runtime value against a `Boolean!`/`Int!` the schema declares, and both
/// raise: there is no asymmetry between them. What differs is the position the error belongs to,
/// and therefore its classification: draft §6.4.1 runs at a field that already has a slot and a
/// path, so the error is that field's and only that field is nulled; draft §6.3 runs while the
/// enclosing object's fields are being collected, so there is no field yet and the error is the
/// object's. Hence two [`Kind`]s for one message family.
#[test]
fn the_same_null_at_a_field_argument_reports_at_the_field_instead() {
  let query = "query ($n: Int = 1) { counted(count: $n) }";
  assert_valid(LOC_SDL, query);
  let (data, errors) = run_with(
    LOC_SDL,
    query,
    obj(vec![("counted", J::Int(7))]),
    vec![("n", J::Null)],
  );
  assert_eq!(data, r#"{"counted":null}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::ArgumentNull);
  assert_eq!(
    errors[0].1,
    "Argument \"count\" of non-null type \"Int!\" must not be null."
  );
  assert_eq!(errors[0].2, "counted");
}

const NESTED_SDL: &str = r#"
input Filter { flag: Boolean! }
type Query {
  picked(filter: Filter): String
  ided(ids: [ID!]): String
}
"#;

/// Where draft §6.4.1 stops being the executor's, stated as a case rather than only as prose.
///
/// A variable nested *inside* an argument's literal is reached by step 5.j — "coerce value
/// according to the input coercion rules of argumentType" — and §3.10 raises for a null at a
/// non-null input field, as §3.11 does for a non-null list element. `proto` runs neither, because
/// step 5.j's product is a coerced value in the service's representation and `proto` builds no
/// values at all; the whole literal reaches the driver as
/// [`ArgumentSource::Literal`](smear::proto::ArgumentSource::Literal) and applying §3.10 to its
/// contents is the driver's, on the same line as reading it.
///
/// Both documents are valid — 5.8.5 allows the nullable variable at the non-null location for the
/// same reason it allows it at `@include(if:)` — so this is the reachable edge of the boundary and
/// not a hypothetical. A change that moved the check inside would turn both of these red, which is
/// what makes the seam visible rather than merely written down.
#[test]
fn a_variable_nested_in_an_argument_literal_reaches_the_driver_uncoerced() {
  for (query, answered) in [
    (
      "query ($flag: Boolean = false) { picked(filter: {flag: $flag}) }",
      r#"{"picked":"P"}"#,
    ),
    (
      "query ($id: ID = \"x\") { ided(ids: [$id]) }",
      r#"{"ided":"I"}"#,
    ),
  ] {
    assert_valid(NESTED_SDL, query);
    let (data, errors) = run_with(
      NESTED_SDL,
      query,
      obj(vec![
        ("picked", J::Str("P".into())),
        ("ided", J::Str("I".into())),
      ]),
      vec![("flag", J::Null), ("id", J::Null)],
    );
    assert!(errors.is_empty(), "{query}: {errors:?}");
    assert_eq!(
      data, answered,
      "{query}: the field was offered and answered"
    );
  }
}

/// The value a bare variable argument delivers is the value draft §6.4.1 checked.
///
/// Steps 5.d through 5.i.i read the variable to decide whether the argument has a value and
/// whether that value is `null` at a non-null type. If the driver then had to look the same name
/// up again, the value reaching the resolver would be a *second* read, and a table that changed in
/// between would put a value past a check it never passed — a `String!` argument checked non-null
/// and resolved against nothing, with no entry in `errors`.
///
/// [`Space::consume_variables`] is what makes that a test rather than an argument: the table hands
/// each value out once and forgets it, so any second read returns `None`. The request still
/// carries `7`, because it carries the value and not the name. The whole disagreement is
/// unrepresentable now —
/// [`ArgumentSource::Variable`](smear::proto::ArgumentSource::Variable) has no name-only form —
/// and this is what fails if one comes back.
#[test]
fn a_variable_argument_delivers_the_value_that_passed_the_check() {
  let query = "query ($n: Int!) { counted(count: $n) }";
  assert_valid(LOC_SDL, query);
  let (schema, document) = compile(LOC_SDL, query);
  let mut space = Space {
    variables: vec![("n", J::Int(7))],
    consume_variables: true,
  };
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, obj(Vec::new()))
    .expect("the operation resolves");

  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let delivered = match request.arguments() {
      [argument] => match argument.source() {
        ArgumentSource::Variable { name, value } => {
          assert_eq!(*name, "n");
          value.clone()
        }
        other => panic!("`count` is a bare variable and arrived as {other:?}"),
      },
      arguments => panic!("`counted` has one argument, and got {}", arguments.len()),
    };
    executor.handle_resolved(&mut space, id, delivered);
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  assert!(response.is_ok(), "{:?}", response.errors().next());
  assert_eq!(render(&response.data()), r#"{"counted":7}"#);
  assert!(
    space.variables.is_empty(),
    "the executor read the variable, so the consuming table is empty"
  );
}

/// Draft §6.4 `MergeSelectionSets`: one response key, both sub-selections, in document order.
#[test]
fn fields_sharing_a_response_key_merge_their_selection_sets() {
  let (data, _) = run(
    NEST_SDL,
    "{ a { x } a { y } }",
    obj(vec![(
      "a",
      J::Obj(
        "Leaf",
        vec![("x", J::Str("X".into())), ("y", J::Str("Y".into()))],
      ),
    )]),
  );
  assert_eq!(data, r#"{"a":{"x":"X","y":"Y"}}"#);
}

// ------------------------------------------------------------------------------------------
// draft §7.1.2 locations over a merged group
// ------------------------------------------------------------------------------------------

const LOC_SDL: &str = r#"
type Query {
  boom: String
  items: [String!]
  counted(count: Int!): Int
}
"#;

/// A merged group is one response field and several source positions, and §7.1.2 wants all of
/// them.
///
/// Checked against `graphql-js` 16.11.0, which passes the whole `fieldNodes` array to
/// `locatedError` and maps one `locations` entry per node: `{ boom boom }` with a throwing
/// resolver yields a single error at `[{1,3},{1,8}]` — the two `boom` tokens, in collection order.
#[test]
fn a_merged_field_group_reports_every_location() {
  let query = "{ boom boom }";
  let errors = run_locations(LOC_SDL, query, obj(vec![("boom", J::Fail("kaboom"))]));
  assert_eq!(
    errors.len(),
    1,
    "one response field, so one error: {errors:?}"
  );
  assert_eq!(errors[0].0, "boom");
  assert_eq!(errors[0].1, "kaboom");
  assert_eq!(
    errors[0].2,
    occurrences(query, "boom"),
    "both selections that merged into the response key are places the error belongs to"
  );
}

/// The two positions of a group need not be anywhere near each other.
///
/// A spread is what makes the list load-bearing rather than cosmetic: a client reading only the
/// first location would be pointed at the operation and never told the fragment is involved.
#[test]
fn a_group_merged_through_a_fragment_reports_both_locations() {
  let query = "{ boom ...F }\nfragment F on Query { boom }";
  let errors = run_locations(LOC_SDL, query, obj(vec![("boom", J::Fail("kaboom"))]));
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].2, occurrences(query, "boom"));
}

/// An error at a list element carries the element's path and the *field's* locations.
///
/// Draft §6.4.3's list clause has no node of its own — an element is not written in the document —
/// so upstream reuses the field's, which is why the path and the locations disagree about how deep
/// the error is. Confirmed against `graphql-js`: `{ items items }` over `["a", null]` reports
/// `path: ["items", 1]` with both `items` locations.
#[test]
fn a_list_element_error_reports_the_whole_field_group() {
  let query = "{ items items }";
  let errors = run_locations(
    LOC_SDL,
    query,
    obj(vec![("items", J::List(vec![J::Str("a".into()), J::Null]))]),
  );
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, "items.1");
  assert_eq!(errors[0].2, occurrences(query, "items"));
}

/// Draft §6.4.1's errors are the exception, and upstream draws the line in the same place.
///
/// `getArgumentValues` is handed `fieldNodes[0]` alone, so a missing argument over a merged group
/// reports one location where a resolver error over the same group reports every field's.
#[test]
fn an_argument_error_reports_only_the_node_it_can_name() {
  let query = "{ counted counted }";
  let errors = run_locations(LOC_SDL, query, obj(vec![]));
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(
    errors[0].1,
    "Argument \"count\" of required type \"Int!\" was not provided."
  );
  assert_eq!(errors[0].2, occurrences(query, "counted")[..1]);
}

/// The alias is the response key, so two aliases of one field are two groups and two errors.
#[test]
fn two_aliases_of_one_field_are_two_groups_of_one_location() {
  let query = "{ p: boom q: boom }";
  let errors = run_locations(LOC_SDL, query, obj(vec![("boom", J::Fail("kaboom"))]));
  assert_eq!(errors.len(), 2, "{errors:?}");
  assert_eq!(errors[0].0, "p");
  assert_eq!(errors[1].0, "q");
  assert_eq!(errors[0].2, occurrences(query, "p: boom"));
  assert_eq!(errors[1].2, occurrences(query, "q: boom"));
}

/// The alias is the response key and the path segment; the schema's name is what the driver sees.
#[test]
fn an_alias_is_the_response_key_and_the_path_segment() {
  let (data, errors) = run(
    NEST_SDL,
    "{ first: a { x } second: a { x } }",
    obj(vec![("a", J::Obj("Leaf", vec![("x", J::Fail("boom"))]))]),
  );
  assert_eq!(data, r#"{"first":{"x":null},"second":{"x":null}}"#);
  assert_eq!(errors.len(), 2, "{errors:?}");
  assert_eq!(errors[0].2, "first.x");
  assert_eq!(errors[1].2, "second.x");
}

const IFACE_SDL: &str = r#"
type Query {
  pet: Pet
  pets: [Pet]
  thing: Thing
}
interface Pet {
  name: String
}
type Dog implements Pet {
  name: String
  barks: Boolean
}
type Cat implements Pet {
  name: String
  meows: Boolean
}
union Thing = Dog | Cat
"#;

#[test]
fn fragment_spreads_and_inline_fragments_apply_their_type_conditions() {
  let query = r#"
    { pet { name ...DogBits ... on Cat { meows } } }
    fragment DogBits on Dog { barks }
  "#;
  let (data, _) = run(
    IFACE_SDL,
    query,
    obj(vec![(
      "pet",
      J::Obj(
        "Dog",
        vec![("name", J::Str("Rex".into())), ("barks", J::Bool(true))],
      ),
    )]),
  );
  assert_eq!(data, r#"{"pet":{"name":"Rex","barks":true}}"#);
}

/// A union member is reached through an inline fragment, and `__typename` agrees with it.
///
/// The agreement is the point. `Cat` is what draft §6.4.3's `ResolveAbstractType` concluded, and
/// draft §6.3 spent that conclusion choosing `... on Cat` over `... on Dog`. Were the meta-field
/// resolved like any other, the response could carry Cat-only selections under a `"Dog"` — or here,
/// under [`LIE`] — and be internally contradictory with nothing able to notice.
#[test]
fn a_union_resolves_to_its_member() {
  let (data, _) = run(
    IFACE_SDL,
    "{ thing { __typename ... on Cat { meows } ... on Dog { barks } } }",
    obj(vec![(
      "thing",
      J::Obj("Cat", vec![("meows", J::Bool(true))]),
    )]),
  );
  assert_eq!(data, r#"{"thing":{"__typename":"Cat","meows":true}}"#);
}

#[test]
fn an_unresolvable_abstract_type_is_a_field_error() {
  let (data, errors) = run(
    IFACE_SDL,
    "{ pet { name } }",
    obj(vec![("pet", J::Str("not an object".into()))]),
  );
  assert_eq!(data, r#"{"pet":null}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::AbstractUnresolved);
}

#[test]
fn an_abstract_type_resolved_to_an_impossible_type_is_a_field_error() {
  let (data, errors) = run(
    IFACE_SDL,
    "{ thing { __typename } }",
    // `Query` is a real type and is not a member of `Thing`, which is the mistake a driver whose
    // `resolveType` returns the wrong table's name actually makes.
    obj(vec![("thing", J::Obj("Query", vec![]))]),
  );
  assert_eq!(data, r#"{"thing":null}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::AbstractNotPossible);
  assert_eq!(
    errors[0].1,
    "Runtime Object type \"Query\" is not a possible type for \"Thing\"."
  );
}

/// The other two ways a named type fails the possible-object test.
///
/// [`Kind::AbstractNotPossible`] covers three branches of one filter — a name the schema never
/// interned, a name it did intern but not as an object, and an object outside the abstract type's
/// possible set — and the case above reaches only the third. A driver naming a type that does not
/// exist and a driver naming the *interface* rather than one of its implementors are the two
/// mistakes that would otherwise turn a field's declared type into whatever the driver said.
#[test]
fn an_abstract_type_resolved_to_a_name_that_is_not_a_possible_object_is_a_field_error() {
  for named in ["Nope", "Pet"] {
    let (data, errors) = run(
      IFACE_SDL,
      "{ pet { name } }",
      obj(vec![(
        "pet",
        J::Obj(named, vec![("name", J::Str("N".into()))]),
      )]),
    );
    assert_eq!(data, r#"{"pet":null}"#, "resolved to {named}");
    assert_eq!(errors.len(), 1, "resolved to {named}: {errors:?}");
    assert_eq!(
      errors[0].0,
      Kind::AbstractNotPossible,
      "resolved to {named}"
    );
    assert_eq!(
      errors[0].1,
      format!("Runtime Object type \"{named}\" is not a possible type for \"Pet\".")
    );
  }
}

// ------------------------------------------------------------------------------------------
// draft §4.4 `__typename`
// ------------------------------------------------------------------------------------------

/// The interface case, and the one the whole meta-field decision is about.
///
/// `Dog` is not in the document and not in the field's declared type: the executor learned it from
/// [`Values::type_name`] and then spent it deciding that `DogBits` applied and `... on Cat` did
/// not. Asking the driver to name the same type a second time is asking for an answer that can
/// disagree with the selections its own first answer produced. The driver here would give exactly
/// that — [`LIE`] — so a green run is the proof it was not asked.
#[test]
fn typename_on_an_interface_names_the_type_the_fragments_were_chosen_by() {
  let query = r#"
    { pet { __typename name ...DogBits ... on Cat { meows } } }
    fragment DogBits on Dog { barks }
  "#;
  let (data, errors) = run(
    IFACE_SDL,
    query,
    obj(vec![(
      "pet",
      J::Obj(
        "Dog",
        vec![("name", J::Str("Rex".into())), ("barks", J::Bool(true))],
      ),
    )]),
  );
  assert_eq!(
    data,
    r#"{"pet":{"__typename":"Dog","name":"Rex","barks":true}}"#
  );
  assert!(errors.is_empty(), "{errors:?}");
}

/// Every element of an abstract list resolves its own type, so one answer for the whole field would
/// look plausible and be wrong on all but the first element.
#[test]
fn typename_follows_each_element_of_an_abstract_list() {
  let (data, errors) = run(
    IFACE_SDL,
    "{ pets { __typename ... on Dog { barks } ... on Cat { meows } } }",
    obj(vec![(
      "pets",
      J::List(vec![
        J::Obj("Dog", vec![("barks", J::Bool(true))]),
        J::Obj("Cat", vec![("meows", J::Bool(false))]),
      ]),
    )]),
  );
  assert_eq!(
    data,
    r#"{"pets":[{"__typename":"Dog","barks":true},{"__typename":"Cat","meows":false}]}"#
  );
  assert!(errors.is_empty(), "{errors:?}");
}

/// On a concrete object position the answer is the *declared* type, and the driver's value is not
/// consulted for it either.
///
/// The value is tagged with a name no schema here defines, which is what an object handed back by a
/// driver that tracks its own types loosely looks like. Draft §6.4.3 asks `ResolveAbstractType`
/// only where the position is an interface or a union, so the tag is never read — and the response
/// key merges and aliases like any other, because it is one.
#[test]
fn typename_merges_and_aliases_like_any_other_response_key() {
  let (data, errors) = run(
    NEST_SDL,
    "{ a { __typename __typename kind: __typename x } }",
    obj(vec![(
      "a",
      J::Obj("NotEvenAType", vec![("x", J::Str("X".into()))]),
    )]),
  );
  assert_eq!(data, r#"{"a":{"__typename":"Leaf","kind":"Leaf","x":"X"}}"#);
  assert!(errors.is_empty(), "{errors:?}");
}

/// A query of nothing but `__typename` asks the driver for nothing at all.
///
/// Driven by hand rather than through [`execute`], because the property is the *absence* of a
/// `FieldRequest` and a loop that answers every request it is handed cannot assert an absence. The
/// variant assertion is the second half: the answer arrives as [`Node::TypeName`] and not as a
/// [`Node::Leaf`] holding a driver value, which is what makes "the driver did not produce this"
/// something a serialiser can read off the type rather than infer from the response key.
#[test]
fn a_query_of_only_typename_issues_no_field_request() {
  let (schema, document) = compile(IFACE_SDL, "{ __typename here: __typename }");

  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, obj(vec![]))
    .expect("the operation resolves");

  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "both response keys are the executor's to answer, so there is no work to hand out"
  );
  assert_eq!(executor.poll_abandoned(), None);

  let response = executor
    .poll_response()
    .expect("a response that needs no driver call is finished as soon as it is started");
  assert_eq!(response.error_count(), 0);

  let Node::Object(mut fields) = response.data() else {
    panic!("the root is an object")
  };
  let (key, value) = fields.next().expect("the first response key");
  assert_eq!(key.to_string(), "__typename");
  assert!(
    matches!(value, Node::TypeName("Query")),
    "the executor's own answer, not a driver value wearing `Node::Leaf`"
  );
  let (key, value) = fields.next().expect("the aliased response key");
  assert_eq!(key.to_string(), "here", "the alias is the response key");
  assert!(matches!(value, Node::TypeName("Query")));
  assert!(fields.next().is_none());
}

// ------------------------------------------------------------------------------------------
// draft §6.4.3 step 4, `CoerceResult`
// ------------------------------------------------------------------------------------------

/// Draft §6.4.3 step 4: the driver said it could not serialise the value, and the executor
/// supplied the message and the path.
#[test]
fn a_leaf_that_cannot_be_serialised_is_a_field_error() {
  let (data, errors) = run(
    NEST_SDL,
    "{ a { n x } }",
    obj(vec![(
      "a",
      // `n` is an `Int` and coerces; `x` is a `String` handed an integer and does not.
      J::Obj("Leaf", vec![("n", J::Int(7)), ("x", J::Int(7))]),
    )]),
  );
  assert_eq!(data, r#"{"a":{"n":7,"x":null}}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::LeafCoercion);
  assert_eq!(errors[0].2, "a.x");
}

const COERCE_SDL: &str = r#"
type Query {
  text: String
  required: String!
  mood: Mood
  requiredMood: Mood!
  texts: [String!]
}
enum Mood { HAPPY SAD }
"#;

/// A serialised leaf that is null is draft §6.4.3 step 4's field error, on a *nullable* position
/// too.
///
/// The position being nullable is what makes this the easy case to get wrong: `null` is a legal
/// value here, so storing what the serialiser returned produces a response no reader can fault.
/// Draft §6.4.3 still forbids it — `CoerceResult` returning a value that is not a legal serialised
/// leaf is a field error whatever the position admits — and the reference implementation agrees:
/// `completeLeafValue` null-checks `serialize`'s result and throws before the nullability of the
/// position is ever consulted. So the observable difference is not in `data`, which is `null`
/// either way, but in whether `errors` says why.
#[test]
fn a_leaf_serialised_to_null_is_a_field_error() {
  let (data, errors) = run(COERCE_SDL, "{ text }", obj(vec![("text", J::Vanishes)]));
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::LeafCoercion);
  assert_eq!(errors[0].2, "text");
  assert_eq!(data, r#"{"text":null}"#);
}

/// The same case, read off the response tree rather than through [`render`].
///
/// [`render`]'s `<unserialised>` marker is what makes every other `data` assertion in this file a
/// guard against this defect, but it is a property of this file: an edit softening it back to
/// `"null"` would unpin all of them at once and no test would say so. This one names the variant
/// instead, so the pin holds whatever [`render`] does.
#[test]
fn a_leaf_the_executor_rejected_is_not_in_the_response() {
  execute(
    COERCE_SDL,
    "{ text }",
    obj(vec![("text", J::Vanishes)]),
    Vec::new(),
    |response| {
      let Node::Object(mut fields) = response.data() else {
        panic!("the root is an object")
      };
      let (key, value) = fields.next().expect("the only response key");
      assert_eq!(key.to_string(), "text");
      assert!(
        matches!(value, Node::Null),
        "the position is null because the executor nulled it, not because it stored the \
         serialiser's null as a leaf"
      );
    },
  );
}

/// `String!` is the case where storing the serialiser's answer contradicts the schema outright.
///
/// Nothing downstream can repair this one. A serialiser writing `null` into a `String!` produces a
/// response that violates its own schema with no `errors` entry saying so, which is the shape a
/// client is entitled to treat as impossible.
#[test]
fn a_non_null_leaf_serialised_to_null_is_a_field_error() {
  let (data, errors) = run(
    COERCE_SDL,
    "{ required }",
    obj(vec![("required", J::Vanishes)]),
  );
  assert_eq!(data, "null", "§6.4.4 walks past `String!` to the root");
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::LeafCoercion);
  assert_eq!(errors[0].2, "required");
  assert_eq!(
    errors[0].1,
    "Expected a value of type \"String\" but received an incompatible value for field \
     \"Query.required\".",
    "the same outcome as declining, and the same message: `None` and `Some(null)` are one \
     failure spelled two ways"
  );
}

/// An enum reaches `CoerceResult` through the same call and must reach the same branch.
///
/// Worth its own case even so: [`Leaf::Enum`] is a distinct variant of the argument, and a
/// discriminant with no member is the most likely way a real driver produces this — draft §6.4.3
/// requires the result to be one of the enum's members, and null is not one of them.
#[test]
fn an_enum_serialised_to_null_is_a_field_error() {
  let (data, errors) = run(COERCE_SDL, "{ mood }", obj(vec![("mood", J::Vanishes)]));
  assert_eq!(data, r#"{"mood":null}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::LeafCoercion);
  assert_eq!(errors[0].2, "mood");
  assert_eq!(
    errors[0].1,
    "Expected a value of type \"Mood\" but received an incompatible value for field \"Query.mood\".",
    "the error names the enum the schema declared"
  );
}

/// `Mood!` reaches §6.4.4 by the leaf branch rather than by step 1.
#[test]
fn a_non_null_enum_serialised_to_null_is_a_field_error() {
  let (data, errors) = run(
    COERCE_SDL,
    "{ requiredMood }",
    obj(vec![("requiredMood", J::Vanishes)]),
  );
  assert_eq!(data, "null");
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::LeafCoercion);
  assert_eq!(errors[0].2, "requiredMood");
}

/// An element of `[String!]` whose serialiser answers null nulls the whole list.
///
/// The list clause is not a second fix. Every element of a `[String!]` sits at a non-null position,
/// so the field error §6.4.3 step 4 raises at the element is the same input to §6.4.4's walk that a
/// null element would have been — the executor gets the list clause here for the reason it gets it
/// everywhere, from a slot's recorded type being the type of the *position*. The case is here to
/// prove the error is raised *at the element*, since a fix applied one frame out would null the
/// list and carry the list's own path.
#[test]
fn a_list_element_serialised_to_null_nulls_a_non_null_list() {
  let (data, errors) = run(
    COERCE_SDL,
    "{ texts }",
    obj(vec![(
      "texts",
      J::List(vec![J::Str("a".into()), J::Vanishes, J::Str("c".into())]),
    )]),
  );
  assert_eq!(data, r#"{"texts":null}"#);
  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(errors[0].0, Kind::LeafCoercion);
  assert_eq!(
    errors[0].2, "texts.1",
    "the failing element's path, not the nulled list's"
  );
}

// ------------------------------------------------------------------------------------------
// the poll surface
// ------------------------------------------------------------------------------------------

/// `boom` is `String!`, so failing it nulls `nest` and everything else under `nest` with it.
const NEST_ABANDON_SDL: &str = "type Query { nest: Wrap } type Wrap { boom: String! slow: String }";

/// Everything still outstanding under a nulled ancestor is reported on its own channel.
///
/// This is what a single `Step` enum would have buried. The driver learns that `slow` can no
/// longer affect the response *before* it finishes resolving it.
#[test]
fn abandoned_work_is_reported_on_its_own_channel() {
  let (schema, document) = compile(NEST_ABANDON_SDL, "{ nest { boom slow } }");

  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, obj(vec![]))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, J::Obj("Wrap", vec![]));
  assert_eq!(executor.poll_abandoned(), None);

  // Both children in flight before either is answered, which is the situation the channel exists
  // for and the one a synchronous test loop never reaches by accident.
  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  let slow = executor.poll_resolve(&mut space).expect("slow").id();
  assert_eq!(executor.poll_abandoned(), None);

  executor.handle_field_error(boom, "boom");
  assert_eq!(
    executor.poll_abandoned(),
    Some(slow),
    "`boom` is `String!`, so nulling it nulls `nest` and `slow` can no longer land anywhere"
  );
  assert_eq!(executor.poll_abandoned(), None);

  // Answering a retired request is a race a driver will lose, and losing it must be harmless.
  executor.handle_resolved(&mut space, slow, J::Str("too late".into()));

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 1);
  assert_eq!(render(&response.data()), r#"{"nest":null}"#);
}

/// `poll_resolve` withholds at the ceiling rather than queueing ahead.
#[test]
fn the_in_flight_ceiling_withholds() {
  let (schema, document) = compile("type Query { a: String b: String c: String }", "{ a b c }");

  let mut space = Space::default();
  let mut executor = Executor::with_limits(
    &schema,
    &document,
    Limits {
      max_in_flight: NonZeroU32::MIN,
      ..Limits::default()
    },
  );
  executor
    .start(&mut space, None, obj(vec![]))
    .expect("the operation resolves");

  let first = executor.poll_resolve(&mut space).expect("one field").id();
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "two fields remain, and the ceiling is one"
  );
  assert!(
    executor.poll_response().is_none(),
    "withholding is not completion"
  );
  executor.handle_resolved(&mut space, first, J::Str("1".into()));
  assert!(executor.poll_resolve(&mut space).is_some());
}

/// `poll_response` yields once, so it can be a loop condition.
#[test]
fn the_response_is_delivered_once() {
  let (schema, document) = compile("type Query { a: String }", "{ a }");

  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, obj(vec![]))
    .expect("the operation resolves");
  let id = executor.poll_resolve(&mut space).expect("a").id();
  executor.handle_resolved(&mut space, id, J::Str("A".into()));
  assert!(executor.poll_response().is_some());
  assert!(executor.poll_response().is_none());
}

/// A request the previous operation abandoned cannot answer a field of the next one.
///
/// The two halves of the trap have to meet for this to bite, and both are ordinary. A response is
/// delivered while abandoned requests are still outstanding — deliberately, because they cannot
/// change it and withholding on them would deadlock a driver that never retires them — so the
/// driver is still holding live-looking ids when the next operation starts. And `start` empties the
/// in-flight slab, so the next operation re-issues the very same indices and generations, in the
/// same order. The assertion below that `stale` and `slow` agree on both is what makes the danger
/// concrete: they are the same handle in every respect except the epoch.
///
/// Nothing here requires the driver to misbehave. Answering late is exactly what a driver that
/// cancels asynchronously does.
#[test]
fn a_request_abandoned_by_a_previous_operation_cannot_answer_this_one() {
  let (schema, document) = compile(NEST_ABANDON_SDL, "{ nest { boom slow } }");

  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);

  executor
    .start(&mut space, None, obj(vec![]))
    .expect("the operation resolves");
  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, J::Obj("Wrap", vec![]));
  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  let stale = executor.poll_resolve(&mut space).expect("slow").id();
  executor.handle_field_error(boom, "boom");

  // Taken without draining `poll_abandoned` first, which is the case the epoch exists for: a
  // driver is under no obligation to retire an id before it stops caring about the operation.
  let (data, errors) = {
    let response = executor
      .poll_response()
      .expect("nothing that could change the response is outstanding");
    (render(&response.data()), response.error_count())
  };
  assert_eq!(data, r#"{"nest":null}"#);
  assert_eq!(errors, 1);

  executor
    .start(&mut space, None, obj(vec![]))
    .expect("the operation resolves");
  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, J::Obj("Wrap", vec![]));
  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  let slow = executor.poll_resolve(&mut space).expect("slow").id();

  assert_eq!(
    (stale.index(), stale.generation()),
    (slow.index(), slow.generation()),
    "the aliasing this test is about: the slab reissues the same index and generation"
  );
  assert_ne!(
    stale.epoch(),
    slow.epoch(),
    "and the epoch is the only thing that tells the two operations' ids apart"
  );

  executor.handle_resolved(&mut space, stale, J::Str("stale".into()));
  executor.handle_resolved(&mut space, boom, J::Str("B".into()));
  executor.handle_resolved(&mut space, slow, J::Str("fresh".into()));

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(
    render(&response.data()),
    r#"{"nest":{"boom":"B","slow":"fresh"}}"#,
    "the late answer belongs to an operation that is over and must not reach this one"
  );
  assert_eq!(response.error_count(), 0);
}

/// Every ceiling the type admits completes a valid query, including the smallest one.
///
/// The `panic!` is the assertion. A ceiling that stops the machine does not fail loudly on its own
/// — `poll_resolve`, `poll_abandoned` and `poll_response` all just answer `None` — so a driver
/// meets it as a spin, which no test can distinguish from slow progress unless it says out loud
/// that all three channels withheld while the response was unfinished.
#[test]
fn every_admissible_ceiling_finishes_a_valid_query() {
  let (schema, document) = compile(
    "type Query { nest: Wrap a: String b: String } type Wrap { boom: String! slow: String }",
    "{ nest { boom slow } a b }",
  );
  let root = obj(vec![
    (
      "nest",
      J::Obj(
        "Wrap",
        vec![("boom", J::Fail("boom")), ("slow", J::Str("S".into()))],
      ),
    ),
    ("a", J::Str("A".into())),
    ("b", J::Str("B".into())),
  ]);

  for ceiling in [1u32, 2, 3, 256] {
    let limits = Limits {
      max_in_flight: NonZeroU32::new(ceiling).expect("the ceilings under test are not zero"),
      ..Limits::default()
    };
    let mut space = Space::default();
    let mut executor = Executor::with_limits(&schema, &document, limits);
    executor
      .start(&mut space, None, root.clone())
      .expect("the operation resolves");

    let mut steps = 0usize;
    let data = loop {
      steps += 1;
      assert!(
        steps < 64,
        "ceiling {ceiling}: {steps} turns of the driver loop for a five-field query"
      );

      if let Some(request) = executor.poll_resolve(&mut space) {
        let id = request.id();
        let name = request.name();
        match request.parent_value().get(name).cloned() {
          Some(J::Fail(message)) => executor.handle_field_error(id, message),
          Some(value) => executor.handle_resolved(&mut space, id, value),
          None => executor.handle_resolved(&mut space, id, J::Null),
        }
        continue;
      }
      if executor.poll_abandoned().is_some() {
        continue;
      }
      if let Some(response) = executor.poll_response() {
        break render(&response.data());
      }
      panic!(
        "ceiling {ceiling}: every channel withheld while the response was unfinished, so no \
         call the driver can make advances the executor"
      );
    };

    assert_eq!(
      data, r#"{"nest":null,"a":"A","b":"B"}"#,
      "ceiling {ceiling}"
    );
  }
}

// ------------------------------------------------------------------------------------------
// how long the executor holds a driver value
// ------------------------------------------------------------------------------------------

/// A driver value that says how many of itself are alive.
///
/// Every other case in this file drives the executor with [`J`], which is inert: a value the
/// executor keeps after nothing can read it costs a few bytes and changes no assertion, so a
/// retention is invisible. `Values::Value` is the driver's own type precisely so that a wasm or FFI
/// handle can be one, and a handle held past the operation that opened it holds open whatever it
/// names — a connection, a cursor, a table entry the driver would otherwise have reclaimed. A
/// counter is the only way that becomes something a test can fail.
#[derive(Debug)]
struct Counted {
  live: Rc<Cell<usize>>,
  payload: Payload,
}

#[derive(Debug)]
enum Payload {
  Str(&'static str),
  Obj,
  /// A list of that many elements, each of which [`Handles::list_item`] mints as an object.
  List(usize),
}

impl Counted {
  fn new(live: &Rc<Cell<usize>>, payload: Payload) -> Self {
    live.set(live.get() + 1);
    Self {
      live: Rc::clone(live),
      payload,
    }
  }

  fn obj(live: &Rc<Cell<usize>>) -> Self {
    Self::new(live, Payload::Obj)
  }

  fn text(live: &Rc<Cell<usize>>, text: &'static str) -> Self {
    Self::new(live, Payload::Str(text))
  }

  fn list(live: &Rc<Cell<usize>>, len: usize) -> Self {
    Self::new(live, Payload::List(len))
  }
}

impl Drop for Counted {
  fn drop(&mut self) {
    self.live.set(self.live.get() - 1);
  }
}

/// A value space whose variable table hands each value **over** rather than lending it.
///
/// Handing over is what makes the count unambiguous. While the executor holds a variable's value
/// there is exactly one of it in the program, so a non-zero counter is the executor's own retention
/// and never a copy the test left behind in a table. It is also not an exotic driver:
/// [`Values::variable`] takes `&mut self` so that a handle table may move, allocate or reclaim on
/// read, and [`Space::consume_variables`] already exercises the same shape.
struct Handles {
  /// The counter every value this space itself mints belongs to.
  mint: Rc<Cell<usize>>,
  variables: Vec<(&'static str, Counted)>,
}

impl Values for Handles {
  type Value = Counted;

  /// Never. Every case in this section counts *held* values, and a null is the one answer that
  /// leaves nothing to hold: draft §6.4.3 steps 1 and 2 would store `State::Null` and drop the
  /// value before any of these assertions could tell a release from a value that was never kept.
  fn is_null(&self, _: &Counted) -> bool {
    false
  }

  fn as_bool(&self, _: &Counted) -> Option<bool> {
    None
  }

  fn list_len(&self, value: &Counted) -> Option<usize> {
    match value.payload {
      Payload::List(len) => Some(len),
      _ => None,
    }
  }

  fn list_item(&mut self, _: &Counted, _: usize) -> Counted {
    // `HOLD_SDL` declares one list and its element type is an object, so every element the
    // executor completes stores a value of its own on the element slot.
    Counted::obj(&self.mint)
  }

  fn type_name<'a>(&'a self, _: &'a Counted) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: Counted, _: Leaf<'_>) -> Option<Counted> {
    Some(value)
  }

  fn variable(&mut self, name: &str) -> Option<Counted> {
    let index = self
      .variables
      .iter()
      .position(|(declared, _)| *declared == name)?;
    Some(self.variables.remove(index).1)
  }
}

const HOLD_SDL: &str = r#"
type Query {
  echo(text: String): String
  pair(first: String, second: String!): String
  nest: Wrap
}
type Wrap {
  boom: String!
  echo(text: String): String
  bulk: [Cell]
  deep: Wrap
}
type Cell {
  text: String
  boom: String!
  echo(text: String): String
}
"#;

/// How many elements the `bulk` cases give the list.
///
/// Large enough that the retention it is watching for is a multiple of the response's own size and
/// not a rounding error, small enough to stay well under the default in-flight ceiling.
const CELLS: usize = 8;

/// The counters a case watches, and the executor's two inputs.
struct Held<'q> {
  /// Counts the values handed to the executor as **variables**.
  arguments: Rc<Cell<usize>>,
  /// Counts everything else the test mints: the root, and every resolved field value.
  tree: Rc<Cell<usize>>,
  schema: Schema,
  document: ExecutableDocument<&'q str>,
}

fn watch<'q>(query: &'q str, variables: &[&'static str]) -> (Held<'q>, Handles) {
  assert_valid(HOLD_SDL, query);
  let (schema, document) = compile(HOLD_SDL, query);
  let arguments = Rc::new(Cell::new(0usize));
  let tree = Rc::new(Cell::new(0usize));
  let space = Handles {
    mint: Rc::clone(&tree),
    variables: variables
      .iter()
      .map(|name| (*name, Counted::text(&arguments, "V")))
      .collect(),
  };
  (
    Held {
      arguments,
      tree,
      schema,
      document,
    },
    space,
  )
}

/// A request is answered, so the value its argument was checked against is released.
///
/// The tight point, asserted as the tight point: the release is at the answer and not at the end of
/// the operation. Both halves matter — the value has to still be readable while the request is the
/// one being offered, which the first assertion pins, and gone once it is not.
#[test]
fn answering_a_request_releases_its_argument_value() {
  let query = "query ($text: String) { echo(text: $text) }";
  let (held, mut space) = watch(query, &["text"]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let request = executor.poll_resolve(&mut space).expect("echo");
  let id = request.id();
  match request.arguments() {
    [argument] => match argument.source() {
      ArgumentSource::Variable { name, value } => {
        assert_eq!(*name, "text");
        assert!(matches!(value.payload, Payload::Str("V")));
      }
      other => panic!("`text` is a bare variable and arrived as {other:?}"),
    },
    arguments => panic!("`echo` has one argument, and got {}", arguments.len()),
  }
  assert_eq!(
    held.arguments.get(),
    1,
    "the executor holds the checked value for as long as the request that carries it is readable"
  );

  executor.handle_resolved(&mut space, id, Counted::text(&held.tree, "E"));
  assert_eq!(
    held.arguments.get(),
    0,
    "the request is answered, so nothing can read its arguments again"
  );

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 0);
  assert_eq!(
    held.arguments.get(),
    0,
    "and the response holds none either"
  );
  let Node::Object(mut fields) = response.data() else {
    panic!("the root is an object")
  };
  let (key, value) = fields.next().expect("one field was selected");
  assert_eq!(key.to_string(), "echo");
  assert!(
    matches!(
      value,
      Node::Leaf(Counted {
        payload: Payload::Str("E"),
        ..
      })
    ),
    "releasing the arguments must not disturb the answer"
  );
}

/// Draft §6.4.4 discards the position a request would have filled, so its argument value goes too.
///
/// The request is never answered — the operation abandons it — which is the exit path an
/// answer-driven release would miss.
#[test]
fn an_abandoned_request_releases_its_argument_value() {
  let query = "query ($text: String) { nest { boom echo(text: $text) } }";
  let (held, mut space) = watch(query, &["text"]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Counted::obj(&held.tree));
  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  let echo = executor.poll_resolve(&mut space).expect("echo").id();
  assert_eq!(
    held.arguments.get(),
    1,
    "`echo` is in flight with its value"
  );

  // `boom` is `String!`, so failing it nulls `nest` and `echo` can no longer land anywhere.
  executor.handle_field_error(boom, "boom");
  assert_eq!(
    held.arguments.get(),
    0,
    "the position the argument was checked for is discarded, so the value is unreachable"
  );
  assert_eq!(executor.poll_abandoned(), Some(echo));

  // A driver that cancels asynchronously answers anyway, and the value it hands over must not be
  // kept by an executor that has nowhere to put it.
  let late = Rc::new(Cell::new(0usize));
  executor.handle_resolved(&mut space, echo, Counted::text(&late, "late"));
  assert_eq!(late.get(), 0, "a value for a retired request is dropped");

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 1);
  assert_eq!(held.arguments.get(), 0);
}

/// A `start` that refuses releases both the operation it reset and the root it was handed.
///
/// Draft §6.1's `GetOperation` failures return before anything is stored, and the reset that
/// precedes them has already emptied the last operation — so a refused restart is the point at
/// which the executor should be holding nothing at all.
#[test]
fn a_refused_start_releases_what_the_last_operation_held() {
  let query = "query ($text: String) { echo(text: $text) }";
  let (held, mut space) = watch(query, &["text"]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");
  executor.poll_resolve(&mut space).expect("echo");
  assert_eq!(held.arguments.get(), 1);

  let refused = executor
    .start(&mut space, Some("nope"), Counted::obj(&held.tree))
    .expect_err("the document has no operation with that name");
  assert_eq!(refused, StartError::UnknownOperation);
  assert_eq!(
    held.arguments.get(),
    0,
    "the operation the argument belonged to is over"
  );
  assert_eq!(
    held.tree.get(),
    0,
    "so is its response tree, and the root the refused start never stored"
  );
}

/// A restart that asks the driver for nothing still releases the last operation's argument value.
///
/// Draft §4.4's `__typename` is answered by the executor, so this operation runs to a complete
/// response without a single call in that coerces an argument. A release that rode on the next
/// coercion would leave the previous operation's value pinned for the whole of it.
#[test]
fn a_restart_that_asks_the_driver_nothing_releases_the_last_argument_value() {
  let query = "query first($text: String) { echo(text: $text) } query second { __typename }";
  let (held, mut space) = watch(query, &["text"]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, Some("first"), Counted::obj(&held.tree))
    .expect("the operation resolves");
  executor.poll_resolve(&mut space).expect("echo");
  assert_eq!(held.arguments.get(), 1);

  executor
    .start(&mut space, Some("second"), Counted::obj(&held.tree))
    .expect("the operation resolves");
  assert_eq!(
    held.arguments.get(),
    0,
    "`start` voids the operation the value belonged to"
  );
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "`__typename` is the executor's own answer, so there is no request to hand out"
  );

  let response = executor
    .poll_response()
    .expect("a response that needs no driver call is finished as soon as it is started");
  assert_eq!(response.error_count(), 0);
  assert_eq!(held.arguments.get(), 0);
}

/// Draft §6.4.1 raising on one argument releases the values of the ones that already passed.
///
/// `pair` is the case a per-answer release would still miss: the field is never offered, so there
/// is no request to answer and no request to abandon, and `first`'s value has already been read and
/// kept when `second` raises. It is the last ready slot too, so nothing coerces after it.
#[test]
fn an_argument_error_releases_the_values_that_passed_before_it() {
  let query = "query ($first: String, $second: String!) { pair(first: $first, second: $second) }";
  // `second` is declared and never supplied, which is draft §6.4.1 step 5.d's field error.
  let (held, mut space) = watch(query, &["first"]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "the only field raised an argument error, so it is never offered"
  );
  assert_eq!(
    held.arguments.get(),
    0,
    "`first` passed and `second` did not, and neither value can be read now"
  );

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 1);
  assert_eq!(
    response
      .errors()
      .next()
      .expect("one error was raised")
      .kind(),
    Kind::ArgumentVariableMissing
  );
  assert_eq!(held.arguments.get(), 0);
}

/// Draft §6.4.4 nulls a parent, so the values completed *below* the parent die with it.
///
/// The subtree is the part that is easy to leave behind. §6.4.4's rewrite is a walk of the
/// response's *depth*, and everything the driver already resolved underneath the position it nulls
/// is off that path — so a response of one `null` can be sitting on every value a large completed
/// subtree ever produced. The in-flight ceiling is no bound on that: it bounds outstanding work,
/// and these are finished.
#[test]
fn a_discarded_ancestor_releases_the_values_completed_beneath_it() {
  let query = "{ nest { bulk { text } boom } }";
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Counted::obj(&held.tree));
  let bulk = executor.poll_resolve(&mut space).expect("bulk").id();
  executor.handle_resolved(&mut space, bulk, Counted::list(&held.tree, CELLS));

  // `boom` is offered before the element fields the list enqueues, so holding it is what puts the
  // field error *after* the subtree it discards has completed.
  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  for _ in 0..CELLS {
    let text = executor
      .poll_resolve(&mut space)
      .expect("one cell's `text`")
      .id();
    executor.handle_resolved(&mut space, text, Counted::text(&held.tree, "T"));
  }
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "`boom` is the only request left outstanding"
  );
  assert_eq!(
    held.tree.get(),
    CELLS,
    "a leaf for each of the {CELLS} cells, and nothing else: the root, `nest` and every cell object \
     expired when the last child each had enqueued was offered, which is the success-path release"
  );

  // `boom` is `String!` on a nullable `nest`, so §6.4.4 nulls `nest` and the whole `bulk` subtree
  // leaves the response with it.
  executor.handle_field_error(boom, "boom");
  assert_eq!(
    held.tree.get(),
    0,
    "nothing at all. The subtree went with `nest`, and the root's own value expired when `nest` \
     was offered — a finished response holds no object value, which is the invariant \
     `poll_response` asserts"
  );

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 1);
  assert_eq!(
    held.tree.get(),
    0,
    "and the delivered response holds nothing at all — not the discarded subtree, and not the root \
     object either, whose value no reader of a `Response` has ever been able to reach"
  );
  let Node::Object(mut fields) = response.data() else {
    panic!("the root is an object")
  };
  let (key, value) = fields.next().expect("one field was selected");
  assert_eq!(key.to_string(), "nest");
  assert!(
    matches!(value, Node::Null),
    "releasing the subtree must not change what the response says"
  );
  assert!(fields.next().is_none(), "`nest` is the only root field");
}

/// A discarded *list element* releases its own subtree and leaves its siblings alone.
///
/// The element is the position §6.4.4 stops at whenever the element type is nullable, so it is the
/// discard root a list-shaped response hits most often — and the one where releasing too much would
/// take the rest of the list with it.
#[test]
fn a_discarded_list_element_releases_only_its_own_subtree() {
  let query = "{ nest { bulk { text boom } } }";
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Counted::obj(&held.tree));
  let bulk = executor.poll_resolve(&mut space).expect("bulk").id();
  executor.handle_resolved(&mut space, bulk, Counted::list(&held.tree, CELLS));

  // The first cell completes its `text` and then fails its `String!`.
  let text = executor.poll_resolve(&mut space).expect("text").id();
  executor.handle_resolved(&mut space, text, Counted::text(&held.tree, "T"));
  assert_eq!(
    held.tree.get(),
    1 + CELLS,
    "{CELLS} cells and one leaf. The root and `nest` expired at their last read; the cells have \
     not, because each still has a `boom` child that has never been offered"
  );
  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  executor.handle_field_error(boom, "boom");
  assert_eq!(
    held.tree.get(),
    CELLS - 1,
    "the first cell's object and leaf are gone; the other cells are not, and the root and `nest` \
     had already expired at their last read"
  );

  // The rest of the list completes normally.
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Counted::text(&held.tree, "T"));
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 1);
  let Node::Object(mut fields) = response.data() else {
    panic!("the root is an object")
  };
  let (_, nest) = fields.next().expect("`nest`");
  let Node::Object(mut wrap) = nest else {
    panic!("`nest` resolved to an object")
  };
  let (_, bulk) = wrap.next().expect("`bulk`");
  let Node::List(cells) = bulk else {
    panic!("`bulk` is a list")
  };
  let shape: Vec<bool> = cells
    .map(|(_, cell)| matches!(cell, Node::Object(_)))
    .collect();
  assert_eq!(
    shape,
    core::iter::once(false)
      .chain(core::iter::repeat_n(true, CELLS - 1))
      .collect::<Vec<_>>(),
    "the failing cell is null and every other cell is still an object"
  );
}

/// A discard above an already-discarded subtree releases what is left and nothing else.
///
/// This is the case that decides whether draining on discard is affordable. The inner cell was
/// drained when its own `String!` failed; nulling `nest` walks over it again, and the walk must
/// find it empty and stop rather than descend. What a counter can say is that the second discard
/// releases exactly the values the first one left. Whether it *walked* the drained cell is not
/// visible from out here — a second walk over an empty subtree releases nothing and changes no
/// response — so `proto::execute`'s own `a_drained_subtree_is_not_walked_again` pins that half.
#[test]
fn a_discard_above_an_already_discarded_subtree_releases_only_the_rest() {
  let query = "{ nest { bulk { text boom } boom } }";
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Counted::obj(&held.tree));
  let bulk = executor.poll_resolve(&mut space).expect("bulk").id();
  executor.handle_resolved(&mut space, bulk, Counted::list(&held.tree, CELLS));
  let outer = executor.poll_resolve(&mut space).expect("nest.boom").id();

  // The first cell fails, which discards that element and nothing above it.
  let text = executor.poll_resolve(&mut space).expect("text").id();
  executor.handle_resolved(&mut space, text, Counted::text(&held.tree, "T"));
  let inner = executor
    .poll_resolve(&mut space)
    .expect("the cell's boom")
    .id();
  executor.handle_field_error(inner, "boom");
  assert_eq!(
    held.tree.get(),
    CELLS - 1,
    "the {} cells the discard did not reach. The root and `nest` expired at their last read, and \
     the first cell went with its own discard",
    CELLS - 1
  );

  // The other cells complete, and then `nest` is nulled over the top of the drained one.
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Counted::text(&held.tree, "T"));
  }
  executor.handle_field_error(outer, "boom");
  assert_eq!(
    held.tree.get(),
    0,
    "nothing at all: the rest of the subtree went with `nest`, and the root expired at its last read"
  );

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 2);
  let Node::Object(mut fields) = response.data() else {
    panic!("the root is an object")
  };
  let (key, value) = fields.next().expect("`nest`");
  assert_eq!(key.to_string(), "nest");
  assert!(matches!(value, Node::Null));
}

/// One discard releases a completed subtree, an in-flight request's argument, and the request.
///
/// The three releases have three different mechanisms — the subtree walk, `retire_arguments`, and
/// `poll_abandoned` — and a discard is where all three come due at once. `deep` is selected after
/// `bulk` so that its `echo` is enqueued behind the cells' fields and is therefore the *last*
/// request offered, which is the only one whose argument values the executor still holds.
#[test]
fn a_discard_releases_a_completed_subtree_and_the_request_racing_it() {
  let query = "query ($t: String) { nest { bulk { text } deep { echo(text: $t) } boom } }";
  let (held, mut space) = watch(query, &["t"]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Counted::obj(&held.tree));
  let bulk = executor.poll_resolve(&mut space).expect("bulk").id();
  executor.handle_resolved(&mut space, bulk, Counted::list(&held.tree, CELLS));
  let deep = executor.poll_resolve(&mut space).expect("deep").id();
  executor.handle_resolved(&mut space, deep, Counted::obj(&held.tree));
  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  for _ in 0..CELLS {
    let text = executor
      .poll_resolve(&mut space)
      .expect("one cell's `text`")
      .id();
    executor.handle_resolved(&mut space, text, Counted::text(&held.tree, "T"));
  }
  let echo = executor.poll_resolve(&mut space).expect("echo").id();
  assert_eq!(
    held.arguments.get(),
    1,
    "`echo` is the request being offered, so its checked value is readable"
  );
  assert_eq!(
    held.tree.get(),
    1 + CELLS,
    "`deep` and a leaf for each of the {CELLS} cells. The root, `nest` and the cells expired at \
     their last read — `deep` has not, and that is the property that matters: its `echo` is the \
     request being offered, so its value is still lent out and must still be readable"
  );

  executor.handle_field_error(boom, "boom");
  assert_eq!(
    held.tree.get(),
    0,
    "the completed subtree under `nest` is gone, `deep` with it, and the root expired earlier — so \
     the discard of an in-flight request's parent releases that parent's value too"
  );
  assert_eq!(
    held.arguments.get(),
    0,
    "and neither is the position `echo`'s argument was checked for"
  );
  assert_eq!(
    executor.poll_abandoned(),
    Some(echo),
    "the request under the discarded subtree is still reported"
  );

  let late = Rc::new(Cell::new(0usize));
  executor.handle_resolved(&mut space, echo, Counted::text(&late, "late"));
  assert_eq!(late.get(), 0, "a value for a retired request is dropped");

  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 1);
  assert_eq!(
    held.tree.get(),
    0,
    "the delivered response holds no object value, `deep` included: its `echo` was retired, so the \
     last thing keeping that value readable is gone"
  );
}

// ------------------------------------------------------------------------------------------
// draft §6.1 GetOperation
// ------------------------------------------------------------------------------------------

fn start_error(sdl: &str, query: &str, operation: Option<&str>) -> StartError {
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
  let mut space = Space::default();
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, operation, obj(vec![]))
    .expect_err("the operation does not resolve")
}

#[test]
fn get_operation_refuses_what_it_cannot_run() {
  let sdl = "type Query { a: String } type Mutation { m: String }";
  assert_eq!(
    start_error(sdl, "query one { a } query two { a }", None),
    StartError::AmbiguousOperation
  );
  assert_eq!(
    start_error(sdl, "query one { a }", Some("two")),
    StartError::UnknownOperation
  );
  assert_eq!(
    start_error(sdl, "mutation { m }", None),
    StartError::NotAQuery,
    "a mutation is refused rather than run serially-by-accident"
  );
  assert_eq!(
    start_error(
      "type Query { a: String }",
      "fragment F on Query { a }",
      None
    ),
    StartError::NoOperation
  );
}

// ------------------------------------------------------------------------------------------
// `Limits::max_response_slots`: the executor's own ceiling on how much response it will build
// ------------------------------------------------------------------------------------------
//
// Draft §6.4.3's list clause is completed *synchronously* inside `handle_resolved`, so
// `max_in_flight` — which is only ever consulted by `poll_resolve` — bounds none of it. Every case
// below is about the other ceiling, and each one is written to fail on a fixture small enough to
// keep: the plants claim a length rather than materialising one, and the budgets are single
// digits, so nothing here has to approach exhaustion to prove that exhaustion is refused.

/// A budget low enough that the assertions read as arithmetic rather than as guesswork.
fn budget(slots: u32) -> Limits {
  Limits {
    max_response_slots: NonZeroU32::new(slots).expect("the budgets under test are not zero"),
    ..Limits::default()
  }
}

fn run_bounded(
  sdl: &str,
  query: &str,
  root: J,
  limits: Limits,
) -> (String, Vec<(Kind, String, String)>) {
  execute_bounded(sdl, query, root, Vec::new(), limits, data_and_errors)
}

const BUDGET_SDL: &str = r#"
type Query {
  nullable: [String]
  items: [String!]!
  a: String
  b: String
  c: String
}
"#;

/// A list longer than the budget is a field error at the list, not an allocation.
#[test]
fn a_list_past_the_budget_is_a_field_error() {
  // Three positions: the root, `nullable`, and one element. The fourth element the driver claims
  // is the one that cannot be represented.
  let (data, errors) = run_bounded(
    BUDGET_SDL,
    "{ nullable }",
    obj(vec![("nullable", J::Phantom(1000))]),
    budget(3),
  );

  assert_eq!(errors.len(), 1, "one error, at the position that ran out");
  assert_eq!(errors[0].0, Kind::ResponseBudget);
  assert_eq!(
    errors[0].1,
    "Cannot complete field Query.nullable: the response would exceed the executor's limit of 3 \
     positions."
  );
  assert_eq!(errors[0].2, "nullable", "the path names where it stopped");
  // `[String]` is nullable, so §6.4.4 stops at the list itself rather than nulling `data`.
  assert_eq!(data, r#"{"nullable":null}"#);
}

/// The same refusal propagates like any other field error when the position is non-null.
#[test]
fn a_list_past_the_budget_propagates_through_non_null() {
  let (data, errors) = run_bounded(
    BUDGET_SDL,
    "{ items }",
    obj(vec![("items", J::Phantom(1000))]),
    budget(3),
  );

  assert_eq!(errors.len(), 1);
  assert_eq!(errors[0].0, Kind::ResponseBudget);
  assert_eq!(
    data, "null",
    "`[String!]!` cannot hold the null, so draft §6.4.4 takes `data` with it"
  );
}

/// A length no `u32` can hold is refused by the budget, not truncated into a wrong path.
///
/// Before the budget existed this loop ran `usize::MAX` times, so there is no version of this case
/// that fails by assertion against the old code — it fails by not terminating, which is the
/// finding. The budget turns it into a single comparison.
#[test]
fn a_length_past_u32_is_refused_rather_than_narrowed() {
  let absurd = u32::MAX as usize + 1;
  let (data, errors) = run_bounded(
    BUDGET_SDL,
    "{ nullable }",
    obj(vec![("nullable", J::Phantom(absurd))]),
    budget(2),
  );

  assert_eq!(errors.len(), 1);
  assert_eq!(errors[0].0, Kind::ResponseBudget);
  assert_eq!(data, r#"{"nullable":null}"#);
}

/// The ceiling is on the response, so nesting cannot multiply its way past it.
///
/// This is the case a *per-list* cap would miss. Schema wrappers are bounded at 15, so a cap of
/// `C` per list still admits `C¹⁵` positions; counting positions instead makes the depth
/// irrelevant.
#[test]
fn nesting_cannot_multiply_past_the_budget() {
  let sdl = "type Query { matrix: [[String]] }";
  let rows = J::List(vec![
    J::Phantom(50),
    J::Phantom(50),
    J::Phantom(50),
    J::Phantom(50),
  ]);
  let (_, errors) = run_bounded(sdl, "{ matrix }", obj(vec![("matrix", rows)]), budget(16));

  assert!(
    errors
      .iter()
      .any(|(kind, ..)| *kind == Kind::ResponseBudget),
    "4 x 50 positions cannot fit in 16, whatever any single list's length is"
  );
}

/// The ceiling covers the object fan-out too, not only the list clause.
///
/// `push_child` is the one function that creates a position and both callers go through it, so a
/// selection set that cannot fit is refused by the same check for the same reason. Without this
/// the bound would be a guard on one loop rather than a property of the tree.
#[test]
fn an_object_wider_than_the_budget_is_refused() {
  // The root plus two of the three fields fit; the third does not.
  let (_, errors) = run_bounded(
    BUDGET_SDL,
    "{ a b c }",
    obj(vec![
      ("a", J::Str("A".to_owned())),
      ("b", J::Str("B".to_owned())),
      ("c", J::Str("C".to_owned())),
    ]),
    budget(3),
  );

  assert!(
    errors
      .iter()
      .any(|(kind, ..)| *kind == Kind::ResponseBudget),
    "the object could not be assembled within the ceiling"
  );
}

/// A response that fits is untouched, so the ceiling is a bound and not a behaviour change.
#[test]
fn a_response_inside_the_budget_is_unchanged() {
  let bounded = run_bounded(
    BUDGET_SDL,
    "{ nullable }",
    obj(vec![("nullable", J::Phantom(3))]),
    budget(8),
  );
  let default = run(
    BUDGET_SDL,
    "{ nullable }",
    obj(vec![("nullable", J::Phantom(3))]),
  );

  assert_eq!(bounded.1.len(), 0, "nothing is refused inside the ceiling");
  assert_eq!(bounded.0, r#"{"nullable":["x","x","x"]}"#);
  assert_eq!(
    bounded, default,
    "the budget changes nothing it does not refuse"
  );
}

// ------------------------------------------------------------------------------------------
// `Limits::max_response_metadata`: the ceiling on the product, not on either factor
// ------------------------------------------------------------------------------------------
//
// Draft §6.3 merges every selection sharing a response key into one field, and the group has to be
// recorded so `MergeSelectionSets` can read it when the value comes back. So one *position* costs
// one entry per *selection* in its group: the first count is the driver's, the second is the
// document's, each is bounded on its own, and `max_response_slots` bounds only the first. A list of
// N elements over a group of G selections therefore costs N x G metadata entries while spending N
// of the position budget — which is why the ceiling below is on the product.

/// `{ rows { x x … } }` — one response key, `duplicates` selections of it, over a list.
fn merged_query(duplicates: usize) -> String {
  let mut query = std::string::String::from("{ rows {");
  for _ in 0..duplicates {
    query.push_str(" x");
  }
  query.push_str(" } }");
  query
}

const MERGED_SDL: &str = "type Query { rows: [Row] } type Row { x: String }";

/// The metadata a merged group costs is charged, so the product cannot outrun the position budget.
#[test]
fn merged_selections_are_charged_against_their_own_ceiling() {
  // Twenty positions is ample for four rows; sixteen merged selections is not ample for
  // four rows x eight duplicate selections.
  let limits = Limits {
    max_response_slots: NonZeroU32::new(64).expect("not zero"),
    max_response_metadata: NonZeroU32::new(16).expect("not zero"),
    ..Limits::default()
  };
  let rows = J::Phantom(4);
  let (_, errors) = run_bounded(
    MERGED_SDL,
    &merged_query(8),
    obj(vec![("rows", rows)]),
    limits,
  );

  assert!(
    errors
      .iter()
      .any(|(kind, ..)| *kind == Kind::ResponseBudget),
    "4 rows x 8 selections cannot be recorded in 16 entries, though 4 rows fit in 64 positions"
  );
  assert!(
    errors
      .iter()
      .any(|(_, message, _)| message.contains("16 metadata entries")),
    "the message names the ceiling that was reached, not the other one: {errors:?}"
  );
}

/// The position ceiling alone does not bound the metadata, which is the whole finding.
///
/// The same document under a position ceiling generous enough to admit every row must still be
/// refused, because it is the product that is too large and not either factor. Without the merged
/// ceiling this passes silently while the two metadata vectors grow as rows x selections.
#[test]
fn the_position_ceiling_alone_does_not_bound_the_metadata() {
  let generous_positions = Limits {
    max_response_slots: NonZeroU32::new(1024).expect("not zero"),
    max_response_metadata: NonZeroU32::new(24).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    MERGED_SDL,
    &merged_query(16),
    obj(vec![("rows", J::Phantom(8))]),
    generous_positions,
  );

  assert!(
    errors
      .iter()
      .any(|(kind, ..)| *kind == Kind::ResponseBudget),
    "8 rows x 16 selections is 128 entries and the ceiling is 24, whatever the position budget says"
  );
}

/// A refused expansion appends nothing, so a later sibling still gets the room it was owed.
///
/// This is the ordering property — decide, then append — and it needs a fixture built to make it
/// *visible*, because a refusal that appended first would produce the same `data` and the same
/// `errors` for the request that was refused. Two things make it observable.
///
/// It cannot be seen across runs: `start` calls `reset`, which clears both metadata vectors, so
/// entries a refused expansion left behind are gone before a second request could trip over them.
/// The observation has to happen *inside* one response.
///
/// So the fixture spends the ceiling deliberately. One selection costs two entries — a merged
/// selection and its location — so the root's three keys cost 6, `a`'s group of eight brings it to
/// 22, and `b`'s group of five would need 32 against a ceiling of 28, so `b` is refused. `c`'s
/// group of two then needs 26, which fits. Under decide-then-append `c` succeeds and `b` is the
/// only error; under append-then-refuse `b` leaves 32 behind, `c` would need 36, and `c` is
/// refused too. The difference is a field in `data`, which an assertion can see.
#[test]
fn a_refused_expansion_leaves_the_room_a_later_sibling_is_owed() {
  let sdl = "type Query { a: A b: B c: C } type A { x: String } type B { y: String } \
             type C { z: String }";
  let query = "{ a { x x x x x x x x } b { y y y y y } c { z z } }";
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(28).expect("not zero"),
    ..Limits::default()
  };
  let root = J::Obj(
    "Query",
    vec![
      ("a", J::Obj("A", vec![("x", J::Str("X".to_owned()))])),
      ("b", J::Obj("B", vec![("y", J::Str("Y".to_owned()))])),
      ("c", J::Obj("C", vec![("z", J::Str("Z".to_owned()))])),
    ],
  );
  let (data, errors) = run_bounded(sdl, query, root, limits);

  assert_eq!(
    errors.len(),
    1,
    "only `b` is over the ceiling; a second error means the refusal left its entries behind: \
     {errors:?}"
  );
  assert_eq!(errors[0].2, "b", "and the one error is `b`'s");
  assert_eq!(
    data, r#"{"a":{"x":"X"},"b":null,"c":{"z":"Z"}}"#,
    "`c` still fits, which it would not if the refusal had spent the room first"
  );
}

/// A document that merges within the ceiling is untouched, so this is a bound and not a rewrite.
#[test]
fn merging_inside_the_ceiling_is_unchanged() {
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(64).expect("not zero"),
    ..Limits::default()
  };
  let bounded = run_bounded(
    MERGED_SDL,
    &merged_query(3),
    obj(vec![("rows", J::Phantom(2))]),
    limits,
  );
  let default = run_bounded(
    MERGED_SDL,
    &merged_query(3),
    obj(vec![("rows", J::Phantom(2))]),
    Limits::default(),
  );

  assert_eq!(bounded.1.len(), 0, "nothing is refused inside the ceiling");
  assert_eq!(
    bounded, default,
    "the ceiling changes nothing it does not refuse"
  );
}

// ------------------------------------------------------------------------------------------
// `Limits::max_selection_visits`: the walk is charged, and the walk has no native frames
// ------------------------------------------------------------------------------------------
//
// Two resources at once. A named fragment chain is flat in the document, so no parser depth limit
// sees it; a recursive walk spent one native frame per link and a measured 1,500 links aborted the
// process with SIGABRT. The walk now runs on an explicit stack, which turns depth into heap, and
// the visit budget is what bounds that heap *and* the collection work. Both plants are below.

/// `{ ...F0 }` with `F0 → F1 → … → Fn`, every definition at nesting depth one.
fn fragment_chain(links: usize) -> String {
  let mut query = std::string::String::from("{ ...F0 }\n");
  for i in 0..links {
    query.push_str(&std::format!(
      "fragment F{i} on Query {{ ...F{} }}\n",
      i + 1
    ));
  }
  query.push_str(&std::format!("fragment F{links} on Query {{ a }}\n"));
  query
}

/// A fragment chain deep enough to have aborted the process now answers.
///
/// This is the regression for the abort, and it is the one case here that cannot be written as a
/// red-first assertion: against a recursive walk it does not fail, it terminates the test binary
/// with `SIGABRT`, taking every other case in the file with it. Measured when the recursion was
/// deleted: 1,000 links survived, 1,500 aborted. **Re-measured by restoring the recursion, release
/// build, different platform: 1,500 answered in 1.9 ms and 10,000 aborted with `SIGABRT`** — so the
/// fixture is ten thousand, and a threshold that moves that far with the frame layout is the reason
/// there is no depth ceiling to set.
///
/// Surviving the walk and walking it in *linear time* were two defects, and this closed only the
/// first: the same chain still scanned every definition once per spread. The second is pinned by
/// `a_flat_fragment_chain_is_linear` in `src/proto/execute/tests.rs`, from inside the crate,
/// because the cost of a collection is not observable from out here.
#[test]
fn a_flat_fragment_chain_no_longer_ends_the_process() {
  let (data, errors) = run_bounded(
    "type Query { a: String }",
    &fragment_chain(10_000),
    obj(vec![("a", J::Str("A".to_owned()))]),
    Limits::default(),
  );

  assert_eq!(
    errors.len(),
    0,
    "a chain is valid, not an error: {errors:?}"
  );
  assert_eq!(
    data, r#"{"a":"A"}"#,
    "and it collects the field at the end of it"
  );
}

/// The visit budget charges a walk that appends nothing.
///
/// The appending path is charged by the metadata ceiling too, so a plant on the counter has to be
/// caught by something *only* the counter can catch. This document collects a single field and
/// walks thousands of fragments to reach it: with the counter removed the metadata ceiling sees
/// two entries and is content, and the walk is free.
#[test]
fn collection_that_appends_nothing_is_still_charged() {
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(64).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    "type Query { a: String }",
    &fragment_chain(4_000),
    obj(vec![("a", J::Str("A".to_owned()))]),
    limits,
  );

  assert!(
    errors
      .iter()
      .any(|(kind, ..)| *kind == Kind::ResponseBudget),
    "4,000 spreads cannot be walked within 64 visits, though they append two entries in total"
  );
  assert!(
    errors
      .iter()
      .any(|(_, message, _)| message.contains("64 selection visits")),
    "and the message names the budget that stopped it: {errors:?}"
  );
}

/// The same budget charges a walk that appends on every step.
///
/// The other half of the pair. Here every selection survives collection, so the metadata ceiling
/// would also refuse this document eventually — which is exactly why it is not enough on its own
/// to pin the counter, and why both plants are needed to prove the counter is load-bearing.
#[test]
fn collection_that_appends_on_every_step_is_charged_by_the_same_budget() {
  let mut query = std::string::String::from("{");
  for i in 0..200 {
    query.push_str(&std::format!(" k{i}: a"));
  }
  query.push_str(" }");

  let limits = Limits {
    max_selection_visits: NonZeroU32::new(32).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    "type Query { a: String }",
    &query,
    obj(vec![("a", J::Str("A".to_owned()))]),
    limits,
  );

  assert!(
    errors
      .iter()
      .any(|(_, message, _)| message.contains("32 selection visits")),
    "200 aliased selections cannot be examined within 32 visits: {errors:?}"
  );
}

/// A document inside the budget is untouched.
#[test]
fn collection_inside_the_visit_budget_is_unchanged() {
  let bounded = run_bounded(
    "type Query { a: String }",
    &fragment_chain(8),
    obj(vec![("a", J::Str("A".to_owned()))]),
    Limits {
      max_selection_visits: NonZeroU32::new(1024).expect("not zero"),
      ..Limits::default()
    },
  );
  let default = run_bounded(
    "type Query { a: String }",
    &fragment_chain(8),
    obj(vec![("a", J::Str("A".to_owned()))]),
    Limits::default(),
  );

  assert_eq!(bounded.1.len(), 0);
  assert_eq!(bounded.0, r#"{"a":"A"}"#);
  assert_eq!(
    bounded, default,
    "the budget changes nothing it does not refuse"
  );
}

/// A refused request is refused again on the **same** executor, which is the property a budget is.
///
/// # The escape this closes
///
/// The fragment index is built on the first spread and kept across `reset`, because it is a
/// function of the document. The *charge* for building it used to be kept with it — so the second
/// operation on an executor found the table already there and paid nothing for it:
///
/// 1. the first `start` pays for the index pass, runs out of budget in the walk that follows, and
///    is refused, leaving the table built;
/// 2. the second `start` — same executor, same document, same limits — skips the pass entirely;
/// 3. the request that was **refused** is now **served**.
///
/// The objection to calling that a defect is that the accounting is sound: the work really was done
/// once, and the document cannot change underneath it. Both halves are true and neither is the
/// point. What a client observes is not the work, it is the answer, and the answer moved between
/// call one and call two. A ceiling that a client clears by sending the request a second time is
/// not a ceiling.
///
/// # Why this fixture reuses an executor when no other one does
///
/// Every other case in this file and in `src/proto/execute/tests.rs` constructs a fresh executor,
/// which is precisely the shape that cannot see this: a fresh executor has an unbuilt table, so its
/// first operation always pays. The reuse *is* the fixture. Rewriting it to run twice through
/// [`run_bounded`] would make it green against the defect.
///
/// # The numbers
///
/// A chain of `LINKS` links defines `LINKS + 1` fragments in `LINKS + 2` definitions, so the index
/// pass costs `2 · LINKS + 3`, and walking it afterwards costs about the same again — one visit per
/// selection and about one comparison per spread. The ceiling is the pass plus `LINKS`: comfortably
/// past the pass, so the first run reaches the walk and leaves the table built, and comfortably
/// short of pass-plus-walk, so it is refused. A second run that skipped the pass would have the
/// whole walk inside what is left.
#[test]
fn a_refused_request_is_refused_again_on_the_same_executor() {
  const LINKS: usize = 16;
  /// One unit per definition walked and one per fragment pushed.
  const INDEX: u32 = 2 * LINKS as u32 + 3;

  let query = fragment_chain(LINKS);
  let (schema, document) = compile("type Query { a: String }", &query);
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(INDEX + LINKS as u32).expect("not zero"),
    ..Limits::default()
  };
  let mut space = Space::default();
  let mut executor = Executor::with_limits(&schema, &document, limits);

  let root = || obj(vec![("a", J::Str("A".to_owned()))]);
  let first = drive(&mut executor, &mut space, root(), data_and_errors);
  let second = drive(&mut executor, &mut space, root(), data_and_errors);

  assert!(
    first
      .1
      .iter()
      .any(|(kind, ..)| *kind == Kind::ResponseBudget),
    "the fixture only says anything if the first run is refused: {first:?}"
  );
  assert!(
    first
      .1
      .iter()
      .any(|(_, message, _)| message.contains("selection visits")),
    "and refused by the visit budget rather than by some other ceiling: {first:?}"
  );
  assert_eq!(
    second, first,
    "the same request, on the same executor, under the same limits, answered differently the \
     second time it was asked"
  );
}

// ------------------------------------------------------------------------------------------
// `Limits::max_interned_bytes`: a correctness ceiling before it is a resource one
// ------------------------------------------------------------------------------------------

/// A driver message that will not fit loses its text and keeps its error.
///
/// The alternative this replaces was not a larger allocation, it was silent corruption: arena
/// offsets are `u32`, so an arena past four gigabytes truncates and every name interned afterwards
/// reads back somebody else's bytes. The field still fails, the reader is told the text is
/// missing, and — the part that matters — the response key beside it is still its own.
#[test]
fn a_driver_message_too_large_to_store_still_reports_its_error() {
  let limits = Limits {
    max_interned_bytes: NonZeroU32::new(16).expect("not zero"),
    ..Limits::default()
  };
  let long = "x".repeat(4096);
  let (data, errors) = run_bounded(
    "type Query { a: String b: String }",
    "{ a b }",
    obj(vec![
      ("a", J::Fail(std::boxed::Box::leak(long.into_boxed_str()))),
      ("b", J::Str("B".to_owned())),
    ]),
    limits,
  );

  assert_eq!(
    errors.len(),
    1,
    "the driver's failure is still reported: {errors:?}"
  );
  assert_eq!(
    errors[0].0,
    Kind::Resolver,
    "and still as the driver's, not as a budget"
  );
  assert!(
    errors[0].1.contains("exceeded the executor's storage"),
    "the reader is told the text was dropped rather than shown the wrong text: {}",
    errors[0].1
  );
  assert_eq!(
    data, r#"{"a":null,"b":"B"}"#,
    "and `b`'s response key is still `b`, which is the property the ceiling exists for"
  );
}

/// Interning inside the ceiling is untouched, message text included.
#[test]
fn a_driver_message_inside_the_ceiling_keeps_its_text() {
  let (_, errors) = run(
    "type Query { a: String }",
    "{ a }",
    obj(vec![("a", J::Fail("the resolver said no"))]),
  );

  assert_eq!(errors.len(), 1);
  assert_eq!(errors[0].1, "the resolver said no");
}

// ------------------------------------------------------------------------------------------
// A resource check must sit where exhaustion means the operation cannot proceed
// ------------------------------------------------------------------------------------------
//
// The ceilings above bound resources. A ceiling placed on a path that does not need the resource
// stops being a resource limit and becomes a correctness defect: the engine has the right answer
// and discards it because an arena it never needed is full. The cases below pin the two halves of
// that rule at draft §6.4.3 step 5, where the runtime type's *spelling* is wanted for exactly one
// thing — quoting it in the error when the type is not possible — and for nothing at all when it
// is.

/// A schema whose concrete type name is far longer than its response keys.
///
/// The length is the instrument: it lets one ceiling admit every key the response emits and still
/// refuse the runtime type's spelling, which is the only way to tell a path that *needs* the arena
/// from one that merely passed through it. A fixture whose names all fit proves nothing — the
/// first version of these cases had that defect and stayed green against the planted bug.
const ARENA_SDL: &str = r#"
type Query {
  pet: Pet
}
interface Pet {
  n: String
}
type AbsurdlyLongConcreteTypeNameForTheArena implements Pet {
  n: String
}
"#;

/// The concrete type name, 39 bytes, against ceilings chosen to admit keys and refuse it.
const LONG_TYPE: &str = "AbsurdlyLongConcreteTypeNameForTheArena";

/// A resolvable abstract position does not touch the interner, so no arena ceiling can null it.
///
/// `ResolveAbstractType` has the concrete type id from the schema before any name is stored.
/// Interning on the way past turned a full arena into `AbstractUnresolved` plus a null on a query
/// that was answerable — the right answer was already in hand and was thrown away.
///
/// Eight bytes admits `pet` and `n`, the two keys this response emits, and comes nowhere near the
/// thirty-nine the type name would need.
#[test]
fn a_resolvable_abstract_type_does_not_need_the_interner() {
  let limits = Limits {
    max_interned_bytes: NonZeroU32::new(8).expect("not zero"),
    ..Limits::default()
  };
  let (data, errors) = run_bounded(
    ARENA_SDL,
    "{ pet { n } }",
    obj(vec![(
      "pet",
      J::Obj(LONG_TYPE, vec![("n", J::Str("Rex".to_owned()))]),
    )]),
    limits,
  );

  assert_eq!(
    errors.len(),
    0,
    "the concrete type resolved from the schema, and answering it stores nothing: {errors:?}"
  );
  assert_eq!(data, r#"{"pet":{"n":"Rex"}}"#);
}

/// The same position with `__typename` selected *does* need the interner, and refusing is right.
///
/// The other half of the rule, and the reason the fix is not "never intern": here the stored name
/// **is** the answer, so an arena with no room for it cannot produce one. Twenty bytes admits both
/// keys — `pet` and `__typename` — and still refuses the thirty-nine-byte value.
#[test]
fn a_typename_on_an_abstract_type_does_need_the_interner() {
  let limits = Limits {
    max_interned_bytes: NonZeroU32::new(20).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    ARENA_SDL,
    "{ pet { __typename } }",
    obj(vec![("pet", J::Obj(LONG_TYPE, vec![]))]),
    limits,
  );

  assert!(
    errors
      .iter()
      .any(|(kind, ..)| *kind == Kind::ResponseBudget),
    "`__typename`'s value is the stored name, so running out is the correct answer: {errors:?}"
  );
}

/// An impossible runtime type whose name cannot be stored keeps its diagnosis and loses its quote.
///
/// The degraded message is a decision rather than a fallback. An empty quote renders as
/// `Runtime Object type ""`, and a placeholder like `<unknown>` reads like a type somebody could go
/// looking for; both invite a reader to hunt for a type the driver never named. Naming the ceiling
/// instead says exactly what happened.
#[test]
fn an_impossible_type_that_cannot_be_quoted_still_says_what_went_wrong() {
  let limits = Limits {
    max_interned_bytes: NonZeroU32::new(20).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    ARENA_SDL,
    "{ pet { n } }",
    obj(vec![(
      "pet",
      J::Obj(
        "AnEquallyLongNameThatIsNotAPetAtAllHere",
        vec![("n", J::Str("Buck".to_owned()))],
      ),
    )]),
    limits,
  );

  assert_eq!(errors.len(), 1);
  assert_eq!(
    errors[0].0,
    Kind::AbstractNotPossible,
    "still the same failure — the driver named a type the position cannot hold: {errors:?}"
  );
  assert!(
    errors[0].1.contains("its name could not be stored"),
    "and it says the name is missing rather than quoting something: {}",
    errors[0].1
  );
  assert!(
    !errors[0].1.contains("\"\""),
    "in particular it renders no empty type name: {}",
    errors[0].1
  );
  assert!(
    errors[0].1.contains("20 interned bytes"),
    "and names the ceiling that actually refused: {}",
    errors[0].1
  );
}

// ------------------------------------------------------------------------------------------
// The refusal and its ceiling travel together
// ------------------------------------------------------------------------------------------
//
// `Interner::intern` refuses for two resources: the arena has no room for the bytes, or the visit
// budget has none for the probe run that looks for them. Every diagnostic about a refusal names a
// number, and the two sites below used to name the *arena's* whichever had happened — so a caller
// whose `max_selection_visits` ran out was told to raise `max_interned_bytes`, a knob that was
// never the constraint. The pairing is now unwritable: `Unstored` carries its own ceiling.
//
// Both fixtures spend the budget to zero during collection and then reach an intern whose bucket is
// already occupied, which is what makes the probe loop run and the charge fall due. The name they
// hand it is one the collection already interned, so the bucket is occupied by construction rather
// than by luck about a hash.

/// A driver message the *work* ceiling refused says so, rather than blaming the arena.
#[test]
fn a_driver_message_refused_for_work_names_the_work_ceiling() {
  // Exactly what `{ a }` costs: one selection examined, and an intern into an empty table that
  // compares nothing. So the budget is spent when the driver's failure arrives.
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(1).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    "type Query { a: String }",
    "{ a }",
    obj(vec![("a", J::Fail("a"))]),
    limits,
  );

  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(
    errors[0].0,
    Kind::Resolver,
    "the driver's failure is still the finding, whichever ceiling ate the text: {errors:?}"
  );
  assert!(
    errors[0].1.contains("1 selection visits"),
    "and the message names the ceiling that refused, which is the knob an operator can move: {}",
    errors[0].1
  );
  assert!(
    !errors[0].1.contains("interned bytes"),
    "not the arena's, which had room for three bytes and was never consulted: {}",
    errors[0].1
  );
}

/// An impossible runtime type the *work* ceiling could not quote says so too.
#[test]
fn an_impossible_type_refused_for_work_names_the_work_ceiling() {
  // One selection at the root and an intern that compares nothing, as above. The driver then names
  // `pet` as the runtime type: the schema knows the spelling — it is a field — so `sym` answers and
  // `type_of_sym` does not, which is the "not a possible type" branch, and the name it wants to
  // quote is the response key already sitting in that bucket.
  let limits = Limits {
    max_selection_visits: NonZeroU32::new(1).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    ARENA_SDL,
    "{ pet { n } }",
    obj(vec![("pet", J::Obj("pet", vec![("n", J::Null)]))]),
    limits,
  );

  assert_eq!(errors.len(), 1, "{errors:?}");
  assert_eq!(
    errors[0].0,
    Kind::AbstractNotPossible,
    "still the driver naming a type the position cannot hold: {errors:?}"
  );
  assert!(
    errors[0].1.contains("1 selection visits"),
    "and it names the ceiling that silenced the quote; this arm used to render the arena's cap \
     unconditionally, so it read `16777216 interned bytes` against an arena that was empty: {}",
    errors[0].1
  );
  assert!(!errors[0].1.contains("interned bytes"), "{}", errors[0].1);
}

/// A missing variable reports a missing variable, whether or not its name can be quoted.
///
/// The third site of the same shape. The request did not supply the variable — that is the
/// finding, and it does not depend on storage — so an exhausted arena shortens the sentence and
/// leaves the diagnosis, rather than swapping it for one about the interner.
#[test]
fn an_unsupplied_variable_keeps_its_diagnosis_when_its_name_cannot_be_stored() {
  let limits = Limits {
    max_interned_bytes: NonZeroU32::new(8).expect("not zero"),
    ..Limits::default()
  };
  let (_, errors) = run_bounded(
    "type Query { needs(flag: Boolean!): String }",
    "query ($absent: Boolean!) { needs(flag: $absent) }",
    obj(vec![("needs", J::Str("x".to_owned()))]),
    limits,
  );

  assert_eq!(errors.len(), 1);
  assert_eq!(
    errors[0].0,
    Kind::ArgumentVariableMissing,
    "an argument problem, not a storage problem"
  );
  assert!(
    errors[0]
      .1
      .contains("was provided a variable which was not provided a runtime value"),
    "and the sentence shortens rather than changing: {}",
    errors[0].1
  );
}

// ------------------------------------------------------------------------------------------
// A refused expansion must cost its siblings nothing
// ------------------------------------------------------------------------------------------
//
// `expand` commits one collected group at a time, so a later group can be refused with earlier
// ones already in. The parent is nulled either way — but the charges those earlier groups made used
// to stay, and the next sibling then met a budget smaller than it was owed. That is not a denial
// and not a degradation: a valid query gets a *wrong response*, because a field that fitted is
// nulled to pay for a field that did not.
//
// Sizing is the whole test in both cases below. The ceiling has to admit the *final correct
// response* and refuse the *transient spend*, or the case cannot tell a leak from a limit — which
// is the way the abstract-type fixture managed to pass against its own planted bug.

const TXN_SDL: &str = r#"
type Query {
  bad: Bad
  other: Other
}
type Bad {
  a: String
  b: String
}
type Other {
  ok: String
}
"#;

fn txn_root() -> J {
  J::Obj(
    "Query",
    vec![
      (
        "bad",
        J::Obj(
          "Bad",
          vec![("a", J::Str("A".to_owned())), ("b", J::Str("B".to_owned()))],
        ),
      ),
      (
        "other",
        J::Obj("Other", vec![("ok", J::Str("OK".to_owned()))]),
      ),
    ],
  )
}

/// A half-committed expansion gives its metadata back, so the next sibling gets what it is owed.
///
/// **Why seven.** A group of one selection costs two entries, one merged selection and one
/// location. The root's two keys spend 4; `bad`'s first group takes it to 6 and its second would
/// need 8, so `bad` is refused. The correct degraded response is root plus `other.ok` — 6 — and it
/// fits under 7. `bad`'s abandoned first group would take the running total to 6 before `other` is
/// even reached, and `other` would then need 8. So seven is the only interesting number here: it
/// admits the right answer and refuses the leaked one, and any ceiling that admits both or refuses
/// both would pass whether or not the charges came back.
#[test]
fn a_refused_expansion_costs_its_siblings_nothing() {
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(7).expect("not zero"),
    ..Limits::default()
  };
  let (data, errors) = run_bounded(TXN_SDL, "{ bad { a b } other { ok } }", txn_root(), limits);

  assert_eq!(
    errors.len(),
    1,
    "only `bad` is over the ceiling; a second error means its charges were still being held: \
     {errors:?}"
  );
  assert_eq!(errors[0].2, "bad", "and the one error is `bad`'s");
  assert_eq!(
    data, r#"{"bad":null,"other":{"ok":"OK"}}"#,
    "`other` fitted and must be in the response, not nulled to pay for `bad`"
  );
}

const TXN_ARENA_SDL: &str = r#"
type Query {
  bad: Bad
  other: Other
}
type Bad {
  a: String
  boom: String
}
type Other {
  ok: String
}
"#;

/// A failed collection gives its interned keys back, so a later sibling can still name itself.
///
/// The medium's shape: the walk interns a response key, *then* meets an unreadable
/// `@include` and faults. The key belongs to a field that will never be a position, and leaving it
/// in the arena spends a sibling's storage.
///
/// **Why nineteen.** The keys the correct response emits are `bad` (3), `other` (5) and `ok` (2),
/// and the fault's own message keeps `missing` (7) — 17 in all, which fits. The throwaway alias is
/// ten bytes, so an arena that kept it would stand at 18 before `other` is reached and `ok` would
/// need 20. Nineteen is between the two. It is also why the alias is spelled at length: with a
/// short key the leaked and unleaked totals both fit under any ceiling that admits the answer, and
/// the case would pass against the very defect it is for.
#[test]
fn a_failed_collection_gives_its_interned_keys_back() {
  let limits = Limits {
    max_interned_bytes: NonZeroU32::new(19).expect("not zero"),
    ..Limits::default()
  };
  let (data, errors) = run_bounded(
    TXN_ARENA_SDL,
    "query ($missing: Boolean!) { bad { zzzzzzzzzz: a boom @include(if: $missing) } other { ok } }",
    J::Obj(
      "Query",
      vec![
        ("bad", J::Obj("Bad", vec![("a", J::Str("A".to_owned()))])),
        (
          "other",
          J::Obj("Other", vec![("ok", J::Str("OK".to_owned()))]),
        ),
      ],
    ),
    limits,
  );

  assert_eq!(
    errors.len(),
    1,
    "only `bad`'s condition is unreadable; a second error means the throwaway key was still \
     holding storage: {errors:?}"
  );
  assert_eq!(errors[0].0, Kind::DirectiveCondition);
  assert_eq!(errors[0].2, "bad");
  assert!(
    errors[0].1.contains("$missing"),
    "and the restore left room to name the variable the message is about: {}",
    errors[0].1
  );
  assert_eq!(
    data, r#"{"bad":null,"other":{"ok":"OK"}}"#,
    "`other` must still be able to intern its own key"
  );
}

// ------------------------------------------------------------------------------------------
// An object's value dies when its last reader departs, not at the next `start`
// ------------------------------------------------------------------------------------------
//
// An object's `V` has exactly one reader in the whole program: `poll_resolve` lending it as
// `parent_value` for the request it is returning. Rendering never touches it — `Node` has no
// variant that carries an object's value — so once the last enqueued child has been offered it is
// unreachable by any public API and unread by any internal path. Holding it to the next `start`
// pinned one driver handle per object, bounded by `max_response_slots` rather than by
// `max_in_flight`, and the whole of the `Drop` suite above pins only failure-path releases, so
// nothing here observed the success path at all.
//
// These are the two directions. Releasing too late is the defect; releasing too early hands a
// resolver another object's value, which is worse, so both are pinned.

/// Every object value in a completed list is gone before the response is delivered.
#[test]
fn a_completed_list_holds_no_object_value_at_delivery() {
  let query = "{ nest { bulk { text } } }";
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Counted::obj(&held.tree));
  let bulk = executor.poll_resolve(&mut space).expect("bulk").id();
  executor.handle_resolved(&mut space, bulk, Counted::list(&held.tree, CELLS));
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    executor.handle_resolved(&mut space, id, Counted::text(&held.tree, "T"));
  }

  assert_eq!(
    held.tree.get(),
    CELLS,
    "the {CELLS} leaves, and not one of the {} object values — the root, `nest` and every cell — \
     which before this fix were all still held here",
    CELLS + 2
  );
  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 0);
  assert_eq!(
    held.tree.get(),
    CELLS,
    "and delivery changes nothing: the leaves are the response, the object values were not"
  );
}

/// The root is an instance, and it is the one a completion-path fix would miss.
///
/// `start` stores the root's value, not `complete`, so a release attached to the completion code
/// path never sees it. Attaching the release to the *state* catches it, because `start` expands the
/// root through the same `expand` every other object goes through.
#[test]
fn the_root_object_value_is_released_when_its_last_child_is_offered() {
  let query = r#"{ echo(text: "x") }"#;
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");
  assert_eq!(held.tree.get(), 1, "the root's value, held by `start`");

  let echo = executor.poll_resolve(&mut space).expect("echo").id();
  assert_eq!(
    held.tree.get(),
    1,
    "still held while `echo` is the offered request, because `echo` is reading it"
  );

  executor.handle_resolved(&mut space, echo, Counted::text(&held.tree, "E"));
  assert_eq!(
    held.tree.get(),
    1,
    "the leaf, and only the leaf: the root's value went at the entry point that ended its lend"
  );
}

/// An object that enqueues nothing expires inside the call that completed it.
///
/// A sub-selection of only `__typename` is answered by the executor, so no child is ever enqueued
/// and no offer will ever arrive to notice the reader set is empty. A release-at-last-offer scheme
/// waits for an event that cannot happen; settling on every live exit of `expand` is what makes
/// this case release at all.
#[test]
fn an_object_that_enqueues_nothing_expires_immediately() {
  let query = "{ nest { __typename } }";
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, Counted::obj(&held.tree));
  assert_eq!(
    held.tree.get(),
    0,
    "`nest` enqueued nothing, so its value died inside the `handle_resolved` that completed it — \
     and the root's died at the same entry point"
  );

  assert!(executor.poll_resolve(&mut space).is_none());
  let response = executor.poll_response().expect("nothing is outstanding");
  assert_eq!(response.error_count(), 0);
}

/// A child that fails argument coercion is a read that will not happen, and is accounted as one.
///
/// It leaves Ready without ever being offered, so nothing borrows the parent and the expiry is
/// immediate rather than parked.
#[test]
fn a_child_that_never_gets_offered_still_releases_its_parent() {
  let query = r#"query ($missing: String!) { pair(first: "a", second: $missing) }"#;
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");
  assert_eq!(held.tree.get(), 1, "the root's value");

  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "the only candidate raises at draft §6.4.1 and is never offered"
  );
  assert_eq!(
    held.tree.get(),
    0,
    "and the root's value goes with it, at that `poll_resolve` rather than at the next `start`"
  );
}

/// The last offered child must still be able to read its parent's value.
///
/// The over-release direction, and the one a release-only suite would pass a broken fix on. When
/// the last enqueued child is offered the parent's count reaches zero — but the `FieldRequest`
/// being returned borrows that very value, so the expiry is parked for the next call in. Plant the
/// transition one step early and this goes red two ways at once: the offer path's parent read hits
/// its `unreachable!`, and if it did not, the payload below would not be the parent's.
#[test]
fn the_last_offered_child_still_reads_its_parents_value() {
  let query = "{ nest { boom } }";
  let (held, mut space) = watch(query, &[]);
  let mut executor = Executor::new(&held.schema, &held.document);
  executor
    .start(&mut space, None, Counted::obj(&held.tree))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  // A tagged payload, so the assertion below is about *which* value is lent and not merely that
  // some value is. The executor decides an object by the schema's type, never by the payload.
  executor.handle_resolved(&mut space, nest, Counted::text(&held.tree, "PARENT"));

  let request = executor
    .poll_resolve(&mut space)
    .expect("`boom`, the only child `nest` enqueued");
  match &request.parent_value().payload {
    Payload::Str(tag) => assert_eq!(
      *tag, "PARENT",
      "the last offered child reads its own parent's value"
    ),
    other => panic!("the parent's value must still be readable, and its own: {other:?}"),
  }
  assert_eq!(
    held.tree.get(),
    1,
    "and it is still alive while lent — an expiry one step early would have dropped it here"
  );
}

// ------------------------------------------------------------------------------------------
// A budget tripped at the top level is still a well-formed error
// ------------------------------------------------------------------------------------------

/// A root-level budget refusal carries a location and does not name a field that does not exist.
///
/// Two contract breaks at one site. `fail` reads the slot's stored locations range, and the root's
/// is empty by construction — `start` builds it as `(0, 0)`, because the root has no field to have
/// a span — so the error reached the driver with `locations()` empty, against this crate's own
/// "always at least one entry for any error a driver can observe". The same row rendered through
/// the root's owner, and `start` fills the root's `field_sym` with the root *type's* name, so the
/// message named `Query.Query` — a field no schema has.
#[test]
fn a_root_budget_error_has_a_location_and_names_no_impossible_field() {
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(2).expect("not zero"),
    ..Limits::default()
  };
  let located = execute_bounded(
    BUDGET_SDL,
    "{ a b c }",
    obj(vec![("a", J::Str("A".to_owned()))]),
    Vec::new(),
    limits,
    |response| {
      response
        .errors()
        .map(|error| {
          (
            error.to_string(),
            error
              .locations()
              .iter()
              .map(|span| (span.start(), span.end()))
              .collect::<Vec<_>>(),
          )
        })
        .collect::<Vec<_>>()
    },
  );

  assert_eq!(located.len(), 1, "one refusal at the root: {located:?}");
  let (message, locations) = &located[0];
  assert!(
    !locations.is_empty(),
    "an error a driver can observe always has at least one location: {message}"
  );
  assert!(
    !message.contains("Query.Query"),
    "the root has no field, so the `Type.field` form would name one that does not exist: {message}"
  );
  assert!(
    message.contains("root selection set"),
    "it names the position it could not complete, which at the root is the selection set: {message}"
  );
}

/// A non-root budget refusal points at the selection it refused, not the whole parent group.
#[test]
fn a_budget_refusal_points_at_the_selection_it_refused() {
  let query = "{ bad { a b } other { ok } }";
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(7).expect("not zero"),
    ..Limits::default()
  };
  let located = execute_bounded(TXN_SDL, query, txn_root(), Vec::new(), limits, |response| {
    response
      .errors()
      .map(|error| {
        (
          error.path().to_string(),
          error
            .locations()
            .iter()
            .map(|span| (span.start(), span.end()))
            .collect::<Vec<_>>(),
        )
      })
      .collect::<Vec<_>>()
  });

  assert_eq!(located.len(), 1);
  let (path, locations) = &located[0];
  assert_eq!(path, "bad");
  assert_eq!(locations.len(), 1, "the one selection that was refused");
  let (start, end) = locations[0];
  assert_eq!(
    &query[start..end],
    "b",
    "and it is `b`, the group that did not fit — not `bad`, whose group did"
  );
}

/// The *position* ceiling refuses mid-expansion, and its rollback owes the sibling the same thing.
///
/// Charging collection against the metadata ceiling moved every metadata refusal one step earlier,
/// to the staging buffer — which is the point of it, and which left `expand`'s own refusal arm
/// exercised by nothing. Planting a late mark or a location-less `fail` there went green against
/// the whole suite. The arm is still live for the two ceilings collection cannot pre-empt, the slot
/// ceiling and the `__typename` arena, so this pins it through the first.
///
/// **Why four.** The root plus `bad` and `other` is three positions; `bad`'s first child takes it to
/// four and its second is refused. The correct degraded response is root, `bad`, `other` and
/// `other.ok` — four — which fits exactly, while `bad`'s abandoned child would leave the count at
/// four before `other` is reached and `other.ok` would need five. Any other ceiling admits both or
/// refuses both.
#[test]
fn a_position_refusal_mid_expansion_costs_its_siblings_nothing() {
  let query = "{ bad { a b } other { ok } }";
  let limits = Limits {
    max_response_slots: NonZeroU32::new(4).expect("not zero"),
    ..Limits::default()
  };
  let located = execute_bounded(TXN_SDL, query, txn_root(), Vec::new(), limits, |response| {
    (
      render(&response.data()),
      response
        .errors()
        .map(|error| {
          (
            error.path().to_string(),
            error
              .locations()
              .iter()
              .map(|span| (span.start(), span.end()))
              .collect::<Vec<_>>(),
          )
        })
        .collect::<Vec<_>>(),
    )
  });

  let (data, errors) = located;
  assert_eq!(
    errors.len(),
    1,
    "only `bad` is over the ceiling; a second error means its positions were still charged: \
     {errors:?}"
  );
  assert_eq!(errors[0].0, "bad");
  assert_eq!(errors[0].1.len(), 1, "the refusal points at one selection");
  let (start, end) = errors[0].1[0];
  assert_eq!(
    &query[start..end],
    "b",
    "and it is `b`, the child that did not fit"
  );
  assert_eq!(
    data, r#"{"bad":null,"other":{"ok":"OK"}}"#,
    "`other` fitted and must be in the response"
  );
}

// ------------------------------------------------------------------------------------------
// A collection-side refusal points where a commit-side one would
// ------------------------------------------------------------------------------------------
//
// Charging the staging buffer moved every metadata refusal from the commit to the collection, and
// the two paths had to stay observably identical for that to be a safe move. Eighty-nine cases
// passing was offered as the evidence and it was not evidence: **none of them has an alias as the
// crossing selection**, and an alias is the only input on which the two spans can differ at all.
// The commit path reports `field.span()`, which covers `p: boom`; the collection path was reporting
// the *name's* span, which covers only `boom`.
//
// So these two are shaped rather than sized. There is no quantity to tune — the crossing selection
// simply has to be aliased, and without that the case cannot fail no matter how wide it is.

/// A metadata refusal on an aliased selection names the alias, not the schema field behind it.
///
/// **Why two.** One selection costs two metadata entries, so a ceiling of two admits exactly one
/// and the second crosses. The second is written `q: b`, which is the whole point: the field it
/// resolves is `b`, and reporting `b` would send a client looking at a response key it never asked
/// for.
#[test]
fn a_metadata_refusal_on_an_alias_points_at_the_alias() {
  let query = "{ p: a q: b }";
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(2).expect("not zero"),
    ..Limits::default()
  };
  let located = execute_bounded(
    BUDGET_SDL,
    query,
    obj(vec![("a", J::Str("A".to_owned()))]),
    Vec::new(),
    limits,
    |response| {
      response
        .errors()
        .map(|error| {
          error
            .locations()
            .iter()
            .map(|span| (span.start(), span.end()))
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    },
  );

  assert_eq!(located.len(), 1, "one refusal at the root");
  assert_eq!(located[0].len(), 1);
  let (start, end) = located[0][0];
  assert_eq!(
    &query[start..end],
    "q: b",
    "the whole aliased selection, exactly as the commit path reports it — the field name alone \
     would be `b`, which is not what the client asked for"
  );
}

/// The interner refusal on a response key does the same, and for the same reason.
///
/// The other collection-side site the staging charge sits beside. It had the same span from an
/// earlier round, so it had the same defect and no case that could see it.
///
/// **Why one.** A one-byte arena admits no key at all, so the very first selection crosses — and it
/// is written `zz: a`, so the two spans differ.
#[test]
fn a_name_storage_refusal_on_an_alias_points_at_the_alias() {
  let query = "{ zz: a }";
  let limits = Limits {
    max_interned_bytes: NonZeroU32::new(1).expect("not zero"),
    ..Limits::default()
  };
  let located = execute_bounded(
    BUDGET_SDL,
    query,
    obj(vec![("a", J::Str("A".to_owned()))]),
    Vec::new(),
    limits,
    |response| {
      response
        .errors()
        .map(|error| {
          (
            error.kind(),
            error
              .locations()
              .iter()
              .map(|span| (span.start(), span.end()))
              .collect::<Vec<_>>(),
          )
        })
        .collect::<Vec<_>>()
    },
  );

  assert_eq!(located.len(), 1);
  assert_eq!(located[0].0, Kind::ResponseBudget);
  assert_eq!(located[0].1.len(), 1);
  let (start, end) = located[0].1[0];
  assert_eq!(
    &query[start..end],
    "zz: a",
    "the aliased selection whose key could not be stored, not the field behind it"
  );
}

// ------------------------------------------------------------------------------------------
// A refused list costs its siblings nothing either
// ------------------------------------------------------------------------------------------
//
// The list arm appends elements one at a time and a later one can be refused, so it is a branch
// that fails after allocating — the shape the expansion rollback was added for, in the one path
// that rollback never reached. Both cases below need the *element* slots released, and the second
// needs something the first cannot see.

const LIST_TXN_SDL: &str = r#"
type Query {
  bad: [String]
  other: Other
}
type Other {
  ok: String
}
"#;

fn list_txn_root(bad: J) -> J {
  J::Obj(
    "Query",
    vec![
      ("bad", bad),
      (
        "other",
        J::Obj("Other", vec![("ok", J::Str("OK".to_owned()))]),
      ),
    ],
  )
}

/// A list refused mid-way releases the elements it had already built.
///
/// **Why four.** The root, `bad` and `other` are three positions; `bad`'s first element is the
/// fourth and its second is refused. The correct degraded response is those three plus `other.ok`
/// — four — so it fits exactly, while a retained element leaves the count at four before `other` is
/// reached and `other.ok` would need five. Any other ceiling admits both or refuses both.
#[test]
fn a_refused_list_costs_its_siblings_nothing() {
  let limits = Limits {
    max_response_slots: NonZeroU32::new(4).expect("not zero"),
    ..Limits::default()
  };
  let (data, errors) = run_bounded(
    LIST_TXN_SDL,
    "{ bad other { ok } }",
    list_txn_root(J::List(vec![
      J::Str("x".to_owned()),
      J::Str("y".to_owned()),
    ])),
    limits,
  );

  assert_eq!(
    errors.len(),
    1,
    "only `bad` is over the ceiling; a second error means its elements were still charged: \
     {errors:?}"
  );
  assert_eq!(errors[0].2, "bad");
  assert_eq!(
    data, r#"{"bad":null,"other":{"ok":"OK"}}"#,
    "`other` fitted in the correct degraded response and must be in it"
  );
}

/// The same refusal over a list whose earlier element had already failed.
///
/// The inverse of the rollback, and the half the case above cannot reach. A nullable element type
/// means an element's own field error does *not* null the list, so the loop continues carrying an
/// error row that names that element's slot. Restoring the slots without the row does not crash,
/// which is what makes it dangerous: the next sibling to expand takes the index the element gave
/// up, so the stale row re-points at a real position that is not its own. Planted, the leaf error
/// from `bad`'s element comes back with the path `other.ok` — releasing something that is still
/// read, reported against a field that never failed. That is the failure a release-only test never
/// sees, and the mark covers the error rows for exactly this reason.
#[test]
fn a_refused_list_takes_its_elements_errors_with_it() {
  let limits = Limits {
    max_response_slots: NonZeroU32::new(4).expect("not zero"),
    ..Limits::default()
  };
  // The first element serialises to null, which draft §6.4.3 step 4 makes a field error at
  // `bad.0`; `[String]` is nullable per element, so the list survives it and reaches the refusal.
  let (data, errors) = run_bounded(
    LIST_TXN_SDL,
    "{ bad other { ok } }",
    list_txn_root(J::List(vec![J::Vanishes, J::Str("y".to_owned())])),
    limits,
  );

  assert_eq!(
    errors.len(),
    1,
    "the element's error went back with the element that no longer exists: {errors:?}"
  );
  assert_eq!(errors[0].0, Kind::ResponseBudget);
  assert_eq!(
    errors[0].2, "bad",
    "and the surviving error is the refusal, at the list"
  );
  assert_eq!(
    data, r#"{"bad":null,"other":{"ok":"OK"}}"#,
    "`other` still fits, and reading the response did not walk a slot that was truncated away"
  );
}

// ------------------------------------------------------------------------------------------
// An answer retires the request, even when the request was already abandoned
// ------------------------------------------------------------------------------------------

/// Answering an abandoned request frees its in-flight entry instead of holding the ceiling.
///
/// `poll_resolve` gates on `live + abandoned`, and an answer for a slot draft §6.4.4 had already
/// discarded used to return without freeing the entry or decrementing the count. The work was
/// finished and the ceiling still believed it was outstanding, so a driver that had answered
/// everything it was asked could still be withheld from — until it polled an abandonment for work
/// it had already done.
///
/// **Why two.** With an in-flight ceiling of two, `echo` abandoned plus one live cell request is
/// exactly the ceiling, so the second cell is withheld. Answering `echo` is the only thing that can
/// free a slot, and a ceiling of three would have admitted the second cell without it.
#[test]
fn answering_an_abandoned_request_frees_its_in_flight_entry() {
  let query = r#"{ nest { deep { boom echo(text: "e") } bulk { text } } }"#;
  let (schema, document) = compile(HOLD_SDL, query);
  let limits = Limits {
    max_in_flight: NonZeroU32::new(2).expect("not zero"),
    ..Limits::default()
  };
  let mut space = Space::default();
  let mut executor = Executor::with_limits(&schema, &document, limits);
  executor
    .start(&mut space, None, obj(vec![]))
    .expect("the operation resolves");

  let nest = executor.poll_resolve(&mut space).expect("nest").id();
  executor.handle_resolved(&mut space, nest, J::Obj("Wrap", vec![]));
  let deep = executor.poll_resolve(&mut space).expect("deep").id();
  executor.handle_resolved(&mut space, deep, J::Obj("Wrap", vec![]));
  let bulk = executor.poll_resolve(&mut space).expect("bulk").id();
  executor.handle_resolved(
    &mut space,
    bulk,
    J::List(vec![
      J::Obj("Cell", vec![("text", J::Str("A".to_owned()))]),
      J::Obj("Cell", vec![("text", J::Str("B".to_owned()))]),
    ]),
  );

  let boom = executor.poll_resolve(&mut space).expect("boom").id();
  let echo = executor.poll_resolve(&mut space).expect("echo").id();
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "two in flight is the ceiling"
  );

  // `boom` is `String!` on a nullable `deep`, so §6.4.4 nulls `deep` and the still-outstanding
  // `echo` underneath it becomes abandoned rather than live.
  executor.handle_field_error(boom, "boom");
  let first_cell = executor
    .poll_resolve(&mut space)
    .expect("a cell's `text`, now that `boom` has been answered")
    .id();
  assert!(
    executor.poll_resolve(&mut space).is_none(),
    "one abandoned plus one live is the ceiling again"
  );

  // The driver answers the request it was never told to stop working on.
  executor.handle_resolved(&mut space, echo, J::Str("late".to_owned()));
  let second_cell = executor.poll_resolve(&mut space).expect(
    "the abandoned request was answered, so its entry retires and the ceiling has room — without \
     that it stays charged until the driver polls an abandonment for finished work",
  );
  assert_ne!(second_cell.id(), first_cell);
}

/// A merged response key refused during collection points at the group's first selection.
///
/// The commit path reports the group's first selection and collection crosses on a later duplicate,
/// so `{ nullable nullable }` reported the second where the commit would report the first. The two
/// no longer compute a span each: the group stores the span of the selection that created it and
/// both read that field, so there is one value rather than two that have to agree.
///
/// **Why two.** One selection costs two metadata entries, so a ceiling of two admits the first
/// duplicate and the second crosses — which is the only arrangement in which the two spans could
/// differ at all.
#[test]
fn a_merged_key_refused_during_collection_points_at_the_first_selection() {
  let query = "{ nullable nullable }";
  let limits = Limits {
    max_response_metadata: NonZeroU32::new(2).expect("not zero"),
    ..Limits::default()
  };
  let located = execute_bounded(
    BUDGET_SDL,
    query,
    obj(vec![]),
    Vec::new(),
    limits,
    |response| {
      response
        .errors()
        .map(|error| {
          error
            .locations()
            .iter()
            .map(|span| (span.start(), span.end()))
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    },
  );

  assert_eq!(located.len(), 1);
  assert_eq!(located[0].len(), 1);
  let first = query.find("nullable").expect("the first duplicate");
  assert_eq!(
    located[0][0],
    (first, first + "nullable".len()),
    "the group's first selection at byte {first}, not the duplicate that happened to cross"
  );
}
