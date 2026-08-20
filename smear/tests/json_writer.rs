#![cfg(all(feature = "proto", feature = "materialized-numbers"))]

//! The draft §7.2.1 writer's gate, and the whole of its design is *where the expectation comes
//! from*.
//!
//! # The trap this file is built to avoid
//!
//! A writer checked against expectations produced by the same writer proves nothing: the two sides
//! agree by construction and the test passes over any consistent mistake. So nothing here spells
//! an expectation by writing it out with `smear::json`. Three oracles carry the load, and none of
//! them has seen a line of the writer:
//!
//! | oracle | what it settles |
//! |---|---|
//! | `serde_json` | the output is JSON at all, and *means* what a hand-built value means |
//! | `apollo-parser` | draft §2.9.4 block-string cooking, from a third-party GraphQL implementation |
//! | fixed byte strings | the choices where the *formatting* is the contract — entry order, and the shape of an `errors` entry |
//!
//! The first is the round trip and it compares **values, not bytes**, so key order and spacing stay
//! free while semantics are pinned. The third is where they are deliberately not free.
//!
//! `serde_json` is a dev-dependency and not a dependency, which is the distinction the serde
//! removal turns on: the edge this workspace is removing is `smear-schema`'s `introspection`
//! naming it in `[dependencies]`, where it reaches a consumer's graph. Cargo drops a
//! dev-dependency when packaging, so `cargo tree -p smear` — the thing that will check the removal
//! claim — never sees this one.
//!
//! # The driver is the materialised value tree
//!
//! `Space` below implements `proto::Values` over
//! `parser::graphql::ast::materialized::ConstInputValue`, which is the arrangement the crate-split
//! design predicted the materialised AST would unlock, and it is what makes this a gate on the
//! writer rather than on a mock: every value in these responses came out of the real parser.

use smear::{
  json,
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument, materialized},
    error::GraphqlErrors,
    syntactic::{
      GraphqlLexer, executable_document, type_system_document, value::materialized::const_value,
    },
  },
  proto::{Executor, Extensions, Leaf, RequestErrorResult, Response, StartError, Values},
  validator::Schema,
};

/// The width every fixture in this file is built at.
///
/// `i64` and not `i32`, because it is the width whose `Int` leaf can reach
/// [`json::Json::int_leaf`]'s string branch — the writer behaviour this file exists to pin. The
/// other width is exercised where its own claim lives, in
/// `the_i32_width_cannot_produce_an_out_of_range_int` below, which asserts the parser refusal that
/// makes that branch unreachable there.
type ConstInputValue<S> = materialized::ConstInputValue<S, i64>;

// ------------------------------------------------------------------------------------------
// a driver whose value space is the materialised value tree
// ------------------------------------------------------------------------------------------

/// One value, parsed from the GraphQL literal that spells it.
///
/// Every value in this file arrives this way, so the tree under test is the one the parser builds
/// and not one assembled by hand out of the same enum.
fn value(literal: &'static str) -> ConstInputValue<&'static str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ConstInputValue<&'static str>,
    GraphqlErrors<&'static str>,
    _,
    GraphQL,
  >(const_value::<_, _, i64>)
  .parse_str(literal)
  .expect("the fixture is a constant GraphQL value")
}

/// A driver whose representation is the materialised value tree.
struct Space;

impl Values for Space {
  type Value = ConstInputValue<&'static str>;

  fn is_null(&self, value: &Self::Value) -> bool {
    matches!(value, ConstInputValue::Null(_))
  }

  fn as_bool(&self, value: &Self::Value) -> Option<bool> {
    match value {
      ConstInputValue::Boolean(flag) => Some(flag.value()),
      _ => None,
    }
  }

  fn list_len(&self, value: &Self::Value) -> Option<usize> {
    match value {
      ConstInputValue::List(list) => Some(list.values().len()),
      _ => None,
    }
  }

  fn list_item(&mut self, value: &Self::Value, index: usize) -> Self::Value {
    match value {
      ConstInputValue::List(list) => list.values()[index].clone(),
      _ => value_null(),
    }
  }

  /// Never asked: no type in the fixtures below is abstract, so draft §6.4.3's
  /// `ResolveAbstractType` is never reached.
  fn type_name<'a>(&'a self, _value: &'a Self::Value) -> Option<&'a str> {
    None
  }

  /// The tree is already the serialised form, so `CoerceResult` is the identity.
  fn coerce_leaf(&mut self, value: Self::Value, _leaf: Leaf<'_>) -> Option<Self::Value> {
    Some(value)
  }

  fn variable(&mut self, _name: &str) -> Option<Self::Value> {
    None
  }
}

/// The `null` literal, parsed rather than constructed — `NullValue`'s constructor is the parser's.
fn value_null() -> ConstInputValue<&'static str> {
  value("null")
}

/// Looks a response key up in an object value.
fn field<'a>(
  parent: &'a ConstInputValue<&'static str>,
  name: &str,
) -> Option<&'a ConstInputValue<&'static str>> {
  match parent {
    ConstInputValue::Object(object) => object
      .fields()
      .iter()
      .find(|field| *field.name().source() == name)
      .map(|field| field.value()),
    _ => None,
  }
}

/// Runs one operation to completion and hands the finished response to `take`.
fn execute<T>(
  sdl: &'static str,
  query: &'static str,
  root: ConstInputValue<&'static str>,
  extensions: &[(&str, &'static str)],
  take: impl FnOnce(&Response<'_, ConstInputValue<&'static str>>) -> T,
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

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, root)
    .expect("the operation resolves");

  if !extensions.is_empty() {
    let mut map = Extensions::new(executor.limits());
    for (key, literal) in extensions {
      map.insert(key, value(literal)).expect("under the ceilings");
    }
    executor
      .set_extensions(map)
      .expect("an operation is running");
  }

  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = field(request.parent_value(), request.name())
      .cloned()
      .unwrap_or_else(value_null);
    executor.handle_resolved(&mut space, id, answer);
    while executor.poll_abandoned().is_some() {}
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  take(&response)
}

/// Runs one operation and returns the writer's bytes.
fn render(
  sdl: &'static str,
  query: &'static str,
  root: ConstInputValue<&'static str>,
  extensions: &[(&str, &'static str)],
) -> String {
  execute(sdl, query, root, extensions, |response| {
    let mut out = String::new();
    json::write_response(&mut out, response, query).expect("the response is writable");
    out
  })
}

// ------------------------------------------------------------------------------------------
// the fixture
// ------------------------------------------------------------------------------------------

const SDL: &str = "
type Query { hero: Character }
type Character {
  name: String!
  height: Float
  id: ID!
  favourite: Episode
  big: Int
  small: Int
  note: String
  friends: [Friend!]
}
type Friend { name: String! }
enum Episode { NEWHOPE EMPIRE JEDI }
";

const QUERY: &str = "{
  hero {
    __typename
    name
    height
    id
    favourite
    big
    small
    note
    friends { name }
  }
}";

/// One `Friend` has a null `name`, which is `Friend.name: String!` — draft §6.4.3 step 1's field
/// error. It nulls the element, and `[Friend!]` propagates that to the whole list, so the response
/// carries a null `friends`, an `errors` entry, and a response path with an index in it.
fn root() -> ConstInputValue<&'static str> {
  value(
    r#"{
      hero: {
        name: "Luke \"Sky\"walker\n",
        height: 1.72,
        id: "1000",
        favourite: JEDI,
        big: 2147483648,
        small: -7,
        note: """
            A block
              string
        """,
        friends: [{ name: "Han" }, { name: null }]
      }
    }"#,
  )
}

// ------------------------------------------------------------------------------------------
// oracle 1 — the round trip, read back by a parser that has never seen this writer
// ------------------------------------------------------------------------------------------

/// The gate. `serde_json` parses what the writer emitted, and the result is compared against a
/// value built by hand — so the expectation is a statement about the *response*, not a second
/// rendering of it.
#[test]
fn the_round_trip_agrees_with_a_hand_built_value() {
  let written = render(SDL, QUERY, root(), &[("traceId", r#""9f2c""#)]);
  let parsed: serde_json::Value = serde_json::from_str(&written).unwrap_or_else(|error| {
    panic!("the writer emitted something that is not JSON: {error}\n{written}")
  });

  let expected = serde_json::json!({
    "errors": [{
      "message": "Cannot return null for non-nullable field Friend.name.",
      "locations": [{ "line": 11, "column": 15 }],
      "path": ["hero", "friends", 1, "name"],
    }],
    "data": {
      "hero": {
        "__typename": "Character",
        "name": "Luke \"Sky\"walker\n",
        "height": 1.72,
        "id": "1000",
        "favourite": "JEDI",
        // Draft §3.5.1's `Int` is 32 bits, so this one is beyond it and is a JSON STRING.
        "big": "2147483648",
        "small": -7,
        "note": "A block\n  string",
        "friends": serde_json::Value::Null,
      }
    },
    "extensions": { "traceId": "9f2c" },
  });

  assert_eq!(parsed, expected, "written: {written}");
}

/// The same round trip over a response with no errors and no extensions, which is a different map:
/// §7.1 forbids an empty `errors` entry and `None` extensions is an absent key rather than `{}`.
#[test]
fn the_absent_entries_are_absent_rather_than_empty() {
  const SMALL: &str = "{ hero { name } }";
  let written = render(SDL, SMALL, root(), &[]);
  let parsed: serde_json::Value = serde_json::from_str(&written).expect("valid JSON");

  assert_eq!(
    parsed,
    serde_json::json!({ "data": { "hero": { "name": "Luke \"Sky\"walker\n" } } })
  );
  assert!(!written.contains("errors"), "{written}");
  assert!(!written.contains("extensions"), "{written}");
}

/// A present but empty `extensions` map is the other side of that distinction, and it survives the
/// round trip as `{}` rather than disappearing.
#[test]
fn a_present_empty_extensions_map_is_written() {
  const SMALL: &str = "{ hero { name } }";

  // The same operation with no map attached writes no entry, which is what makes the assertion
  // below about presence rather than about emptiness.
  assert!(!render(SDL, SMALL, root(), &[]).contains("extensions"));

  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(SMALL)
  .expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, root())
    .expect("the operation resolves");
  executor
    .set_extensions(Extensions::new(executor.limits()))
    .expect("an operation is running");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = field(request.parent_value(), request.name())
      .cloned()
      .unwrap_or_else(value_null);
    executor.handle_resolved(&mut space, id, answer);
  }
  let response = executor.poll_response().expect("nothing is outstanding");
  let mut out = String::new();
  json::write_response(&mut out, &response, SMALL).expect("writable");

  let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");
  assert_eq!(
    parsed,
    serde_json::json!({ "data": { "hero": { "name": "Luke \"Sky\"walker\n" } } , "extensions": {} })
  );
}

/// Draft §7.1.3's other result kind, which has no `data` entry at all.
#[test]
fn a_request_error_result_has_no_data_entry() {
  const TWO: &str = "query A { hero { name } } query B { hero { name } }";

  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(TWO)
  .expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  let refusal = executor
    .start(&mut space, None, root())
    .expect_err("two operations and no name is draft §6.1's ambiguity");
  assert!(matches!(refusal, StartError::AmbiguousOperation));

  let mut result: RequestErrorResult<ConstInputValue<&'static str>> =
    RequestErrorResult::new(executor.limits(), refusal);
  let mut map = Extensions::new(executor.limits());
  map
    .insert("traceId", value(r#""9f2c""#))
    .expect("under the ceilings");
  result.set_extensions(map).expect("under the ceilings");

  let mut out = String::new();
  json::write_request_error_result(&mut out, &result).expect("writable");

  let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");
  assert_eq!(
    parsed,
    serde_json::json!({
      "errors": [{ "message": refusal.to_string() }],
      "extensions": { "traceId": "9f2c" },
    })
  );
  assert!(!out.contains("\"data\""), "{out}");
}

/// Every double the writer can be handed comes back bit-for-bit through an independent reader.
///
/// `zmij` is a shortest-round-trip algorithm, and this is what that claim means from the outside:
/// the bytes it produced, parsed by `serde_json`, are the same `f64`. Nothing here checks the
/// spelling — a shorter or longer correct rendering would still pass — which is the property being
/// asserted and not an accident of the assertion.
#[test]
fn every_double_survives_the_round_trip_bit_for_bit() {
  const DOUBLES: &[f64] = &[
    0.0,
    -0.0,
    1.0,
    -1.0,
    0.1,
    1.0 / 3.0,
    1.7976931348623157e308,
    2.2250738585072014e-308,
    5e-324,
    1e300,
    -1.5e-7,
    123456789.12345679,
  ];

  for &double in DOUBLES {
    let mut out = String::new();
    let mut writer = json::Json::new(&mut out);
    writer.double(double).expect("finite");
    let parsed: f64 = serde_json::from_str(&out)
      .unwrap_or_else(|error| panic!("`{out}` is not a JSON number: {error}"));
    assert_eq!(
      parsed.to_bits(),
      double.to_bits(),
      "`{out}` read back as a different double: {:016x} vs {:016x}",
      parsed.to_bits(),
      double.to_bits()
    );
  }
}

/// The escaping map, checked by reading it back rather than by comparing it to itself.
///
/// The sweep is what makes this a gate on the *class* and not on a list of characters somebody
/// thought of: every scalar value below U+0500 plus a sample above it, written and then parsed,
/// must come back as itself.
#[test]
fn every_string_survives_the_round_trip() {
  let scalars = (0u32..0x500)
    .chain([
      0x2028, 0x2029, 0xfffd, 0xffff, 0x1_0000, 0x1_f600, 0x10_fffe, 0x10_ffff,
    ])
    .filter_map(char::from_u32);

  for ch in scalars {
    let subject = format!("a{ch}b\"c\\d");
    let mut out = String::new();
    let mut writer = json::Json::new(&mut out);
    writer.string(&subject).expect("writable");
    let parsed: String = serde_json::from_str(&out).unwrap_or_else(|error| {
      panic!(
        "U+{:04X} produced `{out}`, which is not a JSON string: {error}",
        ch as u32
      )
    });
    assert_eq!(parsed, subject, "U+{:04X} did not survive", ch as u32);
  }
}

// ------------------------------------------------------------------------------------------
// oracle 2 — a third-party GraphQL implementation, for draft §2.9.4
// ------------------------------------------------------------------------------------------

/// Block-string cooking, against `apollo-parser`.
///
/// This one needs a *GraphQL* oracle rather than a JSON one, and it must not be `smear-lexer`'s:
/// that implementation's own dedent disagrees with draft §2.9.4 on the specification's worked
/// example, which the unit tests record. `apollo-parser` is a dev-dependency of this package
/// already, is a different organisation's implementation, and cooks a `StringValue` into a
/// `String` through its own code.
#[test]
fn block_string_cooking_agrees_with_apollo_parser() {
  const CORPUS: &[&str] = &[
    "\"\"\"\"\"\"",
    "\"\"\"plain\"\"\"",
    "\"\"\"\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n    \"\"\"",
    "\"\"\"\n\t\ta\n\t\t\tb\n\"\"\"",
    "\"\"\"\n   \n  \n\"\"\"",
    "\"\"\"a\n  b\n    c\n\"\"\"",
    "\"\"\"  keeps first line indent\n  and dedents this\n\"\"\"",
    "\"\"\"a\\\"\"\"b\"\"\"",
    "\"\"\"a\\nb\"\"\"",
  ];

  for literal in CORPUS {
    let query = format!("{{ f(a: {literal}) }}");
    let cst = apollo_parser::Parser::new(&query).parse();
    assert_eq!(cst.errors().len(), 0, "apollo-parser rejected `{query}`");

    let theirs = apollo_string(&cst).unwrap_or_else(|| panic!("no string value in `{query}`"));

    let mut out = String::new();
    let mut writer = json::Json::new(&mut out);
    writer.graphql_string(literal).expect("writable");
    let ours: String = serde_json::from_str(&out).expect("a JSON string");

    assert_eq!(ours, theirs, "disagreed with apollo-parser on `{literal}`");
  }
}

/// Digs the one argument's cooked string value out of an `apollo-parser` tree.
fn apollo_string(cst: &apollo_parser::SyntaxTree<apollo_parser::cst::Document>) -> Option<String> {
  use apollo_parser::cst::{CstNode as _, Value};

  cst
    .document()
    .syntax()
    .descendants()
    .find_map(Value::cast)
    .and_then(|value| match value {
      Value::StringValue(string) => Some(String::from(string)),
      _ => None,
    })
}

// ------------------------------------------------------------------------------------------
// oracle 3 — the bytes, where the formatting IS the contract
// ------------------------------------------------------------------------------------------

/// Entry order and the shape of an `errors` entry, pinned exactly.
///
/// A value comparison cannot see either: `{"data":…,"errors":…}` and `{"errors":…,"data":…}` parse
/// to the same map, and §7.1 asks for the `errors` entry to come first. So this is the one gate
/// that reads bytes, and it is deliberately narrow — it covers the fixed-shape envelope, not the
/// leaves, which the round trip above owns.
#[test]
fn the_envelope_is_pinned_to_its_bytes() {
  const SMALL: &str = "{ hero { friends { name } } }";
  let written = render(SDL, SMALL, root(), &[("traceId", r#""9f2c""#)]);

  assert_eq!(
    written,
    concat!(
      r#"{"errors":[{"message":"Cannot return null for non-nullable field Friend.name.","#,
      r#""locations":[{"line":1,"column":20}],"path":["hero","friends",1,"name"]}],"#,
      r#""data":{"hero":{"friends":null}},"#,
      r#""extensions":{"traceId":"9f2c"}}"#,
    )
  );
}

/// `path` is absent — not empty — where draft §7.1.2 says the error cannot be associated with a
/// field, and a `@skip` condition over the root selection set is that place.
#[test]
fn a_rootless_error_carries_no_path_entry() {
  const ROOTLESS: &str = "query ($flag: Boolean!) { hero @skip(if: $flag) { name } }";
  let written = render(SDL, ROOTLESS, root(), &[]);
  let parsed: serde_json::Value = serde_json::from_str(&written).expect("valid JSON");

  let errors = parsed
    .get("errors")
    .and_then(serde_json::Value::as_array)
    .expect("a directive condition that cannot be read is a field error");
  assert_eq!(errors.len(), 1, "{written}");
  assert!(
    errors[0].get("path").is_none(),
    "an error with no position must carry no `path` entry: {written}"
  );
  assert!(errors[0].get("locations").is_some(), "{written}");
}

// ------------------------------------------------------------------------------------------
// the decided rules, each with the case that would notice if it changed
// ------------------------------------------------------------------------------------------

/// The out-of-range rule, end to end: a value beyond draft §3.5.1's `Int` is a JSON **string** in
/// the response, and one inside it is a JSON number.
///
/// Read back through `serde_json` so the assertion is about the JSON *type* rather than about the
/// bytes the writer chose — `is_string` and `is_number` are the reader's classification, not the
/// writer's.
#[test]
fn an_out_of_range_int_is_a_string_in_the_response() {
  const INTS: &str = "{ hero { big small } }";
  let written = render(SDL, INTS, root(), &[]);
  let parsed: serde_json::Value = serde_json::from_str(&written).expect("valid JSON");
  let hero = &parsed["data"]["hero"];

  assert_eq!(hero["big"], serde_json::json!("2147483648"), "{written}");
  assert!(hero["big"].is_string(), "{written}");

  assert_eq!(hero["small"], serde_json::json!(-7), "{written}");
  assert!(hero["small"].is_number(), "{written}");
}

/// The same rule at the boundary, on the writer's own door rather than through a response, so both
/// sides of the edge are pinned rather than only one value on each side.
#[test]
fn the_int_boundary_is_draft_3_5_1s() {
  for (input, expected) in [
    (i64::from(i32::MAX), serde_json::json!(2147483647i64)),
    (i64::from(i32::MIN), serde_json::json!(-2147483648i64)),
    (i64::from(i32::MAX) + 1, serde_json::json!("2147483648")),
    (i64::from(i32::MIN) - 1, serde_json::json!("-2147483649")),
  ] {
    let mut out = String::new();
    let mut writer = json::Json::new(&mut out);
    writer.int_leaf(input).expect("writable");
    let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");
    assert_eq!(parsed, expected, "{input} was written as `{out}`");
  }
}

/// The `i32` width cannot reach the string branch, because its parser refused the literal first.
///
/// This is the two materialised widths meeting the writer: the same rule, one width for which it
/// never fires. The refusal is the parser's and is asserted here rather than assumed, because the
/// claim "unreachable" is about that refusal and not about the writer.
#[test]
fn the_i32_width_cannot_produce_an_out_of_range_int() {
  type ConstInputValue32<S> = materialized::ConstInputValue<S, i32>;

  let parse = |literal: &'static str| {
    Parser::with_parser::<
      GraphqlLexer<'_, str>,
      ConstInputValue32<&'static str>,
      GraphqlErrors<&'static str>,
      _,
      GraphQL,
    >(const_value::<_, _, i32>)
    .parse_str(literal)
  };

  let inside = parse("2147483647").expect("inside draft §3.5.1's Int");
  let mut out = String::new();
  let mut writer = json::Json::new(&mut out);
  json::WriteJson::write_json(&inside, &mut writer).expect("writable");
  assert_eq!(out, "2147483647");

  assert!(
    parse("2147483648").is_err(),
    "the i32 width must refuse a literal past draft §3.5.1's Int, which is what makes the \
     writer's string branch unreachable for it"
  );
}

// ------------------------------------------------------------------------------------------
// the value walk: what it writes, and the three doors it is reached through
// ------------------------------------------------------------------------------------------
//
// The unit gates in `smear::json::tests` measure what the value walk COSTS — its native stack, its
// frames, and what it does when the allocator says no. Two things they cannot check are here
// instead: that the walk still writes the right bytes, container by container, and that each of
// this crate's three public doors reaches it. A response leaf, a §7.1.7 `extensions` entry on an
// execution result, and a §7.1.3 `extensions` entry on a request error result that never executed
// anything — three routes, one walk, and before the repair all three aborted the process after
// 3 001 bytes of the response had already gone to the sink.

/// A composite value survives the round trip through an `extensions` entry, container by
/// container.
///
/// # The branch nothing else in this file reaches
///
/// A container only arrives at `WriteJson` when it is the whole of a **leaf's** value or of an
/// `extensions` entry: everywhere else the executor has already taken a list or an object apart
/// into response positions, and what reaches the writer is a scalar. So the value walk's `List` and
/// `Object` arms — its separators, its keys, and its two empty-container paths — are not covered by
/// a single one of the response fixtures above, and were not covered before this gate existed
/// either. That is uncomfortable to write down and is exactly why it is written down: the walk was
/// rewritten in this change, and a rewrite of an uncovered branch is a rewrite nothing checks.
///
/// Read back with `serde_json` and compared against a hand-built value: this file's first oracle,
/// on the one part of the writer it had never been pointed at.
#[test]
fn a_composite_value_survives_the_round_trip_through_an_extension() {
  const ONE: &str = "{ hero { name } }";
  const COMPOSITE: &str = r#"{
    empty_object: {},
    empty_list: [],
    scalars: [1, 2.5, true, null, "s", JEDI, 2147483648],
    nested: { a: [ { b: [ { c: "deep" } ] } ] },
    escaped: [ "cooked\nand\u0000escaped", [], {} ]
  }"#;

  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(ONE)
  .expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, root())
    .expect("the operation resolves");
  let mut map = Extensions::new(executor.limits());
  map
    .insert("composite", value(COMPOSITE))
    .expect("under the ceilings");
  executor
    .set_extensions(map)
    .expect("an operation is running");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = field(request.parent_value(), request.name())
      .cloned()
      .unwrap_or_else(value_null);
    executor.handle_resolved(&mut space, id, answer);
    while executor.poll_abandoned().is_some() {}
  }
  let response = executor.poll_response().expect("nothing is outstanding");

  let mut out = String::new();
  json::write_response(&mut out, &response, ONE).expect("the response is writable");
  let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");

  assert_eq!(
    parsed["extensions"]["composite"],
    serde_json::json!({
      "empty_object": {},
      "empty_list": [],
      // `2147483648` is past draft §3.5.1's `Int`, so it is a JSON string — the same rule the
      // response's own leaves follow, met here inside a container.
      "scalars": [1, 2.5, true, null, "s", "JEDI", "2147483648"],
      "nested": { "a": [ { "b": [ { "c": "deep" } ] } ] },
      // A GraphQL string leaf INSIDE a container takes the same cooking-then-escaping path a
      // response leaf does, and the two empty containers beside it are the walk's two immediate
      // close paths.
      "escaped": ["cooked\nand\u{0}escaped", [], {}],
    }),
    "{out}"
  );
}

/// One value `depth` lists deep, built out of nothing but this crate's public API.
///
/// **Built by wrapping and not by parsing**, which is the point rather than a shortcut: the parser
/// is a recursive descent with a nesting ceiling, so this shape cannot come out of it. A driver's
/// value does not come out of it either — `ConstList::new` is public, `ConstInputValue::List` is a
/// public variant, and what a resolver returns is whatever the driver built.
fn nested(depth: usize) -> ConstInputValue<&'static str> {
  let mut value = value_null();
  let span = *smear::__private::tokora::span::AsSpan::as_span(&value);
  for _ in 0..depth {
    value = ConstInputValue::List(materialized::ConstList::<&'static str, i64>::new(
      span,
      vec![value],
    ));
  }
  value
}

/// How deep the fixtures below are.
///
/// Five times the depth at which the recursive walk aborted on the harness's own test-thread stack.
const NESTED: usize = 20_000;

/// What one of these values writes as: an opening bracket per level, a `null`, and the closing
/// brackets.
fn nested_json() -> String {
  let mut expected = String::with_capacity(2 * NESTED + 4);
  expected.extend(core::iter::repeat_n('[', NESTED));
  expected.push_str("null");
  expected.extend(core::iter::repeat_n(']', NESTED));
  expected
}

/// A deeply nested value in a response **leaf** is written rather than fatal.
///
/// `note: String` is a leaf, so draft §6.4.3 hands the driver's value to `CoerceResult` and stores
/// whatever comes back — this driver's coercion is the identity, which is the arrangement any
/// service with a composite custom scalar has. The response is one position deep whatever the value
/// is, which is the whole finding: `Response::depth` counts positions, and a value is one.
#[test]
fn a_deeply_nested_leaf_is_written() {
  const ONE: &str = "{ hero { note } }";

  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(ONE)
  .expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, root())
    .expect("the operation resolves");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = if request.name() == "note" {
      nested(NESTED)
    } else {
      field(request.parent_value(), request.name())
        .cloned()
        .unwrap_or_else(value_null)
    };
    executor.handle_resolved(&mut space, id, answer);
    while executor.poll_abandoned().is_some() {}
  }
  let response = executor.poll_response().expect("nothing is outstanding");

  let mut out = String::new();
  json::write_response(&mut out, &response, ONE).expect("the response is writable");
  assert_eq!(
    out,
    format!(r#"{{"data":{{"hero":{{"note":{}}}}}}}"#, nested_json())
  );

  // `response` borrows the executor and has no `Drop`, so its borrow ends at its last use above
  // and the executor can be moved out from under it — including into its own release, which is
  // where this fixture used to kill the process.
  release_rather_than_leak(executor);
}

/// The same value as a §7.1.7 `extensions` entry on an execution result.
#[test]
fn a_deeply_nested_extension_on_an_execution_result_is_written() {
  const ONE: &str = "{ hero { name } }";

  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(ONE)
  .expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, root())
    .expect("the operation resolves");
  let mut map = Extensions::new(executor.limits());
  map
    .insert("trace", nested(NESTED))
    .expect("under the ceilings");
  executor
    .set_extensions(map)
    .expect("an operation is running");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let answer = field(request.parent_value(), request.name())
      .cloned()
      .unwrap_or_else(value_null);
    executor.handle_resolved(&mut space, id, answer);
    while executor.poll_abandoned().is_some() {}
  }
  let response = executor.poll_response().expect("nothing is outstanding");

  let mut out = String::new();
  json::write_response(&mut out, &response, ONE).expect("the response is writable");
  assert!(
    out.ends_with(&format!(r#","extensions":{{"trace":{}}}}}"#, nested_json())),
    "the deep extension is not the last entry of the response"
  );

  // `response` borrows the executor and has no `Drop`, so its borrow ends at its last use above
  // and the executor can be moved out from under it — including into its own release, which is
  // where this fixture used to kill the process.
  release_rather_than_leak(executor);
}

/// And as a §7.1.3 `extensions` entry on a request error result, where nothing executed at all.
///
/// The sharpest of the three: there is no `data`, no response tree and no executor state — the only
/// thing in the map is the driver's value, so `Response::depth` does not merely fail to bound it,
/// there is no response to have a depth.
#[test]
fn a_deeply_nested_extension_on_a_request_error_result_is_written() {
  const TWO: &str = "query A { hero { name } } query B { hero { name } }";

  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(TWO)
  .expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  let refusal = executor
    .start(&mut space, None, root())
    .expect_err("two operations and no name is draft §6.1's ambiguity");
  let mut result: RequestErrorResult<ConstInputValue<&'static str>> =
    RequestErrorResult::new(executor.limits(), refusal);
  let mut map = Extensions::new(executor.limits());
  map
    .insert("trace", nested(NESTED))
    .expect("under the ceilings");
  result.set_extensions(map).expect("under the ceilings");

  let mut out = String::new();
  json::write_request_error_result(&mut out, &result).expect("the result is writable");
  assert!(
    out.ends_with(&format!(r#","extensions":{{"trace":{}}}}}"#, nested_json())),
    "the deep extension is not the last entry of the result"
  );

  release_rather_than_leak(result);
}

/// Releases whatever still owns one of these fixtures, which is the second half of each gate.
///
/// # It used to `mem::forget`, and that is the finding
///
/// `ConstInputValue`'s **derived** `Drop` glue recursed: releasing a value this deep aborted the
/// process with `SIGABRT` whether or not anything ever wrote it out — measured at a depth of
/// 7 773, with nothing from `smear::json` on the stack. So the three gates above established that
/// *writing* one of these no longer spends a native frame per level, and could not establish that
/// a deeply nested value was safe to hold. A test that let one fall out of scope would have died
/// on a defect it was not testing, at a signal no assertion can soften, so each one leaked its
/// fixture instead.
///
/// That defect is repaired (al8n/smear#165): the tree carries a hand-written `Drop` that moves its
/// children onto a heap worklist. **Each of the three gates above now releases a 20 000-deep value
/// as its last act**, so between them they pin both halves — the writer's bound and the release's
/// — and a regression in either takes the whole file down loudly rather than passing quietly.
fn release_rather_than_leak<T>(owner: T) {
  drop(owner);
}
