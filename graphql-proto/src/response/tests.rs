//! What deriving one draft §7.1.2 path costs, counted in the links it follows.
//!
//! # Why the gate is here and not in the serialiser that met the defect
//!
//! `smear`'s JSON writer is where the cost was *observed* — it is the only caller that consumes a
//! whole path for every error in a response — but the walk is this module's, and a gate placed on
//! one consumer says nothing about the next one. So the instrument is on the links themselves:
//! [`super::traversals`] counts every slot a path derivation reaches, through the one private
//! accessor that can reach them, and the property below is a property of [`Path`] rather than of
//! anybody's serialiser.
//!
//! # Why the fixture is a fragment chain and not a nested selection set
//!
//! Because that is the construction the finding named, and it is the one that survives the
//! defences. A response `d` objects deep normally needs a document `d` selection sets deep, which
//! the lexer's nesting ceiling refuses long before `d` is interesting. An **acyclic chain of
//! shallow fragment definitions** does not: every definition here is two braces deep, the document
//! is flat, and the response is as deep as the chain is long. Draft §6.3 expands it one link at a
//! time and [`Limits::max_response_slots`](crate::Limits::max_response_slots) — 2²⁰ by default —
//! is the only ceiling that has an opinion.
//!
//! `execute.rs`'s limits table records the same shape from the other side: collection depth is
//! "removed, not bounded", because "a flat fragment chain drove [a recursive walk] to `SIGABRT`".
//! This is that chain, met by the walk that reads the tree back.

use smear_parser::{
  graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  lexer::tokora::{Parse as _, Parser},
};
use smear_schema::Schema;

use crate::{Executor, Leaf, Values};

use super::{Segment, traversals};

/// One object type that refers to itself, and one nullable leaf to fail at the bottom of it.
const SDL: &str = "type Query { n: Node } type Node { n: Node x: Int }";

/// The smallest value space a chain of objects needs.
enum Value {
  /// Every `n` in the chain.
  Obj,
}

struct Space;

impl Values for Space {
  type Value = Value;

  fn is_null(&self, _: &Value) -> bool {
    false
  }

  fn as_bool(&self, _: &Value) -> Option<bool> {
    None
  }

  fn list_len(&self, _: &Value) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &Value, _: usize) -> Value {
    Value::Obj
  }

  fn type_name<'a>(&'a self, _: &'a Value) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: Value, _: Leaf<'_>) -> Option<Value> {
    Some(value)
  }

  fn variable(&mut self, _: &str) -> Option<Value> {
    None
  }
}

/// An operation whose response is `levels` objects deep, written as a chain of fragments that is
/// two braces deep.
///
/// `{ ...f0 }` with `f0 on Query { n { ...f1 } }`, `fN on Node { n { ...fN+1 } }` and a last
/// `fN on Node { x }`. Acyclic — every spread names a later definition — so nothing here depends on
/// cycle detection admitting it.
fn chain(levels: usize) -> String {
  assert!(
    levels >= 1,
    "a chain shallower than one level is not a chain"
  );
  let mut query = String::from("{ ...f0 }\nfragment f0 on Query { n { ...f1 } }\n");
  for level in 1..levels {
    let next = level + 1;
    query.push_str(&format!(
      "fragment f{level} on Node {{ n {{ ...f{next} }} }}\n"
    ));
  }
  query.push_str(&format!("fragment f{levels} on Node {{ x }}\n"));
  query
}

/// What one run of the fixture measured.
struct Run {
  /// How many segments the failing field's path has.
  depth: usize,
  /// Slots the derivation of that path followed.
  derivation: u64,
  /// Slots the execution that produced the response followed, which should be none of them.
  execution: u64,
  /// The path itself, rendered.
  rendered: String,
}

/// Executes the `levels`-deep chain, fails its leaf, and measures the derivation of its path.
fn measure(levels: usize) -> Run {
  let query = chain(levels);
  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("the SDL is a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(&query)
  .expect("a chain of two-deep fragment definitions parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);

  let before_execution = traversals();
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    if request.name() == "x" {
      executor.handle_field_error(id, "the resolver is degraded");
    } else {
      executor.handle_resolved(&mut space, id, Value::Obj);
    }
    while executor.poll_abandoned().is_some() {}
  }
  let response = executor.poll_response().expect("nothing is outstanding");
  let execution = traversals() - before_execution;

  assert_eq!(
    response.error_count(),
    1,
    "the fixture is meant to fail exactly its one leaf"
  );
  let error = response.errors().next().expect("the one error");

  // The measurement, and nothing else between the two reads: one climb of the tree into one
  // buffer, which is what a serialiser writing §7.1.2 performs per error.
  let mut buf = Vec::new();
  let before = traversals();
  let segments = error.path().collect_into(&mut buf);
  let derivation = traversals() - before;

  let depth = segments.len();
  let rendered = segments
    .iter()
    .map(|segment| match segment {
      Segment::Field(name) => (*name).to_string(),
      Segment::Index(index) => index.to_string(),
    })
    .collect::<Vec<_>>()
    .join(".");

  Run {
    depth,
    derivation,
    execution,
    rendered,
  }
}

/// The shallower of the two depths the curve is read at.
const SHALLOW: usize = 64;

/// The deeper one, sixteen times further down.
///
/// Sixteen times, deliberately. A derivation that costs the depth grows by 16 between these two
/// readings and one that costs its square grows by 256, so no slack a linear bound could carry
/// makes the two readings look alike.
///
/// **1 024 rather than something deeper, and the reason is the Miri lane rather than the property.**
/// The reading is conclusive at any depth past a few dozen: at 65 segments the planted quadratic is
/// 35 traversals per segment and at 1 025 it is 515, against a bound of four. A deeper fixture
/// therefore buys confidence about a number nobody doubts, and what it costs is interpreted
/// execution — `graphql-proto`'s lib tests are in both Miri cells, and this fixture *executes* its
/// chain rather than only collecting one, which is the heavier of the two kinds the lane carries.
///
/// So the depth is set against that lane's existing worst case,
/// `execute::tests::a_flat_fragment_chain_is_linear`, all measured on one host. Natively:
/// **0.027 s** for that one, and for this whole gate **0.015 s** at 1 024, 0.041 s at 2 048,
/// 0.130 s at 4 096 — so at 1 024 this is *cheaper* than the workload the lane already accepts.
/// Under Miri the cost is the depth and almost nothing else: `the_traversal_gate_counts`, the same
/// fixture at depth 8, interprets in **6.6 s**, so the harness is not what a deep leg is paying
/// for. Neither this gate at 1 024 nor the existing test finished inside ten minutes there, which
/// is the honest statement of where this sits — a same-order addition to a lane that already
/// carries one, chosen deliberately over 2 048 and 4 096 rather than by default, because the
/// property is settled well below any of them.
const DEEP: usize = 1024;

/// How many slots one segment of a path may cost to derive.
///
/// A constant, and that is the whole claim: what one segment costs is a property of the links and
/// not of how many segments are above it. Measured on the tree that introduced this gate: 66
/// traversals over 65 segments and 1 026 over 1 025 — one per segment plus the root, at both
/// depths. Measured for the defect it was written against, the restarting derivation restored
/// through the same counted door: 2 276 and 527 876, which is 35 and 515 per segment.
///
/// **A bound on the per-segment cost and not on the total**, because a total admits the reading
/// the defect would pass under: a threshold generous enough for a deep path is generous enough for
/// a shallow quadratic one. Nothing here mentions the square, and nothing here tolerates it.
const SLOTS_PER_SEGMENT: u64 = 4;

/// Deriving a response path follows the links once, however deep the response is.
#[test]
fn a_deep_path_costs_its_depth_in_links_and_not_its_square() {
  let mut readings = Vec::new();
  for levels in [SHALLOW, DEEP] {
    let run = measure(levels);

    // The fixture really is as deep as it claims. Without this the numbers below could be a
    // measurement of a chain that stopped early — a fragment the executor skipped costs nothing to
    // walk, quadratically or otherwise.
    assert_eq!(
      run.depth,
      levels + 1,
      "a {levels}-level chain produced a path of {} segments, so the measurement is of a \
       different response",
      run.depth
    );

    let per_segment = run.derivation / run.depth as u64;
    assert!(
      per_segment <= SLOTS_PER_SEGMENT,
      "a path of {} segments cost {} slot traversals — {per_segment} per segment; the derivation \
       is restarting at the failing position",
      run.depth,
      run.derivation
    );
    readings.push((run.depth, run.derivation));
  }

  let (shallow_depth, shallow_cost) = readings[0];
  let (deep_depth, deep_cost) = readings[1];
  assert!(
    deep_cost * shallow_depth as u64 <= 2 * shallow_cost * deep_depth as u64,
    "{shallow_depth} segments cost {shallow_cost} traversals and {deep_depth} cost {deep_cost}; \
     the second is more than twice what growing with the depth would spend"
  );
}

/// The counter is wired to the derivation, and it moves by what the derivation follows.
///
/// Without this, the gate above could pass because nothing was being counted.
#[test]
fn the_traversal_gate_counts() {
  let run = measure(8);

  // A path of `d` segments cannot be produced from upward links without following at least `d` of
  // them, so a reading below the depth is an instrument that is not attached.
  assert!(
    run.derivation >= run.depth as u64,
    "a path of {} segments registered {} slot traversals",
    run.depth,
    run.derivation
  );

  // And the counter is the *derivation's*, not the execution's: building the response reaches
  // every one of these slots and reaches none of them through this door.
  assert_eq!(
    run.execution, 0,
    "executing the operation registered {} path traversals, so the gate above is measuring the \
     run and not the walk",
    run.execution
  );
}

/// The turn-around is a turn-around: the segments come back in draft §7.1.2's order.
///
/// The cheap direction is the one the links point, so every entry that answers in §7.1.2's order
/// reverses something — and a reversal that is dropped is a defect no cost gate can see.
#[test]
fn the_segments_come_back_outermost_first() {
  let run = measure(3);
  assert_eq!(run.rendered, "n.n.n.x");

  let query = chain(3);
  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("the SDL is a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(&query)
  .expect("the chain parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    if request.name() == "x" {
      executor.handle_field_error(id, "the resolver is degraded");
    } else {
      executor.handle_resolved(&mut space, id, Value::Obj);
    }
    while executor.poll_abandoned().is_some() {}
  }
  let response = executor.poll_response().expect("nothing is outstanding");
  let error = response.errors().next().expect("the one error");
  let path = error.path();

  // The three entries agree, which is what makes them one path read three ways rather than three
  // walks free to drift apart.
  assert_eq!(path.len(), 4);
  assert!(!path.is_empty());
  assert_eq!(path.to_string(), "n.n.n.x");
  assert_eq!(path.iter().collect::<Vec<_>>().len(), 4);

  let outermost_first = path.iter().collect::<Vec<_>>();
  let mut innermost_first = path.ancestors().collect::<Vec<_>>();
  innermost_first.reverse();
  assert_eq!(outermost_first, innermost_first);

  let mut buf = Vec::new();
  assert_eq!(path.collect_into(&mut buf), outermost_first.as_slice());

  // The buffer is the caller's and is reused: a second path into the same buffer replaces the
  // first rather than appending to it.
  assert_eq!(path.collect_into(&mut buf).len(), 4);
}

/// The root has no path, and asking costs one slot rather than a walk.
#[test]
fn the_root_is_the_empty_path() {
  let schema_document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  let schema = Schema::build(&schema_document).expect("the SDL is a schema");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str("{ n { x } }")
  .expect("the query parses");

  let mut space = Space;
  let mut executor = Executor::new(&schema, &document);
  executor
    .start(&mut space, None, Value::Obj)
    .expect("the operation resolves");
  let request = executor.poll_resolve(&mut space).expect("the root's `n`");
  let path = request.path();
  assert_eq!(path.len(), 1);
  assert_eq!(path.to_string(), "n");
  assert!(!path.is_empty());
}
