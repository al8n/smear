//! What deriving one draft §7.1.2 path costs, counted in the links it follows **and** in the times
//! it goes to the allocator.
//!
//! # Why the gate is here and not in the serialiser that met the defect
//!
//! `smear`'s JSON writer is where the cost was *observed* — it is the only caller that consumes a
//! whole path for every error in a response — but the walk is this module's, and a gate placed on
//! one consumer says nothing about the next one. So the instruments are on the derivation itself:
//! [`super::traversals`] counts every slot a path derivation reaches, through the one private
//! accessor that can reach them, and [`super::allocations`] counts every allocating event the
//! process makes while one runs. The properties below are properties of [`Path`] rather than of
//! anybody's serialiser.
//!
//! # Why two counters and not one
//!
//! Because each is blind where the other looks, and this file has now been wrong in both
//! directions. A derivation that restarts at the failing position walks the depth squared and
//! allocates once; a derivation that fills an unhinted buffer walks the depth once and allocates
//! `log₂` of it, copying as it goes. One number cannot separate those from a correct one, and the
//! plants below are run against *both* readings for exactly that reason: removing the reservation
//! must move the allocation count while leaving the traversal count untouched, and that pairing is
//! what shows the axes are measuring different things. The counters' own header in the parent
//! module carries the three rounds this was learned over.
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

use super::{Segment, allocations, traversals};

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

/// What one run of the fixture measured, on both axes.
struct Run {
  /// How many segments the failing field's path has.
  depth: usize,
  /// Slots the derivation of that path followed.
  derivation: u64,
  /// Allocating events that derivation caused, into a buffer that had never held anything.
  allocated: u64,
  /// Allocating events a *second* derivation caused, into the buffer the first one filled.
  reuse: u64,
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

  // The measurement, and nothing else between the reads: one climb of the tree into one buffer,
  // which is what a serialiser writing §7.1.2 performs per error. Both counters are read before
  // the window opens as well as after it, so the first touch of either thread-local is outside it.
  let mut buf = Vec::new();
  let before_links = traversals();
  let before_heap = allocations();
  let segments = error
    .path()
    .collect_into(&mut buf)
    .expect("room for a path");
  let derivation = traversals() - before_links;
  let allocated = allocations() - before_heap;

  let depth = segments.len();
  let rendered = segments
    .iter()
    .map(|segment| match segment {
      Segment::Field(name) => (*name).to_string(),
      Segment::Index(index) => index.to_string(),
    })
    .collect::<Vec<_>>()
    .join(".");

  // The number the executor recorded IS the length of the chain, at every depth this file reads.
  // Everything above rests on it: the reservation is exact only if the two agree, and only one
  // direction of a disagreement is visible in the allocation count — an undercount reallocates
  // and shows up, an OVERCOUNT allocates once and looks perfect while asking for the wrong size.
  assert_eq!(
    error.path().len(),
    depth,
    "the depth recorded on the slot is not the number of segments the walk produced"
  );

  // The same buffer, a second time, which is what the next error in a response does to it. The
  // reading is the other half of "one buffer serves the whole response": the first path pays, and
  // no path no deeper than it pays again.
  let before_reuse = allocations();
  let again = error
    .path()
    .collect_into(&mut buf)
    .expect("room for a path");
  let reuse = allocations() - before_reuse;
  assert_eq!(
    again.len(),
    depth,
    "the second derivation produced a different path, so the reuse reading is of something else"
  );

  Run {
    depth,
    derivation,
    allocated,
    reuse,
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
/// **1 024 rather than something deeper, because the reading is conclusive well below it.** At 65
/// segments the planted quadratic is 33 traversals per segment and at 1 025 it is 513, against a
/// bound of four; the planted geometric growth is 6 allocations and 10, against a bound of one. A
/// deeper fixture buys confidence about numbers nobody doubts and costs wall clock for it — natively
/// **0.015 s** at 1 024, 0.041 s at 2 048 and 0.130 s at 4 096, all on one host. Chosen against
/// those readings rather than by default.
///
/// Under Miri it is not run at all, and the gate below says so with `#[cfg_attr(miri, ignore)]`;
/// read that attribute for why a cost gate has no business in an interpreter.
const DEEP: usize = 1024;

/// How many slots one segment of a path may cost to derive.
///
/// A constant, and that is the whole claim: what one segment costs is a property of the links and
/// not of how many segments are above it. Measured on this tree: **67** traversals over 65 segments
/// and **1 027** over 1 025 — one per segment, plus the root, plus the single slot
/// [`Path::len`](super::Path::len) reads to size the buffer before the walk begins. Measured for the
/// defect it was written against, the restarting derivation restored through the same counted door:
/// **2 146** and **525 826**, which is 33 and 513 per segment.
///
/// **A bound on the per-segment cost and not on the total**, because a total admits the reading
/// the defect would pass under: a threshold generous enough for a deep path is generous enough for
/// a shallow quadratic one. Nothing here mentions the square, and nothing here tolerates it.
const SLOTS_PER_SEGMENT: u64 = 4;

/// How many times deriving one path into a fresh buffer may go to the allocator.
///
/// **A constant, and it has to be a constant.** The obvious bound here is a logarithm — an unhinted
/// `Vec` filled from [`Ancestors`](super::Ancestors) doubles, so `log₂(depth)` is what the *defect*
/// costs — and a gate shaped like the defect's own curve is a gate the defect passes. What
/// reserving up front buys is a number that does not move with the depth at all, so that is the
/// number written down.
///
/// Measured on this tree: **1** at 65 segments and **1** at 1 025. Measured with the reservation
/// removed and the `extend` left exactly as it was, through the same counted allocator: **6** and
/// **10** — while the traversal reading stayed at **67** and **1 027**, not one link different.
/// That pair is the reason both axes are here, and the converse pair is in
/// [`SLOTS_PER_SEGMENT`]'s own note: under the restarting derivation the traversals go to 2 146
/// and 525 826 and this number stays at **1**. Neither counter can see the other's defect.
///
/// `<=` rather than `==` for the reason the layout assertions in the parent module use it: a future
/// `Vec` that satisfied the request without going to the allocator at all would be an improvement,
/// and a gate that failed a green tree for one is a gate nobody keeps.
/// [`the_cost_gates_count`] is what stops `0` from being an instrument that is not attached.
const ALLOCATIONS_PER_DERIVATION: u64 = 1;

/// Deriving a response path follows the links once and allocates once, however deep the response is.
///
/// # Two axes, because the first one alone passed the defect the second was written for
///
/// The link counter reads 67 and 1 027 on a walk that reallocates its buffer six and ten times: the
/// segments are the same, the links are the same, and the copying is invisible to it. The
/// allocation counter reads 1 on a walk that restarts at the failing position for every segment and
/// costs half a million links: one buffer, filled once. Each is blind exactly where the other
/// looks, so both are asserted here and each one's plant was checked against the other reading —
/// see the counters' own header in the parent module for the three rounds that taught it.
#[cfg_attr(
  miri,
  ignore = "A COST GATE, AND MIRI PRICES A DIFFERENT THING. What is asserted below is slot \
            traversals and allocator calls at depth 1 024, which is a claim about the shape of the \
            work; an interpreter answers whether that work has undefined behaviour, and it answers \
            it out of the same MIR that the shallow legs of this file already interpret. So running \
            it there re-decides nothing and costs the depth: measured on this host, neither this \
            gate nor its neighbour `execute::tests::a_flat_fragment_chain_is_linear` returned \
            inside ten minutes under `cargo miri test`. `the_cost_gates_count` runs the same \
            fixture at depth 8 and IS interpreted, so every line reached here is still reached \
            there. Declared in `ci/miri_scope.py`'s ignore table, which is what stops this from \
            being a coverage cut nobody chose."
)]
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

    // The allocation axis, at the same depth and in the same breath. A constant is asserted
    // directly rather than compared between the two readings, because `log₂` of these two depths
    // differs by four and any comparison loose enough to carry that is loose enough to carry the
    // growth itself.
    assert!(
      run.allocated <= ALLOCATIONS_PER_DERIVATION,
      "a path of {} segments cost {} allocating events; the buffer is growing into the walk \
       instead of being reserved before it",
      run.depth,
      run.allocated
    );

    // And the buffer really is reused: the next error in a response pays nothing for a path no
    // deeper than one already collected. Without this the reading above would be satisfied by a
    // buffer that reallocated exactly once per error.
    assert_eq!(
      run.reuse, 0,
      "a second path of {} segments into the same buffer cost {} allocating events",
      run.depth, run.reuse
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

/// Both counters are wired to the derivation, and both move by what it does.
///
/// Without this, the gate above could pass because nothing was being counted — on either axis, and
/// the two are attached by different mechanisms, so each needs saying.
#[test]
fn the_cost_gates_count() {
  let run = measure(8);

  // A path of `d` segments cannot be produced from upward links without following at least `d` of
  // them, so a reading below the depth is an instrument that is not attached.
  assert!(
    run.derivation >= run.depth as u64,
    "a path of {} segments registered {} slot traversals",
    run.depth,
    run.derivation
  );

  // And a path of `d` segments cannot be produced into an empty `Vec` without the allocator being
  // asked once, so a reading of zero is the counting allocator not being installed.
  assert!(
    run.allocated >= 1,
    "collecting a path of {} segments into an empty buffer registered no allocating event, so the \
     counting allocator is not the one this binary is running on",
    run.depth
  );

  // And the traversal counter is the *derivation's*, not the execution's: building the response
  // reaches every one of these slots and reaches none of them through this door.
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
  assert_eq!(
    path.collect_into(&mut buf).expect("room for a path"),
    outermost_first.as_slice()
  );

  // The buffer is the caller's and is reused: a second path into the same buffer replaces the
  // first rather than appending to it.
  assert_eq!(
    path.collect_into(&mut buf).expect("room for a path").len(),
    4
  );
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
