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

use crate::{Executor, Leaf, Response, Values};

use super::{
  ELIDED, INLINE_SEGMENTS, Segment, allocations, allow_allocations, refuse_allocations_of_at_least,
  traversals,
};

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

  // The four entries agree, which is what makes them one path read four ways rather than four
  // walks free to drift apart. `Debug` is in here and not only in the cost gates below because it
  // is now written over a different walk from `try_iter`'s, and a reversal that is dropped is a
  // defect no cost gate can see.
  assert_eq!(path.len(), 4);
  assert!(!path.is_empty());
  assert_eq!(path.to_string(), "n.n.n.x");
  assert_eq!(path.try_iter().expect("room for a path").count(), 4);
  assert_eq!(
    format!("{path:?}"),
    r#"[Field("n"), Field("n"), Field("n"), Field("x")]"#
  );

  let outermost_first = path
    .try_iter()
    .expect("room for a path")
    .collect::<Vec<_>>();
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

// =================================================================================================
// What the two FORMATTERS cost, which is a different question from what the two collectors cost
// =================================================================================================
//
// The gates above are about `collect_into`: the door a serialiser takes, one buffer for a whole
// response, one allocation per path. They were complete about that door and they were read as
// though they were complete about `Path`. They were not, and the row that fell through the gap had
// been *enumerated* — an infallible `Vec::with_capacity` in the iterator, crossed off a census on
// the grounds that the JSON writer one crate up does not reach it.
//
// It does not. `Debug` and `Display` do, `Error`'s own `Debug` renders a path through them, and the
// depth is the client's — so one `log::warn!` about a deep field error was an abort. A dismissal by
// entry point is only as good as the WIDEST entry point it names, and the widest one for a public
// type is `{}` and `{:?}`.
//
// Hence three doors below rather than one, and a fourth gate on the OTHER fatal axis, because the
// re-audit that found this row found a second one of exactly the same shape: `Node`'s `Debug`
// recursed through the response tree.
//
// AND THEN THE REPAIR ITSELF HAD A ROW. The three doors were not three: the first fix gave the two
// formatters a fallible buffer and answered a refusal with `fmt::Error`, which is not an error a
// formatter may invent — `format!` and `ToString` `expect` on it, so the abort became a panic on
// the same input. Two of the three doors have no refusal to give at all, and the gate below now
// reads that: they truncate, and only `try_iter` refuses.

/// A sink that allocates nothing, so that what a write costs is what the formatter cost.
///
/// [`Silent::low`] is how the native-stack axis is read: the lowest address the sink was reached
/// at, against a local in the caller's frame, is how far down the formatter had gone by the time it
/// produced a byte.
struct Silent {
  bytes: usize,
  low: usize,
}

impl Silent {
  const fn new() -> Self {
    Self { bytes: 0, low: 0 }
  }

  /// How many bytes of frame the formatter had spent below `base` when it first wrote.
  fn frame_below(&self, base: usize) -> usize {
    base.saturating_sub(self.low)
  }
}

impl core::fmt::Write for Silent {
  fn write_str(&mut self, s: &str) -> core::fmt::Result {
    let here = core::ptr::from_ref(&s) as usize;
    if self.low == 0 || here < self.low {
      self.low = here;
    }
    self.bytes += s.len();
    Ok(())
  }
}

/// Runs the `levels`-deep chain and hands its finished response to `body`.
///
/// The same fixture [`measure`] uses, without the measurement — the response borrows the executor,
/// so it cannot be returned and is passed to a closure instead.
fn with_response<R>(levels: usize, body: impl FnOnce(&Response<'_, Value>) -> R) -> R {
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
  body(&response)
}

/// A chain that produces a path filling the window exactly, and one segment longer.
///
/// `chain(n)` fails a leaf `n + 1` segments down, so these are [`INLINE_SEGMENTS`] and one more.
const FILLS_THE_WINDOW: usize = INLINE_SEGMENTS - 1;
const OVERFLOWS_THE_WINDOW: usize = INLINE_SEGMENTS;

/// Printing a path allocates nothing, at **any** depth, and the window is where the rendering
/// stops.
///
/// **Zero is the assertion, and it is a stronger one than a refusal.** A formatter that reserves
/// fallibly still calls the allocator, so it still has a failure to answer and still pays a
/// `malloc` and a `free` to print twenty bytes; one that does not call it has no failure to answer
/// at all. And "no failure to answer" is the whole property here rather than a saving, because the
/// only answer a formatter *has* is [`fmt::Error`](core::fmt::Error), which `core` reserves for a
/// failure the sink reported — see the fixture below.
///
/// # What changed under the second reading, and why it is not the same evidence
///
/// The deep leg used to assert **one** allocating event, which is what showed the window was
/// exactly [`INLINE_SEGMENTS`] wide and that the zero above was the window's doing rather than the
/// counter's silence. There is no allocation past the window any more, so that evidence is gone and
/// something has to replace it: the **marker**. A path of exactly [`INLINE_SEGMENTS`] segments
/// renders without one and a path one segment deeper renders with one, which pins the same edge
/// through the output instead of through the allocator.
#[test]
fn printing_a_path_never_reaches_the_allocator() {
  use core::fmt::Write as _;

  with_response(FILLS_THE_WINDOW, |response| {
    let error = response.errors().next().expect("the one error");
    let path = error.path();
    assert_eq!(
      path.len(),
      INLINE_SEGMENTS,
      "the fixture is meant to fill the window exactly"
    );

    let mut display = Silent::new();
    let before = allocations();
    write!(display, "{path}").expect("a sink that cannot fail");
    let display_cost = allocations() - before;

    let mut debug = Silent::new();
    let before = allocations();
    write!(debug, "{path:?}").expect("a sink that cannot fail");
    let debug_cost = allocations() - before;

    assert_eq!(
      display_cost,
      0,
      "`Display` on a {}-segment path went to the allocator {display_cost} times",
      path.len()
    );
    assert_eq!(
      debug_cost,
      0,
      "`Debug` on a {}-segment path went to the allocator {debug_cost} times",
      path.len()
    );
    assert!(
      display.bytes > 0 && debug.bytes > 0,
      "neither formatter wrote anything, so the readings above are of nothing"
    );

    // A path that fits carries no marker, which is the other half of the edge the deep leg reads.
    assert!(
      !path.to_string().contains(ELIDED) && !format!("{path:?}").contains(ELIDED),
      "a path of exactly {INLINE_SEGMENTS} segments rendered as though it had been cut"
    );
  });

  // One segment further down. The allocator is still untouched — that is the repair — so the
  // window's width is read off the rendering instead.
  with_response(OVERFLOWS_THE_WINDOW, |response| {
    let error = response.errors().next().expect("the one error");
    let path = error.path();
    assert_eq!(path.len(), INLINE_SEGMENTS + 1);

    let mut display = Silent::new();
    let before = allocations();
    write!(display, "{path}").expect("a sink that cannot fail");
    let display_cost = allocations() - before;

    let mut debug = Silent::new();
    let before = allocations();
    write!(debug, "{path:?}").expect("a sink that cannot fail");
    let debug_cost = allocations() - before;

    assert_eq!(
      (display_cost, debug_cost),
      (0, 0),
      "a path one segment past the window went to the allocator {display_cost} times to print and \
       {debug_cost} times to debug"
    );

    // What the truncation KEEPS, in the order it keeps it, and where the marker sits.
    //
    // This assertion used to be a parity check: there were two walks, the window's and a buffered
    // one, and every pre-existing ordering fixture rendered a four-segment path through the first.
    // **The two walks are one walk now**, so it is no longer covering a second implementation and
    // saying so is the point — what it covers instead is which END of a deep path survives
    // (the innermost, which is the end that names the field), that the survivors still come out
    // outermost first, and that the marker stands where the dropped ones were.
    let mut expected = String::from(ELIDED);
    for _ in 0..INLINE_SEGMENTS - 1 {
      expected.push_str(".n");
    }
    expected.push_str(".x");
    assert_eq!(
      path.to_string(),
      expected,
      "a path one segment past the window did not render as its innermost {INLINE_SEGMENTS} \
       segments behind a marker"
    );
  });
}

/// The size at and above which the fixtures below make the allocator answer with null.
///
/// Between the two quantities the fixture has to separate, with a doubling of margin on each side.
/// **Below it**: everything `format!` and `ToString` allocate for an *answer*, which the window now
/// caps — the longest rendering below is 67 bytes and a `String` grown from nothing reaches it in
/// requests of at most 128. **At or above it**: the path buffer for a path one segment past the
/// window, which is 33 [`Segment`]s and therefore exactly 528 bytes, and which is the one
/// allocation the fixture is about.
const REFUSE_BYTES: usize = 256;

/// A path too deep for the window is **truncated**, because a formatter has no refusal to give.
///
/// # `fmt::Error` is a claim about the sink, and this one would have been false
///
/// The round before this one gave [`Debug`](core::fmt::Debug) and [`Display`](core::fmt::Display) a
/// fallible buffer past the window and answered a refusal with
/// [`fmt::Error`](core::fmt::Error). `core` reserves that error for a failure the
/// [`Formatter`](core::fmt::Formatter) itself reported — string formatting is otherwise infallible
/// — so `format!` ends in `.expect("a formatting trait implementation returned an error when the
/// underlying stream did not")` and `ToString` in `.expect("a Display implementation returned an
/// error unexpectedly")`. Manufacturing the error therefore turned an allocation failure into a
/// **panic in the caller**, which is the abort of two rounds ago wearing a different coat.
///
/// So this reads the repair rather than a refusal: under an allocator that says no, both formatters
/// *return*, truncated and marked. Four renderings, because the change to the representation
/// propagates into everything that embeds one and the two nested `Debug`s are not reachable from
/// each other: [`Error`](crate::Error) exists only once the response is finished and
/// [`FieldRequest`](crate::FieldRequest) only before it is.
///
/// [`Path::try_iter`] is the one door that still refuses, and it is read under the *same* threshold
/// so that the refusal is a difference rather than an absence — a threshold that stopped nothing
/// would leave every reading here green for the wrong reason.
///
/// # Why the propagating door is read before the two that `expect`
///
/// Because a regression has to fail *legibly*. Restoring the manufactured error and running the
/// `format!` leg first was measured: it panics, the panic machinery asks for 1 792 bytes, the armed
/// allocator refuses that too, and the binary dies inside `handle_alloc_error` — under `cargo test`
/// that hangs the harness rather than failing it. `write!` into a sink *propagates* the error
/// instead of expecting on it, so putting that window first turns the same regression into a plain
/// assertion failure and the `format!` window is never reached.
#[test]
fn a_path_past_the_window_is_truncated_rather_than_answering_fmt_error() {
  use core::fmt::Write as _;

  let query = chain(OVERFLOWS_THE_WINDOW);
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

  // The mid-flight door. A `FieldRequest` renders its path inside its own output the way an `Error`
  // does, and it is handed out *before* the value is resolved, so it cannot be reached from the
  // finished response below.
  let mut request_answer = Err(core::fmt::Error);
  let mut request_rendering = String::new();
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    if request.name() == "x" {
      // Armed with nothing between arming and disarming that could panic — the panic machinery
      // allocates, so a panic inside the window meets a refusal while the first one is handled.
      let mut sink = Silent::new();
      refuse_allocations_of_at_least(REFUSE_BYTES);
      request_answer = write!(sink, "{request:?}");
      allow_allocations();
      request_rendering = format!("{request:?}");
      executor.handle_field_error(id, "the resolver is degraded");
    } else {
      executor.handle_resolved(&mut space, id, Value::Obj);
    }
    while executor.poll_abandoned().is_some() {}
  }

  let response = executor.poll_response().expect("nothing is outstanding");
  let error = response.errors().next().expect("the one error");
  let path = error.path();
  assert_eq!(path.len(), INLINE_SEGMENTS + 1);

  // First window: the door that propagates. Under the plant every one of these is `Err` and the
  // assertions below fire before anything reaches an `expect`.
  let mut sink = Silent::new();
  refuse_allocations_of_at_least(REFUSE_BYTES);
  let display_answer = write!(sink, "{path}");
  let debug_answer = write!(sink, "{path:?}");
  let error_answer = write!(sink, "{error:?}");
  allow_allocations();

  assert_eq!(
    (display_answer, debug_answer, error_answer, request_answer),
    (Ok(()), Ok(()), Ok(()), Ok(())),
    "a formatter answered `fmt::Error` for a path the allocator refused room for, which is an \
     error `core` reserves for a failure the sink reported"
  );

  // Second window: the two spellings that turn an answered `fmt::Error` into a panic, and the one
  // door that still has a refusal to give.
  refuse_allocations_of_at_least(REFUSE_BYTES);
  let displayed = format!("{path}");
  let stringified = path.to_string();
  let refused_iter = path.try_iter().is_err();
  allow_allocations();

  // `{:?}`'s own answer is 389 bytes and needs a 512-byte `String` to hold it, which is *larger*
  // than the 528-byte buffer under test — so no threshold separates the two and this one is
  // rendered unarmed. Nothing is lost: `Debug` was read armed above through a sink that cannot
  // allocate, which is the stronger reading, and a `Debug` that returns `Ok` under refusal has
  // nothing for `format!` to `expect` on.
  let debugged = format!("{path:?}");

  assert!(
    refused_iter,
    "`try_iter` served a path the allocator refused room for, so the threshold stopped nothing and \
     every reading here is green for the wrong reason"
  );

  // What `{}` produces, exactly: the marker, then the innermost `INLINE_SEGMENTS` segments in
  // §7.1.2's order. The chain fails its leaf `x` under a run of `n`s, so the survivors are the last
  // of those and the leaf.
  let mut expected = String::from(ELIDED);
  for _ in 0..INLINE_SEGMENTS - 1 {
    expected.push_str(".n");
  }
  expected.push_str(".x");
  assert_eq!(
    displayed, expected,
    "`Display` did not truncate a path the allocator refused room for"
  );
  assert_eq!(
    stringified, expected,
    "`ToString` and `{{}}` rendered the same path differently"
  );

  // And what `{:?}` produces, exactly. The marker is a bare `…` rather than a quoted one, so it is
  // not even shaped like the `Field("…")` no `Segment` could render as in the first place.
  let mut expected_debug = format!("[{ELIDED}");
  for _ in 0..INLINE_SEGMENTS - 1 {
    expected_debug.push_str(", Field(\"n\")");
  }
  expected_debug.push_str(", Field(\"x\")]");
  assert_eq!(
    debugged, expected_debug,
    "`Debug` did not truncate a path the allocator refused room for"
  );

  // The two nested renderings carry the truncated path, and carry it where their own field says.
  assert_eq!(
    format!("{error:?}"),
    format!(
      "Error {{ kind: {:?}, path: {expected_debug}, locations: {:?} }}",
      error.kind(),
      error.locations()
    ),
    "`Error`'s `Debug` did not render the truncated path in its `path` field"
  );
  assert!(
    request_rendering.starts_with("FieldRequest {")
      && request_rendering.contains(&format!("path: {expected_debug}")),
    "`FieldRequest`'s `Debug` rendered {request_rendering}"
  );
}

/// The shallow reading of the response-tree formatter, and the deep one.
///
/// Four times apart rather than sixteen: what is being separated here is a constant from a term
/// that grows by roughly a kilobyte a level, so four levels of separation is 26 KiB against a
/// tolerance of 128 bytes — and the fixture stays cheap enough to interpret.
const SHALLOW_TREE: usize = 8;
const DEEP_TREE: usize = 32;

/// How much more frame the deeper reading may spend. A constant, because the claim is that the
/// number does not move with the depth at all.
const FRAME_SLACK: usize = 128;

/// Rendering a response with `{:?}` does not put its depth on the native stack.
///
/// # The second row the re-audit turned up, and it is not on the heap
///
/// [`Node`](super::Node)'s `Debug` used to descend into its children, which is one frame per level
/// of a tree whose depth the client chooses: **1 088 bytes per level** measured on this host, and
/// `{:?}` on a 4 000-deep response overflowed a test thread's stack and aborted the binary. It is
/// the [`Path`] finding with the axis swapped — a public formatter, request-shaped input, off the
/// serialiser's path and reached by anything that logs — which is why it is gated here beside it
/// rather than reported and left.
///
/// Read as a difference of two addresses, because that is the only instrument that sees this: the
/// allocation counter reads **0** for the recursive version and 0 for this one. That pairing is the
/// whole reason both gates exist.
#[test]
fn rendering_a_response_does_not_reach_the_native_stack() {
  use core::fmt::Write as _;

  let mut readings = Vec::new();
  for levels in [SHALLOW_TREE, DEEP_TREE] {
    let reading = with_response(levels, |response| {
      let anchor = 0u8;
      let base = core::ptr::from_ref(&anchor) as usize;

      let mut sink = Silent::new();
      let before = allocations();
      write!(sink, "{response:?}").expect("a sink that cannot fail");
      let allocated = allocations() - before;

      assert!(
        sink.bytes > 0,
        "the response rendered no bytes, so the frame reading is of nothing"
      );
      (sink.frame_below(base), allocated)
    });
    readings.push((levels, reading));
  }

  let (shallow_levels, (shallow_frame, shallow_allocs)) = readings[0];
  let (deep_levels, (deep_frame, deep_allocs)) = readings[1];

  assert!(
    deep_frame <= shallow_frame + FRAME_SLACK,
    "a {shallow_levels}-deep response rendered {shallow_frame} bytes below the caller's frame and \
     a {deep_levels}-deep one rendered {deep_frame}; the formatter is descending"
  );

  // The other axis, in the same breath and for the reason the counters' header gives: a formatter
  // that swapped the recursion for a work stack would pass the reading above and fail this one.
  assert_eq!(
    (shallow_allocs, deep_allocs),
    (0, 0),
    "rendering a response went to the allocator"
  );
}
