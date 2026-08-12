//! The property the materialisation axis exists to preserve: **converting the numbers allocates
//! nothing.**
//!
//! Not "allocates little", and not "allocates nothing at all" either — the slice parser already
//! allocates, one container per list and per object, and that cost is the AST's rather than
//! materialisation's. The claim is the exact one: parsing a document with `i64`/`f64` leaves makes
//! the *same* allocations as parsing it with slice leaves. Same count, same bytes. Anything a
//! normalising design would have added — an owned `String` per unescaped string literal, a
//! `OnceCell` per numeric node — shows up here as a difference.
//!
//! # Why a counting allocator and not a size assertion
//!
//! `size_of` would catch the `OnceCell` and miss the `String`, which is the one that scales with
//! the document. The measurement has to be of the work, and the work is the allocator's.
//!
//! The counter is **thread-local**, so `cargo test`'s other harness threads cannot pollute it,
//! and it is off unless a measurement is in progress. `Cell<bool>` and `Cell<usize>` behind a
//! `const`-initialised `thread_local!` have no destructor and allocate nothing themselves, so the
//! allocator cannot recurse through its own counter.

#![cfg(all(feature = "graphql", feature = "materialized-numbers", feature = "std"))]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
};

use smear_parser::{
  graphql::{
    GraphQL,
    ast::{InputValue, materialized::InputValue as MaterializedInputValue},
    error::GraphqlErrors,
    syntactic::{GraphqlInput, GraphqlLexer, value::materialized},
  },
  lexer::graphql::syntactic::SyntacticLexer,
};
use tokora::{FatalContext, Parse, Parser};

thread_local! {
  static RECORDING: Cell<bool> = const { Cell::new(false) };
  static EVENTS: Cell<usize> = const { Cell::new(0) };
  static BYTES: Cell<usize> = const { Cell::new(0) };
}

/// A pass-through allocator that tallies every allocating event on the recording thread.
///
/// `realloc` counts too, and counts its *new* size: a `Vec` that grows by doubling does its work
/// through `realloc`, and an implementation that allocated more would most likely show up there
/// rather than in `alloc`.
struct Counting;

#[allow(unsafe_code)]
unsafe impl GlobalAlloc for Counting {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    record(layout.size());
    unsafe { System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    record(layout.size());
    unsafe { System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    record(new_size);
    unsafe { System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    unsafe { System.dealloc(ptr, layout) }
  }
}

#[inline]
fn record(size: usize) {
  let _ = RECORDING.try_with(|recording| {
    if recording.get() {
      let _ = EVENTS.try_with(|c| c.set(c.get().wrapping_add(1)));
      let _ = BYTES.try_with(|c| c.set(c.get().wrapping_add(size)));
    }
  });
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// Allocating events and bytes requested while `f` ran on this thread.
fn measure<T>(f: impl FnOnce() -> T) -> (usize, usize) {
  EVENTS.with(|c| c.set(0));
  BYTES.with(|c| c.set(0));
  RECORDING.with(|r| r.set(true));
  let produced = f();
  RECORDING.with(|r| r.set(false));
  let counts = (EVENTS.with(Cell::get), BYTES.with(Cell::get));
  drop(produced);
  counts
}

type Ctx<'inp> = FatalContext<'inp, SyntacticLexer<'inp>, GraphqlErrors<&'inp str>, GraphQL>;

/// A document with every leaf kind and two levels of nesting, so the comparison covers the
/// container allocations as well as the leaves — and so the counts are large enough that a
/// difference of one is visible.
const DOCUMENT: &str = r#"{
  ints: [1, -2, 3, 4, 5, 6, 7, 8, 9, 10],
  floats: [1.5, -2.25e3, 3.0, 4.5e-2, 5.125],
  strings: ["alpha", "beta\ngamma", "delta", "epsilon"],
  misc: [true, false, null, SOME_ENUM],
  nested: {a: {b: {c: [1, 2.0, "three"]}}, d: [[1], [2], [3.5]]}
}"#;

/// Drives one production over `input` under `Fatal<GraphqlErrors<&str>>`, the same way the
/// crate's own value suites do.
fn drive<'inp, O>(
  f: impl for<'c> FnMut(
    &mut GraphqlInput<'inp, 'c, str, Ctx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser::<'inp, GraphqlLexer<'inp, str>, O, GraphqlErrors<&'inp str>, _, GraphQL>(f)
    .parse_str(input)
}

fn parse_slice_payload() -> InputValue<&'static str> {
  drive(InputValue::<&str>::graphql, DOCUMENT).expect("the slice parser must accept the fixture")
}

fn parse_materialized_payload() -> MaterializedInputValue<&'static str> {
  drive(materialized::value, DOCUMENT).expect("the materialising parser must accept the fixture")
}

#[test]
fn materialization_allocates_nothing() {
  // One un-measured run of each, so a lazily-initialised static cannot land in whichever
  // measurement happens to run first.
  parse_slice_payload();
  parse_materialized_payload();

  let (slice_events, slice_bytes) = measure(parse_slice_payload);
  let (materialized_events, materialized_bytes) = measure(parse_materialized_payload);

  // Non-vacuity first. A fixture that allocated nothing at all would make the equality below
  // true for the wrong reason — the shape of gate this repository has shipped before and had to
  // repair.
  assert!(
    slice_events > 10,
    "the fixture allocated {slice_events} times; the comparison would be near-vacuous",
  );

  assert_eq!(
    materialized_events, slice_events,
    "materialisation changed the number of allocations: {slice_events} -> {materialized_events}",
  );
  assert_eq!(
    materialized_bytes, slice_bytes,
    "materialisation changed the bytes allocated: {slice_bytes} -> {materialized_bytes}",
  );
}

/// The measurement instrument itself, proven to move. Without this the gate above cannot
/// distinguish "materialisation allocates nothing" from "the counter is stuck at zero" — and a
/// stuck counter is exactly how the previous no-op plants in this stack passed.
#[test]
fn the_counter_counts() {
  let (events, bytes) = measure(|| std::vec![0_u8; 4096]);
  assert!(events > 0, "an allocating closure recorded no events");
  assert!(
    bytes >= 4096,
    "an allocating closure recorded {bytes} bytes"
  );

  let (events, bytes) = measure(|| 1_u32 + 1);
  assert_eq!(
    (events, bytes),
    (0, 0),
    "a non-allocating closure recorded work"
  );
}
