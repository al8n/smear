//! The property the materialisation axis exists to preserve: **converting the numbers allocates
//! nothing.**
//!
//! Not "allocates little", and not "allocates nothing at all" either — the slice parser already
//! allocates, one container per list and per object, and that cost is the AST's rather than
//! materialisation's. The claim is the exact one: parsing a document with materialised numeric
//! leaves makes the *same* allocations as parsing it with slice leaves, at **every** width the
//! axis ships. Same count, same bytes. Anything a normalising design would have added — an owned
//! `String` per unescaped string literal, a `OnceCell` per numeric node — shows up here as a
//! difference.
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
//!
//! # The second property, measured with the same instrument: a refusal costs a constant
//!
//! `IntOverflow::checked` is the axis's public validating door and it takes **arbitrary** bytes,
//! so its argument is whatever an untrusted payload contained. Deciding whether those bytes are
//! an `IntValue` means asking the lexer, and a lexer dispatches on the first byte: a quoted
//! payload full of malformed escapes entered the inline-string sub-lexer, which appends one
//! diagnostic per escape to a container that `checked` then discards unread. The heap cost of a
//! refusal was proportional to the input being refused — a validating call that grows with what
//! it is going to reject.
//!
//! What is asserted is a **constant**, and the only honest way to measure a constant is to hold
//! it against itself at two sizes far apart: the same shape at 1 KiB and at 1 MiB must cost the
//! same. "Fewer than N allocations" would be satisfied by an implementation that still grew, and
//! "it did not crash" measures nothing at all.

#![cfg(all(feature = "graphql", feature = "materialized-numbers", feature = "std"))]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
};

use smear_parser::{
  graphql::{
    GraphQL,
    ast::{
      InputValue, materialized::InputValue as MaterializedInputValue,
      materialized32::InputValue as Materialized32InputValue,
    },
    error::{GraphqlErrors, IntOverflow, IntWidth},
    syntactic::{
      GraphqlInput, GraphqlLexer,
      value::{materialized, materialized32},
    },
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

fn parse_materialized32_payload() -> Materialized32InputValue<&'static str> {
  drive(materialized32::value, DOCUMENT)
    .expect("the specified-width parser must accept the fixture")
}

/// **Both widths, not just the one that shipped first.** The property is the axis's, so it is
/// measured once per tree: a second width that quietly allocated — a payload that stopped being
/// `Copy`, a conversion that grew a buffer — would be invisible to a gate that only ever read
/// the `i64` tree.
#[test]
fn materialization_allocates_nothing() {
  // One un-measured run of each, so a lazily-initialised static cannot land in whichever
  // measurement happens to run first.
  parse_slice_payload();
  parse_materialized_payload();
  parse_materialized32_payload();

  let (slice_events, slice_bytes) = measure(parse_slice_payload);

  // Non-vacuity first. A fixture that allocated nothing at all would make the equalities below
  // true for the wrong reason — the shape of gate this repository has shipped before and had to
  // repair.
  assert!(
    slice_events > 10,
    "the fixture allocated {slice_events} times; the comparison would be near-vacuous",
  );

  for (width, measured) in [
    ("i64", measure(parse_materialized_payload)),
    ("i32", measure(parse_materialized32_payload)),
  ] {
    let (events, bytes) = measured;
    assert_eq!(
      events, slice_events,
      "materialisation at {width} changed the number of allocations: {slice_events} -> {events}",
    );
    assert_eq!(
      bytes, slice_bytes,
      "materialisation at {width} changed the bytes allocated: {slice_bytes} -> {bytes}",
    );
  }
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

/// The body length of the small and the large refusal. Three orders of magnitude apart, so a cost
/// that tracks the input cannot come back equal by accident.
const SMALL: usize = 1 << 10;
const LARGE: usize = 1 << 20;

/// A byte the gate refuses (`"`), followed by one malformed escape every two bytes — **the
/// finding's own input.** Every escape used to become a diagnostic that `checked` then dropped.
fn quoted_invalid_escapes(body: usize) -> Vec<u8> {
  let mut input = Vec::with_capacity(body + 2);
  input.push(b'"');
  input.extend(b"\\q".repeat(body / 2));
  input.push(b'"');
  input
}

/// The same body left unterminated, which is a different arm of the same sub-lexer.
fn unterminated_invalid_escapes(body: usize) -> Vec<u8> {
  let mut input = Vec::with_capacity(body + 1);
  input.push(b'"');
  input.extend(b"\\q".repeat(body / 2));
  input
}

/// And the block-string arm, whose scanner is the structural twin of the inline one.
fn block_string_invalid_escapes(body: usize) -> Vec<u8> {
  let mut input = Vec::with_capacity(body + 3);
  input.extend_from_slice(b"\"\"\"");
  input.extend(b"\\q".repeat(body / 2));
  input
}

/// A first byte the gate **admits**, with a long illegal suffix behind it: this one reaches the
/// number arm and is refused there, so it is the case that would still grow if the number
/// grammar's diagnostics were per-byte rather than per-token.
fn digit_then_illegal_suffix(body: usize) -> Vec<u8> {
  let mut input = Vec::with_capacity(body + 1);
  input.push(b'1');
  input.extend(std::iter::repeat_n(b'z', body));
  input
}

/// Two number diagnostics at once — leading zeroes *and* an illegal suffix — because a container
/// that holds one error inline starts allocating at the second.
fn leading_zeros_then_suffix(body: usize) -> Vec<u8> {
  let mut input = Vec::with_capacity(body + 1);
  input.extend(std::iter::repeat_n(b'0', body / 2 + 1));
  input.extend(std::iter::repeat_n(b'z', body / 2));
  input
}

/// The other admitted first byte.
fn minus_then_junk(body: usize) -> Vec<u8> {
  let mut input = Vec::with_capacity(body + 1);
  input.push(b'-');
  input.extend(b"\\q".repeat(body / 2));
  input
}

/// A long name: refused by the gate, and refused by the lexer's identifier arm behind it.
fn a_long_name(body: usize) -> Vec<u8> {
  std::iter::repeat_n(b'z', body).collect()
}

/// One shape `checked` has to refuse, named for the lexer path it takes rather than for the bytes
/// it holds, and grown by the length of its repeated part.
struct Refusal {
  what: &'static str,
  build: fn(usize) -> Vec<u8>,
}

impl Refusal {
  const fn new(what: &'static str, build: fn(usize) -> Vec<u8>) -> Self {
    Self { what, build }
  }
}

/// Every path a refused input can take, both sides of the gate.
const REFUSALS: &[Refusal] = &[
  Refusal::new("quoted invalid escapes", quoted_invalid_escapes),
  Refusal::new("unterminated invalid escapes", unterminated_invalid_escapes),
  Refusal::new("block string invalid escapes", block_string_invalid_escapes),
  Refusal::new("digit then illegal suffix", digit_then_illegal_suffix),
  Refusal::new("leading zeros then suffix", leading_zeros_then_suffix),
  Refusal::new("minus then junk", minus_then_junk),
  Refusal::new("a long name", a_long_name),
];

/// **A refusal costs the same heap whatever it is refusing.**
///
/// The measurement the round turns on: `IntOverflow::checked` over a 1 KiB non-literal and over
/// the same shape at 1 MiB, allocations counted on both. Equality is the assertion — not a bound,
/// because a bound is satisfied by growth that has not yet reached it.
///
/// Both widths, because the door takes one and a reader that allocated at only one of them would
/// be invisible to a single-width gate.
///
/// The inputs are built before the recording starts, so what is measured is the refusal and not
/// the fixture. No failure message names the input: printing a megabyte of it would bury the two
/// numbers that matter.
#[test]
fn a_refusal_costs_the_same_heap_at_a_kilobyte_and_at_a_megabyte() {
  for Refusal { what, build } in REFUSALS {
    let small = build(SMALL);
    let large = build(LARGE);
    assert!(
      large.len() > 500 * small.len(),
      "{what}: the two sizes are {} and {} bytes, which is not far enough apart to see growth",
      small.len(),
      large.len(),
    );

    for width in [IntWidth::I32, IntWidth::I64] {
      // Refusals, and established as such outside the measurement — a door that answered `Ok`
      // early would also be constant, and would be constant about nothing.
      assert!(
        IntOverflow::checked(small.as_slice(), width).is_err(),
        "{what} at {} bytes was not refused at {width}",
        small.len(),
      );
      assert!(
        IntOverflow::checked(large.as_slice(), width).is_err(),
        "{what} at {} bytes was not refused at {width}",
        large.len(),
      );

      let small_cost = measure(|| IntOverflow::checked(small.as_slice(), width));
      let large_cost = measure(|| IntOverflow::checked(large.as_slice(), width));

      assert_eq!(
        small_cost,
        large_cost,
        "{what} at {width}: refusing {} bytes cost {small_cost:?} and refusing {} bytes cost \
         {large_cost:?} — (events, bytes); the refusal path is proportional to the input",
        small.len(),
        large.len(),
      );
    }
  }
}

/// The same property **derived over every first byte there is**, rather than over the ones worth
/// guessing.
///
/// The gate admits `-` and the ten digits and refuses the other 245, and the question that leaves
/// is whether an admitted byte reaches a path that accumulates anyway. Enumerating all 256
/// answers it without a curated list — the defect's signature is a case nobody thought of, and a
/// list is written by the person who did not think of it.
///
/// Three tails, none of which can complete an `IntValue`, so every one of the 768 pairs is a
/// refusal and the equality is never about an acceptance. The buffer is built once per tail and
/// its first byte overwritten, so the enumeration adds no allocation of its own to measure
/// around.
#[test]
fn no_first_byte_reaches_a_refusal_whose_cost_grows() {
  const TAILS: &[(&str, &[u8; 2])] = &[
    ("malformed escapes", b"\\q"),
    ("an identifier suffix", b"zz"),
    ("fraction points", b".."),
  ];

  let mut refusals = 0usize;
  for (tail, pattern) in TAILS {
    let mut small = pattern.repeat(SMALL / 2);
    let mut large = pattern.repeat(LARGE / 2);

    for first in u8::MIN..=u8::MAX {
      small[0] = first;
      large[0] = first;

      assert!(
        IntOverflow::checked(small.as_slice(), IntWidth::I64).is_err()
          && IntOverflow::checked(large.as_slice(), IntWidth::I64).is_err(),
        "first byte {first:#04x} then {tail}: not a refusal, so its cost proves nothing",
      );
      refusals += 2;

      let small_cost = measure(|| IntOverflow::checked(small.as_slice(), IntWidth::I64));
      let large_cost = measure(|| IntOverflow::checked(large.as_slice(), IntWidth::I64));

      assert_eq!(
        small_cost,
        large_cost,
        "first byte {first:#04x} then {tail}: refusing {} bytes cost {small_cost:?} and refusing \
         {} bytes cost {large_cost:?} — (events, bytes)",
        small.len(),
        large.len(),
      );
    }
  }

  assert_eq!(
    refusals,
    2 * 256 * TAILS.len(),
    "the enumeration did not reach every first byte",
  );
}
