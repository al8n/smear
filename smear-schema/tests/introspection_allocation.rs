//! The reader copies nothing out of a response, measured rather than asserted.
//!
//! # What is being claimed
//!
//! A draft §4 response is read **shape-first**, straight out of the caller's buffer: names, type
//! kinds and directive locations are matched against the response's own bytes and never copied,
//! and the only member that can allocate is `__InputValue.defaultValue`, which allocates only when
//! the literal it was written as actually contains a backslash.
//!
//! # How a public door can measure a private property
//!
//! [`to_sdl`] renders what it reads, so lengthening a name that reaches the SDL lengthens the
//! output and the reading with it. The measurement needs a name the reader **reads and the
//! renderer drops**, and every real response has one: the introspection meta-schema is in every
//! server's `__Schema.types` and the door drops all eight of its types, because every build
//! injects them. So `__Schema`'s own fields are decoded in full and rendered not at all —
//! lengthening them changes what the reader handles and cannot change what it emits.
//!
//! Two corpora that differ only in those names must therefore produce **the same SDL** and **the
//! same allocation**, and [`the_gate_counts`] is the discrimination check that says a matching
//! reading means "nothing was copied" rather than "nothing was looking".

// The door itself is behind `introspection`, so with the feature off this file is an empty crate
// rather than a hard `E0432` — the gate `smear/tests/validator_introspection.rs` carried until it
// was given the same header.
#![cfg(feature = "introspection")]
#![allow(missing_docs)]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
};

use smear_schema::introspection::to_sdl;

// ---------------------------------------------------------------------------------------------
// the counting allocator
// ---------------------------------------------------------------------------------------------

thread_local! {
  /// Bytes requested on this thread. Thread-local so a test running beside this one cannot
  /// perturb the reading.
  static BYTES: Cell<u64> = const { Cell::new(0) };
}

struct Counting;

/// Counts the bytes of every allocation event and forwards to the system allocator.
///
/// A growing `realloc` counts only its growth, which is what makes a `String` that doubles read as
/// the bytes it gained rather than as the bytes it already held.
unsafe impl GlobalAlloc for Counting {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    bump(layout.size() as u64);
    unsafe { System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    bump(layout.size() as u64);
    unsafe { System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    bump(new_size.saturating_sub(layout.size()) as u64);
    unsafe { System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    unsafe { System.dealloc(ptr, layout) }
  }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

fn bump(bytes: u64) {
  let _ = BYTES.try_with(|count| count.set(count.get() + bytes));
}

/// Runs `body` and returns how many bytes it asked the allocator for on this thread.
fn allocated(body: impl FnOnce()) -> u64 {
  let before = BYTES.with(Cell::get);
  body();
  BYTES.with(Cell::get) - before
}

// ---------------------------------------------------------------------------------------------
// the corpus
// ---------------------------------------------------------------------------------------------

/// A response a server would return, with two knobs the door is supposed to be indifferent to.
///
/// `pad` lengthens every field name of `__Schema` — read in full, rendered never. `prose`
/// lengthens a `description`, which is a member the door does not read at all. Neither reaches the
/// SDL, so neither may reach the allocator.
fn response(pad: usize, prose: usize) -> String {
  let p = "z".repeat(pad);
  let description = "d".repeat(prose);
  let meta_fields: Vec<String> = (0..24)
    .map(|index| {
      format!(
        r#"{{"name":"meta{index}{p}","description":"{description}","args":[
             {{"name":"arg{index}{p}","type":{{"kind":"SCALAR","name":"String"}},
               "defaultValue":null}}
           ],"type":{{"kind":"NON_NULL","name":null,"ofType":
             {{"kind":"LIST","name":null,"ofType":{{"kind":"SCALAR","name":"String"}}}}}}}}"#
      )
    })
    .collect();

  format!(
    r#"{{"data":{{"__schema":{{
      "queryType":{{"name":"Query"}},
      "mutationType":null,
      "subscriptionType":null,
      "directives":[
        {{"name":"tag","description":"{description}","locations":["FIELD_DEFINITION"],"args":[
          {{"name":"note","type":{{"kind":"SCALAR","name":"String"}},"defaultValue":"\"x\""}}
        ],"isRepeatable":false}}
      ],
      "types":[
        {{"kind":"OBJECT","name":"Query","description":"{description}","interfaces":[],"fields":[
          {{"name":"ok","args":[
            {{"name":"first","type":{{"kind":"SCALAR","name":"Int"}},"defaultValue":"10"}}
          ],"type":{{"kind":"NON_NULL","name":null,"ofType":{{"kind":"SCALAR","name":"String"}}}}}}
        ]}},
        {{"kind":"SCALAR","name":"String"}},
        {{"kind":"SCALAR","name":"Int"}},
        {{"kind":"OBJECT","name":"__Schema","description":"{description}","fields":[{}]}}
      ]
    }}}}}}"#,
    meta_fields.join(",")
  )
}

fn sdl(response: &str) -> String {
  to_sdl(response).unwrap_or_else(|error| panic!("expected SDL, got: {error}"))
}

// ---------------------------------------------------------------------------------------------
// the gate
// ---------------------------------------------------------------------------------------------

/// A document of only names allocates nothing for them.
#[test]
fn lengthening_a_name_the_door_reads_and_drops_costs_nothing() {
  let short = response(0, 0);
  let long = response(4096, 0);
  assert!(
    long.len() > short.len() + 100_000,
    "the corpora do not differ enough for the reading to mean anything"
  );

  // The two responses describe the same schema, so the renderer's own cost is identical and the
  // only thing that could differ is what the reader did with the names it dropped.
  assert_eq!(
    sdl(&short),
    sdl(&long),
    "the two corpora do not render the same SDL, so their allocations are not comparable"
  );

  // Warm whatever the parser initialises once, so the first reading is not the one measured.
  let _ = sdl(&short);
  let _ = sdl(&long);

  let short_bytes = allocated(|| drop(sdl(&short)));
  let long_bytes = allocated(|| drop(sdl(&long)));
  assert_eq!(
    short_bytes,
    long_bytes,
    "reading {} more bytes of names cost {} more bytes of allocation; the names were copied",
    long.len() - short.len(),
    long_bytes.saturating_sub(short_bytes)
  );
}

/// A member the door does not read costs nothing either.
///
/// This is the half a value tree could never have: a reader that materialises the document before
/// consulting the shape pays for every `description` in it, and the descriptions here are the bulk
/// of the response.
#[test]
fn a_member_the_door_never_reads_costs_nothing() {
  let plain = response(0, 0);
  let verbose = response(0, 8192);
  assert!(verbose.len() > plain.len() + 100_000);
  assert_eq!(sdl(&plain), sdl(&verbose));

  let _ = sdl(&plain);
  let _ = sdl(&verbose);

  assert_eq!(
    allocated(|| drop(sdl(&plain))),
    allocated(|| drop(sdl(&verbose))),
    "the door paid for prose it does not read"
  );
}

/// Reading a response costs a fraction of the response, not a multiple of it.
#[test]
fn the_whole_door_allocates_less_than_the_response_it_read() {
  let response = response(4096, 8192);
  let rendered = sdl(&response);
  let _ = sdl(&response);
  let bytes = allocated(|| drop(sdl(&response)));

  assert!(
    bytes < response.len() as u64 / 8,
    "reading a {}-byte response allocated {bytes} bytes for {} bytes of SDL",
    response.len(),
    rendered.len()
  );
}

/// The allocator is installed, the counter moves, and it moves by what a copy costs.
///
/// Without this, every reading above could be zero because nothing was being counted.
#[test]
fn the_gate_counts() {
  let text = "y".repeat(200_000);
  let copied = allocated(|| drop(text.clone()));
  assert!(
    copied >= text.len() as u64,
    "copying {} bytes registered as {copied}",
    text.len()
  );

  // And it moves for the door too, when the door has more to emit: the same corpus with a name
  // that does reach the SDL is strictly more expensive, so a matching reading above is a fact
  // about copying rather than about the measurement.
  let small = response(0, 0);
  let big = small.replace(
    r#""name":"ok""#,
    &format!(r#""name":"ok{}""#, "w".repeat(4096)),
  );
  assert_ne!(sdl(&small), sdl(&big));
  let _ = sdl(&small);
  let _ = sdl(&big);
  assert!(
    allocated(|| drop(sdl(&big))) > allocated(|| drop(sdl(&small))),
    "a longer SDL did not cost more, so the counter is not tracking the door"
  );
}
