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

/// An escaped key inside a member the door does not read costs nothing.
///
/// A key is a *dispatch* key and comes back decoded, which is what makes `"data"` the key
/// `data` — but the walk over a member nobody reads dispatches on nothing, so it must not pay
/// for the decoding. Two responses whose unread subtree spells the same 200 keys, one plainly and
/// one entirely in `\u` escapes, describe the same schema and have to cost the same.
#[test]
fn an_escaped_key_in_an_unread_member_costs_nothing() {
  // Assembled rather than written, so nothing between this source and the fixture can normalise
  // an escape into the character it stands for.
  let escape = format!("\\u{}", "0061");
  assert!(escape.contains('\\'), "the fixture lost its escape");

  let buried = |key: &str| -> String {
    let members: Vec<String> = (0..200)
      .map(|index| format!(r#""{key}{index}":{index}"#))
      .collect();
    response(0, 0).replace(
      r#""description":"""#,
      &format!(r#""description":{{{}}}"#, members.join(",")),
    )
  };
  let plain = buried("a");
  let escaped = buried(&escape);
  assert!(
    escaped.len() > plain.len() + 1000,
    "the two corpora do not differ enough for the reading to mean anything"
  );
  assert_eq!(
    sdl(&plain),
    sdl(&escaped),
    "an escaped key changed the schema, so the two are not comparable"
  );

  let _ = sdl(&plain);
  let _ = sdl(&escaped);
  assert_eq!(
    allocated(|| drop(sdl(&plain))),
    allocated(|| drop(sdl(&escaped))),
    "the door expanded the keys of a member it never reads"
  );
}

/// A member written twice and refused twice costs one owner, not one owner per occurrence.
///
/// Last-wins reaches validation by holding a member's refusal until its object closes, because a
/// later occurrence replaces the failure along with the value. So refusals are **built and
/// discarded**, and the expensive part of building one is reading the owning object's `name` back
/// out of the response — a walk of that whole object. Paying it per occurrence makes a response
/// that repeats one bad member inside one large object cost the product of the two, which is a
/// denial of service and not a slow path.
///
/// The owner here is 64 KiB long and the response repeats an unspellable `__Field.name` a thousand
/// times, so resolving it once is a few tens of kilobytes and resolving it per occurrence is tens
/// of megabytes. Measured as allocation rather than as time because a threshold on bytes is a
/// fact and a threshold on a clock is a hope.
#[test]
fn a_refusal_a_duplicate_takes_away_costs_no_owner() {
  const OWNER: usize = 64 * 1024;

  let repeated = |duplicates: usize| -> String {
    let names = r#""name":"a b","#.repeat(duplicates);
    format!(
      r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[],"types":[
        {{"kind":"OBJECT","name":"{}","fields":[
          {{{names}"args":[],"type":{{"kind":"SCALAR","name":"Int"}}}}
        ]}}
      ]}}}}"#,
      "z".repeat(OWNER)
    )
  };

  // Every occurrence fails, so every one of them builds a refusal, and all but the last are
  // thrown away. The response is refused, and the refusal names the 64 KiB owner.
  let response = repeated(1000);
  let error = match to_sdl(&response) {
    Err(smear_schema::introspection::IntrospectionError::Response(error)) => error,
    other => panic!("expected a refusal, got {other:?}"),
  };
  assert_eq!(error.owner().map(str::len), Some(OWNER));

  let _ = to_sdl(&response);
  let bytes = allocated(|| drop(to_sdl(&response)));
  assert!(
    bytes < 4 * OWNER as u64,
    "reading 1000 refused occurrences allocated {bytes} bytes; the owner was resolved for the \
     occurrences that were discarded"
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
