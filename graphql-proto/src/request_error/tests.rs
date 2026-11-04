//! What the retained buffers hold, rather than what they report.
//!
//! `smear/tests/proto_execute.rs` watches draft §7.1.3's result from outside — the shape, the
//! refusals, and the driver values a refusal hands back — and that is where those cases belong.
//! Two properties are not visible from there, and both are allocations: [`Extensions::capacity`]
//! is crate-visible, deliberately, because it is not something a caller acts on, and the list a
//! result keeps its driver entries in is a private field. So the cases that a result does not
//! retain a spine its own ceilings never authorised — its own map's, an entry's map's, or the
//! list's — have to be written here, on the same terms as `execute/tests.rs`'s executor-side twin.
//!
//! # Under Miri
//!
//! `an_accepted_map_retains_no_capacity_the_result_never_authorised` carries
//! `#[cfg_attr(miri, ignore)]` for its kind: it is an allocation gate — how many slots a result
//! retains — and the shape of an allocation is not a question of undefined behaviour. What found it
//! was a measurement, the one `execute/tests.rs`'s header describes: alone under
//! `cargo miri test -p graphql-proto --lib -- --exact`, Stacked Borrows, aarch64-apple-darwin,
//! 2026-09-25, it had not returned after five minutes, the `GROWN_ENTRIES` quadratic inserts and
//! removes being the cost. The other tests here run: the entry-side cases grow their maps to
//! [`ENTRY_GROWN`] rather than to [`GROWN_ENTRIES`], which states the same overshoot against a
//! ceiling of four at a sixty-fourth of the quadratic cost, and the list case pushes a hundred
//! entries.

use core::num::NonZeroU32;

use super::{Head, RequestError, RequestErrorCeiling, RequestErrorResult, TooLarge};
use crate::{Ceiling, Extensions, Limits, StartError};

/// The driver value these cases carry. Nothing reads one; §7.1.7 reserves the contents entirely.
type Value = u32;

/// How many entries the lax map is grown to before it is emptied.
///
/// A power of two, so the spine's capacity lands on exactly that many slots — `Vec` doubles — and
/// the overshoot against the strict ceiling is a number rather than an argument. The same number
/// the executor-side case uses, for the same reason it is not a million: `insert` scans for a
/// duplicate on every call.
const GROWN_ENTRIES: usize = 4096;

/// How many entries an entry's own map is grown to before it is emptied.
///
/// A power of two for [`GROWN_ENTRIES`]' reason, and sixteen times the strict ceiling below, so the
/// overshoot is unmistakable; small, so that the two cases using it stay Miri subjects.
const ENTRY_GROWN: usize = 64;

/// A driver's entry carrying a map grown to [`ENTRY_GROWN`] entries under `lax` and emptied.
///
/// It reports nothing over any ceiling — no entries, no key bytes — while holding every slot it
/// grew to, which is the entry-side shape of the gap the module header names.
fn grown_and_emptied(lax: &Limits) -> RequestError<Value> {
  let mut map: Extensions<Value> = Extensions::new(lax);
  for index in 0..ENTRY_GROWN {
    map
      .insert(&std::format!("k{index}"), index as Value)
      .expect("the lax map has room");
  }
  for index in 0..ENTRY_GROWN {
    map
      .remove(&std::format!("k{index}"))
      .expect("every key was inserted");
  }
  assert!(
    map.capacity() >= ENTRY_GROWN,
    "the map did not actually grow: {} slots",
    map.capacity()
  );
  let mut entry =
    RequestError::new(lax, "grown and emptied", &[]).expect("an empty message fits anything");
  assert!(entry.set_extensions(map).is_none(), "the entry had no map");
  entry
}

/// A map grown under a lax `Limits` and emptied does not carry its allocation into a strict result.
///
/// The two ceilings [`RequestErrorResult::set_extensions`] re-checks bound what the map *reports* —
/// `len` and `key_bytes` — and the resource being bounded is what it *holds*. `remove` gives the
/// key bytes back to the ceiling and gives no slots back to the allocator, so an empty map can
/// arrive carrying a spine no [`Limits`] this result was created under ever authorised.
///
/// Read off the result's own field rather than through
/// [`take_extensions`](RequestErrorResult::take_extensions), because the claim is about what is
/// *retained*, and taking the map back is one of the two things that ends the retention.
#[cfg_attr(
  miri,
  ignore = "AN ALLOCATION GATE, AND NOT A MIRI SUBJECT. What is asserted below is how many entry \
            slots a result retains after accepting a map grown under laxer limits, which is the \
            shape of an allocation, re-derived by an interpreter out of the same MIR, and not a \
            question of undefined behaviour. Its fixture is `GROWN_ENTRIES` inserts and removes \
            into a map whose `insert` scans for a duplicate, quadratic by design; its \
            executor-side twin in `execute/tests.rs` is skipped alike. Found by the five-minute \
            detector's measurement: over five minutes under `cargo miri test` on \
            aarch64-apple-darwin (Stacked Borrows, 2026-09-25). The file's header carries the \
            measurement. Declared in `ci/miri_scope.py`'s ignore table, which is what stops this \
            from being a coverage cut nobody chose."
)]
#[test]
fn an_accepted_map_retains_no_capacity_the_result_never_authorised() {
  let lax = Limits {
    max_extension_entries: NonZeroU32::new(GROWN_ENTRIES as u32).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(1 << 20).expect("not zero"),
    ..Limits::default()
  };
  // The key-byte ceiling is deliberately *above* the capacity being planted, and not a small
  // number chosen to look strict. There are two ceilings a repair could read here and only one of
  // them bounds entry slots; with the other one set low, a repair reading it would shrink anyway
  // and this case would pass a bound it never checked.
  let strict = Limits {
    max_extension_entries: NonZeroU32::new(4).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(GROWN_ENTRIES as u32 * 2).expect("not zero"),
    ..Limits::default()
  };

  let mut map: Extensions<Value> = Extensions::new(&lax);
  for index in 0..GROWN_ENTRIES {
    map
      .insert(&std::format!("k{index}"), index as Value)
      .expect("the lax map has room");
  }
  let grown = map.capacity();
  assert!(
    grown >= GROWN_ENTRIES,
    "the map did not actually grow: {grown} slots"
  );
  for index in 0..GROWN_ENTRIES {
    map
      .remove(&std::format!("k{index}"))
      .expect("every key was inserted");
  }
  assert_eq!(map.len(), 0, "empty by every ceiling this result reads");
  assert_eq!(map.key_bytes(), 0, "and by the other one");
  assert_eq!(
    map.capacity(),
    grown,
    "and still holding every slot: `remove` refunds the budget and never the allocation, which is \
     the whole gap this case is about"
  );

  let mut result: RequestErrorResult<Value> =
    RequestErrorResult::new(&strict, StartError::AmbiguousOperation);
  result
    .set_extensions(map)
    .expect("nothing it reports is over a ceiling");

  let retained = result
    .extensions
    .as_ref()
    .expect("the map was accepted")
    .capacity();
  let slot = core::mem::size_of::<(std::boxed::Box<str>, Value)>();
  assert!(
    retained as u64 <= u64::from(strict.max_extension_entries.get()),
    "the result is holding {retained} entry slots ({} bytes) under a ceiling of {}, grown to \
     {grown} under a `Limits` it never agreed to",
    retained * slot,
    strict.max_extension_entries.get()
  );
}

/// The map a refusal hands back is the map that was refused, allocation included.
///
/// The mirror of the case above and the reason `shrink_to_ceiling` runs *after* the checks: a
/// caller repairing a refusal keeps working with its own map, and a refusal that reallocated it on
/// the way out would have spent the caller's time on a value this result never kept.
#[test]
fn a_refused_map_is_handed_back_without_being_reallocated() {
  let lax = Limits {
    max_extension_entries: NonZeroU32::new(64).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(1 << 20).expect("not zero"),
    ..Limits::default()
  };
  let strict = Limits {
    max_extension_entries: NonZeroU32::new(4).expect("not zero"),
    max_extension_key_bytes: NonZeroU32::new(1 << 20).expect("not zero"),
    ..Limits::default()
  };

  // Grown to sixteen and emptied back to eight, so the map is refused (eight entries against a
  // ceiling of four) **and** its spine is strictly larger than its entries. Both halves are load
  // bearing: with capacity equal to length, a `shrink_to_fit` moved onto this path would return
  // the same number and the assertion below would pass over the very defect it is written for.
  let mut map: Extensions<Value> = Extensions::new(&lax);
  for index in 0..16 {
    map
      .insert(&std::format!("k{index}"), index as Value)
      .expect("the lax map has room");
  }
  for index in 0..8 {
    map
      .remove(&std::format!("k{index}"))
      .expect("every key was inserted");
  }
  let before = map.capacity();
  assert!(
    before as u64 > u64::from(strict.max_extension_entries.get()),
    "the refused map has to be holding more than the strict ceiling, or the assertion below \
     cannot tell a preserved allocation from a shrunk one"
  );
  assert!(
    before > map.len(),
    "and strictly more slots than entries, or a re-derivation on this path would be a no-op and \
     the assertion below would pass whether or not one happened"
  );

  let mut result: RequestErrorResult<Value> =
    RequestErrorResult::new(&strict, StartError::UnknownOperation);
  let refused: TooLarge<Value> = result
    .set_extensions(map)
    .expect_err("eight entries under a ceiling of four");
  assert_eq!(refused.ceiling(), Ceiling::Entries);

  let back = refused.into_extensions();
  assert_eq!(back.len(), 8, "every entry survived the refusal");
  assert_eq!(
    back.capacity(),
    before,
    "and so did the allocation: the normalization belongs to acceptance, not to refusal"
  );
  assert!(
    result.extensions().is_none(),
    "and nothing was retained by the result that refused it"
  );
}

/// An entry's map grown under a lax `Limits` and emptied does not carry its allocation into a strict
/// result, through either door that lists an entry.
///
/// The entry-side twin of the first case: [`RequestErrorResult::from_driver`] and
/// [`RequestErrorResult::push`] re-check the two quantities the entry's map reports, and the
/// resource being bounded is what it holds. Read off the result's own fields, because the claim is
/// about what is *retained*.
#[test]
fn an_accepted_entrys_map_retains_no_capacity_the_result_never_authorised() {
  let lax = Limits {
    max_extension_entries: NonZeroU32::new(ENTRY_GROWN as u32).expect("not zero"),
    ..Limits::default()
  };
  // The key-byte ceiling is left at its default, which is well above anything planted here, for the
  // reason the first case gives: only one of the two ceilings bounds entry slots.
  let strict = Limits {
    max_extension_entries: NonZeroU32::new(4).expect("not zero"),
    ..Limits::default()
  };
  let ceiling = u64::from(strict.max_extension_entries.get());

  let mut result = RequestErrorResult::from_driver(&strict, grown_and_emptied(&lax))
    .expect("nothing the entry reports is over a ceiling");
  let Head::Driver(first) = &result.head else {
    panic!("a result built from a driver's entry starts with it")
  };
  let held = first
    .extensions
    .as_ref()
    .expect("the map was accepted with its entry")
    .capacity();
  assert!(
    held as u64 <= ceiling,
    "`from_driver` kept an entry whose map holds {held} slots under a ceiling of {ceiling}"
  );

  result
    .push(grown_and_emptied(&lax))
    .expect("nothing the entry reports is over a ceiling");
  let held = result.rest[0]
    .extensions
    .as_ref()
    .expect("the map was accepted with its entry")
    .capacity();
  assert!(
    held as u64 <= ceiling,
    "`push` kept an entry whose map holds {held} slots under a ceiling of {ceiling}"
  );
}

/// The entry a refusal hands back is the entry that was refused, its map's allocation included.
///
/// The mirror of the case above, at both doors, and the reason the map's spine is re-derived only
/// once every check has passed.
#[test]
fn a_refused_entrys_map_is_handed_back_without_being_reallocated() {
  let lax = Limits {
    max_extension_entries: NonZeroU32::new(64).expect("not zero"),
    ..Limits::default()
  };
  let strict = Limits {
    max_extension_entries: NonZeroU32::new(4).expect("not zero"),
    ..Limits::default()
  };

  // Grown to sixteen and emptied back to eight, for the reason the map-side case gives: refused
  // (eight entries against a ceiling of four), and holding strictly more slots than entries.
  let mut map: Extensions<Value> = Extensions::new(&lax);
  for index in 0..16 {
    map
      .insert(&std::format!("k{index}"), index as Value)
      .expect("the lax map has room");
  }
  for index in 0..8 {
    map
      .remove(&std::format!("k{index}"))
      .expect("every key was inserted");
  }
  let before = map.capacity();
  assert!(
    before as u64 > u64::from(strict.max_extension_entries.get()) && before > map.len(),
    "the planted map has to be over the strict ceiling and holding more slots than entries, or the \
     assertions below cannot tell a preserved allocation from a shrunk one"
  );
  let mut entry = RequestError::new(&lax, "over the strict map ceiling", &[])
    .expect("the message and the spans fit");
  assert!(entry.set_extensions(map).is_none(), "the entry had no map");

  let refused = RequestErrorResult::from_driver(&strict, entry)
    .expect_err("eight entries under a ceiling of four");
  assert_eq!(
    refused.ceiling(),
    RequestErrorCeiling::Extensions(Ceiling::Entries)
  );
  let entry = refused.into_error();
  assert_eq!(
    entry
      .extensions()
      .expect("the map came back with its entry")
      .capacity(),
    before,
    "`from_driver` reallocated a map it was not going to keep"
  );

  let first = RequestError::new(&strict, "listed first", &[]).expect("it fits");
  let mut result = RequestErrorResult::from_driver(&strict, first).expect("it fits");
  let refused = result
    .push(entry)
    .expect_err("eight entries under a ceiling of four");
  assert_eq!(
    refused.ceiling(),
    RequestErrorCeiling::Extensions(Ceiling::Entries)
  );
  let back = refused.into_error();
  let map = back.extensions().expect("the map came back with its entry");
  assert_eq!(map.len(), 8, "every entry survived both refusals");
  assert_eq!(
    map.capacity(),
    before,
    "`push` reallocated a map it was not going to keep"
  );
  assert!(result.rest.is_empty(), "and nothing was retained");
}

/// The list a result keeps its driver entries in never holds a slot the count ceiling refuses.
///
/// `Vec` grows by doubling, so a list filled to a ceiling that is not a power of two would hold up
/// to twice what the ceiling admits. A hundred is the case: plain doubling reaches a hundred and
/// twenty-eight slots for the ninety-nine entries after the head.
#[test]
fn the_list_spine_holds_no_slot_the_count_ceiling_refuses() {
  let limits = Limits {
    max_request_errors: NonZeroU32::new(100).expect("not zero"),
    ..Limits::default()
  };
  let entry = || RequestError::<Value>::new(&limits, "listed", &[]).expect("it fits");

  let mut result = RequestErrorResult::from_driver(&limits, entry()).expect("it fits");
  for _ in 1..100 {
    result.push(entry()).expect("room below the ceiling");
  }
  assert_eq!(result.errors().len(), 100, "the head and ninety-nine more");

  let held = result.rest.capacity();
  assert!(
    held <= 99,
    "the list holds {held} slots for the ninety-nine entries a ceiling of a hundred leaves it"
  );

  let refused = result.push(entry()).expect_err("a hundred is the ceiling");
  assert_eq!(refused.ceiling(), RequestErrorCeiling::Errors);
  assert_eq!(
    result.rest.capacity(),
    held,
    "and a refusal does not grow it"
  );
}
