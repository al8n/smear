//! What the retained map holds, rather than what it reports.
//!
//! `smear/tests/proto_execute.rs` watches draft §7.1.3's result from outside — the shape, the
//! refusals, and the driver values a refusal hands back — and that is where those cases belong.
//! One property is not visible from there: [`Extensions::capacity`] is crate-visible, deliberately,
//! because it is not something a caller acts on. So the case that a result does not retain a spine
//! its own ceilings never authorised has to be written here, on the same terms as
//! `execute/tests.rs`'s executor-side twin.

use core::num::NonZeroU32;

use super::{RequestErrorResult, TooLarge};
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
