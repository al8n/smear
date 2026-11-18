//! What the map keeps in order, and what it measures of itself.
//!
//! The retainers' own cases — a map grown under laxer limits, emptied and handed to an executor or
//! a request error result — live beside the retainers, in `execute/tests.rs` and
//! `request/error/tests.rs`. These are the container's: the order a serialiser writes, which a
//! hashed map keeps only because insertion order is what it iterates, and the high-water mark a
//! clone has to carry.

use core::num::NonZeroU32;

use super::{Ceiling, Extensions};
use crate::{Limits, RequestErrorResult, StartError};

/// The driver value these cases carry. Nothing reads one; §7.1.7 reserves the contents entirely.
type Value = u32;

/// A map holding `entries`, inserted in the order given, under the default ceilings.
fn map(entries: &[(&str, Value)]) -> Extensions<Value> {
  let mut map = Extensions::new(&Limits::default());
  for &(key, value) in entries {
    assert_eq!(
      map.insert(key, value).expect("under the default ceilings"),
      None,
      "`{key}` is new"
    );
  }
  map
}

/// The keys, in the order the map iterates them.
fn keys(map: &Extensions<Value>) -> std::vec::Vec<&str> {
  map.iter().map(|(key, _)| key).collect()
}

/// Two maps holding the same entries are equal only when they were inserted in the same order.
///
/// The order is part of what a serialiser writes, so a comparison that ignored it would call two
/// different responses the same one — which is what the map's own `==` would do here.
#[test]
fn the_same_entries_in_another_order_are_another_map() {
  let forwards = map(&[("a", 1), ("b", 2), ("c", 3)]);
  let backwards = map(&[("c", 3), ("b", 2), ("a", 1)]);
  assert_ne!(
    forwards, backwards,
    "the same three entries, inserted in two orders, serialise as two different maps"
  );
  assert_eq!(
    forwards,
    map(&[("a", 1), ("b", 2), ("c", 3)]),
    "and inserted in the same order they are the same map"
  );
}

/// A removal leaves the rest of the entries where they were, and a key inserted again goes to the
/// end.
#[test]
fn a_removal_keeps_the_rest_in_insertion_order() {
  let mut map = map(&[("a", 1), ("b", 2), ("c", 3), ("d", 4)]);
  assert_eq!(map.remove("b"), Some(2));
  assert_eq!(keys(&map), ["a", "c", "d"]);
  assert_eq!(
    map.key_bytes(),
    3,
    "the removed key's byte went back to the ceiling"
  );
  assert_eq!(map.remove("b"), None, "and the key is gone");

  map.insert("b", 5).expect("room again");
  assert_eq!(
    keys(&map),
    ["a", "c", "d", "b"],
    "a key inserted again is a new entry, so it goes last"
  );
}

/// A repeated key replaces its value in place: same position, the old value handed back, and no
/// charge against either ceiling — even on a map that is full.
#[test]
fn a_repeated_key_keeps_its_place_and_hands_the_old_value_back() {
  let full = Limits::default().with_max_extension_entries(NonZeroU32::new(3).expect("not zero"));
  let mut map = Extensions::new(&full);
  for (key, value) in [("a", 1), ("b", 2), ("c", 3)] {
    map
      .insert(key, value)
      .expect("three entries fit three slots");
  }
  let bytes = map.key_bytes();

  let refused = map
    .insert("d", 4)
    .expect_err("a fourth key is over the ceiling");
  assert_eq!(refused.ceiling(), Ceiling::Entries);
  assert_eq!(refused.into_value(), 4, "and the value comes back");

  assert_eq!(
    map
      .insert("b", 20)
      .expect("a replacement takes no slot, so a full map still accepts it"),
    Some(2)
  );
  assert_eq!(keys(&map), ["a", "b", "c"], "`b` kept its place");
  assert_eq!(map.get("b"), Some(&20));
  assert_eq!(map.len(), 3);
  assert_eq!(map.key_bytes(), bytes, "and nothing was charged");
}

/// [`ExtensionsIter`](super::ExtensionsIter) is the same walk as a `for` loop over `&Extensions`,
/// from either end, and counts down as it goes.
#[test]
fn the_iterator_agrees_with_iteration_from_both_ends() {
  let map = map(&[("a", 1), ("b", 2), ("c", 3)]);

  let mut looped = std::vec::Vec::new();
  for (key, value) in &map {
    looped.push((key, *value));
  }
  let iterated: std::vec::Vec<_> = map.iter().map(|(key, value)| (key, *value)).collect();
  assert_eq!(looped, iterated, "`&Extensions` iterates as `iter` does");
  assert_eq!(iterated, [("a", 1), ("b", 2), ("c", 3)]);

  let reversed: std::vec::Vec<_> = map.iter().rev().map(|(key, value)| (key, *value)).collect();
  assert_eq!(reversed, [("c", 3), ("b", 2), ("a", 1)]);

  let mut entries = map.iter();
  assert_eq!(entries.len(), 3);
  assert_eq!(entries.next(), Some(("a", &1)));
  assert_eq!(entries.next_back(), Some(("c", &3)));
  assert_eq!(entries.len(), 1, "one from each end");
  assert_eq!(
    std::format!("{entries:?}"),
    r#"["b"]"#,
    "`Debug` writes the keys still to come, and asks nothing of the values"
  );
  assert_eq!(entries.next(), Some(("b", &2)));
  assert_eq!(entries.len(), 0);
  assert_eq!(entries.next(), None);
  assert_eq!(entries.next_back(), None, "exhausted from either end");
  assert_eq!(entries.next(), None, "and it stays exhausted");
}

/// The clone of a map grown and emptied is re-derived on its way into a retainer, as its source
/// would be.
///
/// `IndexMap`'s clone keeps the source's hash index at full size and gives the entry vector no
/// room, so the map's own capacity reads nothing on the clone while its index is as large as it
/// ever was. The high-water mark is what [`Extensions::shrink_to_ceiling`] reads, and the
/// hand-written `Clone` copies it; a `Clone` that rebuilt the mark from the clone's length would
/// let the clone keep the index past every ceiling.
#[test]
fn a_clone_of_a_grown_and_emptied_map_is_re_derived_on_the_way_in() {
  /// Sixteen times the strict ceiling, and small, so the case stays a Miri subject.
  const GROWN: usize = 64;

  let lax =
    Limits::default().with_max_extension_entries(NonZeroU32::new(GROWN as u32).expect("not zero"));
  let strict = Limits::default().with_max_extension_entries(NonZeroU32::new(4).expect("not zero"));

  let mut map: Extensions<Value> = Extensions::new(&lax);
  for index in 0..GROWN {
    map
      .insert(&std::format!("k{index}"), index as Value)
      .expect("the lax map has room");
  }
  for index in 0..GROWN {
    map
      .remove(&std::format!("k{index}"))
      .expect("every key was inserted");
  }

  let clone = map.clone();
  assert!(
    clone.is_empty(),
    "the clone holds the source's entries, none"
  );
  assert_eq!(
    clone.capacity(),
    GROWN,
    "and the source's high-water mark, because it holds the source's hash index"
  );

  let mut result: RequestErrorResult<Value> =
    RequestErrorResult::new(&strict, StartError::AmbiguousOperation);
  result
    .set_extensions(clone)
    .expect("nothing the clone reports is over a ceiling");
  let retained = result
    .extensions()
    .expect("the clone was accepted")
    .capacity();
  assert!(
    retained as u64 <= u64::from(strict.max_extension_entries.get()),
    "the result kept a clone holding room for {retained} entries under a ceiling of {}",
    strict.max_extension_entries.get()
  );
}
