//! Releasing a value does not spend a native frame per level of nesting, and does not leak.
//!
//! # What used to happen, and why a ceiling did not stop it
//!
//! The `Drop` glue the compiler generates for a value tree descends one native frame per level.
//! Measured on `aarch64-apple-darwin` before the repair, unoptimised: 175 bytes of frame per
//! level, so a value 11 967 lists deep released on libtest's default 2 MiB test thread and 11 968
//! aborted the process with `SIGABRT`. The parser's own nesting ceiling does not reach this — a
//! value does not have to come from a parser. It is what a `graphql_proto::Values` driver
//! resolved, or what a draft §7.1.7 `extensions` map was handed, and every constructor it needs
//! is public.
//!
//! # Neither reading here can abort the harness, and that is deliberate
//!
//! The deep fixture is **1 024** levels, which the *recursive* release survived with room to
//! spare — about 180 KiB of the smallest stack this suite runs on. So a regression to recursing
//! does not overflow anything: it moves the number the assertion reads, and the test goes red
//! like any other. A fixture deep enough to abort would be a fixture that reports a regression by
//! killing the run, which is the failure this file is about rather than a way to detect it.
//!
//! `smear/tests/json_writer.rs` holds the other half — three 20 000-deep fixtures released
//! end-to-end through the executor and the writer — where the depth *is* past the boundary and a
//! regression is loud rather than legible. Between them, both readings exist.
//!
//! # The probe is the deallocator, because a release has no other callback
//!
//! There is nowhere to put an instrument inside a `Drop`: it takes no argument and returns
//! nothing. But freeing a level's container is an allocator event, and the allocator sees the
//! native stack it was called on. So the frame depth is read from the lowest address a `dealloc`
//! is made from, against an anchor in this frame — the same difference-of-two-addresses shape as
//! `smear::json`'s writer probe, with the sink's callback replaced by the one call a release
//! always makes.
//!
//! The same instrument answers the second question. A worklist that dropped a subtree on the
//! floor would be worse than the abort it replaced, so every reading below also carries the
//! outstanding-bytes balance across build **and** release, and asserts it returns to zero.

#![cfg(all(feature = "graphql", feature = "materialized-numbers", feature = "std"))]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
  ptr,
};

use smear_parser::graphql::ast::{
  ConstInputValue, ConstList, ConstObject, ConstObjectField, InputValue, List, Name, Object,
  ObjectField,
  materialized::{
    ConstInputValue as MConstInputValue, ConstList as MConstList, ConstObject as MConstObject,
    ConstObjectField as MConstObjectField, InputValue as MInputValue, List as MList,
    Object as MObject, ObjectField as MObjectField,
  },
};
use tokora::SimpleSpan;

thread_local! {
  /// Whether the probe is armed on this thread. Off for every other harness thread, so a
  /// concurrent test cannot move these readings.
  static PROBING: Cell<bool> = const { Cell::new(false) };
  /// The lowest native-stack address a `dealloc` has been made from while armed.
  static DEEPEST: Cell<usize> = const { Cell::new(usize::MAX) };
  /// Bytes requested minus bytes released, over the whole measurement.
  static LIVE: Cell<isize> = const { Cell::new(0) };
  /// How many `dealloc` calls the release made, so a fixture that is not the shape it claims is
  /// caught rather than measured.
  static FREES: Cell<usize> = const { Cell::new(0) };
}

/// A pass-through allocator that records where on the native stack a release is running.
struct Probe;

#[allow(unsafe_code)]
unsafe impl GlobalAlloc for Probe {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    charge(layout.size() as isize);
    unsafe { System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    charge(layout.size() as isize);
    unsafe { System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    charge(new_size as isize - layout.size() as isize);
    unsafe { System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    charge(-(layout.size() as isize));
    mark_frame();
    unsafe { System.dealloc(ptr, layout) }
  }
}

#[global_allocator]
static ALLOCATOR: Probe = Probe;

#[inline]
fn charge(delta: isize) {
  let _ = PROBING.try_with(|probing| {
    if probing.get() {
      let _ = LIVE.try_with(|live| live.set(live.get().wrapping_add(delta)));
    }
  });
}

/// Records the caller's position on the native stack, from a local in this frame.
#[inline]
fn mark_frame() {
  let here = 0u8;
  let address = ptr::addr_of!(here) as usize;
  let _ = PROBING.try_with(|probing| {
    if probing.get() {
      let _ = FREES.try_with(|frees| frees.set(frees.get().wrapping_add(1)));
      let _ = DEEPEST.try_with(|deepest| deepest.set(deepest.get().min(address)));
    }
  });
}

/// One reading: the native stack the release descended through, the frees it made, and whether
/// the heap came back to where it started.
struct Reading {
  stack: usize,
  frees: usize,
  live: isize,
}

/// Builds a value with `build`, releases it, and reports what the release cost.
///
/// The measurement spans the build as well, because [`Reading::live`] is the leak question and a
/// release that forgot a subtree would balance only against what was allocated to make it.
fn release<T>(build: impl FnOnce() -> T) -> Reading {
  let anchor = 0u8;
  let base = ptr::addr_of!(anchor) as usize;

  LIVE.with(|live| live.set(0));
  FREES.with(|frees| frees.set(0));
  DEEPEST.with(|deepest| deepest.set(usize::MAX));
  PROBING.with(|probing| probing.set(true));

  let value = build();
  drop(value);

  PROBING.with(|probing| probing.set(false));
  Reading {
    stack: base.saturating_sub(DEEPEST.with(Cell::get)),
    frees: FREES.with(Cell::get),
    live: LIVE.with(Cell::get),
  }
}

/// The shallower of the two depths every reading is taken at.
const SHALLOW: usize = 64;

/// The deeper one, sixteen times further down. Past the recursive release's cost per level by a
/// wide margin and nowhere near its boundary, which is what makes a regression legible instead of
/// fatal — see the module header.
const DEEP: usize = 1_024;

/// What a difference of two depths may still contain: the constant part of the release's own
/// frame, which cancels, plus whatever the platform puts between two calls at the same depth.
///
/// A release that recursed would put 175 bytes per level in it — about 168 KiB across these two
/// depths, which is 164x this.
const SLACK: usize = 1_024;

/// A span, for fixtures that are pure containers and never hold a source slice.
fn span() -> SimpleSpan {
  SimpleSpan::new(0, 0)
}

/// Asserts that a builder's release is flat in the depth and balances its allocations.
///
/// `build` takes a depth and returns a value nested that many levels.
fn assert_release_is_flat<T>(what: &str, build: impl Fn(usize) -> T) {
  let shallow = release(|| build(SHALLOW));
  let deep = release(|| build(DEEP));

  // The fixtures really are the depths they claim: one container freed per level, and the deep
  // one has sixteen times as many of them.
  assert!(
    shallow.frees >= SHALLOW && deep.frees >= DEEP,
    "{what}: {} and {} frees for {SHALLOW} and {DEEP} levels, so the fixture is not the shape \
     this reading is about",
    shallow.frees,
    deep.frees
  );

  // Nothing was dropped on the floor. This is the half that a worklist gets wrong: an iterative
  // release that forgets a subtree is worse than the abort it replaced.
  assert_eq!(
    shallow.live, 0,
    "{what}: {} bytes outstanding after releasing a {SHALLOW}-deep value",
    shallow.live
  );
  assert_eq!(
    deep.live, 0,
    "{what}: {} bytes outstanding after releasing a {DEEP}-deep value",
    deep.live
  );

  // And the reading this file exists for: sixteen times the nesting costs the same native stack.
  let extra = deep.stack.saturating_sub(shallow.stack);
  assert!(
    extra < SLACK,
    "{what}: {DEEP} levels used {extra} more bytes of native stack than {SHALLOW} did, which is \
     past the {SLACK} a constant-frame release may drift by. A release that recursed would show \
     roughly {}.",
    175 * (DEEP - SHALLOW)
  );
}

/// A field name, for the object levels. The spelling never leaves the fixture.
fn name() -> Name<&'static str> {
  Name::new(span(), "at")
}

/// Every fixture below **alternates** its dialect's nesting carriers rather than repeating one,
/// so an arm that was left out of `unnest` cannot hide behind the sibling above it: a chain of
/// pure lists would be released flat by an implementation that knew only about lists.
#[test]
fn the_materialised_constant_tree_releases_flat() {
  assert_release_is_flat("materialized::ConstInputValue", |depth| {
    let mut value = MConstInputValue::<&str, i64>::List(MConstList::new(span(), Vec::new()));
    for level in 0..depth {
      value = if level % 2 == 0 {
        MConstInputValue::List(MConstList::new(span(), std::vec![value]))
      } else {
        MConstInputValue::Object(MConstObject::new(
          span(),
          std::vec![MConstObjectField::new(span(), name(), value)],
        ))
      };
    }
    value
  });
}

#[test]
fn the_materialised_executable_tree_releases_flat() {
  assert_release_is_flat("materialized::InputValue", |depth| {
    let mut value = MInputValue::<&str, i64>::List(MList::new(span(), Vec::new()));
    for level in 0..depth {
      value = if level % 2 == 0 {
        MInputValue::List(MList::new(span(), std::vec![value]))
      } else {
        MInputValue::Object(MObject::new(
          span(),
          std::vec![MObjectField::new(span(), name(), value)],
        ))
      };
    }
    value
  });
}

#[test]
fn the_slice_constant_tree_releases_flat() {
  assert_release_is_flat("ast::ConstInputValue", |depth| {
    let mut value = ConstInputValue::<&str>::List(ConstList::new(span(), Vec::new()));
    for level in 0..depth {
      value = if level % 2 == 0 {
        ConstInputValue::List(ConstList::new(span(), std::vec![value]))
      } else {
        ConstInputValue::Object(ConstObject::new(
          span(),
          std::vec![ConstObjectField::new(span(), name(), value)],
        ))
      };
    }
    value
  });
}

#[test]
fn the_slice_executable_tree_releases_flat() {
  assert_release_is_flat("ast::InputValue", |depth| {
    let mut value = InputValue::<&str>::List(List::new(span(), Vec::new()));
    for level in 0..depth {
      value = if level % 2 == 0 {
        InputValue::List(List::new(span(), std::vec![value]))
      } else {
        InputValue::Object(Object::new(
          span(),
          std::vec![ObjectField::new(span(), name(), value)],
        ))
      };
    }
    value
  });
}

/// GraphQLx nests through four carriers rather than two, and its map nests through **both** halves
/// of an entry — so a chain that alternates all four is the one fixture where a missed arm cannot
/// hide behind a sibling.
#[cfg(feature = "graphqlx")]
mod graphqlx {
  use super::{assert_release_is_flat, span};
  use smear_parser::graphqlx::ast::{
    ConstInputValue, ConstList, ConstMap, ConstMapEntry, ConstObject, ConstObjectField, ConstSet,
    InputValue, List, Map, MapEntry, Name, Object, ObjectField, Set,
  };

  fn name() -> Name<&'static str> {
    Name::new(span(), "at")
  }

  #[test]
  fn the_extended_constant_tree_releases_flat_through_every_carrier() {
    assert_release_is_flat("graphqlx::ConstInputValue", |depth| {
      let mut value = ConstInputValue::<&str>::List(ConstList::new(span(), Vec::new()));
      for level in 0..depth {
        value = match level % 4 {
          0 => ConstInputValue::List(ConstList::new(span(), std::vec![value])),
          1 => ConstInputValue::Set(ConstSet::new(span(), std::vec![value])),
          2 => ConstInputValue::Object(ConstObject::new(
            span(),
            std::vec![ConstObjectField::new(span(), name(), value)],
          )),
          // The key is the chain and the value is a leaf on this level, so the entry's *key* slot
          // carries the nesting — the one recursion that a `map` arm reading only values misses.
          _ => ConstInputValue::Map(ConstMap::new(
            span(),
            std::vec![ConstMapEntry::new(
              span(),
              value,
              ConstInputValue::List(ConstList::new(span(), Vec::new())),
            )],
          )),
        };
      }
      value
    });
  }

  #[test]
  fn the_extended_executable_tree_releases_flat_through_every_carrier() {
    assert_release_is_flat("graphqlx::InputValue", |depth| {
      let mut value = InputValue::<&str>::List(List::new(span(), Vec::new()));
      for level in 0..depth {
        value = match level % 4 {
          0 => InputValue::List(List::new(span(), std::vec![value])),
          1 => InputValue::Set(Set::new(span(), std::vec![value])),
          2 => InputValue::Object(Object::new(
            span(),
            std::vec![ObjectField::new(span(), name(), value)],
          )),
          _ => InputValue::Map(Map::new(
            span(),
            std::vec![MapEntry::new(
              span(),
              value,
              InputValue::List(List::new(span(), Vec::new())),
            )],
          )),
        };
      }
      value
    });
  }
}
