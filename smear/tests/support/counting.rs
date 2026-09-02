//! A counting global allocator, and the two windows that read it.
//!
//! # Why this is a file and not an inline module
//!
//! It was `tests/validator_allocation.rs`'s, written inline, and it moved here unchanged when
//! `benches/solo/peak_alloc.rs` needed the same instrument. A second copy is the failure the move
//! exists to prevent: the test pins a claim about *events* (`Schema::build` allocates at most `N`
//! times) and the gate pins one about *bytes*, and two copies would let the two answer from
//! different definitions of what an allocation is. One `realloc` accounted two ways is two
//! instruments that disagree about a repair while both report green.
//!
//! What the move costs, stated because it is not free: a reader of `validator_allocation.rs` now
//! opens one more file to see what its numbers mean, and an edit here changes a `cargo test` gate
//! and a `cargo bench` gate at once. That second half is the point rather than the price — the
//! instrument is shared *so that* it cannot be changed for one reader only — but it does mean this
//! file is load-bearing for two targets and its own header is the only place that says so.
//!
//! Reached by `#[path]` from both, because cargo compiles neither `tests/support/` nor
//! `benches/solo/perf/` into a library: this is source included into two binaries, so each gets
//! its own `#[global_allocator]` and neither can perturb the other.
//!
//! # What it measures, and the one thing it does not
//!
//! [`ALLOCATIONS`], [`LIVE`] and [`PEAK`] are **thread-local**, so a test running beside this one
//! on another thread cannot perturb a reading. The cost is the matching bound: a subject that
//! allocates on one thread and frees on another is not measured correctly by this, and every
//! caller in this repository is single-threaded inside its window for that reason.

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
};

thread_local! {
  /// Allocation events on this thread. `alloc`, `alloc_zeroed` and a growing `realloc` all count.
  pub static ALLOCATIONS: Cell<u64> = const { Cell::new(0) };
  /// Bytes this thread holds from the allocator right now.
  pub static LIVE: Cell<usize> = const { Cell::new(0) };
  /// The high-water mark of [`LIVE`] since it was last armed.
  pub static PEAK: Cell<usize> = const { Cell::new(0) };
}

pub struct Counting;

/// Counts every allocation event, tracks the live and peak byte figures, and forwards to the
/// system allocator.
///
/// The counters are thread-local and updated through `try_with`, so an allocation made while the
/// thread's local storage is being set up or torn down is simply not counted rather than
/// re-entering it.
///
/// # Two instruments, because they answer different questions
///
/// The event count is what pins "the steady state allocates nothing" — a claim about *whether* the
/// allocator is asked at all. The byte figure is what pins an **amplification**: a reduction that
/// makes one allocation per element passes an event count no matter how big each one is, so a copy
/// whose size the caller chose is invisible to it.
unsafe impl GlobalAlloc for Counting {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    bump();
    grew(layout.size());
    unsafe { System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    bump();
    grew(layout.size());
    unsafe { System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    bump();
    if new_size >= layout.size() {
      grew(new_size - layout.size());
    } else {
      shrank(layout.size() - new_size);
    }
    unsafe { System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    shrank(layout.size());
    unsafe { System.dealloc(ptr, layout) }
  }
}

pub fn bump() {
  let _ = ALLOCATIONS.try_with(|count| count.set(count.get() + 1));
}

pub fn grew(bytes: usize) {
  let _ = LIVE.try_with(|live| {
    let now = live.get() + bytes;
    live.set(now);
    let _ = PEAK.try_with(|peak| {
      if now > peak.get() {
        peak.set(now);
      }
    });
  });
}

pub fn shrank(bytes: usize) {
  let _ = LIVE.try_with(|live| live.set(live.get().saturating_sub(bytes)));
}

/// Runs `body` and returns how many allocation events it caused on this thread.
#[allow(dead_code)]
pub fn allocations(body: impl FnOnce()) -> u64 {
  let before = ALLOCATIONS.with(Cell::get);
  body();
  ALLOCATIONS.with(Cell::get) - before
}

/// Runs `body` and returns the highest live-byte figure this thread reached inside it, measured
/// from what was already live when it started.
///
/// The subject's own inputs are therefore *below* the window — they are live before it opens — and
/// what comes back is what the body itself added at its worst moment.
#[allow(dead_code)]
pub fn peak_bytes(body: impl FnOnce()) -> usize {
  let before = LIVE.with(Cell::get);
  PEAK.with(|peak| peak.set(before));
  body();
  PEAK.with(Cell::get) - before
}

/// Installed by every target that includes this file, so no caller has to remember to.
///
/// A `#[global_allocator]` is per-BINARY, and this file is compiled separately into each one that
/// `#[path]`s it in — so the test binary and the bench binary each install their own and neither
/// is visible to the other.
#[global_allocator]
static ALLOCATOR: Counting = Counting;
