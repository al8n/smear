//! Projecting a nested document does not spend a native frame per level of nesting.
//!
//! # The defect this exists to catch coming back
//!
//! The grammar is bounded above a value, and four cycles below one are not: `value` ↔
//! `object_field`, `const_value` ↔ `const_object_field`, `selection_set` ↔
//! `field`/`inline_fragment`, and `ty` ↔ `list_element`/`non_null_type`. Each spent one native
//! frame per level of nesting, with no counter of its own, and no ceiling reachable from
//! `lossless::project` closed it: cutting `MAX_GREEN_DEPTH` under what the doors produce refuses a
//! parse this crate just made.
//!
//! So at the top of the lexer's `HARD_MAX` the doors produced exactly the tree the dispatch could
//! not descend. Measured on `aarch64-apple-darwin`, unoptimised, on the 2 MiB thread
//! `std::thread::spawn` and the libtest harness each give: `scalar Foo @x(a: {a: … 1 … })` at 253
//! brackets and 514 green levels projected, and at 254 brackets and 516 levels aborted the process
//! with `SIGABRT` — 4 080 bytes of frame per green level. 254 and 255 parse clean and 256 is
//! refused, so the window was two bracket counts wide. al8n/smear#201.
//!
//! # Why the reading is a stack address and not a thread that is too small
//!
//! The property is *the projection returns at `HARD_MAX` on a small stack*, and a test that spawns
//! a small thread to check it reports a regression by **aborting the harness**: a stack overflow is
//! `SIGABRT`, which no `#[should_panic]` sees and which takes every other test in the binary with
//! it. That is the failure this file is about rather than a way to detect it.
//!
//! So the depth is read the way `deep_value_release.rs` reads a release's: the projection
//! allocates — every list, object and selection set it builds is a `Vec` — and the allocator sees
//! the native stack it was called on. The reading is the difference between the lowest address an
//! allocation was made from and an anchor in this frame, taken at two depths. A walk that recursed
//! puts one frame per level in that difference; a walk that does not puts nothing.
//!
//! Both fixtures are also chosen so a regression is **legible rather than fatal**: the deeper one
//! is 64 brackets, which a recursive projection descends in about 530 KiB — a quarter of the
//! smallest stack this suite runs on. The assertion goes red; the harness lives.
//!
//! # Four fixtures, because there are four cycles
//!
//! One shape would leave three of them unmeasured, and they are genuinely separate walks over
//! separate frame types. A constant object value reaches `const_value`, an executable argument's
//! object value reaches `value`, a selection chain reaches `selection_set`, and a nested list type
//! reaches `ty`.

#![cfg(all(feature = "graphql", feature = "rowan", feature = "std"))]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
  ptr,
};

use smear_parser::{
  graphql::lossless::{
    Parse, parse_executable_document_with_limits, parse_type_system_document_with_limits,
    project_executable_document, project_type_system_document,
  },
  lexer::limits::LosslessLimits,
};

thread_local! {
  /// Whether the probe is armed on this thread. Off for every other harness thread, so a
  /// concurrent test cannot move these readings.
  static PROBING: Cell<bool> = const { Cell::new(false) };
  /// The lowest native-stack address an allocation has been made from while armed.
  static DEEPEST: Cell<usize> = const { Cell::new(usize::MAX) };
  /// How many allocations the projection made, so a fixture that allocates nothing is caught
  /// rather than measured as flat.
  static ALLOCS: Cell<usize> = const { Cell::new(0) };
}

/// A pass-through allocator that records where on the native stack a request is coming from.
struct Probe;

#[allow(unsafe_code)]
unsafe impl GlobalAlloc for Probe {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    mark_frame();
    unsafe { System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    mark_frame();
    unsafe { System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    mark_frame();
    unsafe { System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    mark_frame();
    unsafe { System.dealloc(ptr, layout) }
  }
}

#[global_allocator]
static ALLOCATOR: Probe = Probe;

/// Records the caller's position on the native stack, from a local in this frame.
#[inline]
fn mark_frame() {
  let here = 0u8;
  let address = ptr::addr_of!(here) as usize;
  let _ = PROBING.try_with(|probing| {
    if probing.get() {
      let _ = ALLOCS.try_with(|allocs| allocs.set(allocs.get().wrapping_add(1)));
      let _ = DEEPEST.try_with(|deepest| deepest.set(deepest.get().min(address)));
    }
  });
}

/// One reading: the native stack the projection descended through, and the allocations it made.
struct Reading {
  stack: usize,
  allocs: usize,
}

/// Projects `parse` against `source` with the probe armed, and reports what the walk cost.
///
/// The parse is built by the caller and the AST is released after the probe is disarmed, so the
/// reading is the projection's own and nothing else's.
fn project(parse: &Parse, source: &str, executable: bool) -> Reading {
  let anchor = 0u8;
  let base = ptr::addr_of!(anchor) as usize;

  ALLOCS.with(|allocs| allocs.set(0));
  DEEPEST.with(|deepest| deepest.set(usize::MAX));
  PROBING.with(|probing| probing.set(true));

  let projected = if executable {
    project_executable_document(parse, source).map(|ast| ast.definitions().len())
  } else {
    project_type_system_document(parse, source).map(|ast| ast.definitions().len())
  };

  PROBING.with(|probing| probing.set(false));

  assert_eq!(
    projected.as_ref().map_err(ToString::to_string),
    Ok(&1),
    "the fixture did not project to one definition, so the reading is not of a whole walk"
  );

  Reading {
    stack: base.saturating_sub(DEEPEST.with(Cell::get)),
    allocs: ALLOCS.with(Cell::get),
  }
}

/// The shallower of the two depths every reading is taken at.
const SHALLOW: usize = 8;

/// The deeper one, eight times further down.
///
/// A recursive projection descends this in about 530 KiB, which is a quarter of the smallest stack
/// this suite runs on — past the per-level cost by a wide margin and nowhere near the boundary,
/// which is what makes a regression legible instead of fatal. See the module header.
const DEEP: usize = 64;

/// What a difference of two depths may still contain: the constant part of the walk's own frames,
/// which cancels, plus whatever the platform puts between two calls at the same depth.
///
/// A projection that recursed would put 4 080 bytes per green level in it — over 450 KiB across
/// these two depths for the object-value fixture, which is 55x this.
const SLACK: usize = 8 * 1024;

/// A projection that recursed would spend this per green level, measured at `fcd7f5e` on
/// `aarch64-apple-darwin`, unoptimised. Reported in the failure message so a red reading can be
/// read against the defect rather than against a bare number.
const RECURSIVE_BYTES_PER_LEVEL: usize = 4_080;

/// Asserts that a fixture's projection is flat in its nesting depth.
///
/// `build` takes a bracket count and returns the source to project; `executable` picks the root.
fn assert_projection_is_flat(what: &str, executable: bool, build: impl Fn(usize) -> String) {
  let read = |brackets: usize| {
    let source = build(brackets);
    let parse = if executable {
      parse_executable_document_with_limits(&source, LosslessLimits::unlimited())
    } else {
      parse_type_system_document_with_limits(&source, LosslessLimits::unlimited())
    };
    assert!(
      !parse.has_errors(),
      "{what}: the lossless parser rejects the {brackets}-bracket fixture, so it is not the shape \
       this reading is about"
    );
    let levels = green_depth(parse.green());
    (levels, project(&parse, &source, executable))
  };

  let (shallow_levels, shallow) = read(SHALLOW);
  let (deep_levels, deep) = read(DEEP);

  // The fixtures really are the depths they claim, and the walk really did allocate — a reading
  // taken over a walk that allocated nothing would be flat for the wrong reason.
  assert!(
    deep_levels > shallow_levels + 32,
    "{what}: {shallow_levels} and {deep_levels} green levels for {SHALLOW} and {DEEP} brackets, \
     so the fixture does not nest with its bracket count"
  );
  assert!(
    shallow.allocs > 0 && deep.allocs > 0,
    "{what}: the projection allocated {} and {} times, so the probe saw no walk at all",
    shallow.allocs,
    deep.allocs
  );

  // And the reading this file exists for: eight times the nesting costs the same native stack.
  let extra = deep.stack.saturating_sub(shallow.stack);
  assert!(
    extra < SLACK,
    "{what}: {DEEP} brackets ({deep_levels} green levels) used {extra} more bytes of native stack \
     than {SHALLOW} did ({shallow_levels} levels), which is past the {SLACK} a constant-frame walk \
     may drift by. A projection that recursed would show roughly {}. al8n/smear#201.",
    RECURSIVE_BYTES_PER_LEVEL * (deep_levels - shallow_levels)
  );
}

/// The fixture's green depth, read off the parse rather than derived from the bracket count.
///
/// Iterative, so the instrument cannot be what overflows, and read from the real tree so a fixture
/// whose spelling stops nesting is caught by the assertion above rather than assumed.
fn green_depth(root: &rowan::GreenNode) -> usize {
  let mut deepest = 1usize;
  let mut stack: Vec<(usize, rowan::Children<'_>)> = std::vec![(1, root.children())];
  while let Some((level, children)) = stack.last_mut() {
    let level = *level;
    match children.next() {
      Some(rowan::NodeOrToken::Node(child)) => {
        deepest = deepest.max(level + 1);
        stack.push((level + 1, child.children()));
      }
      Some(rowan::NodeOrToken::Token(_)) => {}
      None => {
        stack.pop();
      }
    }
  }
  deepest
}

/// `scalar S @d(a: {a: {a: … 1 … }})` — the constant value cycle, and the shape #201 measures.
#[test]
fn a_constant_object_value_projects_flat() {
  assert_projection_is_flat("const_value", false, |brackets| {
    let mut source = String::from("scalar S @d(a: ");
    for _ in 0..brackets {
      source.push_str("{a: ");
    }
    source.push('1');
    for _ in 0..brackets {
      source.push('}');
    }
    source.push(')');
    source
  });
}

/// `query { f(a: {a: … 1 … }) }` — the executable value cycle, which is the constant one's twin
/// and a separate walk over separate types.
#[test]
fn an_executable_object_value_projects_flat() {
  assert_projection_is_flat("value", true, |brackets| {
    let mut source = String::from("query { f(a: ");
    for _ in 0..brackets {
      source.push_str("{a: ");
    }
    source.push('1');
    for _ in 0..brackets {
      source.push('}');
    }
    source.push_str(") }");
    source
  });
}

/// `query { a { a { … } } }` — the selection cycle.
#[test]
fn a_selection_chain_projects_flat() {
  assert_projection_is_flat("selection_set", true, |brackets| {
    let mut source = String::from("query ");
    for _ in 0..brackets {
      source.push_str("{ a ");
    }
    source.push_str("{ __typename }");
    for _ in 0..brackets {
      source.push_str(" }");
    }
    source
  });
}

/// `type T { f: [[[ … Int … ]]] }` — the type-reference cycle, whose frames carry the `!` fold as
/// well as the element.
#[test]
fn a_nested_list_type_projects_flat() {
  assert_projection_is_flat("ty", false, |brackets| {
    let mut source = String::from("type T { f: ");
    for _ in 0..brackets {
      source.push('[');
    }
    source.push_str("Int");
    for _ in 0..brackets {
      source.push_str("]!");
    }
    source.push_str(" }");
    source
  });
}
