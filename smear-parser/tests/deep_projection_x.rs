//! Projecting a nested GraphQLx document does not spend a native frame per level of nesting.
//!
//! # The defect this exists to catch coming back
//!
//! `deep_projection.rs` is this file's twin and carries the measurement: at the top of the lexer's
//! `HARD_MAX` the lossless doors produce exactly the tree a recursive node dispatch cannot descend,
//! no value of `MAX_GREEN_DEPTH` closes that, and the repair is a worklist per cycle
//! (al8n/smear#201). What is re-derived here is **which cycles this dialect has**, because the two
//! do not have the same ones and a fixture set ported from the other would leave four of these
//! unmeasured.
//!
//! GraphQLx has three cycles where the vanilla dialect has four, and the arithmetic is not a
//! narrowing:
//!
//! - the **value** cycle absorbs two more containers — `set { … }` and `map { k => v }` — and a map
//!   entry is the one place a single child slot yields *two* subtrees, so it nests through its key
//!   as well as through its value;
//! - the **type** cycle absorbs what GraphQL splits off as a `NonNullType` wrapper (the `!` is a
//!   token here) and gains three shapes of its own: a set type, a map type through either half, and
//!   a path's **generic arguments**, which nest without passing through a bracket at all;
//! - the **selection** cycle is the vanilla dialect's, unchanged.
//!
//! So there are nine fixtures below rather than four, and each of them is a separate walk over
//! separate frame types. A shape with no fixture is a shape whose worklist nobody measured.
//!
//! # Why the reading is a stack address and not a thread that is too small
//!
//! The property is *the projection returns on a small stack*, and a test that spawns a small thread
//! to check it reports a regression by **aborting the harness**: a stack overflow is `SIGABRT`,
//! which no `#[should_panic]` sees and which takes every other test in the binary with it.
//!
//! So the depth is read the way the twin reads it: the projection allocates — every list, object,
//! map, selection set and argument list it builds is a `Vec` — and the allocator sees the native
//! stack it was called on. The reading is the difference between the lowest address an allocation
//! was made from and an anchor in this frame, taken at two depths. A walk that recursed puts one
//! frame per level in that difference; a walk that does not puts nothing.

#![cfg(all(feature = "graphqlx", feature = "rowan", feature = "std"))]

use std::{
  alloc::{GlobalAlloc, Layout, System},
  cell::Cell,
  ptr,
};

use smear_parser::{
  graphqlx::lossless::{
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
const DEEP: usize = 64;

/// What a difference of two depths may still contain: the constant part of the walk's own frames,
/// which cancels, plus whatever the platform puts between two calls at the same depth.
const SLACK: usize = 8 * 1024;

/// A projection that recursed would spend roughly this per green level, measured for the vanilla
/// dialect at `fcd7f5e` on `aarch64-apple-darwin`, unoptimised. Reported in the failure message so
/// a red reading can be read against the defect rather than against a bare number.
const RECURSIVE_BYTES_PER_LEVEL: usize = 4_080;

/// Asserts that a fixture's projection is flat in its nesting depth.
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
      "{what}: the lossless parser rejects the {brackets}-level fixture, so it is not the shape \
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
    "{what}: {shallow_levels} and {deep_levels} green levels for {SHALLOW} and {DEEP} levels, so \
     the fixture does not nest with its level count"
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
    "{what}: {DEEP} levels ({deep_levels} green levels) used {extra} more bytes of native stack \
     than {SHALLOW} did ({shallow_levels} levels), which is past the {SLACK} a constant-frame walk \
     may drift by. A projection that recursed would show roughly {}. al8n/smear#201.",
    RECURSIVE_BYTES_PER_LEVEL * (deep_levels - shallow_levels)
  );
}

/// The fixture's green depth, read off the parse rather than derived from the level count.
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

/// `wrap` applied `depth` times to `"1"`, which is the leaf every value fixture bottoms out on.
fn nested_value(depth: usize, wrap: impl Fn(&str) -> String) -> String {
  let mut value = String::from("1");
  for _ in 0..depth {
    value = wrap(&value);
  }
  value
}

/// A constant value in a directive argument on an SDL definition — the `const_value` grammar.
fn constant(value: &str) -> String {
  std::format!("scalar S @d(a: {value})")
}

/// The same value in an executable field's argument — the `value` grammar, a separate
/// monomorphisation over separate frame types.
fn executable(value: &str) -> String {
  std::format!("query {{ f(a: {value}) }}")
}

/// A type reference in an SDL field position.
fn typed(reference: &str) -> String {
  std::format!("type T {{ f: {reference} }}")
}

/// `wrap` applied `depth` times to `"Int"`.
fn nested_type(depth: usize, wrap: impl Fn(&str) -> String) -> String {
  let mut reference = String::from("Int");
  for _ in 0..depth {
    reference = wrap(&reference);
  }
  reference
}

// ---------------------------------------------------------------------------------------------
// the value cycle — five shapes, three of them GraphQLx-only
// ---------------------------------------------------------------------------------------------

#[test]
fn a_constant_object_value_projects_flat() {
  assert_projection_is_flat("const_value / object", false, |depth| {
    constant(&nested_value(depth, |inner| std::format!("{{a: {inner}}}")))
  });
}

#[test]
fn an_executable_object_value_projects_flat() {
  assert_projection_is_flat("value / object", true, |depth| {
    executable(&nested_value(depth, |inner| std::format!("{{a: {inner}}}")))
  });
}

#[test]
fn a_list_value_projects_flat() {
  assert_projection_is_flat("const_value / list", false, |depth| {
    constant(&nested_value(depth, |inner| std::format!("[{inner}]")))
  });
}

/// `set { set { … } }` — GraphQLx only, and a container the vanilla dialect's fixture set has no
/// shape for.
#[test]
fn a_set_value_projects_flat() {
  assert_projection_is_flat("const_value / set", false, |depth| {
    constant(&nested_value(depth, |inner| {
      std::format!("set {{ {inner} }}")
    }))
  });
}

/// `map { 1 => map { … } }` — the entry's **value** half.
#[test]
fn a_map_values_value_half_projects_flat() {
  assert_projection_is_flat("const_value / map value", false, |depth| {
    constant(&nested_value(depth, |inner| {
      std::format!("map {{ 1 => {inner} }}")
    }))
  });
}

/// `map { map { … } => 2 }` — the entry's **key** half, which is the one a frame has to hold a
/// finished value across. Measured separately because the two halves are two states of one frame
/// and a walk could be flat in one and not the other.
#[test]
fn a_map_values_key_half_projects_flat() {
  assert_projection_is_flat("const_value / map key", false, |depth| {
    constant(&nested_value(depth, |inner| {
      std::format!("map {{ {inner} => 2 }}")
    }))
  });
}

// ---------------------------------------------------------------------------------------------
// the type cycle — four shapes, three of them GraphQLx-only
// ---------------------------------------------------------------------------------------------

#[test]
fn a_nested_list_type_projects_flat() {
  assert_projection_is_flat("ty / list", false, |depth| {
    typed(&nested_type(depth, |inner| std::format!("[{inner}]")))
  });
}

/// `<<<Int>>>` — GraphQLx only.
#[test]
fn a_nested_set_type_projects_flat() {
  assert_projection_is_flat("ty / set", false, |depth| {
    typed(&nested_type(depth, |inner| std::format!("<{inner}>")))
  });
}

/// `<Int => <Int => … >>` — GraphQLx only, and the type-side twin of the map entry: two subtrees
/// under one node, with the key travelling on the frame while the value is built.
#[test]
fn a_nested_map_type_projects_flat() {
  assert_projection_is_flat("ty / map", false, |depth| {
    typed(&nested_type(depth, |inner| {
      std::format!("<Int => {inner}>")
    }))
  });
}

/// `A<A<A<Int>>>` — the fourth way a GraphQLx type nests, and the one a reading of the three
/// bracket shapes alone would miss: a path's arguments are types, so this nests without passing
/// through a bracket at all.
#[test]
fn a_nested_generic_argument_list_projects_flat() {
  assert_projection_is_flat("ty / generics", false, |depth| {
    typed(&nested_type(depth, |inner| std::format!("A<{inner}>")))
  });
}

// ---------------------------------------------------------------------------------------------
// the selection cycle
// ---------------------------------------------------------------------------------------------

#[test]
fn a_selection_chain_projects_flat() {
  assert_projection_is_flat("selection_set", true, |depth| {
    let mut source = String::from("query ");
    for _ in 0..depth {
      source.push_str("{ a ");
    }
    source.push_str("{ __typename }");
    for _ in 0..depth {
      source.push_str(" }");
    }
    source
  });
}
