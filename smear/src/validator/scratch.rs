//! The caller-held working set, and the policy that bounds work.
//!
//! # Why the caller holds it
//!
//! Validation is a pure function of `(schema, document)`, but it needs somewhere to put a
//! fragment index, a few bitsets and two explicit traversal stacks. Allocating those per request
//! would put a handful of `malloc`s on the hot path of a server that otherwise makes none, so the
//! buffers live in a [`Scratch`] the caller owns and reuses: [`Scratch::reset`] clears without
//! freeing, capacity survives, and the steady state allocates nothing at all.
//!
//! This is `mdns-proto`'s caller-held `Pool` idea in the shape validation actually needs. It is a
//! concrete struct rather than a pluggable container trait because the buffers are heterogeneous
//! and internal to the algorithms — a trait would push generic parameters through every rule to
//! let a caller swap something that is not policy. What *is* policy is [`Budget`].
//!
//! # Nothing in here is source-dependent
//!
//! Every buffer holds integers, so one `Scratch` serves documents whose source slice types differ
//! and can be reused across them. That is also why the two traversal stacks are *coordinates*
//! rather than node references: a frame names a definition and the chain of child indices that
//! reaches it, which is what lets an explicit stack replace recursion over document shape without
//! the stack itself becoming generic.

use std::vec::Vec;

use tokora::SimpleSpan;

use super::schema::{PackedType, Range32, TypeId};

/// How much work draft 5.3.2's merge engine may do before a document is refused.
///
/// Both knobs are **absolute**, never proportional to the input: an attacker chooses the input,
/// so a proportional bound is not a bound. On a trip the engine emits its own rule and fails the
/// document — never "passes unvalidated", never panics.
///
/// This is a *validation* budget and has nothing to do with the parser's recursion budget. They
/// bound different resources — parse-time nesting against merge-time work — and are set
/// independently.
///
/// # Status
///
/// `OverlappingFieldsCanBeMerged` (draft 5.3.2) is the only rule that reads these knobs, and it
/// lands in its own wave so that its resource bound is designed rather than bolted on. The type
/// is here now because it is part of the entry point's shape, and moving that later would move it
/// under callers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Budget {
  merge_depth: u32,
  merge_work: u32,
}

impl Budget {
  /// The default response-shape nesting bound inside the merge recursion.
  ///
  /// 128, matching apollo-compiler's serde_json-derived limit.
  pub const DEFAULT_MERGE_DEPTH: u32 = 128;

  /// The default bound on total expanded field rows plus pair comparisons.
  ///
  /// Depth alone does not bound the merge engine — breadth times fragment reuse does — so the
  /// work counter is the knob that actually caps the worst case.
  pub const DEFAULT_MERGE_WORK: u32 = 65_536;

  /// Creates a budget from both knobs.
  #[inline]
  pub const fn new(merge_depth: u32, merge_work: u32) -> Self {
    Self {
      merge_depth,
      merge_work,
    }
  }

  /// Returns the response-shape nesting bound.
  #[inline]
  pub const fn merge_depth(&self) -> u32 {
    self.merge_depth
  }

  /// Returns the total-work bound.
  #[inline]
  pub const fn merge_work(&self) -> u32 {
    self.merge_work
  }

  /// Returns the budget with a different nesting bound.
  #[inline]
  pub const fn with_merge_depth(mut self, merge_depth: u32) -> Self {
    self.merge_depth = merge_depth;
    self
  }

  /// Returns the budget with a different work bound.
  #[inline]
  pub const fn with_merge_work(mut self, merge_work: u32) -> Self {
    self.merge_work = merge_work;
    self
  }
}

impl Default for Budget {
  #[inline]
  fn default() -> Self {
    Self::new(Self::DEFAULT_MERGE_DEPTH, Self::DEFAULT_MERGE_WORK)
  }
}

/// The sentinel a `u32` field uses for "absent".
pub(crate) const NONE: u32 = u32::MAX;

/// One operation definition, as the prep sweep recorded it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct OperationRow {
  /// Index into the document's definition list.
  pub(crate) definition: u32,
  /// The root operation slot the operation needs, as [`RootOperation::index`].
  ///
  /// [`RootOperation::index`]: super::schema::RootOperation::index
  pub(crate) root: u8,
  /// Whether the operation carries a name.
  pub(crate) named: bool,
  /// The span to blame for an operation-level diagnostic.
  pub(crate) span: SimpleSpan,
  /// The operation's outgoing fragment-spread edges, into [`Scratch::edges`].
  pub(crate) edges: Range32,
}

/// One fragment definition, as the prep sweep recorded it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct FragmentRow {
  /// Index into the document's definition list.
  pub(crate) definition: u32,
  /// The span of the fragment's name.
  pub(crate) span: SimpleSpan,
  /// Every fragment sharing this one's name, as a range into [`Scratch::order`].
  ///
  /// The group's first element is the definition every spread of that name resolves to, and
  /// reachability propagates across the whole group — so one duplicated name reports 5.5.1.1 and
  /// does not also read as an unused fragment.
  pub(crate) group: Range32,
  /// The fragment's outgoing fragment-spread edges, into [`Scratch::edges`].
  pub(crate) edges: Range32,
}

/// One fragment spread, as an edge of the fragment graph.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Edge {
  /// The fragment ordinal the spread names, or [`NONE`] when no fragment has that name.
  pub(crate) to: u32,
  /// The spread's span.
  pub(crate) span: SimpleSpan,
}

/// One level of the explicit selection-set walk.
///
/// A frame is a *coordinate*, not a reference: `definition` plus the `child` chain of the frames
/// beneath it locates the selection set by descent. That is what lets the stack live in a
/// [`Scratch`] that knows nothing about the document's source slice type.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Frame {
  /// Index into the document's definition list of the definition this level lives in.
  pub(crate) definition: u32,
  /// Index in the parent level's selection list that led here, or [`NONE`] at a definition root.
  pub(crate) child: u32,
  /// The next selection index to visit at this level.
  pub(crate) cursor: u32,
  /// The type this level's selections are written against, or [`NONE`] when it did not resolve.
  pub(crate) ty: u32,
  /// See the `frame` flag constants.
  pub(crate) flags: u8,
}

impl Frame {
  /// The level's definition-local rules run here.
  ///
  /// Off when the level is a fragment body some earlier operation already checked: those rules
  /// are properties of the fragment, not of the operation that reached it, so they fire once.
  pub(crate) const CHECK: u8 = 1 << 0;

  /// Creates the root frame of a definition's own selection set.
  #[inline]
  pub(crate) const fn root(definition: u32, ty: u32, flags: u8) -> Self {
    Self {
      definition,
      child: NONE,
      cursor: 0,
      ty,
      flags,
    }
  }

  /// Creates the frame for a selection set nested inside the level above it.
  #[inline]
  pub(crate) const fn child(definition: u32, child: u32, ty: u32, flags: u8) -> Self {
    Self {
      definition,
      child,
      cursor: 0,
      ty,
      flags,
    }
  }

  /// Returns whether this frame begins a definition rather than continuing one.
  #[inline]
  pub(crate) const fn is_definition_root(&self) -> bool {
    self.child == NONE
  }

  /// Returns the level's type, or `None` when it did not resolve.
  #[inline]
  pub(crate) const fn type_id(&self) -> Option<TypeId> {
    if self.ty == NONE {
      None
    } else {
      Some(TypeId::new(self.ty))
    }
  }
}

/// What a value-walk level is descending through.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ValueLevel {
  /// The level's children are a list literal's entries.
  List,
  /// The level's children are an object literal's field values.
  Object,
}

/// One level of the explicit value-literal walk.
///
/// The same coordinate idea as [`Frame`], one tree down: the root value is held by the caller for
/// the duration of the walk and a frame names the chain of list indices and object-field indices
/// that reaches a nested value.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ValueFrame {
  /// Index in the parent level that led here, or [`NONE`] at the root value.
  pub(crate) child: u32,
  /// The next child index to visit at this level.
  pub(crate) cursor: u32,
  /// For a list level, the type each entry is expected to have; unused for an object level, whose
  /// children take their types from the schema. `None` when the position did not resolve.
  pub(crate) expected: Option<PackedType>,
  /// Whether the level's children are list entries or object field values.
  pub(crate) level: ValueLevel,
  /// The input object type this level's fields belong to, when [`ValueFrame::level`] is
  /// [`ValueLevel::Object`]; [`NONE`] when it did not resolve.
  pub(crate) object: u32,
  /// The position bits, as `ValueLocation` spells them.
  pub(crate) flags: u8,
}

/// One level of the fragment-graph depth-first search.
#[derive(Debug, Clone, Copy)]
pub(crate) struct GraphFrame {
  /// The fragment ordinal being explored.
  pub(crate) fragment: u32,
  /// The next outgoing edge index to follow.
  pub(crate) edge: u32,
}

/// The caller-held working set.
///
/// Create one, hand it to every call, and reuse it. It grows to the high-water mark of the
/// documents it has seen and never shrinks on its own, so the second and every later request
/// allocate nothing.
///
/// ```
/// # use smear::validator::Scratch;
/// let mut scratch = Scratch::new();
/// // … validate_executable(&schema, &document, &mut scratch, &budget, &mut sink) …
/// scratch.reset(); // clears, keeps capacity
/// ```
#[derive(Debug, Default, Clone)]
pub struct Scratch {
  /// The document's operations, in source order.
  pub(crate) operations: Vec<OperationRow>,
  /// The document's fragments, in source order; the index is the fragment ordinal.
  pub(crate) fragments: Vec<FragmentRow>,
  /// Fragment ordinals sorted by name bytes, for lookup by binary search.
  pub(crate) order: Vec<u32>,
  /// Every fragment spread in the document, grouped by the definition that wrote it.
  pub(crate) edges: Vec<Edge>,
  /// The selection-set walk's frame stack.
  pub(crate) frames: Vec<Frame>,
  /// The subscription root-field collection walk's frame stack.
  ///
  /// Separate from [`Scratch::frames`] because draft 5.2.4.1 runs its own collection with its own
  /// rules about what it may descend into.
  pub(crate) roots: Vec<Frame>,
  /// The value-literal walk's frame stack.
  pub(crate) values: Vec<ValueFrame>,
  /// The fragment-graph search's frame stack.
  pub(crate) graph: Vec<GraphFrame>,
  /// The duplicate-scan index permutation.
  ///
  /// Argument sets, variable lists, directive lists and input object literals are all short, so
  /// duplicate detection sorts a segment of indices in place and scans neighbours rather than
  /// building a map. Comparison is always on the *bytes*: interning the names first would collapse
  /// every name the schema does not know onto one sentinel, and two distinct undefined names would
  /// read as duplicates of each other.
  ///
  /// Segments stack — a scan pushes at `len`, sorts its own slice and truncates back — so an input
  /// object literal nested inside an argument list does not disturb the scan above it.
  pub(crate) keys: Vec<u32>,
  /// Fragment ordinals reachable from some operation (draft 5.5.1.4).
  pub(crate) reachable: Vec<u64>,
  /// Fragment ordinals already entered during the current walk.
  pub(crate) visited: Vec<u64>,
  /// Fragment ordinals on the current fragment-graph search path (draft 5.5.2.2).
  pub(crate) on_path: Vec<u64>,
  /// Fragment ordinals whose fragment-graph search has finished.
  pub(crate) done: Vec<u64>,
  /// Definition indices whose definition-local rules have already run.
  pub(crate) checked: Vec<u64>,
  /// Variable-definition indices used by the operation being walked (draft 5.8.4).
  pub(crate) used: Vec<u64>,
}

impl Scratch {
  /// Creates an empty working set.
  ///
  /// It allocates on its first use, not here, so a server can build one per connection cheaply and
  /// let the sizes settle.
  #[inline]
  pub const fn new() -> Self {
    Self {
      operations: Vec::new(),
      fragments: Vec::new(),
      order: Vec::new(),
      edges: Vec::new(),
      frames: Vec::new(),
      roots: Vec::new(),
      values: Vec::new(),
      graph: Vec::new(),
      keys: Vec::new(),
      reachable: Vec::new(),
      visited: Vec::new(),
      on_path: Vec::new(),
      done: Vec::new(),
      checked: Vec::new(),
      used: Vec::new(),
    }
  }

  /// Empties every buffer, keeping every allocation.
  ///
  /// `validate_executable` calls this itself on entry, so a caller never has to remember; it is
  /// public for the case where a caller wants the memory quiet between bursts without dropping
  /// the value.
  pub fn reset(&mut self) {
    self.operations.clear();
    self.fragments.clear();
    self.order.clear();
    self.edges.clear();
    self.frames.clear();
    self.roots.clear();
    self.values.clear();
    self.graph.clear();
    self.keys.clear();
    self.reachable.clear();
    self.visited.clear();
    self.on_path.clear();
    self.done.clear();
    self.checked.clear();
    self.used.clear();
  }

  /// Returns a rough count of the rows the working set is currently holding.
  ///
  /// Exposed so a test can show that reuse is real — that the second validation of the same
  /// document grows nothing — rather than asserting it from the outside.
  pub fn capacity(&self) -> usize {
    self.operations.capacity()
      + self.fragments.capacity()
      + self.order.capacity()
      + self.edges.capacity()
      + self.frames.capacity()
      + self.roots.capacity()
      + self.values.capacity()
      + self.graph.capacity()
      + self.keys.capacity()
      + self.reachable.capacity()
      + self.visited.capacity()
      + self.on_path.capacity()
      + self.done.capacity()
      + self.checked.capacity()
      + self.used.capacity()
  }
}

/// Sizes a bitset to hold `bits` bits and clears it, without freeing.
pub(crate) fn reset_bits(words: &mut Vec<u64>, bits: usize) {
  let needed = bits.div_ceil(64);
  words.clear();
  words.resize(needed, 0);
}

/// Returns whether bit `index` is set.
#[inline]
pub(crate) fn get_bit(words: &[u64], index: u32) -> bool {
  let word = (index / 64) as usize;
  words
    .get(word)
    .is_some_and(|bits| bits & (1u64 << (index % 64)) != 0)
}

/// Sets bit `index`, returning whether it was already set.
#[inline]
pub(crate) fn set_bit(words: &mut [u64], index: u32) -> bool {
  let word = (index / 64) as usize;
  match words.get_mut(word) {
    Some(bits) => {
      let mask = 1u64 << (index % 64);
      let was = *bits & mask != 0;
      *bits |= mask;
      was
    }
    None => true,
  }
}

/// Clears bit `index`.
#[inline]
pub(crate) fn clear_bit(words: &mut [u64], index: u32) {
  let word = (index / 64) as usize;
  if let Some(bits) = words.get_mut(word) {
    *bits &= !(1u64 << (index % 64));
  }
}

#[cfg(test)]
mod tests {
  use super::{Budget, Scratch, get_bit, reset_bits, set_bit};
  use std::vec::Vec;

  #[test]
  fn budget_defaults_are_the_designed_numbers() {
    let budget = Budget::default();
    assert_eq!(budget.merge_depth(), 128);
    assert_eq!(budget.merge_work(), 65_536);
    assert_eq!(budget.with_merge_depth(8).merge_depth(), 8);
    assert_eq!(budget.with_merge_work(9).merge_work(), 9);
    // The knobs are independent; setting one must not disturb the other.
    assert_eq!(budget.with_merge_depth(8).merge_work(), 65_536);
  }

  #[test]
  fn reset_keeps_capacity() {
    let mut scratch = Scratch::new();
    scratch.order.extend(0..64u32);
    let capacity = scratch.capacity();
    assert!(capacity >= 64);
    scratch.reset();
    assert!(scratch.order.is_empty());
    assert_eq!(scratch.capacity(), capacity, "reset must not free");
  }

  #[test]
  fn bitsets_round_trip_and_report_previous_state() {
    let mut words = Vec::new();
    reset_bits(&mut words, 130);
    assert_eq!(words.len(), 3);
    assert!(!get_bit(&words, 129));
    assert!(!set_bit(&mut words, 129));
    assert!(set_bit(&mut words, 129));
    assert!(get_bit(&words, 129));
    // Out of range reads as unset and refuses to record, rather than panicking mid-traversal.
    assert!(!get_bit(&words, 1_000));
    assert!(set_bit(&mut words, 1_000));
  }
}
