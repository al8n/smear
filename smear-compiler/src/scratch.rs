//! The caller-held working set, and the policy that bounds work.
//!
//! # Why the caller holds it
//!
//! Validation is a pure function of `(schema, document)`, but it needs somewhere to put a
//! fragment index, a few bitsets, several explicit traversal stacks, and — for draft 5.3.2 — a
//! name interner, a table of every selection set in the document, and a memo over merged field
//! sets. Allocating those per request would put a handful of `malloc`s on the hot path of a server
//! that otherwise makes none, so the buffers live in a [`Scratch`] the caller owns and reuses:
//! [`Scratch::reset`] clears without freeing, capacity survives, and the steady state allocates
//! nothing at all.
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
/// # What the two knobs count
///
/// - [`merge_depth`](Budget::merge_depth) bounds how deeply the merge recursion may nest. One
///   level is one field-nesting level of the *response shape*, so it is the same quantity
///   `serde_json` bounds when it deserialises the answer.
/// - [`merge_work`](Budget::merge_work) bounds everything else, as one running total for the whole
///   call: expanded field rows, pair comparisons, the rows a common-parent partition duplicates,
///   and the tree steps a node resolution walks. Depth alone does not bound the engine — breadth
///   times fragment reuse does — so this is the knob that actually caps the worst case.
///
/// A document that exceeds either one is **refused**, with
/// [`Rule::MergeDepthBudget`](super::Rule::MergeDepthBudget) or
/// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) naming which, and
/// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) set on the verdict.
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

// ---------------------------------------------------------------------------------------------
// draft 5.3.2's working set
// ---------------------------------------------------------------------------------------------

/// One syntactic selection set, as draft 5.3.2's expansion sees it.
///
/// Every selection set in the document gets one of these, once per call, and every later
/// expansion reads them instead of the tree. A set's *scope* — the type its selections are written
/// against — is a syntactic property, fixed by where the set is written, which is what makes the
/// table shareable across every expansion that reaches it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MergeSet {
  /// Index into the document's definition list of the definition this set lives in.
  pub(crate) definition: u32,
  /// The set that encloses this one, or [`NONE`] at a definition's own selection set.
  pub(crate) parent: u32,
  /// The index in the parent set's selection list that leads here; [`NONE`] at a definition root.
  pub(crate) index: u32,
  /// The type the set's selections are written against, or [`NONE`] when it did not resolve.
  pub(crate) scope: u32,
  /// The fields written directly in the set, as a range into [`Scratch::merge_fields`].
  pub(crate) fields: Range32,
  /// The sub-sets an expansion must also visit, as a range into [`Scratch::merge_kids`].
  pub(crate) kids: Range32,
}

/// One field occurrence, precomputed into integers.
///
/// The merge engine compares response names, field names, return types and parent types with no
/// tree access at all; it goes back to the AST only to compare argument values and to name a
/// subject in a diagnostic.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MergeField {
  /// The [`MergeSet`] the field is written in.
  pub(crate) set: u32,
  /// The field's index in that set's selection list.
  pub(crate) index: u32,
  /// The interned response name — the alias when there is one.
  pub(crate) response: u32,
  /// The interned field name.
  pub(crate) name: u32,
  /// The type the field is selected on, or [`NONE`] when it did not resolve.
  pub(crate) parent: u32,
  /// The field's own selection set, or [`NONE`] when it has none.
  pub(crate) child: u32,
  /// The field's declared return type, or `None` when the schema does not define the field.
  pub(crate) ty: Option<PackedType>,
  /// How many arguments the field was written with.
  pub(crate) args: u32,
  /// The span covering the whole field.
  pub(crate) span: SimpleSpan,
}

/// A sub-set an expansion must visit after the set that names it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MergeKid {
  /// The [`MergeSet`] to visit.
  pub(crate) set: u32,
  /// The fragment ordinal a named spread reaches, or [`NONE`] for an inline fragment.
  ///
  /// An expansion enters each *named* fragment at most once — the specification's inclusion is a
  /// set — which is both what makes a cyclic graph terminate here and what keeps a widely reused
  /// fragment from being expanded once per spread.
  pub(crate) fragment: u32,
}

/// One level of the merge recursion.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MergeFrame {
  /// The merged field set this level is checking, as a range into [`Scratch::merge_rows`].
  pub(crate) rows: Range32,
  /// The next row slot a response-name group starts at.
  pub(crate) cursor: u32,
  /// Where the current group's partition starts in [`Scratch::merge_parts`].
  pub(crate) parts: u32,
  /// Where the current group's partition starts in [`Scratch::merge_bounds`].
  pub(crate) bounds: u32,
  /// The next partition slot to check, as an index into [`Scratch::merge_bounds`].
  pub(crate) part: u32,
}

/// One merged field set the engine has already met.
///
/// The key is the *content* of the row range, so two expansions that reach the same set of field
/// occurrences by different routes share one entry — which is the memoisation that keeps a
/// fragment-heavy document from re-deriving the same merge over and over.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MergeMemo {
  /// The key's hash, compared before the contents are.
  pub(crate) hash: u64,
  /// The canonical row range for this content.
  pub(crate) rows: Range32,
  /// Which passes have already run over it.
  pub(crate) flags: u8,
}

/// A document-local name interner.
///
/// Draft 5.3.2 groups by response name and compares field names, and it does so over a working
/// set that may not name the document's source type — so the names are copied into an arena here
/// and reduced to `u32`s. The schema's own interner cannot serve: an alias is not a schema name,
/// and two *different* names the schema does not know would both resolve to "absent" and read as
/// equal.
#[derive(Debug, Default, Clone)]
pub(crate) struct Names {
  /// Every interned name's bytes, concatenated.
  bytes: Vec<u8>,
  /// Name id to its `(start, end)` range in [`Names::bytes`].
  ranges: Vec<(u32, u32)>,
  /// Open-addressing probe slots, power-of-two, [`NONE`] when empty.
  slots: Vec<u32>,
}

impl Names {
  /// The smallest probe table the interner builds.
  const MIN_SLOTS: usize = 64;

  /// Creates an empty interner, allocating nothing.
  #[inline]
  pub(crate) const fn new() -> Self {
    Self {
      bytes: Vec::new(),
      ranges: Vec::new(),
      slots: Vec::new(),
    }
  }

  /// Empties the interner, keeping every allocation.
  pub(crate) fn reset(&mut self) {
    self.bytes.clear();
    self.ranges.clear();
    self.slots.fill(NONE);
  }

  /// Returns how many rows the interner is holding capacity for.
  pub(crate) fn capacity(&self) -> usize {
    self.bytes.capacity() + self.ranges.capacity() + self.slots.capacity()
  }

  /// Returns how many distinct names have been interned.
  #[cfg(test)]
  pub(crate) fn len(&self) -> usize {
    self.ranges.len()
  }

  /// Returns `key`'s id, interning it if this is the first time it has been seen.
  pub(crate) fn intern(&mut self, key: &[u8]) -> u32 {
    // Load factor 3/4. Checked before the probe so the loop below always finds an empty slot.
    if (self.ranges.len() + 1) * 4 >= self.slots.len() * 3 {
      self.grow();
    }
    let mask = self.slots.len() - 1;
    let mut slot = (hash_bytes(key) as usize) & mask;
    loop {
      let id = self.slots[slot];
      if id == NONE {
        let start = self.bytes.len() as u32;
        self.bytes.extend_from_slice(key);
        let id = self.ranges.len() as u32;
        self.ranges.push((start, self.bytes.len() as u32));
        self.slots[slot] = id;
        return id;
      }
      let (start, end) = self.ranges[id as usize];
      if &self.bytes[start as usize..end as usize] == key {
        return id;
      }
      slot = (slot + 1) & mask;
    }
  }

  /// Doubles the probe table and reinserts every name.
  fn grow(&mut self) {
    let next = (self.slots.len() * 2).max(Self::MIN_SLOTS);
    self.slots.clear();
    self.slots.resize(next, NONE);
    let mask = next - 1;
    for id in 0..self.ranges.len() as u32 {
      let (start, end) = self.ranges[id as usize];
      let mut slot = (hash_bytes(&self.bytes[start as usize..end as usize]) as usize) & mask;
      while self.slots[slot] != NONE {
        slot = (slot + 1) & mask;
      }
      self.slots[slot] = id;
    }
  }
}

/// FxHash-style multiply-fold over short keys.
///
/// The same shape the schema's own [`NameIndex`](super::schema::NameIndex) uses, and for the same
/// reason: the keys are identifiers, so one multiply per eight bytes is all the mixing a probe
/// table needs and it costs no dependency.
#[inline]
pub(crate) fn hash_bytes(bytes: &[u8]) -> u64 {
  const K: u64 = 0x517c_c1b7_2722_0a95;
  let mut h: u64 = 0;
  let (chunks, rest) = bytes.as_chunks::<8>();
  for chunk in chunks {
    h = (h.rotate_left(5) ^ u64::from_le_bytes(*chunk)).wrapping_mul(K);
  }
  let mut tail = [0u8; 8];
  tail[..rest.len()].copy_from_slice(rest);
  let value = u64::from_le_bytes(tail) ^ ((rest.len() as u64) << 56);
  (h.rotate_left(5) ^ value).wrapping_mul(K)
}

/// Mixes one `u32` into a running hash.
#[inline]
pub(crate) fn hash_u32(state: u64, value: u32) -> u64 {
  const K: u64 = 0x517c_c1b7_2722_0a95;
  (state.rotate_left(5) ^ u64::from(value)).wrapping_mul(K)
}

/// The caller-held working set.
///
/// Create one, hand it to every call, and reuse it. It grows to the high-water mark of the
/// documents it has seen and never shrinks on its own, so the second and every later request
/// allocate nothing.
///
/// ```
/// # use smear_compiler::Scratch;
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

  // -- draft 5.3.2 ------------------------------------------------------------------------------
  /// Every selection set in the document, one row each.
  pub(crate) merge_sets: Vec<MergeSet>,
  /// Every field occurrence, grouped by the set that writes it.
  pub(crate) merge_fields: Vec<MergeField>,
  /// Every inline fragment and named spread, grouped by the set that writes it.
  pub(crate) merge_kids: Vec<MergeKid>,
  /// Definition index to its root [`MergeSet`], or [`NONE`].
  pub(crate) merge_roots: Vec<u32>,
  /// Selection sets the index build has yet to fill.
  pub(crate) merge_todo: Vec<u32>,
  /// The child-index chain a node resolution descends.
  pub(crate) merge_path: Vec<u32>,
  /// Every merged field set the engine has expanded, as indices into [`Scratch::merge_fields`].
  ///
  /// This is the arena the [`Budget`]'s work knob indirectly caps: an expansion appends to it, so
  /// refusing to do more work is also refusing to grow it further.
  pub(crate) merge_rows: Vec<u32>,
  /// The expansion's own traversal stack, over [`MergeSet`] ids.
  pub(crate) merge_queue: Vec<u32>,
  /// Fragment ordinal to the expansion generation that last entered it.
  pub(crate) merge_seen: Vec<u32>,
  /// The selection sets the next expansion will merge.
  pub(crate) merge_inputs: Vec<u32>,
  /// A response-name group split by common parent type; rows repeat across parts.
  pub(crate) merge_parts: Vec<u32>,
  /// Where each part of the current group starts and ends in [`Scratch::merge_parts`].
  pub(crate) merge_bounds: Vec<Range32>,
  /// The merge recursion's frame stack.
  pub(crate) merge_stack: Vec<MergeFrame>,
  /// One entry per distinct merged field set met so far.
  pub(crate) merge_memo: Vec<MergeMemo>,
  /// Open-addressing probe slots over [`Scratch::merge_memo`], [`NONE`] when empty.
  pub(crate) merge_slots: Vec<u32>,
  /// The value comparison's frame stack: `(index in the left value, index in the right, cursor)`.
  pub(crate) merge_compare: Vec<(u32, u32, u32)>,
  /// The document's own names, interned to integers.
  pub(crate) names: Names,
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
      merge_sets: Vec::new(),
      merge_fields: Vec::new(),
      merge_kids: Vec::new(),
      merge_roots: Vec::new(),
      merge_todo: Vec::new(),
      merge_path: Vec::new(),
      merge_rows: Vec::new(),
      merge_queue: Vec::new(),
      merge_seen: Vec::new(),
      merge_inputs: Vec::new(),
      merge_parts: Vec::new(),
      merge_bounds: Vec::new(),
      merge_stack: Vec::new(),
      merge_memo: Vec::new(),
      merge_slots: Vec::new(),
      merge_compare: Vec::new(),
      names: Names::new(),
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
    self.merge_sets.clear();
    self.merge_fields.clear();
    self.merge_kids.clear();
    self.merge_roots.clear();
    self.merge_todo.clear();
    self.merge_path.clear();
    self.merge_rows.clear();
    self.merge_queue.clear();
    self.merge_seen.clear();
    self.merge_inputs.clear();
    self.merge_parts.clear();
    self.merge_bounds.clear();
    self.merge_stack.clear();
    self.merge_memo.clear();
    // The probe table is emptied by refilling it, not by shrinking it: its size is the working
    // set's, and a `clear` would throw that away and make the next document allocate again.
    self.merge_slots.fill(NONE);
    self.merge_compare.clear();
    self.names.reset();
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
      + self.merge_sets.capacity()
      + self.merge_fields.capacity()
      + self.merge_kids.capacity()
      + self.merge_roots.capacity()
      + self.merge_todo.capacity()
      + self.merge_path.capacity()
      + self.merge_rows.capacity()
      + self.merge_queue.capacity()
      + self.merge_seen.capacity()
      + self.merge_inputs.capacity()
      + self.merge_parts.capacity()
      + self.merge_bounds.capacity()
      + self.merge_stack.capacity()
      + self.merge_memo.capacity()
      + self.merge_slots.capacity()
      + self.merge_compare.capacity()
      + self.names.capacity()
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
  fn the_interner_round_trips_and_survives_a_reset() {
    use super::Names;

    let mut names = Names::new();
    let a = names.intern(b"hero");
    let b = names.intern(b"hero");
    let c = names.intern(b"heroes");
    assert_eq!(a, b, "the same name must intern to the same id");
    assert_ne!(a, c, "a different name must not");
    assert_eq!(names.len(), 2);

    // Past the initial probe table, so the growth path is on the measured path rather than a
    // branch nothing takes.
    for index in 0..500u32 {
      let key = std::format!("field{index}");
      assert_eq!(names.intern(key.as_bytes()), 2 + index);
    }
    for index in 0..500u32 {
      let key = std::format!("field{index}");
      assert_eq!(
        names.intern(key.as_bytes()),
        2 + index,
        "growth lost an entry"
      );
    }
    assert_eq!(names.len(), 502);

    // A reset empties it without giving the memory back, which is the whole contract.
    let capacity = names.capacity();
    names.reset();
    assert_eq!(names.len(), 0);
    assert_eq!(names.capacity(), capacity, "reset must not free");
    assert_eq!(names.intern(b"heroes"), 0, "ids restart after a reset");
  }

  /// Names are not text: a `&[u8]` document may spell one with bytes that are not UTF-8, and the
  /// interner is byte-keyed precisely so that it does not care.
  #[test]
  fn the_interner_is_byte_keyed() {
    use super::Names;

    let mut names = Names::new();
    let a = names.intern(&[0xff, 0x00, b'a']);
    let b = names.intern(&[0xff, 0x00, b'b']);
    assert_ne!(a, b);
    assert_eq!(names.intern(&[0xff, 0x00, b'a']), a);
    assert_eq!(names.intern(b""), 2, "the empty key is a key");
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
