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

/// A running work total and the ceiling it is charged against.
///
/// [`Budget`] is what a caller sets; this is what the engine spends against it. It exists as a type
/// rather than a pair of fields on the walker because the tables in this module have loops whose
/// length the **document** decides, and a loop like that must not be reachable without something to
/// charge. [`Names::intern`] takes one, so there is no way into its chain walk that does not carry
/// the ledger with it — the shape `graphql-proto` arrived at for the same reason, and the shape
/// al8n/smear#196 found missing here.
///
/// Every method charges *before* the step it prices. A charge taken afterwards bounds nothing: the
/// work is already spent by the time the counter can refuse it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Work {
  spent: u32,
  limit: u32,
}

impl Work {
  /// A fresh ledger against `limit`.
  #[inline]
  pub(crate) const fn new(limit: u32) -> Self {
    Self { spent: 0, limit }
  }

  /// Charges `units` and answers whether the engine may continue.
  ///
  /// Saturating, so a document large enough to overflow the counter refuses rather than wrapping
  /// back under the ceiling.
  #[inline]
  pub(crate) fn take(&mut self, units: u32) -> bool {
    self.spent = self.spent.saturating_add(units);
    self.spent <= self.limit
  }

  /// What has been charged so far.
  ///
  /// Counted independently of any structure's own idea of what it did, which is what lets a test
  /// say the charge and the work agree rather than say it of itself.
  #[cfg(test)]
  #[inline]
  pub(crate) const fn spent(&self) -> u32 {
    self.spent
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
  /// The next entry in the same bucket; [`NONE`] ends the chain.
  pub(crate) next: u32,
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
///
/// # It chains rather than probes, and the reason is the budget and not the speed
///
/// An open-addressed table's *rehash* walks a probe run per entry, so its cost is decided by the
/// same collisions the lookup is, and there is no amount that can be charged for it in advance. A
/// chained table's relink is one step per name and no probe run at all, so [`Names::intern`] can
/// charge for it *before* it happens and refuse instead of starting it. That is what makes the
/// whole structure's construction bounded by [`Budget::merge_work`] rather than by the hash
/// behaving, and it is the shape `graphql-proto`'s executor interner already had. al8n/smear#196.
#[derive(Debug, Default, Clone)]
pub(crate) struct Names {
  /// Every interned name's bytes, concatenated.
  bytes: Vec<u8>,
  /// Name id to its `(start, end)` range in [`Names::bytes`].
  ranges: Vec<(u32, u32)>,
  /// The next id in the same bucket, parallel to [`Names::ranges`]; [`NONE`] ends a chain.
  chain: Vec<u32>,
  /// Bucket heads, power-of-two, [`NONE`] when the bucket is empty.
  heads: Vec<u32>,
}

impl Names {
  /// The smallest bucket table the interner builds.
  const MIN_BUCKETS: usize = 64;

  /// Creates an empty interner, allocating nothing.
  #[inline]
  pub(crate) const fn new() -> Self {
    Self {
      bytes: Vec::new(),
      ranges: Vec::new(),
      chain: Vec::new(),
      heads: Vec::new(),
    }
  }

  /// Empties the interner, keeping every allocation.
  pub(crate) fn reset(&mut self) {
    self.bytes.clear();
    self.ranges.clear();
    self.chain.clear();
    self.heads.fill(NONE);
  }

  /// Returns how many rows the interner is holding capacity for.
  pub(crate) fn capacity(&self) -> usize {
    self.bytes.capacity() + self.ranges.capacity() + self.chain.capacity() + self.heads.capacity()
  }

  /// Returns how many distinct names have been interned.
  #[cfg(test)]
  pub(crate) fn len(&self) -> usize {
    self.ranges.len()
  }

  /// Returns `key`'s id, interning it if this is the first time it has been seen, or [`None`]
  /// when `work` refuses.
  ///
  /// # The chain walk is charged, one unit per entry, before the entry is compared
  ///
  /// The keys are **response names and field names out of the executable document**, so a client
  /// chooses every byte of them. [`hash_bytes`] is unkeyed and each of its rounds is invertible in
  /// the word it folds, so a pile-up in one bucket is *constructible* rather than unlucky, and it
  /// stays that way: a deterministic search over `q<decimal>` aliases reaches 512 valid names in
  /// one bucket of 1,024 after 450,077 candidates, against the hash as it ships today.
  ///
  /// Against the version of this table that did not charge — open-addressed, and probing without a
  /// ledger — those 512 names cost 130,816 insertion probes and 96,844 rehash probes: **227,660
  /// steps against the 512 selections `fill_merge_set` had charged for**, which is what made
  /// `merge_work` not a bound on CPU work at all. al8n/smear#196.
  ///
  /// A charge per compared entry makes that spend the client's budget instead of the server's
  /// time, and it bounds the *chain* and not merely one walk: putting an `L`th name into a bucket
  /// first walks the `L - 1` already there, so building a run of length `L` costs about `L²/2` and
  /// an adversary reaches `√(2 · work)` and no further. It is the same bound, taken for the same
  /// reason, that `graphql-proto`'s executor interner takes.
  ///
  /// **Before the work, not after it.** A charge taken after the walk it prices has already let
  /// the walk happen; the counter notices a run it cannot un-spend. That is why the refusal is a
  /// [`None`] the caller has to handle rather than a total read back afterwards, and why the
  /// relink below is charged from a count taken *before* it runs.
  pub(crate) fn intern(&mut self, key: &[u8], work: &mut Work) -> Option<u32> {
    let hash = hash_bytes(key);
    if !self.heads.is_empty() {
      let mut id = self.heads[self.bucket(hash)];
      while id != NONE {
        if !work.take(1) {
          return None;
        }
        let (start, end) = self.ranges[id as usize];
        if &self.bytes[start as usize..end as usize] == key {
          return Some(id);
        }
        id = self.chain[id as usize];
      }
    }

    // A miss inserts, and an insert past the load factor relinks every name. Relinking is exactly
    // one step per name — no probe run, which is the whole reason this table chains — so the cost
    // is known before it is paid and is charged here rather than discovered inside the loop.
    let relink = if self.ranges.len() + 1 > self.heads.len() {
      self.ranges.len() as u32 + 1
    } else {
      0
    };
    if !work.take(relink) {
      return None;
    }

    // Nothing above this line mutated anything, so a refusal leaves the interner exactly as it
    // was: there is no half-built state for a later call to read.
    let start = self.bytes.len() as u32;
    self.bytes.extend_from_slice(key);
    let id = self.ranges.len() as u32;
    self.ranges.push((start, self.bytes.len() as u32));
    self.chain.push(NONE);
    if self.ranges.len() > self.heads.len() {
      self.relink();
    } else {
      let bucket = self.bucket(hash);
      self.chain[id as usize] = self.heads[bucket];
      self.heads[bucket] = id;
    }
    Some(id)
  }

  /// The bucket `hash` lands in. Never called with [`Names::heads`] empty.
  #[inline]
  fn bucket(&self, hash: u64) -> usize {
    (hash as usize) & (self.heads.len() - 1)
  }

  /// Doubles the bucket table and relinks every name, one step each.
  fn relink(&mut self) {
    let next = self
      .heads
      .len()
      .max(Self::MIN_BUCKETS)
      .max(self.ranges.len().next_power_of_two());
    self.heads.clear();
    self.heads.resize(next, NONE);
    for id in 0..self.ranges.len() {
      let (start, end) = self.ranges[id];
      let bucket = self.bucket(hash_bytes(&self.bytes[start as usize..end as usize]));
      self.chain[id] = self.heads[bucket];
      self.heads[bucket] = id as u32;
    }
  }
}

/// FxHash-style multiply-fold over short keys, finished with an avalanche step.
///
/// The same shape `smear_schema::hash_bytes` has, and for the same reason: the keys are
/// identifiers, so one multiply per eight bytes is all the compression a probe table needs and it
/// costs no dependency.
///
/// # It is a *copy*, which is the whole reason this comment exists
///
/// al8n/smear#172 found the fold on its own mixing far too little for ordinary names — bit `j` of
/// `v · K` depends only on bits `0..=j` of `v`, so a name's late bytes decide nothing about where
/// it lands — and added splitmix64's finalizer to `smear-schema`'s copy. Nothing propagated here,
/// because this is a private duplicate of that function rather than a call to it. A reader who
/// assumes the shared dependency carried the repair gets the opposite of what is true.
///
/// This table was the **worse** of the two. `smear-schema` masks the hash's high half through
/// `smear_schema::bucket`; this one masks the low half, which is the least mixed word in the
/// product. Measured over 4,096 names of the most ordinary spelling there is — `k0` to `k4095` —
/// the unfinished hash masked low occupies **ten** buckets of 4,096 and costs 464.27 comparisons
/// per interned name. Through the finalizer the same names cost 0.50 across 2,586 buckets. Hence
/// the finalizer here too, and hence not simply calling the other crate's: a probe hash is an
/// internal detail, and importing one would make it a compatibility surface.
///
/// The `h ^= h >> 32` between rounds arrived the same way and for the same reason (al8n/smear#196):
/// the multiply leaves a chunk's late bytes in the high bits, `rotate_left(5)` delivers exactly
/// those bits to the low byte the *next* chunk's first byte occupies, and the two cancel — so
/// `x00000009` and `x00000084` hashed identically, and 4,096 eight-digit base-36 aliases produced
/// 1,660 hashes. Folding the high half down before the next round is what stops a difference from
/// living in a window one input byte can erase. It costs nothing for the names under nine bytes
/// that dominate here, because the loop it sits in does not run for them.
///
/// It fixes what honest names cost. It does **not** bound what chosen ones do: each round is
/// invertible in the word it folds and the finalizer is a bijection, so a caller who interns
/// document text still owes an argument that its probe runs are bounded by something other than the
/// hash. [`Names::intern`] now makes that argument — it charges [`Work`] one unit per entry a chain
/// walk compares, before comparing it — and until al8n/smear#196 it did not: draft 5.3.2's index
/// charged `selections().len()` before interning, which counts the names but not the entries
/// finding one compares.
#[inline]
pub(crate) fn hash_bytes(bytes: &[u8]) -> u64 {
  const K: u64 = 0x517c_c1b7_2722_0a95;
  let mut h: u64 = 0;
  let (chunks, rest) = bytes.as_chunks::<8>();
  for chunk in chunks {
    h = (h.rotate_left(5) ^ u64::from_le_bytes(*chunk)).wrapping_mul(K);
    h ^= h >> 32;
  }
  let mut tail = [0u8; 8];
  tail[..rest.len()].copy_from_slice(rest);
  let value = u64::from_le_bytes(tail) ^ ((rest.len() as u64) << 56);
  finalize((h.rotate_left(5) ^ value).wrapping_mul(K))
}

/// splitmix64's finalizer, so that every input bit reaches every output bit.
///
/// A bijection on `u64`, so it maps no key onto another — it only moves where a key lands.
#[inline]
const fn finalize(mut hash: u64) -> u64 {
  hash ^= hash >> 30;
  hash = hash.wrapping_mul(0xbf58_476d_1ce4_e5b9);
  hash ^= hash >> 27;
  hash = hash.wrapping_mul(0x94d0_49bb_1331_11eb);
  hash ^ (hash >> 31)
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
  /// Bucket heads over [`Scratch::merge_memo`], power-of-two, [`NONE`] when the bucket is empty.
  ///
  /// Chained for the reason [`Names`] is: a relink is one step per entry and no probe run, so the
  /// walk that rebuilds it can be charged before it starts rather than discovered inside it.
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
  use super::{Budget, Scratch, Work, get_bit, reset_bits, set_bit};
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

  /// An interner with nothing to refuse it, for the cases that are about identity and not cost.
  fn unbounded() -> Work {
    Work::new(u32::MAX)
  }

  #[test]
  fn the_interner_round_trips_and_survives_a_reset() {
    use super::Names;

    let mut work = unbounded();
    let mut names = Names::new();
    let a = names.intern(b"hero", &mut work).expect("budget");
    let b = names.intern(b"hero", &mut work).expect("budget");
    let c = names.intern(b"heroes", &mut work).expect("budget");
    assert_eq!(a, b, "the same name must intern to the same id");
    assert_ne!(a, c, "a different name must not");
    assert_eq!(names.len(), 2);

    // Past the initial bucket table, so the growth path is on the measured path rather than a
    // branch nothing takes.
    for index in 0..500u32 {
      let key = std::format!("field{index}");
      assert_eq!(
        names.intern(key.as_bytes(), &mut work),
        Some(2 + index),
        "growth lost an entry"
      );
    }
    for index in 0..500u32 {
      let key = std::format!("field{index}");
      assert_eq!(
        names.intern(key.as_bytes(), &mut work),
        Some(2 + index),
        "growth lost an entry"
      );
    }
    assert_eq!(names.len(), 502);

    // A reset empties it without giving the memory back, which is the whole contract.
    let capacity = names.capacity();
    names.reset();
    assert_eq!(names.len(), 0);
    assert_eq!(names.capacity(), capacity, "reset must not free");
    assert_eq!(
      names.intern(b"heroes", &mut work),
      Some(0),
      "ids restart after a reset"
    );
  }

  /// Names are not text: a `&[u8]` document may spell one with bytes that are not UTF-8, and the
  /// interner is byte-keyed precisely so that it does not care.
  #[test]
  fn the_interner_is_byte_keyed() {
    use super::Names;

    let mut work = unbounded();
    let mut names = Names::new();
    let a = names
      .intern(&[0xff, 0x00, b'a'], &mut work)
      .expect("budget");
    let b = names
      .intern(&[0xff, 0x00, b'b'], &mut work)
      .expect("budget");
    assert_ne!(a, b);
    assert_eq!(names.intern(&[0xff, 0x00, b'a'], &mut work), Some(a));
    assert_eq!(
      names.intern(b"", &mut work),
      Some(2),
      "the empty key is a key"
    );
  }

  /// `count` aliases that share one bucket of `mask + 1`, searched for against the **shipped**
  /// hash.
  ///
  /// Derived rather than listed on purpose. A hard-coded set stops colliding the moment the hash
  /// moves, and the test then goes green over exactly the work it exists to price — the defect's
  /// signature here is *absence*, so the case has to be re-derived from whatever the hash currently
  /// is. Every name is a valid draft §2.1.9 `Name`, so this is a document a client can send.
  fn colliding_aliases(mask: u64, count: usize) -> Vec<std::string::String> {
    let mut by_bucket: Vec<Vec<std::string::String>> = std::vec![Vec::new(); mask as usize + 1];
    for candidate in 0u64..8_000_000 {
      let name = std::format!("q{candidate}");
      let at = (super::hash_bytes(name.as_bytes()) & mask) as usize;
      by_bucket[at].push(name);
      if by_bucket[at].len() == count {
        let found = core::mem::take(&mut by_bucket[at]);
        assert!(
          found
            .iter()
            .all(|name| crate::schema::is_name(name.as_bytes())),
          "the search must produce names a document can spell"
        );
        return found;
      }
    }
    panic!("no bucket of {} reached {count} names", mask + 1);
  }

  /// A pile-up a client can construct costs what it compares, and the ledger stops it.
  ///
  /// # Why this is a gate and not a demonstration
  ///
  /// [`super::hash_bytes`] is unkeyed and each round is invertible in the word it folds, so a set
  /// of names sharing one bucket is *constructible* rather than unlucky. Until al8n/smear#196 the
  /// chain walk was uncharged, and 512 such aliases — the search below finds them in well under a
  /// million candidates — cost **130,816 comparisons** against the 512 selections
  /// `fill_merge_set` had charged for. `merge_work` was therefore not a bound on the work at all.
  ///
  /// **The plant.** Delete the `work.take(1)` from the chain walk and the first half of this test
  /// reads 452 instead of 131,268 — the relinks alone — while the second half stops refusing and
  /// interns all 512.
  ///
  /// The two-sided shape is what makes it a gate rather than a ceiling nobody can hit: the *same
  /// count* of ordinary aliases passes the same ledger the constructed ones exhaust.
  #[test]
  fn a_constructed_pile_up_is_charged_and_refused() {
    use super::Names;

    /// 512 names live in 1,024 buckets, and a set sharing a bucket under the widest mask the table
    /// reaches shares one under every narrower mask it grew through.
    const RUN: usize = 512;
    const MASK: u64 = 1023;
    /// `0 + 1 + … + 511`: the `L`th name into a chain walks the `L - 1` already there.
    const COMPARES: u32 = (RUN * (RUN - 1) / 2) as u32;
    /// One step per name at each doubling: 1, then 65, 129 and 257.
    const RELINKS: u32 = 1 + 65 + 129 + 257;

    let names = colliding_aliases(MASK, RUN);

    let mut work = unbounded();
    let mut table = Names::new();
    for (index, name) in names.iter().enumerate() {
      assert_eq!(
        table.intern(name.as_bytes(), &mut work),
        Some(index as u32),
        "a colliding name is still a distinct name"
      );
    }
    assert_eq!(
      work.spent(),
      COMPARES + RELINKS,
      "{RUN} names in one bucket compare {COMPARES} entries and relink {RELINKS} times; a total        of {RELINKS} says the walk is not charged at all"
    );

    // The ceiling an adversary reaches is `sqrt(2 * work)` and no further, so a budget well under
    // the pile-up's cost stops partway through it.
    const CEILING: u32 = 8192;
    let mut work = Work::new(CEILING);
    let mut table = Names::new();
    let refused = names
      .iter()
      .position(|name| table.intern(name.as_bytes(), &mut work).is_none())
      .expect("the ledger must refuse before the run is exhausted");
    assert!(
      refused < RUN,
      "{refused} of {RUN} interned under a ceiling of {CEILING}"
    );

    // Same count, ordinary spelling, same ceiling: it serves. A bound that refused this too would
    // be a bound on documents rather than on abuse.
    let mut work = Work::new(CEILING);
    let mut table = Names::new();
    for index in 0..RUN {
      let key = std::format!("q{index}");
      assert!(
        table.intern(key.as_bytes(), &mut work).is_some(),
        "{RUN} ordinary aliases must fit a ceiling of {CEILING}; refused at {index}"
      );
    }
  }

  /// A refused intern leaves the interner exactly as it was.
  ///
  /// This is the property that decided the structure. An open-addressed table's rehash walks a
  /// probe run per entry, so a budget can only stop it *inside* the walk — which leaves the slots
  /// describing a prefix of the arena, and a later lookup then misses a name that is present. A
  /// chained relink is one step per name, so its whole cost is charged before it starts and a
  /// refusal happens with nothing yet moved.
  #[test]
  fn a_refused_intern_moves_nothing() {
    use super::Names;

    let mut work = Work::new(4);
    let mut table = Names::new();
    for index in 0..64u32 {
      let key = std::format!("n{index}");
      if table.intern(key.as_bytes(), &mut work).is_none() {
        break;
      }
    }
    let interned = table.len();

    // Whatever it managed, every one of those names is still findable and still has its own id.
    let mut work = unbounded();
    for index in 0..interned as u32 {
      let key = std::format!("n{index}");
      assert_eq!(
        table.intern(key.as_bytes(), &mut work),
        Some(index),
        "a refusal lost an entry the table had already taken"
      );
    }
    assert_eq!(table.len(), interned, "a refusal left a half-written name");
  }

  /// [`hash_u32`] spreads the row-id sequences the merge memo keys on.
  ///
  /// `claim` charges its probe walk because the *loop* is one a document decides the length of, not
  /// because the hash misbehaves — and this is the half of that sentence that can be checked. The
  /// keys are ordinals the walk assigns, so the shapes below are the ones a document produces: one
  /// row, two adjacent rows, and runs of four and eight.
  ///
  /// Anything at or near `8192 · (1 − e^(−1/2)) ≈ 3,224` is a hash behaving. This is a canary
  /// rather than a bound: if it ever fires, the charge is still the bound and what has changed is
  /// what an honest document pays.
  #[test]
  fn the_memo_hash_spreads_ordinary_row_sequences() {
    use super::hash_u32;

    const SETS: u32 = 4096;
    const SLOTS: usize = 8192;
    /// Two thirds of the 3,224 an ideal hash occupies.
    const FLOOR: usize = 2149;

    for run in [1u32, 2, 4, 8] {
      let mut seen = std::vec![false; SLOTS];
      for first in 0..SETS {
        let mut state = 0xcbf2_9ce4_8422_2325u64 ^ u64::from(run);
        for row in first..first + run {
          state = hash_u32(state, row);
        }
        seen[(state as usize) & (SLOTS - 1)] = true;
      }
      let occupied = seen.iter().filter(|hit| **hit).count();
      assert!(
        occupied >= FLOOR,
        "runs of {run}: {SETS} sets occupy {occupied} buckets of {SLOTS}, under a floor of {FLOOR} \
         and against the 3,224 an ideal hash occupies"
      );
    }
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
