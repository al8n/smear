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

/// How much work validation may do before a document is refused.
///
/// Every knob is **absolute**, never proportional to the input: an attacker chooses the input, so
/// a proportional bound is not a bound. On a trip the validator emits its own rule and fails the
/// document — never "passes unvalidated", never panics.
///
/// This is a *validation* budget and has nothing to do with the parser's recursion budget. They
/// bound different resources — parse-time nesting against validation-time work — and are set
/// independently. Nor may one be leaned on for the other: the AST constructors are public, so a
/// caller can hand [`validate_executable`](super::validate_executable) a document no parser ever
/// saw and no parser limit ever bounded. Nothing here rests on a document having provenance.
///
/// # What the three knobs count
///
/// - [`merge_depth`](Budget::merge_depth) bounds how deeply the merge recursion may nest. One
///   level is one field-nesting level of the *response shape*, so it is the same quantity
///   `serde_json` bounds when it deserialises the answer.
/// - [`merge_work`](Budget::merge_work) bounds everything else *inside draft 5.3.2*, as one
///   running total: expanded field rows, pair comparisons, the rows a common-parent partition
///   duplicates, and the tree steps a node resolution walks. Depth alone does not bound the engine
///   — breadth times fragment reuse does — so this is the knob that caps that rule's worst case.
/// - [`validation_work`](Budget::validation_work) bounds **every other pass**, as one running
///   total for the whole call including the projection the lossless door runs before any rule
///   does. One unit is one node examined; one more is charged per eight bytes of every
///   document-chosen name a pass reads, because a name has no length ceiling and where two names
///   differ decides what comparing them costs.
///
/// A document that exceeds any of the three is **refused**, with
/// [`Rule::MergeDepthBudget`](super::Rule::MergeDepthBudget),
/// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) or
/// [`Rule::ValidationWorkBudget`](super::Rule::ValidationWorkBudget) naming which — when that
/// bound's rule is in the [`RuleSet`](super::RuleSet) — and
/// [`Invalid::budget_tripped`](super::Invalid::budget_tripped) set on the verdict either way. The
/// refusal does not depend on the rule being enabled; only being told which bound does.
///
/// The third knob is not redundant with the first two, and the reason is *pass order*: draft
/// 5.3.2 runs **last**, so the merge budget is consulted after every other pass has already
/// finished spending. Measured, a 129 KB document of 3,200 operations spreading one 3,200-field
/// fragment spent 189 ms in the selection walk with the merge rules fully enabled, and tripped
/// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) afterwards, on work already done.
///
/// # Turning the validation bound off
///
/// Set [`validation_work`](Budget::validation_work) to [`u32::MAX`]. That is the supported
/// spelling of "never refuse for this resource", and it is the only one: an empty
/// [`RuleSet`](super::RuleSet) switches off the bound's *diagnostic*, not the bound. A caller who
/// wants a validator that cannot refuse — an offline linter over trusted input, a test harness —
/// sets the knob, not the rules.
///
/// It is a **state** and not a large number. The validator turns it into one the moment the budget
/// is read, so no charge, and no prepayment the lossless door subtracts before validation starts,
/// can wear it down into a merely-large finite budget. That is the fourth counter in this
/// repository to need saying: a maximum is not an absence.
///
/// The same sentence is **not** made here about [`merge_depth`](Budget::merge_depth) and
/// [`merge_work`](Budget::merge_work). Their engine reads them itself, and what a maximum means
/// there is that engine's to say.
///
/// # What [`validation_work`](Budget::validation_work) does not bound: the caller's `Clone`
///
/// A diagnostic carries the spelling it is about, as a value of the document's own source type —
/// which is how a caller holding an owned source gets that spelling back at all. Producing it is a
/// `S::clone`, and the validator charges the **name's bytes** in front of every one of them.
///
/// That bounds two things and not a third. It bounds **how many** clones a run can make, because a
/// clone needs a node and every node is charged before it is read. It bounds the length of the name
/// each clone names. It does **not** bound what `S::clone` does: the bound on this type is
/// `AsRef<[u8]> + Clone`, and `Clone` carries no promise of any relationship to `as_ref().len()`.
///
/// For the source types this crate is written around the relationship holds and the ceiling is a
/// ceiling: `&str` and `&[u8]` clone in constant time, and `String` or `Vec<u8>` copy `L` bytes
/// against a charge of `L / 8` — a constant multiple. For a source type whose `Clone` is expensive
/// independently of its length, the ceiling bounds the count and nothing else, and the remaining
/// cost is the caller's own. That is the same standing this crate gives [`Sink`](super::Sink): a
/// caller-implemented behaviour on the validator's hot path, priced by what the validator can
/// measure and named where the measurement stops.
///
/// The alternative was to stop storing the spelling — a representation whose copy cost is
/// measurable, which is the move that has closed every other unknown here. It is not taken because
/// it removes a *capability* rather than a caveat: a diagnostic's span indexes bytes the AST owns,
/// not bytes an owned-source caller holds, so
/// [`Diagnostic::subject_source`](super::Diagnostic::subject_source) is the only way back to the
/// name. A cost contract on the source type was the other, and no signature can prove a `Clone` is
/// proportional to a length — it would be a documentation request wearing a trait bound's clothes,
/// on every public entry point of this crate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Budget {
  merge_depth: u32,
  merge_work: u32,
  validation_work: u32,
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

  /// The default bound on validation work outside draft 5.3.2.
  ///
  /// 2^22. Measured in these units: the four executable fixtures in
  /// `smear/tests/fixtures/executables` spend 4, 54, 124 and 789 — about a fifth of a unit per
  /// byte — so this admits over twenty megabytes of document written that way. Fragment reuse is
  /// where the units are a product rather than a sum, and it is the shape this knob is for: fifty
  /// operations sharing one two-hundred-field fragment is 3,451 bytes and 20,651 units, and even
  /// there the default admits about two hundred times what a real client sends.
  pub const DEFAULT_VALIDATION_WORK: u32 = 1 << 22;

  /// Creates a budget from the two merge knobs, leaving
  /// [`validation_work`](Budget::validation_work) at its default.
  ///
  /// Two parameters and not three, deliberately. This constructor predates the third knob and is
  /// spelled in a hundred call sites and fixtures; widening it would have moved every one of them
  /// to say nothing new. [`with_validation_work`](Budget::with_validation_work) is how the third
  /// is set, which is also how a reader can see at the call site that it was set at all.
  #[inline]
  pub const fn new(merge_depth: u32, merge_work: u32) -> Self {
    Self {
      merge_depth,
      merge_work,
      validation_work: Self::DEFAULT_VALIDATION_WORK,
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

  /// Returns the bound on validation work outside draft 5.3.2.
  #[inline]
  pub const fn validation_work(&self) -> u32 {
    self.validation_work
  }

  /// Returns the budget with a different bound on validation work outside draft 5.3.2.
  ///
  /// [`u32::MAX`] is the spelling of "do not refuse for this resource". It is not a bound and is
  /// not treated as one; it is here so that a caller who needs a validator that cannot refuse has
  /// a way to say so that does not involve switching off a diagnostic and hoping.
  #[inline]
  pub const fn with_validation_work(mut self, validation_work: u32) -> Self {
    self.validation_work = validation_work;
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
  /// # Checked, which is not the same as saturating
  ///
  /// This saturated, and the comment said that made an overflowing document refuse rather than
  /// wrap. True at every limit **except the largest one a caller can set**: saturation lands
  /// `spent` on exactly [`u32::MAX`], and `spent <= limit` with `limit == u32::MAX` is `true`. So
  /// the charge that overflowed passed, every later charge saturated onto the same value and
  /// passed as well, and a [`Budget::merge_work`] of `u32::MAX` — a number
  /// [`Budget::with_merge_work`] accepts — meant *no bound at all*: hashing consumed the nominal
  /// maximum and every relink, copy and comparison after it was free.
  ///
  /// [`Work::take_bytes`] is what made that reachable rather than arithmetic trivia, which is why
  /// it is repaired with the byte charges and not before them: a ledger over entries counts
  /// document nodes, and four billion nodes is not a document anybody can send, while a ledger
  /// over *bytes* reaches the same total from about thirty gigabytes of names.
  ///
  /// So the counter is checked, and [`u32::MAX`] is its poison rather than a value it may rest on:
  /// reaching it — by overflow or exactly — refuses and leaves `spent` there, so nothing after it
  /// can pass either. What that costs is one unit off the very top of the largest configurable
  /// budget, and it is the whole price of the strictness. al8n/smear#196.
  #[inline]
  pub(crate) fn take(&mut self, units: u32) -> bool {
    match self.spent.checked_add(units) {
      Some(spent) if spent < u32::MAX => {
        self.spent = spent;
        spent <= self.limit
      }
      // The total overflowed, or it landed on the poison value. Both refuse, and both leave the
      // counter where no later charge of any size the caller can name will pass either.
      _ => {
        self.spent = u32::MAX;
        false
      }
    }
  }

  /// Charges one pass over `len` bytes, and answers whether the engine may continue.
  ///
  /// The unit is [`byte_units`]: one per eight-byte chunk plus one for the tail. Hashing a key,
  /// comparing it against a stored one and copying it into an arena all read it once, at roughly a
  /// word a step, so the three share a unit — and a step of a chain walk, which is two integer
  /// comparisons, is the one unit `take` already charges.
  ///
  /// It exists because a ledger that counts *entries* while the entry's work is decided by a
  /// *length the client chose* is not a ledger over the work at all. That is the same defect
  /// al8n/smear#196 found in the chain walk, one dimension over: `k` names colliding in a bucket
  /// recorded `O(k²)` and ran `O(k² · L)`, and a GraphQL name has no local length ceiling.
  #[inline]
  pub(crate) fn take_bytes(&mut self, len: usize) -> bool {
    self.take(byte_units(len))
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
  ///
  /// # `usize` and not two `u32`s, which is a decision about what documents are admissible
  ///
  /// These were `u32`s, checked through an `arena_range` helper that answered [`None`] when the
  /// pair would not narrow — and that [`None`] was the *same* [`None`]
  /// [`Names::intern`](Names::intern) returns when the ledger refuses. The caller reads a refusal
  /// as [`Rule::MergeWorkBudget`](crate::Rule::MergeWorkBudget) and sets
  /// [`Invalid::budget_tripped`](crate::Invalid::budget_tripped), so a document whose only problem
  /// was that four gibibytes of names will not fit a `u32` was told to raise a knob that could not
  /// help it. Two different abandonments wearing one `None` — the shape
  /// [`Match`](crate::executable) was introduced for one level up, where a scan the budget stopped
  /// had not established that a name was absent. An arena that cannot represent a name has not
  /// established that the budget was exhausted.
  ///
  /// Of the two repairs, this is the one that removes the *condition* rather than widening the
  /// type that reports it: a `usize` range cannot fail to represent a slice of a `Vec<u8>`, so
  /// there is nothing left for the second variant to say. What it costs is eight bytes an entry on
  /// a 64-bit target — the per-name overhead beside the name's own bytes goes from twenty to
  /// twenty-eight. What it buys is that a caller who raised
  /// [`Budget::merge_work`](crate::Budget::merge_work) far enough to admit such a document gets the
  /// document validated rather than a verdict naming a ceiling they had already raised. That is
  /// this crate's own posture about ceilings: the knob is the bound, and a limit no caller can
  /// reach past is not one.
  ranges: Vec<(usize, usize)>,
  /// Each name's whole [`hash_bytes`], parallel to [`Names::ranges`].
  ///
  /// Eight bytes a name, bought for two things the ledger could not otherwise see. A chain step
  /// rejects a bucket collision on this word and the range's length, so it no longer runs a
  /// `memcmp` whose length the client picked — the one unit a step is charged buys a two-integer
  /// test, and the bytes are read only when they are about to be equal. And [`Names::relink`]
  /// reads it instead of re-hashing every stored byte, which is what makes "one step per name"
  /// true of the *work* rather than only of the loop count. al8n/smear#196.
  hashes: Vec<u64>,
  /// The next id in the same bucket, parallel to [`Names::ranges`]; [`NONE`] ends a chain.
  chain: Vec<u32>,
  /// Bucket heads, power-of-two, [`NONE`] when the bucket is empty.
  ///
  /// Emptied with [`Vec::clear`], never with a `fill` that keeps the high-water length: the
  /// logical length is what [`Names::intern`] charges growth against, so a length that outlived
  /// the previous document made this document's charge depend on that one. al8n/smear#196.
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
      hashes: Vec::new(),
      chain: Vec::new(),
      heads: Vec::new(),
    }
  }

  /// Empties the interner, keeping every allocation.
  ///
  /// # The bucket table is *cleared*, and that is the whole of a defect
  ///
  /// [`Vec::clear`] keeps the allocation and returns the length to zero; `fill` keeps both. The
  /// difference is invisible to the arena and decisive to the ledger, because the length is what
  /// [`Names::intern`] charges growth against: a table refilled at the high-water mark of every
  /// document this [`Scratch`] had ever seen made *this* document's relinks free. A cold run paid
  /// them at 1, 65, 129 …; the identical run behind a larger one paid none, so with a
  /// [`Budget::merge_work`] between the two totals the same schema, document, budget and rule set
  /// came back `Err` on a fresh working set and `Ok` on a reused one. Clearing rebuilds the table
  /// through [`Names::relink`], which is charged, so the charge stops depending on history.
  ///
  /// It also deletes the `fill` itself — an O(high-water) sweep that ran *before* the call's
  /// [`Work`] existed and so was charged to nobody at all. `clear` on a `Vec<u32>` is O(1).
  /// al8n/smear#196.
  pub(crate) fn reset(&mut self) {
    self.bytes.clear();
    self.ranges.clear();
    self.hashes.clear();
    self.chain.clear();
    self.heads.clear();
  }

  /// Returns how many rows the interner is holding capacity for.
  pub(crate) fn capacity(&self) -> usize {
    self.bytes.capacity()
      + self.ranges.capacity()
      + self.hashes.capacity()
      + self.chain.capacity()
      + self.heads.capacity()
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
  /// # And the bytes are charged too, because a name has no length ceiling
  ///
  /// A step of the walk is one unit; a `memcmp` of two `L`-byte names is `L`. Charging the first
  /// and running the second is the *same* defect one dimension over: `k` aliases sharing a bucket
  /// recorded `O(k²)` and ran `O(k² · L)`, with `L` a number the client writes, so a hostile
  /// document could scale both CPU and retained arena bytes without moving the ledger at all.
  ///
  /// Two changes close it, and the second is what makes the first cheap. Every pass over the key's
  /// bytes — hashing it, comparing it, copying it into the arena — is charged with
  /// [`Work::take_bytes`] *before* the pass. And the whole hash is stored, so a chain step tests
  /// two integers and reads no bytes: on the bucket collision that is the adversary's whole
  /// instrument, the byte charge is never even reached, and it is paid only when the bytes are
  /// about to be equal.
  ///
  /// **Before the work, not after it.** A charge taken after the walk it prices has already let
  /// the walk happen; the counter notices a run it cannot un-spend. That is why the refusal is a
  /// [`None`] the caller has to handle rather than a total read back afterwards, and why the
  /// relink below is charged from a count taken *before* it runs.
  pub(crate) fn intern(&mut self, key: &[u8], work: &mut Work) -> Option<u32> {
    // Reading the key is work whose length the document chose, so it is charged before the read
    // and not after it.
    if !work.take_bytes(key.len()) {
      return None;
    }
    let hash = hash_bytes(key);
    if !self.heads.is_empty() {
      let mut id = self.heads[self.bucket(hash)];
      while id != NONE {
        if !work.take(1) {
          return None;
        }
        // What that unit buys: two integers. A bucket collision — the constructible case, and the
        // only one an adversary has — is rejected here without touching a byte.
        let (start, end) = self.ranges[id as usize];
        if self.hashes[id as usize] == hash && end - start == key.len() {
          if !work.take_bytes(key.len()) {
            return None;
          }
          if &self.bytes[start..end] == key {
            return Some(id);
          }
        }
        id = self.chain[id as usize];
      }
    }

    // A miss inserts, and an insert past the load factor relinks every name. Relinking is one step
    // per name and one step's worth of work — no probe run, which is the whole reason this table
    // chains, and no re-hash, which is what storing the hash bought — so the cost is known before
    // it is paid and is charged here rather than discovered inside the loop. The copy into the
    // arena is the key's third and last read.
    // The arena's range needs no narrowing check, because there is no narrowing: a `usize` pair
    // names a slice of a `Vec<u8>` and cannot fail to. That is deliberate, and the field says what
    // it costs — the `u32` pair this replaced could refuse, and its refusal wore the same `None`
    // the ledger's does, so a document too large for the *arena* was reported against the *budget*.
    //
    // The addition below cannot overflow either, and at every pointer width rather than only the
    // one this happens to be built for: `self.bytes` and `key` are two allocations that are both
    // live at this instant, so their lengths sum to at most the address space. That is a property
    // of the machine and not of a ceiling somebody chose — which is exactly what the `u32` pair
    // was not, and why widening these is width-*safe* and not merely width-shaped. 32-bit is a
    // supported target here and `cross` builds this crate for four of them.
    //
    // The id is still a `u32`, and its narrowing cannot be reached rather than being argued not to
    // be: interning a name charges at least two units — one pass to hash it, one to copy it — and
    // `Work::take` poisons at `u32::MAX`, so no `Budget` any caller can construct admits more than
    // `u32::MAX / 2` names. `NONE` is `u32::MAX`, which is above that with a factor of two to
    // spare, and the arithmetic is pinned rather than described. The check stays regardless,
    // because a check that never fires costs one comparison and an argument that stops being true
    // costs a wrapped index. al8n/smear#196.
    let start = self.bytes.len();
    let end = start + key.len();
    let id = u32::try_from(self.ranges.len()).ok()?;

    let relink = if self.ranges.len() + 1 > self.heads.len() {
      id.saturating_add(1)
    } else {
      0
    };
    if !work.take(relink) || !work.take_bytes(key.len()) {
      return None;
    }

    // Nothing above this line mutated anything, so a refusal leaves the interner exactly as it
    // was: there is no half-built state for a later call to read.
    self.bytes.extend_from_slice(key);
    self.ranges.push((start, end));
    self.hashes.push(hash);
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
  ///
  /// One step, and one step's worth of *work*: the hash is read out of [`Names::hashes`] rather
  /// than recomputed. Re-hashing would have made this loop cost every stored byte in the table
  /// while [`Names::intern`] charged one unit per name for it — true about steps, false about
  /// work, which is precisely the accounting al8n/smear#196 exists to correct.
  fn relink(&mut self) {
    let next = self
      .heads
      .len()
      .max(Self::MIN_BUCKETS)
      .max(self.ranges.len().next_power_of_two());
    self.heads.clear();
    self.heads.resize(next, NONE);
    for id in 0..self.ranges.len() {
      let bucket = self.bucket(self.hashes[id]);
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
/// walk *rejects*, on two integers and before reading a byte, and [`byte_units`] for each pass it
/// does make over the key — and until al8n/smear#196 it did not: draft 5.3.2's index charged
/// `selections().len()` before interning, which counts the names but neither the entries finding
/// one compares nor the bytes comparing one reads.
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

/// The work one pass over `len` bytes costs: one unit per eight-byte chunk, plus one for the tail.
///
/// Read off [`hash_bytes`]'s own loop, which folds exactly that many rounds, and reused for the
/// `memcmp` and the `memcpy` of the same key because all three move about a word a step. Saturating
/// at [`u32::MAX`], so a key no ledger could pay for refuses rather than wrapping.
///
/// The merge engine charges with it too, and not only the interner. Pairing two argument lists or
/// two object literals by name is a scan over lengths the client wrote, so the scan cannot be one
/// unit an entry — but neither is it this many units an entry, which is what a *step* costs only
/// when the step reads the whole name. The engine hashes each scanned name once, charged here,
/// and a step then rejects on the stored hash and the stored length for one unit and no bytes;
/// this unit is charged again in front of the `memcmp` a step that agrees on both goes on to make.
#[inline]
pub(crate) const fn byte_units(len: usize) -> u32 {
  let units = len / 8 + 1;
  if units > u32::MAX as usize {
    u32::MAX
  } else {
    units as u32
  }
}

/// Mixes one `u32` into a running hash.
#[inline]
pub(crate) fn hash_u32(state: u64, value: u32) -> u64 {
  const K: u64 = 0x517c_c1b7_2722_0a95;
  (state.rotate_left(5) ^ u64::from(value)).wrapping_mul(K)
}

/// One name in a merge comparison's lookup index.
///
/// The whole hash is stored, not a bucket's worth of it, so a colliding candidate is rejected on
/// two integers and reads no bytes — the same trade [`Names`] makes, and the reason the byte charge
/// beside it is reached only where the bytes are about to be compared.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MergeName {
  /// The name's bytes, hashed whole.
  pub(crate) hash: u64,
  /// How many bytes the name is.
  pub(crate) len: usize,
  /// The next name in this one's bucket, relative to the index's own base, or [`NONE`].
  ///
  /// Linked so that a bucket's chain runs in *ascending* order. Two arguments may share a name —
  /// draft 5.4.2's business and not draft 5.3.2's — and a scan answered with the first of them,
  /// so a chain that walked them backwards would pair a different one and could settle the
  /// comparison differently.
  pub(crate) next: u32,
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
  /// Fragment ordinals already entered during the current walk. See [`Visited`].
  pub(crate) visited: Visited,
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
  /// Every name a merge comparison is about to look one of its own up against, as a stack.
  ///
  /// One segment per index: an argument list's two sides, and one object literal's fields for each
  /// live frame of [`Scratch::merge_compare`], so a literal nested inside an argument sits above
  /// the lists it was reached through and leaves with the frame that hashed it.
  ///
  /// It is here for the reason [`Names`] stores a hash beside each entry, and then for the reason
  /// [`Names`] buckets them. A lookup that rejects a candidate by comparing its bytes has to be
  /// charged for those bytes *before* it runs, which means charging every candidate the whole of
  /// the longest thing it could read; a lookup that rejects on two integers is charged one unit
  /// and reads the bytes only where they are about to matter. And a lookup that reaches the
  /// candidates through [`Scratch::merge_buckets`] does not visit them all: pairing two lists of
  /// `n` distinct names by scanning cost `n(n + 1)` steps a pair, which is quadratic in a width
  /// the client writes, and five identical selections of a hundred and twenty-eight short
  /// arguments spent 66,048 of a 65,536 budget on the steps alone. al8n/smear#196.
  pub(crate) merge_hashes: Vec<MergeName>,
  /// Bucket heads over [`Scratch::merge_hashes`], one power-of-two run per live index.
  ///
  /// Chained rather than probed, for the reason [`Names::heads`] is: a chain step is one unit
  /// whose cost is known before it is taken, and a colliding run can be abandoned in the middle of
  /// itself. The heads index a segment of [`Scratch::merge_hashes`] *relative to that segment's
  /// own base*, so a run built for one argument list means the same thing wherever the stack
  /// happens to have put it.
  pub(crate) merge_buckets: Vec<u32>,
  /// The [`Scratch::merge_fields`] row whose argument index currently sits at the bottom of
  /// [`Scratch::merge_hashes`] and [`Scratch::merge_buckets`], or [`NONE`].
  ///
  /// Draft 5.3.2's common-parent pass compares every member of a part against the *first* member,
  /// so one selection's arguments are the left side of every comparison the part makes. Indexing
  /// them once per part rather than once per pair is what keeps that side's cost proportional to
  /// the document rather than to the document times the part's width.
  ///
  /// It is set only after a complete build, so a refusal partway through one leaves this at
  /// [`NONE`] and the next call rebuilds rather than reading a segment that was never finished.
  pub(crate) merge_indexed: u32,
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
      visited: Visited::new(),
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
      merge_hashes: Vec::new(),
      merge_buckets: Vec::new(),
      merge_indexed: NONE,
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
    // Not cleared, and that is the point of the type: a mark is only current for one generation,
    // and retiring the generation makes every mark stale in `O(1)`. `Scratch::reset` clears without
    // freeing; this is that idea one step further, with nothing left to clear.
    self.visited.retire();
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
    // Cleared, not refilled — and `clear` does not give the allocation back, so the next document
    // still costs nothing to size. What refilling kept was the *length*, and the length is what
    // `claim` charges the memo's growth against: a table left at a previous document's high-water
    // mark made this document's relinks free, so the same request could be refused on a fresh
    // `Scratch` and served on a reused one. The sweep it also ran was O(high-water) and happened
    // before the call's `Work` existed, so nobody was charged for it. `Names::reset` carries the
    // same correction and the same reasoning. al8n/smear#196.
    self.merge_slots.clear();
    self.merge_compare.clear();
    self.merge_hashes.clear();
    self.merge_buckets.clear();
    self.merge_indexed = NONE;
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
      + self.merge_hashes.capacity()
      + self.merge_buckets.capacity()
      + self.names.capacity()
  }
}

/// Sizes a bitset to hold `bits` bits and clears it, without freeing.
pub(crate) fn reset_bits(words: &mut Vec<u64>, bits: usize) {
  let needed = bits.div_ceil(64);
  words.clear();
  words.resize(needed, 0);
}

/// The per-walk "already entered" set over fragment ordinals, as generation **stamps**.
///
/// # Why not a bitset
///
/// It was one, and clearing it was `O(F / 64)` writes at the top of **every** operation's walk and
/// every subscription's root collection. `O` operations against `F` fragments is `Θ(O · F / 64)` of
/// zeroing that no ledger saw, because the ledger charges what a pass *examines* and this is what a
/// pass *prepares* — a class no audit on this branch had a row for until al8n/smear#198's ninth
/// round. A valid document pairing many operations with many distinct fragments could spend
/// gigabytes on it while staying under the ceiling.
///
/// Pricing it was the alternative. Stamping removes it instead: a walk advances a counter, and a
/// mark from an earlier walk is stale because its number is. There is no clear to charge for, no
/// gate to get wrong, and nothing left at this site for a later round to find one level in — which
/// is the move that has actually ended things on this branch, from `Ledger::Off` to
/// `Option<Recovery>`.
///
/// Draft 5.3.2's engine already does this, one axis over: `Validator::generation` distinguishes one
/// fragment *expansion* from the next without clearing a bitset per expansion. This is the same
/// technique at the operation boundary rather than the expansion boundary.
///
/// # What it costs
///
/// Four bytes per fragment instead of one bit. In this buffer's own company that is not a new
/// order of cost: `Scratch::fragments` already holds a `FragmentRow` per fragment, several times
/// wider, and every other document-sized table here is `u32`-keyed.
///
/// # The objection that sinks generation counters elsewhere does not exist here
///
/// A generation outside a rollback set can miscount an operation that was undone — a stamp survives
/// while the state it described is rewound, and a later walk reads it as current. This counter
/// lives in a [`Scratch`] the validator owns for the length of one call, and validation has no
/// checkpoint, no speculation and no rollback: a walk either finishes or the whole run is abandoned
/// through [`ControlFlow::Break`](core::ops::ControlFlow::Break). There is nothing to rewind past.
#[derive(Debug, Clone, Default)]
pub(crate) struct Visited {
  /// The generation that last entered each fragment, indexed by ordinal.
  stamps: Vec<u32>,
  /// The generation the current walk is in. Zero before the first walk, and no stamp is ever
  /// written as zero, so a freshly grown entry reads as "not entered".
  current: u32,
}

impl Visited {
  /// An empty set.
  pub(crate) const fn new() -> Self {
    Self {
      stamps: Vec::new(),
      current: 0,
    }
  }

  /// Opens a walk over `fragments` ordinals, retiring every mark from the previous one.
  ///
  /// `O(1)` for a buffer that is already wide enough, which after the first walk of a run it is.
  /// Growth is amortised and bounded by the document's fragment count, which the prep sweep has
  /// already charged one name at a time.
  pub(crate) fn begin(&mut self, fragments: usize) {
    self.advance();
    if self.stamps.len() < fragments {
      self.stamps.resize(fragments, 0);
    }
  }

  /// Retires every mark without touching the buffer.
  pub(crate) fn retire(&mut self) {
    self.advance();
  }

  /// Marks `ordinal` entered, answering whether it already was.
  ///
  /// The answer for an ordinal outside the table is `true` — "already entered", so do not enter —
  /// which is what the bitset this replaces answered when the word was out of range.
  #[inline]
  pub(crate) fn visit(&mut self, ordinal: u32) -> bool {
    match self.stamps.get_mut(ordinal as usize) {
      Some(slot) if *slot == self.current => true,
      Some(slot) => {
        *slot = self.current;
        false
      }
      None => true,
    }
  }

  /// Entries the buffer has reserved room for.
  pub(crate) fn capacity(&self) -> usize {
    self.stamps.capacity()
  }

  /// Moves to the next generation, emptying the table if the counter has run out of them.
  ///
  /// Four billion walks on one [`Scratch`] is not reachable by any document the ledger admits, and
  /// the wrap is handled anyway: correctness that rests on a number being big enough is the defect
  /// this repository has now written under four different names.
  fn advance(&mut self) {
    self.current = match self.current.checked_add(1) {
      Some(next) => next,
      None => {
        self.stamps.clear();
        1
      }
    };
  }
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
    assert_eq!(budget.validation_work(), 1 << 22);
    assert_eq!(budget.with_validation_work(7).validation_work(), 7);
    // The knobs are independent; setting one must not disturb the others.
    assert_eq!(budget.with_merge_depth(8).merge_work(), 65_536);
    assert_eq!(budget.with_merge_depth(8).validation_work(), 1 << 22);
    assert_eq!(budget.with_validation_work(7).merge_work(), 65_536);
    // `Budget::new` predates the third knob and leaves it at the default rather than at zero,
    // which is what keeps every existing two-argument call site meaning what it meant.
    assert_eq!(Budget::new(1, 2).validation_work(), 1 << 22);
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

  /// A ceiling of [`u32::MAX`] is still a ceiling.
  ///
  /// [`Work::take`] saturated, and the comment above it said that made an overflowing document
  /// refuse rather than wrap. It did — at every limit but the one a caller reaches by asking for
  /// the most work they can ask for. Saturation lands `spent` on exactly [`u32::MAX`], and
  /// `spent <= limit` with `limit == u32::MAX` is **true**, so the charge that overflowed passed
  /// and so did every charge after it, each saturating onto the same value. `merge_work` at its
  /// public maximum bounded nothing at all.
  ///
  /// The byte charges are what put that in reach rather than leaving it arithmetic trivia: a
  /// ledger over entries counts document nodes and four billion nodes is not a document, while a
  /// ledger over *bytes* gets there from about thirty gigabytes of names.
  ///
  /// **The plant.** Restore `saturating_add` and the first `assert!(!…)` below fails — the ledger
  /// says yes to a charge that overflowed it — and so does every one after.
  #[test]
  fn an_overflowing_charge_refuses_at_the_largest_limit_too() {
    // The largest budget `Budget::with_merge_work` accepts.
    let mut work = Work::new(u32::MAX);
    assert!(
      work.take(u32::MAX - 1),
      "one unit short of the ceiling is inside it"
    );
    assert!(
      !work.take(1),
      "the charge that reaches the ceiling is refused, not admitted onto it"
    );
    assert!(
      !work.take(1),
      "and the ledger stays refused; a counter that saturates and then answers `true` has no \
       bound left to enforce"
    );
    assert!(!work.take(u32::MAX), "at any size");

    // A single charge no counter could hold refuses on its own, without a run-up.
    let mut work = Work::new(u32::MAX);
    assert!(!work.take(u32::MAX));
    assert!(!work.take(1));

    // Under an ordinary ceiling nothing moves: the limit is what refuses, exactly where it did.
    let mut work = Work::new(10);
    assert!(work.take(10), "spending the whole budget is spending it");
    assert!(!work.take(1));
    let mut work = Work::new(10);
    assert!(
      !work.take(u32::MAX),
      "and a charge that would overflow is refused here as it always was"
    );
  }

  /// The arena's narrowing is gone, and the one narrowing left cannot be reached.
  ///
  /// # What this fixture used to say, and why it stopped saying it
  ///
  /// It pinned `arena_range`. A name's slice endpoints were `u32`s over a `usize` arena, the gap
  /// between the two was reachable past four gibibytes of names, and the helper refused rather than
  /// letting an `as u32` wrap. The check was right; its *report* was not. The [`None`] it answered
  /// is the same [`None`] a ledger refusal answers, and [`Names::intern`]'s only caller reads a
  /// [`None`] as [`Rule::MergeWorkBudget`](crate::Rule::MergeWorkBudget) — so a document whose only
  /// problem was that its names will not fit a `u32` was told to raise a knob that could not help
  /// it, and `Invalid::budget_tripped` said a budget had refused when none had. Two abandonments
  /// wearing one `None`, which is the shape `Match` was introduced for one level up.
  ///
  /// [`Names::ranges`] is a `usize` pair now, so the condition that helper guarded does not exist
  /// and there is nothing left for it to check. The field says what the eight bytes an entry buy
  /// and what they cost.
  ///
  /// # What is left is the id, and the ledger is what makes it unreachable
  ///
  /// The entry id is still a `u32` against a [`NONE`] of [`u32::MAX`]. Interning a name charges at
  /// least two units — one pass to hash it and one to copy it — and [`Work::take`] poisons at
  /// [`u32::MAX`], so no [`Budget`] any caller can construct admits enough interns to reach the
  /// sentinel. That is derived below from the interner's own measured floor rather than restated
  /// from the comment, so an intern that got cheaper would move this fixture instead of quietly
  /// invalidating it.
  ///
  /// **The plant.** Make an intern cost one unit instead of two — delete either `take_bytes` — and
  /// the margin below halves; delete both and it vanishes, because a free intern is one the ledger
  /// cannot bound the count of.
  #[test]
  fn the_only_narrowing_left_cannot_be_reached() {
    use super::{NONE, Names};

    /// Enough distinct keys to get past the first bucket doubling, so the floor measured below is
    /// the steady-state one and not the first insert's.
    const RUN: u32 = 256;

    let mut work = unbounded();
    let mut table = Names::new();
    let mut floor = u32::MAX;
    for index in 0..RUN {
      let key = std::format!("{index}");
      let before = work.spent();
      table
        .intern(key.as_bytes(), &mut work)
        .expect("the budget is unbounded");
      floor = floor.min(work.spent() - before);
    }
    assert!(
      floor >= 2,
      "one intern costs {floor} units at its cheapest, and two is what the two passes over the key \
       are worth; below that the count of interns is not bounded by the ledger at all"
    );

    // Every unit any `Budget` can spend, because `Work::take` refuses at the poison and leaves the
    // counter there. The id narrows at `NONE`, and the ledger cannot pay for that many.
    let payable = u64::from(u32::MAX) / u64::from(floor);
    assert!(
      payable < u64::from(NONE),
      "the ledger can pay for {payable} interns and the id narrows at {NONE}, so the narrowing is \
       reachable — and its refusal would wear the same `None` the ledger's does, which is the \
       defect this fixture replaced"
    );
  }
  /// The charge a request pays must not depend on what the previous request left behind.
  ///
  /// [`Names::reset`] used to `fill` the bucket table with [`NONE`], which keeps its *length* as
  /// well as its allocation — and the length is exactly what `intern` charges growth against. So a
  /// cold table paid the relinks at 1, 65, 129 …, and the identical run behind a larger one paid
  /// none of them. With a [`Budget::merge_work`] between the two totals, the same schema,
  /// document, budget and rule set was refused on a fresh working set and served on a reused one.
  ///
  /// **The plant.** Put the `fill(NONE)` back in [`Names::reset`] and the warm total drops by the
  /// whole relink term — 195 for the 200 names below — while the cold one does not move.
  #[test]
  fn a_reset_charges_what_a_fresh_table_charges() {
    use super::Names;

    /// Past two doublings, so the relink term the warm run used to skip is 1 + 65 + 129 and not a
    /// rounding error.
    const RUN: u32 = 200;

    fn charge(table: &mut Names) -> u32 {
      let mut work = unbounded();
      for index in 0..RUN {
        let key = std::format!("subject{index}");
        table.intern(key.as_bytes(), &mut work).expect("budget");
      }
      work.spent()
    }

    let mut cold = Names::new();
    let cold_spent = charge(&mut cold);

    // A larger request first, on the same table, and then the same names again after a reset.
    let mut warm = Names::new();
    let mut prelude = unbounded();
    for index in 0..500u32 {
      let key = std::format!("prelude{index}");
      warm.intern(key.as_bytes(), &mut prelude).expect("budget");
    }
    let capacity = warm.capacity();
    warm.reset();
    assert_eq!(
      warm.capacity(),
      capacity,
      "clearing the table must not give the arena back — the reuse is the whole point"
    );

    let warm_spent = charge(&mut warm);
    assert_eq!(
      cold_spent, warm_spent,
      "{RUN} names cost {cold_spent} on a fresh table and {warm_spent} behind a larger request"
    );
    assert_eq!(warm.len(), RUN as usize, "the reused table lost a name");
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
