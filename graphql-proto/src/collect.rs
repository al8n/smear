//! Draft §6.3 `CollectFields`, and the response-key interner it groups into.
//!
//! # Grouping, and why the order survives
//!
//! `CollectFields` produces an *ordered* map from response key to the list of selections that
//! landed under it, and both halves of that matter: the key order is the field order of the
//! response object (draft §7.1.1 requires it to follow the query), and the list order is what
//! `MergeSelectionSets` concatenates.
//!
//! Neither is stored as a map. Selections are appended to a flat vector as `(group, field)`, and a
//! stable sort by group then makes every group a contiguous range, preserving document order inside
//! each. One vector, one sort, and no allocation per response key.
//!
//! # Every lookup here is a probe, and it used to be a scan
//!
//! Three of them, all on the same path and all linear in the document: the response key's entry in
//! [`Interner`], its group among the groups collected so far, and a fragment spread's definition
//! among the document's. They grew together — one group per newly interned key — so a selection set
//! of `n` distinct response keys cost `n²`, and **fixing one of them would have changed nothing**,
//! because the two left behind would still have been scanned once per selection. That is
//! al8n/smear#141, and the flat fragment chain it shares a path with; measured on this tree, 8,000
//! distinct keys spent 61 ms inside `start()` and a 50,000-link chain spent 2.1 s, both before the
//! first field request exists for a driver to refuse.
//!
//! All three are now indexed, and every entry a name lookup compares is charged to [`Visits`]
//! before it is compared — so [`Limits::max_selection_visits`](super::Limits::max_selection_visits)
//! bounds the **work** and not merely the number of selections looked at. There is no uncharged way
//! into either table: the budget is a parameter of [`Interner::intern`], which is what turned "no
//! document-derived name is interned for free" from a claim into a signature. See [`Visits`] for
//! what that closes and the one thing it cannot.
//!
//! # `visitedFragments` is per collection, not per operation
//!
//! Draft §6.3 threads `visitedFragments` through a single `CollectFields` call so a fragment
//! reached twice through different spreads is expanded once. It is deliberately *not* carried
//! across sibling object values: two elements of a list each collect their own fields, and a
//! fragment must expand in both.

use tokora::{SimpleSpan, span::AsSpan};

use smear_parser::graphql::ast::{
  Directive, Directives, ExecutableDocument, Field, FragmentDefinition, InputValue, Selection,
  SelectionSet,
};
use smear_schema::{Schema, TypeId, bucket, hash_bytes, is_name};

use super::{
  Values,
  error::{ConditionFault, Raw},
};

use groups::{Appending, Groups};
use table::Table;

/// Why [`Interner::intern`] could not store a name, and the ceiling that refused it.
///
/// Two, because the two degrade differently at the one caller that does not simply fail: a name is
/// still *readable* when the arena is full, and the collection path has a different message for
/// each.
///
/// # The ceiling travels with the cause, and that is what stops them being mismatched
///
/// Every diagnostic about a refusal names a number for the operator to act on, and that number is
/// only useful if it is the number that actually refused. Two call sites got that wrong in the same
/// way: they discarded the variant and reported the *arena's* ceiling whatever had happened, so a
/// caller whose `max_selection_visits` ran out was told to raise `max_interned_bytes`. Both had the
/// arena's cap in easy reach and the budget's limit somewhere else, which is the shape of mistake a
/// pairing invites.
///
/// So the limit is carried here, from the branch that knows which one it is. A render site cannot
/// pair the wrong number with the wrong cause, because it is not given a choice of numbers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Unstored {
  /// [`max_interned_bytes`](super::Limits::max_interned_bytes) has no room for the bytes.
  Arena {
    /// [`Limits::max_interned_bytes`](super::Limits::max_interned_bytes)'s value.
    limit: u32,
  },
  /// [`max_selection_visits`](super::Limits::max_selection_visits) had no room for the lookup.
  Budget {
    /// [`Visits::limit`]'s value.
    limit: u32,
  },
}

/// The work one pass over `len` bytes costs: one unit per eight-byte chunk, plus one for the tail.
///
/// Read off [`hash_bytes`]'s own loop, which folds exactly that many rounds, and reused for the
/// `memcmp` and the `memcpy` of the same key because all three move about a word a step.
/// Saturating at [`u32::MAX`], so a key no budget could pay for refuses rather than wrapping.
///
/// # Why the ledger needs a second unit at all
///
/// [`Visits`] counts *things looked at* — a selection examined, an entry a lookup compares, a
/// definition the index pass reads — and every one of those was charged one unit. That is a bound
/// on the number of comparisons and not on the comparisons, because the keys are the document's
/// and draft §2.1.9 puts no ceiling on a name's length: `k` aliases in one bucket recorded `O(k²)`
/// and ran `O(k² · L)`, with `L` a number the client writes. About 512 aliases of thirty-two
/// kilobytes fit under the default `max_interned_bytes`, and their 130,816 charged comparisons
/// moved about four gigabytes.
///
/// A single long key needs no collision at all: looked up once per object position, it hashes and
/// then `memcmp`s its whole length for the one or two units the entry costs, and positions are a
/// factor the query does not have to pay for twice.
///
/// So a pass over the bytes is charged in this unit before the pass, and the whole hash is stored
/// beside each entry so that the pass a bucket collision used to force is not made at all.
/// al8n/smear#172.
#[inline]
pub(super) const fn byte_units(len: usize) -> u32 {
  let units = len / 8 + 1;
  if units > u32::MAX as usize {
    u32::MAX
  } else {
    units as u32
  }
}

/// An empty open-addressing slot, an unterminated chain, and "this key has no group yet".
///
/// One sentinel for all three because all three are `u32` indexes into a table this module bounds
/// below `u32::MAX` — the interner by
/// [`max_interned_bytes`](super::Limits::max_interned_bytes), the groups by the visit budget, and
/// the fragment table by the document a parser already accepted.
const NONE: u32 = u32::MAX;

/// A `@skip`/`@include` condition that could not be read, and where in the document to point.
///
/// Collection stops at the first one, which is also what the reference implementation does — it
/// throws out of `collectFields`, so a selection set with two unreadable conditions produces one
/// error and not two.
pub(super) struct Fault<'a> {
  pub(super) raw: Raw,
  /// The `if` argument's value, or the directive itself when there is no `if` argument to point at.
  pub(super) location: SimpleSpan,
  /// A name the message wants, still as the document's own bytes.
  ///
  /// Deliberately not an interner id. A fault means this collection is about to be *undone* —
  /// every response key it interned goes back, because a key belonging to a field that never became
  /// a position must not spend a later sibling's budget. An id minted before that restore would
  /// point into bytes the restore removes, which is the truncation defect the checked interner
  /// exists to refuse. So the caller interns this *after* restoring, when the arena has room again
  /// and the id it gets back is one that will still be there.
  pub(super) name: Option<&'a [u8]>,
}

/// What is left of [`Limits::max_selection_visits`](super::Limits::max_selection_visits).
///
/// # It is per operation, and that is the whole point
///
/// A per-*call* budget would bound one selection set and nothing else, and `collect_fields` runs
/// once per object position — so the total would be `positions × budget`, with positions coming
/// from the driver's list lengths. That is the product shape that has already cost this module two
/// rounds. Counting down across the whole operation means a driver **cannot** amplify collection
/// work at all: the query alone decides how much walking there is to do.
///
/// It bounds [`walk`]'s explicit stack too, since a frame is only ever pushed by a selection that
/// was charged — which is why deleting the recursion needed no depth knob of its own.
///
/// # A unit of one selection was a budget on one factor of a product
///
/// Charging one unit per selection bounded how many selections were *examined* and said nothing
/// about what examining one costs, and the cost was linear in the document three separate ways —
/// the interner scan, the group scan and the fragment scan. `n` selections over `n` names is `n²`
/// of work under a budget that reads `n`. **Each factor was the query's and neither bound reached
/// their product**, which is the same shape [`max_response_slots`] met from the driver's side.
///
/// So the unit is now *work*: one for a selection examined, one for every entry a name lookup
/// compares, and one for every definition and fragment [`Fragments::build`] handles. Both indexes
/// hash text an adversary writes, and [`hash_bytes`] is unkeyed with every round invertible in the
/// word it folds, so a pile-up in one bucket is constructible rather than unlucky. Charging makes
/// that spend the client's budget instead of the server's time, and is why the bound does not rest
/// on the hash behaving. The mixing al8n/smear#172 and al8n/smear#196 added changed what an
/// *honest* document pays and nothing at all about this. (It is not a bijection over byte strings
/// and no 64-bit hash is: `x00000009` and `x00000084` hashed alike until #196, which is a defect
/// about honest cost and never was a claim about this bound.)
///
/// # Charged before the work, over the population as well as the lookup, and before the storage
///
/// Three properties, one per round that found the previous list short.
///
/// **Before the work.** [`spend`](Visits::spend) is called ahead of the work it pays for, so a
/// probe run is abandoned in the middle of a bucket rather than at the end of one. A charge taken
/// afterwards bounds nothing by itself: the run is already walked when the refusal arrives.
///
/// **Over the population.** [`Interner`] is filled *during* collection, so a client that builds a
/// collision pays for building it. [`Fragments`] was filled at executor construction, outside this
/// budget entirely, from names the document chooses — so the same argument was simply false there:
/// the collision came free and one cheap valid spread walked it. Its pass is now charged too.
///
/// **Before the storage.** Charging before the *work* is not yet charging before the *allocation*,
/// and [`Fragments::build`] had the first without the second: it learnt the fragment count by
/// populating the table, charged for what it had populated, and cleared the vector when the charge
/// was refused. A cleared vector keeps its capacity and the table outlives the operation, so the
/// refusal left an allocation nothing would reclaim. The answer was right on every call — which is
/// the point worth carrying, because a fixture that reads the answer cannot see this at all.
///
/// **The question to ask of every structure phases 2–8 add is therefore all three:** is the lookup
/// charged, *was the thing being looked in built inside the budget or outside it*, and is the
/// charge taken before the storage or merely before the work. A bound on the lookup is worth
/// nothing over a table an adversary filled for free, and a refusal that still reserves is a
/// ceiling on the answer and not on the cost.
///
/// # It is not "collection only" any more, and the reason is a lesson about residuals
///
/// It was, and the sentence that said so was: *"`expand`'s `__typename` and `handle_field_error`'s
/// message intern uncharged, and neither reads bytes a client chooses, so neither can lengthen a
/// run on purpose."* That is a **universal over callers**, and there were three callers, not two:
/// two more sites interned a variable's spelling out of the *executable document*, so a client
/// could name its variables into one bucket and walk an attacker-sized run per failed argument
/// coercion — after collection had already been admitted.
///
/// Writing a more careful universal would have been the wrong repair, because a universal over
/// callers does not survive new callers and phases 2–8 add them. **The uncharged entry point is
/// gone instead.** [`Interner::intern`] takes `&mut Visits`, so a caller who has no budget to give
/// cannot reach the probe loop, and "no document-derived name is interned uncharged" stopped being
/// a claim to verify and became a signature.
///
/// The form generalises: a residual worth writing down says *X is prevented by the type of Y*, not
/// *no caller does X*. Two of this module's three residuals have now been falsified by the round
/// that followed them, both of them universals.
///
/// # What it does not bound, in the form that survives a new caller
///
/// **The group lookup, which is not charged and is guarded by a capability instead.** It is a
/// direct index with no loop, so there is nothing to charge — and, equally, nothing a counter could
/// see if a scan came back: replace `keys[key as usize]` with a `position` over the groups and
/// `spent()` reads exactly what it read before, so
/// `distinct_response_keys_are_linear_however_they_are_spelled` passes over an uncharged quadratic.
/// A charge cannot guard this one.
///
/// A **type** can, and this residual used to say otherwise — that hiding the vector was impossible
/// because `expand` iterates it to commit the groups. It does, but not at the same time. The two
/// uses are phase-disjoint: [`walk`] only appends and reads one entry back by the index it was just
/// handed, while every iteration ([`collect_fields`]'s unmark and range-assign, and `expand`'s
/// commit) happens strictly before the walk begins or after it ends. Different capabilities in
/// disjoint phases are two types, not one contradictory one — so the walk is handed
/// [`Appending`], which has no iterator, no length and no path to the vector
/// behind it, and a scan by key is no longer something a walk can be written to do. See
/// [`Groups`].
///
/// **`Schema::sym`, which was examined, cleared, and cleared on the wrong axis.** `expand` and
/// [`applies`] probe the schema's [`NameIndex`](smear_schema::NameIndex) with *document* bytes.
/// The residual here used to end there: the table is populated from the **schema**, which the
/// operator wrote, so a client chooses the lookup key and never the run it walks — the same
/// question that condemned [`Fragments`], answered the other way.
///
/// Every word of that is true about the run's **length** and silent about what one probe costs. A
/// probe begins by hashing the key, and the key is the client's, so its length is too: a
/// megabyte-long type condition inside a fragment reached once per object position hashed a
/// megabyte for the one unit the selection was charged, and positions are a factor the query does
/// not pay for twice. Both probes are charged in [`byte_units`] now, before the hash. The lesson
/// is the same one this module keeps re-learning one dimension over: a residual that clears a site
/// has to name the axis it clears it on, because "a client cannot lengthen the run" and "a client
/// cannot lengthen the work" are different sentences. al8n/smear#172.
///
/// # State that survives `reset`, and exactly how much of it is enforced
///
/// The residual left by the round that separated the fragment charge from the fragment table said
/// *no state surviving `reset` moves a verdict*, and it was broader than what any gate could see:
/// the fragment charge was prevented by this type, and the rest was an enumeration of `reset` under
/// a totals gate that read `max_selection_visits` alone. A table kept across `reset` that fed one
/// of the other three ceilings would have passed everything in the suite. The claim is narrowed to
/// what is checked, and the check is widened to meet it.
///
/// **Enforced.** `a_second_operation_charges_what_the_first_did` runs one operation twice on one
/// executor and requires the two to agree on **every cumulative ceiling**, not just this one:
/// [`max_selection_visits`], [`max_response_slots`], [`max_response_metadata`] and
/// [`max_interned_bytes`], each read as the quantity its own check compares against. Any state a
/// second operation inherits and does not pay for turns one of those four rows red, whether or not
/// a ceiling happens to sit close enough for a caller to notice today.
///
/// **Named, and guarded by review.** Three things that gate does not reach.
///
/// [`max_in_flight`] has no cumulative total to compare: it bounds requests outstanding at an
/// instant, `reset` empties the slab, and the epoch bump that voids the previous operation's ids is
/// pinned separately. There is no population to inherit.
///
/// A **fifth** ceiling is not picked up on its own. `Charges` holds one field per accumulating
/// ceiling and nothing derives that list from [`Limits`](super::Limits), so a ceiling phases 2–8
/// add needs a field adding beside the others, or the gate silently stays four wide.
///
/// And the gate is a fixture, so it covers the state *its* document creates — fragments, the
/// interner, the groups, the slots and the metadata. A structure added by phases 2–8 and exercised
/// by no selection this query writes is not in it. That is the maintenance obligation the fixture
/// carries, and it is written on the fixture: a new table kept across `reset` needs a selection in
/// that document, or a gate of its own.
///
/// [`max_response_slots`]: super::Limits::max_response_slots
/// [`max_response_metadata`]: super::Limits::max_response_metadata
/// [`max_selection_visits`]: super::Limits::max_selection_visits
/// [`max_interned_bytes`]: super::Limits::max_interned_bytes
/// [`max_in_flight`]: super::Limits::max_in_flight
pub(super) struct Visits {
  left: u32,
  limit: u32,
  /// Whether this operation has already paid for [`Fragments::build`]'s index pass.
  ///
  /// # It lives in the budget because the budget is the thing that is per operation
  ///
  /// The fragment table outlives the operation that paid for it — it is a function of the document,
  /// which does not change between operations — and a charge remembered beside it would be a charge
  /// the *next* operation never pays. [`Fragments::build`] says what that costs. The ledger
  /// therefore sits in the object a new operation always brings with it: `Executor::reset` builds a
  /// new [`Visits`], so the pass is re-armed by the same statement that refills the budget, and
  /// there is no second line for a later phase to forget.
  fragments_charged: bool,
}

impl Visits {
  #[inline]
  pub(super) const fn new(limit: u32) -> Self {
    Self {
      left: limit,
      limit,
      fragments_charged: false,
    }
  }

  /// Whether this operation still owes [`Fragments::build`]'s index pass its charge.
  #[inline]
  const fn owes_fragment_pass(&self) -> bool {
    !self.fragments_charged
  }

  /// Records that this operation has paid for [`Fragments::build`]'s index pass.
  #[inline]
  fn fragment_pass_charged(&mut self) {
    self.fragments_charged = true;
  }

  /// Charges `work` units **before** the work they pay for, answering whether there was room.
  ///
  /// A selection examined is one unit, an entry a name lookup compares is one unit, and a
  /// definition or fragment the index pass handles is one unit.
  ///
  /// # Every caller charges first, and that is a property and not a convention
  ///
  /// A charge taken *after* the work bounds nothing on its own: the work is already spent when the
  /// refusal arrives, so the ceiling is exceeded by whatever that one call cost. Charging first
  /// makes "units spent" equal to "work done" at every instant, which is what lets a probe run be
  /// abandoned in the middle of a bucket instead of at the end of one.
  ///
  /// A refusal therefore leaves the remainder **unspent**, because a refused caller does not do its
  /// work. An earlier version zeroed it, which was right while a lookup charged for a run it had
  /// already walked and is over-charging now.
  ///
  /// This is the shape the callers that *degrade* need: an error message that cannot be charged is
  /// shortened, not raised, so they have no selection to point at and no fault to build.
  #[inline]
  #[must_use]
  pub(super) fn take(&mut self, work: u32) -> bool {
    match self.left.checked_sub(work) {
      Some(left) => {
        self.left = left;
        true
      }
      None => false,
    }
  }

  /// Charges one pass over `len` bytes, in [`byte_units`].
  ///
  /// A unit here used to mean an *entry*, and that is a ledger over the number of things compared
  /// and not over what comparing one costs. Every name this module reads is the **document's**, so
  /// the client writes the length as well as the count, and the two multiply: hashing a key,
  /// comparing it against a stored one, copying it into the arena and probing the schema with it
  /// all read the whole key at about a word a step, so they all charge this. al8n/smear#172.
  #[inline]
  #[must_use]
  pub(super) fn take_bytes(&mut self, len: usize) -> bool {
    self.take(byte_units(len))
  }

  /// [`take`](Visits::take), as the collection fault a walk returns.
  #[inline]
  fn spend(&mut self, work: u32, location: SimpleSpan) -> Result<(), Fault<'static>> {
    if self.take(work) {
      return Ok(());
    }
    Err(Fault {
      raw: Raw::CollectionBudget { limit: self.limit },
      location,
      name: None,
    })
  }

  /// [`take_bytes`](Visits::take_bytes), as the collection fault a walk returns.
  #[inline]
  fn spend_bytes(&mut self, len: usize, location: SimpleSpan) -> Result<(), Fault<'static>> {
    self.spend(byte_units(len), location)
  }

  /// The ceiling, for the message that reports a refusal.
  #[inline]
  pub(super) const fn limit(&self) -> u32 {
    self.limit
  }

  /// How much of the budget this operation has spent.
  ///
  /// The gate that pins collection's cost reads this: it is the only observable that separates a
  /// linear walk from a quadratic one without a clock, and a clock is not a gate.
  #[cfg(test)]
  #[inline]
  pub(super) const fn spent(&self) -> u32 {
    self.limit - self.left
  }
}

/// What the response metadata ceiling still has room for when a collection begins.
///
/// Collection is the metadata population's *first* writer — `expand`'s commit and `fail_at`'s spans
/// are the other two — so it charges the same ceiling the other two do. That is not collection
/// borrowing expansion's budget: the budget belongs to the population, and this is the earliest
/// point at which growth in that population happens, which is where the module puts a charge.
#[derive(Debug, Clone, Copy)]
pub(super) struct Allowance {
  /// Entries the ceiling still permits, after what is already committed.
  room: u64,
  /// The ceiling itself, for the message.
  limit: u32,
}

impl Allowance {
  #[inline]
  pub(super) const fn new(room: u64, limit: u32) -> Self {
    Self { room, limit }
  }

  /// Whether one more staged selection still fits.
  ///
  /// `staged` is `fields.len()` before the push, so the entry about to be added is the
  /// `staged + 1`th, and each costs two entries.
  #[inline]
  fn admits(self, staged: usize, location: SimpleSpan) -> Result<(), Fault<'static>> {
    if (staged as u64 + 1) * 2 <= self.room {
      return Ok(());
    }
    Err(Fault {
      raw: Raw::MetadataBudget { limit: self.limit },
      location,
      name: None,
    })
  }
}

/// A document's named fragments, indexed by name once instead of scanned once per spread.
///
/// # Why it holds the definition and not the definition's index
///
/// A spread used to be resolved twice: `fragment_index` scanned `definitions()` for the name and
/// returned a position, and the walk then went back to `definitions()` with that position and
/// re-matched `ExecutableDefinition::Fragment` — an arm that cannot fail, guarded by a `continue`
/// that cannot run. Carrying the definition itself deletes the second lookup and the impossible
/// arm with it, which is the module's standing preference for a checked value over a key to one.
///
/// # It is populated under the budget, which is a different question from being read under it
///
/// The first version of this table was filled in `Executor::with_limits`, from names the
/// *document* chooses, entirely outside [`Visits`]. That is the hole: an adversary got the
/// collision built for free and one cheap, perfectly valid spread walked it. The same reasoning
/// that made the interner safe — "a client that fills a bucket pays for filling it" — was simply
/// **false here**, because nothing charged the filling.
///
/// So the pass is charged and it happens on the first spread, not at construction. That fixes two
/// things at once. A service that caches parsing and builds an executor per execution used to pay
/// `executions × definitions` before any ceiling existed; now every execution pays it out of its
/// own budget. And a document whose fragments are never spread pays nothing at all.
///
/// *Every* execution, including the ones that reuse an executor and find the table already built —
/// see [`build`](Fragments::build), where the charge and the table were separated because keeping
/// them together let a refused request pass on the retry.
///
/// **Charged before the storage exists, not merely before the lookup.** The count the second charge
/// needs is only known once something has walked the definitions, and the first version walked them
/// by *populating* the table and charged for what it had populated. The refusal path cleared the
/// vector, which returns no capacity — so a refused operation left a fragment-sized allocation on
/// an executor that keeps this table across `reset`. That does not move the verdict, and it is
/// still the property broken: a ceiling that refuses and charges the server memory anyway has not
/// bounded what it says it bounds. The count now comes from a walk that allocates nothing, and the
/// storage is behind a type only an accepted charge produces. See [`mod table`](table).
///
/// **The rule this leaves behind, for the structures phases 2–8 will add:** asking whether a
/// lookup is charged is half the question. The other half is whether the thing being looked *in*
/// was built inside the budget or outside it, because a bound on the lookup is worth nothing over
/// a table an adversary filled for free.
///
/// # Chaining, so that filling it cannot be quadratic
///
/// Open addressing made the *build* the sharper edge of the same defect: inserting `n` names that
/// collide probes `n²/2` slots, so the constructor was quadratic in a quantity the document chose.
/// Chaining pushes at a bucket head and never probes, so the pass is `O(definitions + fragments)`
/// **whatever the names are** — which is what makes its cost knowable before it is paid, and
/// therefore chargeable in advance rather than discovered afterwards.
///
/// It degrades gracefully too: a bucket count clamped below the fragment count lengthens chains,
/// where open addressing would have had nowhere to put the entry.
pub(super) struct Fragments<'a, S> {
  document: &'a ExecutableDocument<S>,
  /// The index itself, and the three vectors this type cannot reach. See [`mod table`](table).
  table: Table<'a, S>,
}

impl<'a, S> Fragments<'a, S>
where
  S: AsRef<[u8]>,
{
  /// An index over `document`'s named fragments, which does not read `document` yet.
  pub(super) const fn new(document: &'a ExecutableDocument<S>) -> Self {
    Self {
      document,
      table: Table::new(),
    }
  }

  /// Runs the indexing pass once per executor, and charges it once per **operation**.
  ///
  /// Two charges rather than one, because the second quantity is not known until something has
  /// walked the definitions: `definitions` for the walk that finds the fragments, then `fragments`
  /// for the pushes that index them. Both are known **before** the work they pay for — the first is
  /// a slice length, and the second is exact because chaining has no data-dependent insertion loop.
  ///
  /// # One walk, and both charges are for that walk
  ///
  /// The two amounts are two costs of a **single** pass over the definitions: reading them, and
  /// indexing the ones that turned out to be fragments. [`Table::charge`] keeps what it reads, so
  /// the receipt it hands back is the selection itself and [`Table::fill`] never sees the slice.
  ///
  /// Two earlier versions each made the same walk twice. The first learnt the count by *populating*
  /// the table, which put the second charge after the work it paid for; the version that fixed that
  /// counted with `filter().count()` and then let `fill` sieve the fragments out of the slice
  /// again, which put a full scan of the document's operations under a charge that prices only its
  /// fragments. `definitions` is the same number in both charges, but only one of the two costs is
  /// bounded by the fragment count, and a document is free to be all operations.
  ///
  /// # The second charge is taken before the storage it pays for exists
  ///
  /// Populating first made the charge a charge *after* the work, and the refusal path cleared the
  /// vector, which hands back no capacity: this table is owned by the executor and kept across
  /// `reset`, so a refused operation left a fragment-sized allocation nothing would ever reclaim.
  ///
  /// The verdict was never wrong there — the same document under the same limits is refused on
  /// every call, because [`Visits`] is rebuilt by `reset` and the two charges are the same two
  /// amounts each time. That is what makes it worth writing down: an adversary could not change the
  /// answer, only make a refusal cost memory that outlived it, and a fixture keyed on the answer is
  /// green over exactly that. The ordering is now enforced by [`mod table`](table) rather than by
  /// this function keeping to it.
  ///
  /// Nothing can fail after [`Table::fill`] begins, because nothing that can fail is left: both
  /// charges have been accepted. So "charged" and "complete" are the same state, and there is no
  /// half-built table to undo.
  ///
  /// # The table survives `reset` and the charge does not, because the answer must not move
  ///
  /// `Executor::reset` keeps this table: it is a function of the document, and the document does
  /// not change between operations. The charge used to be kept with it, and that made the outcome
  /// of a request depend on what the executor had been asked before.
  ///
  /// **A request refused on the first `start` was served on the second.** The first pays for the
  /// pass, runs out of budget in the lookup that follows, and is refused — leaving the table built.
  /// The second, on the same executor with the same document and the same limits, finds the table
  /// already there, spends nothing on it, and now has room to finish. Same request, two answers. A
  /// ceiling a client clears by sending the request twice is not a ceiling, and the objection that
  /// the accounting was sound because the work really was done once prices the *work* while the
  /// observable that matters is the *answer*.
  ///
  /// So the pass is charged whether or not it runs, in the same two amounts, in the same order, at
  /// the same point in the walk — which makes the budget's remainder at every instant a function of
  /// the operation alone and never of the executor's history. What a cached table saves is the
  /// walking and the hashing. What it must not save is the verdict.
  ///
  /// The once-per-operation ledger is [`Visits::fragments_charged`], and it is there rather than
  /// here for the reason that field gives: a new operation is a new [`Visits`], so nothing has to
  /// remember to re-arm it.
  fn build(&mut self, visits: &mut Visits, location: SimpleSpan) -> Result<(), Fault<'static>> {
    if !visits.owes_fragment_pass() {
      return Ok(());
    }
    let definitions = self.document.definitions();
    visits.spend(
      u32::try_from(definitions.len()).unwrap_or(u32::MAX),
      location,
    )?;
    if self.table.is_indexed() {
      // The second charge, in the amount the pass would cost if it ran now. The table holds exactly
      // the fragments the counting pass would find again, so this is the same number the build path
      // spends and not an estimate of it.
      visits.spend(
        u32::try_from(self.table.count()).unwrap_or(u32::MAX),
        location,
      )?;
      visits.fragment_pass_charged();
      return Ok(());
    }
    // Select, charge, populate — in that order, and in no other order this crate can be written to
    // take: the receipt `charge` returns on acceptance is the only argument `fill` has, and it is
    // the selection, so there is no second walk for `fill` to make.
    let paid = self.table.charge(definitions, visits, location)?;
    self.table.fill(paid);
    visits.fragment_pass_charged();
    Ok(())
  }

  /// Returns the fragment `name` denotes and its ordinal, charging **before** each entry compared.
  ///
  /// Before, not after: the run is as long as the document's collisions make it, so a charge taken
  /// at the end lets one spread walk the whole bucket and only then hear that it had no budget for
  /// it. Charging first abandons the run at the ceiling.
  ///
  /// `Ok(None)` is an undefined spread, which draft 5.5.2.1 makes a validation failure — so it is
  /// unreachable for a validated document, and skipping is the only behaviour that cannot invent a
  /// field.
  fn get(
    &mut self,
    name: &[u8],
    visits: &mut Visits,
    location: SimpleSpan,
  ) -> Result<Option<(u32, &'a FragmentDefinition<S>)>, Fault<'static>> {
    self.table.get(name, visits, location)
  }

  /// Entries this executor's fragment lookups have compared. See [`Table`]'s field.
  #[cfg(test)]
  pub(super) const fn compares(&self) -> u64 {
    self.table.compares()
  }

  /// Definitions this executor's index pass has read. See [`Table`]'s field.
  #[cfg(test)]
  pub(super) const fn walked(&self) -> u64 {
    self.table.walked()
  }

  /// Entries the index has reserved room for. See [`Table::reserved`].
  #[cfg(test)]
  pub(super) fn reserved(&self) -> usize {
    self.table.reserved()
  }
}

/// The fragment index's storage, and the receipt that is the only way to reserve it.
///
/// # What this is defending against
///
/// [`Fragments::build`] pays for the index pass in two charges, and the second — one unit per
/// fragment — is a number nothing knows until the definitions have been walked. The first version
/// walked them by *populating* `defs`, charged `defs.len()`, and cleared the vector when that
/// charge was refused.
///
/// Clearing a vector returns no capacity, and this table is owned by the executor and deliberately
/// kept across `reset`. So a refused operation left a fragment-sized allocation behind, and every
/// retry — also refused — found it still there and freed nothing. **The verdict never moved**,
/// which is precisely what makes the defect easy to be green over: a fixture comparing answers
/// passes against it on the first call and on all of them after. What moved is what a refusal
/// *costs*, and a ceiling whose refusal still spends the server's memory is not bounding the thing
/// it names.
///
/// # Why a module and a receipt rather than a rule about ordering
///
/// [`mod groups`](groups) is the precedent: a property about what a caller may do, and when, is a
/// type here rather than a sentence in a doc comment. The property is *no storage is reserved
/// before a charge that bounds it has been accepted*, and it is enforced the same way. The three
/// vectors are private to this module, so nothing in the crate outside it can put an entry in one —
/// and inside it, both of the things that fill them sit behind a charge that bounds what they fill.
/// [`Table::charge`] builds the selection that becomes `defs`, at most one pointer per definition,
/// after [`Fragments::build`] has spent one unit per definition. [`Table::fill`] sizes `heads` and
/// `chain`, both functions of the fragment count alone, and takes a [`Paid`](table::Paid) — whose
/// fields are private to this module and whose only constructor is `charge`, which returns one only
/// when [`Visits::spend`] has said yes to the fragment charge.
///
/// Populating after the charge is therefore not a discipline `build` keeps. It is the only program
/// that compiles: there is no other argument for `fill` and no second way to the vectors. Putting
/// the defect back means adding a writer to this module, which is a diff that says what it is
/// doing.
///
/// # The receipt carries the fragments, which is what stops the population walking again
///
/// A receipt holding only a *number* would leave the caller free to pay for one slice and populate
/// from another — the count and the population agreeing by argument again. A receipt holding the
/// *slice* closed that, and left a second hole in the same accounting: `fill` sieved the fragments
/// back out of it, so the pass walked every definition **twice** while the second charge priced
/// only the fragments. The definitions charge bought one walk and two were taken, and the extra one
/// is a scan of the document's *operations* — a quantity the fragment count says nothing about. A
/// document of 4,000 named operations spreading one fragment paid 4,001 units for the walk and
/// spent 8,002.
///
/// So the walk that counts is the walk that selects: it keeps what it finds, the receipt carries
/// that, and `fill` cannot walk the definitions because it is not given them. The count and the
/// population are the same walk over the same data and now over the same *selection*, by
/// construction rather than by argument.
///
/// The vector that walk builds is the vector the table keeps — moved into `defs` where the old
/// version reserved a second one, and dropped with the receipt when the fragment charge is refused,
/// which is how a refusal still ends with nothing reserved.
mod table {
  use tokora::SimpleSpan;

  use smear_parser::graphql::ast::{
    DescribedExecutableDefinition, ExecutableDefinition, FragmentDefinition,
  };
  use smear_schema::{bucket, hash_bytes};

  use super::{Fault, NONE, Visits};

  /// One accepted charge, and the fragments it was taken over.
  ///
  /// Neither [`Copy`] nor [`Clone`], and [`Table::fill`] takes it by value, so one charge admits
  /// one population and a second needs a second charge.
  pub(super) struct Paid<'a, S> {
    /// The fragments the charged walk selected, which is also what the population indexes — not a
    /// count of them, and not the slice they were selected out of. See the module.
    fragments: std::vec::Vec<&'a FragmentDefinition<S>>,
  }

  /// A document's named fragments, chained by the hash of their names.
  pub(super) struct Table<'a, S> {
    /// The named fragments in document order, empty until [`fill`](Table::fill) has run. An index
    /// into this is a *fragment ordinal*, which is what [`Visited`](super::Visited) is a bitset
    /// over.
    defs: std::vec::Vec<&'a FragmentDefinition<S>>,
    /// Each definition's whole [`hash_bytes`], parallel to `defs`.
    ///
    /// The same eight bytes an entry, bought for the same thing, that [`Interner`](super::Interner)
    /// buys them for: a chain step rejects a bucket collision on this word and the name's length,
    /// so the `memcmp` a collision used to force is not made at all. The names here are the
    /// **document's**, and the hash is unkeyed, so the collision is constructible — and without the
    /// stored hash `k` colliding names of `L` bytes cost `k` charged units and `k · L` compared
    /// bytes on every spread that lands in that bucket. al8n/smear#172.
    hashes: std::vec::Vec<u64>,
    /// The newest ordinal in each bucket, or [`NONE`]. Power-of-two length.
    heads: std::vec::Vec<u32>,
    /// The ordinal pushed into the same bucket before this one, or [`NONE`]. Parallel to `defs`.
    chain: std::vec::Vec<u32>,
    /// Whether the pass has run. Distinct from `defs.is_empty()`, which is also true of a document
    /// that defines no fragments and must not be indexed again on every spread it does not have.
    ///
    /// It is **not** the record of who has paid: this outlives the operation and the charge does
    /// not. That one is `Visits::fragments_charged`.
    indexed: bool,
    /// Entries compared, over the executor's whole life.
    ///
    /// The gate for "a refused probe run stops at the refusal" reads this. It cannot be read from
    /// the budget: every comparison is charged before it happens, so the charge and the comparison
    /// count agree by construction and a version that charged afterwards would agree with itself
    /// too. Only a count taken independently of the charge can tell the two apart.
    #[cfg(test)]
    compares: u64,
    /// Definitions the index pass has read, over the executor's whole life.
    ///
    /// Counted here for the same reason `compares` is, one step further out. The pass is charged
    /// one unit per definition and one per fragment, and *that total does not move* when the
    /// population walks the definitions a second time to sieve the fragments out of them: the
    /// budget is spent up front, from a slice length, so it reads the same over one walk and over
    /// two. Only a count taken at the read itself can say how many walks there were.
    #[cfg(test)]
    walked: u64,
  }

  impl<'a, S> Table<'a, S> {
    /// A table that has read nothing and reserved nothing.
    pub(super) const fn new() -> Self {
      Self {
        defs: std::vec::Vec::new(),
        hashes: std::vec::Vec::new(),
        heads: std::vec::Vec::new(),
        chain: std::vec::Vec::new(),
        indexed: false,
        #[cfg(test)]
        compares: 0,
        #[cfg(test)]
        walked: 0,
      }
    }

    /// Walks `definitions` once keeping the fragments, charges for indexing them, and answers with
    /// the receipt that admits populating this table from what the walk kept.
    ///
    /// The walk retains at most one pointer per definition, which is what
    /// [`Fragments::build`](super::Fragments::build) has already spent before it calls this — so
    /// the selection is storage a charge has already admitted, and it is the *only* storage: it is
    /// moved into `defs` rather than copied into a second vector. What that costs is a `defs`
    /// capacity grown to reach the count instead of reserved exactly for it: at most one spare
    /// pointer per fragment, in a table that already keeps up to five `u32` slots for each. A
    /// refusal drops the vector and leaves the table not cleared but *untouched*, there being
    /// nothing in it yet to clear.
    ///
    /// The charge is exact rather than an upper bound, because chaining has no data-dependent
    /// insertion loop: indexing what the walk selected costs one push each whatever the names are.
    pub(super) fn charge(
      &mut self,
      definitions: &'a [DescribedExecutableDefinition<S>],
      visits: &mut Visits,
      location: SimpleSpan,
    ) -> Result<Paid<'a, S>, Fault<'static>> {
      let mut fragments = std::vec::Vec::new();
      for described in definitions {
        #[cfg(test)]
        {
          self.walked += 1;
        }
        if let ExecutableDefinition::Fragment(fragment) = described.node() {
          fragments.push(fragment);
        }
      }
      visits.spend(u32::try_from(fragments.len()).unwrap_or(u32::MAX), location)?;
      Ok(Paid { fragments })
    }

    /// Whether the pass has run. See the field.
    #[inline]
    pub(super) const fn is_indexed(&self) -> bool {
      self.indexed
    }

    /// How many fragments it holds, which is what the population cost.
    #[inline]
    pub(super) fn count(&self) -> usize {
      self.defs.len()
    }

    /// Entries this executor's fragment lookups have compared. See the field.
    #[cfg(test)]
    #[inline]
    pub(super) const fn compares(&self) -> u64 {
      self.compares
    }

    /// Definitions this executor's index pass has read. See the field.
    #[cfg(test)]
    #[inline]
    pub(super) const fn walked(&self) -> u64 {
      self.walked
    }

    /// Entries the three vectors have reserved room for.
    ///
    /// *Capacity* and not length, because the defect this module closes populated the table and
    /// then cleared it: every length reads zero and every allocation is exactly where it was.
    #[cfg(test)]
    pub(super) fn reserved(&self) -> usize {
      self.defs.capacity() + self.hashes.capacity() + self.heads.capacity() + self.chain.capacity()
    }
  }

  impl<'a, S> Table<'a, S>
  where
    S: AsRef<[u8]>,
  {
    /// Indexes the fragments `paid` paid for, over one entry each and no walk of the document.
    ///
    /// Infallible, and that is the property rather than a convenience: everything able to refuse
    /// has refused before a [`Paid`] exists, so no failure survives into the population — and
    /// therefore no half-built table to undo, and no `clear` leaving behind a capacity a refusal
    /// has no way to give back.
    ///
    /// It costs what the fragment charge bought and nothing else. The receipt is the selection, so
    /// there is no definitions slice here to sieve a second time and no second allocation to fill:
    /// the vector is moved in, and every term left is one per fragment.
    pub(super) fn fill(&mut self, paid: Paid<'a, S>) {
      debug_assert!(
        !self.indexed,
        "the table is being populated a second time; the second selection would replace `defs` \
         while `heads` and `chain` still chain the ordinals of the first"
      );
      let Paid { fragments } = paid;
      self.indexed = true;
      let count = fragments.len();
      if count == 0 {
        return;
      }
      self.defs = fragments;
      // Load factor a half, as `NameIndex` uses. Clamped rather than checked, because a chained
      // table with fewer buckets than entries is slower and still correct — there is no capacity
      // this can fail to have.
      let buckets = count
        .next_power_of_two()
        .saturating_mul(2)
        .min(1usize << 31);
      let mask = (buckets - 1) as u32;
      self.heads.resize(buckets, NONE);
      self.chain.resize(count, NONE);
      self.hashes.reserve(count);
      for ordinal in 0..count {
        // One pass over each fragment's name, once per document, and the hash is kept so that no
        // later lookup or growth has to make it again. It is charged in *definitions* rather than
        // in bytes deliberately: this reads every name exactly once for the whole executor's life,
        // so it is a single pass over text the parser has already read, with no factor a client can
        // apply to it. That is the question the byte charges elsewhere in this module answer the
        // other way — see `Visits::take_bytes`.
        let hash = hash_bytes(self.defs[ordinal].name().source().as_ref());
        self.hashes.push(hash);
        let at = bucket(hash, mask) as usize;
        self.chain[ordinal] = self.heads[at];
        self.heads[at] = ordinal as u32;
      }
    }

    /// The fragment `name` denotes and its ordinal, charging **before** each entry compared and
    /// before every pass over the spelling. See [`Fragments::get`](super::Fragments::get).
    ///
    /// The entry charge bounds the chain; it did not bound the chain's *cost*, because comparing
    /// one entry was `fragment.name() == name` at whatever length the document wrote. The whole
    /// hash is stored beside each definition now, so a chain step tests two integers and reads no
    /// bytes, and the two passes that do read the spelling — hashing it to find the bucket, and the
    /// one `memcmp` a matching hash and length admits — are charged in [`byte_units`] first.
    /// Without that, `k` colliding `L`-byte names cost `k` units and `k · L` bytes per lookup, and
    /// a document is free to pay for the pile-up once and walk it on every spread. al8n/smear#172.
    pub(super) fn get(
      &mut self,
      name: &[u8],
      visits: &mut Visits,
      location: SimpleSpan,
    ) -> Result<Option<(u32, &'a FragmentDefinition<S>)>, Fault<'static>> {
      if self.heads.is_empty() {
        return Ok(None);
      }
      visits.spend_bytes(name.len(), location)?;
      let hash = hash_bytes(name);
      let mask = (self.heads.len() - 1) as u32;
      let mut ordinal = self.heads[bucket(hash, mask) as usize];
      while ordinal != NONE {
        visits.spend(1, location)?;
        #[cfg(test)]
        {
          self.compares += 1;
        }
        // What that unit buys: two integers, and no read of a name a client chose the length of.
        let fragment = self.defs[ordinal as usize];
        let spelling = fragment.name().source().as_ref();
        if self.hashes[ordinal as usize] == hash && spelling.len() == name.len() {
          visits.spend_bytes(name.len(), location)?;
          if spelling == name {
            return Ok(Some((ordinal, fragment)));
          }
        }
        ordinal = self.chain[ordinal as usize];
      }
      Ok(None)
    }
  }
}

/// Draft §6.3's `visitedFragments`, as a bitset over fragment ordinals.
///
/// # The membership test and the reset are bounded separately
///
/// This was a vector scanned with `contains`, which is linear in the fragments already visited and
/// runs once per spread — the third of the collection scans. A bitset answers in one word read.
///
/// Clearing is the half that is easy to get wrong: `collect_fields` runs once per object position,
/// so a `fill(0)` over the whole bitset would be `positions × fragments`, and positions are the
/// driver's. The ordinals actually set are therefore kept as a list, and clearing costs what the
/// last collection marked — work the visit budget already charged for.
#[derive(Default)]
pub(super) struct Visited {
  /// One bit per fragment ordinal, grown on demand.
  bits: std::vec::Vec<u64>,
  /// The ordinals whose bit is set, so the reset costs what was set and not what could have been.
  seen: std::vec::Vec<u32>,
}

impl Visited {
  fn clear(&mut self) {
    for ordinal in self.seen.drain(..) {
      self.bits[ordinal as usize / 64] &= !(1u64 << (ordinal % 64));
    }
  }

  /// Marks `ordinal`, returning whether it was already there.
  ///
  /// It grows to reach the ordinal rather than being sized up front, because the fragment table it
  /// indexes is not built until the first spread and a size taken before that would be a guess.
  /// Growth is bounded by the document's fragment count, since every ordinal comes from that table,
  /// and the alternative to the length compare is a panic on a path nothing can catch.
  fn visited(&mut self, ordinal: u32) -> bool {
    let index = ordinal as usize / 64;
    if index >= self.bits.len() {
      self.bits.resize(index + 1, 0);
    }
    let word = &mut self.bits[index];
    let bit = 1u64 << (ordinal % 64);
    if *word & bit != 0 {
      return true;
    }
    *word |= bit;
    self.seen.push(ordinal);
    false
  }
}

/// One response key's group of selections.
#[derive(Debug, Clone, Copy)]
pub(super) struct Group {
  /// The interned response key.
  pub(super) key: u32,
  /// Where the group's selections start in the flat vector.
  pub(super) start: u32,
  /// How many selections it holds. Always at least one.
  pub(super) len: u32,
  /// The span of the selection that created this group, which is also the span of
  /// `selections[0]` after the stable sort — the group's first selection in document order.
  ///
  /// # This exists so that a claim can stop being a claim
  ///
  /// A budget refusal names the position it refuses, and the refusal can now happen at two places:
  /// during collection, when the staged total crosses the ceiling, and at the commit, for the two
  /// ceilings collection cannot pre-empt. Those two were supposed to report the same span, and the
  /// assertion that they did was wrong twice — first reporting the field's *name* where the commit
  /// reports the whole aliased selection, then reporting the *crossing duplicate* where the commit
  /// reports the group's first. Each time the evidence offered could not reach the counterexample.
  ///
  /// So the span is stored once, here, at the moment the group is created, and both paths read this
  /// field. There is no longer an equivalence to establish: there is one value.
  pub(super) first: SimpleSpan,
}

/// The group list, and the phase split that keeps a scan out of the walk.
///
/// # What this is defending against
///
/// Finding a response key's group used to be a scan of the groups collected so far, which is one of
/// the three linear lookups that made a selection set of `n` distinct keys cost `n²`. It is now a
/// direct index through [`Scratch`]'s `keys` table, and the danger is that somebody puts the scan back:
/// `groups.iter().position(|group| group.key == key)` is one line, is obviously correct, and on
/// `{ k0: a k1: a … k4095: a }` walks about 8.4 million entries.
///
/// **No gate in this crate can see that.** The other two lookups are probe loops, so a scan charges
/// [`Visits`] what it compares and both the upper and the lower bound in
/// `a_repeated_response_key_charges_one_comparison_each_time`'s section move. This one has no loop
/// to charge: `visits.spent()` reads the same either way, and
/// `distinct_response_keys_are_linear_however_they_are_spelled` goes green over the quadratic. A
/// counter cannot guard work that declines to count itself.
///
/// # Why it is two types
///
/// The residual that stood here said the vector could not be hidden, because `expand` iterates it
/// to commit the groups — so the type would have to permit and forbid the same access. It does
/// iterate it, but not at the same time, and *when* is the whole of the argument:
///
/// - **During the walk**, the only operations are appending a group and reading one back by the
///   index that append just returned. No iteration, no length, no search.
/// - **Before and after the walk**, everything is iteration: `collect_fields` unmarks the previous
///   collection's keys, assigns each group its contiguous range, and `expand` commits them. See
///   [`collect_fields`].
///
/// Two capabilities in disjoint phases are two types. [`Groups`] is the whole list, and
/// [`Appending`] is what a walk is handed: it can `push` and it can read a group's `first` span, and
/// it holds the only reference to the vector while it exists. The vector is private to *this
/// module* rather than to `collect`, so the restriction binds [`walk`] too — a scan by
/// key is not something the walk can be written to do, whatever a future editor believes about it.
///
/// The cost is this module and a `push` that returns its index. That buys a guarantee where there
/// was a sentence.
mod groups {
  use tokora::SimpleSpan;

  use super::Group;

  /// One response key's group of selections, in the order the keys were first seen.
  ///
  /// Cleared, never shrunk, like the rest of [`Scratch`](super::Scratch).
  #[derive(Default)]
  pub(super) struct Groups {
    inner: std::vec::Vec<Group>,
  }

  impl Groups {
    /// The walk's view: append, and read back what was appended. See the module.
    pub(super) fn appending(&mut self) -> Appending<'_> {
      Appending {
        inner: &mut self.inner,
      }
    }

    /// Every group, for the phases that are allowed to enumerate them.
    #[inline]
    pub(super) fn as_slice(&self) -> &[Group] {
      &self.inner
    }

    /// Every group, mutably, for the range assignment that follows the walk.
    #[inline]
    pub(super) fn iter_mut(&mut self) -> core::slice::IterMut<'_, Group> {
      self.inner.iter_mut()
    }

    #[inline]
    pub(super) fn clear(&mut self) {
      self.inner.clear();
    }

    /// What the vector is holding, for the gate that pins the scratch against the ceiling that
    /// refuses it.
    #[cfg(test)]
    #[inline]
    pub(super) fn capacity(&self) -> usize {
      self.inner.capacity()
    }
  }

  /// The groups, as a walk may touch them: append one, and read one back by its index.
  ///
  /// The field is private to this module, so nothing in `collect` — [`walk`](super::walk)
  /// included — can reach the vector through it. That is the mechanism; see the module for why the
  /// alternative guards do not work here.
  pub(super) struct Appending<'g> {
    inner: &'g mut std::vec::Vec<Group>,
  }

  impl Appending<'_> {
    /// Appends `group` and answers with its index, which is the only index a walk ever has.
    ///
    /// Returning it rather than making the caller read `len()` first is what leaves this type with
    /// no length: a search needs a bound to walk to, and there is none to ask for.
    #[inline]
    pub(super) fn push(&mut self, group: Group) -> u32 {
      let index = self.inner.len() as u32;
      self.inner.push(group);
      index
    }

    /// The span of the group's first selection. See [`Group::first`].
    ///
    /// One field rather than the whole group, because one field is what the walk reads — and a span
    /// carries no response key, so even a caller with an index cannot compare its way through the
    /// list.
    #[inline]
    pub(super) fn first(&self, index: u32) -> SimpleSpan {
      self.inner[index as usize].first
    }
  }
}

/// The response keys and type names a response refers to, held once as bytes.
///
/// A response key repeats on every element of a list, so storing the bytes per slot would make a
/// thousand-element list a thousand copies of the same handful of names. Slots carry a `u32` into
/// this table instead. It also keeps the source type `S` out of the response types entirely: a
/// [`Node`](super::Node) is generic only over the driver's value.
///
/// # Why this one refuses instead of growing
///
/// Every other table here holds something the document or the schema produced, and both are
/// already in memory before the executor exists. This one also holds **driver** text:
/// [`Executor::handle_field_error`](super::Executor::handle_field_error) interns the message it is
/// given, once per failed position, and neither the message's length nor the number of failures is
/// anything the query bounds.
///
/// Left alone that is not merely unbounded memory, it is **silent corruption**. Offsets into the
/// arena are `u32`; an arena past four gigabytes does not fail to allocate, it *truncates*, and
/// every name interned afterwards reads back somebody else's bytes — wrong response keys, wrong
/// `__typename`, in a response that still looks well formed. A refusal is a contract a caller can
/// act on; that is not. So the narrowing is checked rather than argued, and the ceiling is what
/// keeps the check from ever being the thing that fires.
///
/// # The index, and what suffix-`restore` costs it
///
/// Finding an existing name used to be a scan of every name, which is one of the three scans that
/// made a selection set of `n` distinct keys cost `n²`. It is now a chained hash: `heads[bucket]`
/// holds the newest id in that bucket and `chain[id]` the one interned before it.
///
/// **Chaining rather than open addressing, because of [`restore`](Interner::restore).** A failed
/// collection unwinds a contiguous *suffix* of ids, and open addressing has no cheap way to remove
/// scattered entries without tombstones or a backward shift. With ids pushed at the head of their
/// bucket, a bucket's chain is in decreasing id order — so every id in the suffix is the head of
/// its own bucket when it is removed, and unwinding in reverse costs one pointer write each. That
/// is the property `restore` asserts rather than assumes.
///
/// # Its memory is a multiple of the byte ceiling, not the byte ceiling
///
/// [`max_interned_bytes`](super::Limits::max_interned_bytes) bounds the arena's *bytes*, and every
/// entry carries bookkeeping on top: eight bytes of `spans`, eight of `hashes`, four of `chain`, up
/// to eight of `heads`, and four of the caller's key-to-group scratch. A GraphQL name is at least
/// one byte, so an arena of `B` bytes can hold `B` entries and cost about `33 · B`, where before
/// this index it cost about `9 · B`. That is a constant and not a second factor, which is what
/// keeps it a memory *cost* rather than the product shape this module refuses — but it is a
/// constant a caller choosing the ceiling is choosing too, so [`max_interned_bytes`] says so as
/// well.
///
/// The `hashes` row is the newest eight of those, and it is bought back in CPU rather than in
/// memory: without it a chain step reads the arena to reject a bucket collision, and a rehash reads
/// all of it. See [`intern`](Interner::intern). al8n/smear#172.
///
/// [`max_interned_bytes`]: super::Limits::max_interned_bytes
#[derive(Debug)]
pub(super) struct Interner {
  /// # A `String`, and that is the second half of al8n/smear#139
  ///
  /// This was a `Vec<u8>`, and both of its readers — [`Error::name`](super::Error) and the
  /// response's [`Segment::Field`](super::Segment) — ended in
  /// `core::str::from_utf8(..).unwrap_or("")`. That fallback is not a degraded reading, it is a
  /// *different name*: every spelling it cannot read becomes the same one, so two response keys
  /// collapse into one and a diagnostic quotes `$` with nothing after it. Both were reachable, and
  /// the second one through the *honest* path — draft §6.3's condition refuses a spelling it
  /// cannot read and then interns the raw bytes for its message, which rendered empty anyway.
  ///
  /// A `str` store moves the question to admission, where there is one caller per kind of name and
  /// each has an answer. The driver's strings are already `&str`; the schema's arena is ASCII by
  /// its own builder's admission rule; the two that come out of the document convert once, and
  /// refuse rather than substitute. What is left is a reader that cannot fail, so there is no
  /// fallback left to choose wrongly.
  names: std::string::String,
  spans: std::vec::Vec<(u32, u32)>,
  /// Each entry's whole [`hash_bytes`], parallel to [`spans`](Interner::spans).
  ///
  /// Eight bytes an entry, bought for three things the budget could not otherwise see. A chain step
  /// rejects a bucket collision on this word and the span's length, so it no longer runs a `memcmp`
  /// whose length the client picked — the unit a step is charged buys a two-integer test, and the
  /// bytes are read only when they are about to be equal, which is the pass the collision was the
  /// instrument for. [`rehash`](Interner::rehash) reads it instead of hashing every stored byte
  /// again, and so does [`restore`](Interner::restore), which unwinds a failed collection.
  /// al8n/smear#172.
  hashes: std::vec::Vec<u64>,
  /// Entries compared, over the executor's whole life. See [`Fragments::compares`].
  #[cfg(test)]
  compares: u64,
  /// The newest id in each bucket, or [`NONE`]. Power-of-two length; empty until the first intern.
  heads: std::vec::Vec<u32>,
  /// The id interned before `id` in the same bucket, or [`NONE`]. Parallel to `spans`.
  chain: std::vec::Vec<u32>,
  /// [`Limits::max_interned_bytes`](super::Limits::max_interned_bytes).
  cap: u32,
}

/// Buckets the first growth allocates.
///
/// Small because most responses intern a handful of names and the table doubles from here; the
/// vector's capacity survives [`clear`](Interner::clear), so a reused executor pays for it once.
const FIRST_BUCKETS: usize = 16;

impl Interner {
  #[inline]
  pub(super) const fn new(cap: u32) -> Self {
    Self {
      names: std::string::String::new(),
      spans: std::vec::Vec::new(),
      hashes: std::vec::Vec::new(),
      #[cfg(test)]
      compares: 0,
      heads: std::vec::Vec::new(),
      chain: std::vec::Vec::new(),
      cap,
    }
  }

  /// Returns the id for `name`, adding it if it is not already there, charging `visits` before
  /// **each** entry it compares.
  ///
  /// # `&str` and not `&[u8]`
  ///
  /// The arena's readers hand a `&str` back — to a diagnostic and to a response key — so the
  /// question "can these bytes be printed" has to be answered *somewhere*, and taking bytes here
  /// left it to the two readers, which both answered it the same wrong way. See the type's own
  /// header.
  ///
  /// It is not the *whole* question, and reading it as the whole question is what
  /// [`name_key`](super::name_key) exists to stop. This parameter admits every `&str`, which is
  /// right: the arena also holds a resolver's message and a driver's `type_name`, and neither is a
  /// draft §2.1.9 `Name` nor should be made one. "Is this spelling a name" is a question only the
  /// *caller* knows to ask, so it is answered at each admission point rather than here — and for
  /// the two names that come out of a document, by one function.
  ///
  /// # There is no uncharged way in, and that is the point
  ///
  /// This used to have an uncharged sibling, documented as being for callers whose bytes are the
  /// schema's or the driver's — a claim about *every* caller, asserted from inspection. A review
  /// found the counterexample immediately: two sites interned a **variable's spelling out of the
  /// executable document**, so a client could name its variables to fill one bucket, fail argument
  /// coercion once per sibling field, and walk an attacker-sized run per failure, all of it after
  /// collection had been admitted.
  ///
  /// A more careful claim would have been the wrong repair, because the next phase adds callers
  /// nobody has read. So the sibling is gone: the budget is a parameter, and a caller who has no
  /// budget to give cannot reach the probe loop. What was a sentence to re-audit is now a signature.
  ///
  /// # What that buys, beyond the one site
  ///
  /// Charging every insertion's probe run bounds the **chain** and not just the walk. Putting an
  /// `L`th name into a bucket first walks the `L - 1` already there, so building a run of length
  /// `L` costs about `L²/2` — an adversary reaches `√(2 · budget)` and no further, and walking what
  /// they built costs the budget again. The whole table is bounded by
  /// [`max_selection_visits`](super::Limits::max_selection_visits), for every population it holds.
  ///
  /// [`Unstored::Arena`] is a storage refusal and never a lookup failure: a name already present is
  /// always returned, whatever the ceiling says, so a full arena degrades what it *records* and
  /// never what it can still *read*.
  ///
  /// # And the bytes are charged, because a key has no length ceiling
  ///
  /// Charging every entry a run compares bounds the *chain*, and it was still a ledger over
  /// entries while the work was bytes: this hashed the whole key before any charge existed, then
  /// charged one unit per entry and ran `&names[start..start + len] == bytes` at whatever length
  /// the client wrote. About 512 aliases of thirty-two kilobytes fit under the default
  /// `max_interned_bytes`, and their 130,816 charged comparisons moved roughly four gigabytes;
  /// growth re-hashed every stored byte for nothing at all. Draft §2.1.9 puts no local ceiling on
  /// a name, and the bucket-colliding suffixes are searchable against an unkeyed hash, so `L`
  /// scaled the CPU with no movement in the recorded charge. It does not even need the collision:
  /// one long key looked up once per object position hashes and `memcmp`s its whole length for the
  /// one or two units the entry costs.
  ///
  /// Two changes, and the second is what makes the first cheap — the same pair
  /// `smear_compiler`'s merge interner took, for the same reason. Every pass over the key is
  /// charged in [`byte_units`] *before* the pass: before hashing it, before comparing it, and
  /// before copying it into the arena. And the whole 64-bit hash is stored beside each entry, so a
  /// chain step tests two integers and reads no bytes — on the bucket collision that is the
  /// adversary's whole instrument the byte charge is never reached, and it is paid only when the
  /// bytes are about to be equal. [`rehash`](Interner::rehash) and [`restore`](Interner::restore)
  /// read that stored hash rather than hashing the arena again. al8n/smear#172.
  pub(super) fn intern(&mut self, name: &str, visits: &mut Visits) -> Result<u32, Unstored> {
    let bytes = name.as_bytes();
    // Reading the key is work whose length the document chose, so it is charged before the read
    // and not after it.
    if !visits.take_bytes(bytes.len()) {
      return Err(Unstored::Budget {
        limit: visits.limit(),
      });
    }
    let hash = hash_bytes(bytes);
    if !self.heads.is_empty() {
      let mut id = self.heads[self.bucket(hash)];
      while id != NONE {
        if !visits.take(1) {
          return Err(Unstored::Budget {
            limit: visits.limit(),
          });
        }
        #[cfg(test)]
        {
          self.compares += 1;
        }
        // What that unit buys: two integers. A bucket collision — the constructible case, and the
        // only one an adversary has — is rejected here without touching a byte.
        let (start, len) = self.spans[id as usize];
        if self.hashes[id as usize] == hash && len as usize == bytes.len() {
          if !visits.take_bytes(bytes.len()) {
            return Err(Unstored::Budget {
              limit: visits.limit(),
            });
          }
          if &self.names.as_bytes()[start as usize..(start + len) as usize] == bytes {
            return Ok(id);
          }
        }
        id = self.chain[id as usize];
      }
    }
    // The key's last read is the copy into the arena, and it is charged like the other two. A
    // refusal here is the budget's and not the arena's, which is what keeps the message pointing
    // at the ceiling that actually stopped it.
    if !visits.take_bytes(bytes.len()) {
      return Err(Unstored::Budget {
        limit: visits.limit(),
      });
    }
    self
      .insert(name, hash)
      .ok_or(Unstored::Arena { limit: self.cap })
  }

  /// Entries this executor's name lookups have compared. See [`Fragments::compares`] for why a
  /// count kept beside the charge is the only witness for charging before rather than after.
  #[cfg(test)]
  pub(super) const fn compares(&self) -> u64 {
    self.compares
  }

  /// What this is holding, as `(entries, arena bytes)`.
  ///
  /// Entries covers `spans`, `hashes`, `chain` and `heads` at once: the first three are parallel to
  /// it by construction, and `heads` is a power of two that [`rehash`](Interner::rehash) keeps at or
  /// above it and never more than double. `clear` empties all five and shrinks none, so both
  /// numbers survive every operation this executor runs.
  #[cfg(test)]
  pub(super) fn capacity(&self) -> (usize, usize) {
    (self.spans.capacity(), self.names.capacity())
  }

  /// Appends `name` and links it, or `None` when the arena has no room.
  ///
  /// Unbudgeted *here*, and it does not need to be: [`intern`](Interner::intern) charges the copy's
  /// bytes before calling this, it runs at most once per selection, which the caller has already
  /// charged, and the rehash it may trigger reads stored hashes rather than the arena — so it is
  /// one step per entry and one step's worth of work, amortised over those same insertions.
  fn insert(&mut self, name: &str, hash: u64) -> Option<u32> {
    // Checked, not reasoned about. The ceiling below makes each of these unreachable, and they
    // stay because "unreachable given the ceiling" is exactly the kind of claim that stops being
    // true when somebody sets a different ceiling.
    let start = u32::try_from(self.names.len()).ok()?;
    let len = u32::try_from(name.len()).ok()?;
    let end = start.checked_add(len)?;
    if end > self.cap {
      return None;
    }
    let id = u32::try_from(self.spans.len()).ok()?;
    self.names.push_str(name);
    self.spans.push((start, len));
    self.hashes.push(hash);
    self.chain.push(NONE);
    if self.spans.len() > self.heads.len() {
      self.rehash();
    } else {
      let bucket = self.bucket(hash);
      self.chain[id as usize] = self.heads[bucket];
      self.heads[bucket] = id;
    }
    Some(id)
  }

  /// The bucket `hash` lands in. Never called with `heads` empty.
  #[inline]
  fn bucket(&self, hash: u64) -> usize {
    bucket(hash, (self.heads.len() - 1) as u32) as usize
  }

  /// Doubles the bucket table and relinks every entry.
  ///
  /// **In increasing id order**, so that each bucket's chain comes out in decreasing id order
  /// again. That ordering is not cosmetic: it is exactly what lets [`restore`](Interner::restore)
  /// unwind a suffix by taking heads, and a rehash that reversed it would leave the unwind removing
  /// entries that are not heads.
  ///
  /// One step per entry, and one step's worth of *work*: the hash is read out of
  /// [`hashes`](Interner::hashes) rather than recomputed. Re-hashing made this loop cost every
  /// stored byte in the arena while the caller had charged one unit per selection for it — true
  /// about steps, false about work, which is the accounting al8n/smear#172 exists to correct.
  fn rehash(&mut self) {
    let buckets = self
      .heads
      .len()
      .max(FIRST_BUCKETS)
      .max(self.spans.len().next_power_of_two());
    self.heads.clear();
    self.heads.resize(buckets, NONE);
    for id in 0..self.spans.len() {
      let bucket = self.bucket(self.hashes[id]);
      self.chain[id] = self.heads[bucket];
      self.heads[bucket] = id as u32;
    }
  }

  /// The arena, as the `str` its readers slice.
  ///
  /// Every entry was appended whole through [`insert`](Interner::insert), so every `(start, len)`
  /// in [`spans`](Interner::spans) is a pair of char boundaries and slicing by one cannot panic.
  #[inline]
  pub(super) fn names(&self) -> &str {
    &self.names
  }

  #[inline]
  pub(super) fn spans(&self) -> &[(u32, u32)] {
    &self.spans
  }

  pub(super) fn clear(&mut self) {
    self.names.clear();
    self.spans.clear();
    self.hashes.clear();
    self.chain.clear();
    // Emptied rather than refilled with the sentinel: writing `NONE` over every bucket would be
    // linear in the *largest* table this executor ever grew, once per operation, and an operation
    // that interns two names would pay for one that interned a million. The first intern grows it
    // back into the capacity this leaves behind, so the allocation is still made once.
    self.heads.clear();
  }

  pub(super) fn set_cap(&mut self, cap: u32) {
    self.cap = cap;
  }

  /// Where the arena stands, so a failed collection or expansion can put it back.
  #[inline]
  pub(super) fn mark(&self) -> (usize, usize) {
    (self.names.len(), self.spans.len())
  }

  /// Undoes every name interned since `mark`.
  ///
  /// Sound only because the ids handed out in between die with the structures being undone. The
  /// one id that would have escaped — a variable's spelling inside a collection fault's message —
  /// is minted after this runs and not before, which is why [`Fault::name`](super::collect::Fault)
  /// carries bytes.
  ///
  /// The index is unwound before the arena, and the bucket each entry is unlinked from is read out
  /// of [`hashes`](Interner::hashes) rather than recomputed from bytes the truncation is about to
  /// remove — so the unwind is one pointer write per entry and reads no arena at all.
  pub(super) fn restore(&mut self, (names, spans): (usize, usize)) {
    for id in (spans..self.spans.len()).rev() {
      let bucket = self.bucket(self.hashes[id]);
      debug_assert_eq!(
        self.heads[bucket], id as u32,
        "an id being unwound was not the head of its bucket; the chain is no longer in decreasing \
         id order, so this removal is dropping somebody else's entry"
      );
      self.heads[bucket] = self.chain[id];
    }
    self.names.truncate(names);
    self.spans.truncate(spans);
    self.hashes.truncate(spans);
    self.chain.truncate(spans);
  }
}

/// Everything one collection reuses from the last, in one struct so that the executor moves it in
/// and out with a single `take` and a new member cannot be forgotten at one of the two call sites.
///
/// Cleared, never shrunk: capacity is what makes a steady-state response allocate nothing.
pub(super) struct Scratch<'a, S> {
  /// `(group, selection)` for every selection that survived, sorted by group at the end.
  pub(super) fields: std::vec::Vec<(u32, &'a Field<S>)>,
  /// One entry per distinct response key, in the order the keys were first seen.
  ///
  /// Private, and reached through [`groups`](Scratch::groups) and [`walking`](Scratch::walking),
  /// because the walk and the commit are allowed different things. See [`mod groups`](groups).
  groups: Groups,
  /// The group each interned key landed in, or [`NONE`] — indexed by the key's interner id.
  ///
  /// This replaces a scan of `groups` per field selection. It is a *sparse* table over interner
  /// ids, so it cannot be cleared by length: `collect_fields` unmarks exactly the keys the previous
  /// collection recorded, which the previous collection's `groups` enumerate.
  ///
  /// **The invariant is `keys[i] != NONE` if and only if some entry of `groups` has key `i`**, and
  /// it holds across `reset` because neither table is emptied there — the next collection unmarks
  /// from the groups it finds, whether or not an operation ended in between.
  keys: std::vec::Vec<u32>,
  /// Draft §6.3's `visitedFragments`.
  visited: Visited,
  /// The descent, as heap rather than as native frames. See [`walk`].
  stack: std::vec::Vec<(&'a SelectionSet<S>, usize)>,
}

/// What each of [`Scratch`]'s five buffers is holding.
///
/// Named rather than a tuple, because the census that reads it is a list whose whole value is that
/// a reader can check it against the struct definition — and a five-tuple is a list a reader cannot
/// check anything against.
#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ScratchCapacity {
  /// Surviving `(group, selection)` pairs. Charged against `max_response_metadata` at the push.
  pub(super) fields: usize,
  /// One per distinct response key, so at most one per `fields` entry.
  pub(super) groups: usize,
  /// Sparse over interner ids, so bounded by the arena rather than by the response.
  pub(super) keys: usize,
  /// Draft §6.3's `visitedFragments`, as `(bitset words, ordinals set)`. Bounded by the document's
  /// fragment count, and every spread that reaches it is charged a visit.
  pub(super) visited: (usize, usize),
  /// The descent. One frame per fragment spread or inline fragment, each of them charged a visit.
  pub(super) stack: usize,
}

/// What [`walk`] is handed, with the group list restricted to the two things a walk does with it.
///
/// One struct rather than five more parameters, and the restriction is the point: [`walk`] never
/// holds a [`Scratch`], so it has no way to reach the group vector other than the [`Appending`] in
/// here — which cannot enumerate. See [`mod groups`](groups).
struct Walking<'s, 'a, S> {
  fields: &'s mut std::vec::Vec<(u32, &'a Field<S>)>,
  groups: Appending<'s>,
  keys: &'s mut std::vec::Vec<u32>,
  visited: &'s mut Visited,
  stack: &'s mut std::vec::Vec<(&'a SelectionSet<S>, usize)>,
}

impl<'a, S> Scratch<'a, S> {
  /// The groups the last collection produced, for the commit that reads them.
  #[inline]
  pub(super) fn groups(&self) -> &[Group] {
    self.groups.as_slice()
  }

  /// What the group vector is holding. See [`Groups::capacity`](groups::Groups::capacity).
  #[cfg(test)]
  #[inline]
  pub(super) fn groups_capacity(&self) -> usize {
    self.groups.capacity()
  }

  /// What every member is holding, for the census the executor keeps of its retained buffers.
  ///
  /// All five, because this scratch is *cleared and never shrunk* and the executor carries it
  /// across every `reset` — so each member is a buffer that survives an unbounded stream, and a
  /// census reading four of them is a census with a hole. See `Limits`'s own list for the ceiling
  /// each is bounded by.
  #[cfg(test)]
  #[inline]
  pub(super) fn capacities(&self) -> ScratchCapacity {
    ScratchCapacity {
      fields: self.fields.capacity(),
      groups: self.groups.capacity(),
      keys: self.keys.capacity(),
      visited: (self.visited.bits.capacity(), self.visited.seen.capacity()),
      stack: self.stack.capacity(),
    }
  }

  /// The walk's view of the scratch.
  #[inline]
  fn walking(&mut self) -> Walking<'_, 'a, S> {
    Walking {
      fields: &mut self.fields,
      groups: self.groups.appending(),
      keys: &mut self.keys,
      visited: &mut self.visited,
      stack: &mut self.stack,
    }
  }
}

impl<S> Default for Scratch<'_, S> {
  /// Scratch that allocates nothing, which is also the placeholder [`core::mem::take`] leaves
  /// behind while the real one is being written to.
  ///
  /// One constructor rather than two, because every member grows on demand — [`Visited`] included,
  /// since the fragment table it indexes is not built until the first spread. There is no size to
  /// hint at here and therefore no wrong hint to leave behind.
  fn default() -> Self {
    Self {
      fields: std::vec::Vec::new(),
      groups: Groups::default(),
      keys: std::vec::Vec::new(),
      visited: Visited::default(),
      stack: std::vec::Vec::new(),
    }
  }
}

/// Draft §6.3 `CollectFields`, over the concatenation of several selection sets.
///
/// Several rather than one because draft §6.4's `MergeSelectionSets` hands the sub-selections of
/// every field that shared a response key to the next round of collection, and running
/// `CollectFields` once over the concatenation is what that step means.
///
/// The [`Fault`] is a `@skip`/`@include` condition that could not be read. Nothing was collected
/// when one is returned, so the caller raises it at the object whose selection set this was.
#[allow(clippy::too_many_arguments)]
pub(super) fn collect_fields<'a, S, V>(
  schema: &Schema,
  fragments: &mut Fragments<'a, S>,
  object_type: TypeId,
  sets: &[&'a SelectionSet<S>],
  ctx: &mut V,
  interner: &mut Interner,
  scratch: &mut Scratch<'a, S>,
  visits: &mut Visits,
  metadata: Allowance,
) -> Result<(), Fault<'a>>
where
  S: AsRef<[u8]>,
  V: Values,
{
  // Before `groups` is emptied, because `groups` is the list of what to unmark.
  for group in scratch.groups.as_slice() {
    scratch.keys[group.key as usize] = NONE;
  }
  scratch.fields.clear();
  scratch.groups.clear();
  scratch.visited.clear();
  for set in sets {
    let walked = walk(
      schema,
      fragments,
      object_type,
      set,
      ctx,
      interner,
      scratch.walking(),
      visits,
      metadata,
    );
    if let Err(fault) = walked {
      // Hygiene, not lifetime: the descent's borrows outlive the executor, so leaving them costs
      // nothing but a reader's time. Done here rather than at the caller so the field stays private
      // to this module and neither exit can forget it.
      scratch.stack.clear();
      return Err(fault);
    }
  }
  scratch.stack.clear();
  // Stable, so document order inside a group survives; by group, so every group is contiguous.
  scratch.fields.sort_by_key(|&(group, _)| group);
  let mut cursor = 0usize;
  for (index, group) in scratch.groups.iter_mut().enumerate() {
    let start = cursor;
    while cursor < scratch.fields.len() && scratch.fields[cursor].0 as usize == index {
      cursor += 1;
    }
    group.start = start as u32;
    group.len = (cursor - start) as u32;
  }
  Ok(())
}

/// Draft §6.3's descent, over an explicit stack rather than the call stack.
///
/// # Why this is not recursive
///
/// A named fragment chain is **flat in the document**: `fragment F0 … ...F1`, each definition at
/// nesting depth one, so no parser depth limit sees it and none can. A recursive walk spends one
/// native frame per link, and a chain of a few thousand fragments — a couple of hundred kilobytes
/// of perfectly valid text — overflowed the stack and took the process down with `SIGABRT`. That is
/// not a catchable panic: a server cannot turn it into a `400`, and one request kills every other
/// in flight.
///
/// **"A few thousand" is as precise as the number can honestly be, and that is the argument.** It
/// was measured at 1,500 links when the recursion was deleted; re-measured by restoring the
/// recursion on this tree, 1,500 links answered in 1.9 ms and 10,000 aborted — a release build on a
/// different platform with a different frame layout. So the frame is gone rather than counted: a
/// depth *ceiling* would be a ceiling whose right value is a function of each deployment's stack
/// size, build profile and frame layout, and an explicit stack makes the question disappear,
/// because depth is heap the visit budget already bounds.
///
/// Inline fragments recurse in the document rather than through definitions, and are **not** the
/// reachable case: the parser aborts on its own at around sixty levels (al8n/smear#61), so no
/// document deep enough to trouble this walk survives to reach it. The flat chain is the one that
/// gets here.
///
/// # Every lookup is charged, including the ones a hash usually makes free
///
/// A spread's fragment, a response key's interner entry and that key's group were three linear
/// scans, one per selection, and together they made a selection set of `n` distinct keys cost `n²`
/// — under a budget whose unit was one *selection*, which read `n`. The group is now a direct index
/// by interner id, and the other two are hash probes whose comparisons are charged to that budget,
/// so the ceiling bounds collection's work rather than its trip count. [`Visits`] carries the
/// argument and its limits.
///
/// The group index is the one of the three a charge cannot guard, and it is guarded by this
/// function's *arguments* instead: what arrives is a [`Walking`], whose [`Appending`] can append a
/// group and read one back and cannot enumerate. [`mod groups`](groups) has the reasoning.
///
/// # The order is the recursion's, exactly
///
/// Draft §6.3 fixes response-key order to document order, and `MergeSelectionSets` concatenates in
/// document order within a key — so this has to be pre-order depth-first, entering a fragment
/// where its spread sits and resuming at the *next sibling* afterwards. That is why the stack
/// holds `(set, index)` and not just `set`: the index is where the recursion would have resumed.
#[allow(clippy::too_many_arguments)]
fn walk<'a, S, V>(
  schema: &Schema,
  fragments: &mut Fragments<'a, S>,
  object_type: TypeId,
  set: &'a SelectionSet<S>,
  ctx: &mut V,
  interner: &mut Interner,
  scratch: Walking<'_, 'a, S>,
  visits: &mut Visits,
  metadata: Allowance,
) -> Result<(), Fault<'a>>
where
  S: AsRef<[u8]>,
  V: Values,
{
  let Walking {
    fields,
    mut groups,
    keys,
    visited,
    stack,
  } = scratch;
  stack.clear();
  stack.push((set, 0));

  while let Some(&mut (set, ref mut cursor)) = stack.last_mut() {
    let selections = set.selections();
    let Some(selection) = selections.get(*cursor) else {
      stack.pop();
      continue;
    };
    *cursor += 1;

    // Charged here, before the arms, so that *every* selection examined costs the same whether or
    // not it survives. Charging what is appended instead — which is what the metadata ceiling
    // does — leaves a document made of fragments that collect nothing walking for free, and that
    // document is as cheap to write as one that collects everything.
    visits.spend(1, *set.as_span())?;

    match selection {
      Selection::Field(field) => {
        if !included(field.directives(), ctx)? {
          continue;
        }
        let spelling = match field.alias() {
          Some(alias) => alias.name().source().as_ref(),
          None => field.name().source().as_ref(),
        };
        // Draft §7.1.2 makes the response key this spelling verbatim, so a spelling that is not a
        // name is a key no response can carry. Raised rather than substituted, and raised rather
        // than skipped: the client asked for this position, and a response that quietly lacks a
        // key it asked for is the one outcome with nothing in `errors` to account for it. Draft
        // §2.1.9 puts this beyond a lexed document, so only an assembled one reaches it.
        //
        // `name_key` and not `from_utf8`: the question is draft §2.1.9's and not "do these bytes
        // print". This line checked only the conversion for one round, and `""`, `1abc`, `a b` and
        // `🙂` were all interned and handed back as a `Segment::Field` — while `variable_key`, in
        // this same module, refused every one of them. See `name_key`.
        let Some(name) = name_key(spelling) else {
          return Err(Fault {
            raw: Raw::ResponseKeyUnreadable,
            location: *field.span(),
            name: None,
          });
        };
        let key = match interner.intern(name, visits) {
          Ok(key) => key,
          Err(unstored) => {
            return Err(Fault {
              raw: match unstored {
                Unstored::Arena { limit } => Raw::NameStorage { limit },
                Unstored::Budget { limit } => Raw::CollectionBudget { limit },
              },
              // The field's span for the reason the staging charge uses it: a collection-side
              // refusal reports where a commit-side one would, and a commit-side location includes
              // the alias.
              location: *field.span(),
              name: None,
            });
          }
        };
        // One index, where this used to scan the groups. `keys` is sparse over interner ids, so it
        // is grown to reach the id rather than to the arena's size — an id is only ever minted by
        // the call above, so growth here is one entry at a time and cannot outrun the arena.
        if keys.len() <= key as usize {
          keys.resize(key as usize + 1, NONE);
        }
        let group = match keys[key as usize] {
          NONE => {
            let index = groups.push(Group {
              key,
              start: 0,
              len: 0,
              first: *field.span(),
            });
            keys[key as usize] = index;
            index
          }
          index => index,
        };
        // Charged before the push, against the ceiling this staging buffer is staging *for*.
        //
        // Every surviving selection here becomes exactly two metadata entries when `expand`
        // commits it — one merged selection and one location — so `fields` is response metadata in
        // waiting, and the population it belongs to already has a ceiling. Without this the buffer
        // grew under the *visit* budget instead: the loosest ceiling on the path rather than the
        // one that decides the outcome, so a request refused for metadata could first grow the
        // scratch by orders of magnitude more than the refusal permits, and `reset` reuses rather
        // than shrinks.
        //
        // Reading lengths, not a counter: `fields.len()` is the staged half and the caller's
        // allowance is the committed half. That is the module's standing rule, and it is what keeps
        // this from drifting out of step with the commit-side charge — `expand` charges
        // `2 * group.len` per group, which sums to exactly the `2 * fields.len()` measured here.
        // The group's first selection, not this one. A merged response key — `{ x x }` — crosses
        // on its later duplicate while the commit path reports the first, and reading the stored
        // span is what makes the two the same value rather than two computations that agree.
        metadata.admits(fields.len(), groups.first(group))?;
        fields.push((group, field));
      }
      Selection::FragmentSpread(spread) => {
        if !included(spread.directives(), ctx)? {
          continue;
        }
        // Indexed here rather than at construction, and charged. A document whose fragments are
        // never spread never reaches this line and never pays for them.
        fragments.build(visits, *spread.span())?;
        let found = fragments.get(spread.name().source().as_ref(), visits, *spread.span())?;
        let Some((ordinal, fragment)) = found else {
          // Draft 5.5.2.1 makes an undefined spread a validation failure, so this is unreachable
          // for a validated document. Skipping is the only behaviour that cannot invent a field.
          continue;
        };
        if visited.visited(ordinal) {
          continue;
        }
        let condition = fragment.type_condition().name().source().as_ref();
        // The probe below hashes the *document's* spelling, and the residual that cleared
        // `Schema::sym` cleared the wrong half of it: the table is the schema's, so a client
        // cannot lengthen the run it walks — and a client does choose every byte of the key, so it
        // can lengthen the *hash*. A one-unit charge over a spelling with no ceiling is a ledger
        // over the number of probes and not over probing. See `Visits::take_bytes`.
        visits.spend_bytes(condition.len(), *spread.span())?;
        if !applies(schema, condition, object_type) {
          continue;
        }
        stack.push((fragment.selection_set(), 0));
      }
      Selection::InlineFragment(inline) => {
        if !included(inline.directives(), ctx)? {
          continue;
        }
        if let Some(condition) = inline.type_condition() {
          // Charged for the same reason the spread's condition above is: the key is the client's
          // and so is its length.
          let condition = condition.name().source().as_ref();
          visits.spend_bytes(condition.len(), *inline.span())?;
          if !applies(schema, condition, object_type) {
            continue;
          }
        }
        stack.push((inline.selection_set(), 0));
      }
    }
  }
  Ok(())
}

/// Draft §6.3's `DoesFragmentTypeApply`, all three arms at once.
///
/// [`Schema::is_possible_object`] is one bitset test and already answers the object, interface and
/// union cases the specification spells out separately — the same word the validator's draft
/// 5.5.2.3 reads.
fn applies(schema: &Schema, condition: &[u8], object_type: TypeId) -> bool {
  let Some(sym) = schema.sym(condition) else {
    return false;
  };
  let Some(id) = schema.type_of_sym(sym) else {
    return false;
  };
  schema.is_possible_object(id, object_type)
}

/// Draft §6.3 steps 3.a and 3.b: `@skip`, then `@include`.
///
/// Two passes rather than one over the directive list, because the step order is the
/// specification's and not the document's. A selection carrying both is removed if `@skip` says
/// so whatever `@include` says — and once step 3.a has removed it, step 3.b never runs, so
/// `{ f @include(if: $unreadable) @skip(if: true) }` produces no error. Reading them in document
/// order would raise one, and the reference implementation does not.
fn included<'a, S, V>(directives: Option<&'a Directives<S>>, ctx: &mut V) -> Result<bool, Fault<'a>>
where
  S: AsRef<[u8]>,
  V: Values,
{
  let Some(directives) = directives else {
    return Ok(true);
  };
  for directive in directives.directives() {
    if directive.name().source().as_ref() == b"skip" && condition_is_true(directive, ctx)? {
      return Ok(false);
    }
  }
  for directive in directives.directives() {
    if directive.name().source().as_ref() == b"include" && !condition_is_true(directive, ctx)? {
      return Ok(false);
    }
  }
  Ok(true)
}

/// The key [`Values::variable`](super::Values::variable) looks a variable up by, or `None` when
/// the document's spelling is not a draft §2.1.9 `Name`.
///
/// # Why there is a conversion here at all
///
/// Not an impedance mismatch to be removed by widening the trait. The two ends genuinely differ:
/// draft §6.1's `CoerceVariableValues` runs over the *request's* `variableValues`, whose keys
/// arrive as text and are a driver's `&str`, while a document's spelling is a slice of an
/// `S: AsRef<[u8]>` that nothing constrains. A conversion at a boundary belongs on the side that
/// knows both key spaces rather than repeated inside every driver, where the same fallback could
/// be written again and no gate in this repository would see it.
///
/// This is not the *only* place the two spaces meet, which is why the function is public rather
/// than private to the executor. Draft §6.4.1 step 5.j — coercing a literal's contents — is the
/// driver's, so a variable nested inside a list or an input object reaches it inside
/// [`ArgumentSource::Literal`](super::ArgumentSource::Literal) and the driver resolves that one
/// itself. What "the conversion happens once" can honestly mean is one *implementation*, called by
/// every reader; a driver walking a literal calls this.
///
/// # The grammar, and not merely UTF-8
///
/// A UTF-8 check answers "can these bytes be printed", and the question is "can these bytes be a
/// key the request supplied a value for". Draft §6.1 iterates the operation's
/// `variableDefinitions`, whose names are `Name`s, so a `variableValues` entry that is not a
/// `Name` matches no definition and names no variable. `$1abc`, `$ `, `$🙂`, an empty spelling and
/// a decomposed spelling of a composed one are all valid UTF-8 and none of them is a `Name` — so a
/// UTF-8 check handed every one of them to the driver as a lookup key, and a driver whose map
/// happened to hold that key **satisfied** the argument with a value no variable in the document
/// declared. The pair is the sharper case: two normalisations are two distinct `&str` keys here and
/// one key in any driver that normalises, which is the collapse this whole issue is about arriving
/// through the readable branch.
///
/// The admission rule is [`is_name`], the schema arena's own, reused rather than respelled — and
/// reached through the *same private function* draft §7.1.2's response key is admitted by, so the
/// executor's name spaces share one predicate by calling it rather than by saying so. That
/// distinction is the whole of al8n/smear#139's third round: an earlier revision of this paragraph
/// claimed the sharing while the response key's site still checked UTF-8 alone, and a claim in
/// prose is not something a compiler can read.
///
/// # `None` is a refusal, and specifically not a substitution
///
/// A spelling the driver's key space cannot express names no variable the request could have
/// supplied, so both readers take their existing "not provided" branch: draft §6.4.1 step 5.d at a
/// field argument, draft §6.3's `VariableMissing` — reported as
/// [`Kind::DirectiveCondition`](super::Kind::DirectiveCondition) — at a condition. Neither asks the
/// driver, because there is no question to ask.
///
/// What this must never do is answer with a *different* name. `from_utf8(..).unwrap_or("")` did,
/// at draft §6.4.1's site, and mapping every unreadable spelling onto one readable one merged two
/// variables into one, asked the driver about a variable the document does not contain, carried
/// that invented name to the resolver in
/// [`ArgumentSource::Variable`](super::ArgumentSource::Variable) and printed it as `$` with
/// nothing after it. See al8n/smear#139.
#[inline]
pub fn variable_key(spelling: &[u8]) -> Option<&str> {
  name_key(spelling)
}

/// The `&str` `spelling` names, or `None` when it is not a draft §2.1.9 `Name`.
///
/// # One function, because the defect was a site that performed half of it
///
/// Every name this executor admits **out of a document** is admitted here: draft §7.1.2's response
/// key, and draft §6.1's variable key through [`variable_key`]. They were not always: the response
/// key's site checked UTF-8 alone, so `1abc`, `a b`, `🙂` and the empty spelling were interned and
/// handed back as [`Segment::Field`](super::Segment) with no error — and the round that introduced
/// that guard is the one whose prose said both spaces shared a predicate. Two sites spelling
/// "predicate, then conversion" is exactly the shape one of them can write half of; one function
/// is not.
///
/// The predicate is [`is_name`], the schema arena's own admission rule, so the *third* name space
/// the executor reads — the schema's, which `Schema::build` already gates with it — is admitted by
/// the same rule rather than by a second spelling of draft §2.1.9 that can drift from the first.
///
/// The conversion after it cannot fail, because a `Name` is ASCII. It is still written as the
/// total conversion: this crate has no `unsafe`, and the alternative to a second pass over a name
/// that is at most a few bytes is exactly the kind of claim that stops being true when someone
/// changes [`is_name`].
///
/// # What is deliberately *not* admitted here
///
/// A spelling this crate never reads as a name does not come through this door, and pushing it
/// through would refuse readable input for nothing. Three kinds:
///
/// - **Byte-exact identity inside one document** — a fragment spread against its definition, an
///   operation name against the one [`Executor::start`](super::Executor::start) was given. Nothing
///   is interned, nothing is rendered, and byte equality cannot map two spellings onto one, which
///   is the failure this whole repair is about.
/// - **A lookup in a key space that is already gated** — a field name, a type condition, an
///   argument name, a directive name, a driver's `type_name`. Each is matched against the schema's
///   arena or against an ASCII literal, and a spelling that is not a `Name` is in neither, so the
///   miss *is* the refusal. A second check here would be the redundant spelling the paragraph
///   above refuses.
/// - **Text that was never a name** — a resolver's message, a draft §7.1.7 extensions key, which
///   §7.1.7 puts under no lexical restriction at all.
#[inline]
pub(super) fn name_key(spelling: &[u8]) -> Option<&str> {
  if !is_name(spelling) {
    return None;
  }
  core::str::from_utf8(spelling).ok()
}

/// Whether the directive's `if` argument is `true`, or why it could not be read as a boolean.
///
/// # Why a condition that cannot be read is an error and not a `false`
///
/// Because the two directives consume the answer with opposite sign. `@skip` removes the selection
/// when the condition is `true` and `@include` removes it when it is not, so a boolean default
/// that closes the disclosure for one opens it for the other: `false` keeps an `@skip`ped
/// selection and `true` keeps an `@include`d one. No third boolean exists. A guard whose condition
/// could not be evaluated therefore cannot be answered with a boolean at all, and the only outcome
/// safe under both senses is to raise and let draft §6.4.4 null the position.
///
/// # What the reference implementation does, measured
///
/// `graphql-js` 16.11.0 reads the condition through `getDirectiveValues`, which is full
/// `CoerceArgumentValues` over the directive's arguments, and raises on all three of the failures
/// draft §6.4.1 names. Run against it, `query ($flag: Boolean = true) { secret @skip(if: $flag) }`
/// with a runtime `flag: null` — a document its own validator accepts — answers
/// `{"errors":[{"message":"Argument \"if\" of non-null type \"Boolean!\" must not be null.",
/// "locations":[…]}],"data":null}`: the error carries no `path`, because at the root selection set
/// there is no field to attribute it to, and `data` is present and null, which is draft §7.1.1's
/// shape for an error raised *during* execution rather than before it. A genuine request error —
/// §6.1 `CoerceVariableValues` failing — omits `data` entirely there, so the two are distinct and
/// this is the field-shaped one. Several levels down the same condition produces the same message
/// with the enclosing object's path, and nulls that object.
///
/// This function reproduces that, including the messages word for word, with one deliberate
/// divergence: a value that is neither null nor a boolean.
/// [`ConditionFault::NotABoolean`] raises where `graphql-js` treats the directive as inert and
/// **returns the guarded selection**. That input needs an invalid document — draft 5.8.5 forbids a
/// non-`Boolean` variable at the `Boolean!` location — or a driver whose §6.1 did not coerce, so
/// no conforming request can tell the two apart; and of the two answers only this one is safe
/// under both senses.
fn condition_is_true<'a, S, V>(directive: &'a Directive<S>, ctx: &mut V) -> Result<bool, Fault<'a>>
where
  S: AsRef<[u8]>,
  V: Values,
{
  let argument = directive.arguments().and_then(|arguments| {
    arguments
      .arguments()
      .iter()
      .find(|argument| argument.name().source().as_ref() == b"if")
  });
  let Some(argument) = argument else {
    return Err(Fault {
      raw: Raw::DirectiveCondition {
        fault: ConditionFault::Missing,
      },
      location: *directive.span(),
      name: None,
    });
  };
  // Every remaining failure is about the value, so it is what the error points at — the same node
  // `graphql-js` reports, which for `if: $flag` is the variable and not the directive.
  let location = *argument.value().as_span();
  let unreadable = |fault| Fault {
    raw: Raw::DirectiveCondition { fault },
    location,
    name: None,
  };
  match argument.value() {
    InputValue::Boolean(literal) => Ok(literal.value()),
    InputValue::Variable(spelled) => {
      // Read once. Interning the name costs a scan of the name table, so it happens only on the
      // branch that needs it for a message.
      let spelling = spelled.name().source().as_ref();
      match variable_key(spelling).and_then(|name| ctx.variable(name)) {
        // The variable was not supplied, and that is the finding whether or not its spelling can
        // be quoted — so an arena with no room shortens the message and keeps the diagnosis. The
        // spelling travels as bytes because the caller has an arena to restore before it can mint
        // an id that survives.
        None => Err(Fault {
          raw: Raw::DirectiveCondition {
            fault: ConditionFault::VariableMissing { variable: None },
          },
          location,
          name: Some(spelling),
        }),
        Some(value) if ctx.is_null(&value) => Err(unreadable(ConditionFault::Null)),
        Some(value) => ctx
          .as_bool(&value)
          .ok_or_else(|| unreadable(ConditionFault::NotABoolean)),
      }
    }
    InputValue::Null(_) => Err(unreadable(ConditionFault::Null)),
    _ => Err(unreadable(ConditionFault::NotABoolean)),
  }
}
