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

use crate::{
  parser::graphql::ast::{
    Directive, Directives, ExecutableDefinition, ExecutableDocument, Field, FragmentDefinition,
    InputValue, Selection, SelectionSet,
  },
  validator::schema::{Schema, TypeId, bucket, hash_bytes},
};

use super::{
  Values,
  error::{ConditionFault, Raw},
};

/// Why [`Interner::intern`] could not store a name.
///
/// Two, because the two degrade differently at the one caller that does not simply fail: a name is
/// still *readable* when the arena is full, and the collection path has a different message for
/// each.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Unstored {
  /// [`max_interned_bytes`](super::Limits::max_interned_bytes) has no room for the bytes.
  Arena,
  /// [`max_selection_visits`](super::Limits::max_selection_visits) had no room for the lookup.
  Budget,
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
/// hash text an adversary writes, and [`hash_bytes`]'s multiply-fold is invertible, so a pile-up in
/// one bucket is constructible rather than unlucky — charging makes that spend the client's budget
/// instead of the server's time, and is why the bound does not rest on the hash behaving.
///
/// # Charged before the work, and over the population as well as the lookup
///
/// Two properties, and the first version of this had neither for one of the two tables.
///
/// **Before.** [`spend`](Visits::spend) is called ahead of the work it pays for, so a probe run is
/// abandoned in the middle of a bucket rather than at the end of one. A charge taken afterwards
/// bounds nothing by itself: the run is already walked when the refusal arrives.
///
/// **Over the population.** [`Interner`] is filled *during* collection, so a client that builds a
/// collision pays for building it. [`Fragments`] was filled at executor construction, outside this
/// budget entirely, from names the document chooses — so the same argument was simply false there:
/// the collision came free and one cheap valid spread walked it. Its pass is now charged too.
///
/// **The question to ask of every structure phases 2–8 add is therefore both halves:** is the
/// lookup charged, *and* was the thing being looked in built inside the budget or outside it. A
/// bound on the lookup is worth nothing over a table an adversary filled for free.
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
/// **The group lookup, and this one cannot be made a type.** It is a direct index with no loop, so
/// there is nothing to charge — and equally nothing a counter could see if a scan came back. Hiding
/// `groups` behind a lookup-only API would prevent it, but `expand` iterates that very vector to
/// commit the groups, so the type it would need is one that both permits and forbids the same
/// access. Held by review and by the timings, and stated as unenforceable rather than as safe.
///
/// **`Schema::sym`, which is examined and cleared rather than left unsaid.** `expand` and
/// [`applies`] probe the schema's [`NameIndex`](crate::validator::NameIndex) with *document* bytes,
/// uncharged. The rule above is what clears it: the table is populated from the **schema**, which
/// the operator wrote, so a client chooses the lookup key and never the run it walks. That is the
/// same question that condemned [`Fragments`], answered the other way.
///
/// [`max_response_slots`]: super::Limits::max_response_slots
pub(super) struct Visits {
  left: u32,
  limit: u32,
}

impl Visits {
  #[inline]
  pub(super) const fn new(limit: u32) -> Self {
    Self { left: limit, limit }
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
  /// The named fragments in document order, empty until [`build`](Fragments::build) has run. An
  /// index into this is a *fragment ordinal*, which is what [`Visited`] is a bitset over.
  defs: std::vec::Vec<&'a FragmentDefinition<S>>,
  /// The newest ordinal in each bucket, or [`NONE`]. Power-of-two length.
  heads: std::vec::Vec<u32>,
  /// The ordinal pushed into the same bucket before this one, or [`NONE`]. Parallel to `defs`.
  chain: std::vec::Vec<u32>,
  /// Whether the pass has run. Distinct from `defs.is_empty()`, which is also true of a document
  /// that defines no fragments and must not be indexed again on every spread it does not have.
  built: bool,
  /// Entries compared, over the executor's whole life.
  ///
  /// The gate for "a refused probe run stops at the refusal" reads this. It cannot be read from the
  /// budget: every comparison is charged before it happens, so the charge and the comparison count
  /// agree by construction and a version that charged afterwards would agree with itself too. Only
  /// a count taken independently of the charge can tell the two apart.
  #[cfg(test)]
  compares: u64,
}

impl<'a, S> Fragments<'a, S>
where
  S: AsRef<[u8]>,
{
  /// An index over `document`'s named fragments, which does not read `document` yet.
  pub(super) fn new(document: &'a ExecutableDocument<S>) -> Self {
    Self {
      document,
      defs: std::vec::Vec::new(),
      heads: std::vec::Vec::new(),
      chain: std::vec::Vec::new(),
      built: false,
      #[cfg(test)]
      compares: 0,
    }
  }

  /// Runs the indexing pass once, charging it before it runs.
  ///
  /// Two charges rather than one, because the second quantity is not known until the first pass has
  /// happened: `definitions` for the walk that finds the fragments, then `fragments` for the pushes
  /// that index them. Both are known **before** the work they pay for — the first is a slice
  /// length, and the second is exact because chaining has no data-dependent insertion loop.
  ///
  /// A refusal leaves the table unbuilt rather than half built. Nothing here can fail after the
  /// second charge, so "charged" and "complete" are the same state.
  fn build(&mut self, visits: &mut Visits, location: SimpleSpan) -> Result<(), Fault<'static>> {
    if self.built {
      return Ok(());
    }
    let definitions = self.document.definitions();
    visits.spend(
      u32::try_from(definitions.len()).unwrap_or(u32::MAX),
      location,
    )?;
    self.defs.extend(
      definitions
        .iter()
        .filter_map(|described| match described.node() {
          ExecutableDefinition::Fragment(fragment) => Some(fragment),
          ExecutableDefinition::Operation(_) => None,
        }),
    );
    let count = self.defs.len();
    if let Err(refusal) = visits.spend(u32::try_from(count).unwrap_or(u32::MAX), location) {
      // Back to unbuilt, so a later collection sees a table that was never indexed rather than one
      // indexed halfway. The pass this discards was charged and stays charged: it happened.
      self.defs.clear();
      return Err(refusal);
    }
    if count > 0 {
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
      for ordinal in 0..count {
        let at = bucket(
          hash_bytes(self.defs[ordinal].name().source().as_ref()),
          mask,
        ) as usize;
        self.chain[ordinal] = self.heads[at];
        self.heads[at] = ordinal as u32;
      }
    }
    self.built = true;
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
    if self.heads.is_empty() {
      return Ok(None);
    }
    let mask = (self.heads.len() - 1) as u32;
    let mut ordinal = self.heads[bucket(hash_bytes(name), mask) as usize];
    while ordinal != NONE {
      visits.spend(1, location)?;
      #[cfg(test)]
      {
        self.compares += 1;
      }
      let fragment = self.defs[ordinal as usize];
      if fragment.name().source().as_ref() == name {
        return Ok(Some((ordinal, fragment)));
      }
      ordinal = self.chain[ordinal as usize];
    }
    Ok(None)
  }

  /// Entries this executor's fragment lookups have compared. See the field.
  #[cfg(test)]
  pub(super) const fn compares(&self) -> u64 {
    self.compares
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
/// entry carries bookkeeping on top: eight bytes of `spans`, four of `chain`, up to eight of
/// `heads`, and four of the caller's key-to-group scratch. A GraphQL name is at least one byte, so
/// an arena of `B` bytes can hold `B` entries and cost about `25 · B`, where before this index it
/// cost about `9 · B`. That is a constant and not a second factor, which is what keeps it a memory
/// *cost* rather than the product shape this module refuses — but it is a constant a caller
/// choosing the ceiling is choosing too, so [`max_interned_bytes`] says so as well.
///
/// [`max_interned_bytes`]: super::Limits::max_interned_bytes
#[derive(Debug)]
pub(super) struct Interner {
  bytes: std::vec::Vec<u8>,
  spans: std::vec::Vec<(u32, u32)>,
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
      bytes: std::vec::Vec::new(),
      spans: std::vec::Vec::new(),
      #[cfg(test)]
      compares: 0,
      heads: std::vec::Vec::new(),
      chain: std::vec::Vec::new(),
      cap,
    }
  }

  /// The ceiling this table refuses at, for the message that reports the refusal.
  #[inline]
  pub(super) const fn cap(&self) -> u32 {
    self.cap
  }

  /// Returns the id for `bytes`, adding it if it is not already there, charging `visits` before
  /// **each** entry it compares.
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
  pub(super) fn intern(&mut self, bytes: &[u8], visits: &mut Visits) -> Result<u32, Unstored> {
    let hash = hash_bytes(bytes);
    if !self.heads.is_empty() {
      let mut id = self.heads[self.bucket(hash)];
      while id != NONE {
        if !visits.take(1) {
          return Err(Unstored::Budget);
        }
        #[cfg(test)]
        {
          self.compares += 1;
        }
        let (start, len) = self.spans[id as usize];
        if &self.bytes[start as usize..(start + len) as usize] == bytes {
          return Ok(id);
        }
        id = self.chain[id as usize];
      }
    }
    self.insert(bytes, hash).ok_or(Unstored::Arena)
  }

  /// Entries this executor's name lookups have compared. See [`Fragments::compares`] for why a
  /// count kept beside the charge is the only witness for charging before rather than after.
  #[cfg(test)]
  pub(super) const fn compares(&self) -> u64 {
    self.compares
  }

  /// Appends `bytes` and links it, or `None` when the arena has no room.
  ///
  /// Unbudgeted, and it does not need to be: it runs at most once per selection, which the caller
  /// has already charged, and the rehash it may trigger is amortised over those same insertions.
  fn insert(&mut self, bytes: &[u8], hash: u64) -> Option<u32> {
    // Checked, not reasoned about. The ceiling below makes each of these unreachable, and they
    // stay because "unreachable given the ceiling" is exactly the kind of claim that stops being
    // true when somebody sets a different ceiling.
    let start = u32::try_from(self.bytes.len()).ok()?;
    let len = u32::try_from(bytes.len()).ok()?;
    let end = start.checked_add(len)?;
    if end > self.cap {
      return None;
    }
    let id = u32::try_from(self.spans.len()).ok()?;
    self.bytes.extend_from_slice(bytes);
    self.spans.push((start, len));
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
  fn rehash(&mut self) {
    let buckets = self
      .heads
      .len()
      .max(FIRST_BUCKETS)
      .max(self.spans.len().next_power_of_two());
    self.heads.clear();
    self.heads.resize(buckets, NONE);
    for id in 0..self.spans.len() {
      let (start, len) = self.spans[id];
      let hash = hash_bytes(&self.bytes[start as usize..(start + len) as usize]);
      let bucket = self.bucket(hash);
      self.chain[id] = self.heads[bucket];
      self.heads[bucket] = id as u32;
    }
  }

  #[inline]
  pub(super) fn bytes(&self) -> &[u8] {
    &self.bytes
  }

  #[inline]
  pub(super) fn spans(&self) -> &[(u32, u32)] {
    &self.spans
  }

  pub(super) fn clear(&mut self) {
    self.bytes.clear();
    self.spans.clear();
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
    (self.bytes.len(), self.spans.len())
  }

  /// Undoes every name interned since `mark`.
  ///
  /// Sound only because the ids handed out in between die with the structures being undone. The
  /// one id that would have escaped — a variable's spelling inside a collection fault's message —
  /// is minted after this runs and not before, which is why [`Fault::name`](super::collect::Fault)
  /// carries bytes.
  ///
  /// The index is unwound before the arena, because unlinking an entry needs the bytes the
  /// truncation is about to remove.
  pub(super) fn restore(&mut self, (bytes, spans): (usize, usize)) {
    for id in (spans..self.spans.len()).rev() {
      let (start, len) = self.spans[id];
      let hash = hash_bytes(&self.bytes[start as usize..(start + len) as usize]);
      let bucket = self.bucket(hash);
      debug_assert_eq!(
        self.heads[bucket], id as u32,
        "an id being unwound was not the head of its bucket; the chain is no longer in decreasing \
         id order, so this removal is dropping somebody else's entry"
      );
      self.heads[bucket] = self.chain[id];
    }
    self.bytes.truncate(bytes);
    self.spans.truncate(spans);
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
  pub(super) groups: std::vec::Vec<Group>,
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
      groups: std::vec::Vec::new(),
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
  for group in &scratch.groups {
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
      scratch,
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
  scratch: &mut Scratch<'a, S>,
  visits: &mut Visits,
  metadata: Allowance,
) -> Result<(), Fault<'a>>
where
  S: AsRef<[u8]>,
  V: Values,
{
  let Scratch {
    fields,
    groups,
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
        let name = match field.alias() {
          Some(alias) => alias.name().source().as_ref(),
          None => field.name().source().as_ref(),
        };
        let key = match interner.intern(name, visits) {
          Ok(key) => key,
          Err(unstored) => {
            return Err(Fault {
              raw: match unstored {
                Unstored::Arena => Raw::NameStorage {
                  limit: interner.cap(),
                },
                Unstored::Budget => Raw::CollectionBudget {
                  limit: visits.limit(),
                },
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
            let index = groups.len() as u32;
            groups.push(Group {
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
        metadata.admits(fields.len(), groups[group as usize].first)?;
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
        if !applies(schema, condition, object_type) {
          continue;
        }
        stack.push((fragment.selection_set(), 0));
      }
      Selection::InlineFragment(inline) => {
        if !included(inline.directives(), ctx)? {
          continue;
        }
        if let Some(condition) = inline.type_condition()
          && !applies(schema, condition.name().source().as_ref(), object_type)
        {
          continue;
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
      match core::str::from_utf8(spelling)
        .ok()
        .and_then(|name| ctx.variable(name))
      {
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
