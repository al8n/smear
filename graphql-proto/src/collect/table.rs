use tokora::SimpleSpan;

use smear_parser::graphql::ast::{
  DescribedExecutableDefinition, ExecutableDefinition, FragmentDefinition,
};
use smear_schema::{bucket, hash_bytes};

use std::vec::Vec;

use super::{Compared, Fault, NONE, Visits, byte_units, metered_eq};

/// One accepted charge, and the fragments it was taken over.
///
/// Neither [`Copy`] nor [`Clone`], and [`Table::fill`] takes it by value, so one charge admits
/// one population and a second needs a second charge.
pub(super) struct Paid<'a, S> {
  /// The fragments the charged walk selected, which is also what the population indexes — not a
  /// count of them, and not the slice they were selected out of. See the module.
  fragments: Vec<&'a FragmentDefinition<S>>,
  /// What hashing every one of those names costs, in [`byte_units`], saturating.
  ///
  /// Carried on the receipt rather than recomputed by [`Table::fill`] so that the number the
  /// population is admitted to spend is the number that was accepted, and so that the table can
  /// keep it: a warm executor charges this again without the names being read a second time.
  name_units: u32,
}

/// A document's named fragments, chained by the hash of their names.
pub(super) struct Table<'a, S> {
  /// The named fragments in document order, empty until [`fill`](Table::fill) has run. An index
  /// into this is a *fragment ordinal*, which is what [`Visited`](super::Visited) is a bitset
  /// over.
  defs: Vec<&'a FragmentDefinition<S>>,
  /// Each definition's whole [`hash_bytes`], parallel to `defs`.
  ///
  /// The same eight bytes an entry, bought for the same thing, that [`Interner`](super::Interner)
  /// buys them for: a chain step rejects a bucket collision on this word and the name's length,
  /// so the `memcmp` a collision used to force is not made at all. The names here are the
  /// **document's**, and the hash is unkeyed, so the collision is constructible — and without the
  /// stored hash `k` colliding names of `L` bytes cost `k` charged units and `k · L` compared
  /// bytes on every spread that lands in that bucket. al8n/smear#172.
  hashes: Vec<u64>,
  /// The newest ordinal in each bucket, or [`NONE`]. Power-of-two length.
  heads: Vec<u32>,
  /// The ordinal pushed into the same bucket before this one, or [`NONE`]. Parallel to `defs`.
  chain: Vec<u32>,
  /// Whether the pass has run. Distinct from `defs.is_empty()`, which is also true of a document
  /// that defines no fragments and must not be indexed again on every spread it does not have.
  ///
  /// It is **not** the record of who has paid: this outlives the operation and the charge does
  /// not. That one is `Visits::fragments_charged`.
  indexed: bool,
  /// What hashing every indexed name cost, in [`byte_units`], for the operation that paid it.
  ///
  /// Zero until [`fill`](Table::fill) has run. It is here for the same reason the count is
  /// re-spent on the cached path: a table this executor already built is work a later operation
  /// does not repeat, and a *charge* a later operation must repeat anyway, or the same request
  /// is refused cold and served warm.
  name_units: u32,
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
  /// Name bytes [`fill`](Table::fill) has hashed, over the executor's whole life.
  ///
  /// Counted for the reason `walked` is, one dimension over. `Table::charge` prices this pass and
  /// `fill` performs it, so the charge and the pass agree by construction and a version that
  /// priced it in *definitions* — as this did — agrees with itself too. Only a count taken at the
  /// read can say whether a refused operation hashed a name before it was refused.
  /// al8n/smear#196.
  #[cfg(test)]
  hashed: u64,
}

impl<'a, S> Table<'a, S> {
  /// A table that has read nothing and reserved nothing.
  pub(super) const fn new() -> Self {
    Self {
      defs: Vec::new(),
      hashes: Vec::new(),
      heads: Vec::new(),
      chain: Vec::new(),
      indexed: false,
      name_units: 0,
      #[cfg(test)]
      compares: 0,
      #[cfg(test)]
      walked: 0,
      #[cfg(test)]
      hashed: 0,
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
  /// The count charge is exact rather than an upper bound, because chaining has no data-dependent
  /// insertion loop: indexing what the walk selected costs one push each whatever the names are.
  ///
  /// # And a second charge, because one push each is not one *pass* each
  ///
  /// A push is a constant, and the hash [`Table::fill`] takes to decide the bucket is not: it
  /// reads the whole fragment name, and draft §2.1.9 puts no ceiling on one. A count charge in
  /// front of a byte pass is the wrong-dimension defect this branch exists to close, and here it
  /// was reachable from a *valid* document — one long fragment name and the spread that selects
  /// it exhausts the definition and fragment counts exactly, and `fill` then hashes the whole
  /// spelling before the metered lookup that follows can refuse anything.
  ///
  /// So the pass is priced with the names' own lengths, before `fill` may run. Reading a name's
  /// length is not reading the name — it is a slice header — so the accumulation below is not
  /// itself the work being charged for, exactly as the merge engine's argument scan is charged
  /// off lengths it reads before it compares anything. Saturating, which the poison in
  /// [`Visits::take`] then turns into a refusal rather than into an accepted `u32::MAX`.
  /// al8n/smear#196.
  pub(super) fn charge(
    &mut self,
    definitions: &'a [DescribedExecutableDefinition<S>],
    visits: &mut Visits,
    location: SimpleSpan,
  ) -> Result<Paid<'a, S>, Fault<'static>>
  where
    S: AsRef<[u8]>,
  {
    let mut fragments = Vec::new();
    let mut name_units = 0u32;
    for described in definitions {
      #[cfg(test)]
      {
        self.walked += 1;
      }
      if let ExecutableDefinition::Fragment(fragment) = described.node() {
        name_units = name_units.saturating_add(byte_units(fragment.name().source().as_ref().len()));
        fragments.push(fragment);
      }
    }
    visits.spend(u32::try_from(fragments.len()).unwrap_or(u32::MAX), location)?;
    visits.spend(name_units, location)?;
    Ok(Paid {
      fragments,
      name_units,
    })
  }

  /// Whether the pass has run. See the field.
  #[inline]
  pub(super) const fn is_indexed(&self) -> bool {
    self.indexed
  }

  /// What hashing the indexed names cost the operation that built this table. See the field.
  #[inline]
  pub(super) const fn name_units(&self) -> u32 {
    self.name_units
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

  /// Name bytes this executor's index pass has hashed. See the field.
  #[cfg(test)]
  #[inline]
  pub(super) const fn hashed(&self) -> u64 {
    self.hashed
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
    let Paid {
      fragments,
      name_units,
    } = paid;
    self.indexed = true;
    // Kept so a warm executor charges what a cold one charges. The number was already accepted
    // by the receipt above; storing it means the *verdict* does not depend on whether this table
    // happens to be built, which is the same thing `Names::reset` had to fix one crate over.
    self.name_units = name_units;
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
      // later lookup or growth has to make it again. It used to be charged in *definitions*
      // rather than in bytes, on the argument that reading every name exactly once for the
      // executor's whole life is a single pass over text the parser has already read, with no
      // factor a client can apply to it. "No factor" is true and is not the question: one pass
      // over a name draft §2.1.9 puts no ceiling on is still a pass whose length the client
      // chose, and the counts in front of it can be exhausted exactly by a valid document that
      // then gets the whole spelling hashed for free. `Table::charge` prices it now, in
      // `byte_units`, before this may run. al8n/smear#196.
      let name = self.defs[ordinal].name().source().as_ref();
      #[cfg(test)]
      {
        self.hashed += name.len() as u64;
      }
      let hash = hash_bytes(name);
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
        // Charged for the bytes the `memcmp` reads, not for the length it is over. Reaching
        // here needs the whole stored hash **and** the length to agree, so the bytes are about
        // to be equal and this ordinarily pays `byte_units(len)` exactly as the whole-length
        // charge did; what it no longer does is bill a genuine hash collision for a pass that
        // stops at its first differing byte. al8n/smear#198's twenty-third round.
        match metered_eq(spelling, name, visits) {
          Compared::Equal => return Ok(Some((ordinal, fragment))),
          Compared::Differs => (),
          Compared::Refused => return Err(visits.budget_fault(location)),
        }
      }
      ordinal = self.chain[ordinal as usize];
    }
    Ok(None)
  }
}
