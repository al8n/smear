//! Draft §7.1.7's `extensions` map, and §7.1.6's per-error one, as a type that cannot be anything
//! else and cannot grow without being charged for it.
//!
//! # One container for two clauses, because the clauses are one shape
//!
//! §7.1.6's *Error Result Format* gives an error the same entry §7.1.7 gives a result, in the same
//! words: "GraphQL services may provide an additional entry to errors with key `extensions`. This
//! entry, if set, must have a map as its value. This entry is reserved for implementers to add
//! additional information to errors however they see fit, and there are no additional restrictions
//! on its contents." One structural demand at the top level and none below it, which is the seam
//! the section below draws for §7.1.7 — so the argument there is this one's too, and so is the
//! budget. [`Executor::handle_field_error_with`](crate::Executor::handle_field_error_with) is the
//! per-error map's retainer, and it re-checks and re-derives exactly as
//! [`set_extensions`](crate::Executor::set_extensions) does.
//!
//! # Why this is a container and not an opaque `V::Value`
//!
//! It was an opaque `V::Value` first, on the argument that §7.1.7 reserves the entry "for
//! implementers to extend the protocol however they see fit, and hence there are no additional
//! restrictions on its contents" — so the whole map, spine included, belongs to the driver and
//! `proto` should carry it the way it carries a leaf. **That argument reads one sentence too far,
//! and review caught it.** §7.1.7 opens with a restriction:
//!
//! > The `extensions` entry in an *execution result* or *request error result*, **if set, must have
//! > a map as its value**.
//!
//! So the section makes exactly one structural demand, and it lands on exactly one level. "No
//! additional restrictions on its contents" begins *below* the top level. A container that owns the
//! top level and stops there is therefore not an arbitrary seam — it is the seam the specification
//! draws, and the earlier design put the boundary one level too shallow.
//!
//! What that buys is the difference between a documented obligation and an unrepresentable state.
//! With an opaque value, safe driver code could hand over a null, a string or a list and reach a
//! [`Response`](crate::Response) that is not a conforming §7.1 response, with nothing anywhere
//! returning an error. `proto` could not even detect it: [`Values`](crate::Values) asks eight
//! questions and none is "is this a map", and by that trait's own admission rule each method
//! discharges a numbered draft §6 step, so it must not grow one that discharges none. A partial
//! check was considered and rejected for being worse than either alternative —
//! [`Values::is_null`](crate::Values::is_null) would catch `"extensions": null` and let every other
//! non-map through, which reads as enforcement without being it.
//!
//! [`Extensions`] removes the question. There is no way to build one that is not a map.
//!
//! # Why it is budgeted, which the first version of this container was not
//!
//! **Making the shape unrepresentable traded a shape hazard for a resource one**, and review caught
//! that too. The container was written as a new *type* rather than as a new *member of the systems
//! this crate already has*, and there are two of them: [`Limits`](crate::Limits)'s budget, and the
//! executor's operation lifecycle. It joined neither.
//!
//! The budget half, precisely. The first version's row in `execute.rs`'s lifetime table gave its
//! bound as "one, by the `Option`" — which bounds the number of *maps* at one and says nothing
//! about a map's size. That is the exact shape the spend table's fourth row exists to warn about:
//! **a budget on one factor of a product bounds nothing.** `insert` boxed a key and appended with
//! no ceiling on entries, no ceiling on key bytes, and a linear scan per call, so a driver
//! reflecting request-controlled data into extensions could spend unbounded storage and quadratic
//! comparisons with no budgeted refusal anywhere. Measured before the repair: 4,000 entries cost
//! 57 ms and 8,000 cost 183 ms — a ratio of 3.19 where linear is 2.0 — all of it inside this
//! module and none of it charged.
//!
//! **Two ceilings and not one, because the two are the factors.** Bounding entries alone leaves one
//! key free to be a gigabyte; bounding key bytes alone leaves an unbounded number of empty-keyed
//! entries, since a zero-length key is a legal key. Together they bound what the map reports
//! (`Σ key_len ≤ max_extension_key_bytes` and `entries ≤ max_extension_entries`), and with it the
//! work of finding a key: a lookup compares its key only against the entries whose hash it shares,
//! and even where a client can make every hash collide that is the linear scan's worst case of
//! `entries × Σ key_len` byte comparisons over the life of the map. [`Extensions`] has the hasher
//! and the argument.
//!
//! [`Extensions::insert`] is therefore fallible and **hands the caller's value back** on refusal,
//! and it takes `&str` rather than an owned key for a reason that is not stylistic: an owning
//! parameter would have allocated the key *before* the ceiling could refuse it, and a ceiling that
//! allocates first is not a ceiling. For the same reason a repeated key is resolved by looking
//! first and replacing in place, so a duplicate costs no allocation and charges nothing.
//!
//! # The two ceilings bound what the map reports; a third quantity is what it holds
//!
//! **The round after the one above found that the pair was still a bound on the wrong thing**, and
//! the diagnosis is one step past the one that produced them. Entries and key bytes are the two
//! factors of the map's *reported content*, and they do bound it. The resource
//! [`set_extensions`](crate::Executor::set_extensions) is actually protecting is *retained memory*,
//! whose factors are not those two: the map's allocations — its entry vector and its hash index —
//! are not derived from the entries at all. Each grows to fit the most entries the map has held and
//! neither shrinks on its own, and [`remove`](Extensions::remove) — whose own doc comment used to
//! stop at "giving its key bytes back to the ceiling" — refunds the budget and returns nothing to
//! the allocator. So a map grown to a million entries under a lax [`Limits`] and emptied reads as
//! `len() == 0` and `key_bytes() == 0` through both re-checks while holding room for a million,
//! and the executor keeps that allocation until the next `start`, a `take_extensions`, or drop.
//!
//! `Extensions::shrink_to_ceiling` closes it by re-deriving the allocations from the entries on the
//! way in, and carries the argument for normalizing rather than refusing. What it reads is a
//! high-water mark the map keeps itself, and `Extensions::capacity` has why no allocation's own
//! capacity is the measure. The rule worth keeping is the general one: **a ceiling bounds a
//! retained buffer only if every growth of that buffer passed through it.** This map was the only
//! buffer in the crate grown under a ceiling its retainer does not own — which is exactly what "a
//! map built under a laxer `Limits`" means, and the re-check was written for the two quantities
//! that were easy to see.
//!
//! # What it costs, stated rather than waved at
//!
//! One hashed map — an entry vector and a hash index — and one frozen key per entry, inline up to
//! 62 bytes where the target has pointer-width atomics, paid once per response. An opaque
//! `V::Value` allocated nothing, and that was its only real advantage. The comparison that settles
//! it: a response tree is thousands of slots and the driver's own values are already owned by it,
//! while an extensions map is a handful of keys written once — so this is not on any path that
//! scales with the response, and it buys a state that cannot be reached rather than one that is
//! merely discouraged.
//!
//! Keys are owned rather than borrowed because they are the *driver's* strings, not the document's.
//! An extension key is under no lexical restriction at all — it is not a GraphQL name — so the
//! executor's interner, which stores names as `u32` ids into a byte arena under
//! [`max_interned_bytes`](crate::Limits::max_interned_bytes), is the wrong home for them: that
//! arena's ceiling is a *response* budget shared with field names and driver messages, and spending
//! it on protocol metadata would make an extensions map able to refuse an error message.
//!
//! # No tier withholds it
//!
//! Both tiers have it, because both have a heap: `std` and `alloc` each turn on `indexmap`, and
//! `alloc` without `std` turns on `foldhash` as well, for the hasher [`Extensions`] describes.
//! With neither tier this crate exports nothing, this included — which is also what keeps
//! `indexmap`, a crate that links `alloc` whatever its features, out of a build with no allocator.
//! Between the two tiers only the hasher differs: what a map holds, refuses and iterates is the
//! same.

use core::{fmt, iter::FusedIterator};

use derive_more::IsVariant;
use indexmap::IndexMap;
use smear_schema::FrozenStr;

use crate::Limits;

#[cfg(test)]
mod tests;

/// The `BuildHasher` an [`Extensions`] map hashes its keys with: the standard library's, keyed
/// per map from its per-thread random keys.
#[cfg(feature = "std")]
type KeyHasher = std::hash::RandomState;

/// The `BuildHasher` an [`Extensions`] map hashes its keys with: foldhash's, seeded per map. A
/// build without the standard library has no randomness to draw a seed from, so foldhash derives
/// one from addresses; [`Extensions`] says what that seed is and what it is not.
#[cfg(not(feature = "std"))]
type KeyHasher = foldhash::quality::RandomState;

#[cfg(feature = "std")]
#[inline]
fn key_hasher() -> KeyHasher {
  std::hash::RandomState::new()
}

#[cfg(not(feature = "std"))]
#[inline]
fn key_hasher() -> KeyHasher {
  foldhash::quality::RandomState::default()
}

/// Which ceiling refused an [`Extensions::insert`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum Ceiling {
  /// [`Limits::max_extension_entries`] has no room for another entry.
  Entries,
  /// [`Limits::max_extension_key_bytes`] has no room for the key.
  KeyBytes,
}

impl Ceiling {
  /// Returns the [`Limits`] ceiling that refused, by the name of its getter.
  #[inline]
  pub const fn field(self) -> &'static str {
    match self {
      Self::Entries => "max_extension_entries",
      Self::KeyBytes => "max_extension_key_bytes",
    }
  }
}

impl fmt::Display for Ceiling {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`Limits::{}` has no room for the entry", self.field())
  }
}

/// An [`Extensions::insert`] a ceiling refused, carrying the value back.
///
/// The value is returned rather than dropped for the reason every other driver value in this crate
/// is handed back: it may be a wasm or FFI handle, and dropping one inside a refusal would close
/// whatever it names without the driver ever seeing it. The key is not returned because it was
/// never taken — [`Extensions::insert`] borrows it and refuses before allocating.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Full<V> {
  ceiling: Ceiling,
  value: V,
}

impl<V> Full<V> {
  #[inline]
  const fn new(c: Ceiling, v: V) -> Self {
    Self {
      ceiling: c,
      value: v,
    }
  }

  /// Returns which ceiling refused.
  #[inline]
  pub const fn ceiling(&self) -> Ceiling {
    self.ceiling
  }

  /// Returns the value the insert was refused, giving up ownership.
  #[inline]
  pub fn into_value(self) -> V {
    self.value
  }
}

impl<V> fmt::Display for Full<V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&self.ceiling, f)
  }
}

impl<V: fmt::Debug> core::error::Error for Full<V> {}

/// Draft §7.1.7's `extensions` map, and draft §7.1.6's per-error one.
///
/// A map by construction, which is half the point of the type: §7.1.7 requires the entry's value to
/// be a map "if set", §7.1.6 requires the same of an error's, and there is no way to build one of
/// these that is not. Absence is [`Option::None`] around it — see
/// [`Executor::set_extensions`](crate::Executor::set_extensions) for the three states and which of
/// them §7 allows, and [`Error::extensions`](crate::Error::extensions) for the same three on an
/// error.
///
/// The other half is the budget. A map is created against a [`Limits`] and carries the two ceilings
/// it was created under, so [`insert`](Extensions::insert) can refuse before it allocates; the
/// module header says why there are two of them and why an unbudgeted constructor would be a hole
/// rather than a convenience.
///
/// **The values are still the driver's and are never read.** §7.1.7 puts no restriction on the
/// contents below the top level, so an entry's value is one `V` — whatever
/// [`Values::Value`](crate::Values::Value) is for this driver — moved in and handed back unchanged.
/// Nothing here interprets one, and a nested map is the driver's own, exactly as a leaf's structure
/// is. Neither ceiling looks at a value either: `proto` bounds what `proto` allocates, and a
/// driver's value is the driver's, the same way a `String` leaf of any length reaches the response.
///
/// # Order is insertion order
///
/// §7 says nothing about the order of an `extensions` map's entries — §7.2.2 *Serialized Map
/// Ordering* is about a selection set's response keys, not about this. Insertion order is chosen
/// because it is deterministic and it is the one order the driver can predict; a serialiser that
/// writes [`iter`](Extensions::iter) in order produces the same bytes for the same calls.
///
/// # Lookup is hashed, and the order is still the calls'
///
/// [`insert`](Extensions::insert), [`get`](Extensions::get) and [`remove`](Extensions::remove) find
/// a key by its hash, under a seed the map draws for itself when it is created: the standard
/// library's `RandomState` under `std`, and foldhash's `RandomState` without it. The hasher decides
/// nothing a reader can see. Iteration is insertion order whatever the hasher and whatever the
/// seed, so a serialiser's bytes are still a function of the calls alone.
///
/// A key longer than all the stored keys together is not looked up at all: none of them can equal
/// it, so the answer is known without reading a byte of it. A lookup therefore hashes at most
/// [`key_bytes`](Extensions::key_bytes) bytes, which the key-byte ceiling bounds, whatever key it
/// is handed.
///
/// # The seed without the standard library, and what it is not
///
/// A build without the standard library has no source of randomness, so foldhash makes the seed
/// from what the process has: the address of a stack slot, mixed with a value foldhash changes
/// each time it makes a hasher, and a seed for the whole process made from the addresses of a
/// stack slot, a function and a static, or a constant where the target has no compare-and-swap.
/// The changing value keeps two maps from sharing a seed. Where address-space layout randomization
/// moves the addresses from one run to the next, a client cannot know a map's seed ahead of time
/// either. **It is not a cryptographic seed**, and foldhash claims only minimal resistance to
/// someone who can study the process.
///
/// On a target without address randomization, which is most targets with no operating system, the
/// addresses are the same on every run. There the seed can be worked out from the binary and the
/// order the process made its hashers in, and a client who has done that can choose keys whose
/// hashes collide. A deployment on such a target that hashes untrusted keys keeps
/// [`max_extension_entries`](Limits::max_extension_entries) small.
///
/// The ceilings bound what a collision costs either way. A lookup compares its key only with the
/// stored keys, so with every hash colliding it compares at most
/// [`key_bytes`](Extensions::key_bytes) bytes, and building a map up to its ceilings compares at
/// most `max_extension_entries × max_extension_key_bytes`: what the linear scan this map used
/// before it hashed cost. A [`remove`](Extensions::remove) finds each entry after the one it
/// removes again, to move it down a place, so with every hash colliding one removal probes on the
/// order of `max_extension_entries²` slots, comparing positions rather than key bytes.
pub struct Extensions<V> {
  entries: IndexMap<FrozenStr, V, KeyHasher>,
  key_bytes: u32,
  max_entries: u32,
  max_key_bytes: u32,
  /// The most entries this map has held since its allocations were last derived from its entries.
  /// See [`capacity`](Extensions::capacity) for what it bounds and why it is kept rather than read
  /// off the map.
  high_water: u32,
}

impl<V> Extensions<V> {
  /// Creates an empty map under `limits`.
  ///
  /// The only constructor, deliberately. There is no `Default` and no `FromIterator`: neither can
  /// name a budget, so either would be an unbudgeted way to build the same value and the ceilings
  /// would be advice rather than a bound. Build one with
  /// [`Executor::limits`](crate::Executor::limits) in hand — `Extensions::new(executor.limits())` —
  /// so the map's ceilings are the ones the executor will check it against.
  ///
  /// An empty map is a legal, *present* `extensions` entry — §7.1.7 restricts the contents not at
  /// all, and a map with no keys is a map — and it is a different response from one carrying no
  /// `extensions` key. Handing this to
  /// [`set_extensions`](crate::Executor::set_extensions) asks for `"extensions": {}`; not calling
  /// it at all asks for no entry.
  ///
  /// Allocates nothing. Not `const`, because the hasher draws the map's seed when it is created,
  /// with the standard library or without it.
  #[inline]
  #[must_use]
  pub fn new(limits: &Limits) -> Self {
    Self {
      entries: IndexMap::with_hasher(key_hasher()),
      key_bytes: 0,
      max_entries: limits.max_extension_entries.get(),
      max_key_bytes: limits.max_extension_key_bytes.get(),
      high_water: 0,
    }
  }

  /// Inserts an entry, returning the value the key already had.
  ///
  /// # Refusal
  ///
  /// `Err` when the entry would pass [`Limits::max_extension_entries`] or
  /// [`Limits::max_extension_key_bytes`], carrying the value back so a handle is not closed by a
  /// refusal. Nothing is allocated on that path: the key is borrowed and the ceiling is checked
  /// before it is copied, which is the difference between a ceiling and a receipt.
  ///
  /// # A repeated key costs nothing
  ///
  /// The key is looked up first, and a hit replaces the value in place — no allocation, no charge
  /// against either ceiling, the entry's position kept, and the displaced value returned.
  /// Replacement rather than duplication because a map has one value per key, and a serialised
  /// duplicate would be a map only by accident.
  ///
  /// A key longer than all the stored keys together cannot repeat one, so it is not looked up: it
  /// goes straight to the ceilings, in the same order, and one they refuse is never hashed.
  pub fn insert(&mut self, key: &str, value: V) -> Result<Option<V>, Full<V>> {
    if self.may_contain(key)
      && let Some(slot) = self.entries.get_mut(key)
    {
      return Ok(Some(core::mem::replace(slot, value)));
    }
    if self.entries.len() as u64 >= self.max_entries as u64 {
      return Err(Full::new(Ceiling::Entries, value));
    }
    if self.key_bytes as u64 + key.len() as u64 > self.max_key_bytes as u64 {
      return Err(Full::new(Ceiling::KeyBytes, value));
    }
    self.key_bytes += key.len() as u32;
    // A new key, by the lookup above or by its length, so this displaces nothing.
    self.entries.insert(FrozenStr::from(key), value);
    // At most `max_entries`, which is a `u32`, by the first ceiling above.
    self.high_water = self.high_water.max(self.entries.len() as u32);
    Ok(None)
  }

  /// Removes an entry, returning its value and giving its key bytes back to the ceiling.
  ///
  /// The entries after it keep their order, each moved down one place: that shift is linear in
  /// them, and it is what keeps iteration insertion order.
  ///
  /// **The budget it refunds and the memory it frees are not the same thing.** The key's bytes are
  /// freed with the key, and the entry's room is not: neither the entry vector nor the hash index
  /// returns capacity to the allocator on its own, so a map emptied this way reports nothing and is
  /// still holding everything it grew to. That is only a footprint the driver chose while the map
  /// is the driver's; the moment it crosses into an executor it is a footprint that executor never
  /// authorised, which is why `Extensions::shrink_to_ceiling` exists and why it is called by
  /// [`Executor::set_extensions`](crate::Executor::set_extensions) and
  /// [`Executor::handle_field_error_with`](crate::Executor::handle_field_error_with) on the way in.
  pub fn remove(&mut self, key: &str) -> Option<V> {
    if !self.may_contain(key) {
      return None;
    }
    let value = self.entries.shift_remove(key)?;
    // The key removed is equal to `key`, so it is `key.len()` bytes, all of them charged.
    self.key_bytes -= key.len() as u32;
    Some(value)
  }

  /// Returns the value at `key`.
  #[inline]
  pub fn get(&self, key: &str) -> Option<&V> {
    if !self.may_contain(key) {
      return None;
    }
    self.entries.get(key)
  }

  /// Whether `key` is short enough to be one of the stored keys.
  ///
  /// Each stored key is at most [`key_bytes`](Extensions::key_bytes) long, all of them together
  /// being that, so a longer `key` equals none of them and is answered without being hashed.
  #[inline]
  const fn may_contain(&self, key: &str) -> bool {
    key.len() as u64 <= self.key_bytes as u64
  }

  /// Returns how many entries the map holds.
  #[inline]
  pub fn len(&self) -> usize {
    self.entries.len()
  }

  /// Returns whether the map holds no entries.
  ///
  /// A `true` here is still a *present* `extensions` entry. Absence is the `None` outside this
  /// type, never an empty map inside it.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.entries.is_empty()
  }

  /// Returns how many bytes of key the map is holding, which is what
  /// [`Limits::max_extension_key_bytes`] bounds.
  #[inline]
  pub const fn key_bytes(&self) -> u32 {
    self.key_bytes
  }

  /// The most entries the map has held since its allocations were last derived from its entries,
  /// which is not what [`len`](Extensions::len) reports.
  ///
  /// A high-water mark the map keeps itself. Both of its allocations — the entry vector and the
  /// hash index — are within a small constant multiple of it, because each grows only when the
  /// entries outgrow it and neither shrinks on its own; so bounding the mark bounds what the map
  /// holds, where bounding [`len`](Extensions::len) does not.
  ///
  /// **Not the map's own capacity, and that is measured rather than argued.** `IndexMap::capacity`
  /// is the smaller of its two allocations, and the hash index's half falls with every removal
  /// that leaves a tombstone — so it reads low on exactly the grown and emptied map this exists to
  /// catch.
  ///
  /// A clone's mark is its length. [`Clone`] rebuilds the map from its live entries, so the clone
  /// holds room for those alone, whatever its source grew to.
  ///
  /// Crate-visible rather than public because it is not something a caller needs to act on: the
  /// invariant it measures is enforced on the way into an executor rather than reported back for
  /// the driver to fix. See [`shrink_to_ceiling`](Extensions::shrink_to_ceiling).
  #[inline]
  pub(crate) const fn capacity(&self) -> usize {
    self.high_water as usize
  }

  /// Re-derives the map's allocations from the entries it actually holds, when its high-water mark
  /// is over `max_entries`, and lowers the mark to what it holds.
  ///
  /// `max_entries` is the **executor's** [`max_extension_entries`](Limits::max_extension_entries)
  /// and never this map's. The map's own ceilings are the ones the caller picked, and the point of
  /// this call is that the caller's number stops mattering the instant the map is retained by
  /// somebody else's budget.
  ///
  /// # Why the entry ceilings did not already bound this
  ///
  /// `len` and `key_bytes` are the map's *reported content*, and the two ceilings bound them
  /// exactly. Its allocations are derived from neither: they follow the most entries the map has
  /// ever held, and [`remove`](Extensions::remove) refunds the budget without returning any of
  /// that room. So a map grown to a million entries under a lax [`Limits`] and emptied back down
  /// reads as `len() == 0` and `key_bytes() == 0` through every check
  /// [`set_extensions`](crate::Executor::set_extensions) performs, while holding room for a
  /// million.
  ///
  /// The rule that generalises it: **a ceiling bounds a retained buffer only if every growth of
  /// that buffer passed through it.** Every other buffer the executor keeps satisfies that by
  /// construction — the interner's arena can only ever have been grown through
  /// `Interner::intern`, which is gated by the executor's own `max_interned_bytes`, so its
  /// never-shrunk capacity is bounded by the executor's ceiling however many operations it has
  /// served. This map was the one buffer grown under a ceiling the executor does not own.
  ///
  /// # Normalized rather than refused
  ///
  /// A map that was grown and emptied is a perfectly valid `extensions` map — §7.1.7 has nothing
  /// to say about how one was built — and its capacity is invisible from the public API, so a
  /// refusal would fail a legal program on a property its author cannot see. Nor could they act on
  /// it if they could see it: there is no `shrink` and no `drain` out here, so the only recovery
  /// would be to [`remove`](Extensions::remove) every entry into a freshly constructed map and
  /// re-attach that. Re-deriving the allocations here keeps acceptance exactly as documented and
  /// makes the ceiling mean what it says.
  ///
  /// # Rebuilt at the cost of what it holds
  ///
  /// Nothing is cloned. The entries move, in order, out of the grown map through its owning
  /// iterator and into a fresh one sized to them, so no `V: Clone` bound appears anywhere and a
  /// driver's handle is neither duplicated nor dropped. The owning iterator reads the dense entry
  /// vector, and the old hash index is freed without being read, so the work follows the entries
  /// the map holds and not the room it grew to: one hash of each key, `key_bytes` bytes in all,
  /// which every caller has checked against its own ceiling first.
  ///
  /// Not `IndexMap::shrink_to_fit`, which keeps the hash index and rebuilds it smaller by scanning
  /// the old one for the entries still in it. That scan is as long as the most entries the map ever
  /// held, so a map kept down to one entry from a million would be read through room for a
  /// million on its way in.
  pub(crate) fn shrink_to_ceiling(&mut self, max_entries: u32) {
    if self.capacity() > max_entries as usize {
      let fresh = IndexMap::with_capacity_and_hasher(self.entries.len(), key_hasher());
      let grown = core::mem::replace(&mut self.entries, fresh);
      // The keys are distinct, so each is new here and the reserved room is never outgrown.
      self.entries.extend(grown);
      // At most `max_entries` of this map's own ceiling, which is a `u32`.
      self.high_water = self.entries.len() as u32;
    }
  }

  /// Returns the entries in insertion order.
  #[inline]
  pub fn iter(&self) -> ExtensionsIter<'_, V> {
    ExtensionsIter {
      inner: self.entries.iter(),
    }
  }
}

impl<'e, V> IntoIterator for &'e Extensions<V> {
  type Item = (&'e str, &'e V);
  type IntoIter = ExtensionsIter<'e, V>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl<V: Clone> Clone for Extensions<V> {
  /// The entries in insertion order, with the ceilings they were inserted under, in a map that
  /// holds room for those entries and no more.
  ///
  /// Rebuilt from the live entries rather than cloned whole. `IndexMap`'s own clone copies the hash
  /// index at its full size, which follows the most entries the source ever held, so the clone of a
  /// map grown and emptied would hold that index for nothing, once per copy and for as long as each
  /// copy lives. Rebuilt, a clone holds room for its own entries alone and its high-water mark is
  /// its length. Each key is hashed again on the way, which costs at most the bytes the key-byte
  /// ceiling allows.
  fn clone(&self) -> Self {
    let mut entries: IndexMap<FrozenStr, V, KeyHasher> =
      IndexMap::with_capacity_and_hasher(self.entries.len(), key_hasher());
    // The source's keys are distinct, so each is new here and the reserved room is never outgrown.
    entries.extend(
      self
        .entries
        .iter()
        .map(|(key, value)| (key.clone(), value.clone())),
    );
    Self {
      entries,
      key_bytes: self.key_bytes,
      max_entries: self.max_entries,
      max_key_bytes: self.max_key_bytes,
      // At most `max_entries`, which is a `u32`.
      high_water: self.entries.len() as u32,
    }
  }
}

impl<V: PartialEq> PartialEq for Extensions<V> {
  /// Entry-wise and *in order*, because the order is part of what a serialiser writes.
  ///
  /// Not `IndexMap`'s own `==`, which asks only that every key map to an equal value and so
  /// calls two maps equal that serialise differently.
  ///
  /// The ceilings are not compared. They are the budget the map was built under, not part of the
  /// `extensions` entry any response carries, and two maps with the same entries serialise
  /// identically whatever they were allowed to grow to.
  fn eq(&self, other: &Self) -> bool {
    self.len() == other.len() && self.iter().eq(other.iter())
  }
}

impl<V: Eq> Eq for Extensions<V> {}

impl<V> fmt::Debug for Extensions<V> {
  /// The keys, and the presence of a value rather than the value.
  ///
  /// `V` carries no [`Debug`](fmt::Debug) bound anywhere else a driver value is rendered — see
  /// [`Node`](crate::Node), whose leaf prints as `<leaf>` — and requiring one here would make the
  /// bound reach [`Response`](crate::Response) through this field.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_map()
      .entries(self.iter().map(|(key, _)| (key, "<value>")))
      .finish()
  }
}

/// The entries of an [`Extensions`] map, in insertion order: what [`Extensions::iter`] returns and
/// what `&Extensions` iterates as.
///
/// Each item is a key and its value. It runs from both ends, knows how many entries it has left,
/// and stays exhausted once it is.
pub struct ExtensionsIter<'e, V> {
  inner: indexmap::map::Iter<'e, FrozenStr, V>,
}

impl<'e, V> Iterator for ExtensionsIter<'e, V> {
  type Item = (&'e str, &'e V);

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self.inner.next().map(|(key, value)| (key.as_str(), value))
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.inner.size_hint()
  }
}

impl<V> DoubleEndedIterator for ExtensionsIter<'_, V> {
  #[inline]
  fn next_back(&mut self) -> Option<Self::Item> {
    self
      .inner
      .next_back()
      .map(|(key, value)| (key.as_str(), value))
  }
}

impl<V> ExactSizeIterator for ExtensionsIter<'_, V> {
  #[inline]
  fn len(&self) -> usize {
    self.inner.len()
  }
}

impl<V> FusedIterator for ExtensionsIter<'_, V> {}

impl<V> Clone for ExtensionsIter<'_, V> {
  /// Written out rather than derived, because a derive would bound `V: Clone` for an iterator that
  /// holds only references.
  #[inline]
  fn clone(&self) -> Self {
    Self {
      inner: self.inner.clone(),
    }
  }
}

impl<V> fmt::Debug for ExtensionsIter<'_, V> {
  /// The keys still to come, and not their values, for the reason [`Extensions`]' own `Debug`
  /// gives: no `V: Debug` bound.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list()
      .entries(self.clone().map(|(key, _)| key))
      .finish()
  }
}
