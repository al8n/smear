//! Draft §7.1.7's `extensions` map, as a type that cannot be anything else and cannot grow without
//! being charged for it.
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
//! returning an error. `proto` could not even detect it: [`Values`](crate::Values) asks seven
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
//! entries, since a zero-length key is a legal key. Together they bound storage
//! (`Σ key_len ≤ max_extension_key_bytes`, plus `entries ≤ max_extension_entries` `Vec` slots) and
//! they bound the scan, whose worst case is `entries × Σ key_len` byte comparisons over the life of
//! the map.
//!
//! [`Extensions::insert`] is therefore fallible and **hands the caller's value back** on refusal,
//! and it takes `&str` rather than an `impl Into<Box<str>>` for a reason that is not stylistic: an
//! owning parameter would have allocated the key *before* the ceiling could refuse it, and a
//! ceiling that allocates first is not a ceiling. For the same reason a repeated key is resolved by
//! looking first and replacing in place, so a duplicate costs no allocation and charges nothing.
//!
//! # The two ceilings bound what the map reports; a third quantity is what it holds
//!
//! **The round after the one above found that the pair was still a bound on the wrong thing**, and
//! the diagnosis is one step past the one that produced them. Entries and key bytes are the two
//! factors of the map's *reported content*, and they do bound it. The resource
//! [`set_extensions`](crate::Executor::set_extensions) is actually protecting is *retained memory*,
//! whose factors are not those two: the spine's **capacity** is not derived from the entries at
//! all. It is a high-water mark, and [`remove`](Extensions::remove) — whose own doc comment used to
//! stop at "giving its key bytes back to the ceiling" — refunds the budget and returns no slot to
//! the allocator. So a map grown to a million entries under a lax [`Limits`] and emptied reads as
//! `len() == 0` and `key_bytes() == 0` through both re-checks while holding a million slots, and
//! the executor keeps that allocation until the next `start`, a `take_extensions`, or drop.
//!
//! `Extensions::shrink_to_ceiling` closes it by re-deriving the allocation from the entries on the
//! way in, and carries the argument for normalizing rather than refusing. The rule worth keeping is
//! the general one: **a ceiling bounds a retained buffer only if every growth of that buffer passed
//! through it.** This spine was the only buffer in the crate grown under a ceiling its retainer
//! does not own — which is exactly what "a map built under a laxer `Limits`" means, and the
//! re-check was written for the two quantities that were easy to see.
//!
//! # What it costs, stated rather than waved at
//!
//! One `Vec` and one boxed key per entry, paid once per response. An opaque `V::Value` allocated
//! nothing, and that was its only real advantage. The comparison that settles it: a response tree is
//! thousands of slots and the driver's own values are already owned by it, while an extensions map
//! is a handful of keys written once — so this is not on any path that scales with the response,
//! and it buys a state that cannot be reached rather than one that is merely discouraged.
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
//! `alloc` is unconditional in this crate — the crate root aliases it as `std` when the `std`
//! feature is off — and [`Executor`](crate::Executor) already allocates for every table it keeps.
//! So this compiles and behaves the same under `--no-default-features` as it does with `std`, and
//! no feature gates it.

use core::fmt;

use crate::Limits;

/// Which ceiling refused an [`Extensions::insert`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Ceiling {
  /// [`Limits::max_extension_entries`] has no room for another entry.
  Entries,
  /// [`Limits::max_extension_key_bytes`] has no room for the key.
  KeyBytes,
}

impl Ceiling {
  /// Returns the [`Limits`] field that refused.
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

#[cfg(feature = "std")]
impl<V: fmt::Debug> std::error::Error for Full<V> {}

/// Draft §7.1.7's `extensions` map.
///
/// A map by construction, which is half the point of the type: §7.1.7 requires the entry's value to
/// be a map "if set", and there is no way to build one of these that is not. Absence is
/// [`Option::None`] around it — see
/// [`Executor::set_extensions`](crate::Executor::set_extensions) for the three states and which of
/// them §7 allows.
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
/// # It is a small map, and it is searched
///
/// Lookup and [`insert`](Extensions::insert) are linear, and that is affordable *because* of the
/// ceilings rather than in spite of them: the whole life of a map costs at most
/// `max_extension_entries × max_extension_key_bytes` byte comparisons. A hash map would cost a
/// hasher, a `no_std` story for it and a non-deterministic iteration order, to save nothing at this
/// size.
pub struct Extensions<V> {
  entries: std::vec::Vec<(std::boxed::Box<str>, V)>,
  key_bytes: u32,
  max_entries: u32,
  max_key_bytes: u32,
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
  #[inline]
  #[must_use]
  pub const fn new(limits: &Limits) -> Self {
    Self {
      entries: std::vec::Vec::new(),
      key_bytes: 0,
      max_entries: limits.max_extension_entries.get(),
      max_key_bytes: limits.max_extension_key_bytes.get(),
    }
  }

  /// Inserts an entry, returning the value the key already had.
  ///
  /// # Refusal
  ///
  /// `Err` when the entry would pass [`Limits::max_extension_entries`] or
  /// [`Limits::max_extension_key_bytes`], carrying the value back so a handle is not closed by a
  /// refusal. Nothing is allocated on that path: the key is borrowed and the ceiling is checked
  /// before it is boxed, which is the difference between a ceiling and a receipt.
  ///
  /// # A repeated key costs nothing
  ///
  /// The key is looked up first, and a hit replaces the value in place — no allocation, no charge
  /// against either ceiling, and the displaced value returned. Replacement rather than duplication
  /// because a map has one value per key, and a serialised duplicate would be a map only by
  /// accident.
  pub fn insert(&mut self, key: &str, value: V) -> Result<Option<V>, Full<V>> {
    if let Some((_, slot)) = self.entries.iter_mut().find(|(name, _)| &**name == key) {
      return Ok(Some(core::mem::replace(slot, value)));
    }
    if self.entries.len() as u64 >= u64::from(self.max_entries) {
      return Err(Full {
        ceiling: Ceiling::Entries,
        value,
      });
    }
    if u64::from(self.key_bytes) + key.len() as u64 > u64::from(self.max_key_bytes) {
      return Err(Full {
        ceiling: Ceiling::KeyBytes,
        value,
      });
    }
    self.key_bytes += key.len() as u32;
    self.entries.push((key.into(), value));
    Ok(None)
  }

  /// Removes an entry, returning its value and giving its key bytes back to the ceiling.
  ///
  /// **The budget it refunds and the memory it frees are not the same thing.** The key's bytes are
  /// freed with its `Box<str>`, and the entry's *slot* is not: a `Vec` never returns capacity to
  /// the allocator on its own, so a map emptied this way reports nothing and is still holding
  /// everything it grew to. That is only a footprint the driver chose while the map is the
  /// driver's; the moment it crosses into an executor it is a footprint that executor never
  /// authorised, which is why `Extensions::shrink_to_ceiling` exists and why it is called by
  /// [`Executor::set_extensions`](crate::Executor::set_extensions) on the way in.
  pub fn remove(&mut self, key: &str) -> Option<V> {
    let index = self.entries.iter().position(|(name, _)| &**name == key)?;
    let (name, value) = self.entries.remove(index);
    self.key_bytes -= name.len() as u32;
    Some(value)
  }

  /// Returns the value at `key`.
  pub fn get(&self, key: &str) -> Option<&V> {
    self
      .entries
      .iter()
      .find(|(name, _)| &**name == key)
      .map(|(_, value)| value)
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

  /// How many entry slots the spine is *holding*, which is not what [`len`](Extensions::len)
  /// reports.
  ///
  /// Crate-visible rather than public because it is not something a caller needs to act on: the
  /// invariant it measures is enforced on the way into an executor rather than reported back for
  /// the driver to fix. See [`shrink_to_ceiling`](Extensions::shrink_to_ceiling).
  #[inline]
  pub(crate) fn capacity(&self) -> usize {
    self.entries.capacity()
  }

  /// Re-derives the spine from the entries it actually holds, when it is holding more slots than
  /// `max_entries`.
  ///
  /// `max_entries` is the **executor's** [`max_extension_entries`](Limits::max_extension_entries)
  /// and never this map's. The map's own ceilings are the ones the caller picked, and the point of
  /// this call is that the caller's number stops mattering the instant the map is retained by
  /// somebody else's budget.
  ///
  /// # Why the entry ceilings did not already bound this
  ///
  /// `len` and `key_bytes` are the map's *reported content*, and the two ceilings bound them
  /// exactly. Capacity is not derived from either: it is a high-water mark of every entry the map
  /// has ever held, and [`remove`](Extensions::remove) refunds the budget without returning the
  /// slot. So a map grown to a million entries under a lax [`Limits`] and emptied back down reads
  /// as `len() == 0` and `key_bytes() == 0` through every check
  /// [`set_extensions`](crate::Executor::set_extensions) performs, while holding a million slots.
  ///
  /// The rule that generalises it: **a ceiling bounds a retained buffer only if every growth of
  /// that buffer passed through it.** Every other buffer the executor keeps satisfies that by
  /// construction — the interner's arena can only ever have been grown through
  /// `Interner::intern`, which is gated by the executor's own `max_interned_bytes`, so its
  /// never-shrunk capacity is bounded by the executor's ceiling however many operations it has
  /// served. This spine was the one buffer grown under a ceiling the executor does not own.
  ///
  /// # Normalized rather than refused
  ///
  /// A map that was grown and emptied is a perfectly valid `extensions` map — §7.1.7 has nothing
  /// to say about how one was built — and its capacity is invisible from the public API, so a
  /// refusal would fail a legal program on a property its author cannot see. Nor could they act on
  /// it if they could see it: there is no `shrink` and no `drain` out here, so the only recovery
  /// would be to [`remove`](Extensions::remove) every entry into a freshly constructed map and
  /// re-attach that. Re-deriving the allocation here keeps acceptance exactly as documented and
  /// makes the ceiling mean what it says.
  ///
  /// Nothing is cloned. [`std::vec::Vec::shrink_to_fit`] moves the entries into the smaller
  /// allocation, so no `V: Clone` bound appears anywhere and a driver's handle is neither
  /// duplicated nor dropped. The cost is one reallocation, on the one path where the spine is over
  /// the ceiling.
  pub(crate) fn shrink_to_ceiling(&mut self, max_entries: u32) {
    if self.capacity() as u64 > u64::from(max_entries) {
      self.entries.shrink_to_fit();
    }
  }

  /// Returns the entries in insertion order.
  pub fn iter(&self) -> impl ExactSizeIterator<Item = (&str, &V)> + '_ {
    self.entries.iter().map(|(key, value)| (&**key, value))
  }
}

impl<'e, V> IntoIterator for &'e Extensions<V> {
  type Item = (&'e str, &'e V);
  type IntoIter = core::iter::Map<
    core::slice::Iter<'e, (std::boxed::Box<str>, V)>,
    fn(&'e (std::boxed::Box<str>, V)) -> (&'e str, &'e V),
  >;

  fn into_iter(self) -> Self::IntoIter {
    fn pair<V>(entry: &(std::boxed::Box<str>, V)) -> (&str, &V) {
      (&entry.0, &entry.1)
    }
    self.entries.iter().map(pair as fn(_) -> _)
  }
}

impl<V: Clone> Clone for Extensions<V> {
  fn clone(&self) -> Self {
    Self {
      entries: self.entries.clone(),
      key_bytes: self.key_bytes,
      max_entries: self.max_entries,
      max_key_bytes: self.max_key_bytes,
    }
  }
}

impl<V: PartialEq> PartialEq for Extensions<V> {
  /// Entry-wise and *in order*, because the order is part of what a serialiser writes.
  ///
  /// The ceilings are not compared. They are the budget the map was built under, not part of the
  /// `extensions` entry any response carries, and two maps with the same entries serialise
  /// identically whatever they were allowed to grow to.
  fn eq(&self, other: &Self) -> bool {
    self.entries == other.entries
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
      .entries(self.entries.iter().map(|(key, _)| (key, "<value>")))
      .finish()
  }
}
