//! Interned names: the symbol type and the build-once name index.
//!
//! GraphQL names are ASCII by grammar (`/[_A-Za-z][_0-9A-Za-z]*/`), so the interner is byte-keyed
//! and serves `&str` and `&[u8]` documents alike — one arena, one index, whatever the document's
//! source slice type was.

use std::{boxed::Box, vec};

/// An interned name.
///
/// A `Sym` is an index into the owning [`Schema`]'s name tables and is **meaningless without
/// it** — two schemas assign symbols independently, and a symbol from one is not a symbol in the
/// other. Every API that returns a `Sym` documents the schema it belongs to.
///
/// [`Schema`]: super::Schema
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sym(u32);

impl Sym {
  /// Creates a symbol from its raw index.
  ///
  /// Callers are responsible for the index belonging to the schema it will be used against; see
  /// the type-level note.
  #[inline]
  pub const fn new(index: u32) -> Self {
    Self(index)
  }

  /// Returns the raw index.
  #[inline]
  pub const fn get(&self) -> u32 {
    self.0
  }
}

/// A half-open `[start, end)` range into one of the schema's flat tables.
///
/// The tables are grouped rather than nested — every field group, argument group, enum-value group
/// and interface group is a contiguous run of one `Box<[T]>` — so a "child list" is two `u32`s and
/// no pointer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Range32 {
  start: u32,
  end: u32,
}

impl Range32 {
  /// The empty range at offset zero.
  pub const EMPTY: Self = Self { start: 0, end: 0 };

  /// Creates a range from its endpoints.
  #[inline]
  pub const fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }

  /// Returns the inclusive start offset.
  #[inline]
  pub const fn start(&self) -> u32 {
    self.start
  }

  /// Returns the exclusive end offset.
  #[inline]
  pub const fn end(&self) -> u32 {
    self.end
  }

  /// Returns how many rows the range covers.
  #[inline]
  pub const fn len(&self) -> u32 {
    self.end.saturating_sub(self.start)
  }

  /// Returns whether the range covers no rows.
  #[inline]
  pub const fn is_empty(&self) -> bool {
    self.end <= self.start
  }

  /// Borrows the range's rows out of a table.
  #[inline]
  pub fn slice<'a, T>(&self, table: &'a [T]) -> &'a [T] {
    let start = (self.start as usize).min(table.len());
    let end = (self.end as usize).clamp(start, table.len());
    &table[start..end]
  }
}

/// The largest symbol count [`NameIndex::build`] can index.
///
/// The index is a power-of-two open-addressing table at load factor 1/2, so it needs
/// `2 * next_power_of_two(count)` slots; this cap is what keeps that product inside `u32`.
pub const MAX_SYMBOLS: u32 = 1 << 30;

/// FxHash-style multiply-fold over short ASCII keys, finished with an avalanche step.
///
/// Names are identifiers — a handful of bytes each — so an 8-byte-at-a-time fold with one
/// multiply is the whole of the compression, and it costs no dependency.
///
/// # The fold alone is not enough mixing, and *honest* names are what proved it
///
/// Bit `j` of `v · K` depends only on bits `0..=j` of `v`, so a multiply-fold pushes entropy
/// **upward** and the bytes late in a key decide nothing about where it lands. A name is short
/// enough to get one or two of those multiplies with a five-bit rotate between them, which is
/// nowhere near enough to bring a late byte back down into the half a mask reads: 4,096 eight-byte
/// names differing in four bytes at offset `at` occupied 2741, 2716, 1595, 488 and **32** of 4,096
/// buckets for `at = 0, 1, 2, 3, 4`. No choice of mask recovers what the product never separated.
///
/// That is not an adversary, it is how generated documents are named. Over 4,096 **distinct**
/// response keys — no collision search anywhere — the executor's table charged 1.89 units per key
/// for `k0…k4095`, 15.83 for `user0000Name…`, 41.45 for `field0…` and 64.60 for `h00000000…`,
/// against the one comparison per lookup an ideal hash gives. Splitmix64's finalizer — about five
/// instructions — brought all four to 0.49–0.53 at 2,523–2,608 buckets of 4,096, against the
/// `4096 · (1 − 1/e) ≈ 2,589` an ideal hash occupies. al8n/smear#172. It was not the whole of the
/// repair, and the section after next is why.
///
/// **The clustering is a property of a size range, not an asymptote**, which is worth saying
/// because it is the shape an extrapolation gets wrong. Work grows quadratically while the bytes
/// that distinguish a document's names sit above the fold's reach — `h{i:0>8}` measured ×3.95 then
/// ×3.98 per doubling and fitted `n²/64` to within 1% at 4,096 and 8,192 keys — and then flattens,
/// because a zero-padded decimal suffix widens leftward as the count grows and drags its own
/// entropy into byte positions the multiply does mix. By 65,536 keys that scheme measures 3,113,305
/// units against the 67,108,864 the same law predicts. The cost is real at the sizes documents are
/// written at; a refusal boundary derived by extending it is not.
///
/// # The finalizer cannot repair a state that has already collapsed, and multi-chunk names did
///
/// An avalanche step at the end is a bijection, so it moves where a key lands and nothing else. If
/// two keys have already met inside the fold it maps them onto the same place, and this fold had a
/// channel that put them there for ordinary spellings. Each round combines the running state with
/// the next word as `h.rotate_left(5) ^ v`. The multiply leaves a chunk's *late* bytes in `h`'s top
/// bits — that is the same upward push, seen from the other end — and a five-bit rotate delivers
/// exactly those bits into the low byte the **next** input word occupies. A difference in byte 7
/// and a difference in byte 8 therefore cancel before the multiply that follows, and
/// `x00000009` and `x00000084` produced the same complete 64-bit hash.
///
/// That is a naming convention, not an attack. 4,096 aliases spelled `x` plus an eight-digit
/// base-36 counter produced **1,660** hashes and charged 11,943 units end to end against a ceiling
/// of 8,192; the same family over all sixty-three valid continuation characters produced **895**
/// and charged 18,401. Two of the five spellings al8n/smear#172 pinned were already emitting
/// duplicate hashes — 4,004 and 3,996 for 4,096 names — and passed because the duplicates were few.
///
/// `h ^= h >> 32` between rounds is the repair: it folds the half the multiply just filled onto the
/// half it did not, so a difference the multiply left at the top now also sits thirty-two bits away
/// and one input word cannot erase both copies without differing at two separated byte offsets with
/// exactly matching values — which is a search, not a spelling. **It costs nothing for the names
/// that dominate**, because for eight bytes or fewer that loop body never runs. Crossing radix
/// against counter width — the two things that decide where a spelling's varying bytes sit relative
/// to a chunk boundary — twenty-four families of 4,096 names now produce 4,096 hashes each and
/// occupy 2,549 to 2,618 buckets of 4,096, against the 2,589 an ideal hash occupies. Thirty-three
/// families measure 7,081 to 7,265 units end to end through the executor: 1.73 to 1.77 per key,
/// every one of them within 3% of the others. al8n/smear#196.
///
/// # It is still unkeyed, and every caller has to say why that is safe for its keys
///
/// **`hash_bytes` is not injective and no version of it can be**, which is worth stating because
/// the sentence this replaced said the opposite: it called the fold invertible, the finalizer a
/// bijection, and "their composition one as well". Each *round* is invertible in the word it folds
/// — `K` is odd, so `v ≡ target · K⁻¹ (mod 2ᵐ)` solves it — and `finalize` is a bijection on `u64`,
/// but the function over variable-length input is a compression and the two names above are a
/// two-line proof of it.
///
/// What per-round invertibility buys an adversary is that colliding keys stay *constructible*
/// rather than merely unlucky: a search against the **finished** hash put 512 names in one bucket
/// of 1,024 after 458,312 candidates. What the mixing above removes is the honest cost. The
/// adversarial one it does not touch, and nothing here should be read as bounding it.
///
/// [`NameIndex`] holds the **schema's** names, which the operator wrote, so nothing an adversary
/// sends can lengthen a probe run. The execution module's tables hold the **document's**, which an
/// adversary does choose — so they charge every entry a probe compares against a work budget, and a
/// constructed pile-up spends that budget instead of the server's time. That charge is the bound,
/// and it was the bound before this finalizer existed.
#[inline]
pub fn hash_bytes(bytes: &[u8]) -> u64 {
  const K: u64 = 0x517c_c1b7_2722_0a95;
  let mut h: u64 = 0;
  let (chunks, rest) = bytes.as_chunks::<8>();
  for c in chunks {
    let v = u64::from_le_bytes(*c);
    h = (h.rotate_left(5) ^ v).wrapping_mul(K);
    h ^= h >> 32;
  }
  let mut tail = [0u8; 8];
  tail[..rest.len()].copy_from_slice(rest);
  let v = u64::from_le_bytes(tail) ^ ((rest.len() as u64) << 56);
  finalize((h.rotate_left(5) ^ v).wrapping_mul(K))
}

/// splitmix64's finalizer: the avalanche that lets every input bit reach every output bit.
///
/// A bijection on `u64`, so it maps no key onto another key — it only moves where a key lands. It
/// is **not** a bijection on the bucket index, which is exactly why it repartitions where an
/// xor-seed on the finished hash cannot: masking a power-of-two index after xor-with-a-constant is
/// a relabelling, and measures the same occupancy for every constant. A seed meant to move anything
/// has to enter the fold.
///
/// Being a bijection is also the limit of what it can do. Two keys the fold has already mapped
/// together arrive here as one value and leave as one value, so this step cannot separate them and
/// no amount of avalanche at the end substitutes for mixing between the rounds — which is the whole
/// of al8n/smear#196 and the reason [`hash_bytes`]'s loop carries a fold of its own.
#[inline]
const fn finalize(mut hash: u64) -> u64 {
  hash ^= hash >> 30;
  hash = hash.wrapping_mul(0xbf58_476d_1ce4_e5b9);
  hash ^= hash >> 27;
  hash = hash.wrapping_mul(0x94d0_49bb_1331_11eb);
  hash ^ (hash >> 31)
}

/// The bucket `hash` lands in, for a power-of-two table of `mask + 1` slots.
///
/// # The high half, and the measurement that used to say why
///
/// A multiply-fold pushes entropy **upward**: bit `i` of `v · K` depends only on bits `0..=i` of
/// `v`, so the low bits are the least mixed word in the product and masking them keeps whichever
/// bits of the key happened to be low. That is not a tail risk, it is what ordinary names do. A
/// document naming `F0 … F4095` puts its 5-byte names' first digit in bits 8–12 and everything
/// that distinguishes them above bit 13, so a 13-bit mask of the low half saw **ten** buckets for
/// three thousand names: resolving a 4,096-link fragment chain compared 1.9 million entries, against
/// the 8.4 million of the linear scan the index replaced. Shifting first costs one instruction and
/// brings the same chain to 4,472 — about one comparison per lookup.
///
/// **That is history now, and saying so is the point.** [`hash_bytes`] ends in an avalanche step
/// (al8n/smear#172) and folds its high half down between rounds (al8n/smear#196), which together
/// spread a key across all sixty-four bits, so *which* half a mask reads stopped deciding anything:
/// over seven naming schemes the finished hash occupies 2,574–2,630 buckets of 4,096 masking the
/// high half and 2,560–2,622 masking the low one, against the `4096 · (1 − 1/e) ≈ 2,589` an ideal
/// hash occupies, and over the twenty-four-row radix-against-width axis in `name/tests.rs` the high
/// half occupies 2,549–2,618. Masking the low half of the **unfinished** hash is what those numbers
/// used to look like, and still does: 464.27 comparisons per key over `k0…k4095` at ten buckets of
/// 4,096.
///
/// The shift stays because it costs one instruction and it is the half whose entropy does not
/// depend on the finalizer being present — belt as well as braces, and the thing to fix if a caller
/// ever needs the fold without it.
///
/// It does not make [`hash_bytes`] keyed, and nothing here claims it does — a caller whose keys an
/// adversary chooses still owes the argument that function's documentation asks for.
#[inline]
pub fn bucket(hash: u64, mask: u32) -> u32 {
  ((hash >> 32) as u32) & mask
}

/// Open-addressing name index: name bytes to [`Sym`].
///
/// Power-of-two capacity, linear probing, `u32::MAX` for an empty slot. Built once from the
/// finished arena and probed read-only thereafter — which is what lets the whole schema be shared
/// as `&Schema` with no interior mutability anywhere.
///
/// The index stores symbol numbers, not keys: a probe resolves each candidate back through the
/// arena, so there is exactly one copy of every name in the whole structure.
#[derive(Debug, Clone)]
pub struct NameIndex {
  mask: u32,
  slots: Box<[u32]>,
}

impl NameIndex {
  /// Builds an index over symbols `0..count`, resolving each to its bytes through `resolve`.
  ///
  /// Returns `None` when `count` exceeds [`MAX_SYMBOLS`].
  pub fn build<'a>(count: u32, resolve: impl Fn(u32) -> &'a [u8]) -> Option<Self> {
    if count > MAX_SYMBOLS {
      return None;
    }
    let cap = count.checked_next_power_of_two()?.checked_mul(2)?.max(8);
    let mask = cap - 1;
    let mut slots = vec![u32::MAX; cap as usize].into_boxed_slice();
    for sym in 0..count {
      let mut i = bucket(hash_bytes(resolve(sym)), mask);
      while slots[i as usize] != u32::MAX {
        i = (i + 1) & mask;
      }
      slots[i as usize] = sym;
    }
    Some(Self { mask, slots })
  }

  /// Looks a name up by its bytes.
  #[inline]
  pub fn get<'a>(&self, bytes: &[u8], resolve: impl Fn(u32) -> &'a [u8]) -> Option<Sym> {
    let mut i = bucket(hash_bytes(bytes), self.mask);
    loop {
      let s = self.slots[i as usize];
      if s == u32::MAX {
        return None;
      }
      if resolve(s) == bytes {
        return Some(Sym(s));
      }
      i = (i + 1) & self.mask;
    }
  }

  /// Returns how many probe slots the index holds.
  ///
  /// Exposed so a test can pin the load factor rather than infer it.
  #[inline]
  pub fn capacity(&self) -> usize {
    self.slots.len()
  }
}

/// Returns whether `bytes` spells a GraphQL `Name`.
///
/// This is the arena's admission rule. It is what makes every interned name ASCII, and therefore
/// what makes [`Schema::name`] infallible.
///
/// [`Schema::name`]: super::Schema::name
#[inline]
pub const fn is_name(bytes: &[u8]) -> bool {
  if bytes.is_empty() {
    return false;
  }
  let first = bytes[0];
  if !(first == b'_' || first.is_ascii_alphabetic()) {
    return false;
  }
  let mut i = 1;
  while i < bytes.len() {
    let b = bytes[i];
    if !(b == b'_' || b.is_ascii_alphanumeric()) {
      return false;
    }
    i += 1;
  }
  true
}

/// Returns whether `bytes` is a reserved (introspection) name — one starting with `__`.
#[inline]
pub const fn is_reserved(bytes: &[u8]) -> bool {
  bytes.len() >= 2 && bytes[0] == b'_' && bytes[1] == b'_'
}

#[cfg(test)]
mod tests;
