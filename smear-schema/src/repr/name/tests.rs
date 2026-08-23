//! The hash's own gate, in the crate that owns it.
//!
//! Everything al8n/smear#172 measured about [`hash_bytes`] was measured from `graphql-proto`'s
//! executor, which is three crates away and reads the function through two tables and a budget.
//! That is a fine end-to-end gate and a bad unit one: it can only fail when a *document* costs too
//! much, so a hash that maps two names onto one hash passes it silently as long as the collision is
//! cheap. This module fails on the hash.

use std::{string::String, vec, vec::Vec};

use super::{NameIndex, bucket, hash_bytes, is_name};

/// The alphabets a generated alias is actually spelled in.
const RADICES: [&[u8]; 4] = [
  b"0123456789",
  b"0123456789abcdef",
  b"0123456789abcdefghijklmnopqrstuvwxyz",
  b"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
];

/// Counter widths that put the varying bytes on both sides of both chunk boundaries.
///
/// A name is `x` plus the counter, so these are lengths 7, 8, 9, 16, 17 and 18 — the byte before a
/// boundary, the boundary itself, and the byte after it, twice over.
const WIDTHS: [usize; 6] = [6, 7, 8, 15, 16, 17];

/// `count` names spelled `x` followed by `index` in `radix`, zero-padded to `width`.
fn family(radix: &[u8], width: usize, count: usize) -> Vec<String> {
  (0..count)
    .map(|index| {
      let mut digits = vec![radix[0]; width];
      let mut rest = index;
      for slot in (0..width).rev() {
        digits[slot] = radix[rest % radix.len()];
        rest /= radix.len();
      }
      let mut name = String::from("x");
      name.push_str(core::str::from_utf8(&digits).expect("ascii"));
      name
    })
    .collect()
}

/// A name's late bytes decide where it lands, on both sides of every chunk boundary.
///
/// # The two names that made this module exist
///
/// `hash_bytes` folds eight bytes at a time and combines the running state with the next word as
/// `h.rotate_left(5) ^ v`. The multiply leaves a chunk's *late* bytes in the high bits of `h`, and
/// a five-bit rotate delivers exactly those bits to the low byte the next input word occupies — so
/// a difference in byte 7 and a difference in byte 8 cancel, and `x00000009` and `x00000084` hashed
/// **identically**, before any finalizer had a chance to be a bijection over an already-collapsed
/// state. 4,096 eight-digit base-36 aliases produced 1,660 hashes and 4,096 base-63 ones produced
/// 895. That is not an adversary; it is `{prefix}{counter:0>width}` (al8n/smear#196).
///
/// # The axis, and why it is not a list of spellings
///
/// The version of this that shipped with al8n/smear#172 pinned five *named* spellings end to end.
/// All five passed here — the two that lose hashes lose only about ninety of them — because the
/// property is positional and five points do not cover a plane. What decides it is how many values
/// a byte position takes (the radix) and where the chunk boundary falls among the varying bytes
/// (the width), so those are the two axes, crossed. The rows that fail without the between-rounds
/// fold are exactly widths 8 and 16 at radices 36 and 63: name lengths 9 and 17, one byte past a
/// boundary, which is the mechanism written as a coordinate.
///
/// **The plant.** Delete `h ^= h >> 32` from [`hash_bytes`]'s loop and the two names at the top of
/// this test hash alike again, so it fails on the first line; with that assertion removed it fails
/// on `radix 36, width 8` at 1,660 distinct hashes of 4,096 and on `radix 63, width 8` at 895.
/// Deleting the *finalizer* instead leaves this test green — a bijection maps no key onto another
/// — which is why the two that follow it are here.
#[test]
fn a_generated_alias_family_has_one_hash_per_name() {
  const COUNT: usize = 4096;

  assert_ne!(
    hash_bytes(b"x00000009"),
    hash_bytes(b"x00000084"),
    "the two names al8n/smear#196 opened with still collide"
  );

  for radix in RADICES {
    for width in WIDTHS {
      let names = family(radix, width, COUNT);
      assert!(
        names.iter().all(|name| is_name(name.as_bytes())),
        "the family must be spellable"
      );
      let mut hashes: Vec<u64> = names.iter().map(|n| hash_bytes(n.as_bytes())).collect();
      hashes.sort_unstable();
      hashes.dedup();
      assert_eq!(
        hashes.len(),
        COUNT,
        "radix {}, width {width} (names of {} bytes): {COUNT} distinct names produced {} distinct \
         hashes. Differences in separate fold inputs are cancelling before the finalizer sees them",
        radix.len(),
        width + 1,
        hashes.len()
      );
    }
  }
}

/// The same axis through [`bucket`], which is what a table actually reads.
///
/// One hash per name is necessary and not sufficient: 4,096 distinct hashes that share three
/// hundred buckets cost a table just as much as 1,660 that share four thousand. So this pins
/// occupancy, against the `n · (1 − 1/e) ≈ 2,589` an ideal hash puts in 4,096 buckets.
///
/// **The plants, and they land on different rows.** Delete the `finalize` call and this fails on
/// `radix 10, width 6` at **406** buckets of 4,096 — the short-name half, which the fold between
/// rounds cannot help with because for a name of eight bytes or fewer that loop body never runs.
/// Delete `h ^= h >> 32` instead and it fails on `radix 36, width 8` at **1,343** — the
/// multi-chunk half. Each defect leaves the other one's rows green, which is what the second axis
/// buys.
#[test]
fn a_generated_alias_family_spreads_across_the_buckets() {
  const COUNT: usize = 4096;
  const MASK: u32 = (COUNT - 1) as u32;
  /// Under every row this measures and far over what either plant reads.
  ///
  /// The twenty-four rows occupy 2,549 to 2,618 buckets against the 2,589 an ideal hash occupies —
  /// a spread of about one standard deviation either way, so a floor 7% under the lowest of them
  /// does not move when a constant does. Deleting the finalizer reads 406 and deleting the fold
  /// between rounds reads 1,343, both of them several hundred units clear on the other side.
  const FLOOR: usize = 2400;

  for radix in RADICES {
    for width in WIDTHS {
      let names = family(radix, width, COUNT);
      let mut seen = vec![false; COUNT];
      for name in &names {
        seen[bucket(hash_bytes(name.as_bytes()), MASK) as usize] = true;
      }
      let occupied = seen.iter().filter(|hit| **hit).count();
      assert!(
        occupied >= FLOOR,
        "radix {}, width {width}: {COUNT} names occupy {occupied} buckets of {COUNT}, under a \
         floor of {FLOOR} and against the 2,589 an ideal hash occupies",
        radix.len()
      );
    }
  }
}

/// [`NameIndex`] answers with the symbol it was built from, and it does so in a bounded walk.
///
/// The index had no unit test at all: every number about it came from `graphql-proto`'s executor,
/// which does not use it. The lookup here is the one this crate ships — open addressing at load
/// factor 1/2 over the whole family — and the probe total is what a clustered hash inflates. The
/// twenty-four rows walk 6,018 to 6,289 slots for 4,096 lookups, about 1.5 each, which is what
/// open addressing at this load factor costs when the hash is behaving.
///
/// **The plants.** Deleting `finalize` reads 23,336 on `radix 10, width 6`; deleting
/// `h ^= h >> 32` reads 15,132 on `radix 36, width 8`. Both against a ceiling of 8,192.
#[test]
fn the_name_index_finds_every_symbol_in_a_bounded_walk() {
  const COUNT: usize = 4096;
  /// Two probes per lookup. An ideal hash at load factor 1/2 averages about 1.5 for a hit; a
  /// family that shared a hundred buckets would need thousands.
  const CEILING: usize = 2 * COUNT;

  for radix in RADICES {
    for width in WIDTHS {
      let names = family(radix, width, COUNT);
      let resolve = |symbol: u32| names[symbol as usize].as_bytes();
      let index = NameIndex::build(COUNT as u32, resolve).expect("under MAX_SYMBOLS");
      assert_eq!(index.capacity(), 2 * COUNT, "load factor 1/2");

      let mut probes = 0usize;
      for (symbol, name) in names.iter().enumerate() {
        let found = index
          .get(name.as_bytes(), resolve)
          .expect("built from these");
        assert_eq!(
          found.get(),
          symbol as u32,
          "the index answered with another name"
        );
        // The walk `get` takes, counted over the same slots it reads. A child module can see the
        // fields, which is the reason this gate lives here and not three crates away.
        let mut slot = bucket(hash_bytes(name.as_bytes()), index.mask);
        loop {
          probes += 1;
          let candidate = index.slots[slot as usize];
          assert_ne!(candidate, u32::MAX, "the walk ran off the end of a chain");
          if resolve(candidate) == name.as_bytes() {
            break;
          }
          slot = (slot + 1) & index.mask;
        }
      }
      assert!(
        probes <= CEILING,
        "radix {}, width {width}: {COUNT} lookups walked {probes} slots, over a ceiling of \
         {CEILING}",
        radix.len()
      );
    }
  }

  // A name the arena never held is absent rather than a wrong answer or a walk off the end.
  let names = family(RADICES[0], 4, 64);
  let resolve = |symbol: u32| names[symbol as usize].as_bytes();
  let index = NameIndex::build(64, resolve).expect("under MAX_SYMBOLS");
  assert!(index.get(b"absent", resolve).is_none());
  assert!(index.get(b"", resolve).is_none());
}

/// Length is part of the key: a name is not its own prefix padded with zeros.
///
/// The tail word is `bytes ^ (len << 56)`, which is the only thing separating `"a"` from `"a\0"`
/// once both are widened to eight bytes — and the eight-byte case is the one where the tail word is
/// *empty*, so the length is all there is.
#[test]
fn the_tail_length_is_part_of_the_key() {
  for len in 0..24usize {
    let short = vec![b'a'; len];
    let long = vec![b'a'; len + 1];
    assert_ne!(
      hash_bytes(&short),
      hash_bytes(&long),
      "{len} and {} `a`s hash alike",
      len + 1
    );
  }
}
