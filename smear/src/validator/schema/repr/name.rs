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

/// FxHash-style multiply-fold over short ASCII keys.
///
/// Names are identifiers — a handful of bytes each — so an 8-byte-at-a-time fold with one
/// multiply is all the mixing the index needs, and it costs no dependency.
#[inline]
fn hash_bytes(bytes: &[u8]) -> u64 {
  const K: u64 = 0x517c_c1b7_2722_0a95;
  let mut h: u64 = 0;
  let (chunks, rest) = bytes.as_chunks::<8>();
  for c in chunks {
    let v = u64::from_le_bytes(*c);
    h = (h.rotate_left(5) ^ v).wrapping_mul(K);
  }
  let mut tail = [0u8; 8];
  tail[..rest.len()].copy_from_slice(rest);
  let v = u64::from_le_bytes(tail) ^ ((rest.len() as u64) << 56);
  (h.rotate_left(5) ^ v).wrapping_mul(K)
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
      let mut i = (hash_bytes(resolve(sym)) as u32) & mask;
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
    let mut i = (hash_bytes(bytes) as u32) & self.mask;
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
