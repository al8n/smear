//! Byte spellings interned into dense symbols, refused rather than wrapped at the arena's two
//! ceilings.

use std::{borrow::ToOwned, boxed::Box, collections::BTreeMap, vec::Vec};

/// A dense symbol: an index into one interner's arena.
pub trait Symbol: Copy {
  /// The symbol whose index into the arena is `index`.
  fn from_index(index: u32) -> Self;

  /// This symbol's index into the arena.
  fn index(self) -> u32;
}

/// The largest arena an [`Interner`] will fill, in bytes.
///
/// # Why `u32::MAX` — the interval, then the pick inside it
///
/// | bound | from | value |
/// |---|---|---|
/// | lower: the arena a **parsed** document needs | one copy per distinct spelling, and a parsed document's leaves hold disjoint token spans | **that document's own source** |
/// | upper: the last offset a span can name | [`Interner::spans`] is a `(u32, u32)` pair of byte offsets into [`Interner::strings`] | **`u32::MAX`** |
///
/// The lower row is why a ceiling at the top of the interval refuses nothing this workspace
/// parses. A `TypeSystemDocument` that `type_system_document` produced over one source has one
/// leaf per token and the tokens are disjoint, so the sum of its spellings is at most that source
/// — and a source this workspace accepts is itself addressed by `u32` spans. The two rows meet,
/// so there is no interval to pick inside.
///
/// **For a tree a caller assembles the row has no bottom, and that is what the ceiling is for.**
/// `Name::new` is public, and `IntValue::graphql` and its two siblings are public associated
/// parsers, so `N` leaves may be `N` overlapping suffixes of one `B`-byte buffer: `B(B+1)/2`
/// interned bytes from `B` of input, which reaches four gigabytes at `B ≈ 92 682` — a 92 KB
/// input. `RawShape` carries the measurement. So this is not slack a caller can plan around; it
/// is the width of the offset, and refusing at it is what the offset is worth.
///
/// The upper row is the representation and not a preference. `bytes()` answers
/// `strings[start..end]`, and past `u32::MAX` bytes there is no `start` to record — the cast that
/// used to record one wrapped, which is the defect this constant closes rather than a capacity it
/// buys. See the [`Interner`] header for the two failures wrapping produced.
pub const MAX_ARENA_BYTES: u32 = u32::MAX;

/// The largest number of distinct spellings an [`Interner`] will hold.
///
/// The width of the [`Symbol`] that addresses them, for the same reason [`MAX_ARENA_BYTES`] is the
/// width of an offset — and it is the *slack* bound of the pair. A spelling costs at least one
/// byte of arena, so `spans.len() <= strings.len() + 1` always holds and [`MAX_ARENA_BYTES`] is
/// reached first from every direction but one: the single empty spelling, which the map admits
/// once and then deduplicates. It is checked anyway, because "the other guard covers it" is a
/// property of today's call sites rather than of this type.
pub const MAX_ARENA_SYMBOLS: u32 = u32::MAX;

/// An arena of distinct byte spellings, each stored once and addressed by a dense [`Symbol`].
///
/// # The offsets are checked, and here is what they did when they were not
///
/// A span is a pair of `u32` **byte offsets** into `strings`, and the three that record one were
/// `… .len() as u32`. Two of them index `strings`, so they wrap at four gigabytes of interned
/// bytes; the third counts symbols and needs four billion of them, which is the bar a review of
/// this type read all three against. Measured on this type at `fcac941`,
/// `aarch64-apple-darwin`, release, by padding `strings` and interning `"beta"` after `"alpha"`:
///
/// | `strings.len()` | span recorded | `bytes()` |
/// |---|---|---|
/// | `u32::MAX` | `(4294967295, 3)` | **panic**: `slice index starts at 4294967295 but ends at 3` |
/// | `2^32` | `(0, 4)` | **`"alph"`** — the wrong spelling, handed back through `Schema::name` |
///
/// Neither is a capacity limit a caller can plan around: the first ends the process and the second
/// answers a question wrongly, both on a path that returns `Result`. Four gigabytes arrives from a
/// 92 KB input, through either arena: `Name::new` is public and `EnumValue::graphql` is a public
/// associated parser, so `N` leaves may be `N` overlapping suffixes of one `B`-byte buffer —
/// `B(B+1)/2` interned bytes from `B`, which crosses `u32::MAX` at `B ≈ 92 682`. `RawShape`
/// carries the measurement. The ceilings above turn both rows into a refusal.
///
/// The quadratic growth itself is not bounded here, and for the two populations that still reach
/// it that is not a duplicate of the ceiling: every retained byte is a *distinct* spelling — a name
/// the finished `Schema` hands back through `Schema::name`, or an enum member's spelling that
/// only the declared members can rule on — so the dedup below is the strongest bound of its kind
/// and there is no copy left to remove. The checked conversion is what makes the growth harmless.
/// The two numeric literal arms used to be a third population and are not one any more: they
/// retain nothing at all, which is a stronger answer than a ceiling and is not available to these
/// two.
pub struct Interner<S> {
  strings: Vec<u8>,
  spans: Vec<(u32, u32)>,
  map: BTreeMap<Box<[u8]>, u32>,
  /// The symbol a refused intern answers with, minted at the first refusal.
  ///
  /// `Some` is the arena saying it stopped growing. Holding the placeholder here rather than a
  /// bare flag is what makes one refusal cost one span however many spellings follow it.
  refused: Option<S>,
}

// By hand, because a derive would bound `S: Default`, and a symbol has no default to give.
impl<S> Default for Interner<S> {
  fn default() -> Self {
    Self {
      strings: Vec::new(),
      spans: Vec::new(),
      map: BTreeMap::new(),
      refused: None,
    }
  }
}

impl<S> core::fmt::Debug for Interner<S> {
  /// The arena's *shape*, never its contents.
  ///
  /// A derived `Debug` renders `strings` as one decimal integer per interned byte and `map` as a
  /// second copy of every spelling — so formatting a `SchemaBuilder` printed the caller's whole
  /// literal and name arenas twice, up to [`MAX_ARENA_BYTES`] each. That is the same instruction
  /// the builder's `RawShape` impl follows, in the other direction: a walk whose output is chosen
  /// by the document rather than by the type is not a `Debug`, it is a dump.
  ///
  /// What a reader loses is the spellings, which are the caller's own bytes and are reachable
  /// through [`Interner::bytes`]; what a reader gains is the three numbers a build is ever
  /// debugged against.
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Interner")
      .field("symbols", &self.spans.len())
      .field("bytes", &self.strings.len())
      .field("refused", &self.refused.is_some())
      .finish()
  }
}

/// The map of an [`Interner`] that [`Interner::into_parts`] took apart.
///
/// Nothing reads it again. It is handed back rather than dropped inside `into_parts`, so that
/// when its memory is released is the caller's decision.
pub struct Retired {
  _map: BTreeMap<Box<[u8]>, u32>,
}

impl<S: Symbol> Interner<S> {
  /// The symbol `bytes` was interned with, or a new one; a new spelling that would pass either
  /// arena ceiling is not stored and gets the placeholder [`Interner::refused`] reports.
  pub fn intern(&mut self, bytes: &[u8]) -> S {
    self.intern_within(bytes, MAX_ARENA_BYTES, MAX_ARENA_SYMBOLS)
  }

  /// [`Interner::intern`] with the two ceilings supplied.
  ///
  /// The ceilings are parameters rather than constants read inside so that a test can stand at the
  /// boundary: the real one is four gigabytes of interned bytes, which no suite allocates. This is
  /// the whole mechanism — [`Interner::intern`] adds only the two numbers — so a cell driven to an
  /// injected ceiling exercises the same guards a document past the real one would.
  pub fn intern_within(&mut self, bytes: &[u8], max_bytes: u32, max_symbols: u32) -> S {
    if let Some(sym) = self.map.get(bytes) {
      return S::from_index(*sym);
    }
    // Each of the three was a `len() as u32`, and each is asked before anything grows: a refusal
    // that has already pushed the bytes is a refusal with a wrapped arena behind it.
    let (Ok(start), Ok(width), Ok(sym)) = (
      u32::try_from(self.strings.len()),
      u32::try_from(bytes.len()),
      u32::try_from(self.spans.len()),
    ) else {
      return self.refuse();
    };
    let Some(end) = start.checked_add(width).filter(|end| *end <= max_bytes) else {
      return self.refuse();
    };
    if sym >= max_symbols {
      return self.refuse();
    }
    self.strings.extend_from_slice(bytes);
    self.spans.push((start, end));
    self.map.insert(bytes.to_owned().into_boxed_slice(), sym);
    S::from_index(sym)
  }

  /// The symbol a refused intern answers with: an empty spelling, minted once.
  ///
  /// A real in-range symbol rather than a sentinel, because a [`Symbol`] is a **dense index** in
  /// the builder — `Positions` addresses a table by it, `set_type_index` resizes one to it — so a
  /// symbol outside the arena is an out-of-bounds read or a four-billion-slot `resize`, which is a
  /// worse failure than the one being repaired. The empty range is in bounds for every arena,
  /// including one that holds nothing.
  fn refuse(&mut self) -> S {
    if let Some(sym) = self.refused {
      return sym;
    }
    let sym = match u32::try_from(self.spans.len()) {
      // One slot short of the width, so that pushing the placeholder cannot be the thing that
      // overflows `len`.
      Ok(sym) if sym < u32::MAX => {
        self.spans.push((0, 0));
        sym
      }
      // The symbol space is full as well. Symbol zero exists, because filling it took `2^32`
      // accepted interns.
      _ => 0,
    };
    let sym = S::from_index(sym);
    self.refused = Some(sym);
    sym
  }

  /// The symbol `bytes` was interned with, or `None` when this arena never interned it. It interns
  /// nothing.
  pub fn lookup(&self, bytes: &[u8]) -> Option<S> {
    self.map.get(bytes).copied().map(S::from_index)
  }

  /// The spelling `sym` was interned with.
  ///
  /// Indexed rather than probed: `intern_within` records a span only after establishing
  /// `start <= end <= strings.len()`, and `sym` is the index it returned, so both reads are in
  /// bounds by construction. A `get` here would answer a future break with the empty spelling
  /// instead of saying so.
  pub fn bytes(&self, sym: S) -> &[u8] {
    let (start, end) = self.spans[sym.index() as usize];
    &self.strings[start as usize..end as usize]
  }

  /// The spelling `sym` was interned with, as text, or `""` when that spelling is not UTF-8.
  pub fn text(&self, sym: S) -> &str {
    // ASCII by construction; every name is admitted through `is_name` first.
    core::str::from_utf8(self.bytes(sym)).unwrap_or("")
  }

  /// The number of symbols minted, the refusal placeholder included, saturating at `u32::MAX`.
  //
  // No `is_empty` beside it: callers read the symbol count to size tables by it and to refuse
  // past a ceiling, never to ask whether the arena is empty.
  #[allow(clippy::len_without_is_empty)]
  pub fn len(&self) -> u32 {
    // `intern_within` refuses past `MAX_ARENA_SYMBOLS`, so this is the count rather than a wrap of
    // it. Saturating rather than `as` is what makes that a claim a reader can check: a count past
    // the width answers `u32::MAX`, which `flatten` refuses as `SchemaErrorKind::TooManyNames`
    // instead of returning a schema addressed by the low bits of it.
    u32::try_from(self.spans.len()).unwrap_or(u32::MAX)
  }

  /// The placeholder the first refusal minted, or `None` while the arena has refused nothing.
  pub fn refused(&self) -> Option<S> {
    self.refused
  }

  /// The arena's bytes, its spans, and the retired map, in that order.
  pub fn into_parts(self) -> (Vec<u8>, Vec<(u32, u32)>, Retired) {
    (self.strings, self.spans, Retired { _map: self.map })
  }

  /// The spans, one per symbol: the builder's tests read the arena's shape through this.
  pub fn spans(&self) -> &[(u32, u32)] {
    &self.spans
  }

  /// The interned bytes: the builder's tests read the arena's shape through this.
  pub fn strings(&self) -> &[u8] {
    &self.strings
  }
}

#[cfg(test)]
mod tests;
