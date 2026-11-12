//! What the arena does at its ceilings.
//!
//! # Why the ceilings are injected rather than reached
//!
//! [`MAX_ARENA_BYTES`] is four gigabytes of interned bytes. No suite allocates that, so every cell
//! below drives [`Interner::intern_within`] — the whole mechanism, with the two ceilings as
//! parameters — at a ceiling it can stand on. [`Interner::intern`] adds only the two constants, and
//! `the_shipped_ceilings_are_the_widths_they_address` is what keeps those two numbers from drifting
//! away from the widths they are derived from.
//!
//! The unchecked arithmetic these replaced was measured at the real boundary, at `fcac941`, by
//! padding `strings` directly; the two rows are in the [`Interner`] header. They are not cells here
//! because a 4 GiB resize is not something a test run should do.

use super::*;

/// A symbol of this module's own, so that no cell names a symbol type from outside it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Id(u32);

impl Symbol for Id {
  fn from_index(index: u32) -> Self {
    Self(index)
  }

  fn index(self) -> u32 {
    self.0
  }
}

/// The arena every cell drives, over [`Id`].
type Interner = super::Interner<Id>;

/// The two shipped ceilings are the widths their derivations name.
///
/// A constant that drifts off the width it is derived from is the failure the derivation cannot
/// see: the guard still refuses, at a number that no longer means anything.
#[test]
fn the_shipped_ceilings_are_the_widths_they_address() {
  assert_eq!(MAX_ARENA_BYTES, u32::MAX, "a span's offsets are `u32`");
  assert_eq!(MAX_ARENA_SYMBOLS, u32::MAX, "a symbol is a `u32`");
}

/// Exactly at the byte ceiling a spelling is interned; one byte past it the arena refuses.
///
/// Both halves, because a ceiling that only ever refuses is indistinguishable from a prohibition —
/// and because the accepting half is what says the guard is `<=` rather than `<`.
#[test]
fn the_byte_ceiling_is_a_price_at_it_and_a_refusal_one_past_it() {
  let mut interner = Interner::default();
  let alpha = interner.intern_within(b"alpha", 8, MAX_ARENA_SYMBOLS);
  let beta = interner.intern_within(b"bet", 8, MAX_ARENA_SYMBOLS);

  assert_eq!(interner.bytes(alpha), b"alpha");
  assert_eq!(interner.bytes(beta), b"bet");
  assert_eq!(
    interner.spans[beta.index() as usize],
    (5, 8),
    "a spelling ending exactly at the ceiling is interned"
  );
  assert!(interner.refused.is_none());

  let past = interner.intern_within(b"c", 8, MAX_ARENA_SYMBOLS);
  assert_eq!(
    interner.refused,
    Some(past),
    "one byte past the ceiling is a refusal, and the arena says so"
  );
}

/// A refused spelling answers an empty one, and nothing that was already interned moves.
///
/// This is the differential against `fcac941`, at an injected ceiling instead of at four
/// gigabytes: the wrapped cast recorded `(4294967295, 3)` and `(0, 4)` there, so `bytes()` either
/// panicked on the inverted range or handed back a *different* name's bytes. Here the span is
/// never recorded at all.
#[test]
fn a_refused_spelling_is_empty_rather_than_a_slice_of_another_one() {
  let mut interner = Interner::default();
  let alpha = interner.intern_within(b"alpha", 5, MAX_ARENA_SYMBOLS);
  let refused = interner.intern_within(b"beta", 5, MAX_ARENA_SYMBOLS);

  assert_eq!(
    interner.bytes(refused),
    b"",
    "the placeholder spells nothing; wrapping spelled `alph`"
  );
  assert_eq!(
    interner.strings.len(),
    5,
    "a refusal grows the byte arena by nothing"
  );
  assert_eq!(
    interner.bytes(alpha),
    b"alpha",
    "and leaves every span already recorded where it was"
  );
  assert_eq!(
    interner.lookup(b"beta"),
    None,
    "a refused spelling is not in the arena, so nothing can find it there"
  );
}

/// However many spellings an arena turns away, it mints one placeholder.
#[test]
fn every_refusal_after_the_first_answers_the_same_symbol() {
  let mut interner = Interner::default();
  interner.intern_within(b"alpha", 5, MAX_ARENA_SYMBOLS);
  let first = interner.intern_within(b"beta", 5, MAX_ARENA_SYMBOLS);
  let spans = interner.spans.len();

  for spelling in [&b"gamma"[..], b"delta", b"epsilon"] {
    assert_eq!(
      interner.intern_within(spelling, 5, MAX_ARENA_SYMBOLS),
      first
    );
  }
  assert_eq!(
    interner.spans.len(),
    spans,
    "a refused arena stops growing rather than growing by a span per refusal"
  );
}

/// The symbol count is the other cast, and it refuses on its own ceiling.
///
/// Unreachable through [`Interner::intern`] — a spelling costs a byte, so [`MAX_ARENA_BYTES`] is
/// hit first — and guarded anyway, which is what this cell is for: the guard is a property of the
/// type rather than of the call sites it has today.
#[test]
fn the_symbol_ceiling_refuses_rather_than_wrapping_the_index() {
  let mut interner = Interner::default();
  let alpha = interner.intern_within(b"alpha", MAX_ARENA_BYTES, 2);
  let beta = interner.intern_within(b"beta", MAX_ARENA_BYTES, 2);
  assert_eq!(interner.bytes(alpha), b"alpha");
  assert_eq!(interner.bytes(beta), b"beta");
  assert!(interner.refused.is_none(), "two symbols is the ceiling");

  let past = interner.intern_within(b"gamma", MAX_ARENA_BYTES, 2);
  assert_eq!(interner.refused, Some(past));
  assert_eq!(interner.bytes(past), b"");
  assert_eq!(
    interner.strings.len(),
    9,
    "the refusal is asked before anything grows, so the bytes are not spent either"
  );
}
