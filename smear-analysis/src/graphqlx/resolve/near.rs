//! Near misses: the "did you mean" of a name that does not resolve.
//!
//! The names visible where the name is written, of the namespace wanted, are compared with it one
//! by one, and the nearest within reach is suggested: the fewest insertions, deletions,
//! substitutions and swaps of two neighbouring bytes, at most a third of the name's length and at
//! least one; of two at one distance, the bytewise lesser.
//!
//! # Bounds
//!
//! - The whole analysis looks at most `Limits::max_near_miss_comparisons` names, every search
//!   together, and each module a glob names, whose names a search reads, costs one more, whatever
//!   it declares. A search the budget runs out in suggests nothing, so a suggestion never depends
//!   on where the budget ran out.
//! - A name longer than `MAX_LEN` bytes is not searched for at all.
//! - A name whose length differs from the misspelt one's by more than the distance allowed is
//!   looked at but not compared: no name compared is more than a third longer than the misspelt
//!   one.
//! - A comparison keeps three rows of the table, one cell longer than the name compared, in a
//!   buffer its search reuses, and fills them row by row: its work is at most the product of the
//!   two names' lengths, and it stops at the first row whose every distance is past the one
//!   allowed.

use core::cell::Cell;
use std::vec::Vec;

/// The longest name a near miss is searched for: no suggestion helps a name longer than this, and
/// the bound keeps one comparison small.
const MAX_LEN: usize = 64;

/// One search for a near miss of a name.
pub(super) struct Search<'c> {
  /// The name that does not resolve.
  wanted: &'c [u8],
  /// The greatest distance a suggestion may be at; 0 for a name not searched for.
  reach: usize,
  /// The nearest name so far, with its distance.
  best: Option<(usize, &'c [u8])>,
  /// Whether the budget ran out before the search ended.
  starved: bool,
  /// The rows every comparison of the search fills, allocated by the first.
  rows: Vec<usize>,
}

impl<'c> Search<'c> {
  /// A search for a near miss of `wanted`: none at all when it is empty or longer than `MAX_LEN`.
  pub(super) fn new(wanted: &'c [u8]) -> Self {
    let reach = if wanted.is_empty() || wanted.len() > MAX_LEN {
      0
    } else {
      wanted.len().max(3) / 3
    };
    Self {
      wanted,
      reach,
      best: None,
      starved: false,
      rows: Vec::new(),
    }
  }

  /// Whether the search goes on: it searches for a name at all, and the budget has not run out.
  pub(super) fn open(&self) -> bool {
    self.reach != 0 && !self.starved
  }

  /// Looks into a module, which a glob names, whose names are offered next: spends one of
  /// `budget`'s comparisons, whatever the module declares.
  pub(super) fn visit(&mut self, budget: &Cell<u32>) {
    if self.open() {
      self.spend(budget);
    }
  }

  /// Looks at `candidate`, spending one of `budget`'s comparisons.
  pub(super) fn offer(&mut self, budget: &Cell<u32>, candidate: &'c [u8]) {
    if !self.open() || !self.spend(budget) {
      return;
    }
    let wanted = self.wanted;
    if candidate.is_empty()
      || candidate == wanted
      || candidate.len().abs_diff(wanted.len()) > self.reach
    {
      return;
    }
    let Some(distance) = distance(wanted, candidate, self.reach, &mut self.rows) else {
      return;
    };
    let nearer = match self.best {
      None => true,
      Some((best, spelled)) => distance < best || (distance == best && candidate < spelled),
    };
    if nearer {
      self.best = Some((distance, candidate));
    }
  }

  /// Spends one of `budget`'s comparisons: `false`, and the search starved, when none is left.
  fn spend(&mut self, budget: &Cell<u32>) -> bool {
    let left = budget.get();
    if left == 0 {
      self.starved = true;
      return false;
    }
    budget.set(left - 1);
    true
  }

  /// The nearest name looked at: `None` when none is within reach, or the budget ran out before
  /// the search ended.
  pub(super) fn found(&self) -> Option<&'c [u8]> {
    if self.starved {
      return None;
    }
    self.best.map(|(_, spelled): (usize, &'c [u8])| spelled)
  }
}

/// The optimal string alignment distance between `a` and `b`, when it is at most `reach`: each
/// insertion, deletion, substitution and swap of two neighbouring bytes costs one.
///
/// `rows` holds three rows of the table, of `b.len() + 1` cells each, and the comparison stops at
/// the first row whose every distance is past `reach`, since a row's least distance never falls in
/// the rows after it.
fn distance(a: &[u8], b: &[u8], reach: usize, rows: &mut Vec<usize>) -> Option<usize> {
  let width = b.len() + 1;
  rows.clear();
  rows.resize(3 * width, 0);
  // Where each row starts in `rows`: the row two up, the row above, and the row being filled.
  let (mut before, mut previous, mut current) = (0, width, 2 * width);
  for (column, cell) in rows[previous..current].iter_mut().enumerate() {
    *cell = column;
  }
  for row in 1..=a.len() {
    rows[current] = row;
    let mut least = row;
    for column in 1..width {
      let cost = usize::from(a[row - 1] != b[column - 1]);
      let mut cell = (rows[previous + column] + 1)
        .min(rows[current + column - 1] + 1)
        .min(rows[previous + column - 1] + cost);
      if row > 1 && column > 1 && a[row - 1] == b[column - 2] && a[row - 2] == b[column - 1] {
        cell = cell.min(rows[before + column - 2] + 1);
      }
      rows[current + column] = cell;
      least = least.min(cell);
    }
    if least > reach {
      return None;
    }
    (before, previous, current) = (previous, current, before);
  }
  let found = rows[previous + b.len()];
  (found <= reach).then_some(found)
}
