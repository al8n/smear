//! The earlier position of a name list that wrote the name at a given position, scanned while the
//! list is narrow and sorted once when it is wide.

use std::{vec, vec::Vec};

/// The width at or below which a list finds its duplicates by scanning the names before them.
///
/// Sixty-four because that is where `smear-schema` has already measured the scan's quadratic term
/// to sit below the per-list overhead around it — the widest permitted argument list built in
/// 0.060 ms, and one sixty-four-argument list was *cheaper* than sixty-four one-argument lists —
/// and because it is `MAX_FIELD_ARGUMENTS`, so a declared argument list, which a ceiling already
/// holds at that width, never builds an index it could not need. al8n/smear#198.
pub const NARROW_LIST: usize = 64;

/// Which earlier position of a name list wrote the name at a given position.
///
/// # Sorted pairs, and why `first` is still the first
///
/// The wide path pairs every position with the name written there and sorts once. Sorting the
/// *pair* orders equal names by ascending position, so the head of each run is the first
/// occurrence rather than merely one of them, and the answer read back at position `p` is the
/// same position the trail would have been holding when the walk reached `p`.
///
/// # What it is not
///
/// **Not a hash map.** A `HashMap<Sym, u32>` per list allocates and hashes where an integer sort
/// does neither.
///
/// **Not a table indexed by `Sym`.** That is the shape `type_of_sym` and `directive_of_sym`
/// already have in the builder, it is `O(1)` rather than `O(list log list)`, and it does not
/// survive the nesting: a field list is walked *around* its arguments' lists and a directive list
/// around each usage's arguments, so one slot per symbol shared across a nested pair has the inner
/// list erase the outer's record of the same name. A value per list is reentrant because there is
/// nothing to share.
///
/// **Not more memory.** The trail it replaces recorded `(Sym, SimpleSpan)`, twenty-four bytes per
/// name; this is eight bytes per position while the index is built and four while it is read. A
/// position is a `u32` because everything `smear-schema` addresses is — `Sym` itself, `Range32`,
/// the flat tables `flatten` builds — so the width is the representation's and not a new one.
pub struct Duplicates<S> {
  repr: Repr<S>,
}

/// How one list finds its duplicates: the narrow path's trail, or the wide path's index.
enum Repr<S> {
  /// The names written so far and the position that wrote each, scanned in order.
  Trail(Vec<(S, u32)>),
  /// One entry per position: the position that first wrote that position's name, which is the
  /// position itself when this is the first.
  Index(Vec<u32>),
}

impl<S: Copy + Ord> Duplicates<S> {
  /// Resolves one list, `name` addressing it by position.
  ///
  /// The accessor is read at most once per position and only on the wide path; a narrow list
  /// never calls it.
  pub fn over(len: usize, name: impl Fn(usize) -> S) -> Self {
    if len <= NARROW_LIST {
      return Self {
        repr: Repr::Trail(Vec::with_capacity(len)),
      };
    }

    let mut order: Vec<(S, u32)> = (0..len).map(|at| (name(at), at as u32)).collect();
    order.sort_unstable();

    let mut first = vec![0u32; len];
    let mut head = order[0];
    first[head.1 as usize] = head.1;
    for &entry in &order[1..] {
      if entry.0 != head.0 {
        head = entry;
      }
      first[entry.1 as usize] = head.1;
    }
    Self {
      repr: Repr::Index(first),
    }
  }

  /// Answers the earlier position that wrote `name`, or records this one as its first.
  ///
  /// Call it once per position, in source order, and only where the rule actually applies — a
  /// repeatable directive is not recorded, exactly as the trail did not record one.
  pub fn first(&mut self, position: usize, name: S) -> Option<usize> {
    match &mut self.repr {
      Repr::Trail(trail) => match trail.iter().find(|(written, _)| *written == name) {
        Some(&(_, at)) => Some(at as usize),
        None => {
          trail.push((name, position as u32));
          None
        }
      },
      Repr::Index(first) => {
        let at = first[position] as usize;
        (at != position).then_some(at)
      }
    }
  }
}

#[cfg(test)]
mod tests;
