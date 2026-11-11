use tokora::SimpleSpan;

use super::Group;

/// One response key's group of selections, in the order the keys were first seen.
///
/// Cleared, never shrunk, like the rest of [`Scratch`](super::Scratch).
#[derive(Default)]
pub(super) struct Groups {
  inner: std::vec::Vec<Group>,
}

impl Groups {
  /// The walk's view: append, and read back what was appended. See the module.
  pub(super) fn appending(&mut self) -> Appending<'_> {
    Appending {
      inner: &mut self.inner,
    }
  }

  /// Every group, for the phases that are allowed to enumerate them.
  #[inline]
  pub(super) fn as_slice(&self) -> &[Group] {
    &self.inner
  }

  /// Every group, mutably, for the range assignment that follows the walk.
  #[inline]
  pub(super) fn iter_mut(&mut self) -> core::slice::IterMut<'_, Group> {
    self.inner.iter_mut()
  }

  #[inline]
  pub(super) fn clear(&mut self) {
    self.inner.clear();
  }

  /// What the vector is holding, for the gate that pins the scratch against the ceiling that
  /// refuses it.
  #[cfg(test)]
  #[inline]
  pub(super) fn capacity(&self) -> usize {
    self.inner.capacity()
  }
}

/// The groups, as a walk may touch them: append one, and read one back by its index.
///
/// The field is private to this module, so nothing in `collect` — [`walk`](super::walk)
/// included — can reach the vector through it. That is the mechanism; see the module for why the
/// alternative guards do not work here.
pub(super) struct Appending<'g> {
  inner: &'g mut std::vec::Vec<Group>,
}

impl Appending<'_> {
  /// Appends `group` and answers with its index, which is the only index a walk ever has.
  ///
  /// Returning it rather than making the caller read `len()` first is what leaves this type with
  /// no length: a search needs a bound to walk to, and there is none to ask for.
  #[inline]
  pub(super) fn push(&mut self, group: Group) -> u32 {
    let index = self.inner.len() as u32;
    self.inner.push(group);
    index
  }

  /// The span of the group's first selection. See [`Group::first`].
  ///
  /// One field rather than the whole group, because one field is what the walk reads — and a span
  /// carries no response key, so even a caller with an index cannot compare its way through the
  /// list.
  #[inline]
  pub(super) fn first(&self, index: u32) -> SimpleSpan {
    self.inner[index as usize].first
  }
}
