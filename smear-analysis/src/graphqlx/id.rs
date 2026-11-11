//! The dense ids an analysis numbers its packages and sources with, and ranges of them.

use core::marker::PhantomData;

/// A package of one analysis.
///
/// Dense from 0, in this order: `core`, `alloc` and `std`; then the root package; then the
/// supplied dependencies by name, bytewise. The order is a function of the packages alone, so the
/// order the dependencies were passed in changes no id.
///
/// An id belongs to the analysis that issued it, and means nothing against another. Ids are stable
/// within one analysis, not across versions of smear.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageId(u32);

impl PackageId {
  /// The id whose index is `index`.
  ///
  /// The caller is responsible for the index belonging to the analysis it is used against.
  #[inline]
  pub const fn new(index: u32) -> Self {
    Self(index)
  }

  /// The raw index.
  #[inline]
  pub const fn get(&self) -> u32 {
    self.0
  }
}

/// A source of one analysis: a package source or a file.
///
/// Dense from 0, in package order. Within a package, its package source comes first, then its
/// files by path, compared segment by segment, each segment bytewise. The order is a function of
/// the packages alone, so the order a package's files were passed in changes no id.
///
/// This is the number a [`Location`](tokora::diagnostic::Location) carries as its source. An id at
/// or past the analysis's source count is not one of its sources: those ids are the consumer's, for
/// diagnostics about inputs of its own.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(u32);

impl SourceId {
  /// The id whose index is `index`: the number a diagnostic's location carries.
  #[inline]
  pub const fn new(index: u32) -> Self {
    Self(index)
  }

  /// The raw index: the number a diagnostic's location carries.
  #[inline]
  pub const fn get(&self) -> u32 {
    self.0
  }
}

/// A table length as the dense `u32` its ids are. A table of `2^32` rows is out of reach of any
/// input that fits in memory, so the saturation is never reached.
pub(crate) fn dense(len: usize) -> u32 {
  u32::try_from(len).unwrap_or(u32::MAX)
}

/// The ids an [`IdRange`] can hold.
mod sealed {
  /// A dense `u32` id. Sealed: only this crate's ids implement it.
  pub trait Dense: Copy {
    /// The id at `index`.
    fn at(index: u32) -> Self;
    /// This id's index.
    fn index(self) -> u32;
  }
}

impl sealed::Dense for PackageId {
  #[inline]
  fn at(index: u32) -> Self {
    Self(index)
  }

  #[inline]
  fn index(self) -> u32 {
    self.0
  }
}

impl sealed::Dense for SourceId {
  #[inline]
  fn at(index: u32) -> Self {
    Self(index)
  }

  #[inline]
  fn index(self) -> u32 {
    self.0
  }
}

/// A run of consecutive ids: a whole table, or one owner's children in it.
///
/// `Copy`, and iterated by value: `for id in range { … }`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IdRange<Id> {
  start: u32,
  end: u32,
  _id: PhantomData<Id>,
}

impl<Id> IdRange<Id> {
  /// The ids with indices `start..end`.
  #[inline]
  pub(crate) const fn new(start: u32, end: u32) -> Self {
    Self {
      start,
      end,
      _id: PhantomData,
    }
  }

  /// The first index, which is the end when the range is empty.
  #[inline]
  pub(crate) const fn start(&self) -> u32 {
    self.start
  }

  /// One past the last index.
  #[inline]
  pub(crate) const fn end(&self) -> u32 {
    self.end
  }

  /// How many ids the range holds.
  #[inline]
  pub const fn len(&self) -> u32 {
    self.end.saturating_sub(self.start)
  }

  /// Whether the range holds no id.
  #[inline]
  pub const fn is_empty(&self) -> bool {
    self.end <= self.start
  }
}

impl<Id: sealed::Dense> IdRange<Id> {
  /// Whether `id` is one of the range's ids.
  #[inline]
  pub fn contains(&self, id: Id) -> bool {
    let index = id.index();
    self.start <= index && index < self.end
  }
}

impl<Id: sealed::Dense> IntoIterator for IdRange<Id> {
  type Item = Id;
  type IntoIter = IdIter<Id>;

  #[inline]
  fn into_iter(self) -> IdIter<Id> {
    IdIter {
      next: self.start,
      end: self.end,
      _id: PhantomData,
    }
  }
}

/// The ids of an [`IdRange`], in order.
#[derive(Debug, Clone)]
pub struct IdIter<Id> {
  next: u32,
  end: u32,
  _id: PhantomData<Id>,
}

impl<Id: sealed::Dense> Iterator for IdIter<Id> {
  type Item = Id;

  #[inline]
  fn next(&mut self) -> Option<Id> {
    if self.next >= self.end {
      return None;
    }
    let id = Id::at(self.next);
    self.next += 1;
    Some(id)
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    let remaining = self.end.saturating_sub(self.next) as usize;
    (remaining, Some(remaining))
  }
}

impl<Id: sealed::Dense> DoubleEndedIterator for IdIter<Id> {
  #[inline]
  fn next_back(&mut self) -> Option<Id> {
    if self.next >= self.end {
      return None;
    }
    self.end -= 1;
    Some(Id::at(self.end))
  }
}

impl<Id: sealed::Dense> ExactSizeIterator for IdIter<Id> {}

impl<Id: sealed::Dense> core::iter::FusedIterator for IdIter<Id> {}
