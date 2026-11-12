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

/// Declares one of the model's dense ids: a `u32` newtype with `new` and `get`, as [`PackageId`]
/// and [`SourceId`] are, and the sealed impl an [`IdRange`] of it needs.
macro_rules! model_id {
  ($(#[$meta:meta])* $name:ident) => {
    $(#[$meta])*
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct $name(u32);

    impl $name {
      /// The id whose index is `index`.
      ///
      /// The caller is responsible for the index belonging to the model it is used against.
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

    impl sealed::Dense for $name {
      #[inline]
      fn at(index: u32) -> Self {
        Self(index)
      }

      #[inline]
      fn index(self) -> u32 {
        self.0
      }
    }
  };
}

model_id! {
  /// A module of one analysis: a package's root module, a directory module, or an inline `mod`.
  ///
  /// Dense from 0, in package order. Within a package, by module path, compared segment by
  /// segment, each segment bytewise, so a package's root module comes first and a module precedes
  /// its descendants. Two inline `mod`s with one path are one module.
  ModuleId
}

model_id! {
  /// A type or directive definition of one analysis.
  ///
  /// Dense from 0, ordered by package, then module path (as [`ModuleId`] orders it), then source
  /// id, then position. The definitions materialised for intrinsic primitives sit at `core`'s
  /// package source, so they come first in `core`'s root module, in the order `bool`, `f32`, `f64`,
  /// then the signed widths ascending, then the unsigned widths ascending.
  DefId
}

model_id! {
  /// A field, argument, input field or enum value of one analysis.
  ///
  /// Dense from 0, ordered by the owner's [`DefId`]. Within an owner, its direct members come
  /// first (fields, input fields, enum values, or a directive definition's arguments) in
  /// contribution order: the definition, then each extension by source id and position, and in
  /// written order within each. Then, field by field in that order, each field's arguments. So an
  /// owner's direct members, and each field's arguments, are runs of consecutive ids.
  MemberId
}

model_id! {
  /// One written type occurrence of one analysis: what a diagnostic about a type points at.
  ///
  /// Dense from 0, in the order the walk over every package reaches them, nested occurrences in
  /// pre-order: an occurrence first, then its arguments or its element.
  TypeExprId
}

model_id! {
  /// A type's identity in one analysis: two occurrences of one type share one `TyId`, so `==` on
  /// `TyId` is type equality.
  ///
  /// Dense from 0, interned as the walk over every package reaches each type expression, a type's
  /// arguments before the type itself. A type no expression writes (an argument filled from a
  /// default, an alias's argument) is interned where the walk first needs it.
  TyId
}

model_id! {
  /// One value occurrence of one analysis: a default, an argument's value, or a value nested in
  /// one.
  ///
  /// Dense from 0, in the order the walk over every package reaches them, nested values in
  /// pre-order: a list, set, map or object first, then what it holds.
  ValueId
}

model_id! {
  /// One directive application of one analysis: one `@directive(…)` as written.
  ///
  /// Dense from 0, in the order the walk over every package reaches them.
  ApplicationId
}

model_id! {
  /// An interned name of one analysis.
  ///
  /// Dense from 0, by first occurrence in source-id order, then position. A name the analyzer
  /// makes itself is interned when it is made. Read it with
  /// [`Model::name`](super::Model::name).
  Sym
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
