//! Releasing a nested value without a native frame per level.
//!
//! # The defect this exists to remove
//!
//! A value tree owns its children, so the `Drop` glue the compiler generates for one descends
//! **one native frame per level of nesting** and there is nothing at the bottom to stop it. Nothing
//! measured the depth on the way in, so nothing can refuse on the way out: releasing a value is not
//! a fallible operation and there is no diagnostic to return. The process aborts.
//!
//! Measured on `aarch64-apple-darwin`, unoptimised, against the generated glue: **175 bytes of
//! frame per level**, so a value 11 967 lists deep released on libtest's default 2 MiB test thread
//! and 11 968 aborted with `SIGABRT`; on an 8 MiB thread the boundary was 47 722 / 47 723. The
//! numbers are a property of the host and the build. The *shape* is not.
//!
//! # Why the parser's nesting ceiling is not the answer
//!
//! [`MAX_NESTING_DEPTH`](smear_lexer::MAX_NESTING_DEPTH) refuses a parse past 24 brackets, and a
//! parse is a recursive descent whose frames are an order of magnitude fatter than the drop glue's
//! — measured in `smear-lexer`'s own header at roughly 4 KiB per level, against the 175 bytes here.
//! So a value that **came from the parser** can never be deep enough for its release to matter: the
//! parse gives out first, by a factor of more than twenty, whatever the ceiling is set to.
//!
//! That argument does not reach this tree, and the reason is the whole of why this module exists.
//! A value is the *driver's* — a `graphql_proto::Values::Value` a resolver returned, an entry in a
//! draft §7.1.7 `extensions` map, a request-error result's payload. Every constructor these
//! carriers need is public, so building one 100 000 deep is safe code that no parser saw. It was
//! measured going the whole way: an execution result whose `extensions` map held a deep value
//! **wrote its response out whole** — 40 050 bytes, the writer's own bound holding — and the
//! process then died in the executor's release of the map.
//!
//! # Where the release lives, and why it is not on the enum
//!
//! The obvious place for a hand-written `Drop` is the value enum, which is the type every cycle in
//! these trees passes through. It was written there first and it works. **It also deletes about a
//! hundred public methods**, and that is a fact about the language rather than about the code:
//! `E0509` forbids moving a payload out of a type that implements `Drop`, so an enum that carries
//! one loses every by-value `unwrap_*` and `try_unwrap_*` `derive_more` generates for it, and its
//! `IntoSpan` can no longer reach a span by matching itself apart.
//!
//! `E0509` fires on **the type that implements `Drop`**, not on a type one of whose *fields*
//! implements it. So the release lives here, on [`Nested`] — the container the recursive positions
//! sit behind — and the six value enums declare no `Drop` at all. Their derives, their owned
//! `unwrap_list(self)`, their `IntoSpan` bodies and their destructuring are exactly what they were.
//!
//! The relocation is also what makes the loop *simpler* rather than harder: because the enums have
//! no `Drop`, [`Nestable::into_children`] takes one **by value** and matches it apart, instead of
//! reaching through `&mut` and draining containers in place.
//!
//! The relocation is not slower, either. Measured on `aarch64-apple-darwin`, release, against the
//! enum-side `Drop` this module replaced: a leaf-only container costs the same either way, and a
//! container whose elements themselves nest is *cheaper* here, 2.25x–2.61x the derived glue's
//! per-element cost against the enum-side design's 2.71x–2.96x. Both shapes pay for not recursing;
//! only this one also comes out ahead once the tree actually nests.
//!
//! # What makes it expressible at all
//!
//! **Every recursive position in these trees sits behind a container** — a list's elements, a set's
//! members, an object's fields, a map's entries. That is what gives the release a type to hang a
//! `Drop` on, and a `Vec` it can take by [`core::mem::take`] with nothing to put back.
//!
//! A recursion through a *single* owned value has no such door. `ty::Type`'s
//! `List(Box<ListType<Self>>)` is the example in this workspace: there is no container in that
//! cycle, so there is no type in it whose `Drop` could be written without also making the `Type`
//! enum itself undroppable-by-move. That recursion wants a representation change rather than this
//! instrument, and it is not repaired here.
//!
//! # What the worklist costs
//!
//! The worklist allocates nothing for a leaf: a leaf is released the moment it is reached rather
//! than being put on it, so a list of a million scalars never grows `pending` past its initial
//! `Vec::new`, which does not allocate. Every element still pays a call into
//! [`Nestable::into_children`] to find that out, though. Measured on `aarch64-apple-darwin`,
//! release: releasing a leaf this way still costs roughly 3x what the derived glue does, even with
//! `#[inline]` on all six impls. A tree of *n* container nodes pays that same call plus a worklist
//! proportional to its widest frontier of containers, bounded by the tree that is already resident
//! — +12–21 ns per element relative to the derived glue.
//!
//! It grows through `Vec`'s infallible `push`, deliberately and unlike `smear::json`'s value walk,
//! which grows through `try_reserve` and reports `Error::Allocation`. A writer can refuse; a `Drop`
//! has no return value and no caller to tell. What is bought is a failure that needs the allocator
//! exhausted by a request proportional to a tree already in memory, in place of one that arrives at
//! a fixed depth on every machine.

use core::{fmt, ops::Deref, slice};

use std::vec::Vec;

mod sealed {
  /// Closes [`Nestable`](super::Nestable) to this crate.
  ///
  /// The trait is `pub` because it is a bound on [`Nested`](super::Nested)'s *definition* and so
  /// reaches the public signature of every value carrier. It is sealed because an outside
  /// implementation would be handed the release's invariant to keep — see
  /// [`Nestable::into_children`](super::Nestable::into_children) — and a `Drop` that a consumer can
  /// make recurse is the defect back again with a longer path to it.
  pub trait Sealed {}
}

pub(crate) use sealed::Sealed;

/// An element a [`Nested`] container knows how to take apart.
///
/// Implemented on the value enums, and on the field and entry carriers a container holds instead of
/// values. It is sealed: only this crate can implement it, so the release's invariant cannot be
/// broken from outside.
pub trait Nestable: Sized + Sealed {
  /// What the worklist holds.
  ///
  /// For a value enum this is the enum itself. For a carrier that merely *wraps* a value — an
  /// object's field, a map's entry — it is the value, because that is what can nest and what the
  /// loop must be able to come back to.
  type Node: Nestable<Node = Self::Node>;

  /// Consumes this element and puts every child that can itself nest onto `pending`.
  ///
  /// # The invariant the bound rests on
  ///
  /// **Anything not pushed is released here, so anything not pushed must be a leaf.** A child
  /// released inside this call runs its own container's [`Drop`], which re-enters this loop; that
  /// is exactly two frames deep as long as what was released is a leaf, because a leaf's
  /// `into_children` pushes nothing and releases nothing further. A *container* released here
  /// instead of pushed would add a frame per level, which is the recursion back again wearing a
  /// worklist.
  ///
  /// The implementations match exhaustively and without a wildcard arm, so a variant added to one
  /// of these enums is a compile error here rather than a silent return to recursing.
  fn into_children(self, pending: &mut Vec<Self::Node>);
}

/// A container whose release is a loop rather than a descent.
///
/// It is a `Vec` in every respect a consumer can observe — it derefs to a slice, collects from an
/// iterator, compares, clones and prints the same — and the one thing it adds is the [`Drop`] that
/// keeps a deep value from taking the process with it.
///
/// # Why the tree's container parameter defaults to this and not to [`Vec`]
///
/// Because the parameter is where the release has to be installed. `List`, `Object`, `Set` and
/// `Map` are generic over their container and cannot carry the release themselves: a `Drop` impl
/// may not add bounds their definitions do not have, and iterating a `Container` needs bounds.
/// Putting those bounds on the carriers instead would make every alias in both dialects carry them,
/// and would still cost `into_values` to `E0509`.
///
/// A consumer who names a container explicitly — `List<S, Vec<InputValue<S>>>` — gets a `Vec` and
/// gets the old behaviour for *that one level*; the values inside it still hold this type, so the
/// release is still bounded below the first level. The default is what the value enums use, and the
/// value enums are what nests.
pub struct Nested<T: Nestable> {
  values: Vec<T>,
}

impl<T: Nestable> Nested<T> {
  /// Wraps a `Vec` of elements.
  #[inline]
  pub const fn new(values: Vec<T>) -> Self {
    Self { values }
  }

  /// An empty container, allocating nothing.
  #[inline]
  #[must_use]
  pub const fn empty() -> Self {
    Self::new(Vec::new())
  }

  /// The elements as a slice.
  #[inline]
  pub fn as_slice(&self) -> &[T] {
    &self.values
  }

  /// Adds one element to the end.
  #[inline]
  pub fn push(&mut self, value: T) {
    self.values.push(value);
  }

  /// Consumes this container and returns the elements.
  ///
  /// Takes rather than moves the field out: this type implements [`Drop`], which is `E0509`'s
  /// trigger, and taking is what a `Vec` supports for free.
  #[inline]
  #[must_use]
  pub fn into_vec(mut self) -> Vec<T> {
    core::mem::take(&mut self.values)
  }
}

impl<T: Nestable> Drop for Nested<T> {
  fn drop(&mut self) {
    // Empty until an element with a nesting child is met, and `Vec::new` does not allocate — a
    // container of scalars allocates nothing here. It is not released for free, though: every
    // element still pays a call into `into_children`. Measured on `aarch64-apple-darwin`, release:
    // a leaf-only container is still roughly 3x slower to release this way than through the derived
    // glue, about +2.9 ns per element before `#[inline]` reached these impls, less after. That gap
    // is what the call costs, not what `Vec::new` costs.
    let mut pending: Vec<T::Node> = Vec::new();
    for element in core::mem::take(&mut self.values) {
      element.into_children(&mut pending);
    }
    while let Some(node) = pending.pop() {
      node.into_children(&mut pending);
      // `node` is consumed by the call, and whatever it released instead of pushing was a leaf.
      // That is the one frame this loop ever spends.
    }
  }
}

impl<T: Nestable> Default for Nested<T> {
  #[inline]
  fn default() -> Self {
    Self::empty()
  }
}

impl<T: Nestable> Deref for Nested<T> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.as_slice()
  }
}

impl<T: Nestable> AsRef<[T]> for Nested<T> {
  #[inline]
  fn as_ref(&self) -> &[T] {
    self.as_slice()
  }
}

impl<T: Nestable> From<Vec<T>> for Nested<T> {
  #[inline]
  fn from(values: Vec<T>) -> Self {
    Self::new(values)
  }
}

impl<T: Nestable> From<Nested<T>> for Vec<T> {
  #[inline]
  fn from(nested: Nested<T>) -> Self {
    nested.into_vec()
  }
}

impl<T: Nestable> FromIterator<T> for Nested<T> {
  #[inline]
  fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
    Self::new(iter.into_iter().collect())
  }
}

impl<T: Nestable> IntoIterator for Nested<T> {
  type Item = T;
  type IntoIter = std::vec::IntoIter<T>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.into_vec().into_iter()
  }
}

impl<'a, T: Nestable> IntoIterator for &'a Nested<T> {
  type Item = &'a T;
  type IntoIter = slice::Iter<'a, T>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.as_slice().iter()
  }
}

impl<T: Nestable + fmt::Debug> fmt::Debug for Nested<T> {
  /// The elements, as a `Vec` prints them — this wrapper is not part of what a value *is*.
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(self.as_slice(), f)
  }
}

impl<T: Nestable + Clone> Clone for Nested<T> {
  #[inline]
  fn clone(&self) -> Self {
    Self::new(self.values.clone())
  }
}

impl<T: Nestable + PartialEq> PartialEq for Nested<T> {
  #[inline]
  fn eq(&self, other: &Self) -> bool {
    self.values == other.values
  }
}

impl<T: Nestable + Eq> Eq for Nested<T> {}

impl<T: Nestable + core::hash::Hash> core::hash::Hash for Nested<T> {
  #[inline]
  fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
    self.values.hash(state);
  }
}

/// A `Vec`'s own behaviour: a delimiter is the *combinator's* business, not the container's.
impl<'inp, L, T: Nestable> tokora::parser::DelimiterHandler<'inp, L> for Nested<T> {
  #[inline(always)]
  fn on_open_delimiter(&mut self, _open: tokora::span::Spanned<L::Token, L::Span>)
  where
    L: tokora::Lexer<'inp>,
  {
  }

  #[inline(always)]
  fn on_close_delimiter(&mut self, _close: tokora::span::Spanned<L::Token, L::Span>)
  where
    L: tokora::Lexer<'inp>,
  {
  }
}

/// So the parser's `collect_with` can accumulate straight into this container.
impl<T: Nestable> tokora::container::Container<T> for Nested<T> {
  #[inline]
  fn push(&mut self, item: T) -> Result<(), T> {
    self.values.push(item);
    Ok(())
  }

  #[inline]
  fn first(&self) -> Option<&T> {
    self.values.first()
  }

  #[inline]
  fn last(&self) -> Option<&T> {
    self.values.last()
  }

  #[inline]
  fn len(&self) -> usize {
    self.values.len()
  }

  /// `usize::MAX`, which is what the trait asks of a container with no fixed maximum and what
  /// tokora's own `Vec` impl answers.
  ///
  /// Not `Vec::capacity`. That is what is currently *allocated*, and this container grows past it
  /// on the next push — so reporting it as a maximum says a fresh [`Nested::empty`] is full, and
  /// says something different again after every reallocation. tokora reads this in exactly one
  /// place, to name the bound in a `FullContainer` diagnostic **after** a push was refused, and a
  /// push here is never refused; but the method is public on a public type, and a wrong answer
  /// that no caller happens to read is still a wrong answer.
  #[inline]
  fn max_capacity(&self) -> usize {
    usize::MAX
  }
}
