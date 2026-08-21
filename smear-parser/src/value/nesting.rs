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
//! implements it. So the release lives here, on [`Nested`] — the container the grammar's recursive
//! positions sit behind — and the six value enums declare no `Drop` at all. Their derives, their
//! owned `unwrap_list(self)`, their `IntoSpan` bodies and their destructuring are exactly what they
//! were.
//!
//! The relocation is also what makes the loop *simpler* rather than harder: because the enums have
//! no `Drop`, [`Nestable::into_children`] takes one **by value** and matches it apart, instead of
//! reaching through `&mut` and draining containers in place.
//!
//! The relocation is not slower, either. Measured on `aarch64-apple-darwin`, release, against the
//! enum-side `Drop` this module replaced, and before the `#[inline]` mitigation reached either of
//! them: a leaf-only container costs the same both ways, and a container whose elements themselves
//! nest is *cheaper* here, 2.25x–2.61x the derived glue's per-element cost against the enum-side
//! design's 2.71x–2.96x — an ordering `#[inline]` does not disturb, since it takes this side to
//! 2.20x–2.58x. Both shapes pay for not recursing; only this one also comes out ahead once the
//! tree actually nests.
//!
//! **One instrument stands behind that comparison**, unlike the band further down: the enum-side
//! design exists only as a superseded commit on the branch that wrote this module, and the second
//! harness measured this container against a plain `Vec`, never against that.
//!
//! # What makes it expressible at all
//!
//! **Every recursive position the grammar forms sits behind a container** — a list's elements, a
//! set's members, an object's fields, a map's entries. That is what gives the release a type to
//! hang a `Drop` on, and a `Vec` it can take by [`core::mem::take`] with nothing to put back. That
//! sentence has to name the grammar, and the next section is why.
//!
//! A recursion through a *single* owned value has no such door. `ty::Type`'s
//! `List(Box<ListType<Self>>)` is the example in this workspace: there is no container in that
//! cycle, so there is no type in it whose `Drop` could be written without also making the `Type`
//! enum itself undroppable-by-move. That recursion wants a representation change rather than this
//! instrument, and it is not repaired here.
//!
//! # What the guarantee ranges over, and where it stops
//!
//! Everything above is a statement about the recursive positions **these types form themselves**.
//! Over that set it is exact: a value that nests through a list, a set, an object or a map is
//! released in constant native stack at any depth, on all six enums, and the grammar reaches a
//! recursive position by no other route.
//!
//! It is not a statement about what a caller puts in a payload parameter. These types are generic
//! and most of those parameters carry no bound at all, so a caller may instantiate one with a type
//! that owns a node. That builds a cycle which never passes through a container: a value enum is
//! *on* the cycle, but by an arm this loop correctly releases as a leaf, so [`Nested`] is not in it
//! and cannot see it. Releasing one descends the derived glue of the caller's own type, one native
//! frame per level, exactly as it did before this module existed. Measured on
//! `aarch64-apple-darwin`, unoptimised, on chains threaded through a `Name`'s source: through an
//! object field's name, 1 000 released and 20 000 aborted; through the `Variable` arm, which spends
//! fewer frames per level, 20 000 released and 100 000 aborted. It is tracked as `al8n/smear#176`,
//! and it is **not** a regression —
//! the same chain aborted identically before this branch, and the enum-side `Drop` this module
//! rejected would not have helped, because the recursion is in the caller's own glue rather than in
//! anything either design owns.
//!
//! **Sealing does not address this and never could.** [`Nestable`] being sealed decides *who may
//! implement the trait*. That is why `T` in `Nested<T>` can only be one of this crate's own
//! elements, and over that parameter it is complete. It decides nothing about *what a payload may
//! be*, and a payload is where a caller's type gets in.
//!
//! ## Which parameters, derived from the tree rather than named from one witness
//!
//! **Unbounded, owned by value, and reachable through a public constructor.** These are the ways
//! in:
//!
//! * `S`, the source representation, on all six value enums. `Name<S>` owns its `S` and
//!   `Name::new` is public, so `S` reaches a live tree through an object field's *name* slot on
//!   every one of the six, and through the `Variable` arm on the three that have one. The `String`,
//!   `Float`, `Int`, `Enum` and `Null` leaves own an `S` too, but their constructors are
//!   `pub(crate)`, so they are not additional doors today.
//! * `Span`, on the GraphQLx pair — the only pair that leaves it a parameter rather than pinning it
//!   to `SimpleSpan`. Every carrier in that dialect holds a `Span` by value and every one of their
//!   constructors is public, so this is the widest of the three.
//! * `Name` and `Span` on `ObjectField`, and `Span` on `MapEntry`: the same parameters seen from
//!   the carrier instead of from the enum. `ObjectField`'s `Name` is exactly the slot its own
//!   [`Nestable::into_children`] releases as a leaf.
//!
//! **Unbounded and nameable, but not reachable today:**
//!
//! * `I`, the integer width on the materialised pair. `IntValue` owns it by value, but
//!   `IntValue::new` is `pub(crate)` and no public item returns one, so a caller can write
//!   `InputValue<S, MyInt>` and has no way to build the `Int` arm that would hold a `MyInt`. That
//!   is a fact about one constructor's visibility rather than about the type, and it stops holding
//!   the day the constructor is published.
//!
//! **Structurally unable to hold a node, so not on the list at all:**
//!
//! * `Lang`, the dialect marker on every shared carrier: `?Sized` and held only as `PhantomData`,
//!   and pinned to `GraphQL` or `GraphQLx` by the aliases besides — two independent reasons.
//! * `S` in `BooleanValue`, alone among the leaves, which is `PhantomData<S>` with no `S` field.
//!   That one arm cannot own a node even at a caller-chosen `S`; the other six are why the enum
//!   still can.
//! * The element parameters of `List`, `Set`, `Object` and `Map`. Each is `PhantomData`, and what
//!   those carriers actually own is their `Container`.
//! * `T` in `Nested<T>`, and the `Nestable`-bounded halves of `ObjectField` and `MapEntry`. These
//!   are the parameters sealing does reach, and over them it is airtight.
//!
//! The `Container` parameter is unbounded and owned and is still not a way in, for a reason that is
//! about shape rather than about a bound — see [`Nested`]'s own header below.
//!
//! # What this did to the ceiling, which is not what it looks like
//!
//! `Drop` is one of three generated impls that descend one frame per level; the derived `Debug`
//! and the derived `Clone` still do. **So this module did not lower a ceiling. It removed the only
//! member of the three that fires without a call being made.** For anyone who formats or clones a
//! value, the binding ceiling was already the smaller of those two, before any of this: measured on
//! the same host and the same 2 MiB thread, `Debug` aborts at **2 270** levels and `Clone` at
//! **1 711**, against the recursive release's **11 968**. Release is not a call anyone makes — it
//! happens to whoever holds the value, on scope exit, on unwind, in an executor's teardown of its
//! own map, and it can be neither caught nor refused. That is why it went first, and it is the
//! whole of what "unavoidable" bought.
//!
//! The relocation's own charge, stated rather than left out: putting a container in the cycle adds
//! a frame to the two recursions that remain. Re-measured after it, `Debug` aborts at **2 231** and
//! `Clone` at **1 626** — those chosen-call ceilings came down about 2% and 5%. A consumer who
//! neither formats nor clones had 11 968 and now has none; a consumer who does either is a few
//! percent worse off on a ceiling it already had. The repair for those two also lands here rather
//! than on the enums — [`Nested`]'s own `Debug` and `Clone` can walk a subtree iteratively without
//! any derive noticing — so this is where they get cheaper to fix, not harder.
//!
//! # What the worklist costs
//!
//! The worklist allocates nothing for a leaf: a leaf is released the moment it is reached rather
//! than being put on it, so a list of a million scalars never grows `pending` past its initial
//! `Vec::new`, which does not allocate. **What it costs is a call, not an allocation** — every
//! element pays one call into [`Nestable::into_children`] to find out that it is a leaf. A tree of
//! *n* container nodes pays that same call plus a worklist proportional to its widest frontier of
//! containers, bounded by the tree that is already resident.
//!
//! Two independent harnesses priced that call on `aarch64-apple-darwin`, release, with `#[inline]`
//! on all six impls. **They disagree, and the band they span is the claim** — neither end of it is:
//!
//! * a leaf-only container, **+1.4 to +2.0 ns per element, 2.4x–3.6x** the derived glue;
//! * a container whose elements themselves nest, **+9 to +20 ns per element**.
//!
//! Where the disagreement comes from, as far as it can be attributed. One harness compares two
//! separately compiled binaries — the container as a `Vec` before this branch against [`Nested`]
//! after — and times the release of a whole parsed document, 1 000 to 200 000 elements. The other
//! names both containers inside one build and times the container alone, at 1 000 000. At a
//! million elements of a fat enum the *baseline* is bandwidth-bound: it reads 1.01 ns per element
//! against the smaller runs' 0.62–0.83, while both instruments put this container itself inside
//! 2.2–2.8. So most of the spread is in the denominator rather than in the subject, and
//! the additive figure is the steadier of the two readings. Neither harness is committed, so
//! neither number is a regression target; the band is.
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
  /// [`Nestable::into_children`](super::Nestable::into_children) — and an `into_children` that
  /// pushed nothing for a container is the defect back again with a longer path to it.
  ///
  /// **What sealing buys is that one thing.** It fixes the set of elements a
  /// [`Nested`](super::Nested) can hold, so the loop's invariant is kept by code in this crate and
  /// nowhere else. It does not fix what those elements' *payload parameters* may be, and a caller
  /// who instantiates one with a type owning a node builds a recursion the loop never sees — the
  /// module header derives which parameters those are.
  pub trait Sealed {}
}

pub(crate) use sealed::Sealed;

/// An element a [`Nested`] container knows how to take apart.
///
/// Implemented on the value enums, and on the field and entry carriers a container holds instead of
/// values. It is sealed: only this crate can implement it, so no outside impl can break the
/// release's invariant. Sealing reaches the *implementors*; it does not reach the payload
/// parameters those implementors are generic over, which is a separate question — see [`Nested`]'s
/// own documentation.
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
  /// "Leaf" means **holds no node of this crate's own**. An arm an implementation treats as a leaf
  /// still releases whatever its payload parameters were instantiated with, and a caller who put a
  /// value tree in one of those has a recursion here that no implementation can push — see
  /// [`Nested`] for the scope that follows from it.
  ///
  /// The implementations match exhaustively and without a wildcard arm, so a variant added to one
  /// of these enums is a compile error here rather than a silent return to recursing.
  fn into_children(self, pending: &mut Vec<Self::Node>);
}

/// A container whose release is a loop rather than a descent.
///
/// **Every trait it implements answers as `Vec`'s does.** It derefs to a slice, collects from an
/// iterator, compares, hashes, clones and prints the same, and tokora's `Container` and
/// `DelimiterHandler` get the same answers `Vec` gives them, `max_capacity` included. The one
/// thing it adds is the [`Drop`] that keeps a value nested through this crate's own carriers from
/// taking the process with it, however deep it is.
///
/// It is not `Vec`'s whole *surface*, and that is the weaker claim to make. There is no
/// `DerefMut`, no `AsMut`, no `Extend`, no `PartialOrd`, no cross-type `PartialEq`, and no
/// inherent growth or mutation API beyond [`push`](Self::push). Reaching for one of those is a
/// compile error at the call site, which is the failure mode worth having: an absent impl cannot
/// be mistaken for a `Vec` that behaves differently. [`into_vec`](Self::into_vec) hands the real
/// `Vec` back for the rest.
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
///
/// That is why the container parameter, unbounded though it is, is not a way to rebuild the defect:
/// a nesting arm names its alias **with the default**, so a consumer's container appears at the
/// level they wrote it and can never be re-embedded under one. The parameters that *are* a way in
/// are the payload ones, below — the container axis is the one this paragraph closes, and it closes
/// only that one.
///
/// # What the release covers, and what it does not
///
/// It covers every recursive position the value types form themselves — a list's elements, a set's
/// members, an object's fields, a map's entries — at any depth, on every value enum in both
/// dialects. That is the shape a parse or a resolver produces, and over it the bound is exact.
///
/// It does not cover a node reached through a **payload parameter**. These types are generic and
/// most of those parameters carry no bound: instantiate a value enum's source representation `S`,
/// or GraphQLx's `Span`, with a type that owns a value, and the cycle runs through an arm
/// [`Nestable::into_children`] correctly releases as a leaf. This container is not on that cycle
/// and cannot see it, so releasing one descends the caller's own derived glue at one native frame
/// per level. Measured unoptimised on `aarch64-apple-darwin`, on a chain threaded through an object
/// field's name: 1 000 released, 20 000 aborted. Tracked as `al8n/smear#176`; it predates this
/// container and is not something moving the release onto the value enums would have fixed either.
///
/// **Sealing [`Nestable`] does not address that.** Sealing decides who may implement the trait,
/// which is why `T` here is always one of this crate's own elements and why the loop's invariant
/// is kept in this crate and nowhere else. It decides nothing about what a payload may be.
/// `value/nesting.rs`'s module header derives the rest: which parameters are a way in, which are
/// unbounded but have no public constructor to reach them through, and which cannot hold a node at
/// all.
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
    // element still pays a call into `into_children` to find out it is a leaf, and that call is
    // the whole of the cost. Two harnesses priced it on `aarch64-apple-darwin`, release, and they
    // bracket it rather than agree: +1.4 to +2.0 ns per element on a leaf-only container, 2.4x to
    // 3.6x the derived glue. The module header says which instrument read which end, and why.
    let mut pending: Vec<T::Node> = Vec::new();
    for element in core::mem::take(&mut self.values) {
      element.into_children(&mut pending);
    }
    while let Some(node) = pending.pop() {
      node.into_children(&mut pending);
      // `node` is consumed by the call, and whatever it released instead of pushing held no node of
      // this crate's own. That is the one frame this loop ever spends on the tree these types form;
      // what a caller instantiated a payload parameter with is released inside it, and the module
      // header says why that is a bound this container cannot place.
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
