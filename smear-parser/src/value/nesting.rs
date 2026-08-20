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
//! # The shape of the repair, and what makes it available here
//!
//! [`release`] takes a node's children onto a heap worklist and drives them in a loop, so the
//! native stack sees one frame however deep the tree is. What makes that expressible without an
//! `unsafe` read or a placeholder value is that **every recursive position in these trees is behind
//! a container**: a list's elements, an object's fields, a map's entries. A container can be
//! emptied through `&mut` — [`Vec::drain`] needs nothing to put back — so a child can be taken out
//! of a node the loop only borrows.
//!
//! A recursion through a *single* owned value has no such door. `ty::Type`'s `List(Box<ListType<Self>>)`
//! is the example in this workspace: taking the inner `Type` out through `&mut` needs another
//! `Type` to leave in its place, and no `Type` is constructible without a `Name`. That recursion
//! wants a representation change rather than this instrument, and it is not repaired here.
//!
//! # What the worklist costs
//!
//! Only nodes that themselves nest are pushed — [`Unnest::nests`] is a discriminant test — so a
//! list of a million scalars costs **nothing**: `Vec::new` does not allocate and the leaves are
//! released where they are found. A tree of *n* container nodes pays a worklist proportional to its
//! widest frontier of containers, which is bounded by the tree that is already resident.
//!
//! The worklist grows through `Vec`'s infallible `push`, deliberately and unlike
//! `smear::json`'s value walk, which grows through `try_reserve` and reports
//! `Error::Allocation`. A writer can refuse; a `Drop` has no return value and no caller to tell.
//! What is bought is a failure that needs the allocator to be exhausted by a request proportional
//! to a tree already in memory, in place of one that arrives at a fixed depth on every machine.

use std::vec::Vec;

/// A node that can hand its nesting children to a worklist.
///
/// Implemented on the value enums rather than on the carriers, because the enum is the one type
/// every cycle in these trees passes through: a list's elements and an object's fields are values,
/// and a value is what holds the next list. One implementation per enum therefore breaks every
/// cycle in it.
///
/// **The implementations match exhaustively and without a wildcard arm.** A variant added to one of
/// these enums is then a compile error here rather than a silent return to recursing — which is the
/// failure this whole module is written against, and the only kind of regression it could suffer
/// that no test would report.
pub(crate) trait Unnest: Sized {
  /// Whether this node owns children that can themselves own children.
  ///
  /// A discriminant test, so that a leaf is never put on the worklist.
  ///
  /// **It has to agree with [`unnest`](Self::unnest) about which variants are containers, and that
  /// is the invariant the whole bound rests on.** A child that is *not* pushed is released where
  /// it is found, inside the iterator — which runs its `Drop`, which calls [`release`] again. That
  /// nesting is exactly two frames deep as long as what is dropped in place is a leaf, because a
  /// leaf's `unnest` pushes nothing and drops nothing further. A container dropped in place
  /// instead of pushed would make the nesting one frame deeper *per level*, which is the recursion
  /// back again wearing a worklist.
  ///
  /// The two are the same predicate over the same variant list, written next to each other, and
  /// `smear-parser/tests/deep_value_release.rs` reads the frame cost at two depths so a
  /// disagreement between them is a number rather than an argument.
  fn nests(&self) -> bool;

  /// Moves every child of this node onto `pending`, leaving the node's containers empty.
  ///
  /// After this returns, releasing the node itself descends no further.
  fn unnest(&mut self, pending: &mut Vec<Self>);
}

/// Releases everything below `root`, on the heap rather than on the native stack.
///
/// `root` itself is left intact and empty: the caller is `Drop::drop`, which is holding it.
pub(crate) fn release<T: Unnest>(root: &mut T) {
  // Empty until a container with a nesting child is met, and `Vec::new` does not allocate, so a
  // leaf — the overwhelmingly common case, since every node released by the loop below is one —
  // pays nothing at all for being released this way.
  let mut pending = Vec::new();
  root.unnest(&mut pending);

  while let Some(mut node) = pending.pop() {
    node.unnest(&mut pending);
    // `node` is released here, and its own `Drop` runs: it finds emptied containers, allocates no
    // worklist and descends nowhere. That is the one frame this loop ever spends.
  }
}

/// Puts the nesting members of `children` onto `pending` and releases the rest where they are.
///
/// The filter is what keeps a wide, shallow container free: a scalar is dropped inside the
/// iterator, which costs the one bounded frame [`release`] already establishes.
pub(crate) fn push_nesting<T: Unnest>(pending: &mut Vec<T>, children: impl Iterator<Item = T>) {
  pending.extend(children.filter(Unnest::nests));
}
