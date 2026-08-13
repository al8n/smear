//! The response tree, and the response path draft §7.1.2 attaches to every field error.
//!
//! # The tree is also the work list
//!
//! Draft §6.4.4 is a *retroactive* operation: a field error deep in a selection set nulls an
//! ancestor that has already been built. So a tree has to exist before the response is finished,
//! and once it does there is no reason to keep a second structure listing the fields still to be
//! resolved — every one of them is already a slot in this tree, in state [`State::Ready`]. The
//! executor threads an intrusive singly-linked chain through those slots
//! ([`Slot::next_ready`]), so the outstanding work costs one `u32` per node that had to exist
//! anyway and there is no queue to grow.
//!
//! That is what "no unbounded queue anywhere in `proto`" means here, and it is worth stating what
//! it does *not* mean. The tree itself grows with the response — a service that returns a hundred
//! thousand list elements gets a hundred thousand slots — because that is the answer, not a buffer
//! between a producer and a consumer. What `proto` refuses to do is accumulate work the driver has
//! not asked for: `poll_resolve` withholds at the in-flight ceiling rather than reading ahead.
//!
//! Growing *with the response* is not the same as growing *without bound*, and the difference is a
//! ceiling rather than an argument.
//! [`Limits::max_response_slots`](super::Limits::max_response_slots) caps how many positions this
//! tree may hold and is checked before each one is created, because the length of a list is
//! [`Values::list_len`](super::Values::list_len)'s answer and draft §6.4.3 completes the elements
//! synchronously — so without it a single `handle_resolved` would allocate for as long as the
//! driver claimed, with no poll in between at which a caller could push back.
//!
//! # Shape is `proto`'s, leaves are the driver's
//!
//! A slot is a position: a parent, a key, the [`PackedType`] that position has in the schema, and
//! a state. Only [`State::Leaf`] and [`State::Object`] hold a `V`, and in both cases the `V` is the
//! driver's own value, moved in and never inspected except through
//! [`Values`](super::Values). Nothing in this module knows what an `Int` is.
//!
//! [`State::TypeName`] is the one position whose *value* is `proto`'s, and it is the exception that
//! states the rule. Draft §4.4's `__typename` answers with the name of the object type §6.4.3
//! resolved — a fact about the shape of the response, discovered by the executor and already used
//! by it to decide which fragments applied. It is held here the same way a response key is, as an
//! id into the executor's name table, and it reaches a reader as [`Node::TypeName`] rather than
//! [`Node::Leaf`] so that "this came from the driver" stays a property a serialiser can read off
//! the type.
//!
//! # Why the type is on the slot
//!
//! [`Slot::ty`] is the type of the *position*, not of the field — for a list element it is the
//! element type, for the field itself it is the field's type. Null propagation is then a parent
//! walk that stops at the first position whose type is nullable, which is draft §6.4.4 read
//! literally, including its list clause: `[String!]` puts `String!` on every element slot, so one
//! null element propagates to the list, and `[String]` does not.
//!
//! # Every link points up, and draft §7.1.2 reads down
//!
//! One `parent` per slot is all the upward structure there is, and it is all §6.4.4 needs. But
//! §7.1.2's `path` is the same chain read the other way, and a tree that can only be climbed has
//! no cheap way to be descended: whoever wants the specified order has to reverse the chain, and
//! reversing it *per segment* is quadratic in the depth. [`Path`]'s own header says what this
//! module does about that, why the answer is three entries on it rather than one, and why its two
//! formatters are none of the three.

use core::fmt;

use std::collections::TryReserveError;

use smear_schema::{PackedType, TypeId};

#[cfg(all(test, feature = "std"))]
mod tests;

/// The sentinel for "no such slot".
pub(super) const NONE: u32 = u32::MAX;

/// One position in the response.
#[derive(Debug)]
pub(super) struct Slot<V> {
  pub(super) parent: u32,
  /// How many draft §7.1.2 segments the path of this position has — the root's is `0`.
  ///
  /// The chain's length, written down where the chain is *built* instead of counted where it is
  /// read. The executor creates a position only under a parent it already holds, so this is the
  /// parent's depth plus one and costs an increment; recovering it afterwards costs the depth,
  /// which is exactly the walk [`Path::collect_into`] exists to perform once. Storing it is what
  /// lets that walk size its buffer **before** filling it, with no preliminary pass to measure —
  /// see that method, and see the block below for what the field measured.
  ///
  /// Structural, so nothing rewrites it: `discard` and `reset` overwrite a slot's *state*, and a
  /// position's distance from the root is not part of it.
  pub(super) depth: u32,
  pub(super) next_sibling: u32,
  pub(super) first_child: u32,
  pub(super) last_child: u32,
  pub(super) key: Key,
  /// The type of this position, which is what draft §6.4.4's walk reads.
  pub(super) ty: PackedType,
  pub(super) state: State<V>,
  /// Set when draft §6.4.4 nulled this slot or an ancestor: the position is no longer part of the
  /// response, any request outstanding under it is abandoned, and it holds no driver value.
  ///
  /// It is set on the whole subtree and not only on the position §6.4.4 nulled, because
  /// `Executor::discard` marks and empties in the same pass. That is what lets a later discard
  /// higher up stop at this slot instead of walking into a subtree that is already empty.
  pub(super) discarded: bool,
  /// The next slot on the ready chain, or [`NONE`].
  pub(super) next_ready: u32,
}

impl<V> Slot<V> {
  #[inline]
  pub(super) const fn new(
    parent: u32,
    depth: u32,
    key: Key,
    ty: PackedType,
    state: State<V>,
  ) -> Self {
    Self {
      parent,
      depth,
      next_sibling: NONE,
      first_child: NONE,
      last_child: NONE,
      key,
      ty,
      state,
      discarded: false,
      next_ready: NONE,
    }
  }
}

/// What a slot holds.
#[derive(Debug)]
pub(super) enum State<V> {
  /// Collected, on the ready chain, not yet offered to the driver.
  Ready,
  /// Offered to the driver; a [`ReqId`](super::ReqId) refers to this slot.
  InFlight,
  /// Resolved to `null`, either because the value was null or because draft §6.4.4 put it here.
  Null,
  /// A serialised leaf, as [`Values::coerce_leaf`](super::Values::coerce_leaf) returned it, and
  /// never null — draft §6.4.3 step 4 makes a null return the field error the `None` return is.
  Leaf(V),
  /// Draft §4.4's `__typename`, as an id into the executor's name table.
  ///
  /// Complete the moment it is created: the executor knows the answer when it collects the field,
  /// so the slot never joins the ready chain and the driver is never asked. An id rather than the
  /// bytes for the reason [`Key::Field`] is one — the name repeats on every element of a list, and
  /// the table already holds it once.
  TypeName(u32),
  /// A list. The children are its elements, keyed by index.
  List,
  /// An object whose value and resolved type are both dead.
  ///
  /// Renders exactly as [`Object`](State::Object) does — from `first_child`, which is what
  /// rendering always read. Nothing ever rendered the value, and that is the whole of the finding
  /// this variant exists for: an object's `V` has one reader in the program, `poll_resolve` lending
  /// it as `parent_value`, so once the last enqueued child has been offered it is unreachable by
  /// any public API and unread by any internal path. Holding it to the next `start` pinned one
  /// driver handle per object — a database row, an FFI pointer, a wasm handle — bounded by
  /// `max_response_slots` rather than by `max_in_flight`.
  ///
  /// So the value dies when its reader set empties, and the state records that it has. The
  /// transition is the release: there is no separate call to forget.
  Expanded,
  /// An object whose value is still readable, because a child of it has yet to be offered.
  Object {
    /// The value the driver resolved, handed back as the parent value of every child request.
    value: V,
    /// How many future reads of `value` remain: children enqueued by `expand` and not yet departed
    /// from Ready.
    ///
    /// The count is *inside* the variant on purpose. A slot holding no object pays nothing for it,
    /// and — the property that matters more — a stale count is unrepresentable, because `discard`
    /// and `reset` overwrite the whole state and the number dies with the value it was counting
    /// reads of. A parallel table would have to be kept in step with two paths that already forget
    /// things for a living.
    ///
    /// Its *meaning* is "outstanding future reads", and that is what a later phase extends. Phase 6
    /// making list completion resumable adds registered-but-uncreated readers to the meaning; it
    /// does not redefine it. Carry the meaning, not the formula.
    pending: u32,
    /// The concrete object type, after `ResolveAbstractType` where the position was abstract.
    ty: TypeId,
  },
}

/// What the read counter costs, measured rather than derived.
///
/// The design this implements predicted that `pending` would pack beside the type id for an
/// eight-aligned driver value and cost nothing, growing the state only in the four-aligned case.
/// **Half of that is wrong, and measuring is how it was caught.** On this target and toolchain:
///
/// | `V` | `State<V>` before | after | `Slot<V>` before | after |
/// |---|---|---|---|---|
/// | `u64` (align 8) | 16 | **24** | 64 | **72** |
/// | `u32` (align 4) | 12 | **16** | 56 | **60** |
///
/// The four-aligned prediction held; the eight-aligned one did not, because the spare four bytes
/// the counter was expected to occupy were already carrying the discriminant. So the real cost is
/// eight bytes per slot for an eight-aligned value, not zero — about a ninth of a `Slot`, against a
/// defect that pinned one driver handle per object for the whole response. Worth it, but worth it
/// on the measured number rather than the hoped one.
///
/// Bounds rather than equalities, deliberately. `repr(Rust)` layout is unspecified, so a future
/// compiler is entitled to pack this better and a `==` would fail a green tree for an improvement.
/// A `<=` still fails the thing worth failing: a field added here that grows the slot again.
///
/// # And what [`Slot::depth`] cost, measured the same way and the other way round
///
/// The `pending` row above is the eight-aligned case paying and the four-aligned case being
/// cheap. `depth` is the inverse, and again the measurement is the only thing that says so:
///
/// | `V` | `Slot<V>` without `depth` | with |
/// |---|---|---|
/// | `u64` (align 8) | 72 | **72** |
/// | `u32` (align 4) | 60 | **64** |
///
/// Free for an eight-aligned driver value, because `State<u64>`'s alignment already rounds the
/// slot up and left seven spare bytes at the end that nothing was using; four bytes per slot for a
/// four-aligned one, where there was no such tail. So at the default 2²⁰-slot ceiling the field
/// costs between nothing and 4 MiB, depending on the driver's value — against a scratch buffer
/// that reached 16 MiB and briefly held two of them, which is what the field is here to make
/// unnecessary. Neither number was predicted; both were read off the compiler.
const _: () = {
  assert!(core::mem::size_of::<State<u64>>() <= 24);
  assert!(core::mem::size_of::<Slot<u64>>() <= 72);
  assert!(core::mem::size_of::<State<u32>>() <= 16);
  assert!(core::mem::size_of::<Slot<u32>>() <= 64);
};

/// A slot's key in its parent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Key {
  /// The root of the response. Never appears in a path.
  Root,
  /// A response key, as an id into the executor's name table.
  Field(u32),
  /// A list index.
  Index(u32),
}

/// One segment of a draft §7.1.2 response path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Segment<'r> {
  /// A field's response key — its alias when it has one, otherwise its name.
  Field(&'r str),
  /// A zero-based index into a list.
  Index(u32),
}

impl fmt::Display for Segment<'_> {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Field(name) => f.write_str(name),
      Self::Index(index) => write!(f, "{index}"),
    }
  }
}

/// How many segments a [`Path`]'s formatters turn around inside their own frame.
///
/// **Public because it is the contract rather than an implementation detail**: it is the depth up
/// to which rendering a path with `{}` or `{:?}` reaches the allocator zero times, which is
/// something a caller deciding what to log is entitled to check. Past it a formatter takes one
/// fallible buffer and can answer [`fmt::Error`].
///
/// Thirty-two [`Segment`]s is 512 bytes of automatic storage, held for the length of one
/// `write!` and by a function that cannot recurse — a fixed frame cost of the kind
/// [`itoa`](https://docs.rs/itoa)'s and `zmij`'s number buffers already are, one crate up.
///
/// Chosen against what a response path *is* rather than as a round number. A segment is a field a
/// query asked for or an index into a list the driver returned, so a path is as deep as the
/// selection set is nested plus the list nesting the schema's types declare; both of those are
/// written by hand, and neither reaches thirty-two in a schema anybody maintains. What *can* reach
/// it is a fragment chain, which is exactly the construction this module's tests use to drive the
/// depth to 1 024 — and which is the case the window deliberately does not try to serve.
pub const INLINE_SEGMENTS: usize = 32;

/// A draft §7.1.2 response path, outermost segment first.
///
/// Derived from the response tree rather than stored: an error records the slot it happened at and
/// the path is the chain of keys from the root down to it. That is not only cheaper — nothing is
/// allocated when the error is *recorded* — it is also the only way the path can be *right*. A path
/// assembled while descending would have to be copied at every field, and the copy is where
/// implementations lose a list index.
///
/// # The links point up, so the turn-around happens here and it happens once
///
/// A path is *discovered* innermost-first. A slot's `parent` is the only link along it — there is
/// no "which child leads to this slot" to follow downwards — and draft §7.1.2 wants the other order.
/// Something therefore has to reverse the chain, and the only reversal that is not quadratic is one
/// that walks it **once**, into a buffer.
///
/// That is why the surface below is three entries rather than one, and each of them is a different
/// answer to "whose buffer":
///
/// | | order | per segment | buffer | on refusal |
/// |---|---|---|---|---|
/// | [`ancestors`](Path::ancestors) | innermost first | one parent link | none | cannot refuse |
/// | [`collect_into`](Path::collect_into) | outermost first | one parent link | the caller's, reused across a whole response | [`TryReserveError`] |
/// | [`try_iter`](Path::try_iter) | outermost first | one parent link | its own, one per call | [`TryReserveError`] |
/// | [`Debug`](fmt::Debug), [`Display`](fmt::Display) | outermost first | one parent link | **[`INLINE_SEGMENTS`] of frame, and the heap only past it** | [`fmt::Error`] |
///
/// # One walk is not one allocation, and the buffer is the half a link counter cannot see
///
/// Walking the chain once says nothing about how the buffer that receives it *grows*. An
/// [`Ancestors`] has no size hint, so filling an empty `Vec` from it reallocates and copies its way
/// up in powers of two — a walk that is linear in links and still `log₂(depth)` allocations, the
/// last of which briefly holds the old buffer and the new one at once. At the default 2²⁰-slot
/// ceiling a [`Segment`] is 16 bytes and that peak is 24 MiB.
///
/// So both entries that fill a buffer reserve it **up front** against [`len`](Path::len), which is
/// a number the executor recorded on the position as it built the chain rather than one this type
/// counts — so the size is known without a preliminary walk to measure it. Filling a fresh buffer
/// is then one allocation of exactly the right size at every depth, which is the number the gate
/// in this module's tests asserts.
///
/// # And a formatter does not need a buffer at all, which is the part that was got wrong
///
/// The two entries above are for a caller assembling something. [`Debug`](fmt::Debug) and
/// [`Display`](fmt::Display) are not assembling anything — they already hold the sink they are
/// reversing the chain *into*, so the only thing a `Vec` bought them was somewhere to put the
/// segments while the order was turned around. That is [`INLINE_SEGMENTS`] segments of this type's
/// own frame for every path a service actually produces, and **no allocator call at all**: a
/// depth-three path used to cost a `malloc` and a `free` to print twenty bytes.
///
/// Past the window there is no free reversal left. A chain in a shared slice cannot be turned
/// around in constant space without walking it once per output segment, and *that* is the
/// quadratic this whole section exists to remove — trading the heap for it would move the cost onto
/// the one axis with no refusal at all, since an allocator can say no and a spent second cannot. So
/// a path deeper than the window takes [`collect_into`](Path::collect_into), the same fallible door
/// a serialiser takes, and a refusal arrives as [`fmt::Error`] rather than as the process-wide
/// allocation handler.
///
/// **This was found as a dismissal and not as a defect**, which is worth recording where the next
/// reader will meet it: the allocation below was enumerated, and then crossed off because the JSON
/// writer one crate up reaches paths through `collect_into`. It is off *that* caller's path and it
/// is public, and `Debug`/`Display` are how anything else reaches a path — so the criterion was
/// about the wrong caller, and a `log::warn!` carrying a request-shaped path was enough to abort
/// the process. A dismissal by entry point has to name the *widest* entry point.
///
/// **There is deliberately no random accessor.** `get(i)` over upward links costs the depth per
/// call, so the `0..len()` loop every caller eventually writes over it is exactly the quadratic
/// this section exists to remove — and an accessor whose only non-quadratic use is a single call is
/// not an accessor. It existed, unused, and its shape was the shape the defect took.
pub struct Path<'r, V> {
  slots: &'r [Slot<V>],
  names: &'r [u8],
  name_spans: &'r [(u32, u32)],
  slot: u32,
}

// Written out rather than derived, for the reason [`Children`]'s is: a derive would demand
// `V: Clone` for a type that holds no `V`, only a shared slice of slots that happen to contain
// some. The driver's value is never cloned here and never could be.
impl<V> Clone for Path<'_, V> {
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

impl<V> Copy for Path<'_, V> {}

impl<'r, V> Path<'r, V> {
  /// The path of the position at `slot`.
  #[inline]
  pub(super) const fn new(
    slots: &'r [Slot<V>],
    names: &'r [u8],
    name_spans: &'r [(u32, u32)],
    slot: u32,
  ) -> Self {
    Self {
      slots,
      names,
      name_spans,
      slot,
    }
  }

  /// Returns the number of segments.
  ///
  /// One slot and no buffer, whatever the depth. The executor wrote the answer onto the position
  /// as it created it, so this reads a number rather than counting a chain — and that is what lets
  /// [`collect_into`](Path::collect_into) size its buffer exactly without a pass to measure it
  /// first.
  pub fn len(&self) -> usize {
    if self.slot == NONE {
      return 0;
    }
    self.at(self.slot).depth as usize
  }

  /// Returns whether the path is empty, which happens only at the root.
  ///
  /// One slot, whatever the depth — the root is the only position with no segment, so the question
  /// is about this position and not about the chain above it.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.slot == NONE || matches!(self.at(self.slot).key, Key::Root)
  }

  /// Returns the segments, innermost first — the direction the tree's links point.
  ///
  /// One parent link per step and nothing allocated, which is what makes this the primitive the
  /// other two are written over. A caller writing draft §7.1.2's `path` wants one of those, since
  /// this is the reverse of the order §7.1.2 specifies; a caller *asking* something of the path —
  /// which field this error is under, how deep it is — wants this one, and stops as soon as it has
  /// the answer.
  #[inline]
  pub fn ancestors(&self) -> Ancestors<'r, V> {
    Ancestors {
      path: *self,
      cursor: self.slot,
    }
  }

  /// Collects the segments into `buf`, outermost first, and returns them.
  ///
  /// `buf` is cleared first and stays the caller's, which is the point: one buffer serves every
  /// error in a response, so a serialiser writing draft §7.1.2 pays one allocation for the whole
  /// response rather than one per path — and pays it once across responses if it keeps the buffer.
  ///
  /// The room is reserved from [`len`](Path::len) before the chain is walked, so an empty buffer
  /// takes **one** allocation of exactly the right size at any depth, and a buffer that has
  /// already held a path this long takes none — instead of the `log₂(depth)` reallocations that
  /// filling an unhinted `Vec` costs, the last of which holds the old buffer and the new one at
  /// once. See the type's header for what that peaked at.
  ///
  /// # `try_reserve` and not `try_reserve_exact`, because the buffer is *shared*
  ///
  /// Worth writing down, because "reserve exactly what this path needs" is the obvious reading and
  /// it is wrong here for the reason the first sentence of this method gives: `buf` serves every
  /// error in a response, so a reservation is almost never the last one made on it. `std` says the
  /// same thing about the choice — "prefer `try_reserve` if future insertions are expected" — and
  /// the case that decides it is a response whose error depths *increase*, which a fragment chain
  /// failing a leaf at every level produces. Exact growth reallocates and recopies for each of
  /// them, which is a quadratic in the depth over a shared buffer; amortised growth costs
  /// `log₂(depth)` for the whole response. Where the finding that prompted this actually bites —
  /// one deep path into a fresh buffer — the two are identical, because doubling from nothing
  /// lands on exactly the requested capacity.
  ///
  /// # Errors
  ///
  /// [`TryReserveError`] when the allocator cannot give the buffer room for a path this deep.
  /// **Fallible rather than aborting**, and the reason is what this crate is: the depth is chosen
  /// by whoever wrote the query and by however many list elements the driver returned, both of
  /// which a ceiling bounds and neither of which a server should die of. A caller that has already
  /// written part of a response gets to answer it. `buf` is left empty.
  pub fn collect_into<'b>(
    &self,
    buf: &'b mut std::vec::Vec<Segment<'r>>,
  ) -> Result<&'b [Segment<'r>], TryReserveError> {
    buf.clear();
    buf.try_reserve(self.len())?;
    Ok(self.fill(buf))
  }

  /// Returns the segments, outermost first, in a buffer the iterator owns.
  ///
  /// The chain is walked once, into a buffer reserved up front from [`len`](Path::len) — see the
  /// type's header for why there is a buffer at all. A caller with more than one path to write
  /// wants [`collect_into`](Path::collect_into), which lends its own instead of allocating one per
  /// call.
  ///
  /// # Errors
  ///
  /// [`TryReserveError`] when the allocator cannot give the buffer room for a path this deep, for
  /// the reason [`collect_into`](Path::collect_into) is fallible: the depth is the client's and a
  /// server should not die of it. **There is deliberately no infallible spelling of this.** There
  /// was, it took `Vec::with_capacity`, and it was left infallible on the argument that the two
  /// formatters below had nowhere to put a refusal — which was true of them and said nothing about
  /// this method, whose caller has a `Result` to receive one in. The formatters no longer come
  /// through here at all.
  pub fn try_iter(&self) -> Result<PathIter<'r>, TryReserveError> {
    let mut segments = std::vec::Vec::new();
    segments.try_reserve(self.len())?;
    self.fill(&mut segments);
    Ok(PathIter {
      segments: segments.into_iter(),
    })
  }

  /// Hands every segment to `emit` in draft §7.1.2's order.
  ///
  /// The turn-around happens in [`INLINE_SEGMENTS`] segments of this frame, so a path that fits the
  /// window reaches the allocator zero times. One deeper falls through to
  /// [`each_segment_buffered`](Path::each_segment_buffered), which is one call and not a cycle —
  /// nothing here recurses, at any depth.
  ///
  /// `&mut dyn FnMut` rather than a generic, because the two callers are the two formatters and one
  /// copy of this walk is the point.
  fn each_segment(&self, emit: &mut dyn FnMut(Segment<'r>) -> fmt::Result) -> fmt::Result {
    let mut window = [Segment::Index(0); INLINE_SEGMENTS];
    let mut filled = 0;
    // Bounded by the window and not by `len`, deliberately: the strategy may be chosen from a
    // recorded number, and what is *written* may not be. An overflowing chain takes the branch
    // below rather than being truncated to whatever the slot said.
    for segment in self.ancestors() {
      if filled == INLINE_SEGMENTS {
        return self.each_segment_buffered(emit);
      }
      window[filled] = segment;
      filled += 1;
    }
    for segment in window[..filled].iter().rev() {
      emit(*segment)?;
    }
    Ok(())
  }

  /// The same, for a path too deep to turn around in a frame.
  ///
  /// Re-walks from the leaf, which costs the [`INLINE_SEGMENTS`] links the window had already
  /// followed and nothing else — the alternative is threading a half-consumed
  /// [`Ancestors`] through both branches to save a constant.
  #[cold]
  fn each_segment_buffered(&self, emit: &mut dyn FnMut(Segment<'r>) -> fmt::Result) -> fmt::Result {
    let mut buf = std::vec::Vec::new();
    for segment in self.collect_into(&mut buf).map_err(|_| fmt::Error)? {
      emit(*segment)?;
    }
    Ok(())
  }

  /// Walks the chain into `buf` and turns it around.
  ///
  /// `buf` must be empty and must already have room for [`len`](Path::len) segments; both callers
  /// above make it so, and the reservation is theirs precisely because they answer an allocation
  /// failure differently.
  fn fill<'b>(&self, buf: &'b mut std::vec::Vec<Segment<'r>>) -> &'b [Segment<'r>] {
    buf.extend(self.ancestors());
    buf.reverse();
    buf
  }

  /// Reads one slot, and records the traversal.
  ///
  /// **The only way this file reaches a slot while deriving a path**, which is what makes
  /// `traversals` a measurement of the derivation rather than of whichever walk someone remembered
  /// to instrument. The field it reads is private to this module for the same reason.
  #[inline]
  fn at(&self, index: u32) -> &'r Slot<V> {
    traverse();
    &self.slots[index as usize]
  }

  /// Turns one position's key into its segment. The root has none.
  #[inline]
  fn segment(&self, key: Key) -> Option<Segment<'r>> {
    match key {
      Key::Root => None,
      Key::Index(index) => Some(Segment::Index(index)),
      Key::Field(name) => Some(Segment::Field(interned(self.names, self.name_spans, name))),
    }
  }
}

impl<V> fmt::Debug for Path<'_, V> {
  /// Allocates nothing for a path of [`INLINE_SEGMENTS`] segments or fewer, and answers a deeper
  /// one the allocator refuses with [`fmt::Error`] rather than aborting. See [`Path`]'s header.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut list = f.debug_list();
    self.each_segment(&mut |segment| {
      list.entry(&segment);
      Ok(())
    })?;
    list.finish()
  }
}

impl<V> fmt::Display for Path<'_, V> {
  /// Allocates nothing for a path of [`INLINE_SEGMENTS`] segments or fewer, and answers a deeper
  /// one the allocator refuses with [`fmt::Error`] rather than aborting. See [`Path`]'s header.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut first = true;
    self.each_segment(&mut |segment| {
      if !first {
        f.write_str(".")?;
      }
      first = false;
      write!(f, "{segment}")
    })
  }
}

/// The segments of a [`Path`], innermost first.
///
/// One `parent` link per step, which is the whole of its cost: the chain is walked forwards and
/// never restarted, so consuming the iterator costs the depth and not its square.
pub struct Ancestors<'r, V> {
  path: Path<'r, V>,
  /// The position whose key is the next segment, or [`NONE`] once the root has been reached.
  cursor: u32,
}

impl<V> Clone for Ancestors<'_, V> {
  #[inline]
  fn clone(&self) -> Self {
    Self {
      path: self.path,
      cursor: self.cursor,
    }
  }
}

impl<'r, V> Iterator for Ancestors<'r, V> {
  type Item = Segment<'r>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.cursor == NONE {
      return None;
    }
    let slot = self.path.at(self.cursor);
    // The root carries no segment and is where the chain ends, so it retires the cursor rather
    // than stepping it — which is also what makes this iterator fused.
    self.cursor = match slot.key {
      Key::Root => NONE,
      _ => slot.parent,
    };
    self.path.segment(slot.key)
  }
}

impl<V> core::iter::FusedIterator for Ancestors<'_, V> {}

/// The segments of a [`Path`], outermost first.
///
/// Backed by the buffer [`Path::try_iter`] filled with one leaf-to-root walk. It is not a lazy view
/// of the tree, and it cannot be: the links run the other way, so an iterator that held only a
/// cursor would have to restart at the failing position for every segment it yielded. That is also
/// why the buffer is a `Vec` and not the window [`Path`]'s formatters use — an iterator outlives
/// the call that made it, so its storage cannot be a frame.
pub struct PathIter<'r> {
  segments: std::vec::IntoIter<Segment<'r>>,
}

impl<'r> Iterator for PathIter<'r> {
  type Item = Segment<'r>;

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self.segments.next()
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.segments.size_hint()
  }
}

impl DoubleEndedIterator for PathIter<'_> {
  #[inline]
  fn next_back(&mut self) -> Option<Self::Item> {
    self.segments.next_back()
  }
}

impl ExactSizeIterator for PathIter<'_> {}

impl core::iter::FusedIterator for PathIter<'_> {}

// ---------------------------------------------------------------------------------------------
// what deriving a path costs — TWO axes, and neither one implies the other
// ---------------------------------------------------------------------------------------------

// How many response slots a path derivation has followed, and how many times it went to the
// allocator. Both counted only when this crate's own tests are built.
//
// ── WHY SLOTS ─────────────────────────────────────────────────────────────────────────────────
//
// A derivation that restarts at the failing position for every segment costs the depth squared in
// LINKS and nothing extra anywhere else: it fills the same buffer with the same segments and emits
// the same O(depth) bytes. So neither an allocation pin nor a byte counter can see it, and only
// the links themselves can. A slot reached twice counts twice, because that is precisely the
// defect.
//
// `smear`'s JSON writer counts document bytes for the same reason and by the same rule; this is
// that instrument, one crate down, where the traversal actually happens.
//
// ── AND WHY ALLOCATIONS, BESIDE IT RATHER THAN INSTEAD OF IT ──────────────────────────────────
//
// Because the link counter is blind in exactly the direction the byte counter was. Filling an
// unhinted `Vec` from `Ancestors` walks each link ONCE — a perfect reading on the axis above —
// while reallocating and copying its way up in powers of two, with the old buffer and the new one
// both live at the peak. The link counter cannot see a reallocation; the byte counter cannot see
// it either, because the emitted document is the same. Only the allocator can.
//
// This branch's lineage has now met that shape three times, from both directions:
//
//   | round   | the gate measured | it was blind to             |
//   |---------|-------------------|-----------------------------|
//   | #157 R3 | allocations       | a walk of the document      |
//   | #160 R5 | allocations       | the bytes a walk added      |
//   | #162 R3 | traversal steps   | reallocation and copying    |
//
// The last one is the first two INVERTED — there an allocation pin could not see a byte walk, here
// a traversal pin could not see an allocation — and the conclusion is the same in both directions:
// **a cost gate needs every axis its subject can grow along, and no axis implies another.** Anyone
// adding a cost gate to this file should reach for both of these, and ask what a third one would
// be before deciding two is enough.
//
// ── `std`, WHILE THE REPAIR IS NOT ────────────────────────────────────────────────────────────
//
// A counter shared by parallel tests has to be thread-local, and `thread_local!` is `std`'s; a
// counting allocator is `std::alloc::System` plus a `#[global_allocator]`, which is `std`'s too.
// So `cargo test -p graphql-proto --no-default-features` compiles the exactly-reserved derivation
// without the gates that measure it; every other test configuration has both.
#[cfg(all(test, feature = "std"))]
thread_local! {
  /// Slots a path derivation has followed on this thread.
  static TRAVERSED: core::cell::Cell<u64> = const { core::cell::Cell::new(0) };
  /// Allocating events on this thread, whoever made them.
  static ALLOCATED: core::cell::Cell<u64> = const { core::cell::Cell::new(0) };
  /// The size at and above which [`Counting`] answers a request on this thread with null.
  ///
  /// Thread-local and not a global, because the harness runs these tests in parallel and a global
  /// switch would refuse whatever a neighbour happened to be doing.
  static REFUSE_AT_LEAST: core::cell::Cell<usize> = const { core::cell::Cell::new(usize::MAX) };
}

/// Records that a path derivation followed one slot.
///
/// Compiles to nothing outside this crate's own tests.
#[inline]
fn traverse() {
  #[cfg(all(test, feature = "std"))]
  TRAVERSED.with(|traversed| traversed.set(traversed.get() + 1));
}

/// Returns how many slots path derivation has followed on this thread.
#[cfg(all(test, feature = "std"))]
fn traversals() -> u64 {
  TRAVERSED.with(core::cell::Cell::get)
}

/// A pass-through global allocator that tallies every allocating event on the calling thread.
///
/// The shape `smear`, `smear-parser` and `smear-schema` each use in an integration target of their
/// own; this is the first one that has to live in a crate's *library* tests, because the property
/// it serves is measured beside [`traversals`] and that counter is private to this module. There
/// is no shared one to import — a `#[global_allocator]` belongs to a binary, and an integration
/// target's is not reachable from a lib test.
///
/// `realloc` counts, and counts as one event. That is not incidental: geometric growth is mostly
/// `realloc`, so an allocator that forwarded it silently would report the defect this instrument
/// exists for as a single allocation.
///
/// # And it can say no, which is the only way to read the other half of the claim
///
/// A count cannot tell a `try_reserve` from a `with_capacity`: both are one event, right up to the
/// point where one returns an error and the other calls the process-wide allocation handler. So
/// [`refuse_allocations_of_at_least`] arms a size threshold on the calling thread and every request
/// at or above it is answered with null, which is what an allocator does when it has nothing left.
/// A door that is fallible answers; a door that is not aborts the test binary, loudly.
///
/// **Nothing may panic while the threshold is armed.** Rust's panic machinery allocates, so a
/// panic inside the window meets a refusal inside the code handling the first one. Every fixture
/// here arms immediately before the call under test, disarms immediately after, and asserts
/// afterwards.
#[cfg(all(test, feature = "std"))]
struct Counting;

#[cfg(all(test, feature = "std"))]
// SAFETY: every method forwards to `System` with the layout it was given, unchanged, and returns
// exactly what `System` returned — or, when this thread has armed a refusal at or below the
// requested size, returns null without calling `System` at all, which `GlobalAlloc` defines as the
// answer for a request that cannot be served. The tally and the threshold are `Cell`s in
// thread-local storage, reached through `try_with` so that an allocation made while that storage is
// being set up or torn down is left uncounted and unrefused rather than re-entering it.
unsafe impl core::alloc::GlobalAlloc for Counting {
  unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
    allocate();
    if refused(layout.size()) {
      return core::ptr::null_mut();
    }
    unsafe { std::alloc::System.alloc(layout) }
  }

  unsafe fn alloc_zeroed(&self, layout: core::alloc::Layout) -> *mut u8 {
    allocate();
    if refused(layout.size()) {
      return core::ptr::null_mut();
    }
    unsafe { std::alloc::System.alloc_zeroed(layout) }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: core::alloc::Layout, new_size: usize) -> *mut u8 {
    allocate();
    if refused(new_size) {
      return core::ptr::null_mut();
    }
    unsafe { std::alloc::System.realloc(ptr, layout, new_size) }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: core::alloc::Layout) {
    unsafe { std::alloc::System.dealloc(ptr, layout) }
  }
}

#[cfg(all(test, feature = "std"))]
#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// Records one allocating event.
#[cfg(all(test, feature = "std"))]
fn allocate() {
  let _ = ALLOCATED.try_with(|allocated| allocated.set(allocated.get() + 1));
}

/// Returns how many allocating events this thread has caused.
#[cfg(all(test, feature = "std"))]
fn allocations() -> u64 {
  ALLOCATED.with(core::cell::Cell::get)
}

/// Answers whether this thread has armed a refusal that covers a request of `size`.
#[cfg(all(test, feature = "std"))]
fn refused(size: usize) -> bool {
  REFUSE_AT_LEAST
    .try_with(|threshold| size >= threshold.get())
    .unwrap_or(false)
}

/// Makes every allocation request of `size` bytes or more on this thread answer with null.
///
/// A size threshold rather than a switch, because the window has to be wide enough to include the
/// buffer under test and narrow enough to exclude the harness's own housekeeping.
#[cfg(all(test, feature = "std"))]
fn refuse_allocations_of_at_least(size: usize) {
  REFUSE_AT_LEAST.with(|threshold| threshold.set(size));
}

/// Puts this thread's allocator back.
#[cfg(all(test, feature = "std"))]
fn allow_allocations() {
  REFUSE_AT_LEAST.with(|threshold| threshold.set(usize::MAX));
}

/// A completed value in the response.
///
/// The shape is `proto`'s and the leaves are the driver's, so a leaf arrives as `&V` — exactly the
/// value [`Values::coerce_leaf`](super::Values::coerce_leaf) returned — and serialising it is the
/// driver's job, as it must be.
///
/// A [`Node::Leaf`] never holds a null. Draft §6.4.3 step 4 makes a null serialised leaf a field
/// error, so the position becomes [`Node::Null`] with an entry in `errors` and the value is not
/// kept; a null in the response is therefore always `proto`'s conclusion and never a leaf a
/// serialiser has to second-guess. Which matters most where the position is non-null: `Leaf` in a
/// `String!` position is a value, so a serialiser need not re-check the schema to know its output
/// will satisfy it.
///
/// [`Node::TypeName`] is the one value that does not arrive as a `&V`, because the driver did not
/// produce it: draft §4.4's `__typename` is answered by the executor from the object type it
/// resolved. A serialiser writes it as a GraphQL `String`, the same way it writes a
/// [`Segment::Field`] key.
pub enum Node<'r, V> {
  /// The `null` literal.
  Null,
  /// A serialised leaf.
  Leaf(&'r V),
  /// Draft §4.4's `__typename`: the concrete object type's name, as the schema spells it.
  TypeName(&'r str),
  /// A list, in order.
  List(Children<'r, V>),
  /// An object, in the order draft §6.3 collected its fields.
  Object(Children<'r, V>),
}

impl<V> fmt::Debug for Node<'_, V> {
  /// **One level, and the level is the keys.** A container renders as its own children's keys and
  /// stops there, so this walks siblings and never descends.
  ///
  /// # Why not the whole tree, which is what this used to render
  ///
  /// Because it recursed to do it, one frame per level of a response whose depth is the client's:
  /// measured at **1 088 bytes per level** on this host, and `{:?}` on a 4 000-deep response
  /// overflowed a test thread's stack and took the process with it. It is the same defect
  /// [`Path`]'s formatters had, met on the other fatal axis — the writer's `data` walk one crate up
  /// was moved onto an explicit stack for exactly this reason, and a `Debug` that recurses through
  /// the same tree hands the depth straight back to whoever logs a response.
  ///
  /// So the shape is the fix rather than a stack: a formatter that descends needs somewhere to keep
  /// the un-taken siblings, and the only places are the native stack (fatal) and the heap (an
  /// allocation, in a formatter, sized by the client). Not descending needs neither.
  ///
  /// **And nothing is lost that this impl was ever able to give.** It already refuses to render a
  /// value — [`Node::Leaf`] prints as `<leaf>`, because `V` carries no [`Debug`](fmt::Debug) bound
  /// anywhere a driver value is reached — so it was never a rendering of the response. Whatever
  /// wants that wants `smear`'s draft §7.2.1 JSON writer, which is written for it, runs on an
  /// explicit stack and can refuse.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Null => f.write_str("null"),
      Self::Leaf(_) => f.write_str("<leaf>"),
      Self::TypeName(name) => write!(f, "{name:?}"),
      // `Children`'s own `Debug` is the keys, so the two arms differ by the name in front of them —
      // which is the one thing the keys alone do not say.
      Self::List(children) => f.debug_tuple("List").field(children).finish(),
      Self::Object(children) => f.debug_tuple("Object").field(children).finish(),
    }
  }
}

/// The children of a list or an object, in response order.
pub struct Children<'r, V> {
  slots: &'r [Slot<V>],
  names: &'r [u8],
  name_spans: &'r [(u32, u32)],
  next: u32,
}

impl<V> Clone for Children<'_, V> {
  #[inline]
  fn clone(&self) -> Self {
    Self {
      slots: self.slots,
      names: self.names,
      name_spans: self.name_spans,
      next: self.next,
    }
  }
}

impl<'r, V> Iterator for Children<'r, V> {
  /// The child's key and value. The key is [`Segment::Index`] for a list element.
  type Item = (Segment<'r>, Node<'r, V>);

  fn next(&mut self) -> Option<Self::Item> {
    let index = self.next;
    if index == NONE {
      return None;
    }
    let slot = &self.slots[index as usize];
    self.next = slot.next_sibling;
    let key = match slot.key {
      Key::Root => return None,
      Key::Index(i) => Segment::Index(i),
      Key::Field(name) => Segment::Field(interned(self.names, self.name_spans, name)),
    };
    Some((key, node(self.slots, self.names, self.name_spans, index)))
  }
}

impl<V> fmt::Debug for Children<'_, V> {
  /// The keys, and not the values under them — which is what [`Node`]'s own `Debug` is written
  /// over, and why that one does not descend.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list()
      .entries(self.clone().map(|(key, _)| key))
      .finish()
  }
}

/// Returns one entry of the executor's name table.
///
/// GraphQL names are `[_A-Za-z][_0-9A-Za-z]*`, established by the lexer before any of this can
/// run, so the slice is ASCII. The fallback is unreachable and costs nothing.
#[inline]
fn interned<'r>(names: &'r [u8], name_spans: &[(u32, u32)], id: u32) -> &'r str {
  let (start, len) = name_spans[id as usize];
  core::str::from_utf8(&names[start as usize..(start + len) as usize]).unwrap_or("")
}

/// Renders the slot at `index` as a [`Node`].
pub(super) fn node<'r, V>(
  slots: &'r [Slot<V>],
  names: &'r [u8],
  name_spans: &'r [(u32, u32)],
  index: u32,
) -> Node<'r, V> {
  let slot = &slots[index as usize];
  // A slot draft §6.4.4 discarded reads as `null` whatever it used to hold. The flag is the whole
  // of the rewrite: the subtree below keeps its links and its keys, and this one branch is what
  // stops any of it from being read. What the executor *does* walk a discarded subtree for is
  // release — see `Executor::discard` — and that changes nothing here, because this has already
  // answered `null` before reaching a slot the walk emptied.
  if slot.discarded {
    return Node::Null;
  }
  match &slot.state {
    // `Ready` and `InFlight` are unreachable in a finished response: `poll_response` only yields
    // once nothing is outstanding. Rendering them as null keeps the function total for the
    // `Debug` impls, which may run mid-flight.
    State::Ready | State::InFlight | State::Null => Node::Null,
    State::Leaf(value) => Node::Leaf(value),
    State::TypeName(name) => Node::TypeName(interned(names, name_spans, *name)),
    State::List => Node::List(Children {
      slots,
      names,
      name_spans,
      next: slot.first_child,
    }),
    // Both object states render the same way, because rendering never read the value or the
    // resolved type — only `first_child`. `Expanded` is that fact made into a state.
    State::Object { .. } | State::Expanded => Node::Object(Children {
      slots,
      names,
      name_spans,
      next: slot.first_child,
    }),
  }
}
