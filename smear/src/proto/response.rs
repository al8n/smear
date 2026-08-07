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
//! it does *not* mean. The tree itself grows with the response — a service that returns a million
//! list elements gets a million slots — because that is the answer, not a buffer between a
//! producer and a consumer. What `proto` refuses to do is accumulate work the driver has not asked
//! for: `poll_resolve` withholds at the in-flight ceiling rather than reading ahead.
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

use core::fmt;

use crate::validator::schema::{PackedType, TypeId};

/// The sentinel for "no such slot".
pub(super) const NONE: u32 = u32::MAX;

/// One position in the response.
#[derive(Debug)]
pub(super) struct Slot<V> {
  pub(super) parent: u32,
  pub(super) next_sibling: u32,
  pub(super) first_child: u32,
  pub(super) last_child: u32,
  pub(super) key: Key,
  /// The type of this position, which is what draft §6.4.4's walk reads.
  pub(super) ty: PackedType,
  pub(super) state: State<V>,
  /// Set when draft §6.4.4 nulled this slot or an ancestor: the subtree is no longer part of the
  /// response, and any request outstanding under it is abandoned.
  pub(super) discarded: bool,
  /// The next slot on the ready chain, or [`NONE`].
  pub(super) next_ready: u32,
}

impl<V> Slot<V> {
  #[inline]
  pub(super) const fn new(parent: u32, key: Key, ty: PackedType, state: State<V>) -> Self {
    Self {
      parent,
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
  /// An object. The children are the fields draft §6.3 collected on it.
  Object {
    /// The value the driver resolved, handed back as the parent value of every child request.
    value: V,
    /// The concrete object type, after `ResolveAbstractType` where the position was abstract.
    ty: TypeId,
  },
}

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

/// A draft §7.1.2 response path, outermost segment first.
///
/// Derived from the response tree rather than stored: an error records the slot it happened at and
/// the path is the chain of keys from the root down to it. That is not only cheaper — nothing is
/// allocated per error — it is also the only way the path can be *right*. A path assembled while
/// descending would have to be copied at every field, and the copy is where implementations lose a
/// list index.
#[derive(Clone)]
pub struct Path<'r, V> {
  pub(super) slots: &'r [Slot<V>],
  pub(super) names: &'r [u8],
  pub(super) name_spans: &'r [(u32, u32)],
  pub(super) slot: u32,
}

impl<V> Path<'_, V> {
  /// Returns the number of segments.
  pub fn len(&self) -> usize {
    self.iter().count()
  }

  /// Returns whether the path is empty, which happens only at the root.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.iter().next().is_none()
  }

  /// Returns the segments, outermost first.
  ///
  /// Collected into the caller's buffer rather than returned as a lazy iterator over a reversed
  /// walk, because the tree links point *up*: a path is discovered innermost-first and has to be
  /// turned around. Doing that once here beats every caller doing it.
  pub fn iter(&self) -> PathIter<'_, V> {
    let mut depth = 0;
    let mut cursor = self.slot;
    while cursor != NONE && !matches!(self.slots[cursor as usize].key, Key::Root) {
      depth += 1;
      cursor = self.slots[cursor as usize].parent;
    }
    PathIter {
      path: self,
      depth,
      next: 0,
    }
  }

  /// Returns the segment at `index`, counted from the outermost.
  pub fn get(&self, index: usize) -> Option<Segment<'_>> {
    let mut stack = self.slot;
    let mut depth = 0usize;
    while stack != NONE && !matches!(self.slots[stack as usize].key, Key::Root) {
      depth += 1;
      stack = self.slots[stack as usize].parent;
    }
    if index >= depth {
      return None;
    }
    // Walk up `depth - 1 - index` times to land on the requested segment.
    let mut cursor = self.slot;
    for _ in 0..(depth - 1 - index) {
      cursor = self.slots[cursor as usize].parent;
    }
    self.segment(cursor)
  }

  fn segment(&self, slot: u32) -> Option<Segment<'_>> {
    match self.slots[slot as usize].key {
      Key::Root => None,
      Key::Index(index) => Some(Segment::Index(index)),
      Key::Field(name) => Some(Segment::Field(interned(self.names, self.name_spans, name))),
    }
  }
}

impl<V> fmt::Debug for Path<'_, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

impl<V> fmt::Display for Path<'_, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut first = true;
    for segment in self.iter() {
      if !first {
        f.write_str(".")?;
      }
      first = false;
      write!(f, "{segment}")?;
    }
    Ok(())
  }
}

/// The segments of a [`Path`], outermost first.
pub struct PathIter<'r, V> {
  path: &'r Path<'r, V>,
  depth: usize,
  next: usize,
}

impl<'r, V> Iterator for PathIter<'r, V> {
  type Item = Segment<'r>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.next >= self.depth {
      return None;
    }
    let mut cursor = self.path.slot;
    for _ in 0..(self.depth - 1 - self.next) {
      cursor = self.path.slots[cursor as usize].parent;
    }
    self.next += 1;
    self.path.segment(cursor)
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    let remaining = self.depth - self.next;
    (remaining, Some(remaining))
  }
}

impl<V> ExactSizeIterator for PathIter<'_, V> {}

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
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Null => f.write_str("null"),
      Self::Leaf(_) => f.write_str("<leaf>"),
      Self::TypeName(name) => write!(f, "{name:?}"),
      Self::List(children) => f.debug_list().entries(children.clone()).finish(),
      Self::Object(children) => f.debug_list().entries(children.clone()).finish(),
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
  // A slot draft §6.4.4 discarded reads as `null` whatever it used to hold. Nothing rewrites the
  // subtree — the flag is the rewrite — which is what keeps propagation O(depth) instead of
  // O(subtree).
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
    State::Object { .. } => Node::Object(Children {
      slots,
      names,
      name_spans,
      next: slot.first_child,
    }),
  }
}
