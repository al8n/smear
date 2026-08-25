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
//! **Every recursive position the value grammar forms sits behind a container** — a list's
//! elements, a set's members, an object's fields, a map's entries. That is what gives the release a
//! type to hang a `Drop` on, and a `Vec` it can take by [`core::mem::take`] with nothing to put
//! back. That sentence has to name the grammar, and the next section is why.
//!
//! The *type* grammar does not have that property, which is why this module exports two shapes
//! rather than one. `graphql::ast::Type` nests through a single owned `Box`, and `graphqlx`'s
//! nests through three of them plus a generic-argument container; [`Nest`] covers the owned-pointer
//! positions and [`Nested`] covers the container ones, over one worklist and one [`Nestable`].
//!
//! A recursion through a *single* owned value has no such door, and [`Nest`] is the answer to it:
//! `ty::Type`'s `List(Box<ListType<Self>>)` is the shape, there is no container in that cycle, and
//! a `Drop` written on the `Type` enum could not be written at all. Not merely at the price of
//! `E0509` — it is *unexpressible in safe code*, and the reason is worth stating because it is the
//! same fact from the other side. `Drop::drop` is handed `&mut self`, and unlinking that chain
//! means taking the `Box` out of the `List` arm; taking anything out of a `&mut` place requires a
//! value to leave behind, and every `Type<Name>` needs either a `Name` or another `Type` to build.
//! There is nothing to put back. A `Vec` has `core::mem::take`; a `Box` has no equivalent, which is
//! what the sentence above means by *door*.
//!
//! So the single-child cycle gets a container-shaped thing of its own. [`Nest`] holds
//! `Option<P>`, which is a slot [`Option::take`] empties for free, and it carries the same
//! relocated [`Drop`] for exactly the same reason: the enum keeps every derive, every by-value
//! `unwrap_*`, and an `IntoSpan` that still matches itself apart.
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
//! ## The same charge on [`Nest`], measured in both profiles rather than one
//!
//! The pointer wrapper adds a forwarding frame to the same chosen calls, and on
//! `graphql::ast::Type` it was measured instead of inferred: one child process per depth on an
//! 8 MiB thread, `aarch64-apple-darwin`, bisected to the last level at which the child still
//! returned, three runs a cell and every cell stable to the level.
//!
//! | | base, debug | here, debug | base, release | here, release |
//! |---|---|---|---|---|
//! | `Type::clone` | 16 932 | 15 906 (**-6%**) | 131 253 | 131 253 (**0**) |
//! | `Type == Type` | 34 994 | 29 162 (**-17%**) | > 67 108 864 | > 67 108 864 (**0**) |
//! | `Type` `{:?}` | 12 495 | 12 495 (**0**) | 15 438 | 15 438 (**0**) |
//!
//! **In release there is no charge at all, on any of the three.** The forwarding impls are inlined
//! away, and `==` is not bounded by a stack in either build: 67 108 864 levels returned on both
//! sides, which 8 MiB cannot hold at a *tenth* of a byte per level, so that recursion is not on
//! the stack at all once the optimiser has it. The percentages above are a debug-build charge and
//! nothing else, and they are what a `cargo test` run sees.
//!
//! What narrowed all three is `#[inline(always)]` rather than `#[inline]` on [`Nest`]'s forwarding
//! `clone`, `fmt`, `eq`, `hash`, `as_ref` and `deref`: rustc's MIR inliner honours it at
//! opt-level 0, where a plain `#[inline]` is a hint to a pass that is not running. Without it the
//! debug column reads 14 581 / 27 627 / 12 204. The `clone` row then took a second frame off by
//! writing [`Option::clone`] out as the match it is — 14 997 to 15 906 — for the reason that impl
//! records.
//!
//! **[`Nest::get`] is deliberately not `#[inline(always)]`, and that is a measurement rather than
//! an oversight.** It is the one of the seven with a branch and an `unreachable!` in its body, so
//! inlining it does not remove a frame — it grows every frame that is itself paid once per level.
//! With `inline(always)` on `get` as well, `==` fell to 24 996 and `{:?}` to 11 927: worse than
//! this table, and worse than doing nothing at all.
//!
//! ### Why only `clone` got the second repair
//!
//! The question was put to all six forwarding impls and the answer is that **`clone` is the only
//! one whose `Option` access costs anything.** `Option::clone` builds its `Some` *around* the
//! inner clone, so its frame is live for the whole recursion beneath it — one per level. `fmt`,
//! `eq`, `hash`, `as_ref` and `deref` reach the pointee through [`Nest::get`], which hands back a
//! reference and returns; its frame and `Option::as_ref`'s are both popped *before* the recursive
//! call is made, so neither is on the stack per level. That the `{:?}` row already sits exactly on
//! its base is the same fact read off the table.
//!
//! It was tested rather than argued. Rewriting `eq`, `fmt` and `hash` to match `self.inner`
//! directly — no `get`, no `Option` method — made both measurable rows **worse**: `==` fell to
//! 26 245 and `{:?}` to 12 204. The extra match arms grow the frame the derived impl pays once per
//! level, and they buy back a frame that was never per-level to begin with. It is the `get`
//! result above, from the other direction: what costs on this stack is the size of the frame that
//! recurses, not the number of calls made and returned from before it does.
//!
//! # What the worklist costs
//!
//! The worklist allocates nothing for a leaf: a leaf is released the moment it is reached rather
//! than handed over, so a list of a million scalars never grows it past its initial `Vec::new`,
//! which does not allocate. **What it costs is a call, not an allocation** — every element pays
//! one call into [`Nestable::into_children`] to find out that it is a leaf.
//!
//! A tree of *n* container nodes pays that same call plus a worklist holding **one entry per
//! ancestor of the node in hand that still has an unvisited child**. A container ancestor
//! contributes exactly one entry however wide it is, because it is handed over whole and drained
//! where the tree allocated it; a chain of single children contributes none at all, because the
//! first child of a node goes into a register rather than onto the stack. So the peak follows the
//! tree's **branching nesting**, and neither its width nor its depth.
//!
//! ## What it cost while it was a `Vec` a container arm poured into
//!
//! The trait pushed into a `&mut Vec<Node>` until #199's second round, and a container arm reached
//! that worklist through `Vec::extend` — **which copies every element out of the child's own
//! buffer into the worklist's while both are live**, and whose reserve can reallocate the worklist
//! on top of that. The peak was then one depth-first *frontier*, so for a container of N children
//! it was N. **This was not all #199's to begin with**: sixteen of the twenty-one `extend` sites
//! were the six value enums' list, set, object and map arms, which have been that way since the
//! value tree got this container in #175 — a literal `[1, 2, … N]` among them — and five were the
//! two selection arms and the generic-argument arm #199 itself added. The repair below is one
//! mechanism over both.
//!
//! Measured on `aarch64-apple-darwin`, release, one process per row, as the peak bytes held above
//! what the tree already owned and the worklist's peak entry count:
//!
//! | shape | `241e589` | here |
//! |---|---|---|
//! | `{ f { a1 … aN } }`, N = 100 000 | 23 200 000 B / 100 000 | 928 B / 1 |
//! | `{ f1 { x } … fN { x } }`, N = 100 000 | 928 B / 1 | 928 B / 1 |
//! | `[[v1 … vN]]`, N = 100 000 | 8 800 000 B / 100 000 | 352 B / 1 |
//! | an object of 100 000 fields, inside a list | 8 800 000 B / 100 000 | 352 B / 1 |
//! | a GraphQLx map of 100 000 entries, inside a list | 17 600 000 B / 200 000 | 352 B / 1 |
//! | lists 1 000 wide nested 100 deep, deepest child last | 5 643 264 B / 99 901 | 352 B / 1 |
//! | lists 1 000 wide nested 100 deep, deepest child first | 88 000 B / 1 000 | 16 896 B / 100 |
//! | `[[[[…]]]]`, 1 000 000 deep | 352 B / 1 | 352 B / 1 |
//! | a `Type` chain 20 000 000 deep | 168 B / 1 | **0 B / 0** |
//!
//! **The two deep-and-wide rows are one shape in two orderings, and they are in the table because
//! they are adversarial to opposite implementations rather than ordered.** A `Vec` worklist pops
//! from the back, so it is the *deepest child last* ordering that leaves every level's remaining
//! width behind it; an adopted iterator yields from the front and is dropped when spent, so it is
//! *deepest child first* that keeps one source alive per level. Even on the ordering that suits it
//! least this shape holds 100 entries against 1 000, because 100 is the nesting and 1 000 is the
//! width.
//!
//! What is not repaired is the failure mode underneath: `Vec::push` still aborts the process when
//! the allocator refuses and a `Drop` still cannot refuse back. What changed is the size of the
//! request that would have to be refused.
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
//! **The adoption does not move that leaf-only end**, and it was checked rather than assumed. A
//! third instrument of the second one's kind — both containers named in one build, 1 000 000
//! elements, best of seven, and the base and this branch run alternately in one session — reads
//! **3.24–3.25 ns** per element at `241e589` against **3.28–3.29** here, over a `Vec` baseline of
//! 1.47–1.51 that both sides share: +1.75 against +1.80 additive, which is the two builds reading
//! as one number and inside the band on both sides. Its *ratio* is 2.2x, below the band's 2.4x,
//! and that is the denominator again — its leaf is a `Variable`, the one leaf a caller outside
//! this crate can construct, and its baseline reads 1.47–1.51 where the committed harnesses read
//! 0.62–1.01. The additive figure is the one the paragraph above already called the steadier.
//!
//! **The shape of [`Nestable::into_children`] was decided on that measurement.** It was written to
//! *return* an answer first — `-> Children<Self::Node>`, an enum whose arms were the leaf, one
//! child, two children and the three containers — which is the same adoption in a form that makes
//! an implementation's contract visible in its type. Two children have to be carried in the answer,
//! so `Children<InputValue>` is two `InputValue`s wide, and **every leaf pays to build and match a
//! value that size to say it has nothing**: the same instrument read **4.03 ns** per element that
//! way against 3.28 as a sink, and boxing the two-child arm — the same code with only the answer
//! shrunk — put it back at 3.37. A leaf is the per-element path of the whole design, so it decided
//! it. [`Worklist`]'s own header records what else the sink buys.
//!
//! It grows through `Vec`'s infallible `push`, deliberately and unlike `smear::json`'s value walk,
//! which grows through `try_reserve` and reports `Error::Allocation`. A writer can refuse; a `Drop`
//! has no return value and no caller to tell. What is bought is a failure that needs the allocator
//! exhausted by a request proportional to the branching nesting of a tree already in memory, in
//! place of one that arrives at a fixed depth on every machine.

use core::{convert::Infallible, fmt, marker::PhantomData, ops::Deref, slice};

use std::{boxed::Box, rc::Rc, sync::Arc, vec, vec::Vec};

mod sealed {
  /// Closes [`Nestable`](super::Nestable) to this crate.
  ///
  /// The trait is `pub` because it is a bound on [`Nested`](super::Nested)'s *definition* and so
  /// reaches the public signature of every value carrier. It is sealed because an outside
  /// implementation would be handed the release's invariant to keep — see
  /// [`Nestable::into_children`](super::Nestable::into_children) — and an `into_children` that
  /// handed nothing over for a container is the defect back again with a longer path to it.
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
  /// What the worklist walks.
  ///
  /// For a value enum this is the enum itself. For a carrier that merely *wraps* a value — an
  /// object's field, a map's entry — it is the value, because that is what can nest and what the
  /// loop must be able to come back to.
  type Node: NestNode;

  /// Consumes this element and hands every child that can itself nest to `worklist`.
  ///
  /// # The invariant the bound rests on
  ///
  /// **Anything not handed over is released here, so anything not handed over must be a leaf.** A
  /// child released inside this call runs its own container's [`Drop`], which re-enters this loop;
  /// that is exactly two frames deep as long as what was released is a leaf, because a leaf's
  /// `into_children` hands nothing over and releases nothing further. A *container* released here
  /// instead of handed over would add a frame per level, which is the recursion back again wearing
  /// a worklist.
  ///
  /// "Leaf" means **holds no node of this crate's own**. An arm an implementation treats as a leaf
  /// still releases whatever its payload parameters were instantiated with, and a caller who put a
  /// value tree in one of those has a recursion here that no implementation can hand over — see
  /// [`Nested`] for the scope that follows from it.
  ///
  /// # A container is handed over, never poured
  ///
  /// The worklist took a `&mut Vec<Self::Node>` until #199's second round, and a container arm
  /// reached it through [`Vec::extend`] — which **copies every element out of the child's own
  /// buffer into the worklist's while both are live**, and whose reserve can reallocate the
  /// worklist on top of that. Releasing `{ f { a1 … aN } }` therefore held the selection set twice
  /// over. Measured on `aarch64-apple-darwin`, release, at N = 100 000: **23 200 000 bytes** of
  /// storage above what the tree already owned, which is 100 000 × `size_of::<Selection>`. The
  /// `Vec` that stood in that position before this container did released its elements in place
  /// and allocated *nothing*, so for the two selection arms the worklist was a regression and not
  /// only an incomplete repair.
  ///
  /// [`Worklist::adopt`] and its two carrier siblings are what remove it: a container arm hands
  /// its `Vec` over and the walk drains it where the tree allocated it, so no element is ever
  /// copied from one buffer to another. The same shape at the same N now peaks at **928 bytes**,
  /// which is the worklist's own first allocation and nothing else.
  ///
  /// The implementations match exhaustively and without a wildcard arm, so a variant added to one
  /// of these enums is a compile error here rather than a silent return to recursing.
  fn into_children(self, worklist: &mut Worklist<Self::Node>);
}

/// A node the release walks, and the two carriers a container of its children may hold instead of
/// nodes.
///
/// A list's, a set's and a selection set's container holds the node itself, so [`Worklist::adopt`]
/// takes one as it stands. An **object**'s container holds fields and a **map**'s holds entries,
/// and those are the only two positions in either dialect where the children of one node are
/// stored as something other than nodes. Naming both here is what lets
/// [`adopt_fields`](Worklist::adopt_fields) and [`adopt_entries`](Worklist::adopt_entries) take
/// those containers over whole as well.
///
/// Without them an object arm would have to project its container into a `Vec<Self>`, which is
/// exactly the copy the worklist exists to remove: measured on `aarch64-apple-darwin`, release, an
/// object of 100 000 fields cost **8 800 000 bytes** of worklist that way, and a GraphQLx map of
/// 100 000 entries **17 600 000**. Both are 352 bytes now, and that is one allocation.
///
/// A node with neither names [`Absent`], which is uninhabited: the lane is then not merely unused
/// but unreachable, and a reader does not have to check.
pub trait NestNode: Nestable<Node = Self> {
  /// The carrier an object's container holds: a span, a name, and one node.
  type Field: Nestable<Node = Self>;

  /// The carrier a map's container holds: a span and two nodes.
  type Entry: Nestable<Node = Self>;
}

/// A carrier a node does not have.
///
/// Uninhabited, so `Vec<Absent<N>>` can only ever be empty and
/// [`Worklist::adopt_fields`] or [`adopt_entries`](Worklist::adopt_entries) over it can hand
/// nothing over. GraphQL has no map literal, and neither dialect's *type* grammar or selection
/// grammar has either carrier; each of those names this for the lanes it does not use, and the
/// walk's arms for them are dead by construction rather than by inspection.
pub struct Absent<N> {
  never: Infallible,
  node: PhantomData<N>,
}

impl<N> Sealed for Absent<N> {}

impl<N: NestNode> Nestable for Absent<N> {
  type Node = N;

  #[inline]
  fn into_children(self, _worklist: &mut Worklist<N>) {
    match self.never {}
  }
}

/// Where an element hands its children, and the walk's own storage.
///
/// # What is on it, and what is deliberately not
///
/// A source is either **a container the tree already owned** — adopted, never copied — or **one**
/// node held by value, which is a *second* handover waiting for the first one's subtree to finish:
/// a map type's value after its key, or a map entry's second half. A chain of single children is
/// on neither: [`push`](Self::push) puts the first child in a register instead, so a `Nest` chain
/// of any depth leaves the stack empty and allocates nothing. Measured on `aarch64-apple-darwin`,
/// release: `graphql::ast::Type` nested **20 000 000** deep releases in **0 bytes** and 0 entries.
///
/// # Why this is not the design the first round rejected
///
/// That round refused *a stack of owned iterators, one per level*, because a chain 20 000 000
/// levels deep would put 20 000 000 entries on it — trading a width worst case for a depth one.
/// Two things make this different, and both are load-bearing:
///
/// * **A single child never reaches the stack.** [`push`](Self::push) fills the register, and the
///   register is what a chain runs through. A chain has no entries here at all.
/// * **A source is dropped the moment its last child is taken**, not when it is next reached. So a
///   chain of *one-element containers* — `[[[[…]]]]`, which a caller can nest without limit —
///   pushes and pops one entry per level and peaks at one, rather than leaving a spent iterator
///   behind per level. Measured at 1 000 000 levels: **one** entry, 352 bytes.
///
/// What is left is one entry per ancestor of the node in hand that still has an unvisited child,
/// and **a container ancestor contributes one entry however wide it is**. That is a bound on the
/// tree's *branching* nesting, and it is what [`Nested::drop`]'s own documentation measures.
///
/// # Why it is a sink and not a returned answer
///
/// It was written the other way first — `fn into_children(self) -> Children<Self::Node>`, an enum
/// whose arms were the leaf, one child, two children and the three containers — and that shape is
/// **measurably worse on the one path that runs per element**. Two children have to be carried in
/// the answer, so `Children<InputValue>` is two `InputValue`s wide, and *every leaf* pays to build
/// and match a value that size to say it has no children at all. Measured on
/// `aarch64-apple-darwin`, release, 1 000 000 leaves in one container, all three shapes read by
/// one instrument in one session: **3.28 ns** per element as a sink, **4.03** as a returned enum,
/// and **3.37** with the two-child arm boxed — the same code with only the answer shrunk, so the
/// difference is the size and nothing else. `241e589` reads 3.24–3.25 on that instrument, which is
/// the sink and not the enum.
///
/// The sink costs nothing because a leaf's `into_children` is an empty body: there is no answer to
/// build. It also lets a **map entry hand both halves over by forwarding to each**, which the
/// returned form could not do without either an allocation or merging two answers — and merging
/// two container answers is the copy this whole change removes.
pub struct Worklist<N: NestNode> {
  /// The one child a chain hands over, kept out of the stack so a chain never allocates.
  ///
  /// `None` whenever **the walk** calls [`Nestable::into_children`], because the walk empties it
  /// before calling and nothing else fills it. So the first child a node hands over lands here and
  /// is taken next, which is the whole of why a chain has no stack entries.
  ///
  /// It is *not* `None` at every call: an implementation may forward to a second
  /// `into_children` — a map entry does, once per half — and that second handover finds this
  /// occupied and takes a stack entry. That is the case [`push`](Self::push) exists to be total
  /// over.
  next: Option<N>,
  /// The sources of children the walk has reached but not drained, innermost last.
  sources: Vec<Source<N>>,
}

/// One source of children the walk has not reached yet.
enum Source<N: NestNode> {
  /// A child the walk must come back to, held by value because there is no container in that
  /// position to adopt: the second of the two a map **type** hands over, or the child of the
  /// second half a map entry forwards to. It is the only entry that holds a node rather than a
  /// container.
  Second(Option<N>),
  /// A container of nodes, drained where the tree allocated it.
  Nodes(vec::IntoIter<N>),
  /// A container of object fields, drained where the tree allocated it.
  Fields(vec::IntoIter<N::Field>),
  /// A container of map entries, drained where the tree allocated it.
  Entries(vec::IntoIter<N::Entry>),
}

impl<N: NestNode> Worklist<N> {
  /// An empty worklist, allocating nothing.
  #[inline]
  pub(crate) const fn new() -> Self {
    Self {
      next: None,
      sources: Vec::new(),
    }
  }

  /// Hands one child over.
  ///
  /// The first child a node hands over goes into a register and the walk takes it next, so a chain
  /// of single children never touches the stack and never allocates. A *second* handover — a map
  /// type's value after its key, or a map entry's second half — finds the register occupied and is
  /// the only thing that ever puts a node on the stack.
  #[inline]
  pub fn push(&mut self, child: N) {
    match self.next {
      None => self.next = Some(child),
      Some(_) => self.sources.push(Source::Second(Some(child))),
    }
  }

  /// Hands a container of nodes over whole.
  ///
  /// The `Vec` is **taken**, not read: the walk drains it where the tree allocated it, so no
  /// element is copied and the worklist grows by one entry however wide the container is.
  #[inline]
  pub fn adopt(&mut self, nodes: Vec<N>) {
    if !nodes.is_empty() {
      self.sources.push(Source::Nodes(nodes.into_iter()));
    }
  }

  /// Hands a container of object fields over whole, on the same terms as [`adopt`](Self::adopt).
  #[inline]
  pub fn adopt_fields(&mut self, fields: Vec<N::Field>) {
    if !fields.is_empty() {
      self.sources.push(Source::Fields(fields.into_iter()));
    }
  }

  /// Hands a container of map entries over whole, on the same terms as [`adopt`](Self::adopt).
  #[inline]
  pub fn adopt_entries(&mut self, entries: Vec<N::Entry>) {
    if !entries.is_empty() {
      self.sources.push(Source::Entries(entries.into_iter()));
    }
  }

  /// Releases everything handed over since the last time this returned.
  ///
  /// `#[inline]`, and **the leaf is what pays for it**: a container of a million scalars calls
  /// this a million times and every one of those calls has nothing to do, so what it must not have
  /// to do is enter the walk to find that out.
  #[inline]
  fn drain(&mut self) {
    if self.next.is_some() || !self.sources.is_empty() {
      self.walk();
    }
  }

  /// The walk itself, entered only when something was handed over.
  ///
  /// Depth-first, and exactly so: the children of the node in hand are always taken before
  /// anything reached earlier, because the register is served first and a source is pushed on top
  /// of the sources already there and drained to nothing before any of them is touched again.
  fn walk(&mut self) {
    loop {
      match self.next.take() {
        Some(node) => node.into_children(self),
        None => {
          if !self.step() {
            return;
          }
        }
      }
    }
  }

  /// Takes one child from the innermost source that still has one, and takes it apart.
  ///
  /// A source is dropped the moment its last child is taken rather than when it is next reached,
  /// and that is not tidiness: without it a chain of one-element containers leaves one spent
  /// iterator on the stack per level, and the walk's storage would follow the depth after all.
  fn step(&mut self) -> bool {
    loop {
      let Some(source) = self.sources.last_mut() else {
        return false;
      };
      match source {
        Source::Second(slot) => {
          if let Some(node) = slot.take() {
            self.sources.pop();
            node.into_children(self);
            return true;
          }
        }
        Source::Nodes(nodes) => {
          if let Some(node) = nodes.next() {
            let spent = nodes.as_slice().is_empty();
            if spent {
              self.sources.pop();
            }
            node.into_children(self);
            return true;
          }
        }
        Source::Fields(fields) => {
          if let Some(field) = fields.next() {
            let spent = fields.as_slice().is_empty();
            if spent {
              self.sources.pop();
            }
            field.into_children(self);
            return true;
          }
        }
        Source::Entries(entries) => {
          if let Some(entry) = entries.next() {
            let spent = entries.as_slice().is_empty();
            if spent {
              self.sources.pop();
            }
            entry.into_children(self);
            return true;
          }
        }
      }
      self.sources.pop();
    }
  }
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
  /// A depth-first walk: one element is taken apart and its subtree drained to nothing before the
  /// next element is touched at all.
  ///
  /// # Why the two loops are nested rather than sequential
  ///
  /// Taking every element apart first and draining afterwards releases the same tree — and puts
  /// one entry on the worklist per *top-level element* before a single one comes off. **Width is
  /// not what the parser's nesting ceiling bounds.** `{ f1 { x } … fN { x } }` nests two levels and
  /// is N wide, and N is the caller's: a document that ceiling admits carries any number of
  /// siblings. Measured on `aarch64-apple-darwin`, release, on a selection set of exactly that
  /// shape at N = 100 000, the worklist peaked at **100 000** entries sequential and **1**
  /// interleaved. The interleaved half was re-read at `241e589` and again here, and both read
  /// **one** entry and **928 bytes** above the tree — the worklist's own first allocation, and
  /// nothing else. This change does not touch that property; the sequential figure is round one's
  /// and stands on the shape it measured, which no longer exists in this file.
  ///
  /// # What the peak follows, which is neither the width nor the depth
  ///
  /// It is one entry per **ancestor of the node in hand that still has an unvisited child**, and a
  /// container ancestor contributes exactly one entry however wide it is. A container is handed
  /// over whole and drained where the tree allocated it, so `{ f { a1 … aN } }` peaks at **one**
  /// entry and **928 bytes** at N = 100 000 — against 100 000 entries and 23 200 000 bytes when
  /// the worklist was a `Vec` a container arm `extend`ed into. A single child does not reach the
  /// worklist at all, and a source is dropped the moment its last child is taken, so neither a
  /// pointer chain nor a chain of one-element containers grows it: measured at 20 000 000 levels
  /// of `Type` for the first (**0 entries, 0 bytes**) and 1 000 000 levels of `[[[[…]]]]` for the
  /// second (**one** entry, 352 bytes).
  ///
  /// What is left is the tree's *branching* nesting. Lists a thousand wide nested a hundred deep
  /// peak at **100** entries and 16 896 bytes on the ordering that suits this walk least, against
  /// 1 000 entries and 88 000 bytes before — one per level, which is the walk's own path and
  /// nothing about the width. The module header has both orderings and why they differ.
  ///
  /// **`Vec::push` aborts the process when the allocator refuses, and a `Drop` cannot refuse
  /// back** — it has no return value and no caller to tell it to. That surface is still here; what
  /// it now takes to reach it is a request proportional to the branching nesting of a tree already
  /// in memory, rather than to a frontier of it.
  ///
  /// # What the interleave costs
  ///
  /// Nothing measurable on a leaf-only container, and it is *cheaper* once the elements nest. A
  /// leaf-only container hands nothing over, so the outer loop is one call into `into_children`
  /// and the two checks that find nothing came back. Measured against the sequential shape in the same build on
  /// `aarch64-apple-darwin`, release, 1 000 000 `Null` leaves, best of eleven releases and five
  /// interleaved runs a side: **3.218–3.226 ns** per element sequential against
  /// **3.219–3.224 ns** interleaved, which is the two shapes reading as one number. On 1 000 000
  /// elements that each nest one level the same instrument read **30.6–30.8 ns** sequential
  /// against **22.8–23.0 ns** interleaved, about **26% cheaper**, because the worklist that had
  /// grown to a million entries never left its first allocation.
  ///
  /// That instrument compares the two loop shapes and nothing else, and it was read on the
  /// `extend`-based worklist this one replaced — it says which loop the module header's band was
  /// measured on, not what the band is. The leaf-only end is unmoved by either change, and the
  /// module header has the interleaved base-against-branch reading that establishes it; the
  /// nesting end was read sequentially and both the interleave and the adoption only lower it.
  fn drop(&mut self) {
    // `Vec::new` does not allocate, and a container of leaves never gives it a reason to: a leaf
    // hands nothing over and is released where it is reached. It is not released for free,
    // though — every element still pays a call into `into_children` to find out it is a leaf, and
    // that call is the whole of the cost. Two harnesses priced it on `aarch64-apple-darwin`,
    // release, and they bracket it rather than agree: +1.4 to +2.0 ns per element on a leaf-only
    // container, 2.4x to 3.6x the derived glue. The module header says which instrument read
    // which end, and why.
    let mut worklist = Worklist::new();
    for element in core::mem::take(&mut self.values) {
      element.into_children(&mut worklist);
      // Drained to nothing before the next element is touched, which is what keeps the peak at
      // one path instead of the sum over the whole container.
      worklist.drain();
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

/// The owned pointer a [`Nest`] holds, and the two operations the release needs from it.
///
/// Sealed through the same private trait this module seals [`Nestable`] with. Implemented for
/// [`Box`], [`Rc`] and [`Arc`], which are the three the `ty!` macro in `graphql::ast::ty`
/// instantiates its enums over; `graphqlx`'s type enum uses the [`Box`] impl for all three of its
/// pointer arms.
///
/// [`into_pointee`](Self::into_pointee) is `Option` rather than `T` because two of the three are
/// *shared*: an [`Rc`] or [`Arc`] with another owner left has nothing to unlink, and answering
/// `None` is how the release says so. That is not a weakening — a shared pointer's own [`Drop`]
/// does not descend either, so the level below it is not this release's to reach until the last
/// owner goes, and the last owner runs this loop.
pub trait NestPtr: Sized + Sealed {
  /// What the pointer owns.
  type Pointee: Nestable;

  /// Puts a value behind the pointer.
  fn nest(value: Self::Pointee) -> Self;

  /// The pointee, borrowed.
  fn pointee(&self) -> &Self::Pointee;

  /// Consumes the pointer and returns the pointee, or `None` when another owner remains.
  fn into_pointee(self) -> Option<Self::Pointee>;
}

impl<T: Nestable> Sealed for Box<T> {}

impl<T: Nestable> NestPtr for Box<T> {
  type Pointee = T;

  #[inline]
  fn nest(value: T) -> Self {
    Self::new(value)
  }

  #[inline]
  fn pointee(&self) -> &T {
    self
  }

  /// Always `Some`: a `Box` is the sole owner by construction.
  #[inline]
  fn into_pointee(self) -> Option<T> {
    Some(*self)
  }
}

/// A pointer whose pointee can always be taken back, because nothing else can own it.
///
/// [`Box`] and nothing else. [`Rc`] and [`Arc`] are excluded by what they are: their
/// [`NestPtr::into_pointee`] answers `None` when another owner remains, and no bound can rule that
/// out. What this buys is a *total* [`Nest::into_sole`] — the graphqlx type enum's `IntoSpan`
/// reaches a span by consuming the arm's pointee, and this is what lets it do so without an
/// `Option` it would have to answer for.
pub trait SoleNestPtr: NestPtr {
  /// Consumes the pointer and returns the pointee.
  fn into_sole_pointee(self) -> Self::Pointee;
}

impl<T: Nestable> SoleNestPtr for Box<T> {
  #[inline]
  fn into_sole_pointee(self) -> T {
    *self
  }
}

impl<T: Nestable> Sealed for Rc<T> {}

impl<T: Nestable> NestPtr for Rc<T> {
  type Pointee = T;

  #[inline]
  fn nest(value: T) -> Self {
    Self::new(value)
  }

  #[inline]
  fn pointee(&self) -> &T {
    self
  }

  #[inline]
  fn into_pointee(self) -> Option<T> {
    Self::into_inner(self)
  }
}

impl<T: Nestable> Sealed for Arc<T> {}

impl<T: Nestable> NestPtr for Arc<T> {
  type Pointee = T;

  #[inline]
  fn nest(value: T) -> Self {
    Self::new(value)
  }

  #[inline]
  fn pointee(&self) -> &T {
    self
  }

  #[inline]
  fn into_pointee(self) -> Option<T> {
    Self::into_inner(self)
  }
}

/// An owned pointer whose release is a loop rather than a descent.
///
/// [`Nested`]'s counterpart for the positions that own **one** child instead of a container of
/// them: `Type`'s list element in both dialects, and GraphQLx's set element and map key and value.
/// It answers as the pointer it wraps does — it derefs to the pointee, and clones, compares,
/// hashes and prints exactly as `Box<T>`, `Rc<T>` or `Arc<T>` would, refcount semantics included —
/// and what it adds is the [`Drop`] that keeps a type nested through this crate's own carriers from
/// taking the process with it.
///
/// It is not the pointer's whole surface: there is no `DerefMut`, no `AsMut`, and no way to reach
/// the pointer itself. [`into_inner`](Self::into_inner) hands the pointee back, which is what the
/// release itself uses.
///
/// There is deliberately no `From<P::Pointee>` either, and that one is the language's rather than a
/// choice: `P::Pointee` is an associated type a caller could resolve to `Nest<P>` itself, so the
/// impl overlaps `core`'s reflexive `From<T> for T` and `E0119` refuses it. [`new`](Self::new) is
/// the constructor, and each enum's own `From<ListType<Self>>` is the one a consumer reaches for.
///
/// # Why the enum's arm holds this instead of the pointer
///
/// Because the arm is where the release has to be installed, and it cannot be installed on the
/// enum. `E0509` is the usual reason given, and it is real — a `Drop` on the enum costs every
/// by-value `unwrap_*` and `try_unwrap_*` `derive_more` generates and an `IntoSpan` that reaches a
/// span by matching itself apart. But for *this* shape there is a harder reason underneath it, and
/// the module header states it: the enum-side `Drop` is not merely expensive, it is unwritable.
/// Unlinking a chain from `&mut self` means taking the pointer out of the arm, taking out of a
/// `&mut` place needs a value to leave behind, and no `Type` can be built without a name or another
/// `Type`. `Option<P>` is the slot that makes the take free, and it is exactly one word of nothing:
/// `Option<Box<T>>`, `Option<Rc<T>>` and `Option<Arc<T>>` are all niche-optimised to the pointer's
/// own size.
///
/// # What the worklist costs
///
/// **Nothing, at any depth**, and that is the difference between this shape and [`Nested`]'s. A
/// `Nest` owns exactly one pointee, so what it hands the worklist is one child, and one child goes
/// into a register rather than onto the stack — a chain runs through that register and never
/// allocates. Measured on `aarch64-apple-darwin`, release: `graphql::ast::Type` nested
/// **20 000 000** deep released in **0 bytes** above the tree and 0 worklist entries. It used to
/// pay one allocation, a `Vec` that never grew past its first, and it now pays none.
///
/// A pointee that *branches* does reach the stack, and both branching pointees in this workspace
/// are GraphQLx's: a `MapType`'s key and value are two types, and a path's generic arguments are a
/// container. Those pay what [`Nested`]'s header prices — one entry per branching ancestor — and
/// nothing more.
///
/// **There is no sequential drain here to interleave away**, which is why this release hands over
/// once and drains once, and [`Nested`]'s does it per element. A `Nest` owns exactly one pointee.
/// [`Nested`] owns a container, and taking every element of it apart before draining any is what
/// would make the peak the sum over the container rather than one path; that container is the only
/// place the distinction exists.
///
/// It grows through `Vec`'s infallible `push`, and the same sentence in [`Nested`]'s header applies
/// unchanged: a `Drop` has no return value and no caller to tell, so a refusal is not available to
/// it. What is bought is a failure that needs the allocator exhausted by a request proportional to
/// a tree already in memory, in place of one that arrives at a fixed depth on every machine.
///
/// # What the release covers, and what it does not
///
/// Every recursive position the *type* carriers form themselves, at any depth, in both dialects.
/// It does not cover a node reached through a payload parameter — a caller's `Name` or `S` or
/// `Span` that owns a node builds a cycle running through an arm [`Nestable::into_children`]
/// correctly releases as a leaf. That is `al8n/smear#176`, it predates both containers, and
/// [`Nested`]'s own documentation derives which parameters are a way in.
pub struct Nest<P: NestPtr> {
  /// `Some` for the whole observable life of the value.
  ///
  /// [`into_inner`](Self::into_inner) and [`drop`](Self::drop) are the only writers, both take
  /// `self` or `&mut self` at the end of it, and neither hands out a borrow afterwards.
  inner: Option<P>,
}

impl<P: NestPtr> Nest<P> {
  /// Puts a value behind a fresh pointer.
  #[inline]
  pub fn new(value: P::Pointee) -> Self {
    Self {
      inner: Some(P::nest(value)),
    }
  }

  /// The pointee, borrowed.
  ///
  /// `#[inline]` and **not** `#[inline(always)]`, unlike the forwarding impls below, because this
  /// is the one body among them with a branch in it. At opt-level 0 inlining it does not remove a
  /// frame from the recursions that pass through it — it grows theirs, and theirs is paid once per
  /// level. The module header has the measurement that decided it.
  #[inline]
  pub fn get(&self) -> &P::Pointee {
    match self.inner.as_ref() {
      Some(ptr) => ptr.pointee(),
      // Unreachable: the field is written exactly twice, by `into_inner` and by `drop`, and both
      // consume the value they emptied.
      None => unreachable!("a live `Nest` always holds its pointer"),
    }
  }

  /// Consumes this pointer and returns the pointee, or `None` when another owner remains.
  ///
  /// Takes rather than moves the field out: this type implements [`Drop`], which is `E0509`'s
  /// trigger, and taking is what an `Option` supports for free. What is left behind is the empty
  /// slot the release then finds nothing in.
  #[inline]
  #[must_use]
  pub fn into_inner(mut self) -> Option<P::Pointee> {
    self.inner.take().and_then(NestPtr::into_pointee)
  }
}

impl<P: SoleNestPtr> Nest<P> {
  /// Consumes this pointer and returns the pointee.
  ///
  /// The infallible [`into_inner`](Self::into_inner), available where the pointer's own type says
  /// there can be no second owner.
  #[inline]
  #[must_use]
  pub fn into_sole(mut self) -> P::Pointee {
    match self.inner.take() {
      Some(ptr) => ptr.into_sole_pointee(),
      // Unreachable for the reason `get` states: the slot is emptied only by a method that
      // consumes the value it emptied.
      None => unreachable!("a live `Nest` always holds its pointer"),
    }
  }
}

impl<P: NestPtr> Drop for Nest<P> {
  fn drop(&mut self) {
    // Nothing to unlink when the slot was already taken by `into_inner`, and nothing when the
    // pointer is shared and another owner remains.
    let Some(ptr) = self.inner.take() else {
      return;
    };
    let Some(pointee) = ptr.into_pointee() else {
      return;
    };
    // A chain of single children is followed in a register, so this allocates nothing at any
    // depth: measured at 20 000 000 levels of `graphql::ast::Type` on `aarch64-apple-darwin`,
    // release, **0 bytes** above the tree. Every `Nest` the walk releases instead of naming is one
    // whose own slot the call had already emptied, so the re-entry into this `drop` finds `None`
    // and returns. That is the one frame this walk ever spends on the tree these types form.
    let mut worklist = Worklist::new();
    pointee.into_children(&mut worklist);
    worklist.drain();
  }
}

/// `#[inline(always)]` on this and on the five forwarding impls below it, and that is a stack
/// measurement rather than a speed one.
///
/// `Clone`, `Debug` and `PartialEq` on a type enum stay recursive by design — they are calls
/// somebody makes, unlike the release — so what this wrapper must not do is take levels off the
/// depth at which they abort. At opt-level 0 a plain `#[inline]` is a hint to a pass that is not
/// running, so each forward was a native frame per level; `inline(always)` is honoured by rustc's
/// MIR inliner there. It gave `Type`'s `{:?}` ceiling back in full and about a fifth of the other
/// two. In release it changes nothing, because nothing was left to inline. The module header has
/// the four columns.
impl<P: NestPtr> Deref for Nest<P> {
  type Target = P::Pointee;

  #[inline(always)]
  fn deref(&self) -> &Self::Target {
    self.get()
  }
}

impl<P: NestPtr> AsRef<P::Pointee> for Nest<P> {
  #[inline(always)]
  fn as_ref(&self) -> &P::Pointee {
    self.get()
  }
}

impl<P: NestPtr + Clone> Clone for Nest<P> {
  /// The pointer's own clone: deep for a [`Box`], a refcount bump for an [`Rc`] or an [`Arc`],
  /// which is what the arm did before this type stood in it.
  ///
  /// Written out rather than delegated to [`Option::clone`], which is the same three lines, for a
  /// reason that is entirely about the stack. `Option::clone` builds its `Some` **around** the
  /// inner clone, so at opt-level 0 its frame is live for the whole of the recursion below it —
  /// one more native frame per level of a chain, on the impl whose ceiling is the lowest of the
  /// three. Measured on `aarch64-apple-darwin`, one child process per depth on an 8 MiB thread:
  /// `graphql::ast::Type::clone` aborted at **14 997** levels through `Option::clone` and at
  /// **15 906** written out, against a `9f584d6` base of 16 932 — the residual charge for standing
  /// this type in the arm falls from 11.4% to 6.1%. In release the two are the same code.
  ///
  /// The `allow` below buys those 5.3 percentage points of ceiling, so it is a measurement rather
  /// than a style preference. The lint's own suggestion is measurably the worst of the three:
  /// `self.inner.as_ref().map(P::clone)` aborts at **14 186**, because `Option::map`'s frame is
  /// live across the recursion exactly as `Option::clone`'s is and the call it takes is another
  /// one on top.
  #[allow(
    clippy::manual_map,
    reason = "Option::map's frame is live across the recursion at opt-level 0 and costs one more \
              than Option::clone's: measured, Type::clone aborts at 15 906 levels written out, \
              14 997 through Option::clone and 14 186 through the suggested map"
  )]
  #[inline(always)]
  fn clone(&self) -> Self {
    Self {
      inner: match self.inner.as_ref() {
        Some(ptr) => Some(ptr.clone()),
        None => None,
      },
    }
  }
}

impl<P: NestPtr> fmt::Debug for Nest<P>
where
  P::Pointee: fmt::Debug,
{
  /// The pointee, as the pointer prints it — this wrapper is not part of what a type *is*.
  #[inline(always)]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(self.get(), f)
  }
}

impl<P: NestPtr> PartialEq for Nest<P>
where
  P::Pointee: PartialEq,
{
  #[inline(always)]
  fn eq(&self, other: &Self) -> bool {
    self.get() == other.get()
  }
}

impl<P: NestPtr> Eq for Nest<P> where P::Pointee: Eq {}

impl<P: NestPtr> core::hash::Hash for Nest<P>
where
  P::Pointee: core::hash::Hash,
{
  #[inline(always)]
  fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
    self.get().hash(state);
  }
}
