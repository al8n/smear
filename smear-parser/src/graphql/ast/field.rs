//! GraphQL selection AST node types.
//!
//! Shared selection carriers live at crate level so GraphQLx can bind the same
//! structures later. This module specializes them to GraphQL names, executable
//! arguments, and executable directives.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{Arguments, Directives, FragmentName, Name};
use crate::value::{Absent, NestNode, Nestable, Nested, Sealed, Worklist};

/// A field alias in a GraphQL selection (`Name :`).
pub type Alias<S> = crate::selection::Alias<Name<S>>;

/// A fragment type condition (`on NamedType`).
pub type TypeCondition<S> = crate::selection::TypeCondition<Name<S>>;

/// A named fragment spread (`... FragmentName Directives?`).
pub type FragmentSpread<S> = crate::selection::FragmentSpread<FragmentName<S>, Directives<S>>;

/// An inline fragment (`... TypeCondition? Directives? SelectionSet`).
pub type InlineFragment<S> =
  crate::selection::InlineFragment<TypeCondition<S>, Directives<S>, SelectionSet<S>>;

/// A selection set containing one or more selections.
///
/// The container is [`Nested`] rather than a `Vec`, and that is where the release of a deeply
/// nested selection lives: an [`InlineFragment`] owns another `SelectionSet` and a [`Field`] owns
/// an optional one, so a caller who grows the chain in a loop — every constructor these carriers
/// need is public — built something whose `Drop` glue descended one native frame per level and
/// aborted the process on the way out. [`Nested`]'s own documentation states the mechanism and
/// what it does and does not cover.
pub type SelectionSet<S> = crate::selection::SelectionSet<Selection<S>, Span, Nested<Selection<S>>>;

/// A GraphQL selection.
///
/// A selection is a field, a named fragment spread, or an inline fragment.
///
/// Not `#[non_exhaustive]`: draft §2.4 is `Selection : Field | FragmentSpread | InlineFragment`.
/// The variant set is the specification's, not smear's, so adding to it would mean parsing
/// something that is not GraphQL. `#[non_exhaustive]` here would advertise an evolution freedom
/// this type structurally does not have and charge for it where it costs most — the exhaustiveness
/// `smear-compiler`'s draft §5 rules depend on. Smear's extension mechanism is a separate dialect:
/// `graphqlx`'s own `Selection` keeps the attribute, correctly.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Selection<S> {
  /// A field selection.
  Field(Field<S>),
  /// A named fragment spread.
  FragmentSpread(FragmentSpread<S>),
  /// An inline fragment.
  InlineFragment(InlineFragment<S>),
}

impl<S> Sealed for Selection<S> {}

/// How the release reaches a selection's children.
///
/// The recursive positions are the two selection sets — a field's optional one and an inline
/// fragment's required one — and they are the only children pushed. Everything else an arm owns is
/// released here, which the loop's invariant requires to be a leaf: a name, a type condition, an
/// argument list and a directive list hold *values* and never a selection, and a value's own
/// release is already the iterative one [`Nested`] installs on the value carriers. What they can
/// still own is a node a caller stored in `S`, which no implementation of this trait can push —
/// see [`Nested`]'s documentation and `al8n/smear#176`.
// ── THE RANK EDGE, ASSERTED ─────────────────────────────────────────────────────────────────────
//
// A selection hands over only the nested selection sets. Everything else it destructures is
// dropped **in place**: the alias and the name are leaves, and the arguments and directives are
// not — they hold `InputValue` nodes.
//
// That is allowed, and the invariant on `Nestable::into_children` says why: a node of a
// *lower-ranked* tree may be dropped here because its own release is iterative. Releasing an
// argument list runs `Nested`'s `Drop`, which walks the value tree on its own worklist, so it
// costs *this* release O(1) native frames however deep the values are — not one frame per level.
// (What a caller's own payload destructor costs is the payload's own shape; `Leaf` does not bound
// it and neither does this. See `Nestable::into_children`.)
//
// It is sound only while the edge points downwards, so the direction is checked rather than
// trusted. One concrete instantiation is enough: `RANK` does not depend on the payload parameters.
const _: () = assert!(
  <super::InputValue<&'static str> as Nestable>::RANK < <Selection<&'static str> as Nestable>::RANK,
  "a selection drops value nodes in place, so the value tree must rank below the selection tree",
);

impl<S> Nestable for Selection<S> {
  /// The selection tree: rank 2. It drops value nodes (arguments, directives) and, in
  /// GraphQLx, a type node (a type condition) in place — see the assertions above.
  const RANK: u8 = 2;

  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    match self {
      Self::Field(field) => {
        // span, alias, name, arguments, directives — dropped in place. The first three are
        // leaves; the last two are rank-1 value nodes that release themselves iteratively.
        let (_, _, _, _, _, selection_set) = field.into_components();
        if let Some(selection_set) = selection_set {
          worklist.adopt(selection_set.into_selections().into_vec());
        }
      }
      // A spread names a fragment; the selections it stands for are the fragment definition's.
      Self::FragmentSpread(_) => {}
      Self::InlineFragment(fragment) => {
        // span, type_condition, directives — dropped in place. The directives are rank-1 value
        // nodes; the type condition is a rank-1 type node in GraphQLx and a bare name in GraphQL.
        let (_, _, _, selection_set) = fragment.into_components();
        worklist.adopt(selection_set.into_selections().into_vec());
      }
    }
  }
}

/// A selection's children are selections, and a selection set holds them directly. Neither carrier
/// lane exists here: an object field and a map entry are *value* carriers.
impl<S> NestNode for Selection<S> {
  type Field = Absent<Self>;
  type Entry = Absent<Self>;
}

impl<S> Selection<S> {
  /// Returns the span covering the complete selection.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(field) => field.span(),
      Self::FragmentSpread(spread) => spread.span(),
      Self::InlineFragment(fragment) => fragment.span(),
    }
  }
}

impl<S> AsSpan<Span> for Selection<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for Selection<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(field) => field.into_span(),
      Self::FragmentSpread(spread) => spread.into_span(),
      Self::InlineFragment(fragment) => fragment.into_span(),
    }
  }
}

/// A GraphQL field (`Alias? Name Arguments? Directives? SelectionSet?`).
pub type Field<S> =
  crate::selection::Field<Alias<S>, Name<S>, Arguments<S>, Directives<S>, SelectionSet<S>>;
