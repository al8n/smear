//! GraphQLx selection and field AST aliases.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{Arguments, Directives, FragmentTypePath, Name, Nested, Type, TypePath};
use crate::value::{Absent, NestNode, Nestable, Sealed, Worklist};

/// A GraphQLx field alias (`Name :`).
pub type Alias<S, Span = SimpleSpan> = crate::selection::Alias<Name<S, Span>, Span>;

/// A GraphQLx fragment type condition with a path and optional type arguments.
pub type TypeCondition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::selection::TypeCondition<TypePath<S, Span, Ty>, Span>;

/// A named GraphQLx fragment spread with a path and optional type arguments.
pub type FragmentSpread<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::selection::FragmentSpread<FragmentTypePath<S, Span, Ty>, Directives<S, Span>, Span>;

/// An inline GraphQLx fragment.
pub type InlineFragment<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::selection::InlineFragment<
    TypeCondition<S, Span, Ty>,
    Directives<S, Span>,
    SelectionSet<S, Span>,
    Span,
  >;

/// A GraphQLx selection set containing one or more selections.
///
/// The container defaults to [`Nested`] rather than a `Vec`, and that is where the release of a
/// deeply nested selection lives: an [`InlineFragment`] owns another `SelectionSet` and a [`Field`]
/// owns an optional one, so a caller who grows the chain in a loop — every constructor these
/// carriers need is public — built something whose `Drop` glue descended one native frame per level
/// and aborted the process on the way out. [`Nested`]'s own documentation states the mechanism and
/// what it does and does not cover.
pub type SelectionSet<S, Span = SimpleSpan, Container = Nested<Selection<S, Span>>> =
  crate::selection::SelectionSet<Selection<S, Span>, Span, Container>;

/// A GraphQLx selection.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Selection<S, Span = SimpleSpan> {
  /// A field selection.
  Field(Field<S, Span>),
  /// A named fragment spread.
  FragmentSpread(FragmentSpread<S, Span>),
  /// An inline fragment.
  InlineFragment(InlineFragment<S, Span>),
}

impl<S, Span> Sealed for Selection<S, Span> {}

/// How the release reaches a selection's children.
///
/// The recursive positions are the two selection sets — a field's optional one and an inline
/// fragment's required one — and they are the only children pushed. Everything else an arm owns is
/// released here, which the loop's invariant requires to be a leaf: a name, a type condition, an
/// argument list and a directive list hold *values* and *types*, never a selection, and both of
/// those already carry their own iterative release. What they can still own is a node a caller
/// stored in `S` or in `Span`, which no implementation of this trait can push — see [`Nested`]'s
/// documentation and `al8n/smear#176`.
///
/// The match has no wildcard arm even though this enum is `#[non_exhaustive]`, which it can do
/// because the impl is inside the defining crate: a fourth variant is a compile error here rather
/// than a silent return to recursing.
// ── THE RANK EDGES, ASSERTED ────────────────────────────────────────────────────────────────────
//
// A selection hands over only the nested selection sets. Everything else it destructures is
// dropped **in place**: the alias and the name are leaves; the arguments and directives hold
// `InputValue` nodes; and an inline fragment's type condition holds a `Type` node. This dialect
// therefore has edges into both lower trees.
//
// The invariant on `Nestable::into_children` says why that is allowed: a node of a *lower-ranked*
// tree releases iteratively on its own, so dropping one here costs *this* release O(1) native
// frames however deep it is — its recursive positions sit behind its own containers, not behind
// this call. (A caller's own payload destructor is a separate cost that `Leaf` does not bound;
// see `Nestable::into_children`.)
//
// Both directions are checked rather than trusted. One concrete instantiation each is enough:
// `RANK` does not depend on the payload parameters.
const _: () = assert!(
  <super::InputValue<&'static str> as Nestable>::RANK < <Selection<&'static str> as Nestable>::RANK,
  "a selection drops value nodes in place, so the value tree must rank below the selection tree",
);

const _: () = assert!(
  <Type<&'static str> as Nestable>::RANK < <Selection<&'static str> as Nestable>::RANK,
  "a selection drops a type condition in place, so the type tree must rank below the selection tree",
);

impl<S, Span> Nestable for Selection<S, Span> {
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
impl<S, Span> NestNode for Selection<S, Span> {
  type Field = Absent<Self>;
  type Entry = Absent<Self>;
}

impl<S, Span> Selection<S, Span> {
  /// Returns the complete selection span.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(field) => field.span(),
      Self::FragmentSpread(spread) => spread.span(),
      Self::InlineFragment(fragment) => fragment.span(),
    }
  }
}

impl<S, Span> AsSpan<Span> for Selection<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for Selection<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(field) => field.into_span(),
      Self::FragmentSpread(spread) => spread.into_span(),
      Self::InlineFragment(fragment) => fragment.into_span(),
    }
  }
}

/// A GraphQLx field (`Alias? Name Arguments? Directives? SelectionSet?`).
pub type Field<S, Span = SimpleSpan> = crate::selection::Field<
  Alias<S, Span>,
  Name<S, Span>,
  Arguments<S, Span>,
  Directives<S, Span>,
  SelectionSet<S, Span>,
  Span,
>;
