//! GraphQL selection AST node types.
//!
//! Shared selection carriers live at crate level so GraphQLx can bind the same
//! structures later. This module specializes them to GraphQL names, executable
//! arguments, and executable directives.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::{Arguments, Directives, FragmentName, Name};

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
pub type SelectionSet<S> = crate::selection::SelectionSet<Selection<S>>;

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
