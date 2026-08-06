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
pub type Alias<S> = crate::parser::selection::Alias<Name<S>>;

/// A fragment type condition (`on NamedType`).
pub type TypeCondition<S> = crate::parser::selection::TypeCondition<Name<S>>;

/// A named fragment spread (`... FragmentName Directives?`).
pub type FragmentSpread<S> =
  crate::parser::selection::FragmentSpread<FragmentName<S>, Directives<S>>;

/// An inline fragment (`... TypeCondition? Directives? SelectionSet`).
pub type InlineFragment<S> =
  crate::parser::selection::InlineFragment<TypeCondition<S>, Directives<S>, SelectionSet<S>>;

/// A selection set containing one or more selections.
pub type SelectionSet<S> = crate::parser::selection::SelectionSet<Selection<S>>;

/// A GraphQL selection.
///
/// A selection is a field, a named fragment spread, or an inline fragment.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
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
  crate::parser::selection::Field<Alias<S>, Name<S>, Arguments<S>, Directives<S>, SelectionSet<S>>;
