use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};
use smear_scaffold::ast as scaffold;

use super::*;

/// A fragment spread in a selection set.
pub type FragmentSpread<S> = scaffold::FragmentSpread<FragmentName<S>, Directives<S>>;

/// An inline fragment in a selection set.
pub type InlineFragment<S> =
  scaffold::InlineFragment<TypeCondition<S>, Directives<S>, SelectionSet<S>>;

/// A selection set containing fields, fragment spreads, and inline fragments.
pub type SelectionSet<S> = scaffold::SelectionSet<Selection<S>>;

/// A selection in a GraphQL selection set.
///
/// A selection can be a field, a fragment spread, or an inline fragment.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Selection<S> {
  /// A field selection.
  Field(Field<S>),
  /// A fragment spread selection.
  FragmentSpread(FragmentSpread<S>),
  /// An inline fragment selection.
  InlineFragment(InlineFragment<S>),
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
      Self::Field(f) => f.into_span(),
      Self::FragmentSpread(fs) => fs.into_span(),
      Self::InlineFragment(ifr) => ifr.into_span(),
    }
  }
}

impl<S> Selection<S> {
  /// Returns a reference to the span covering the entire selection.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(f) => f.span(),
      Self::FragmentSpread(fs) => fs.span(),
      Self::InlineFragment(ifr) => ifr.span(),
    }
  }
}

type FieldAlias<S> =
  scaffold::Field<Alias<S>, Name<S>, Arguments<S>, Directives<S>, SelectionSet<S>>;

/// A GraphQL field.
#[derive(Debug, Clone, From, Into)]
pub struct Field<S>(FieldAlias<S>);

impl<S> AsSpan<Span> for Field<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for Field<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for Field<S> {
  type Components = (
    Span,
    Option<Alias<S>>,
    Name<S>,
    Option<Arguments<S>>,
    Option<Directives<S>>,
    Option<SelectionSet<S>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> Field<S> {
  /// Returns a reference to the span covering the entire field.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns a reference to the alias of the field, if any.
  #[inline]
  pub const fn alias(&self) -> Option<&Alias<S>> {
    self.0.alias()
  }

  /// Returns a reference to the name of the field.
  #[inline]
  pub const fn name(&self) -> &Name<S> {
    self.0.name()
  }

  /// Returns a reference to the arguments of the field, if any.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<S>> {
    self.0.arguments()
  }

  /// Returns a reference to the directives of the field, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the field, if any.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet<S>> {
    self.0.selection_set()
  }
}
