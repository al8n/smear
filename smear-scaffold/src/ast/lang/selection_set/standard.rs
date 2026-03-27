use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

use crate::ast::{FragmentSpread, InlineFragment, SelectionSet, StandardField};

/// A standard selection set in GraphQL.
pub type StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> =
  SelectionSet<StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>>;

/// Represents a standard selection in a GraphQL selection set.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> {
  /// A field selection.
  Field(StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>),
  /// A fragment spread selection.
  FragmentSpread(FragmentSpread<FragmentName, Directives>),
  /// An inline fragment selection.
  InlineFragment(
    InlineFragment<
      TypeCondition,
      Directives,
      StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>,
    >,
  ),
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> AsSpan<Span>
  for StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> IntoSpan<Span>
  for StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(f) => f.into_span(),
      Self::FragmentSpread(fs) => fs.into_span(),
      Self::InlineFragment(ifr) => ifr.into_span(),
    }
  }
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
  StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  /// Returns the span of the selection.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(f) => f.span(),
      Self::FragmentSpread(fs) => fs.span(),
      Self::InlineFragment(ifr) => ifr.span(),
    }
  }
}
