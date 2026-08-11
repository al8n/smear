//! GraphQLx selection and field AST aliases.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{Arguments, DefaultVec, Directives, FragmentTypePath, Name, Type, TypePath};

/// A GraphQLx field alias (`Name :`).
pub type Alias<S, Span = SimpleSpan> = crate::parser::selection::Alias<Name<S, Span>, Span>;

/// A GraphQLx fragment type condition with a path and optional type arguments.
pub type TypeCondition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::selection::TypeCondition<TypePath<S, Span, Ty>, Span>;

/// A named GraphQLx fragment spread with a path and optional type arguments.
pub type FragmentSpread<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::selection::FragmentSpread<
    FragmentTypePath<S, Span, Ty>,
    Directives<S, Span>,
    Span,
  >;

/// An inline GraphQLx fragment.
pub type InlineFragment<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::parser::selection::InlineFragment<
    TypeCondition<S, Span, Ty>,
    Directives<S, Span>,
    SelectionSet<S, Span>,
    Span,
  >;

/// A GraphQLx selection set containing one or more selections.
pub type SelectionSet<S, Span = SimpleSpan, Container = DefaultVec<Selection<S, Span>>> =
  crate::parser::selection::SelectionSet<Selection<S, Span>, Span, Container>;

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
pub type Field<S, Span = SimpleSpan> = crate::parser::selection::Field<
  Alias<S, Span>,
  Name<S, Span>,
  Arguments<S, Span>,
  Directives<S, Span>,
  SelectionSet<S, Span>,
  Span,
>;
