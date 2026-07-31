use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{ty::Path, *};

type FragmentSpreadAlias<S, Ty = Type<S>> =
  scaffold::FragmentSpread<FragmentTypePath<S, Ty>, Directives<S, Ty>>;

/// A fragment spread in a GraphQLx selection set.
#[derive(Debug, Clone, From, Into)]
pub struct FragmentSpread<S, Ty = Type<S>>(FragmentSpreadAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for FragmentSpread<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for FragmentSpread<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for FragmentSpread<S, Ty> {
  type Components = (
    Span,
    Path<S>,
    Option<scaffold::generic::TypeGenerics<Ty>>,
    Option<Directives<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, directives) = self.0.into_components();
    let (_, path, type_generics) = name.into_components();
    (span, path, type_generics, directives)
  }
}

impl<S, Ty> FragmentSpread<S, Ty> {
  #[inline]
  pub(super) const fn new(
    span: Span,
    name: FragmentTypePath<S, Ty>,
    directives: Option<Directives<S, Ty>>,
  ) -> Self {
    Self(FragmentSpreadAlias::new(span, name, directives))
  }

  /// Returns the span of the fragment spread.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the fragment spread.
  #[inline]
  pub const fn path(&self) -> &Path<S> {
    self.0.name().path()
  }

  /// Returns the type generics of the fragment spread name, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&scaffold::generic::TypeGenerics<Ty>> {
    self.0.name().type_generics()
  }

  /// Returns the directives of the fragment spread.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }
}

type InlineFragmentAlias<S, Ty = Type<S>> =
  scaffold::InlineFragment<TypeCondition<S, Ty>, Directives<S, Ty>, SelectionSet<S, Ty>>;

/// An inline fragment in a GraphQLx selection set.
#[derive(Debug, Clone, From, Into)]
pub struct InlineFragment<S, Ty = Type<S>>(InlineFragmentAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InlineFragment<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for InlineFragment<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for InlineFragment<S, Ty> {
  type Components = (
    Span,
    Option<TypeCondition<S, Ty>>,
    Option<Directives<S, Ty>>,
    SelectionSet<S, Ty>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Ty> InlineFragment<S, Ty> {
  #[inline]
  pub(super) const fn new(
    span: Span,
    type_condition: Option<TypeCondition<S, Ty>>,
    directives: Option<Directives<S, Ty>>,
    selection_set: SelectionSet<S, Ty>,
  ) -> Self {
    Self(InlineFragmentAlias::new(
      span,
      type_condition,
      directives,
      selection_set,
    ))
  }

  /// Returns the span of the inline fragment.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the type condition of the inline fragment, if any.
  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<S, Ty>> {
    self.0.type_condition()
  }

  /// Returns the directives of the inline fragment, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the selection set of the inline fragment.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<S, Ty> {
    self.0.selection_set()
  }
}

/// A selection set containing fields, fragment spreads, and inline fragments.
pub type SelectionSet<S, Ty = Type<S>> = scaffold::SelectionSet<Selection<S, Ty>>;

/// A selection in a GraphQLx selection set.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Selection<S, Ty = Type<S>> {
  /// A field selection.
  Field(Field<S, Ty>),
  /// A fragment spread selection.
  FragmentSpread(FragmentSpread<S, Ty>),
  /// An inline fragment selection.
  InlineFragment(InlineFragment<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for Selection<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for Selection<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(f) => f.into_span(),
      Self::FragmentSpread(fs) => fs.into_span(),
      Self::InlineFragment(ifr) => ifr.into_span(),
    }
  }
}

impl<S, Ty> Selection<S, Ty> {
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

type FieldAlias<S, Ty = Type<S>> =
  scaffold::Field<Alias<S>, Ident<S>, Arguments<S>, Directives<S, Ty>, SelectionSet<S, Ty>>;

/// A field in a GraphQLx selection set.
#[derive(Debug, Clone, From, Into)]
pub struct Field<S, Ty = Type<S>>(FieldAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for Field<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for Field<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for Field<S, Ty> {
  type Components = (
    Span,
    Option<Alias<S>>,
    Ident<S>,
    Option<Arguments<S>>,
    Option<Directives<S, Ty>>,
    Option<SelectionSet<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Ty> Field<S, Ty> {
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
  pub const fn name(&self) -> &Ident<S> {
    self.0.name()
  }

  /// Returns a reference to the arguments of the field, if any.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<S>> {
    self.0.arguments()
  }

  /// Returns a reference to the directives of the field, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the field, if any.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet<S, Ty>> {
    self.0.selection_set()
  }
}
