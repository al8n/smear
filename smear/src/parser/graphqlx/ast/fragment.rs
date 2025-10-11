use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{
  keywords,
  punctuator::{Ampersand, Bang, Colon, LAngle, PathSeparator, RAngle},
  scaffold::{self, And, generic::Constrained},
};

use super::*;

pub type TypeCondition<S> = scaffold::TypeCondition<scaffold::generic::TypePath<Ident<S>, Type<S>>>;
pub type RcTypeCondition<S> =
  scaffold::TypeCondition<scaffold::generic::TypePath<Ident<S>, RcType<S>>>;
pub type ArcTypeCondition<S> =
  scaffold::TypeCondition<scaffold::generic::TypePath<Ident<S>, ArcType<S>>>;

pub type FragmentTypePath<S> = scaffold::generic::FragmentTypePath<Ident<S>, Type<S>>;

pub type FragmentRcTypePath<S> = scaffold::generic::FragmentTypePath<Ident<S>, RcType<S>>;

pub type FragmentArcTypePath<S> = scaffold::generic::FragmentTypePath<Ident<S>, ArcType<S>>;

type FragmentDefinitionAlias<S> = scaffold::FragmentDefinition<
  And<Option<ExecutableDefinitionTypeGenerics<S>>, ExecutableDefinitionName<S>>,
  TypeCondition<S>,
  Directives<S>,
  Constrained<Ident<S>, Type<S>, SelectionSet<S>>,
>;

#[derive(Debug, Clone, From, Into)]
pub struct FragmentDefinition<S>(FragmentDefinitionAlias<S>);

impl<S> AsSpan<Span> for FragmentDefinition<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for FragmentDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for FragmentDefinition<S> {
  type Components = (
    Span,
    Option<ExecutableDefinitionTypeGenerics<S>>,
    ExecutableDefinitionName<S>,
    TypeCondition<S>,
    Option<Directives<S>>,
    Option<WhereClause<S>>,
    SelectionSet<S>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name_and_generics, type_condition, directives, constrained_selection_set) =
      self.0.into_components();
    let (_, generics, name) = name_and_generics.into_components();
    let (_, where_clause, selection_set) = constrained_selection_set.into_components();
    (
      span,
      generics,
      name,
      type_condition,
      directives,
      where_clause,
      selection_set,
    )
  }
}

impl<S> FragmentDefinition<S> {
  /// Returns a reference to the span covering the entire fragment definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the impl generics of the fragment definition, if any.
  #[inline]
  pub const fn generics(&self) -> Option<&ExecutableDefinitionTypeGenerics<S>> {
    self.0.name().first().as_ref()
  }

  /// Returns a reference to the name of the fragment definition.
  #[inline]
  pub const fn name(&self) -> &ExecutableDefinitionName<S> {
    self.0.name().second()
  }

  /// Returns a reference to the type condition of the fragment definition.
  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition<S> {
    self.0.type_condition()
  }

  /// Returns a reference to the optional directives of the fragment definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the fragment definition.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<S> {
    self.0.selection_set().target()
  }

  /// Returns a reference to the optional where clause of the fragment definition.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S>> {
    self.0.selection_set().where_clause()
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for FragmentDefinition<S>
where
  keywords::Fragment: Parseable<'a, I, T, Error>,
  keywords::On: Parseable<'a, I, T, Error>,
  keywords::Where: Parseable<'a, I, T, Error>,
  PathSeparator: Parseable<'a, I, T, Error>,
  Ampersand: Parseable<'a, I, T, Error>,
  LAngle: Parseable<'a, I, T, Error>,
  RAngle: Parseable<'a, I, T, Error>,
  Bang: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error>,
  Ident<S>: Parseable<'a, I, T, Error>,
  Type<S>: Parseable<'a, I, T, Error>,
  ExecutableDefinitionName<S>: Parseable<'a, I, T, Error>,
  ExecutableDefinitionTypeGenerics<S>: Parseable<'a, I, T, Error>,
  Directives<S>: Parseable<'a, I, T, Error>,
  SelectionSet<S>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    scaffold::FragmentDefinition::parser().map(Self)
  }
}
