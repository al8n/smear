use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{container::Container as ChumskyContainer, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{
  keywords,
  punctuator::{Ampersand, Bang, Colon, LAngle, PathSeparator, RAngle},
  scaffold::{self, And, generic::Constrained},
};

use super::*;

pub type TypeCondition<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<Type<S>>,
> = scaffold::TypeCondition<
  scaffold::generic::TypePath<Ident<S>, Type<S>, PathSegmentContainer, TypeContainer>,
>;
pub type RcTypeCondition<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<RcType<S>>,
> = scaffold::TypeCondition<
  scaffold::generic::TypePath<Ident<S>, RcType<S>, PathSegmentContainer, TypeContainer>,
>;
pub type ArcTypeCondition<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<ArcType<S>>,
> = scaffold::TypeCondition<
  scaffold::generic::TypePath<Ident<S>, ArcType<S>, PathSegmentContainer, TypeContainer>,
>;

pub type FragmentTypePath<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<Type<S>>,
> = scaffold::generic::FragmentTypePath<Ident<S>, Type<S>, PathSegmentContainer, TypeContainer>;

pub type FragmentRcTypePath<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<RcType<S>>,
> = scaffold::generic::FragmentTypePath<Ident<S>, RcType<S>, PathSegmentContainer, TypeContainer>;

pub type FragmentArcTypePath<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<ArcType<S>>,
> = scaffold::generic::FragmentTypePath<Ident<S>, ArcType<S>, PathSegmentContainer, TypeContainer>;

type FragmentDefinitionAlias<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::FragmentDefinition<
  And<
    Option<ExecutableDefinitionTypeGenerics<S, IdentsContainer>>,
    ExecutableDefinitionName<S, IdentsContainer>,
  >,
  TypeCondition<S, IdentsContainer, TypeContainer>,
  Directives<S, ArgumentsContainer>,
  Constrained<
    Ident<S>,
    Type<S>,
    SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
    PredicatesContainer,
  >,
>;

#[derive(Debug, Clone, From, Into)]
pub struct FragmentDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
  >,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
>(
  FragmentDefinitionAlias<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
);

impl<
  S,
  IdentsContainer,
  TypeContainer,
  TypePathContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
> AsSpan<Span>
  for FragmentDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<
  S,
  IdentsContainer,
  TypeContainer,
  TypePathContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
> IntoSpan<Span>
  for FragmentDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >
{
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<
  S,
  IdentsContainer,
  TypeContainer,
  TypePathContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
> IntoComponents
  for FragmentDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >
{
  type Components = (
    Span,
    Option<ExecutableDefinitionTypeGenerics<S, IdentsContainer>>,
    ExecutableDefinitionName<S, IdentsContainer>,
    TypeCondition<S, IdentsContainer, TypeContainer>,
    Option<Directives<S, ArgumentsContainer>>,
    Option<WhereClause<S, IdentsContainer, TypeContainer, TypePathContainer, PredicatesContainer>>,
    SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>,
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

impl<
  S,
  IdentsContainer,
  TypeContainer,
  TypePathContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
>
  FragmentDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >
{
  /// Returns a reference to the span covering the entire fragment definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the impl generics of the fragment definition, if any.
  #[inline]
  pub const fn generics(&self) -> Option<&ExecutableDefinitionTypeGenerics<S, IdentsContainer>> {
    self.0.name().first().as_ref()
  }

  /// Returns a reference to the name of the fragment definition.
  #[inline]
  pub const fn name(&self) -> &ExecutableDefinitionName<S, IdentsContainer> {
    self.0.name().second()
  }

  /// Returns a reference to the type condition of the fragment definition.
  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition<S, IdentsContainer, TypeContainer> {
    self.0.type_condition()
  }

  /// Returns a reference to the optional directives of the fragment definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, ArgumentsContainer>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the fragment definition.
  #[inline]
  pub const fn selection_set(
    &self,
  ) -> &SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer> {
    self.0.selection_set().target()
  }

  /// Returns a reference to the optional where clause of the fragment definition.
  #[inline]
  pub const fn where_clause(
    &self,
  ) -> Option<&WhereClause<S, IdentsContainer, TypeContainer, TypePathContainer, PredicatesContainer>>
  {
    self.0.selection_set().where_clause()
  }
}

impl<
  'a,
  S,
  IdentsContainer,
  TypeContainer,
  TypePathContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for FragmentDefinition<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >
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
  ExecutableDefinitionName<S, IdentsContainer>: Parseable<'a, I, T, Error>,
  ExecutableDefinitionTypeGenerics<S, IdentsContainer>: Parseable<'a, I, T, Error>,
  Directives<S, ArgumentsContainer>: Parseable<'a, I, T, Error>,
  SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>:
    Parseable<'a, I, T, Error>,
  IdentsContainer: ChumskyContainer<Ident<S>>,
  TypeContainer: ChumskyContainer<Type<S>>,
  TypePathContainer: ChumskyContainer<TypePath<S, IdentsContainer, TypeContainer>>,
  PredicatesContainer:
    ChumskyContainer<WherePredicate<S, IdentsContainer, TypeContainer, TypePathContainer>>,
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
