use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{container::Container as ChumskyContainer, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{
  keywords,
  punctuator::{Bang, LAngle, PathSeparator, RAngle},
  scaffold::{self, And},
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
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
> = scaffold::FragmentDefinition<
  And<
    Option<ExecutableDefinitionTypeGenerics<S, IdentsContainer>>,
    ExecutableDefinitionName<S, IdentsContainer>,
  >,
  TypeCondition<S, IdentsContainer, TypeContainer>,
  Directives<S, ArgumentsContainer>,
  SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>,
>;

#[derive(Debug, Clone, From, Into)]
pub struct FragmentDefinition<
  S,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  ArgumentsContainer = DefaultArgumentsContainer<S>,
  DirectivesContainer = DefaultDirectivesContainer<S, ArgumentsContainer>,
>(
  FragmentDefinitionAlias<
    S,
    IdentsContainer,
    TypeContainer,
    ArgumentsContainer,
    DirectivesContainer,
  >,
);

impl<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer> AsSpan<Span>
  for FragmentDefinition<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer> IntoSpan<Span>
  for FragmentDefinition<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer> IntoComponents
  for FragmentDefinition<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>
{
  type Components = (
    Span,
    Option<ExecutableDefinitionTypeGenerics<S, IdentsContainer>>,
    ExecutableDefinitionName<S, IdentsContainer>,
    TypeCondition<S, IdentsContainer, TypeContainer>,
    Option<Directives<S, ArgumentsContainer>>,
    SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name_and_generics, type_condition, directives, selection_set) =
      self.0.into_components();
    let (_, generics, name) = name_and_generics.into_components();
    (
      span,
      generics,
      name,
      type_condition,
      directives,
      selection_set,
    )
  }
}

impl<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>
  FragmentDefinition<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>
{
  /// Returns a reference to the span covering the entire fragment definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
}

impl<'a, S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer, I, T, Error>
  Parseable<'a, I, T, Error>
  for FragmentDefinition<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>
where
  keywords::Fragment: Parseable<'a, I, T, Error>,
  keywords::On: Parseable<'a, I, T, Error>,
  PathSeparator: Parseable<'a, I, T, Error>,
  LAngle: Parseable<'a, I, T, Error>,
  RAngle: Parseable<'a, I, T, Error>,
  Bang: Parseable<'a, I, T, Error>,
  Ident<S>: Parseable<'a, I, T, Error>,
  Type<S>: Parseable<'a, I, T, Error>,
  ExecutableDefinitionName<Ident<S>, IdentsContainer>: Parseable<'a, I, T, Error>,
  ExecutableDefinitionTypeGenerics<Ident<S>, IdentsContainer>: Parseable<'a, I, T, Error>,
  Directives<S, ArgumentsContainer>: Parseable<'a, I, T, Error>,
  SelectionSet<S, IdentsContainer, TypeContainer, ArgumentsContainer, DirectivesContainer>:
    Parseable<'a, I, T, Error>,
  IdentsContainer: ChumskyContainer<Ident<S>>,
  TypeContainer: ChumskyContainer<Type<S>>,
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
