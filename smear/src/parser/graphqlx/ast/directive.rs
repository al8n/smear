use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{container::Container as ChumskyContainer, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{
  keywords,
  punctuator::At,
  scaffold::{self, And, DirectiveLocations, Location},
};

use super::*;

type DirectiveDefinitionAlias<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
> = scaffold::DirectiveDefinition<
  DefinitionName<S, ParametersContainer>,
  ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>,
  And<
    DirectiveLocations<Location, LocationsContainer>,
    Option<WhereClause<S, IdentsContainer, TypeContainer, TypePathsContainer, PredicatesContainer>>,
  >,
>;

#[derive(Debug, Clone, From, Into)]
pub struct DirectiveDefinition<
  S,
  ParametersContainer = DefaultDefinitionTypeParamsContainer<S>,
  IdentsContainer = DefaultIdentsContainer<S>,
  TypeContainer = DefaultTypeContainer<S>,
  TypePathsContainer = DefaultTypePathsContainer<S, IdentsContainer, TypeContainer>,
  PredicatesContainer = DefaultWherePredicatesContainer<
    S,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
  >,
  ArgumentsContainer = DefaultConstArgumentsContainer<S>,
  DirectivesContainer = DefaultConstDirectivesContainer<S, ArgumentsContainer>,
  InputValuesContainer = DefaultInputValuesContainer<S, ArgumentsContainer, DirectivesContainer>,
  LocationsContainer = DefaultLocationsContainer,
>(
  DirectiveDefinitionAlias<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >,
);

impl<
  S,
  ParametersContainer,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  LocationsContainer,
> AsSpan<Span>
  for DirectiveDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >
where
  ParametersContainer: ChumskyContainer<DefinitionTypeParam<S>>,
  IdentsContainer: ChumskyContainer<Ident<S>>,
  TypeContainer: ChumskyContainer<Type<S>>,
  TypePathsContainer: ChumskyContainer<TypePath<Ident<S>, IdentsContainer, TypeContainer>>,
  PredicatesContainer:
    ChumskyContainer<WherePredicate<S, IdentsContainer, TypeContainer, TypePathsContainer>>,
  ArgumentsContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  DirectivesContainer: ChumskyContainer<Directive<S, ArgumentsContainer>>,
  InputValuesContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  LocationsContainer: ChumskyContainer<Location>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<
  S,
  ParametersContainer,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  LocationsContainer,
> IntoSpan<Span>
  for DirectiveDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >
where
  ParametersContainer: ChumskyContainer<DefinitionTypeParam<S>>,
  IdentsContainer: ChumskyContainer<Ident<S>>,
  TypeContainer: ChumskyContainer<Type<S>>,
  TypePathsContainer: ChumskyContainer<TypePath<Ident<S>, IdentsContainer, TypeContainer>>,
  PredicatesContainer:
    ChumskyContainer<WherePredicate<S, IdentsContainer, TypeContainer, TypePathsContainer>>,
  ArgumentsContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  DirectivesContainer: ChumskyContainer<Directive<S, ArgumentsContainer>>,
  InputValuesContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  LocationsContainer: ChumskyContainer<Location>,
{
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<
  S,
  ParametersContainer,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  LocationsContainer,
> IntoComponents
  for DirectiveDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >
where
  ParametersContainer: ChumskyContainer<DefinitionTypeParam<S>>,
  IdentsContainer: ChumskyContainer<Ident<S>>,
  TypeContainer: ChumskyContainer<Type<S>>,
  TypePathsContainer: ChumskyContainer<TypePath<Ident<S>, IdentsContainer, TypeContainer>>,
  PredicatesContainer:
    ChumskyContainer<WherePredicate<S, IdentsContainer, TypeContainer, TypePathsContainer>>,
  ArgumentsContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  DirectivesContainer: ChumskyContainer<Directive<S, ArgumentsContainer>>,
  InputValuesContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  LocationsContainer: ChumskyContainer<Location>,
{
  type Components = (
    Span,
    DefinitionName<S, ParametersContainer>,
    Option<ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>>,
    bool,
    DirectiveLocations<Location, LocationsContainer>,
    Option<WhereClause<S, IdentsContainer, TypeContainer, TypePathsContainer, PredicatesContainer>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, arguments, repeatable, locations_and_where) = self.0.into_components();
    let (_, locations, where_clause) = locations_and_where.into_components();
    (span, name, arguments, repeatable, locations, where_clause)
  }
}

impl<
  S,
  ParametersContainer,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  LocationsContainer,
>
  DirectiveDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >
where
  ParametersContainer: ChumskyContainer<DefinitionTypeParam<S>>,
  IdentsContainer: ChumskyContainer<Ident<S>>,
  TypeContainer: ChumskyContainer<Type<S>>,
  TypePathsContainer: ChumskyContainer<TypePath<Ident<S>, IdentsContainer, TypeContainer>>,
  PredicatesContainer:
    ChumskyContainer<WherePredicate<S, IdentsContainer, TypeContainer, TypePathsContainer>>,
  ArgumentsContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  DirectivesContainer: ChumskyContainer<Directive<S, ArgumentsContainer>>,
  InputValuesContainer:
    ChumskyContainer<InputValueDefinition<S, ArgumentsContainer, DirectivesContainer>>,
  LocationsContainer: ChumskyContainer<Location>,
{
  /// Returns the span of the directive definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the name of the directive definition.
  #[inline]
  pub const fn name(&self) -> &DefinitionName<S, ParametersContainer> {
    self.0.name()
  }

  /// Returns the arguments definition of the directive definition, if any.
  #[inline]
  pub const fn arguments_definition(
    &self,
  ) -> Option<&ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>>
  {
    self.0.arguments_definition()
  }

  /// Returns `true` if the directive definition is repeatable.
  #[inline]
  pub const fn repeatable(&self) -> bool {
    self.0.repeatable()
  }

  /// Returns the where clause of the directive definition, if any.
  #[inline]
  pub const fn where_clause(
    &self,
  ) -> Option<
    &WhereClause<S, IdentsContainer, TypeContainer, TypePathsContainer, PredicatesContainer>,
  > {
    self.0.locations().second().as_ref()
  }

  /// Returns the directive locations of the directive definition.
  #[inline]
  pub const fn locations(&self) -> &DirectiveLocations<Location, LocationsContainer> {
    self.0.locations().first()
  }
}

impl<
  'a,
  S,
  ParametersContainer,
  IdentsContainer,
  TypeContainer,
  TypePathsContainer,
  PredicatesContainer,
  ArgumentsContainer,
  DirectivesContainer,
  InputValuesContainer,
  LocationsContainer,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for DirectiveDefinition<
    S,
    ParametersContainer,
    IdentsContainer,
    TypeContainer,
    TypePathsContainer,
    PredicatesContainer,
    ArgumentsContainer,
    DirectivesContainer,
    InputValuesContainer,
    LocationsContainer,
  >
where
  At: Parseable<'a, I, T, Error>,
  keywords::Directive: Parseable<'a, I, T, Error>,
  keywords::On: Parseable<'a, I, T, Error>,
  keywords::Repeatable: Parseable<'a, I, T, Error>,
  DirectiveLocations<Location, LocationsContainer>: Parseable<'a, I, T, Error>,
  WhereClause<S, IdentsContainer, TypeContainer, TypePathsContainer, PredicatesContainer>:
    Parseable<'a, I, T, Error>,
  DefinitionName<S, ParametersContainer>: Parseable<'a, I, T, Error>,
  ArgumentsDefinition<S, ArgumentsContainer, DirectivesContainer, InputValuesContainer>:
    Parseable<'a, I, T, Error>,
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
    scaffold::DirectiveDefinition::parser().map(Self)
  }
}
