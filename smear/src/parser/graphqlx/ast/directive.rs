use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{
  keywords,
  punctuator::At,
  scaffold::{self, And, DirectiveLocations, Location},
};

use super::*;

type DirectiveDefinitionAlias<S> = scaffold::DirectiveDefinition<
  DefinitionName<S>,
  ArgumentsDefinition<S>,
  And<DirectiveLocations<Location>, Option<WhereClause<S>>>,
>;

#[derive(Debug, Clone, From, Into)]
pub struct DirectiveDefinition<S>(DirectiveDefinitionAlias<S>);

impl<S> AsSpan<Span> for DirectiveDefinition<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for DirectiveDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for DirectiveDefinition<S> {
  type Components = (
    Span,
    DefinitionName<S>,
    Option<ArgumentsDefinition<S>>,
    bool,
    DirectiveLocations<Location>,
    Option<WhereClause<S>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, arguments, repeatable, locations_and_where) = self.0.into_components();
    let (_, locations, where_clause) = locations_and_where.into_components();
    (span, name, arguments, repeatable, locations, where_clause)
  }
}

impl<S> DirectiveDefinition<S> {
  /// Returns the span of the directive definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the name of the directive definition.
  #[inline]
  pub const fn name(&self) -> &DefinitionName<S> {
    self.0.name()
  }

  /// Returns the arguments definition of the directive definition, if any.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&ArgumentsDefinition<S>> {
    self.0.arguments_definition()
  }

  /// Returns `true` if the directive definition is repeatable.
  #[inline]
  pub const fn repeatable(&self) -> bool {
    self.0.repeatable()
  }

  /// Returns the where clause of the directive definition, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S>> {
    self.0.locations().second().as_ref()
  }

  /// Returns the directive locations of the directive definition.
  #[inline]
  pub const fn locations(&self) -> &DirectiveLocations<Location> {
    self.0.locations().first()
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for DirectiveDefinition<S>
where
  At: Parseable<'a, I, T, Error>,
  keywords::Directive: Parseable<'a, I, T, Error>,
  keywords::On: Parseable<'a, I, T, Error>,
  keywords::Repeatable: Parseable<'a, I, T, Error>,
  DirectiveLocations<Location>: Parseable<'a, I, T, Error>,
  WhereClause<S>: Parseable<'a, I, T, Error>,
  DefinitionName<S>: Parseable<'a, I, T, Error>,
  ArgumentsDefinition<S>: Parseable<'a, I, T, Error>,
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
