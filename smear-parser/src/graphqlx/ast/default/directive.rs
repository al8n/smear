use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};
use smear_lexer::{keywords, punctuator::At};
use smear_scaffold::ast::{self as scaffold, And, DirectiveLocations, Location};

use super::*;

type DirectiveDefinitionAlias<S, Ty = Type<S>> = scaffold::DirectiveDefinition<
  DefinitionName<S, Ty>,
  ArgumentsDefinition<S, Ty>,
  And<DirectiveLocations<Location>, Option<WhereClause<S, Ty>>>,
>;

type DirectiveAlias<S, Ty = Type<S>> = scaffold::Directive<TypePath<S, Ty>, Arguments<S>>;

type ConstDirectiveAlias<S, Ty = Type<S>> = scaffold::Directive<TypePath<S, Ty>, ConstArguments<S>>;

/// A GraphQLx directive.
#[derive(Debug, Clone, From, Into)]
pub struct Directive<S, Ty = Type<S>>(DirectiveAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for Directive<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for Directive<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for Directive<S, Ty> {
  type Components = (Span, TypePath<S, Ty>, Option<Arguments<S>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Ty> Directive<S, Ty> {
  #[inline]
  pub(super) const fn new(
    span: Span,
    name: TypePath<S, Ty>,
    arguments: Option<Arguments<S>>,
  ) -> Self {
    Self(DirectiveAlias::new(span, name, arguments))
  }

  /// Returns the span of the directive.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the directive.
  #[inline]
  pub const fn path(&self) -> &super::ty::Path<S> {
    self.0.name().path()
  }

  /// Returns the type generics of the directive path, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&scaffold::generic::TypeGenerics<Ty>> {
    self.0.name().type_generics()
  }

  /// Returns the arguments of the directive, if any.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<S>> {
    self.0.arguments()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for Directive<S, Ty>
where
  DirectiveAlias<S, Ty>: Parseable<'a, I, T, Error>,
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
    DirectiveAlias::parser().map(Self)
  }
}

/// A const directive (used in type system definitions).
#[derive(Debug, Clone, From, Into)]
pub struct ConstDirective<S, Ty = Type<S>>(ConstDirectiveAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for ConstDirective<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for ConstDirective<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for ConstDirective<S, Ty> {
  type Components = (Span, TypePath<S, Ty>, Option<ConstArguments<S>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Ty> ConstDirective<S, Ty> {
  /// Returns the span of the const directive.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the const directive.
  #[inline]
  pub const fn path(&self) -> &super::ty::Path<S> {
    self.0.name().path()
  }

  /// Returns the type generics of the const directive path, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&scaffold::generic::TypeGenerics<Ty>> {
    self.0.name().type_generics()
  }

  /// Returns the const arguments of the const directive, if any.
  #[inline]
  pub const fn arguments(&self) -> Option<&ConstArguments<S>> {
    self.0.arguments()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for ConstDirective<S, Ty>
where
  ConstDirectiveAlias<S, Ty>: Parseable<'a, I, T, Error>,
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
    ConstDirectiveAlias::parser().map(Self)
  }
}

/// A GraphQLx directive definition.
///
/// Defines a custom directive that can be applied to various locations
/// in a GraphQL schema or query. Directives provide a way to describe
/// additional metadata or execution behavior.
///
/// ## Grammar
///
/// ```text
/// DirectiveDefinition :
///   Description? directive @ Name TypeGenerics? ArgumentsDefinition? repeatable? on DirectiveLocations WhereClause?
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct DirectiveDefinition<S, Ty = Type<S>>(DirectiveDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for DirectiveDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for DirectiveDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for DirectiveDefinition<S, Ty> {
  type Components = (
    Span,
    DefinitionName<S, Ty>,
    Option<ArgumentsDefinition<S, Ty>>,
    bool,
    DirectiveLocations<Location>,
    Option<WhereClause<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, arguments, repeatable, locations_and_where) = self.0.into_components();
    let (_, locations, where_clause) = locations_and_where.into_components();
    (span, name, arguments, repeatable, locations, where_clause)
  }
}

impl<S, Ty> DirectiveDefinition<S, Ty> {
  /// Returns the span of the directive definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the name of the directive definition.
  #[inline]
  pub const fn name(&self) -> &Ident<S> {
    self.0.name().name()
  }

  /// Returns the type generics of the directive definition.
  #[inline]
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S, Ty>> {
    self.0.name().generics()
  }

  /// Returns the arguments definition of the directive definition, if any.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&ArgumentsDefinition<S, Ty>> {
    self.0.arguments_definition()
  }

  /// Returns `true` if the directive definition is repeatable.
  #[inline]
  pub const fn repeatable(&self) -> bool {
    self.0.repeatable()
  }

  /// Returns the where clause of the directive definition, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    self.0.locations().second().as_ref()
  }

  /// Returns the directive locations of the directive definition.
  #[inline]
  pub const fn locations(&self) -> &DirectiveLocations<Location> {
    self.0.locations().first()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for DirectiveDefinition<S, Ty>
where
  At: Parseable<'a, I, T, Error>,
  keywords::Directive: Parseable<'a, I, T, Error>,
  keywords::On: Parseable<'a, I, T, Error>,
  keywords::Repeatable: Parseable<'a, I, T, Error>,
  DirectiveLocations<Location>: Parseable<'a, I, T, Error>,
  WhereClause<S, Ty>: Parseable<'a, I, T, Error>,
  DefinitionName<S, Ty>: Parseable<'a, I, T, Error>,
  ArgumentsDefinition<S, Ty>: Parseable<'a, I, T, Error>,
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
