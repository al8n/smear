use derive_more::{From, Into};
use logosky::{
  LogoStream, Logos, Source, Token,
  chumsky::{Parseable, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};
use smear_lexer::{
  keywords,
  punctuator::{Ampersand, Bang, Colon, LAngle, PathSeparator, RAngle},
};
use smear_scaffold::ast::{self as scaffold, And, generic::Constrained};

use super::*;

type TypeConditionAlias<S, Ty = Type<S>> =
  scaffold::TypeCondition<scaffold::generic::TypePath<Ident<S>, Ty>>;
/// A type path for a fragment reference with optional type parameters.
pub type FragmentTypePath<S, Ty = Type<S>> = scaffold::generic::FragmentTypePath<Ident<S>, Ty>;

/// A type condition specifying the type on which a fragment operates.
#[derive(Debug, Clone, From, Into)]
pub struct TypeCondition<S, Ty = Type<S>>(TypeConditionAlias<S, Ty>);

impl<S, Type> AsSpan<Span> for TypeCondition<S, Type> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Type> IntoSpan<Span> for TypeCondition<S, Type> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Type> IntoComponents for TypeCondition<S, Type> {
  type Components = (Span, scaffold::generic::TypePath<Ident<S>, Type>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Type> TypeCondition<S, Type> {
  #[inline]
  pub(super) const fn new(span: Span, name: scaffold::generic::TypePath<Ident<S>, Type>) -> Self {
    Self(TypeConditionAlias::new(span, name))
  }

  /// Returns a reference to the span covering the entire type condition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the type condition.
  #[inline]
  pub const fn path(&self) -> &super::ty::Path<S> {
    self.0.name().path()
  }

  /// Returns the type generics of the type condition, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&scaffold::generic::TypeGenerics<Type>> {
    self.0.name().type_generics()
  }
}

impl<'a, S, Type, I, T, Error> Parseable<'a, I, T, Error> for TypeCondition<S, Type>
where
  TypeConditionAlias<S, Type>: Parseable<'a, I, T, Error>,
  T: Token<'a>,
  I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
  Error: 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    scaffold::TypeCondition::parser().map(Self)
  }
}

type FragmentDefinitionAlias<S, Ty = Type<S>> = scaffold::FragmentDefinition<
  And<Option<ExecutableDefinitionTypeGenerics<S>>, ExecutableDefinitionName<S>>,
  TypeCondition<S, Ty>,
  Directives<S, Ty>,
  Constrained<Ident<S>, Ty, SelectionSet<S, Ty>>,
>;

/// A GraphQLx fragment definition.
#[derive(Debug, Clone, From, Into)]
pub struct FragmentDefinition<S, Ty = Type<S>>(FragmentDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for FragmentDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for FragmentDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for FragmentDefinition<S, Ty> {
  type Components = (
    Span,
    Option<ExecutableDefinitionTypeGenerics<S>>,
    ExecutableDefinitionName<S>,
    TypeCondition<S, Ty>,
    Option<Directives<S, Ty>>,
    Option<WhereClause<S, Ty>>,
    SelectionSet<S, Ty>,
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

impl<S, Ty> FragmentDefinition<S, Ty> {
  /// Returns a reference to the span covering the entire fragment definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the impl generics of the fragment definition, if any.
  #[inline]
  pub const fn impl_generics(&self) -> Option<&ExecutableDefinitionTypeGenerics<S>> {
    self.0.name().first().as_ref()
  }

  /// Returns a reference to the name of the fragment definition.
  #[inline]
  pub const fn name(&self) -> &Ident<S> {
    self.0.name().second().ident()
  }

  /// Returns a reference to the name of the fragment definition.
  #[inline]
  pub const fn type_generics(&self) -> Option<&ExecutableDefinitionTypeGenerics<S>> {
    self.0.name().second().generics()
  }

  /// Returns a reference to the type condition of the fragment definition.
  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition<S, Ty> {
    self.0.type_condition()
  }

  /// Returns a reference to the optional directives of the fragment definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the fragment definition.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<S, Ty> {
    self.0.selection_set().target()
  }

  /// Returns a reference to the optional where clause of the fragment definition.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    self.0.selection_set().where_clause()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for FragmentDefinition<S, Ty>
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
  Ident<S>: Parseable<'a, I, T, Error> + 'a,
  Ty: Parseable<'a, I, T, Error> + 'a,
  ExecutableDefinitionName<S>: Parseable<'a, I, T, Error> + 'a,
  ExecutableDefinitionTypeGenerics<S>: Parseable<'a, I, T, Error> + 'a,
  Directives<S, Ty>: Parseable<'a, I, T, Error> + 'a,
  SelectionSet<S, Ty>: Parseable<'a, I, T, Error> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    scaffold::FragmentDefinition::parser().map(Self)
  }
}
