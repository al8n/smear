use super::*;

use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::extra::ParserExtra,
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type NamedOperationDefinitionAlias<S, Ty = Type<S>> = scaffold::NamedOperationDefinition<
  DefinitionName<S, Ty>,
  OperationType,
  VariablesDefinition<S, Ty>,
  Directives<S, Ty>,
  scaffold::generic::Constrained<Ident<S>, Ty, SelectionSet<S, Ty>>,
>;

#[derive(Debug, Clone, From, Into)]
pub struct NamedOperationDefinition<S, Ty = Type<S>>(NamedOperationDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for NamedOperationDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for NamedOperationDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for NamedOperationDefinition<S, Ty> {
  type Components = (
    Span,
    Option<Ident<S>>,
    Option<DefinitionTypeGenerics<S, Ty>>,
    OperationType,
    Option<VariablesDefinition<S, Ty>>,
    Option<Directives<S, Ty>>,
    Option<WhereClause<S, Ty>>,
    SelectionSet<S, Ty>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, op_type, name, var_defs, directives, selection_set) = self.0.into_components();
    let (name, generics) = match name {
      Some(def_name) => {
        let (_, name, generics) = def_name.into_components();
        (Some(name), generics)
      }
      None => (None, None),
    };
    let (_, where_clause, selection_set) = selection_set.into_components();
    (
      span,
      name,
      generics,
      op_type,
      var_defs,
      directives,
      where_clause,
      selection_set,
    )
  }
}

impl<S, Ty> NamedOperationDefinition<S, Ty> {
  /// Returns the name of the operation definition, if it exists.
  #[inline]
  pub const fn name(&self) -> Option<&Ident<S>> {
    match self.0.name() {
      Some(def_name) => Some(def_name.name()),
      None => None,
    }
  }

  /// Returns the definition type generics, if they exist.
  #[inline]
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S, Ty>> {
    match self.0.name() {
      Some(def_name) => def_name.generics(),
      None => None,
    }
  }

  /// Returns the operation type.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    self.0.operation_type()
  }

  /// Returns the optional variable definitions of the operation.
  #[inline]
  pub const fn variable_definitions(&self) -> Option<&VariablesDefinition<S, Ty>> {
    self.0.variable_definitions()
  }

  /// Returns the optional directives of the operation.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the optional where clause of the operation.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    self.0.selection_set().where_clause()
  }

  /// Returns the selection set of the operation.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<S, Ty> {
    self.0.selection_set().target()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for NamedOperationDefinition<S, Ty>
where
  NamedOperationDefinitionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    NamedOperationDefinitionAlias::parser::<E>().map(Self)
  }
}

type RootOperationTypeDefinitionAlias<S, Ty = Type<S>> =
  scaffold::RootOperationTypeDefinition<TypePath<S, Ty>, OperationType>;

#[derive(Debug, Clone, From, Into)]
pub struct RootOperationTypeDefinition<S, Ty = Type<S>>(RootOperationTypeDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for RootOperationTypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for RootOperationTypeDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for RootOperationTypeDefinition<S, Ty> {
  type Components = (
    Span,
    OperationType,
    super::ty::Path<S>,
    Option<scaffold::generic::TypeGenerics<Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, operation_type, name) = self.0.into_components();
    let (_, path, generics) = name.into_components();
    (span, operation_type, path, generics)
  }
}

impl<S, Ty> RootOperationTypeDefinition<S, Ty> {
  /// Returns the operation type (e.g., Query, Mutation, Subscription).
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the operation type (e.g., Query, Mutation, Subscription).
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    self.0.operation_type()
  }

  /// Returns the path of the root operation type definition.
  #[inline]
  pub const fn path(&self) -> &super::ty::Path<S> {
    self.0.name().path()
  }

  /// Returns the optional type generics of the root operation type definition.
  #[inline]
  pub const fn type_generics(&self) -> Option<&scaffold::generic::TypeGenerics<Ty>> {
    self.0.name().type_generics()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for RootOperationTypeDefinition<S, Ty>
where
  RootOperationTypeDefinitionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    RootOperationTypeDefinitionAlias::parser::<E>().map(Self)
  }
}
