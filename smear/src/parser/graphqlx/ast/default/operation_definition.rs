use super::*;

use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::extra::ParserExtra,
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type NamedOperationDefinitionAlias<S> = scaffold::NamedOperationDefinition<
  DefinitionName<S>,
  OperationType,
  VariablesDefinition<S>,
  Directives<S>,
  scaffold::generic::Constrained<Ident<S>, Type<S>, SelectionSet<S>>,
>;

#[derive(Debug, Clone, From, Into)]
pub struct NamedOperationDefinition<S>(NamedOperationDefinitionAlias<S>);

impl<S> AsSpan<Span> for NamedOperationDefinition<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for NamedOperationDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for NamedOperationDefinition<S> {
  type Components = (
    Span,
    Option<Ident<S>>,
    Option<DefinitionTypeGenerics<S>>,
    OperationType,
    Option<VariablesDefinition<S>>,
    Option<Directives<S>>,
    Option<WhereClause<S>>,
    SelectionSet<S>,
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

impl<S> NamedOperationDefinition<S> {
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
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S>> {
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
  pub const fn variable_definitions(&self) -> Option<&VariablesDefinition<S>> {
    self.0.variable_definitions()
  }

  /// Returns the optional directives of the operation.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S>> {
    self.0.directives()
  }

  /// Returns the optional where clause of the operation.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S>> {
    self.0.selection_set().where_clause()
  }

  /// Returns the selection set of the operation.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<S> {
    self.0.selection_set().target()
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for NamedOperationDefinition<S>
where
  NamedOperationDefinitionAlias<S>: Parseable<'a, I, T, Error>,
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
