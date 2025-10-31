use core::marker::PhantomData;
use logosky::utils::{AsSpan, IntoSpan, Span};
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of operation definition (query, mutation, subscription)
#[derive(Debug, Clone)]
pub struct OperationDefinition<
  OperationType,
  Name,
  Variables,
  Directives,
  SelectionSet,
  S,
  TriviaContainer = Vec<crate::cst::Trivia<S>>,
> {
  span: Span,
  operation_type: Option<OperationType>,
  name: Option<Name>,
  variable_definitions: Option<Variables>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer>
  OperationDefinition<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn operation_type(&self) -> Option<&OperationType> {
    self.operation_type.as_ref()
  }
  pub const fn name(&self) -> Option<&Name> {
    self.name.as_ref()
  }
  pub const fn variable_definitions(&self) -> Option<&Variables> {
    self.variable_definitions.as_ref()
  }
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }
}

impl<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer> AsSpan<Span>
  for OperationDefinition<
    OperationType,
    Name,
    Variables,
    Directives,
    SelectionSet,
    S,
    TriviaContainer,
  >
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer> IntoSpan<Span>
  for OperationDefinition<
    OperationType,
    Name,
    Variables,
    Directives,
    SelectionSet,
    S,
    TriviaContainer,
  >
{
  fn into_span(self) -> Span {
    self.span
  }
}
