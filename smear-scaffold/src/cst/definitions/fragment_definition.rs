use core::marker::PhantomData;
use logosky::utils::{AsSpan, IntoSpan, Span};
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of fragment definition
#[derive(Debug, Clone)]
pub struct FragmentDefinition<
  FragmentName,
  TypeCondition,
  Directives,
  SelectionSet,
  S,
  TriviaContainer = Vec<crate::cst::Trivia<S>>,
> {
  span: Span,
  fragment_keyword_padding: Padding<S, TriviaContainer>,
  fragment_name: FragmentName,
  type_condition: TypeCondition,
  directives: Option<Directives>,
  selection_set: SelectionSet,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
  FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn fragment_name(&self) -> &FragmentName {
    &self.fragment_name
  }
  pub const fn type_condition(&self) -> &TypeCondition {
    &self.type_condition
  }
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer> AsSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer> IntoSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  fn into_span(self) -> Span {
    self.span
  }
}
