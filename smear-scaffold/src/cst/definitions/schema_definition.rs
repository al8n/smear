use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of schema definition
#[derive(Debug, Clone)]
pub struct SchemaDefinition<Directives, OperationTypes, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  schema_keyword_padding: Padding<S, TriviaContainer>,
  directives: Option<Directives>,
  operation_types: OperationTypes,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Directives, OperationTypes, S, TriviaContainer>
  SchemaDefinition<Directives, OperationTypes, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn operation_types(&self) -> &OperationTypes { &self.operation_types }
}

impl<Directives, OperationTypes, S, TriviaContainer> AsSpan<Span>
  for SchemaDefinition<Directives, OperationTypes, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Directives, OperationTypes, S, TriviaContainer> IntoSpan<Span>
  for SchemaDefinition<Directives, OperationTypes, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
