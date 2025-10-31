use logosky::utils::{AsSpan, IntoSpan, Span};

use crate::cst::Padding;

/// CST representation of operation type definition: `query: QueryType`
///
/// Preserves operation type keyword, colon, and named type with all trivia
#[derive(Debug, Clone)]
pub struct OperationTypeDefinition<
  OperationType,
  NamedType,
  S,
  TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>,
> {
  span: Span,
  operation_type: OperationType,
  colon_padding: Padding<S, TriviaContainer>,
  named_type: NamedType,
}

impl<OperationType, NamedType, S, TriviaContainer>
  OperationTypeDefinition<OperationType, NamedType, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.colon_padding
  }
  pub const fn named_type(&self) -> &NamedType {
    &self.named_type
  }
}

impl<OperationType, NamedType, S, TriviaContainer> AsSpan<Span>
  for OperationTypeDefinition<OperationType, NamedType, S, TriviaContainer>
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<OperationType, NamedType, S, TriviaContainer> IntoSpan<Span>
  for OperationTypeDefinition<OperationType, NamedType, S, TriviaContainer>
{
  fn into_span(self) -> Span {
    self.span
  }
}
