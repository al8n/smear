use logosky::utils::{AsSpan, IntoSpan, Span};

use crate::cst::{Padding, Trivia};

use std::vec::Vec;

/// CST representation of a scalar type definition: `scalar Name`
///
/// Preserves:
/// - The `scalar` keyword with its padding
/// - The name with its padding  
/// - Optional directives
#[derive(Debug, Clone)]
pub struct ScalarTypeDefinition<Name, Directives, S, TriviaContainer = Vec<Trivia<S>>> {
  span: Span,
  scalar_keyword: Padding<S, TriviaContainer>,
  name: Name,
  directives: Option<Directives>,
}

impl<Name, Directives, S, TriviaContainer>
  ScalarTypeDefinition<Name, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn name(&self) -> &Name {
    &self.name
  }
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

impl<Name, Directives, S, TriviaContainer> AsSpan<Span>
  for ScalarTypeDefinition<Name, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, S, TriviaContainer> IntoSpan<Span>
  for ScalarTypeDefinition<Name, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span {
    self.span
  }
}

/// CST representation of a scalar type extension
#[derive(Debug, Clone)]
pub struct ScalarTypeExtension<Name, Directives, S, TriviaContainer = Vec<Trivia<S>>> {
  span: Span,
  extend_keyword: Padding<S, TriviaContainer>,
  scalar_keyword: Padding<S, TriviaContainer>,
  name: Name,
  directives: Directives,
}

impl<Name, Directives, S, TriviaContainer>
  ScalarTypeExtension<Name, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn name(&self) -> &Name {
    &self.name
  }
  pub const fn directives(&self) -> &Directives {
    &self.directives
  }
}

impl<Name, Directives, S, TriviaContainer> AsSpan<Span>
  for ScalarTypeExtension<Name, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, S, TriviaContainer> IntoSpan<Span>
  for ScalarTypeExtension<Name, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span {
    self.span
  }
}
