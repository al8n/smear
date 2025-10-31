use core::marker::PhantomData;
use logosky::utils::{AsSpan, IntoSpan, Span};
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of variable definition: `$var: Type = default`
#[derive(Debug, Clone)]
pub struct VariableDefinition<
  Variable,
  Type,
  DefaultValue,
  Directives,
  S,
  TriviaContainer = Vec<crate::cst::Trivia<S>>,
> {
  span: Span,
  variable: Variable,
  colon_padding: Padding<S, TriviaContainer>,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
  VariableDefinition<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn variable(&self) -> &Variable {
    &self.variable
  }
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.colon_padding
  }
  pub const fn ty(&self) -> &Type {
    &self.ty
  }
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

impl<Variable, Type, DefaultValue, Directives, S, TriviaContainer> AsSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Variable, Type, DefaultValue, Directives, S, TriviaContainer> IntoSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span {
    self.span
  }
}
