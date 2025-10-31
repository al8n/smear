use core::marker::PhantomData;
use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of input fields definition: `{ field1 field2 ... }`
#[derive(Debug, Clone)]
pub struct InputFieldsDefinition<
  InputValueDef,
  S,
  TriviaContainer = Vec<crate::cst::Trivia<S>>,
  Container = Vec<InputValueDef>,
> {
  span: Span,
  lbrace_padding: Padding<S, TriviaContainer>,
  fields: Container,
  rbrace_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<InputValueDef>,
}

impl<InputValueDef, S, TriviaContainer, Container>
  InputFieldsDefinition<InputValueDef, S, TriviaContainer, Container>
{
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn lbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lbrace_padding
  }
  pub const fn input_value_definitions(&self) -> &Container {
    &self.fields
  }
  pub const fn rbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rbrace_padding
  }
}

impl<InputValueDef, S, TriviaContainer, Container> AsSpan<Span>
  for InputFieldsDefinition<InputValueDef, S, TriviaContainer, Container>
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<InputValueDef, S, TriviaContainer, Container> IntoSpan<Span>
  for InputFieldsDefinition<InputValueDef, S, TriviaContainer, Container>
{
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValueDef, S, TriviaContainer, Container> IntoComponents
  for InputFieldsDefinition<InputValueDef, S, TriviaContainer, Container>
{
  type Components = (
    Span,
    Padding<S, TriviaContainer>,
    Container,
    Padding<S, TriviaContainer>,
  );

  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.lbrace_padding,
      self.fields,
      self.rbrace_padding,
    )
  }
}
