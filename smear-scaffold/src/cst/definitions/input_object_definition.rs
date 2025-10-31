use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of input object type definition
#[derive(Debug, Clone)]
pub struct InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  input_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  directives: Option<Directives>,
  input_fields_definition: Option<InputFields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Directives, InputFields, S, TriviaContainer>
  InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn input_fields_definition(&self) -> Option<&InputFields> { self.input_fields_definition.as_ref() }
}

impl<Name, Directives, InputFields, S, TriviaContainer> AsSpan<Span>
  for InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Directives, InputFields, S, TriviaContainer> IntoSpan<Span>
  for InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
