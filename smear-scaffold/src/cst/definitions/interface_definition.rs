use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of interface type definition
#[derive(Debug, Clone)]
pub struct InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  interface_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer>
  InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn implements(&self) -> Option<&Implements> { self.implements.as_ref() }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn fields_definition(&self) -> Option<&Fields> { self.fields_definition.as_ref() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> AsSpan<Span>
  for InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> IntoSpan<Span>
  for InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
