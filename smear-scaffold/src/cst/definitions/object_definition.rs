use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of object type definition: `type Name implements Interface { fields }`
#[derive(Debug, Clone)]
pub struct ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  type_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer>
  ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn type_keyword_padding(&self) -> &Padding<S, TriviaContainer> { &self.type_keyword_padding }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn implements(&self) -> Option<&Implements> { self.implements.as_ref() }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn fields_definition(&self) -> Option<&Fields> { self.fields_definition.as_ref() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> AsSpan<Span>
  for ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> IntoSpan<Span>
  for ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}

/// Object extension data
#[derive(Debug, Clone)]
pub enum ObjectTypeExtensionData<Implements, Directives, Fields> {
  Directives { implements: Option<Implements>, directives: Directives },
  Fields { implements: Option<Implements>, directives: Option<Directives>, fields: Fields },
  Implements(Implements),
}

impl<Implements, Directives, Fields> ObjectTypeExtensionData<Implements, Directives, Fields> {
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Directives { implements, .. } | Self::Fields { implements, .. } => implements.as_ref(),
      Self::Implements(i) => Some(i),
    }
  }

  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
      Self::Implements(_) => None,
    }
  }

  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields { fields, .. } => Some(fields),
      _ => None,
    }
  }
}

/// CST representation of object type extension
#[derive(Debug, Clone)]
pub struct ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  extend_keyword_padding: Padding<S, TriviaContainer>,
  type_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  data: ObjectTypeExtensionData<Implements, Directives, Fields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer>
  ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn data(&self) -> &ObjectTypeExtensionData<Implements, Directives, Fields> { &self.data }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> AsSpan<Span>
  for ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> IntoSpan<Span>
  for ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
