use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};

use crate::cst::Padding;

/// CST representation of an input value definition: `name: Type = defaultValue`
///
/// Preserves all tokens including name, colon, type, and optional default value.
#[derive(Debug, Clone)]
pub struct InputValueDefinition<Name, Type, DefaultValue, Directives, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  name: Name,
  colon_padding: Padding<S, TriviaContainer>,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl<Name, Type, DefaultValue, Directives, S, TriviaContainer>
  InputValueDefinition<Name, Type, DefaultValue, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> { &self.colon_padding }
  pub const fn ty(&self) -> &Type { &self.ty }
  pub const fn default_value(&self) -> Option<&DefaultValue> { self.default_value.as_ref() }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
}

impl<Name, Type, DefaultValue, Directives, S, TriviaContainer> AsSpan<Span>
  for InputValueDefinition<Name, Type, DefaultValue, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Type, DefaultValue, Directives, S, TriviaContainer> IntoSpan<Span>
  for InputValueDefinition<Name, Type, DefaultValue, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}

impl<Name, Type, DefaultValue, Directives, S, TriviaContainer> IntoComponents
  for InputValueDefinition<Name, Type, DefaultValue, Directives, S, TriviaContainer>
{
  type Components = (Span, Name, Padding<S, TriviaContainer>, Type, Option<DefaultValue>, Option<Directives>);

  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon_padding, self.ty, self.default_value, self.directives)
  }
}
