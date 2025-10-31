use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a single enum value definition
///
/// Preserves the enum value name and optional directives with all trivia
#[derive(Debug, Clone)]
pub struct EnumValueDefinition<EnumValue, Directives, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  enum_value: EnumValue,
  directives: Option<Directives>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<EnumValue, Directives, S, TriviaContainer>
  EnumValueDefinition<EnumValue, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn value(&self) -> &EnumValue { &self.enum_value }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
}

impl<EnumValue, Directives, S, TriviaContainer> AsSpan<Span>
  for EnumValueDefinition<EnumValue, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<EnumValue, Directives, S, TriviaContainer> IntoSpan<Span>
  for EnumValueDefinition<EnumValue, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}

impl<EnumValue, Directives, S, TriviaContainer> IntoComponents
  for EnumValueDefinition<EnumValue, Directives, S, TriviaContainer>
{
  type Components = (Span, EnumValue, Option<Directives>);

  fn into_components(self) -> Self::Components {
    (self.span, self.enum_value, self.directives)
  }
}

/// CST representation of enum values definition: `{ VALUE1 VALUE2 ... }`
///
/// Preserves braces and all enum values with their trivia
#[derive(Debug, Clone)]
pub struct EnumValuesDefinition<EnumValueDef, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<EnumValueDef>> {
  span: Span,
  lbrace_padding: Padding<S, TriviaContainer>,
  enum_values: Container,
  rbrace_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<EnumValueDef>,
}

impl<EnumValueDef, S, TriviaContainer, Container>
  EnumValuesDefinition<EnumValueDef, S, TriviaContainer, Container>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn lbrace_padding(&self) -> &Padding<S, TriviaContainer> { &self.lbrace_padding }
  pub const fn enum_value_definitions(&self) -> &Container { &self.enum_values }
  pub const fn rbrace_padding(&self) -> &Padding<S, TriviaContainer> { &self.rbrace_padding }
}

impl<EnumValueDef, S, TriviaContainer, Container> AsSpan<Span>
  for EnumValuesDefinition<EnumValueDef, S, TriviaContainer, Container>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<EnumValueDef, S, TriviaContainer, Container> IntoSpan<Span>
  for EnumValuesDefinition<EnumValueDef, S, TriviaContainer, Container>
{
  fn into_span(self) -> Span { self.span }
}

impl<EnumValueDef, S, TriviaContainer, Container> IntoComponents
  for EnumValuesDefinition<EnumValueDef, S, TriviaContainer, Container>
{
  type Components = (Span, Padding<S, TriviaContainer>, Container, Padding<S, TriviaContainer>);

  fn into_components(self) -> Self::Components {
    (self.span, self.lbrace_padding, self.enum_values, self.rbrace_padding)
  }
}

/// CST representation of enum type definition: `enum Name { VALUES }`
///
/// Preserves `enum` keyword, name, optional directives, and optional enum values
#[derive(Debug, Clone)]
pub struct EnumTypeDefinition<Name, Directives, EnumValues, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  enum_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  directives: Option<Directives>,
  enum_values: Option<EnumValues>,
}

impl<Name, Directives, EnumValues, S, TriviaContainer>
  EnumTypeDefinition<Name, Directives, EnumValues, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn enum_keyword_padding(&self) -> &Padding<S, TriviaContainer> { &self.enum_keyword_padding }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn enum_values_definition(&self) -> Option<&EnumValues> { self.enum_values.as_ref() }
}

impl<Name, Directives, EnumValues, S, TriviaContainer> AsSpan<Span>
  for EnumTypeDefinition<Name, Directives, EnumValues, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Directives, EnumValues, S, TriviaContainer> IntoSpan<Span>
  for EnumTypeDefinition<Name, Directives, EnumValues, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}

impl<Name, Directives, EnumValues, S, TriviaContainer> IntoComponents
  for EnumTypeDefinition<Name, Directives, EnumValues, S, TriviaContainer>
{
  type Components = (Span, Padding<S, TriviaContainer>, Name, Option<Directives>, Option<EnumValues>);

  fn into_components(self) -> Self::Components {
    (self.span, self.enum_keyword_padding, self.name, self.directives, self.enum_values)
  }
}

/// CST representation of enum extension data
#[derive(Debug, Clone)]
pub enum EnumTypeExtensionData<Directives, EnumValues> {
  /// Extension adds values with optional directives
  Values {
    directives: Option<Directives>,
    values: EnumValues,
  },
  /// Extension adds only directives
  Directives(Directives),
}

impl<Directives, EnumValues> EnumTypeExtensionData<Directives, EnumValues> {
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Values { directives, .. } => directives.as_ref(),
      Self::Directives(d) => Some(d),
    }
  }

  pub const fn enum_values_definition(&self) -> Option<&EnumValues> {
    match self {
      Self::Values { values, .. } => Some(values),
      Self::Directives(_) => None,
    }
  }
}

/// CST representation of enum type extension: `extend enum Name ...`
///
/// Preserves `extend` and `enum` keywords with all content
#[derive(Debug, Clone)]
pub struct EnumTypeExtension<Name, Directives, EnumValues, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  extend_keyword_padding: Padding<S, TriviaContainer>,
  enum_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  data: EnumTypeExtensionData<Directives, EnumValues>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Directives, EnumValues, S, TriviaContainer>
  EnumTypeExtension<Name, Directives, EnumValues, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn extend_keyword_padding(&self) -> &Padding<S, TriviaContainer> { &self.extend_keyword_padding }
  pub const fn enum_keyword_padding(&self) -> &Padding<S, TriviaContainer> { &self.enum_keyword_padding }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn data(&self) -> &EnumTypeExtensionData<Directives, EnumValues> { &self.data }
  pub const fn directives(&self) -> Option<&Directives> { self.data.directives() }
  pub const fn enum_values_definition(&self) -> Option<&EnumValues> { self.data.enum_values_definition() }
}

impl<Name, Directives, EnumValues, S, TriviaContainer> AsSpan<Span>
  for EnumTypeExtension<Name, Directives, EnumValues, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Directives, EnumValues, S, TriviaContainer> IntoSpan<Span>
  for EnumTypeExtension<Name, Directives, EnumValues, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}

impl<Name, Directives, EnumValues, S, TriviaContainer> IntoComponents
  for EnumTypeExtension<Name, Directives, EnumValues, S, TriviaContainer>
{
  type Components = (
    Span,
    Padding<S, TriviaContainer>,
    Padding<S, TriviaContainer>,
    Name,
    EnumTypeExtensionData<Directives, EnumValues>,
  );

  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.extend_keyword_padding,
      self.enum_keyword_padding,
      self.name,
      self.data,
    )
  }
}
