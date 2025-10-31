use core::marker::PhantomData;
use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of an object field: `name: value`
///
/// Preserves:
/// - The field name with its padding
/// - The colon `:` with its padding
/// - The value with its padding
#[derive(Debug, Clone)]
pub struct ObjectField<Name, Value, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// The field name
  name: Name,
  /// Padding around the colon
  colon_padding: Padding<S, TriviaContainer>,
  /// The field value
  value: Value,
}

impl<Name, Value, S, TriviaContainer> AsSpan<Span>
  for ObjectField<Name, Value, S, TriviaContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Value, S, TriviaContainer> IntoSpan<Span>
  for ObjectField<Name, Value, S, TriviaContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Value, S, TriviaContainer> IntoComponents
  for ObjectField<Name, Value, S, TriviaContainer>
{
  type Components = (Span, Name, Padding<S, TriviaContainer>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon_padding, self.value)
  }
}

impl<Name, Value, S, TriviaContainer> ObjectField<Name, Value, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST ObjectField.
  pub fn new(span: Span, name: Name, value: Value) -> Self {
    Self {
      span,
      name,
      colon_padding: Padding::new(),
      value,
    }
  }
}

impl<Name, Value, S, TriviaContainer> ObjectField<Name, Value, S, TriviaContainer> {
  /// Returns the span covering the entire field.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the field name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the colon padding.
  #[inline]
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.colon_padding
  }

  /// Returns a reference to the field value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

/// CST representation of a GraphQL object value: `{ field1: value1, ... }`
///
/// Unlike the AST version, preserves all braces and padding.
///
/// ## Examples
/// ```text
/// { id: 1, name: "Alice" }
/// {
///   id: 1
///   name: "Alice"  # user name
/// }
/// { id : 1 , name : "Alice" }  # preserves spacing
/// ```
#[derive(Debug, Clone)]
pub struct Object<Field, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Field>> {
  span: Span,
  /// Padding around the left brace
  lbrace_padding: Padding<S, TriviaContainer>,
  /// Fields with their trivia
  fields: Container,
  /// Padding around the right brace
  rbrace_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<Field>,
}

impl<Field, S, TriviaContainer, Container> AsSpan<Span>
  for Object<Field, S, TriviaContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Field, S, TriviaContainer, Container> IntoSpan<Span>
  for Object<Field, S, TriviaContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Field, S, TriviaContainer, Container> IntoComponents
  for Object<Field, S, TriviaContainer, Container>
{
  type Components = (
    Span,
    Padding<S, TriviaContainer>,
    Container,
    Padding<S, TriviaContainer>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.lbrace_padding,
      self.fields,
      self.rbrace_padding,
    )
  }
}

impl<Field, S, TriviaContainer, Container> Object<Field, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST Object.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      lbrace_padding: Padding::new(),
      fields: Container::default(),
      rbrace_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST Object with all components.
  pub const fn with_parts(
    span: Span,
    lbrace_padding: Padding<S, TriviaContainer>,
    fields: Container,
    rbrace_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      lbrace_padding,
      fields,
      rbrace_padding,
      _marker: PhantomData,
    }
  }
}

impl<Field, S, TriviaContainer, Container> Object<Field, S, TriviaContainer, Container> {
  /// Returns the span covering the entire object.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left brace padding.
  #[inline]
  pub const fn lbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lbrace_padding
  }

  /// Returns a reference to the fields container.
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }

  /// Returns a reference to the right brace padding.
  #[inline]
  pub const fn rbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rbrace_padding
  }
}
