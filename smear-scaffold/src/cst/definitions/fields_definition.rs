use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a single field definition in a GraphQL type.
///
/// Unlike the AST version, preserves:
/// - The name with its padding
/// - Optional arguments definition with its padding
/// - The colon `:` with its padding
/// - The type with its padding
/// - Optional directives with their padding
///
/// ## Examples
/// ```text
/// name: String
/// name : String  # preserves spacing around colon
/// posts(first: Int): [Post!]!
/// email: String @deprecated
/// ```
#[derive(Debug, Clone)]
pub struct FieldDefinition<Name, Arguments, Type, Directives, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// The field name
  name: Name,
  /// Optional arguments definition
  arguments_definition: Option<Arguments>,
  /// Padding around the colon
  colon_padding: Padding<S, TriviaContainer>,
  /// The field's return type
  ty: Type,
  /// Optional directives
  directives: Option<Directives>,
}

impl<Name, Arguments, Type, Directives, S, TriviaContainer> AsSpan<Span>
  for FieldDefinition<Name, Arguments, Type, Directives, S, TriviaContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Arguments, Type, Directives, S, TriviaContainer> IntoSpan<Span>
  for FieldDefinition<Name, Arguments, Type, Directives, S, TriviaContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Arguments, Type, Directives, S, TriviaContainer> IntoComponents
  for FieldDefinition<Name, Arguments, Type, Directives, S, TriviaContainer>
{
  type Components = (
    Span,
    Name,
    Option<Arguments>,
    Padding<S, TriviaContainer>,
    Type,
    Option<Directives>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.name,
      self.arguments_definition,
      self.colon_padding,
      self.ty,
      self.directives,
    )
  }
}

impl<Name, Arguments, Type, Directives, S, TriviaContainer>
  FieldDefinition<Name, Arguments, Type, Directives, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST FieldDefinition.
  pub fn new(span: Span, name: Name, ty: Type) -> Self {
    Self {
      span,
      name,
      arguments_definition: None,
      colon_padding: Padding::new(),
      ty,
      directives: None,
    }
  }

  /// Creates a new CST FieldDefinition with all components.
  pub const fn with_parts(
    span: Span,
    name: Name,
    arguments_definition: Option<Arguments>,
    colon_padding: Padding<S, TriviaContainer>,
    ty: Type,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      span,
      name,
      arguments_definition,
      colon_padding,
      ty,
      directives,
    }
  }
}

impl<Name, Arguments, Type, Directives, S, TriviaContainer>
  FieldDefinition<Name, Arguments, Type, Directives, S, TriviaContainer>
{
  /// Returns the span covering the entire field definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the field name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the optional arguments definition.
  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Arguments> {
    self.arguments_definition.as_ref()
  }

  /// Returns a reference to the colon padding.
  #[inline]
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.colon_padding
  }

  /// Returns a reference to the field's return type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns a reference to the optional directives.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// CST representation of a collection of field definitions: `{ field1 field2 ... }`
///
/// Unlike the AST version, preserves:
/// - Opening left brace `{` with its padding
/// - All field definitions with their trivia
/// - Closing right brace `}` with its padding
///
/// ## Examples
/// ```text
/// { id: ID! name: String }
/// {
///   id: ID!
///   name: String
/// }
/// ```
#[derive(Debug, Clone)]
pub struct FieldsDefinition<FieldDef, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<FieldDef>> {
  span: Span,
  /// Padding around the left brace
  lbrace_padding: Padding<S, TriviaContainer>,
  /// Field definitions with their trivia
  fields: Container,
  /// Padding around the right brace
  rbrace_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<FieldDef>,
}

impl<FieldDef, S, TriviaContainer, Container> AsSpan<Span>
  for FieldsDefinition<FieldDef, S, TriviaContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<FieldDef, S, TriviaContainer, Container> IntoSpan<Span>
  for FieldsDefinition<FieldDef, S, TriviaContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<FieldDef, S, TriviaContainer, Container> IntoComponents
  for FieldsDefinition<FieldDef, S, TriviaContainer, Container>
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

impl<FieldDef, S, TriviaContainer, Container> FieldsDefinition<FieldDef, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST FieldsDefinition.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      lbrace_padding: Padding::new(),
      fields: Container::default(),
      rbrace_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST FieldsDefinition with all components.
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

impl<FieldDef, S, TriviaContainer, Container> FieldsDefinition<FieldDef, S, TriviaContainer, Container> {
  /// Returns the span covering the entire fields definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left brace padding.
  #[inline]
  pub const fn lbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lbrace_padding
  }

  /// Returns a reference to the field definitions container.
  #[inline]
  pub const fn field_definitions(&self) -> &Container {
    &self.fields
  }

  /// Returns a reference to the right brace padding.
  #[inline]
  pub const fn rbrace_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rbrace_padding
  }

  /// Returns a slice of field definitions if the container supports it.
  #[inline]
  pub fn fields_slice(&self) -> &[FieldDef]
  where
    Container: AsRef<[FieldDef]>,
  {
    self.field_definitions().as_ref()
  }
}
