use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of an arguments definition: `(arg1 arg2 ...)`
///
/// Unlike the AST version, preserves:
/// - Opening left parenthesis `(` with its padding
/// - All input value definitions with their trivia
/// - Closing right parenthesis `)` with its padding
///
/// ## Examples
/// ```text
/// (id: ID!)
/// ( first: Int, after: String )
/// (
///   query: String!
///   limit: Int = 20
/// )
/// ```
#[derive(Debug, Clone)]
pub struct ArgumentsDefinition<InputValueDef, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<InputValueDef>> {
  span: Span,
  /// Padding around the left parenthesis
  lparen_padding: Padding<S, TriviaContainer>,
  /// Input value definitions with their trivia
  values: Container,
  /// Padding around the right parenthesis
  rparen_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<InputValueDef>,
}

impl<InputValueDef, S, TriviaContainer, Container> AsSpan<Span>
  for ArgumentsDefinition<InputValueDef, S, TriviaContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<InputValueDef, S, TriviaContainer, Container> IntoSpan<Span>
  for ArgumentsDefinition<InputValueDef, S, TriviaContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValueDef, S, TriviaContainer, Container> IntoComponents
  for ArgumentsDefinition<InputValueDef, S, TriviaContainer, Container>
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
      self.lparen_padding,
      self.values,
      self.rparen_padding,
    )
  }
}

impl<InputValueDef, S, TriviaContainer, Container>
  ArgumentsDefinition<InputValueDef, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST ArgumentsDefinition.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      lparen_padding: Padding::new(),
      values: Container::default(),
      rparen_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST ArgumentsDefinition with all components.
  pub const fn with_parts(
    span: Span,
    lparen_padding: Padding<S, TriviaContainer>,
    values: Container,
    rparen_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      lparen_padding,
      values,
      rparen_padding,
      _marker: PhantomData,
    }
  }
}

impl<InputValueDef, S, TriviaContainer, Container>
  ArgumentsDefinition<InputValueDef, S, TriviaContainer, Container>
{
  /// Returns the span covering the entire arguments definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left parenthesis padding.
  #[inline]
  pub const fn lparen_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lparen_padding
  }

  /// Returns a reference to the input value definitions container.
  #[inline]
  pub const fn input_value_definitions(&self) -> &Container {
    &self.values
  }

  /// Returns a reference to the right parenthesis padding.
  #[inline]
  pub const fn rparen_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rparen_padding
  }

  /// Returns a slice of input value definitions if the container supports it.
  #[inline]
  pub fn values_slice(&self) -> &[InputValueDef]
  where
    Container: AsRef<[InputValueDef]>,
  {
    self.input_value_definitions().as_ref()
  }
}
