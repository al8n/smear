use logosky::utils::Span;
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a single GraphQL argument.
///
/// Unlike the AST version which only stores name and value, the CST version
/// preserves:
/// - The name with its padding
/// - The colon token with its padding
/// - The value with its padding
///
/// ## Grammar
/// ```text
/// Argument ::= Name ':' Value
/// ```
///
/// ## Examples
/// ```text
/// id: "123"           # name: "id", colon, value: "123"
/// limit : 10          # preserves spacing around colon
/// filter: {           # value can be complex
///   status: ACTIVE
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Argument<Name, Value, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// The argument name with its padding
  name: Name,
  /// Padding around the colon separator
  colon_padding: Padding<S, TriviaContainer>,
  /// The argument value with its padding
  value: Value,
}

impl<Name, Value, S, TriviaContainer> Argument<Name, Value, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST Argument.
  pub fn new(span: Span, name: Name, value: Value) -> Self {
    Self {
      span,
      name,
      colon_padding: Padding::new(),
      value,
    }
  }

  /// Creates a new CST Argument with the given colon padding.
  pub const fn with_colon_padding(
    span: Span,
    name: Name,
    colon_padding: Padding<S, TriviaContainer>,
    value: Value,
  ) -> Self {
    Self {
      span,
      name,
      colon_padding,
      value,
    }
  }
}

impl<Name, Value, S, TriviaContainer> Argument<Name, Value, S, TriviaContainer> {
  /// Returns the span covering the entire argument.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the argument name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the colon padding.
  #[inline]
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.colon_padding
  }

  /// Returns a reference to the argument value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns mutable references to the argument components.
  pub fn as_mut_parts(&mut self) -> (&mut Name, &mut Padding<S, TriviaContainer>, &mut Value) {
    (&mut self.name, &mut self.colon_padding, &mut self.value)
  }

  /// Deconstructs the Argument into its components.
  pub fn into_parts(self) -> (Span, Name, Padding<S, TriviaContainer>, Value) {
    (self.span, self.name, self.colon_padding, self.value)
  }
}

/// CST representation of a GraphQL argument list.
///
/// Unlike the AST version which only stores arguments, the CST version preserves:
/// - The opening left parenthesis `(` with its padding
/// - Each argument with padding between arguments
/// - The closing right parenthesis `)` with its padding
/// - All whitespace and comments throughout
///
/// ## Grammar
/// ```text
/// Arguments ::= '(' Argument+ ')'
/// ```
///
/// ## Examples
/// ```text
/// (id: "123")                      # simple argument list
/// ( id: "123", limit: 10 )         # multiple arguments with spacing
/// (
///   id: "123"    # user ID
///   limit: 10    # max results
/// )                                # formatted with comments
/// ```
#[derive(Debug, Clone)]
pub struct Arguments<Arg, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Arg>> {
  span: Span,
  /// Padding around the left parenthesis
  lparen_padding: Padding<S, TriviaContainer>,
  /// Arguments with their separating trivia
  arguments: Container,
  /// Padding around the right parenthesis
  rparen_padding: Padding<S, TriviaContainer>,
  _marker: PhantomData<Arg>,
}

impl<Arg, S, TriviaContainer, Container> Arguments<Arg, S, TriviaContainer, Container>
where
  TriviaContainer: Default,
  Container: Default,
{
  /// Creates a new empty CST Arguments.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      lparen_padding: Padding::new(),
      arguments: Container::default(),
      rparen_padding: Padding::new(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST Arguments with the given components.
  pub const fn with_parts(
    span: Span,
    lparen_padding: Padding<S, TriviaContainer>,
    arguments: Container,
    rparen_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      lparen_padding,
      arguments,
      rparen_padding,
      _marker: PhantomData,
    }
  }
}

impl<Arg, S, TriviaContainer, Container> Arguments<Arg, S, TriviaContainer, Container> {
  /// Returns the span covering the entire argument list.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the left parenthesis padding.
  #[inline]
  pub const fn lparen_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.lparen_padding
  }

  /// Returns a reference to the arguments container.
  #[inline]
  pub const fn arguments(&self) -> &Container {
    &self.arguments
  }

  /// Returns a reference to the right parenthesis padding.
  #[inline]
  pub const fn rparen_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.rparen_padding
  }

  /// Returns a mutable reference to the left parenthesis padding.
  #[inline]
  pub fn lparen_padding_mut(&mut self) -> &mut Padding<S, TriviaContainer> {
    &mut self.lparen_padding
  }

  /// Returns a mutable reference to the arguments container.
  #[inline]
  pub fn arguments_mut(&mut self) -> &mut Container {
    &mut self.arguments
  }

  /// Returns a mutable reference to the right parenthesis padding.
  #[inline]
  pub fn rparen_padding_mut(&mut self) -> &mut Padding<S, TriviaContainer> {
    &mut self.rparen_padding
  }

  /// Returns a slice of arguments if the container supports it.
  #[inline]
  pub fn arguments_slice(&self) -> &[Arg]
  where
    Container: AsRef<[Arg]>,
  {
    self.arguments().as_ref()
  }

  /// Deconstructs the Arguments into its components.
  pub fn into_parts(
    self,
  ) -> (
    Span,
    Padding<S, TriviaContainer>,
    Container,
    Padding<S, TriviaContainer>,
  ) {
    (
      self.span,
      self.lparen_padding,
      self.arguments,
      self.rparen_padding,
    )
  }
}
