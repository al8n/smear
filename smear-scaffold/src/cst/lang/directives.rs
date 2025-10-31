use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of a single directive: `@name(args)`
///
/// Unlike the AST version, preserves:
/// - The `@` symbol with its padding
/// - The directive name with its padding
/// - Optional arguments with their parentheses and padding
///
/// ## Examples
/// ```text
/// @deprecated
/// @include(if: true)
/// @ skip( if : false )  # preserves extra spacing
/// ```
#[derive(Debug, Clone)]
pub struct Directive<Name, Args, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// Padding around the `@` symbol
  at_padding: Padding<S, TriviaContainer>,
  /// The directive name
  name: Name,
  /// Optional arguments
  arguments: Option<Args>,
}

impl<Name, Args, S, TriviaContainer> AsSpan<Span> for Directive<Name, Args, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Args, S, TriviaContainer> IntoSpan<Span> for Directive<Name, Args, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Args, S, TriviaContainer> IntoComponents for Directive<Name, Args, S, TriviaContainer> {
  type Components = (Span, Padding<S, TriviaContainer>, Name, Option<Args>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.at_padding, self.name, self.arguments)
  }
}

impl<Name, Args, S, TriviaContainer> Directive<Name, Args, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST Directive.
  pub fn new(span: Span, name: Name, arguments: Option<Args>) -> Self {
    Self {
      span,
      at_padding: Padding::new(),
      name,
      arguments,
    }
  }

  /// Creates a new CST Directive with at padding.
  pub const fn with_at_padding(
    span: Span,
    at_padding: Padding<S, TriviaContainer>,
    name: Name,
    arguments: Option<Args>,
  ) -> Self {
    Self {
      span,
      at_padding,
      name,
      arguments,
    }
  }
}

impl<Name, Args, S, TriviaContainer> Directive<Name, Args, S, TriviaContainer> {
  /// Returns the span covering the entire directive.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `@` padding.
  #[inline]
  pub const fn at_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.at_padding
  }

  /// Returns a reference to the directive name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the directive arguments, if present.
  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }
}

/// CST representation of a collection of directives.
///
/// Preserves:
/// - All directives with their trivia
/// - Spacing between directives
///
/// ## Examples
/// ```text
/// @deprecated @skip(if: false)
/// @include(if: true)  @customDirective
/// ```
#[derive(Debug, Clone)]
pub struct Directives<Directive, S, TriviaContainer = Vec<crate::cst::Trivia<S>>, Container = Vec<Directive>> {
  span: Span,
  /// Directives with trivia between them
  directives: Container,
  _marker: PhantomData<(Directive, S, TriviaContainer)>,
}

impl<Directive, S, TriviaContainer, Container> AsSpan<Span>
  for Directives<Directive, S, TriviaContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Directive, S, TriviaContainer, Container> IntoSpan<Span>
  for Directives<Directive, S, TriviaContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directive, S, TriviaContainer, Container> IntoComponents
  for Directives<Directive, S, TriviaContainer, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.directives)
  }
}

impl<Directive, S, TriviaContainer, Container> Directives<Directive, S, TriviaContainer, Container>
where
  Container: Default,
{
  /// Creates a new CST Directives collection.
  pub fn new(span: Span) -> Self {
    Self {
      span,
      directives: Container::default(),
      _marker: PhantomData,
    }
  }

  /// Creates a new CST Directives with the given directives.
  pub const fn with_directives(span: Span, directives: Container) -> Self {
    Self {
      span,
      directives,
      _marker: PhantomData,
    }
  }
}

impl<Directive, S, TriviaContainer, Container> Directives<Directive, S, TriviaContainer, Container> {
  /// Returns the span covering all directives.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the directives container.
  #[inline]
  pub const fn directives(&self) -> &Container {
    &self.directives
  }

  /// Returns a mutable reference to the directives container.
  #[inline]
  pub fn directives_mut(&mut self) -> &mut Container {
    &mut self.directives
  }

  /// Returns a slice of directives if the container supports it.
  #[inline]
  pub fn directives_slice(&self) -> &[Directive]
  where
    Container: AsRef<[Directive]>,
  {
    self.directives().as_ref()
  }
}
