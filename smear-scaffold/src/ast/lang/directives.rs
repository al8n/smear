use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};

use core::marker::PhantomData;
use std::vec::Vec;

/// Represents a single directive in a GraphQL-style syntax.
///
/// A directive consists of an `@` symbol followed by a name and optional arguments.
/// For example: `@deprecated`, `@include(if: true)`, `@customDirective(arg1: "value", arg2: 42)`
///
/// Spec: [Directive](https://spec.graphql.org/draft/#Directive)
#[derive(Debug, Clone, Copy)]
pub struct Directive<Name, Args> {
  span: Span,
  name: Name,
  arguments: Option<Args>,
}

impl<Name, Args> AsSpan<Span> for Directive<Name, Args> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Args> IntoSpan<Span> for Directive<Name, Args> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Args> IntoComponents for Directive<Name, Args> {
  type Components = (Span, Name, Option<Args>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.arguments)
  }
}

impl<Name, Args> Directive<Name, Args> {
  /// Creates a new directive with the given span, name, and optional arguments.
  #[inline]
  pub const fn new(span: Span, name: Name, arguments: Option<Args>) -> Self {
    Self {
      span,
      name,
      arguments,
    }
  }

  /// Returns a reference to the span covering the entire directive.
  ///
  /// The span includes the `@` symbol, name, and arguments (if present).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the directive's name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the directive's arguments, if present.
  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  /// Consumes the directive and returns its arguments, if present.
  #[inline]
  pub fn into_arguments(self) -> Option<Args> {
    self.arguments
  }
}

/// Represents a collection of one or more directives.
///
/// In GraphQL and similar languages, multiple directives can be applied to a single element.
/// This structure represents such a collection, maintaining the source span information
/// and providing access to all individual directives.
///
/// Spec: [Directives](https://spec.graphql.org/draft/#Directives)
#[derive(Debug, Clone, Copy)]
pub struct Directives<Directive, Container = Vec<Directive>> {
  span: Span,
  directives: Container,
  _directive: PhantomData<Directive>,
}

impl<Directive, Container> AsSpan<Span> for Directives<Directive, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Directive, Container> IntoSpan<Span> for Directives<Directive, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directive, Container> IntoComponents for Directives<Directive, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.directives)
  }
}

impl<Directive, Container> Directives<Directive, Container> {
  /// Creates a new directives collection with the given span and directives.
  #[inline]
  pub const fn new(span: Span, directives: Container) -> Self {
    Self {
      span,
      directives,
      _directive: PhantomData,
    }
  }

  /// Returns a reference to the span covering all directives in the collection.
  ///
  /// The span encompasses from the start of the first directive to the end of the last directive.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all directives.
  #[inline]
  pub const fn directives(&self) -> &Container {
    &self.directives
  }

  /// Consumes the directives collection and returns the underlying container.
  #[inline]
  pub fn into_directives(self) -> Container {
    self.directives
  }
}
