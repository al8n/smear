//! Directive-node carriers shared by the GraphQL-family dialect ASTs.
//!
//! These copied structures contain only source-independent grammar data. Dialect
//! assemblies bind their own name and argument node types through public aliases.

use core::marker::PhantomData;

use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A GraphQL-family directive.
///
/// A directive commits to the `@ Name Arguments?` production. Empty argument
/// collections may be represented by `None` on this node.
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Directive<Name, Args, Span = SimpleSpan> {
  span: Span,
  name: Name,
  arguments: Option<Args>,
}

impl<Name, Args, Span> AsSpan<Span> for Directive<Name, Args, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Args, Span> IntoSpan<Span> for Directive<Name, Args, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Args, Span> IntoComponents for Directive<Name, Args, Span> {
  type Components = (Span, Name, Option<Args>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.arguments)
  }
}

impl<Name, Args, Span> Directive<Name, Args, Span> {
  /// Creates a directive from its span, name, and optional arguments.
  #[inline]
  pub const fn new(span: Span, name: Name, arguments: Option<Args>) -> Self {
    Self {
      span,
      name,
      arguments,
    }
  }

  /// Returns the span covering the entire directive.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the directive name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the directive arguments when the parsed collection was nonempty.
  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  /// Consumes the directive and returns its optional arguments.
  #[inline]
  pub fn into_arguments(self) -> Option<Args> {
    self.arguments
  }
}

/// A GraphQL-family directive collection.
///
/// Present collections span their first through last directive. An absent
/// collection is empty with a zero-width span at the parser's starting offset.
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Directives<Directive, Container = Vec<Directive>, Span = SimpleSpan> {
  span: Span,
  directives: Container,
  _marker: PhantomData<Directive>,
}

impl<Directive, Container, Span> AsSpan<Span> for Directives<Directive, Container, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Directive, Container, Span> IntoSpan<Span> for Directives<Directive, Container, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directive, Container, Span> IntoComponents for Directives<Directive, Container, Span> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.directives)
  }
}

impl<Directive, Container, Span> Directives<Directive, Container, Span> {
  /// Creates a directives collection from its span and parsed directives.
  #[inline]
  pub const fn new(span: Span, directives: Container) -> Self {
    Self {
      span,
      directives,
      _marker: PhantomData,
    }
  }

  /// Returns the collection span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parsed directives.
  #[inline]
  pub fn directives(&self) -> &[Directive]
  where
    Container: AsRef<[Directive]>,
  {
    self.directives.as_ref()
  }

  /// Consumes this collection and returns its directives.
  #[inline]
  pub fn into_directives(self) -> Container {
    self.directives
  }
}
