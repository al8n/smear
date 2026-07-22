//! GraphQL `Directive`/`Directives` AST node types.
//!
//! The nodes are local to this crate so their parsers can be exposed as inherent
//! APIs. They are keyed by the source slice `S`, with executable arguments by
//! default and constant aliases for schema contexts.

use core::marker::PhantomData;

use std::vec::Vec;

use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{Arguments, ConstArguments, Name};

/// A GraphQL directive with executable arguments by default.
///
/// A directive commits to the `@ Name Arguments?` production. Empty argument
/// collections are represented by `None` on this node.
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
#[derive(Debug, Clone, Copy)]
pub struct Directive<S, Args = Arguments<S>> {
  span: Span,
  name: Name<S>,
  arguments: Option<Args>,
}

impl<S, Args> AsSpan<Span> for Directive<S, Args> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Args> IntoSpan<Span> for Directive<S, Args> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Args> IntoComponents for Directive<S, Args> {
  type Components = (Span, Name<S>, Option<Args>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.arguments)
  }
}

impl<S, Args> Directive<S, Args> {
  /// Creates a directive from its span, name, and optional arguments.
  #[inline]
  pub const fn new(span: Span, name: Name<S>, arguments: Option<Args>) -> Self {
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
  pub const fn name(&self) -> &Name<S> {
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

/// Directive with constant arguments (no variables).
pub type ConstDirective<S> = Directive<S, ConstArguments<S>>;

/// A GraphQL directive collection.
///
/// Present collections span their first through last directive. An absent
/// collection is empty with a zero-width span at the parser's starting offset.
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
#[derive(Debug, Clone, Copy)]
pub struct Directives<S, Directive = self::Directive<S>, Container = Vec<Directive>> {
  span: Span,
  directives: Container,
  _marker: PhantomData<(S, Directive)>,
}

impl<S, Directive, Container> AsSpan<Span> for Directives<S, Directive, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Directive, Container> IntoSpan<Span> for Directives<S, Directive, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Directive, Container> IntoComponents for Directives<S, Directive, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.directives)
  }
}

impl<S, Directive, Container> Directives<S, Directive, Container> {
  /// Creates a directives collection from its span and container.
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

  /// Returns the directive container.
  #[inline]
  pub const fn directives(&self) -> &Container {
    &self.directives
  }

  /// Consumes the collection and returns its directive container.
  #[inline]
  pub fn into_directives(self) -> Container {
    self.directives
  }
}

/// List of directives with constant arguments.
pub type ConstDirectives<S> = Directives<S, ConstDirective<S>>;
