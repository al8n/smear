//! GraphQLx directive AST aliases.

use tokora::SimpleSpan;

use super::{Arguments, ConstArguments, DefaultVec, TypePath};

/// A GraphQLx directive with executable arguments and a generic-capable path.
pub type Directive<S, Span = SimpleSpan> =
  crate::directive::Directive<TypePath<S, Span>, Arguments<S, Span>, Span>;

/// A GraphQLx directive with constant arguments and a generic-capable path.
pub type ConstDirective<S, Span = SimpleSpan> =
  crate::directive::Directive<TypePath<S, Span>, ConstArguments<S, Span>, Span>;

/// A collection of GraphQLx executable directives.
pub type Directives<S, Span = SimpleSpan, Container = DefaultVec<Directive<S, Span>>> =
  crate::directive::Directives<Directive<S, Span>, Container, Span>;

/// A collection of GraphQLx constant directives.
pub type ConstDirectives<S, Span = SimpleSpan, Container = DefaultVec<ConstDirective<S, Span>>> =
  crate::directive::Directives<ConstDirective<S, Span>, Container, Span>;
