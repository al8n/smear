//! GraphQLx executable-definition AST aliases and enums.

use core::borrow::Borrow;

use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{
  ConstDirectives, Constrained, DefaultInputValue, DefaultVec, DefinitionName, Directives,
  ExecutableDefinitionHeader, ExecutableDefinitionName, ExecutableDefinitionTypeGenerics,
  SelectionSet, StringValue, Type, TypeCondition, VariableValue, WhereClause,
};

/// A GraphQLx node with an optional leading string description.
pub type Described<T, S, Span = SimpleSpan> =
  crate::executable::Described<T, StringValue<S, Span>, Span>;

/// A GraphQLx variable definition in an executable operation.
pub type VariableDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::executable::VariableDefinition<
    VariableValue<S, Span>,
    Ty,
    DefaultInputValue<S, Span>,
    ConstDirectives<S, Span>,
    Span,
  >;

/// A described GraphQLx variable definition.
pub type DescribedVariableDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<VariableDefinition<S, Span, Ty>, S, Span>;

/// A collection of GraphQLx variable definitions.
pub type VariablesDefinition<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<DescribedVariableDefinition<S, Span, Ty>>,
> =
  crate::executable::VariablesDefinition<DescribedVariableDefinition<S, Span, Ty>, Span, Container>;

/// The GraphQLx operation keyword (`query`, `mutation`, or `subscription`).
#[derive(Debug, Display, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum OperationType<Span = SimpleSpan> {
  /// A `query` operation.
  #[display("query")]
  Query(Span),
  /// A `mutation` operation.
  #[display("mutation")]
  Mutation(Span),
  /// A `subscription` operation.
  #[display("subscription")]
  Subscription(Span),
}

impl<Span> OperationType<Span> {
  /// Returns the operation-keyword span.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Query(span) | Self::Mutation(span) | Self::Subscription(span) => span,
    }
  }

  /// Returns the operation keyword spelling.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query(_) => "query",
      Self::Mutation(_) => "mutation",
      Self::Subscription(_) => "subscription",
    }
  }
}

impl<Span> AsSpan<Span> for OperationType<Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for OperationType<Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Query(span) | Self::Mutation(span) | Self::Subscription(span) => span,
    }
  }
}

impl<Span> AsRef<str> for OperationType<Span> {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl<Span> Borrow<str> for OperationType<Span> {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

/// A named GraphQLx operation with optional definition generics and `where`
/// constraints on its selection set.
pub type NamedOperationDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::executable::NamedOperationDefinition<
    DefinitionName<S, Span, Ty>,
    OperationType<Span>,
    VariablesDefinition<S, Span, Ty>,
    Directives<S, Span>,
    Constrained<SelectionSet<S, Span>, WhereClause<S, Span, Ty>, Span>,
    Span,
  >;

/// A GraphQLx fragment definition with implementation/name generics and an
/// optional `where` clause on its selection set.
pub type FragmentDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::executable::FragmentDefinition<
    ExecutableDefinitionHeader<
      ExecutableDefinitionTypeGenerics<S, Span>,
      ExecutableDefinitionName<S, Span>,
      Span,
    >,
    TypeCondition<S, Span, Ty>,
    Directives<S, Span>,
    Constrained<SelectionSet<S, Span>, WhereClause<S, Span, Ty>, Span>,
    Span,
  >;

/// A GraphQLx operation definition, either named or query shorthand.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum OperationDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A named `query`, `mutation`, or `subscription` operation.
  Named(NamedOperationDefinition<S, Span, Ty>),
  /// A query-shorthand selection set.
  Shorthand(SelectionSet<S, Span>),
}

impl<S, Span, Ty> OperationDefinition<S, Span, Ty> {
  /// Returns the complete operation span.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Named(operation) => operation.span(),
      Self::Shorthand(selection_set) => selection_set.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for OperationDefinition<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for OperationDefinition<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Named(operation) => operation.into_span(),
      Self::Shorthand(selection_set) => selection_set.into_span(),
    }
  }
}

/// A GraphQLx executable definition, either an operation or a fragment.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ExecutableDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// An operation definition.
  Operation(OperationDefinition<S, Span, Ty>),
  /// A fragment definition.
  Fragment(FragmentDefinition<S, Span, Ty>),
}

impl<S, Span, Ty> ExecutableDefinition<S, Span, Ty> {
  /// Returns the complete executable-definition span.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Operation(operation) => operation.span(),
      Self::Fragment(fragment) => fragment.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for ExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for ExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Operation(operation) => operation.into_span(),
      Self::Fragment(fragment) => fragment.into_span(),
    }
  }
}

/// A top-level GraphQLx executable definition with an optional description.
pub type DescribedExecutableDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<ExecutableDefinition<S, Span, Ty>, S, Span>;
