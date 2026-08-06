//! GraphQL executable-definition AST node types.
//!
//! Shared executable carriers live at crate level. This module binds them to
//! GraphQL's names, values, directives, selections, and operation keywords.

use core::borrow::Borrow;

use crate::lexer::keywords::{Mutation, Query, Subscription};
use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::{
  ConstDirectives, DefaultInputValue, Directives, FragmentName, Name, SelectionSet, StringValue,
  Type, TypeCondition, VariableValue,
};

/// A node with an optional leading GraphQL string description.
pub type Described<T, S> = crate::parser::executable::Described<T, StringValue<S>>;

/// A variable definition in an executable operation.
///
/// Directives in this grammar position use constant arguments.
pub type VariableDefinition<S, Ty = Type<Name<S>>> = crate::parser::executable::VariableDefinition<
  VariableValue<S>,
  Ty,
  DefaultInputValue<S>,
  ConstDirectives<S>,
>;

/// A variable definition with an optional leading description.
pub type DescribedVariableDefinition<S, Ty = Type<Name<S>>> =
  Described<VariableDefinition<S, Ty>, S>;

/// A variable-definition collection.
///
/// An absent opening parenthesis yields an empty, zero-width collection; a
/// present parenthesized collection contains one or more definitions.
pub type VariablesDefinition<S, Ty = Type<Name<S>>> =
  crate::parser::executable::VariablesDefinition<DescribedVariableDefinition<S, Ty>>;

/// A named fragment definition.
pub type FragmentDefinition<S> = crate::parser::executable::FragmentDefinition<
  FragmentName<S>,
  TypeCondition<S>,
  Directives<S>,
  SelectionSet<S>,
>;

/// The GraphQL operation keyword (`query`, `mutation`, or `subscription`).
#[derive(Debug, Display, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum OperationType {
  /// A read operation.
  #[display("query")]
  Query(Query),
  /// A write operation.
  #[display("mutation")]
  Mutation(Mutation),
  /// A streaming operation.
  #[display("subscription")]
  Subscription(Subscription),
}

impl OperationType {
  /// Returns the span covering the operation keyword.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Query(keyword) => keyword.span(),
      Self::Mutation(keyword) => keyword.span(),
      Self::Subscription(keyword) => keyword.span(),
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

impl AsSpan<Span> for OperationType {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for OperationType {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Query(keyword) => keyword.into_span(),
      Self::Mutation(keyword) => keyword.into_span(),
      Self::Subscription(keyword) => keyword.into_span(),
    }
  }
}

impl AsRef<str> for OperationType {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl Borrow<str> for OperationType {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

/// A named operation definition.
pub type NamedOperationDefinition<S, Ty = Type<Name<S>>> =
  crate::parser::executable::NamedOperationDefinition<
    Name<S>,
    OperationType,
    VariablesDefinition<S, Ty>,
    Directives<S>,
    SelectionSet<S>,
  >;

/// An operation definition, either named or query shorthand.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum OperationDefinition<S, Ty = Type<Name<S>>> {
  /// A named `query`, `mutation`, or `subscription` operation.
  Named(NamedOperationDefinition<S, Ty>),
  /// A query-shorthand selection set.
  Shorthand(SelectionSet<S>),
}

impl<S, Ty> OperationDefinition<S, Ty> {
  /// Returns the span covering the operation definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Named(operation) => operation.span(),
      Self::Shorthand(selection_set) => selection_set.span(),
    }
  }
}

impl<S, Ty> AsSpan<Span> for OperationDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for OperationDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Named(operation) => operation.into_span(),
      Self::Shorthand(selection_set) => selection_set.into_span(),
    }
  }
}

/// An executable definition, either an operation or a fragment.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ExecutableDefinition<S, Ty = Type<Name<S>>> {
  /// An operation definition.
  Operation(OperationDefinition<S, Ty>),
  /// A fragment definition.
  Fragment(FragmentDefinition<S>),
}

impl<S, Ty> ExecutableDefinition<S, Ty> {
  /// Returns the span covering the executable definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Operation(operation) => operation.span(),
      Self::Fragment(fragment) => fragment.span(),
    }
  }
}

impl<S, Ty> AsSpan<Span> for ExecutableDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for ExecutableDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Operation(operation) => operation.into_span(),
      Self::Fragment(fragment) => fragment.into_span(),
    }
  }
}

/// A top-level executable definition with an optional leading description.
///
/// Leading descriptions are frozen-parser/dialect compatibility. They are not
/// part of the standard GraphQL executable-definition grammar.
pub type DescribedExecutableDefinition<S, Ty = Type<Name<S>>> =
  Described<ExecutableDefinition<S, Ty>, S>;

/// A nonempty GraphQL executable document.
///
/// Its definitions may carry leading descriptions for frozen-parser/dialect
/// compatibility; standard GraphQL executable definitions are not described.
pub type ExecutableDocument<S, Ty = Type<Name<S>>> =
  crate::parser::executable::Document<DescribedExecutableDefinition<S, Ty>>;
