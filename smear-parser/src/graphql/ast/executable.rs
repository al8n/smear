//! GraphQL executable-definition AST node types.
//!
//! Shared executable carriers live at crate level. This module binds them to
//! GraphQL's names, values, directives, selections, and operation keywords.

use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::keywords::{Mutation, Query, Subscription};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::{
  ConstDirectives, DefaultInputValue, Directives, FragmentName, Name, SelectionSet, StringValue,
  Type, TypeCondition, VariableValue,
};

/// A node with an optional leading GraphQL string description.
pub type Described<T, S> = crate::executable::Described<T, StringValue<S>>;

/// A variable definition in an executable operation.
///
/// Directives in this grammar position use constant arguments.
pub type VariableDefinition<S, Ty = Type<Name<S>>> = crate::executable::VariableDefinition<
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
  crate::executable::VariablesDefinition<DescribedVariableDefinition<S, Ty>>;

/// A named fragment definition.
pub type FragmentDefinition<S> = crate::executable::FragmentDefinition<
  FragmentName<S>,
  TypeCondition<S>,
  Directives<S>,
  SelectionSet<S>,
>;

/// The GraphQL operation keyword (`query`, `mutation`, or `subscription`).
///
/// Not `#[non_exhaustive]`: draft §2.3 is `OperationType : one of query mutation subscription`.
/// The variant set is the specification's, not smear's, so adding to it would mean parsing
/// something that is not GraphQL. `#[non_exhaustive]` here would advertise an evolution freedom
/// this type structurally does not have and charge for it where it costs most — the exhaustiveness
/// `smear-compiler`'s draft §5 rules depend on. Smear's extension mechanism is a separate dialect:
/// `graphqlx`'s own `OperationType` keeps the attribute, correctly.
///
/// # Not a map key reachable through `&str`
///
/// Each variant carries its keyword, each keyword carries a span, and this type derives `Eq` and
/// `Hash` over both — while [`as_str`](Self::as_str) answers a per-variant **constant**. A
/// `Borrow<str>` impl promised those agree; two `query` keywords read at different offsets borrow
/// *equal*, compare *unequal*, and hash differently. It is gone, and the map below does not
/// compile:
///
/// ```compile_fail,E0308
/// use std::collections::HashMap;
/// use smear_parser::graphql::ast::OperationType;
///
/// let map: HashMap<OperationType, ()> = HashMap::new();
/// let _ = map.get("query");
/// ```
///
/// This one is worth reading beside `graphqlx`'s, because the two look different and are not:
/// there the variants carry a bare `Span` rather than a keyword type, so neither is the fieldless
/// enum whose derived `Eq` a `&str` lookup could have agreed with. Both carry a position and both
/// are excluded for it. Use [`as_str`](Self::as_str) or `AsRef<str>` and key a
/// `HashMap<&str, _>` on that.
///
/// Per this repository's convention the error code is checked only under a nightly
/// `cargo test --doc`; on stable the assertion is that the snippet does not compile at all.
#[derive(Debug, Display, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
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

/// A named operation definition.
pub type NamedOperationDefinition<S, Ty = Type<Name<S>>> =
  crate::executable::NamedOperationDefinition<
    Name<S>,
    OperationType,
    VariablesDefinition<S, Ty>,
    Directives<S>,
    SelectionSet<S>,
  >;

/// An operation definition, either named or query shorthand.
///
/// Not `#[non_exhaustive]`: draft §2.3's `OperationDefinition` has exactly two productions.
/// The variant set is the specification's, not smear's, so adding to it would mean parsing
/// something that is not GraphQL. `#[non_exhaustive]` here would advertise an evolution freedom
/// this type structurally does not have and charge for it where it costs most — the exhaustiveness
/// `smear-compiler`'s draft §5 rules depend on. Smear's extension mechanism is a separate dialect:
/// `graphqlx`'s own `OperationDefinition` keeps the attribute, correctly.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
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
///
/// Not `#[non_exhaustive]`: draft §2.2's `ExecutableDefinition` has exactly two productions.
/// The variant set is the specification's, not smear's, so adding to it would mean parsing
/// something that is not GraphQL. `#[non_exhaustive]` here would advertise an evolution freedom
/// this type structurally does not have and charge for it where it costs most — the exhaustiveness
/// `smear-compiler`'s draft §5 rules depend on. Smear's extension mechanism is a separate dialect:
/// `graphqlx`'s own `ImportOrExecutableDefinition` keeps the attribute, correctly.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
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
/// Both keyworded alternatives carry one — `OperationDefinition : Description?
/// OperationType …` and `FragmentDefinition : Description? fragment …`. The
/// shorthand `OperationDefinition : SelectionSet` does not, so a described
/// wrapper around a shorthand operation is not a document this parser builds.
pub type DescribedExecutableDefinition<S, Ty = Type<Name<S>>> =
  Described<ExecutableDefinition<S, Ty>, S>;

/// A nonempty GraphQL executable document.
///
/// Each definition may carry its own leading description.
pub type ExecutableDocument<S, Ty = Type<Name<S>>> =
  crate::executable::Document<DescribedExecutableDefinition<S, Ty>>;
