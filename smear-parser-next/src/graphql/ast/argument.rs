//! GraphQL `Argument`/`Arguments` AST node types.
//!
//! [`Argument`] is local to this crate so its parser can be exposed as an inherent
//! API. It is keyed by the source slice `S` and generic over the value an argument
//! carries (executable [`InputValue`] or constant [`ConstInputValue`]).
//!
//! The list node [`ArgumentList`] is *not* the frozen `scaffold::Arguments`: that
//! type stores the `(`/`)` delimiter tokens, and parser-next's AST drops pure
//! delimiters (plan Amendment 7 — "we are AST: only useful information and grammar
//! correctness"; the whole-list span suffices, and the blackholing delimited
//! builder no longer has to hand parens back). The frozen `scaffold::Arguments`
//! stays exactly as it is — it is the parity oracle — so parser-next carries its own
//! delimiter-free list node here instead.

use core::marker::PhantomData;

use std::vec::Vec;

use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{ConstInputValue, InputValue, Name};

/// A GraphQL argument in an executable context by default.
///
/// An argument commits to the `Name : Value` production. Its span covers the
/// argument name through the end of its value.
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
#[derive(Debug, Clone, Copy)]
pub struct Argument<S, Value = InputValue<S>> {
  span: Span,
  name: Name<S>,
  value: Value,
}

impl<S, Value> AsSpan<Span> for Argument<S, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Value> IntoSpan<Span> for Argument<S, Value> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Value> IntoComponents for Argument<S, Value> {
  type Components = (Span, Name<S>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.value)
  }
}

impl<S, Value> Argument<S, Value> {
  /// Creates a GraphQL argument.
  #[inline]
  pub const fn new(span: Span, name: Name<S>, value: Value) -> Self {
    Self { span, name, value }
  }

  /// Returns the span from the argument name through its value.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the argument name.
  #[inline]
  pub const fn name(&self) -> &Name<S> {
    &self.name
  }

  /// Returns the argument value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }
}

/// List of arguments in an executable context.
pub type Arguments<S> = ArgumentList<Argument<S>>;

/// Argument in a constant context (no variables, used in schemas).
pub type ConstArgument<S> = Argument<S, ConstInputValue<S>>;

/// List of constant arguments.
pub type ConstArguments<S> = ArgumentList<ConstArgument<S>>;

/// A GraphQL argument collection.
///
/// The delimiter-free twin of the frozen `scaffold::Arguments`: it carries no
/// `LParen`/`RParen` tokens, because parser-next's AST keeps only the information a
/// consumer needs plus the grammar structure (plan Amendment 7). A present list's
/// [`span`](Self::span) bounds its parentheses; an absent list is empty with a
/// zero-width span at the parser's starting offset.
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
///
/// ## Generic Parameters
///
/// - `Arg`: the type representing individual arguments
/// - `Container`: the collection holding the arguments (defaults to `Vec`)
#[derive(Debug, Clone, Copy)]
pub struct ArgumentList<Arg, Container = Vec<Arg>> {
  span: Span,
  arguments: Container,
  _arg: PhantomData<Arg>,
}

impl<Arg, Container> AsSpan<Span> for ArgumentList<Arg, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Arg, Container> IntoSpan<Span> for ArgumentList<Arg, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Arg, Container> IntoComponents for ArgumentList<Arg, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.arguments)
  }
}

impl<Arg, Container> ArgumentList<Arg, Container> {
  /// Creates an argument list from its span and parsed arguments.
  #[inline]
  pub const fn new(span: Span, arguments: Container) -> Self {
    Self {
      span,
      arguments,
      _arg: PhantomData,
    }
  }

  /// Returns the collection span.
  ///
  /// Present lists span their parentheses. Absent lists use a zero-width span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the container holding the arguments.
  #[inline]
  pub const fn arguments(&self) -> &Container {
    &self.arguments
  }
}
