//! GraphQL `Argument`/`Arguments` AST node types.
//!
//! The single [`Argument`] node is copied type-only from the frozen `smear-parser`
//! crate (`graphql/ast/default.rs`): keyed by the source slice `S`, generic over the
//! value an argument carries (executable [`InputValue`] or constant
//! [`ConstInputValue`]).
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

use smear_scaffold::ast as scaffold;
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{ConstInputValue, InputValue, Name};

/// Argument in an executable context (can contain variables).
pub type Argument<S> = scaffold::Argument<Name<S>, InputValue<S>>;

/// List of arguments in an executable context.
pub type Arguments<S> = ArgumentList<Argument<S>>;

/// Argument in a constant context (no variables, used in schemas).
pub type ConstArgument<S> = scaffold::Argument<Name<S>, ConstInputValue<S>>;

/// List of constant arguments.
pub type ConstArguments<S> = ArgumentList<ConstArgument<S>>;

/// A parenthesised argument list — the arguments and the span covering the whole
/// `( … )`, and nothing else.
///
/// The delimiter-free twin of the frozen `scaffold::Arguments`: it carries no
/// `LParen`/`RParen` tokens, because parser-next's AST keeps only the information a
/// consumer needs plus the grammar structure (plan Amendment 7). The whole-list
/// [`span`](Self::span) still bounds the parentheses, so nothing locatable is lost.
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
  /// Creates a new argument list from the span covering the whole `( … )` and the
  /// parsed arguments.
  #[inline]
  pub const fn new(span: Span, arguments: Container) -> Self {
    Self {
      span,
      arguments,
      _arg: PhantomData,
    }
  }

  /// Returns the source span of the entire argument list, opening parenthesis
  /// through closing parenthesis.
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
