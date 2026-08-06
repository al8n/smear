//! GraphQLx argument AST aliases.

use tokora::SimpleSpan;

use super::{ConstInputValue, DefaultVec, InputValue, Name};

/// A GraphQLx argument in an executable context.
pub type Argument<S, Span = SimpleSpan> =
  crate::parser::argument::Argument<Name<S, Span>, InputValue<S, Span>, Span>;

/// A collection of GraphQLx executable arguments.
pub type Arguments<S, Span = SimpleSpan, Container = DefaultVec<Argument<S, Span>>> =
  crate::parser::argument::ArgumentList<Argument<S, Span>, Container, Span>;

/// A GraphQLx argument in a constant context.
pub type ConstArgument<S, Span = SimpleSpan> =
  crate::parser::argument::Argument<Name<S, Span>, ConstInputValue<S, Span>, Span>;

/// A collection of GraphQLx constant arguments.
pub type ConstArguments<S, Span = SimpleSpan, Container = DefaultVec<ConstArgument<S, Span>>> =
  crate::parser::argument::ArgumentList<ConstArgument<S, Span>, Container, Span>;
