//! GraphQL-specialized `Argument`/`Arguments` AST node aliases.
//!
//! The copied carriers are shared by GraphQL-family dialects at crate level. This
//! module binds them to GraphQL names and executable or constant input values.

pub use crate::argument::ArgumentList;

use super::{ConstInputValue, InputValue, Name};

/// A GraphQL argument in an executable context.
///
/// Its span covers the argument name through the end of its value.
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
pub type Argument<S> = crate::argument::Argument<Name<S>, InputValue<S>>;

/// List of arguments in an executable context.
///
/// An absent collection is empty with a zero-width span and consumes no input.
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
pub type Arguments<S> = ArgumentList<Argument<S>>;

/// Argument in a constant context (no variables, used in schemas).
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
pub type ConstArgument<S> = crate::argument::Argument<Name<S>, ConstInputValue<S>>;

/// List of constant arguments.
///
/// An absent collection is empty with a zero-width span and consumes no input.
///
/// See the [GraphQL Arguments specification](https://spec.graphql.org/draft/#sec-Language.Arguments).
pub type ConstArguments<S> = ArgumentList<ConstArgument<S>>;
