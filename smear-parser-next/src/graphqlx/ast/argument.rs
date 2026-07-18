//! GraphQLx `Argument`/`Arguments` AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate
//! (`graphqlx/ast/default.rs`): keyed by the source slice `S`, generic over the
//! value an argument carries (executable [`InputValue`] or constant
//! [`ConstInputValue`]).

use smear_scaffold::ast as scaffold;

use super::{ConstInputValue, InputValue, Name};

/// Argument in an executable context (can contain variables).
pub type Argument<S> = scaffold::Argument<Name<S>, InputValue<S>>;

/// List of arguments in an executable context.
pub type Arguments<S> = scaffold::Arguments<Argument<S>>;

/// Argument in a constant context (no variables, used in schemas).
pub type ConstArgument<S> = scaffold::Argument<Name<S>, ConstInputValue<S>>;

/// List of constant arguments.
pub type ConstArguments<S> = scaffold::Arguments<ConstArgument<S>>;
