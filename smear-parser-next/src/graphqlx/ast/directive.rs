//! GraphQLx `Directive`/`Directives` AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate
//! (`graphqlx/ast/default.rs`): keyed by the source slice `S`, generic over the
//! arguments a directive carries (executable [`Arguments`] or constant
//! [`ConstArguments`]).

use smear_scaffold::ast as scaffold;

use super::{Arguments, ConstArguments, Name};

/// Directive with executable arguments (can contain variables).
pub type Directive<S> = scaffold::Directive<Name<S>, Arguments<S>>;

/// List of directives with executable arguments.
pub type Directives<S> = scaffold::Directives<Directive<S>>;

/// Directive with constant arguments (no variables).
pub type ConstDirective<S> = scaffold::Directive<Name<S>, ConstArguments<S>>;

/// List of directives with constant arguments.
pub type ConstDirectives<S> = scaffold::Directives<ConstDirective<S>>;
