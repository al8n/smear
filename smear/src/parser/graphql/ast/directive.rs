//! GraphQL-specialized `Directive`/`Directives` AST node aliases.
//!
//! The copied carriers are shared by GraphQL-family dialects at crate level. This
//! module binds them to GraphQL names and executable or constant arguments.

use super::{Arguments, ConstArguments, Name};

/// A GraphQL directive with executable arguments.
///
/// Empty argument collections are represented by `None` on this node.
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
pub type Directive<S> = crate::directive::Directive<Name<S>, Arguments<S>>;

/// Directive with constant arguments (no variables).
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
pub type ConstDirective<S> = crate::directive::Directive<Name<S>, ConstArguments<S>>;

/// List of directives with executable arguments.
///
/// An absent collection is empty with a zero-width span and consumes no input.
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
pub type Directives<S> = crate::directive::Directives<Directive<S>>;

/// List of directives with constant arguments.
///
/// An absent collection is empty with a zero-width span and consumes no input.
///
/// See the [GraphQL Directives specification](https://spec.graphql.org/draft/#sec-Language.Directives).
pub type ConstDirectives<S> = crate::directive::Directives<ConstDirective<S>>;
