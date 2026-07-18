//! GraphQLx generic-parameter and where-clause AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate (the generic / where
//! aliases in `graphqlx/ast/default.rs`): the type-parameter lists, the
//! name-with-generics carriers, and the where-clause predicates, aliased onto the
//! [`smear_scaffold`](crate::scaffold) `generic` vocabulary. GraphQLx definitions,
//! extensions, and executable definitions each carry their own generics flavor.

use smear_scaffold::ast as scaffold;

use super::Type;
use crate::ident::Ident;

/// A generic type parameter on a definition, with an optional default type.
pub type DefinitionTypeParam<S, Ty = Type<S>> =
  scaffold::generic::DefinitionTypeParam<Ident<S>, Ty>;
/// A generic parameter list (`< … >`) on a definition.
pub type DefinitionTypeGenerics<S, Ty = Type<S>> =
  scaffold::generic::DefinitionTypeGenerics<Ident<S>, Ty>;
/// A generic type parameter on an extension (no default type).
pub type ExtensionTypeParam<S> = scaffold::generic::ExtensionTypeParam<Ident<S>>;
/// A generic parameter list (`< … >`) on an extension.
pub type ExtensionTypeGenerics<S> = scaffold::generic::ExtensionTypeGenerics<Ident<S>>;
/// A generic parameter list (`< … >`) on an executable definition.
pub type ExecutableDefinitionTypeGenerics<S> =
  scaffold::generic::ExecutableDefinitionTypeGenerics<Ident<S>>;

/// A definition name with optional generics (`Name< … >`).
pub type DefinitionName<S, Ty = Type<S>> = scaffold::generic::DefinitionName<Ident<S>, Ty>;
/// An extension name — a path with optional generics (`a::B< … >`).
pub type ExtensionName<S> = scaffold::generic::ExtensionName<Ident<S>>;
/// An executable-definition name with optional generics (`Name< … >`).
pub type ExecutableDefinitionName<S> = scaffold::generic::ExecutableDefinitionName<Ident<S>>;

/// A single where-clause predicate (`T: bound & bound`).
pub type WherePredicate<S, Ty = Type<S>> = scaffold::generic::WherePredicate<Ident<S>, Ty>;
/// A where clause (`where T: …, U: …`).
pub type WhereClause<S, Ty = Type<S>> = scaffold::generic::WhereClause<Ident<S>, Ty>;
