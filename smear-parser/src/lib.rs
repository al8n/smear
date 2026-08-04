#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![allow(clippy::type_complexity)]
#![deny(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Re-exported lexer crate.
pub use smear_lexer as lexer;

pub mod combinator;

/// Name-node carrier shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod name;

/// Namespaced-path carrier used by the GraphQLx dialect AST; vanilla GraphQL
/// has no namespaced paths.
#[cfg(feature = "graphqlx")]
mod path;

/// Generic-definition carriers shared by extended GraphQL-family dialect ASTs.
#[cfg(feature = "graphqlx")]
mod generic;

/// Selection-node carriers shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod selection;

/// Executable-document carriers shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod executable;

/// Type-reference carriers shared by the GraphQL-family dialect ASTs.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod ty;

/// Argument-node carriers shared by the dialect ASTs, copied type-only from the
/// frozen crate.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod argument;

/// Directive-node carriers shared by the dialect ASTs, copied type-only from the
/// frozen crate.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod directive;

/// Type-system AST carriers shared by the GraphQL-family dialects.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod type_system;

/// Value-node carriers shared by the dialect ASTs, copied type-only from the
/// frozen crate.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod value;

/// The GraphQL dialect: productions, syntax kinds, keyword atoms, AST node types,
/// and the dialect error, all keyed to the [`GraphQL`](graphql::GraphQL) marker.
#[cfg(feature = "graphql")]
pub mod graphql;

/// The GraphQLx dialect: namespaced paths, collection values, generic type
/// references, and imports over the concrete GraphQLx lexer.
#[cfg(feature = "graphqlx")]
pub mod graphqlx;
