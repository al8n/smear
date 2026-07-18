#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![deny(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Re-exported lexer crate.
pub use smear_lexer as lexer;

/// Re-exported scaffold crate.
pub use smear_scaffold as scaffold;

pub mod combinator;
pub mod entry;

/// The [`Ident`](ident::Ident) carrier the dialect `Name` node types alias.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod ident;

/// Value-node carriers shared by the dialect ASTs, copied type-only from the
/// frozen crate.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod value;

/// The GraphQL dialect: productions, syntax kinds, keyword atoms, AST node types,
/// and the dialect error, all keyed to the [`GraphQL`](graphql::GraphQL) marker.
#[cfg(feature = "graphql")]
pub mod graphql;
