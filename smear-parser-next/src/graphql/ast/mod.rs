//! GraphQL AST node types and GraphQL-specialized aliases over shared carriers.
//!
//! Copied type-only from the frozen `smear-parser` crate (`graphql/ast/`): the node
//! types the productions return, keyed by the source slice `S` with spans pinned to
//! [`SimpleSpan`](tokora::SimpleSpan). Source-independent carriers are shared at
//! crate level and specialized here for GraphQL. The productions are rebuilt on
//! the atom layer, wave by wave.

use std::vec::Vec;

pub use argument::*;
pub use directive::*;
pub use name::*;
pub use ty::*;
pub use value::*;

mod argument;
mod directive;
mod name;
mod ty;
mod value;

/// The default container type used for AST collections (lists, objects).
pub type DefaultVec<T> = Vec<T>;
