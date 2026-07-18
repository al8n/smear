//! GraphQL AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate (`graphql/ast/`): the node
//! types the productions return, keyed by the source slice `S` with spans pinned to
//! [`SimpleSpan`](tokora::SimpleSpan). The parser fns that lived beside them in the
//! frozen crate are *not* copied — the productions are rebuilt on the atom layer,
//! wave by wave.

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
