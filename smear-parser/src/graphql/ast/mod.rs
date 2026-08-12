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
pub use executable::*;
pub use field::*;
pub use name::*;
pub use ty::*;
pub use type_system::*;
pub use value::*;

/// The materialised-number alias set: the same nodes with `i64` and `f64` numeric leaves.
///
/// A module rather than a flat re-export because every name in it collides with its slice twin
/// by design — `materialized::InputValue<S>` is `InputValueOf<S, i64, f64>` where the slice
/// `InputValue<S>` is `InputValueOf<S, S, S>`, and what the module boundary says is that the two
/// are one carrier at two instantiations rather than two types. Each twin also keeps its slice
/// counterpart's arity and argument positions, so moving between the sets is a change of import.
#[cfg(feature = "materialized-numbers")]
#[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
pub mod materialized;

mod argument;
mod directive;
mod executable;
mod field;
mod name;
mod ty;
mod type_system;
mod value;

/// The default container type used for AST collections (lists, objects).
pub type DefaultVec<T> = Vec<T>;
