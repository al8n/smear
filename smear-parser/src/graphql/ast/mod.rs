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

/// The materialised-number value tree: the same shape with `i64` and `f64` numeric leaves.
///
/// A module rather than a flat re-export because every name in it collides with its slice twin
/// by design. `materialized::InputValue<S>` is a second `enum`, variant for variant the shape of
/// [`InputValue`], and every one of its leaves except `Int` and `Float` is the *same type* the
/// slice tree holds. Each name also keeps its slice twin's arity and argument positions, so
/// moving a consumer between the sets is a change of import and nothing else.
///
/// Two enums rather than one at two instantiations is a decision the module's own header argues,
/// and the short version is that a type alias cannot be used as a module: `use InputValue::{Int}`
/// has to keep compiling. What it costs — the variant list written twice — is charged to
/// `the_two_value_trees_have_the_same_variants` rather than to the next reader.
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
