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

/// The materialised-number value tree: the same shape with a numeric `Int` leaf and an `f64`
/// `Float` one.
///
/// A module rather than a flat re-export because every name in it collides with its slice twin
/// by design. `materialized::InputValue<S, I>` is a second `enum`, variant for variant the shape
/// of [`InputValue`], and every one of its leaves except `Int` and `Float` is the *same type* the
/// slice tree holds. Each name also keeps its slice twin's arity and argument positions with `I`
/// inserted, so moving a consumer between the sets is a change of import plus one word.
///
/// **`I` is the width, and both readings of GraphQL's `Int` are instantiations of this one tree.**
/// [`i32`] is what draft §3.5.1 specifies; [`i64`] takes draft §2.9.1's unbounded grammar at its
/// word and accepts literals the specification does not. A refusal names its
/// [`IntWidth`](crate::graphql::error::IntWidth), so the two stay distinguishable in the error as
/// well as in the type.
///
/// Two enums rather than one at two instantiations is a decision the module's own header argues,
/// and the short version is that a type alias cannot be used as a module: `use InputValue::{Int}`
/// has to keep compiling. That argument is about the boundary between the *slice* tree and this
/// one and says nothing about the width — `I` is a parameter on an `enum`, so the variant imports
/// resolve at every width. What it costs — the variant list written twice — is charged to
/// `every_value_tree_declares_the_same_variants` rather than to the next reader.
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

/// The container the value and selection carriers hold their children in, and the trait that lets
/// it take one apart.
///
/// Re-exported here because it is the default `Container` argument of every value alias below, so
/// it reaches a consumer's signatures whether or not they name it. [`Nested`] is a `Vec` in every
/// respect a consumer can observe; what it adds is the iterative release that keeps a value nested
/// through these carriers from aborting the process on the way out, however deep it is. That
/// ranges over every recursive position the grammar forms, and not over a node a caller stored in
/// `S` — see [`Nested`]'s own documentation, which states the difference. [`Nestable`] is sealed,
/// which fixes who may implement it and says nothing about what a payload may be.
///
/// It is the default container of the selection aliases too, for the same reason and against the
/// same defect: an inline fragment owns a nested selection set and a field owns an optional one.
pub use crate::value::{Nestable, Nested};
