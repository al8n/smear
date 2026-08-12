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
/// `every_value_tree_declares_the_same_variants` rather than to the next reader.
#[cfg(feature = "materialized-numbers")]
#[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
pub mod materialized;

/// The same tree at the width GraphQL specifies: `Int` is [`i32`], `Float` stays `f64`.
///
/// **This one is the spec-exact reading and [`materialized`] is the permissive one**, which is
/// the opposite of how a "32" beside a "64" usually reads. Draft §3.5.1 defines `Int` as a signed
/// 32-bit integer, while draft §2.9.1's grammar bounds an `IntValue`'s digits not at all — so
/// `2147483648` parses, is not a conformant `Int`, and the two trees are the two honest answers
/// to what the document says. A refusal names its
/// [`IntWidth`](crate::graphql::error::IntWidth), so the two remain distinguishable in the error
/// as well as in the type.
///
/// A sibling module rather than a payload parameter on [`materialized`]'s enums, and a second
/// marker rather than a parameterised one: that module's header has the three compile-time
/// failures the choice avoids.
#[cfg(feature = "materialized-numbers")]
#[cfg_attr(docsrs, doc(cfg(feature = "materialized-numbers")))]
pub mod materialized32;

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
