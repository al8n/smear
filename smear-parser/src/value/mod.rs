//! Value-node carriers shared by the dialect ASTs.
//!
//! Copied type-only from the frozen `smear-parser` crate (`src/value/`): the
//! boolean / enum / int / float / string / null / variable value nodes and the
//! minimal collection carriers the dialect input-value enums are built from.
//! Each is generic over the source slice `S` where applicable and over `Span`, which defaults to
//! [`SimpleSpan`](tokora::SimpleSpan). Literal carriers also have a `Lang` marker
//! for dialect type safety; variables instead inherit their dialect from the name
//! node they contain. The carriers expose constructors plus span and source
//! accessors; parsers and external AST builders can construct them without
//! allocating beyond their selected collection container.
//!
//! # `S` is a leaf, and the type says so
//!
//! The source representation is open — a borrowed slice, an owned buffer, an interned handle —
//! and it is *not* unconstrained. Every door that carries an outside `S` into a value-tree
//! position takes a `Leaf` bound: the four on `Name`, which is the only carrier in a tree position
//! whose `S` a caller can supply. Implementing `Leaf` asserts that dropping the type runs no
//! destructor able to reach a node of any tree this crate releases iteratively — value, type or
//! selection — which is the property those releases need of a leaf
//! and had no way to require.
//!
//! Without it, a caller could instantiate `S` with a type owning an input value and release a
//! chain that descends one native frame per level, past `Nested` and into an abort
//! (`al8n/smear#176`). The same bound sits on `Span` — every carrier here owns one by value, and
//! GraphQLx leaves it a parameter — and on the materialised `I`, so all three payload parameters
//! are leaves by obligation. `nesting.rs`'s `Leaf` carries the contract, the placement
//! measurement and the residual; its module header derives the parameter list the bound ranges
//! over.

// Some scalar constructors remain crate-private to preserve their existing
// parser-minted invariant. The copied collection carriers intentionally keep
// their public constructors, matching the scaffold types they replace.
#![allow(dead_code)]

pub use boolean_value::*;
pub use default_input_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use list::*;
#[cfg(feature = "graphqlx")]
pub use map::*;
pub use null_value::*;
pub use object::*;
#[cfg(feature = "graphqlx")]
pub use set::*;
pub use string::*;
pub use variable::*;

pub(crate) use nesting::Sealed;
pub use nesting::{Absent, Leaf, Nest, NestNode, NestPtr, Nestable, Nested, SoleNestPtr, Worklist};

mod boolean_value;
mod default_input_value;
mod enum_value;
mod float;
mod int;
mod list;
#[cfg(feature = "graphqlx")]
mod map;
mod nesting;
mod null_value;
mod object;
#[cfg(feature = "graphqlx")]
mod set;
mod string;
mod variable;

#[cfg(test)]
mod tests;
