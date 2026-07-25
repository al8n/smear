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
pub use map::*;
pub use null_value::*;
pub use object::*;
pub use set::*;
pub use string::*;
pub use variable::*;

mod boolean_value;
mod default_input_value;
mod enum_value;
mod float;
mod int;
mod list;
mod map;
mod null_value;
mod object;
mod set;
mod string;
mod variable;

#[cfg(test)]
mod tests;
