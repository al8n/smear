//! Value-node carriers shared by the dialect ASTs.
//!
//! Copied type-only from the frozen `smear-parser` crate (`src/value/`): the
//! boolean / enum / int / float / string / null / variable value nodes the
//! dialect input-value enums are built from. Each is generic over the source slice
//! `S`, keeps its span as [`SimpleSpan`](tokora::SimpleSpan), and exposes only span
//! and source accessors — the assemblies construct them, the productions and tests
//! read them.

// The `pub(crate)` `new` constructors are the substrate the Wave 1+ value
// productions build these nodes with; until those productions land they have no
// in-crate caller. Kept crate-private (per the frozen crate) so only the parser
// mints nodes — external users read them out of parse results.
#![allow(dead_code)]

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;
pub use variable::*;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;
mod variable;
