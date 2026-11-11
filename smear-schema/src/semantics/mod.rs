//! The parts of the schema builder that do not depend on what a schema is.
//!
//! # Nothing here reaches back into the crate
//!
//! Code in this directory imports from `core` and `std` alone: no path in it leaves the directory,
//! and no item of the rest of this crate is named in code or linked from a doc, only mentioned in
//! prose. The directory is to become the `smear-semantics` crate, which the builder and a second
//! analyzer will both depend on, and that move stays a `git mv` only while nothing in here names
//! the builder.

pub(crate) mod duplicates;
pub(crate) mod interner;
pub(crate) mod scc;
