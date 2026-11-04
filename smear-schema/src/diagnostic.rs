//! The diagnostic contract smear's errors answer: tokora's, re-exported.
//!
//! [`Diagnose`] and its vocabulary are declared in [`tokora::diagnostic`], and that module's
//! documentation is the contract's own: what a diagnostic answers, why the accessors are indexed,
//! and what [`Labels`] and [`PathSegments`] do when an implementor's count and accessor disagree.
//! This module re-exports each item under the path smear has always published it at, so
//! `smear::diagnostic::Diagnose` and `tokora::diagnostic::Diagnose` are one trait. A renderer
//! written against tokora's contract, such as painty's `tokora` adapter, therefore takes smear's
//! errors as they are, with no glue on either side.
//!
//! # What smear's implementors answer
//!
//! - **Codes** are `smear::<family>::<rule>`: `smear::syntax::unterminated-string`,
//!   `smear::schema::duplicate-type-name`, `smear::validation::field-selection-merging`,
//!   `smear::introspection::malformed-json`. One namespace spans the four families, so a renderer
//!   holding all of them never needs to know which enum a code came from. The syntax family's
//!   kinds are `smear-lexer`'s `SyntaxErrorKind`, whose lexer and parser errors implement the
//!   contract straight from tokora.
//! - **Every severity is [`Severity::Error`]** today. The other rungs exist for a deprecation-lint
//!   class that does not exist yet, and nothing here invents a warning to justify them.
//! - **[`Location::entire`] has one user**, the introspection door's `ResponseError`, whose input
//!   is a machine-generated response with no position worth pointing at. Every other family points
//!   at the bytes it refused.
//! - **No path segments.** All four refuse an input before any request has produced a result,
//!   so none has a field of one to point at.
//!
//! A schema refusal, a validation diagnostic and an introspection refusal erase to the same
//! `&dyn Diagnose`, and `smear/tests/validator_allocation.rs` measures that reading each of them
//! that way, and writing its message out, allocates nothing.

// Every item by name, from a module of this crate rather than a `pub use tokora::diagnostic;` at
// the root. `ci/source_census` finds the contract trait through a NAMED re-export, since a glob
// or a module re-export publishes no `Diagnose` it can follow, and D2 asserts every contract row
// through the path that walk yields. Either shortcut would leave the census with no spelling of
// the trait to assert through, which it refuses as a hard error.
pub use tokora::diagnostic::{
  Code, Diagnose, DiagnoseExt, Label, Labels, Location, PathSegment, PathSegments, Severity,
};
