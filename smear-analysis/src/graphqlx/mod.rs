//! The GraphQLx schema analyzer.
//!
//! Packages of GraphQLx source go in. A resolved model of what they declare comes out, with the
//! diagnostics that say what could not be resolved or was refused.
//!
//! # What goes in
//!
//! A [`Package`] is a name, a [`Layout`], a [`Tier`], its [`SourceFile`]s, the names of the
//! packages it depends on, and optionally an [`IntegerProvider`]. A file's text is any
//! [`SourceText`]: `&str`, `&[u8]`, or one of the owned or shared texts behind the source
//! features. [`Limits`] bounds what an analysis may spend.
//!
//! The analysis is Sans-I/O: the caller walks its directories, applies its include rules, reads
//! its files and reads its manifest, and supplies every package up front.
//!
//! # Sources
//!
//! Every package has a package source, numbered before its files, and every file is a source. A
//! [`SourceId`] numbers them densely, and the [`SourceMap`] records each one's package, path and
//! length, never its text: the texts stay with the caller, and [`SourceMap::bind`] pairs the map
//! with them again for a renderer. The built-in packages `core`, `alloc` and `std` have
//! [`PackageId`]s and sources like any other, and smear supplies their texts itself.

mod builtin;
mod id;
mod limits;
mod package;
mod set;
mod source;
mod text;

pub use id::{IdIter, IdRange, PackageId, SourceId};
pub use limits::Limits;
pub use package::{IntegerProvider, Layout, Package, SourceFile, Tier};
pub use source::{Bound, Source, SourceMap};
pub use text::SourceText;
