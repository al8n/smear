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
//! # What comes out
//!
//! [`analyze`] answers an [`Analysis`]: the [`Model`], its [`Diagnostics`] and its [`SourceMap`].
//! None of the three has a type parameter or a lifetime, whatever text type went in, and
//! [`Analysis::is_ok`] is the verdict. So far the model holds the packages, their modules, from
//! their directories and their inline `mod`s, and every definition, the built-in library's and the
//! primitives the program writes included, with canonical names and lookup: its kind, name,
//! module, location, origin and description, its contributions, its own and then its extensions',
//! its type parameters with their defaults and its bounds, its `implements` entries and union
//! members, its directive applications, a directive definition's locations, and its members with
//! their descriptions, types, defaults and applications, every written type an occurrence with
//! its path and arguments as written and what it resolves to, every application with its type
//! arguments and its arguments, and every value as written; and the schema, with its
//! contributions, one for each schema item, its description, its roots, written or presumed, and
//! its applications. The phases that write the rest of it are still to come.
//!
//! ```
//! use smear_analysis::graphqlx::{Layout, Limits, Package, SourceFile, Tier, analyze};
//!
//! let root = Package::new("app", Layout::Directory, Tier::Std)
//!   .with_files([SourceFile::new("schema.graphqlx", "type Query { ok: Int }")]);
//! let analysis = analyze(&root, &[], &Limits::new());
//! assert!(analysis.is_ok());
//!
//! // The texts stay with the caller, and are paired with the analysis again to render.
//! let model = analysis.model();
//! let file = model.package(model.root_package()).sources().into_iter().nth(1);
//! let texts = analysis.sources().bind(&root, &[]);
//! assert_eq!(file.and_then(|id| texts.text(id)), Some("type Query { ok: Int }"));
//! ```
//!
//! # Sources
//!
//! Every package has a package source, numbered before its files, and every file is a source. A
//! [`SourceId`] numbers them densely, and the [`SourceMap`] records each one's package, path and
//! length, never its text: the texts stay with the caller, and [`SourceMap::bind`] pairs the map
//! with them again for a renderer, finding each file by its package and its path. The built-in
//! packages `core`, `alloc` and `std` have [`PackageId`]s and sources like any other, and smear
//! supplies their texts itself.
//!
//! # Diagnostics
//!
//! A [`Diagnostic`] answers tokora's diagnostic contract, and [`Diagnostic::builder`] is public: a
//! consumer reports its own findings with its own codes, at the analysis's sources or at source
//! ids of its own, and they render exactly as smear's do. [`Diagnostics`] keeps them in one total
//! order, under a ceiling that keeps the least of them. [`DiagnosticKind`] lists the
//! `smear::graphqlx` codes smear reports, and a message of smear's quotes a name or a path from the
//! input only by a bounded excerpt of it.
//!
//! # Rules
//!
//! A rule is a [`Visitor`]: it implements the hooks it needs, and [`visit`] walks the model for
//! it, reaching every entity the chosen [`Packages`] wrote in id order and handing each hook a
//! [`Cx`] that says where the walk is, an occurrence's [`Site`] included, and takes what the rule
//! reports. Rules compose at compile time: a tuple of up to twelve visitors is a visitor, and so
//! are an `Option` of one, a borrowed or boxed one, and `()`.
//!
//! [`analyze`] runs smear's own rule set, which holds no rule yet, through the same walk over the
//! supplied packages, once its phases have written the model. An error that a phase or one of those
//! rules reports marks the contributions, members and definitions that hold where it points,
//! whether the ceiling keeps the diagnostic or not, and their `has_errors()` says so. A consumer's
//! walk marks nothing: it answers its own [`Diagnostics`] under the analysis's ceiling, which
//! [`Diagnostics::merge`] joins with the analysis's, and holds memory bounded by that ceiling
//! however much its visitor reports.

mod analysis;
mod builtin;
mod check;
mod code;
mod collect;
mod declare;
mod diagnostic;
mod id;
mod limits;
mod lower;
mod model;
mod package;
mod parse;
mod resolve;
mod rules;
mod set;
mod source;
mod text;
mod tree;
mod visitor;
mod widths;

// The model's builder, and analyses of the models it builds, for `smear`'s integration tests.
// Hidden, and compiled only with the feature: the builder is crate-private API.
#[cfg(feature = "test-support")]
#[doc(hidden)]
pub mod test_support;

pub use analysis::{Analysis, analyze};
pub use code::DiagnosticKind;
pub use diagnostic::{Diagnostic, DiagnosticBuilder, Diagnostics};
pub use id::{
  ApplicationId, DefId, IdIter, IdRange, MemberId, ModuleId, PackageId, SourceId, Sym, TyId,
  TypeExprId, ValueId,
};
pub use limits::Limits;
pub use model::{
  Application, ApplicationArgument, Builtins, CanonicalName, Collection, Contribution,
  Contributions, Def, DefKind, Description, DirectiveLocation, Extern, Externs, FloatValue,
  IntValue, Literal, LiteralKind, Member, MemberKind, Model, Module, ModuleKind, Namespace,
  ObjectEntry, Origin, PackageInfo, PackageKind, Path, PathRoot, Primitive, Resolution, Root,
  RootOperation, Schema, Ty, TyKind, TypeBound, TypeExpr, TypeExprKind, TypeParam, TypeParams,
  Value, ValueKind,
};
pub use package::{IntegerProvider, Layout, Package, SourceFile, Tier};
pub use source::{Bound, Source, SourceMap};
pub use text::SourceText;
pub use visitor::{Cx, Packages, Site, Visitor, visit};
