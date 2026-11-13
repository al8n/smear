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
//! primitives the program writes included, with canonical names and lookup: its kind, name, module,
//! location, origin and description, its contributions, its own and then its extensions', its type
//! parameters with their defaults and its bounds, its `implements` entries and union members, its
//! directive applications, a directive definition's locations, whether its own package makes it a
//! marker or a collection, the closed instantiations its written occurrences resolve to, and its
//! members with their descriptions, types, defaults and applications; every written type an
//! occurrence with its path and arguments as written, what it resolves to, and its type, the
//! complete instantiation with defaults filled and an alias expanded, each type interned once;
//! every application with its type arguments and its arguments, and every value as written; and the
//! schema, with its contributions, one for each schema item, its description, its roots, written or
//! presumed, and its applications. [`Model::satisfies`] and [`Model::satisfies_ty`] answer what a
//! type satisfies, read off what it declares, and every instantiation the program writes has its
//! bounds judged so: a type argument that does not satisfy a bound of its definition is
//! `smear::graphqlx::unsatisfied-bound`, where the instantiation is written. The phases that write
//! the rest of it, values and tiers, are still to come.
//!
//! [`analyze_with`] answers the same analysis with the parser the caller chose, a GraphQLx parser
//! layer, and the [`Trees`] that layer kept beside it, one for each file it read.
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
//! [`analyze`] runs smear's own rules through the same walk over the supplied packages, once its
//! phases have written the model. A program defines its schema at most once: each schema
//! definition the root package writes after its first is reported with GraphQL's own code,
//! `smear::schema::duplicate-schema-definition`. The schema is the root package's: a schema
//! definition or extension a dependency writes is `smear::graphqlx::schema-outside-root`. And a
//! package extends its own definitions alone: an extension of another supplied package's type or
//! directive is `smear::graphqlx::orphan-extension`, and so is an extension of a definition of
//! `core`, `alloc` or `std`, unless the root package writes it and it adds directives and nothing
//! else, none of them `core`'s `@marker`, `@set` or `@map` unless it wrote the definition, a
//! built-in it replaces; the root package's extension that adds more to one of GraphQL's
//! introspection types keeps GraphQL's code, `smear::schema::introspection-type-extension`. And
//! an enum takes no type parameter: an enum that declares one is
//! `smear::graphqlx::generic-enum`, at its first type parameter's name. And a marker is used as a
//! marker: one that declares a field, one named anywhere but as the outermost type of an
//! `implements` entry or of a bound, and an `implements` entry of a scalar, an enum, an input
//! object or a union that names what is no marker, a type parameter among them, are each
//! `smear::graphqlx::marker-misuse`. And a set has one type parameter and a map two: a scalar its
//! own package makes a collection with another count, or makes both, is
//! `smear::graphqlx::collection-arity`. And `TypeRef` stands only as the type of a directive
//! definition's argument or of an input field, under lists alone: anywhere else it is
//! `smear::graphqlx::typeref-misuse`. And `NonZero` makes an integer never zero: `NonZero` of what
//! is no integer primitive at least one bit wide, written without `!`, is
//! `smear::graphqlx::nonzero-argument`. And a definition's type parameters have distinct names, and
//! so do the names an extension's own list gives them: a type parameter with the name of an
//! earlier one is `smear::graphqlx::duplicate-type-parameter`. And what a definition a supplied
//! package wrote holds keeps GraphQL's own rules, with GraphQL's codes: a member whose name begins
//! with `__` is `smear::schema::reserved-field-name`, or the code of what else it is; and a member
//! whose name an earlier member of its list has is `smear::schema::duplicate-field-name`, or the
//! code of its list; and a definition that holds members and holds none, its extensions'
//! counted, is `smear::schema::empty-fields-definition`, or the code of its kind, a marker
//! holding none; and a member typed by a definition of a kind its position does not take is
//! `smear::schema::field-type-not-output-type`, or the code of its position; and an `implements`
//! entry of an object or an interface that names what is no interface is
//! `smear::schema::implements-non-interface`; and an interface on a cycle of `implements` entries
//! is `smear::schema::self-implementing-interface`; and a union member that is no object type is
//! `smear::schema::union-member-not-object`; and a required argument or input field that applies
//! `core`'s `@deprecated` is `smear::schema::deprecated-required-argument`, or the code of an input
//! field; and a field written non-null of an input object its own package makes OneOf is
//! `smear::schema::one-of-field-not-nullable`, and one with a default
//! `smear::schema::one-of-field-has-default`; and `@oneOf` in an extension of an input object is
//! `smear::schema::one-of-on-input-object-extension`. A rule says nothing about a contribution a
//! phase already refused something in.
//!
//! An error that a phase or one of those rules reports marks the contributions, members and
//! definitions that hold where it points, whether the ceiling keeps the diagnostic or not, and
//! their `has_errors()` says so. A syntax error marks nothing. A consumer's walk marks nothing: it
//! answers its own [`Diagnostics`] under the analysis's ceiling, which [`Diagnostics::merge`]
//! joins with the analysis's, and holds memory bounded by that ceiling however much its visitor
//! reports.

mod analysis;
mod builtin;
mod check;
mod code;
mod collect;
mod declare;
mod diagnostic;
mod id;
mod instantiate;
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

pub use analysis::{Analysis, Trees, TreesIntoIter, TreesIter, analyze, analyze_with};
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
