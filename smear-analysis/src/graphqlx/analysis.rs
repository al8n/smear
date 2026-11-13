//! The entrances, [`analyze`] and [`analyze_with`], and what they answer, [`Analysis`] and
//! [`Trees`].

use core::{cmp::Ordering, iter::FusedIterator};
use std::{string::String, vec::Vec};

use smear_parser::{
  graphqlx::{Layer, Syntactic},
  layer::SyntaxTree,
};
use tokora::diagnostic::Location;

use super::{
  check, collect, declare,
  diagnostic::{Diagnostics, Reporter},
  id::{PackageId, SourceId},
  instantiate,
  limits::Limits,
  lower,
  model::{Model, ModelBuilder, PackageKind},
  package::Package,
  parse::{self, Documents},
  resolve,
  rules::Rules,
  set::{self, PackageView, ROOT},
  source::SourceMap,
  text::SourceText,
  visitor::{Packages, Visitor, drive},
};

/// What one analysis found: the model, the diagnostics, and the source map.
///
/// It has no type parameter and no lifetime: it is the same type whatever text type the packages
/// were supplied in, and it borrows nothing from them. The model and the diagnostics are complete
/// whatever was refused; [`is_ok`](Self::is_ok) is the verdict.
#[derive(Debug, Clone)]
pub struct Analysis {
  model: Model,
  diagnostics: Diagnostics,
  sources: SourceMap,
  /// The diagnostic ceiling it was made with, which a consumer's walk keeps to as well.
  ceiling: usize,
}

impl Analysis {
  /// The analysis `analyze` would conclude from the finished `model`, with `rules` run in place of
  /// smear's own rule set, as the test-support door hands it to its tests: a source map of the
  /// model's packages' sources, and `limits`' diagnostic ceiling.
  #[cfg(feature = "test-support")]
  pub(crate) fn of_model<V: Visitor + ?Sized>(
    model: Model,
    limits: &Limits,
    rules: &mut V,
  ) -> Self {
    let sources = SourceMap::of_model(&model);
    let reporter = Reporter::new(limits.max_diagnostics() as usize);
    conclude(model, sources, reporter, rules)
  }

  /// The diagnostic ceiling the analysis was made with: how many diagnostics a list it answers,
  /// or a walk over it answers, keeps.
  #[inline]
  pub(crate) fn ceiling(&self) -> usize {
    self.ceiling
  }

  /// The model of what the packages declare.
  #[inline]
  pub fn model(&self) -> &Model {
    &self.model
  }

  /// The diagnostics, in their one total order, under the ceiling the analysis was given.
  #[inline]
  pub fn diagnostics(&self) -> &Diagnostics {
    &self.diagnostics
  }

  /// The sources the diagnostics' locations number.
  #[inline]
  pub fn sources(&self) -> &SourceMap {
    &self.sources
  }

  /// Whether nothing was refused: no diagnostic of
  /// [`Severity::Error`](tokora::diagnostic::Severity::Error) was reported, kept or dropped by the
  /// ceiling.
  #[inline]
  pub fn is_ok(&self) -> bool {
    !self.diagnostics.has_errors()
  }
}

/// Analyzes `root` beside `dependencies`, within `limits`.
///
/// # What it does
///
/// - Numbers the packages, `core`, `alloc` and `std` first, then `root`, then `dependencies` by
///   name, and numbers each package's package source and then its files by path. The order the
///   dependencies, or a package's files, are passed in changes no id and no diagnostic.
/// - Checks the package set, reporting each violation at the whole of the offending package's
///   package source: two files with one path; a path that is absolute, uses `\`, is not UTF-8, or
///   has an empty, `.` or `..` segment; a package name that is not a Name, or is `core`, `alloc` or
///   `std`; two supplied packages with one name; a declared dependency that is not supplied.
///   Supplied sources over [`Limits::max_source_bytes`] are reported at the root, and then no
///   supplied file is parsed.
/// - Parses every file with the GraphQLx syntactic parser. A file's syntax errors are reported at
///   that file, with the parser's own code, message, labels and help, and the other files are
///   parsed all the same.
/// - Builds each package's module tree: its root module; in the
///   [`Directory`](super::Layout::Directory) layout a module for every directory that holds a file
///   and for every directory on the way to one, each directory's files sharing its module, and in
///   the [`Flat`](super::Layout::Flat) layout every file in the root module; and a module for every
///   inline `mod`, inside the module it is written in. A directory whose name is not a Name, a
///   directory or a `mod` nested deeper than [`Limits::max_module_depth`], and a `mod` of a module
///   the package already has are reported. Every package's root module is held; the first module a
///   supplied package's directory or `mod` would add past [`Limits::max_modules`] is reported, and
///   no module is added after it.
/// - Collects every module's declarations and numbers them: each type and directive definition a
///   placed file declares is a definition of the model, the built-in packages' included, and so is
///   every intrinsic primitive the packages write where they may have it, which `core` holds first
///   in its root module. A file holding only white space, commas, byte order marks and comments
///   declares nothing, and is no syntax error. In the root package's root module, a scalar named
///   as a GraphQL scalar (`Int`, `Float`, `String`, `Boolean`, `ID`) and a directive named as a
///   GraphQL specified directive replace the built-in of that name, which keeps its id. Two
///   definitions of one name in one namespace of one module, a supplied definition named with
///   `__`, and a type named as a child module of its module are reported. The first definition a
///   supplied package would add past [`Limits::max_definitions`] is reported, and nothing after it
///   is collected.
/// - Builds every module's scope: its own declarations and child modules, then its explicit `use`
///   bindings, then its globs, then the prelude, the root modules of `core`, `alloc` and `std`
///   whatever the package's tier, then, where a module is wanted, the packages its package may
///   reach. A type a supplied package declares with the name of a primitive of the prelude, `bool`,
///   `f32`, `f64`, `iN`, `uN`, `string`, `bytes`, `NonZeroIN` or `NonZeroUN`, shadows it in its
///   module, and is a warning.
/// - Resolves every `use` item, each module's to a fixpoint, which `use` cycles do not disturb. A
///   path that names nothing is reported once, as is a package the item's package may not reach, a
///   string source that is a URL, names a file, leaves its package or stands in a `Flat` package,
///   a name two globs give two items where an item meets it, a group nested past
///   [`Limits::max_use_depth`], and the first glob of a module that would name a module past
///   [`Limits::max_globs`]. An item that names one thing while the module's items are being
///   resolved and another once all are, a `use` of a name its module declares, and two `use` items
///   that give one name two items are reported too. A name that does not resolve gets, in its
///   help, the nearest name visible where it is written, of the namespace wanted, while the
///   analysis has [`Limits::max_near_miss_comparisons`] left to spend.
/// - Lowers what each definition holds beyond its identity. Each extension's target is resolved
///   where the extension is written: one that names nothing, one that names a definition of
///   another kind than the extension's keyword (`extension-kind-mismatch`), and one that is an
///   alias such as `NonZeroU32`, which spells an instantiation and is no definition of its own,
///   is reported and contributes nothing; every other is a contribution of its target. An
///   extension's parameter list names its target's type parameters by position, one name each,
///   and a list of another length is `generic-arity`; an extension that writes no list names none
///   of them. A definition holds its contributions,
///   its own, where its name is written, then its extensions' by source and position, each where
///   its target path is written; its type parameters, of which a definition that replaces a
///   GraphQL built-in declares none, a list it writes being `generic-arity`; a directive
///   definition's locations and
///   whether it is `repeatable`; and its members, the direct members of each contribution in turn
///   as written and then each field's arguments, each with its description. A replaced built-in
///   holds what the root package's definition writes, and smear's own definition of it
///   contributes nothing. Every type written is then a type occurrence, and every directive
///   application and every value written is one too, appended in the order the model's walk
///   reaches it, each before what it nests: a type parameter's default, a bound, an `implements`
///   entry, a union member, an application, a member's type, default and applications. Each
///   keeps its path as written and the arguments written for it, and its path is resolved where
///   its contribution is written, with the contribution's generic parameters in scope, an
///   extension's own names naming its target's by position; one that names nothing is reported,
///   with the code of its place, and stays unresolved. A bound is a definition's own type
///   parameter's: any other left side of a `where` predicate, and any `where` clause on an
///   extension, is `invalid-bound`; and so is a right side that names no interface, the bound
///   still recorded as written. An application whose directive resolves keeps its type
///   arguments and the arguments its directive declares, in the directive's order, an argument
///   written twice twice; one it does not declare is `undefined-directive-argument`, with the
///   nearest name it does declare in its help. A value is kept as written, its syntactic kind and
///   its text. Then the schema's
///   contributions: every package's schema definitions, then its schema extensions, each by source
///   and position and each marked as what it is. The root package's first schema definition gives
///   the schema its description, its roots and its applications, and each schema extension the
///   root package writes its roots and its applications; any other schema item is a contribution
///   alone. The first root written for an operation is the schema's, and a second one is
///   `duplicate-root-operation-type`; with no schema definition of the root package's, an object
///   type its root module declares named `Query`, `Mutation` or `Subscription` is presumed that
///   operation's root. Names and texts past what the model's `u32` offsets address,
///   which only a [`Limits::max_source_bytes`] far above its default admits, are reported at the
///   root package's package source.
/// - Gives each definition its built-in meaning, read off `core`'s directives by what each
///   application resolved to, never by how it is spelled, and only off the applications the
///   package that wrote the definition writes: an interface that applies `@marker` is a marker,
///   and a scalar that applies `@set` or `@map` stands for that collection, the first of the two
///   when it applies both. Then checks each definition's defaults where they are written:
///   a type parameter with no default that follows one with a default is `generic-default`, once
///   for the definition; and so is each group of its defaults that need each other, a default that
///   names its own parameter included, once, at the default of the group's lowest parameter,
///   which a use can never fill.
/// - Gives every type occurrence its type, each type interned where the model's walk first needs
///   it, its arguments before it: a list is the list of its element's type; a type parameter is
///   that parameter of its definition, in a default's own tree too; an alias such as `NonZeroU32`
///   is the `NonZero` it spells; and an occurrence of a definition is that definition with every
///   argument, the written ones first, a collection's key or element non-null however it is
///   written, then each one left out filled from its default, substituted, a default that names a
///   parameter written after it filled first. Type arguments on a type parameter or an alias, more
///   type arguments than a definition has parameters or fewer than it takes, and an argument left
///   to a default that cannot fill it, are `generic-arity`, where they are written; an occurrence
///   inside a default whose filling leads back to a default being filled is `generic-default`. A
///   type nested deeper than [`Limits::max_generic_depth`], an instantiation past
///   [`Limits::max_instantiations`] and filling past [`Limits::max_substitutions`] are
///   `resource-limit`, each where it is written, and the built-in library is typed whatever the
///   limits. An occurrence that cannot be typed, or that holds one that cannot, has the one
///   unresolved type. Each closed type an occurrence of a generic definition resolves to is one of
///   the definition's instantiations. Last, an application writes no more type arguments than its
///   directive has parameters and no fewer than it takes, or it is `generic-arity`, at its path.
/// - Judges the bounds of every written occurrence of a definition that has one, by its complete
///   type, a slot a default filled included: for each bound in its definition's order, the
///   argument for the bounded parameter, without its own `!`, must satisfy the bound's interface
///   type with the type's arguments in place of the definition's parameters, each with its `!`, as
///   [`Model::satisfies_ty`](super::Model::satisfies_ty) answers. An occurrence that names a
///   parameter of the definition it is written in is judged through that parameter's bounds, and
///   an instantiation only a fill derives is not judged. The first bound that fails is
///   `unsatisfied-bound`, at each occurrence of the type, which keeps its type; but for a bound of
///   `core`'s `MapKey` that a `NonZero` fails, which goes on to the next: that `NonZero` holds what
///   is no integer primitive, and `nonzero-argument` refuses it where it is written. A directive
///   application's type arguments are not judged against its directive's bounds. Each type is
///   judged once, whatever its nullness, within [`Limits::max_bound_steps`]; an occurrence whose
///   bounds that budget, or a type they derive past [`Limits::max_generic_depth`] or
///   [`Limits::max_instantiations`], kept from being judged is `resource-limit`, and keeps its
///   type too.
/// - Runs smear's own rules through [`visit`](super::visit)'s driver, over every supplied package
///   ([`Packages::Supplied`]), and keeps what they report in the same list, under the same ceiling.
///   Each schema definition the root package writes after its first is
///   `duplicate-schema-definition`, with a label at the first. Each schema definition and each
///   schema extension a dependency writes is `schema-outside-root`, and no duplicate of the root
///   package's. An extension of a definition another supplied package defines is
///   `orphan-extension`, at its target path, with a label at the definition's name; and so is an
///   extension of a definition of `core`, `alloc` or `std`, a GraphQL built-in the root package
///   replaces among them, unless the root package writes it and it adds directives and nothing
///   else. In the root package's extension of a built-in it did not write, each application of
///   `core`'s `@marker`, `@set` or `@map` is `orphan-extension` too, at its path: what a built-in
///   is is the language's to say. An enum that declares a type parameter is `generic-enum`, at its
///   first type parameter's name. A marker that declares a field is `marker-misuse`, once, at the
///   first field its own package wrote; so is a marker named anywhere but as the outermost type of
///   an `implements` entry or of a bound, where it is named, and an `implements` entry of a scalar,
///   an enum, an input object or a union that names what is no marker, a type parameter among
///   them. A scalar its own package makes a set with `@set` and that has not exactly one type
///   parameter, or a map with `@map`
///   and that has not exactly two, is `collection-arity`, at the application, and so is one that
///   applies both, at the later. `TypeRef` anywhere but as the type of a directive definition's
///   argument or of an input field, under lists alone, is `typeref-misuse`, where it is named, a
///   type argument of any generic definition included. `NonZero` whose type argument, in the
///   occurrence's type, is no integer primitive at least one bit wide written without `!` is
///   `nonzero-argument`, where it is written, with a label at the argument. A type parameter with
///   the name of an earlier one of its definition is `duplicate-type-parameter`, at its name, with
///   a label at the first of that name. A rule says nothing about a contribution a phase already
///   refused something in.
///
/// Every supplied package is checked, parsed and walked, whether anything reaches it or not.
///
/// # What it marks
///
/// Each error a phase or one of smear's rules reports marks, whether the ceiling keeps it or not,
/// the contribution whose extent holds where it points, every member whose extent holds it, and
/// through the contribution its definition: their `has_errors()` say so. An extent runs from the
/// description, or the first keyword when there is none, to the last token, and an error that
/// points at no definition, such as one at a package source, marks nothing. A syntax error marks
/// nothing either. What the parser could not read has no image in its file's document, and so no
/// contribution to mark; and a definition the parser read past a reported mistake stands in the
/// document unmarked: its `has_errors()` does not speak of the syntax error, which is in the
/// diagnostics. While smear's rules walk, a rule sees the phases' marks and no rule's: the rules'
/// marks are applied once the walk has ended, so no rule sees another's, and the order the rules
/// are composed in changes nothing.
///
/// # What it keeps
///
/// Nothing of the packages: it borrows them for the call, and the caller may drop or reuse every
/// text as soon as it returns. [`SourceMap::bind`] pairs the result with the same packages again
/// when a renderer needs their texts.
///
/// The analysis reads no file and no network: the caller supplies every package, the root's
/// dependencies with it.
pub fn analyze<S: SourceText>(
  root: &Package<S>,
  dependencies: &[Package<S>],
  limits: &Limits,
) -> Analysis {
  let mut prepared = prepare(views(root, dependencies), limits);
  let documents = parse::files(
    &Syntactic::new(),
    &prepared.packages,
    &prepared.sources,
    prepared.within,
    &mut prepared.reporter,
    None,
  );
  finish(prepared, &documents, limits)
}

/// Analyzes `root` beside `dependencies`, within `limits`, with `parser` reading every supplied
/// file: [`analyze`], with the parser chosen by the caller, and what the parser kept for each file.
///
/// `parser` is a [`Layer`] of GraphQLx. [`Syntactic`] is one: it stops at a file's first syntax
/// error, keeps nothing, and is what [`analyze`] reads with. The other is `smear-parser`'s lossless
/// layer, behind its `rowan_0_17` and `std` features: it recovers, so the well-formed rest of a
/// broken file is analyzed, and it keeps each file's tree. A layer's limits, such as the nesting
/// depth a parse descends to, are the layer value's, where [`Limits`] bounds the analysis.
///
/// Everything [`analyze`] does around the parse is done here, in the same order. Each supplied file
/// that is text, or bytes that are UTF-8, is read by `parser`, and every syntax error it lends is a
/// diagnostic at that file, in the analysis's one list. Three kinds of file never reach `parser`
/// and have no tree: the built-in packages' files, a byte text that is not UTF-8, and a text that
/// holds nothing but white space, commas, byte order marks and comments. When the supplied sources
/// are over [`Limits::max_source_bytes`], no supplied file is parsed at all.
///
/// The trees borrow nothing of the packages at a concrete call: they are `Trees<()>` for
/// [`Syntactic`], and a tree of the lossless layer holds no text of the caller's. A consumer generic
/// over the layer that keeps the trees past the packages bounds `P::Tree: 'static`.
pub fn analyze_with<'a, P, S>(
  parser: &P,
  root: &'a Package<S>,
  dependencies: &'a [Package<S>],
  limits: &Limits,
) -> (Analysis, Trees<P::Tree>)
where
  P: Layer<'a>,
  S: SourceText,
{
  let mut prepared = prepare(views(root, dependencies), limits);
  let mut trees: Vec<(SourceId, P::Tree)> = Vec::new();
  let documents = parse::files(
    parser,
    &prepared.packages,
    &prepared.sources,
    prepared.within,
    &mut prepared.reporter,
    Some(&mut trees),
  );
  (finish(prepared, &documents, limits), Trees { trees })
}

/// Every package of one analysis, read through its getters, in id order: the built-ins, then
/// `root`, then `dependencies` by name.
fn views<'a, S: SourceText>(
  root: &'a Package<S>,
  dependencies: &'a [Package<S>],
) -> Vec<PackageView<'a>> {
  let mut supplied: Vec<PackageView<'a>> = Vec::with_capacity(dependencies.len());
  for package in dependencies {
    supplied.push(PackageView::supplied(package));
  }
  set::order(PackageView::supplied(root), supplied)
}

/// What an analysis has settled before any file is parsed.
struct Prepared<'a> {
  packages: Vec<PackageView<'a>>,
  reporter: Reporter,
  sources: SourceMap,
  /// Each package's resolved dependencies, one list per package.
  dependencies: Vec<Vec<PackageId>>,
  /// Whether the supplied sources are within their size limit, so that their files are parsed.
  within: bool,
}

/// Everything after the entrance and before the parse, compiled once whatever the caller's text
/// type and whatever the parser: the source map, and the checks of the package set and of the
/// supplied sources' size.
fn prepare<'a>(packages: Vec<PackageView<'a>>, limits: &Limits) -> Prepared<'a> {
  let mut reporter = Reporter::new(limits.max_diagnostics() as usize);
  let sources = SourceMap::of(&packages);

  let dependencies = check::package_set(&packages, &sources, &mut reporter);
  let within = check::source_bytes(&packages, &sources, limits, &mut reporter);
  Prepared {
    packages,
    reporter,
    sources,
    dependencies,
    within,
  }
}

/// Everything after the parse, compiled once whatever the caller's text type and whatever the
/// parser: the phases that read the documents, and the rules.
fn finish<'a>(prepared: Prepared<'a>, documents: &Documents<'a>, limits: &Limits) -> Analysis {
  let Prepared {
    packages,
    mut reporter,
    sources,
    dependencies,
    within: _,
  } = prepared;
  debug_assert_eq!(
    documents.len(),
    sources.len() as usize,
    "one document entry per source"
  );

  let mut builder = builder_of(&packages, &sources, &dependencies);
  let collected = collect::items(&packages, &sources, documents, limits, &mut reporter);
  let declarations = declare::declarations(collected, &sources, &mut builder, &mut reporter);
  let scopes = resolve::scopes(
    &packages,
    &sources,
    &dependencies,
    &declarations,
    limits,
    &mut builder,
    &mut reporter,
  );
  lower::lower(
    &packages,
    &declarations,
    &scopes,
    &sources,
    &mut builder,
    &mut reporter,
  );
  instantiate::instantiate(&declarations, &sources, limits, &mut builder, &mut reporter);

  conclude(builder.finish(), sources, reporter, &mut Rules::default())
}

/// The model of `root` beside `dependencies` as name resolution leaves it, before lowering, and
/// what `path` names in the module `module` of those packages, where `params` are the generic
/// parameters in scope: the test-support door to the resolver. `None` when `module` names no module. See
/// `Scopes::module_at` and `Scopes::door` for how `module` and `path` are spelled.
///
/// Its files are read as [`analyze`] reads them: by the syntactic layer under its default limits.
#[cfg(feature = "test-support")]
pub(crate) fn resolve_door<S: SourceText>(
  root: &Package<S>,
  dependencies: &[Package<S>],
  module: &str,
  params: &[&[u8]],
  path: &str,
) -> Option<(Model, resolve::Resolved)> {
  let limits = Limits::new();
  let mut prepared = prepare(views(root, dependencies), &limits);
  let documents = parse::files(
    &Syntactic::new(),
    &prepared.packages,
    &prepared.sources,
    prepared.within,
    &mut prepared.reporter,
    None,
  );
  let Prepared {
    packages,
    mut reporter,
    sources,
    dependencies: resolved,
    within: _,
  } = prepared;
  let mut builder = builder_of(&packages, &sources, &resolved);
  let collected = collect::items(&packages, &sources, &documents, &limits, &mut reporter);
  let declarations = declare::declarations(collected, &sources, &mut builder, &mut reporter);
  let scopes = resolve::scopes(
    &packages,
    &sources,
    &resolved,
    &declarations,
    &limits,
    &mut builder,
    &mut reporter,
  );
  let at = scopes.module_at(&builder, module)?;
  let answer = scopes.door(&builder, at, params, path);
  Some((builder.finish(), answer))
}

/// What a parser layer kept beside an analysis: one entry for each supplied file the layer read, in
/// source id order.
///
/// [`analyze_with`] answers it. A file the layer refused, a file that never reached the layer, and
/// every package source have no entry.
///
/// A file that holds nothing but white space, commas, byte order marks and comments never reaches
/// the layer: the analysis accepts it as a document with no item, so it has no entry here. A tool
/// that wants a tree for such a file parses its text with `smear-parser`'s lossless layer itself,
/// and is answered a tree with one syntax error, because for the parser a text with no item is
/// not a document.
#[derive(Debug, Clone)]
pub struct Trees<T> {
  trees: Vec<(SourceId, T)>,
}

impl<T> Trees<T> {
  /// What the layer kept for `source`, or `None` when it kept nothing for it.
  pub fn get(&self, source: SourceId) -> Option<&T> {
    let at = self
      .trees
      .binary_search_by_key(&source, |entry: &(SourceId, T)| entry.0)
      .ok()?;
    Some(&self.trees[at].1)
  }

  /// Every entry with its source, in source id order.
  #[inline]
  pub fn iter(&self) -> TreesIter<'_, T> {
    TreesIter {
      inner: self.trees.iter(),
    }
  }

  /// How many sources have an entry.
  #[inline]
  pub fn len(&self) -> usize {
    self.trees.len()
  }

  /// Whether no source has an entry.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.trees.is_empty()
  }

  /// The node an extent belongs to: [`SyntaxTree::covering_node`], in the tree of the extent's
  /// source.
  ///
  /// `None` for a location with no span, for a source with no tree, and for a span that does not
  /// lie inside its tree's text. An extent the analysis answers always has a span.
  pub fn node(&self, at: Location) -> Option<T::Node>
  where
    T: SyntaxTree,
  {
    let span = at.span()?;
    self.get(SourceId::new(at.source()))?.covering_node(span)
  }
}

impl<T> IntoIterator for Trees<T> {
  type Item = (SourceId, T);
  type IntoIter = TreesIntoIter<T>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    TreesIntoIter {
      inner: self.trees.into_iter(),
    }
  }
}

impl<'a, T> IntoIterator for &'a Trees<T> {
  type Item = (SourceId, &'a T);
  type IntoIter = TreesIter<'a, T>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

/// [`Trees`]' entries by reference, in source id order: what [`Trees::iter`] answers.
#[derive(Debug, Clone)]
pub struct TreesIter<'a, T> {
  inner: core::slice::Iter<'a, (SourceId, T)>,
}

impl<'a, T> Iterator for TreesIter<'a, T> {
  type Item = (SourceId, &'a T);

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self
      .inner
      .next()
      .map(|entry: &'a (SourceId, T)| (entry.0, &entry.1))
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.inner.size_hint()
  }
}

impl<T> ExactSizeIterator for TreesIter<'_, T> {}

impl<T> FusedIterator for TreesIter<'_, T> {}

/// [`Trees`]' entries by value, in source id order: what [`Trees`]' `into_iter` answers.
#[derive(Debug, Clone)]
pub struct TreesIntoIter<T> {
  inner: std::vec::IntoIter<(SourceId, T)>,
}

impl<T> Iterator for TreesIntoIter<T> {
  type Item = (SourceId, T);

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self.inner.next()
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.inner.size_hint()
  }
}

impl<T> ExactSizeIterator for TreesIntoIter<T> {}

impl<T> FusedIterator for TreesIntoIter<T> {}

/// What every analysis ends with, once its phases have written `model` and reported into
/// `reporter`: the phases' marks applied, `rules` walked over every supplied package and reporting
/// into the same list, and the marks of the rules' errors applied once the walk has ended.
fn conclude<V: Visitor + ?Sized>(
  mut model: Model,
  sources: SourceMap,
  mut reporter: Reporter,
  rules: &mut V,
) -> Analysis {
  let ceiling = reporter.ceiling();
  // The rules read the phases' marks.
  model.mark(&reporter.take_marks());
  // Theirs are kept aside until every rule has walked, so no rule sees another's.
  let reporter = drive(&model, &sources, Packages::Supplied, reporter, rules);
  let (diagnostics, marks) = reporter.finish();
  model.mark(&marks);
  Analysis {
    model,
    diagnostics,
    sources,
    ceiling,
  }
}

/// A model builder holding every package of `packages`, which are in id order and numbered by
/// `sources`, with its resolved `dependencies`, one list per package, and its integer provider.
fn builder_of(
  packages: &[PackageView<'_>],
  sources: &SourceMap,
  dependencies: &[Vec<PackageId>],
) -> ModelBuilder {
  let mut builder = ModelBuilder::default();
  for ((index, view), resolved) in packages.iter().enumerate().zip(dependencies) {
    let kind = match index.cmp(&ROOT) {
      Ordering::Less => PackageKind::BuiltIn,
      Ordering::Equal => PackageKind::Root,
      Ordering::Greater => PackageKind::Dependency,
    };
    let name: String = String::from_utf8_lossy(view.name).into_owned();
    let package = builder.push_package(
      &name,
      kind,
      view.tier,
      view.layout,
      sources.package_sources(index).len(),
    );
    for &dependency in resolved {
      builder.push_dependency(package, dependency);
    }
    if let Some(integers) = view.integers {
      builder.set_integers(package, integers.clone());
    }
  }
  builder
}
