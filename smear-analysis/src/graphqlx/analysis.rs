//! The entrance, [`analyze`], and what it answers, [`Analysis`].

use core::cmp::Ordering;
use std::{string::String, vec::Vec};

use super::{
  check, collect,
  diagnostic::{Diagnostics, Reporter},
  id::PackageId,
  limits::Limits,
  lower,
  model::{Model, ModelBuilder, PackageKind},
  package::Package,
  parse,
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
///   the package already has are reported.
/// - Runs smear's own rules through [`visit`](super::visit)'s driver, over every supplied package
///   ([`Packages::Supplied`]), and keeps what they report in the same list, under the same ceiling.
///
/// Every supplied package is checked, parsed and walked, whether anything reaches it or not.
///
/// # What it marks
///
/// Each error a phase or one of smear's rules reports marks, whether the ceiling keeps it or not,
/// the contribution whose extent holds where it points, every member whose extent holds it, and
/// through the contribution its definition: their `has_errors()` say so. An extent runs from the
/// description, or the first keyword when there is none, to the last token, and an error that
/// points at no definition, such as one at a package source, marks nothing. While smear's rules
/// walk, a rule sees the phases' marks and no rule's: the rules' marks are applied once the walk has
/// ended, so no rule sees another's, and the order the rules are composed in changes nothing.
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
  let mut supplied: Vec<PackageView<'_>> = Vec::with_capacity(dependencies.len());
  for package in dependencies {
    supplied.push(PackageView::supplied(package));
  }
  run(set::order(PackageView::supplied(root), supplied), limits)
}

/// Everything after the entrance, compiled once whatever the caller's text type.
fn run(packages: Vec<PackageView<'_>>, limits: &Limits) -> Analysis {
  let mut reporter = Reporter::new(limits.max_diagnostics() as usize);
  let sources = SourceMap::of(&packages);

  let dependencies = check::package_set(&packages, &sources, &mut reporter);
  let within = check::source_bytes(&packages, &sources, limits, &mut reporter);
  let documents = parse::files(&packages, &sources, within, &mut reporter);
  debug_assert_eq!(
    documents.len(),
    sources.len() as usize,
    "one document entry per source"
  );

  let mut builder = builder_of(&packages, &sources, dependencies);
  let tree = collect::modules(&packages, &sources, &documents, limits, &mut reporter);
  tree.push(&mut builder);
  lower::lower(&documents);

  conclude(builder.finish(), sources, reporter, &mut Rules::default())
}

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
  dependencies: Vec<Vec<PackageId>>,
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
    for dependency in resolved {
      builder.push_dependency(package, dependency);
    }
    if let Some(integers) = view.integers {
      builder.set_integers(package, integers.clone());
    }
  }
  builder
}
