//! The entrance, [`analyze`], and what it answers, [`Analysis`].

use core::cmp::Ordering;
use std::{string::String, vec::Vec};

use super::{
  check,
  diagnostic::{Collector, Diagnostics},
  id::PackageId,
  limits::Limits,
  lower,
  model::{Model, ModelBuilder, ModuleKind, PackageKind},
  package::Package,
  parse,
  set::{self, PackageView, ROOT},
  source::SourceMap,
  text::SourceText,
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
  /// An analysis of the finished `model`, as the test-support door hands it to its tests: no
  /// diagnostic, a source map of the model's packages' sources, and `limits`' diagnostic ceiling.
  #[cfg(feature = "test-support")]
  pub(crate) fn of_model(model: Model, limits: &Limits) -> Self {
    let ceiling = limits.max_diagnostics() as usize;
    let sources = SourceMap::of_model(&model);
    Self {
      model,
      diagnostics: Collector::new(ceiling).finish(),
      sources,
      ceiling,
    }
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
///
/// Every supplied package is checked and parsed, whether anything reaches it or not.
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
  let ceiling = limits.max_diagnostics() as usize;
  let mut collector = Collector::new(ceiling);
  let sources = SourceMap::of(&packages);

  let dependencies = check::package_set(&packages, &sources, &mut collector);
  let within = check::source_bytes(&packages, &sources, limits, &mut collector);
  let model = model_of(&packages, &sources, dependencies);

  let documents = parse::files(&packages, &sources, within, &mut collector);
  debug_assert_eq!(
    documents.len(),
    sources.len() as usize,
    "one document entry per source"
  );
  lower::lower(&documents);

  Analysis {
    model,
    diagnostics: collector.finish(),
    sources,
    ceiling,
  }
}

/// The model of `packages`, which are in id order and numbered by `sources`, with `dependencies`
/// resolved, one list per package: every package, and each package's root module.
fn model_of(
  packages: &[PackageView<'_>],
  sources: &SourceMap,
  dependencies: Vec<Vec<PackageId>>,
) -> Model {
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
    builder.push_module(package, None, None, ModuleKind::Root);
  }
  builder.finish()
}
