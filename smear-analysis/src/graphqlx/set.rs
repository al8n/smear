//! The package set as the analysis reads it: every package viewed with the caller's text type gone,
//! in the order the ids number them.
//!
//! The analysis and [`SourceMap::bind`](super::SourceMap::bind) both read packages through this
//! module, so the two number the same packages the same way.

use core::cmp::Ordering;
use std::vec::Vec;

use super::{
  builtin::{BUILT_INS, BuiltIn},
  package::{IntegerProvider, Layout, Package, Tier},
  text::{SourceText, View},
};

/// The index of the root package: `core`, `alloc` and `std` come before it.
pub(crate) const ROOT: usize = 3;

/// One package, as the analysis reads it.
pub(crate) struct PackageView<'a> {
  /// The name, as given.
  pub(crate) name: &'a [u8],
  /// How the files become modules.
  pub(crate) layout: Layout,
  /// The tier.
  pub(crate) tier: Tier,
  /// The files in source order: by path, segment by segment, then by text.
  pub(crate) files: Vec<FileView<'a>>,
  /// The declared dependencies' names, sorted bytewise, each once.
  pub(crate) dependencies: Vec<&'a [u8]>,
  /// The integer provider, if one was selected.
  pub(crate) integers: Option<&'a IntegerProvider>,
}

/// One file, as the analysis reads it.
#[derive(Clone, Copy)]
pub(crate) struct FileView<'a> {
  /// The path, as given.
  pub(crate) path: &'a [u8],
  /// The text.
  pub(crate) text: View<'a>,
}

impl<'a> PackageView<'a> {
  /// A supplied package, read through its getters.
  ///
  /// This and the two entrances that call it are the only code generic over the caller's text
  /// type: everything after it reads `View`s.
  pub(crate) fn supplied<S: SourceText>(package: &'a Package<S>) -> Self {
    let mut files: Vec<FileView<'a>> = Vec::with_capacity(package.files().len());
    for file in package.files() {
      files.push(FileView {
        path: file.path(),
        text: file.text().view(),
      });
    }
    files.sort_by(compare_files);

    let mut dependencies: Vec<&'a [u8]> = Vec::with_capacity(package.dependencies().len());
    for name in package.dependencies() {
      dependencies.push(name);
    }
    dependencies.sort_unstable();
    dependencies.dedup();

    Self {
      name: package.name(),
      layout: package.layout(),
      tier: package.tier(),
      files,
      dependencies,
      integers: package.integers(),
    }
  }
}

/// A built-in package, with its embedded files.
fn built_in(package: &BuiltIn) -> PackageView<'static> {
  let mut files: Vec<FileView<'static>> = Vec::with_capacity(package.files.len());
  for &(path, text) in package.files {
    files.push(FileView {
      path: path.as_bytes(),
      text: View::Text(text),
    });
  }
  files.sort_by(compare_files);

  PackageView {
    name: package.name.as_bytes(),
    layout: Layout::Directory,
    tier: package.tier,
    files,
    dependencies: Vec::new(),
    integers: None,
  }
}

/// Every package of one analysis, in id order: the built-ins, then `root`, then `dependencies`
/// sorted by name and, among packages with one name, by everything else they hold.
///
/// The tiebreak is what makes the order a function of the packages alone. Two supplied packages
/// with one name are refused, but they are still numbered, and still numbered the same way
/// whatever order they were passed in; two that tie on everything are indistinguishable.
pub(crate) fn order<'a>(
  root: PackageView<'a>,
  mut dependencies: Vec<PackageView<'a>>,
) -> Vec<PackageView<'a>> {
  dependencies.sort_by(compare_packages);
  let mut views: Vec<PackageView<'a>> = Vec::with_capacity(ROOT + 1 + dependencies.len());
  for package in BUILT_INS.iter() {
    views.push(built_in(package));
  }
  views.push(root);
  views.extend(dependencies);
  views
}

/// Two paths, segment by segment on `/`, each segment bytewise.
///
/// So `a/b` comes before `a.b`, which a bytewise comparison of the whole paths would reverse: a
/// directory's files stay together.
pub(crate) fn compare_paths(a: &[u8], b: &[u8]) -> Ordering {
  let mut left = a.split(is_separator);
  let mut right = b.split(is_separator);
  loop {
    match (left.next(), right.next()) {
      (Some(x), Some(y)) => match x.cmp(y) {
        Ordering::Equal => {}
        unequal => return unequal,
      },
      (None, Some(_)) => return Ordering::Less,
      (Some(_), None) => return Ordering::Greater,
      (None, None) => return Ordering::Equal,
    }
  }
}

/// Whether `byte` separates two path segments.
fn is_separator(byte: &u8) -> bool {
  *byte == b'/'
}

/// Two files of one package: by path, then by text, so that two files with one path still have
/// one order.
pub(crate) fn compare_files(a: &FileView<'_>, b: &FileView<'_>) -> Ordering {
  compare_paths(a.path, b.path).then_with(|| a.text.bytes().cmp(b.text.bytes()))
}

/// Two supplied packages: by name, then by everything else either holds.
fn compare_packages(a: &PackageView<'_>, b: &PackageView<'_>) -> Ordering {
  a.name
    .cmp(b.name)
    .then_with(|| layout_rank(a.layout).cmp(&layout_rank(b.layout)))
    .then_with(|| a.tier.cmp(&b.tier))
    .then_with(|| compare_integers(a.integers, b.integers))
    .then_with(|| a.dependencies.cmp(&b.dependencies))
    .then_with(|| compare_file_lists(&a.files, &b.files))
}

/// A layout's place in the tiebreak.
fn layout_rank(layout: Layout) -> u8 {
  match layout {
    Layout::Directory => 0,
    Layout::Flat => 1,
  }
}

/// Two integer providers, no provider first.
fn compare_integers(a: Option<&IntegerProvider>, b: Option<&IntegerProvider>) -> Ordering {
  match (a, b) {
    (None, None) => Ordering::Equal,
    (None, Some(_)) => Ordering::Less,
    (Some(_), None) => Ordering::Greater,
    (Some(a), Some(b)) => a.key().cmp(&b.key()),
  }
}

/// Two packages' files, file by file, the shorter list first when one is a prefix of the other.
fn compare_file_lists(a: &[FileView<'_>], b: &[FileView<'_>]) -> Ordering {
  for (left, right) in a.iter().zip(b.iter()) {
    let order = compare_files(left, right);
    if order != Ordering::Equal {
      return order;
    }
  }
  a.len().cmp(&b.len())
}
