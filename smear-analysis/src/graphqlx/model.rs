//! The model: what the packages declare, resolved, and read through views.

use core::cmp::Ordering;
use std::{boxed::Box, string::String, vec::Vec};

use super::{
  id::{IdRange, PackageId, SourceId, dense},
  package::{IntegerProvider, Layout, Tier},
  set::{PackageView, ROOT},
  source::SourceMap,
};

/// What the analyzed packages declare, resolved: the descriptor every consumer reads.
///
/// The model owns every spelling it answers with, so a consumer that reads only the model never
/// holds the sources. It has no type parameter and no lifetime: it is the same type whatever text
/// type the packages were supplied in. It is read through views, such as [`PackageInfo`], each a
/// `Copy` pair of the model and an id.
///
/// The model is always complete: what could not be resolved is still in it, and the diagnostics
/// say why.
#[derive(Debug, Clone)]
pub struct Model {
  packages: Vec<PackageRow>,
  /// Every package's resolved dependencies, one run per package.
  dependencies: Vec<PackageId>,
}

/// One package, as the model records it.
#[derive(Debug, Clone)]
struct PackageRow {
  name: Box<str>,
  kind: PackageKind,
  tier: Tier,
  layout: Layout,
  sources: IdRange<SourceId>,
  /// The package's run in the model's dependency table.
  dependencies: (u32, u32),
  integers: Option<IntegerProvider>,
}

/// Which kind of package a package is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum PackageKind {
  /// `core`, `alloc` or `std`: smear's own, embedded in smear and supplied by no caller.
  BuiltIn,
  /// The package the analysis was asked about.
  Root,
  /// A package supplied beside the root.
  Dependency,
}

impl Model {
  /// The model of `packages`, which are in id order and numbered by `sources`, with `dependencies`
  /// resolved, one list per package.
  pub(crate) fn of(
    packages: &[PackageView<'_>],
    sources: &SourceMap,
    dependencies: Vec<Vec<PackageId>>,
  ) -> Self {
    let mut rows: Vec<PackageRow> = Vec::with_capacity(packages.len());
    let mut table: Vec<PackageId> = Vec::new();
    for ((index, view), resolved) in packages.iter().enumerate().zip(dependencies) {
      let start = dense(table.len());
      table.extend(resolved);
      let kind = match index.cmp(&ROOT) {
        Ordering::Less => PackageKind::BuiltIn,
        Ordering::Equal => PackageKind::Root,
        Ordering::Greater => PackageKind::Dependency,
      };
      rows.push(PackageRow {
        name: Box::from(String::from_utf8_lossy(view.name)),
        kind,
        tier: view.tier,
        layout: view.layout,
        sources: sources.package_sources(index),
        dependencies: (start, dense(table.len())),
        integers: view.integers.cloned(),
      });
    }
    Self {
      packages: rows,
      dependencies: table,
    }
  }

  /// Every package id: `core`, `alloc` and `std`, then the root, then the supplied dependencies by
  /// name.
  #[inline]
  pub fn packages(&self) -> IdRange<PackageId> {
    IdRange::new(0, dense(self.packages.len()))
  }

  /// The package `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of [`packages`](Self::packages): an id belongs to the analysis that
  /// issued it.
  #[inline]
  pub fn package(&self, id: PackageId) -> PackageInfo<'_> {
    assert!(
      (id.get() as usize) < self.packages.len(),
      "package id {} is past the {} packages of this model",
      id.get(),
      self.packages.len()
    );
    PackageInfo { model: self, id }
  }

  /// The root package: the one the analysis was asked about, numbered after the built-ins.
  #[inline]
  pub fn root_package(&self) -> PackageId {
    PackageId::new(dense(ROOT))
  }
}

/// One package of the model.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct PackageInfo<'a> {
  model: &'a Model,
  id: PackageId,
}

impl<'a> PackageInfo<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> PackageId {
    self.id
  }

  /// Its name. A supplied name that is not UTF-8 has each such sequence replaced by U+FFFD, and is
  /// refused.
  #[inline]
  pub fn name(&self) -> &'a str {
    &self.row().name
  }

  /// Which kind of package it is.
  #[inline]
  pub fn kind(&self) -> PackageKind {
    self.row().kind
  }

  /// Its tier: what its definitions may use.
  #[inline]
  pub fn tier(&self) -> Tier {
    self.row().tier
  }

  /// How its files become modules.
  #[inline]
  pub fn layout(&self) -> Layout {
    self.row().layout
  }

  /// Its sources: its package source, then its files by path.
  #[inline]
  pub fn sources(&self) -> IdRange<SourceId> {
    self.row().sources
  }

  /// The packages it declares and that were found, in id order. A built-in package declares none;
  /// every package reaches `core`, `alloc` and `std` without declaring them.
  #[inline]
  pub fn dependencies(&self) -> &'a [PackageId] {
    let model: &'a Model = self.model;
    let (start, end) = self.row().dependencies;
    &model.dependencies[start as usize..end as usize]
  }

  /// The integer provider it selected, if any: where the host type of each integer width it writes
  /// beyond `core`'s comes from.
  #[inline]
  pub fn integers(&self) -> Option<&'a IntegerProvider> {
    self.row().integers.as_ref()
  }

  /// The model's record of this package.
  fn row(&self) -> &'a PackageRow {
    let model: &'a Model = self.model;
    &model.packages[self.id.get() as usize]
  }
}

impl core::fmt::Debug for PackageInfo<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("PackageInfo")
      .field("id", &self.id)
      .field("name", &self.name())
      .field("kind", &self.kind())
      .field("tier", &self.tier())
      .field("layout", &self.layout())
      .field("sources", &self.sources())
      .field("dependencies", &self.dependencies())
      .finish()
  }
}
