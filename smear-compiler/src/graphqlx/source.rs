//! The source map: which package and path each source id names, and how long its text was. The
//! texts themselves stay with the caller, and [`SourceMap::bind`] pairs the two again.

use core::cmp::Ordering;
use std::{boxed::Box, string::String, vec, vec::Vec};

use tokora::diagnostic::Location;

use super::{
  id::{IdRange, PackageId, SourceId, dense},
  package::Package,
  set::{self, PackageView, ROOT},
  text::SourceText,
};

/// The sources of one analysis: one package source per package, and every file, the built-in
/// packages' included.
///
/// Sources are numbered in package order. Within a package its package source comes first, then
/// its files by path, compared segment by segment, each segment bytewise. A package source has no
/// text and no path: it is where a diagnostic about the package as a whole points, with
/// [`Location::entire`](tokora::diagnostic::Location::entire).
///
/// The map keeps each source's package, path and length, and never its text, which stays with the
/// caller. [`SourceMap::bind`] pairs the map with the packages it was made from again, and that is
/// what a renderer reads texts through.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMap {
  packages: Vec<PackageSlot>,
  sources: Vec<SourceSlot>,
}

/// One package, as the map records it.
#[derive(Debug, Clone, PartialEq, Eq)]
struct PackageSlot {
  /// The name, as given, which `bind` finds a dependency by and holds the root to.
  name: Box<[u8]>,
  /// Whether another dependency has this name, which leaves `bind` no way to tell the two apart.
  /// Never for the root or a built-in: a dependency named as the root is still the only dependency
  /// with that name.
  shared: bool,
  /// The package source and the files.
  sources: IdRange<SourceId>,
}

/// One source, as the map records it.
#[derive(Debug, Clone, PartialEq, Eq)]
struct SourceSlot {
  /// The package it belongs to.
  package: PackageId,
  /// The path as text, with each sequence that is not UTF-8 replaced by U+FFFD; `""` for a package
  /// source.
  path: Box<str>,
  /// The path's own bytes, kept only when they are not UTF-8 and so differ from `path`.
  raw: Option<Box<[u8]>>,
  /// The text's length in bytes; 0 for a package source.
  len: usize,
  /// Whether another file of its package has its path, which leaves `bind` no way to tell the two
  /// apart. Never for a package source.
  shared: bool,
  /// Whether this is its package's package source.
  package_source: bool,
}

impl SourceSlot {
  /// The path's bytes, as given.
  fn key(&self) -> &[u8] {
    self.raw.as_deref().unwrap_or(self.path.as_bytes())
  }
}

/// A path as the map keeps it: its text, and its bytes when they are not that text.
fn spell(path: &[u8]) -> (Box<str>, Option<Box<[u8]>>) {
  match core::str::from_utf8(path) {
    Ok(text) => (Box::from(text), None),
    Err(_) => (
      Box::from(String::from_utf8_lossy(path)),
      Some(Box::from(path)),
    ),
  }
}

impl SourceMap {
  /// Numbers the sources of `packages`, which are already in id order.
  pub(crate) fn of(packages: &[PackageView<'_>]) -> Self {
    let mut slots: Vec<PackageSlot> = Vec::with_capacity(packages.len());
    let mut sources: Vec<SourceSlot> = Vec::new();
    for (index, view) in packages.iter().enumerate() {
      let package = PackageId::new(dense(index));
      let start = dense(sources.len());
      sources.push(SourceSlot {
        package,
        path: Box::from(""),
        raw: None,
        len: 0,
        shared: false,
        package_source: true,
      });
      for file in &view.files {
        let (path, raw) = spell(file.path);
        sources.push(SourceSlot {
          package,
          path,
          raw,
          len: file.text.bytes().len(),
          shared: false,
          package_source: false,
        });
      }
      slots.push(PackageSlot {
        name: Box::from(view.name),
        shared: false,
        sources: IdRange::new(start, dense(sources.len())),
      });
    }
    let mut map = Self {
      packages: slots,
      sources,
    };
    map.share();
    map
  }

  /// The source map of a model the test-support door built, which no text was read for: each
  /// package's sources as the model numbers them, its package source first and every other one a
  /// file with no path and no length.
  ///
  /// What is shared is decided as for any map. Every file has the empty path, so every file of a
  /// package with two or more is shared, and `bind` answers none of them a text. A package's only
  /// file is answered a text only when the package given for it holds one file at the empty path,
  /// and that file is empty.
  #[cfg(feature = "test-support")]
  pub(crate) fn of_model(model: &super::model::Model) -> Self {
    let mut packages: Vec<PackageSlot> = Vec::with_capacity(model.packages().len() as usize);
    let mut sources: Vec<SourceSlot> = Vec::new();
    for id in model.packages() {
      let package = model.package(id);
      let range = package.sources();
      for source in range {
        sources.push(SourceSlot {
          package: id,
          path: Box::from(""),
          raw: None,
          len: 0,
          shared: false,
          package_source: source.get() == range.start(),
        });
      }
      packages.push(PackageSlot {
        name: Box::from(package.name().as_bytes()),
        shared: false,
        sources: range,
      });
    }
    let mut map = Self { packages, sources };
    map.share();
    map
  }

  /// Marks what `bind` cannot tell apart: both of every two dependencies with one name, and both
  /// of every two files of one package with one path.
  ///
  /// The dependencies follow the root sorted by name, and each package's files follow its package
  /// source sorted by path, so each such pair is a pair of neighbours. The root is compared with no
  /// dependency.
  fn share(&mut self) {
    for index in ROOT + 2..self.packages.len() {
      if self.packages[index - 1].name == self.packages[index].name {
        self.packages[index - 1].shared = true;
        self.packages[index].shared = true;
      }
    }
    for index in 1..self.sources.len() {
      let (before, here) = (&self.sources[index - 1], &self.sources[index]);
      if !before.package_source
        && !here.package_source
        && before.package == here.package
        && before.key() == here.key()
      {
        self.sources[index - 1].shared = true;
        self.sources[index].shared = true;
      }
    }
  }

  /// The sources of the package at `index`: its package source, then its files.
  pub(crate) fn package_sources(&self, index: usize) -> IdRange<SourceId> {
    self.packages[index].sources
  }

  /// How many sources the map holds. Every id below it is one of them; every id at or past it is
  /// the consumer's.
  #[inline]
  pub fn len(&self) -> u32 {
    dense(self.sources.len())
  }

  /// Whether the map holds no source. It always holds the built-in packages' package sources, so
  /// a map an analysis made never is.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.sources.is_empty()
  }

  /// Every source id, in order.
  #[inline]
  pub fn sources(&self) -> IdRange<SourceId> {
    IdRange::new(0, self.len())
  }

  /// The package that wrote what is written at `at`: the package of the source `at` is in.
  ///
  /// It is the one answer the analysis gives to who wrote something, and the question
  /// [`Packages`](crate::graphqlx::Packages) and
  /// [`Cx::contribution`](crate::graphqlx::Cx::contribution) are stated in. For a definition's own
  /// contribution, which is written where its declaration is, it is the package that wrote the
  /// definition: the root package for a GraphQL built-in it replaces, though the definition stays
  /// the built-in's, and `core` for a primitive `core` materialises, whose contribution is the
  /// whole of `core`'s package source. For an extension or a directive application it is the
  /// package that wrote that, which need not be its definition's.
  ///
  /// It never panics. `None` when `at`'s source is none of this map's: a source id at or past
  /// [`len`](Self::len), which is a consumer's own, or a location of a model built by hand.
  #[inline]
  pub fn written_by(&self, at: Location) -> Option<PackageId> {
    self
      .sources
      .get(at.source() as usize)
      .map(|slot: &SourceSlot| slot.package)
  }

  /// The source `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is at or past [`len`](Self::len): such an id is not one of this map's sources.
  #[inline]
  pub fn source(&self, id: SourceId) -> Source<'_> {
    assert!(
      (id.get() as usize) < self.sources.len(),
      "source id {} is past the {} sources of this map",
      id.get(),
      self.sources.len()
    );
    Source { map: self, id }
  }

  /// The texts of every source, read from the packages the analysis was made from.
  ///
  /// Pass the same `root` and `dependencies` the analysis was given, in any order. Each source is
  /// found again by structure alone: by its package, a built-in by its place, the root by its
  /// place and its name, and a dependency by its name; then by its own path. A file found is
  /// answered its text when that text is as long as the one the analysis read and is UTF-8, so a
  /// file changed to a text of the same length is answered the new text. See [`Bound`] for what is
  /// answered when a source is not found.
  pub fn bind<'a, S: SourceText>(
    &'a self,
    root: &'a Package<S>,
    dependencies: &'a [Package<S>],
  ) -> Bound<'a> {
    let mut supplied: Vec<PackageView<'a>> = Vec::with_capacity(dependencies.len());
    for package in dependencies {
      supplied.push(PackageView::supplied(package));
    }
    let packages: Vec<PackageView<'a>> = set::order(PackageView::supplied(root), supplied);
    let given = SourceMap::of(&packages);

    let mut texts: Vec<Option<&'a str>> = vec![None; self.sources.len()];
    for (index, package) in self.packages.iter().enumerate() {
      let found = given.counterpart(index, package);
      for id in package.sources {
        let slot = &self.sources[id.get() as usize];
        texts[id.get() as usize] = if slot.package_source {
          Some("")
        } else if slot.shared {
          None
        } else {
          found.and_then(|at: usize| given.file_text(at, slot, &packages))
        };
      }
    }
    Bound { map: self, texts }
  }

  /// The index in this map, made from the packages given to `bind`, of the package the analysis's
  /// map records as `recorded` at `index`: a built-in at its place; the root at its place, when it
  /// has the recorded name; a dependency by its name, when neither map has two with that name.
  fn counterpart(&self, index: usize, recorded: &PackageSlot) -> Option<usize> {
    match index.cmp(&ROOT) {
      Ordering::Less => Some(index),
      Ordering::Equal => {
        let root = self.packages.get(ROOT)?;
        (root.name == recorded.name).then_some(ROOT)
      }
      Ordering::Greater if recorded.shared => None,
      Ordering::Greater => self.dependency(&recorded.name),
    }
  }

  /// The index of the one dependency named `name`: `None` when no dependency has that name, or two
  /// do.
  fn dependency(&self, name: &[u8]) -> Option<usize> {
    let dependencies: &[PackageSlot] = self.packages.get(ROOT + 1..)?;
    let at = dependencies.partition_point(|slot: &PackageSlot| *slot.name < *name);
    let slot = dependencies.get(at)?;
    (*slot.name == *name && !slot.shared).then_some(ROOT + 1 + at)
  }

  /// The text of the one file of the package at `index` with `wanted`'s path, if it is as long as
  /// `wanted` recorded and is UTF-8: `None` when no file of the package has that path, or two do.
  /// `packages` are the views this map was made from, whose files are in the order it numbered
  /// them.
  fn file_text<'a>(
    &self,
    index: usize,
    wanted: &SourceSlot,
    packages: &[PackageView<'a>],
  ) -> Option<&'a str> {
    let slot = self.packages.get(index)?;
    let files: &[SourceSlot] = self
      .sources
      .get(slot.sources.start() as usize + 1..slot.sources.end() as usize)?;
    let key = wanted.key();
    let at = files
      .partition_point(|file: &SourceSlot| set::compare_paths(file.key(), key) == Ordering::Less);
    let found = files.get(at)?;
    if found.shared || found.key() != key || found.len != wanted.len {
      return None;
    }
    packages.get(index)?.files.get(at)?.text.text()
  }
}

/// One source of an analysis.
///
/// `Copy`: it holds the map and an id, and reads the map's tables on each call.
#[derive(Clone, Copy)]
pub struct Source<'a> {
  map: &'a SourceMap,
  id: SourceId,
}

impl<'a> Source<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> SourceId {
    self.id
  }

  /// The package it belongs to.
  #[inline]
  pub fn package(&self) -> PackageId {
    self.slot().package
  }

  /// Its path, relative to its package: `""` for a package source. A path that is not UTF-8 has
  /// each such sequence replaced by U+FFFD.
  #[inline]
  pub fn path(&self) -> &'a str {
    &self.slot().path
  }

  /// The length in bytes of the text the analysis read: 0 for a package source.
  #[inline]
  pub fn len(&self) -> usize {
    self.slot().len
  }

  /// Whether the text the analysis read was empty, as a package source's always is.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.slot().len == 0
  }

  /// Whether this is its package's package source: the source with no text and no path that a
  /// diagnostic about the package as a whole points at.
  #[inline]
  pub fn is_package_source(&self) -> bool {
    self.slot().package_source
  }

  /// The map's record of this source.
  fn slot(&self) -> &'a SourceSlot {
    let map: &'a SourceMap = self.map;
    &map.sources[self.id.get() as usize]
  }
}

impl core::fmt::Debug for Source<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Source")
      .field("id", &self.id)
      .field("package", &self.package())
      .field("path", &self.path())
      .field("len", &self.len())
      .field("package_source", &self.is_package_source())
      .finish()
  }
}

/// A source map paired with the packages its analysis was made from: the texts a renderer reads.
///
/// [`text`](Self::text) answers:
///
/// - smear's own text, for a file of a built-in package;
/// - `""`, for a package source;
/// - a supplied file's text, when the packages given to [`SourceMap::bind`] hold the file, found by
///   its package and its path, and its text is as long as the one the analysis read and is UTF-8.
///
/// And `None`:
///
/// - for a supplied file that is missing, of another length, or not UTF-8. A byte text that is not
///   UTF-8 always carries a syntax diagnostic of its own, and is rendered without a snippet;
/// - for a file whose path another file of its package has, and for every file of a dependency
///   whose name another dependency has, in the analysis or in the packages given. Two such files,
///   or two such dependencies, are refused inputs: nothing but their contents tells them apart,
///   and contents are exactly what `bind` cannot trust;
/// - for every file of the root, when the root given has another name than the analysis's;
/// - for an id at or past the map's [`len`](SourceMap::len), which is the consumer's.
///
/// A renderer's input list is these texts in id order, followed by the consumer's own.
#[derive(Clone)]
pub struct Bound<'a> {
  map: &'a SourceMap,
  texts: Vec<Option<&'a str>>,
}

impl<'a> Bound<'a> {
  /// The text of source `id`, when it can be read.
  #[inline]
  pub fn text(&self, id: SourceId) -> Option<&'a str> {
    self.texts.get(id.get() as usize).copied().flatten()
  }

  /// The path of source `id`, `""` for a package source, or `None` for an id at or past the map's
  /// [`len`](SourceMap::len).
  #[inline]
  pub fn path(&self, id: SourceId) -> Option<&'a str> {
    let map: &'a SourceMap = self.map;
    let slot = map.sources.get(id.get() as usize)?;
    Some(&slot.path)
  }
}

impl core::fmt::Debug for Bound<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let bound = self.texts.iter().flatten().count();
    f.debug_struct("Bound")
      .field("sources", &self.texts.len())
      .field("bound", &bound)
      .finish()
  }
}
