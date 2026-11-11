//! The source map: which package and path each source id names, and how long its text was. The
//! texts themselves stay with the caller, and [`SourceMap::bind`] pairs the two again.

use core::cmp::Ordering;
use std::{boxed::Box, string::String, vec, vec::Vec};

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
#[derive(Debug, Clone)]
pub struct SourceMap {
  packages: Vec<PackageSlot>,
  sources: Vec<SourceSlot>,
}

/// One package, as the map records it.
#[derive(Debug, Clone)]
struct PackageSlot {
  /// The name, as given, which `bind` finds a dependency by.
  name: Box<[u8]>,
  /// For a dependency, how many dependencies before it in id order share its name; otherwise 0.
  /// Two supplied packages with one name are refused, and `bind` still tells them apart.
  ordinal: u32,
  /// The package source and the files.
  sources: IdRange<SourceId>,
}

/// One source, as the map records it.
#[derive(Debug, Clone)]
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
  /// How many files before this one in its package have its path; 0 for a package source. Two
  /// files with one path are refused, and `bind` still tells them apart.
  ordinal: u32,
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
        ordinal: 0,
        package_source: true,
      });

      // A package's files are sorted by path, so files sharing a path sit side by side.
      let mut previous: Option<&[u8]> = None;
      let mut file_ordinal: u32 = 0;
      for file in &view.files {
        file_ordinal = match previous {
          Some(path) if path == file.path => file_ordinal.saturating_add(1),
          _ => 0,
        };
        previous = Some(file.path);
        let (path, raw) = spell(file.path);
        sources.push(SourceSlot {
          package,
          path,
          raw,
          len: file.text.bytes().len(),
          ordinal: file_ordinal,
          package_source: false,
        });
      }

      // Dependencies follow the root sorted by name, so those sharing a name sit side by side.
      let package_ordinal = match slots.last() {
        Some(last) if index > ROOT + 1 && *last.name == *view.name => {
          last.ordinal.saturating_add(1)
        }
        _ => 0,
      };
      slots.push(PackageSlot {
        name: Box::from(view.name),
        ordinal: package_ordinal,
        sources: IdRange::new(start, dense(sources.len())),
      });
    }
    Self {
      packages: slots,
      sources,
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
  /// found again by its package's name, its own path and its length; see [`Bound`] for what is
  /// answered when one is not.
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
      // The built-ins and the root are where they are; a dependency is found by name.
      let found = if index <= ROOT {
        Some(index)
      } else {
        given.dependency(&package.name, package.ordinal)
      };
      for id in package.sources {
        let slot = &self.sources[id.get() as usize];
        texts[id.get() as usize] = if slot.package_source {
          Some("")
        } else {
          found.and_then(|at: usize| given.file_text(at, slot, &packages[at]))
        };
      }
    }
    Bound { map: self, texts }
  }

  /// The index of the dependency named `name` that `ordinal` dependencies with that name precede.
  fn dependency(&self, name: &[u8], ordinal: u32) -> Option<usize> {
    let dependencies: &[PackageSlot] = self.packages.get(ROOT + 1..)?;
    let first = dependencies.partition_point(|slot: &PackageSlot| *slot.name < *name);
    let at = first.checked_add(ordinal as usize)?;
    let slot = dependencies.get(at)?;
    if *slot.name == *name {
      Some(ROOT + 1 + at)
    } else {
      None
    }
  }

  /// The text of the file of package `index` that has `wanted`'s path and ordinal, if it is as long
  /// as `wanted` recorded and is UTF-8. `package` is that package's view, whose files are in the
  /// order this map numbered them.
  fn file_text<'a>(
    &self,
    index: usize,
    wanted: &SourceSlot,
    package: &PackageView<'a>,
  ) -> Option<&'a str> {
    let slot = self.packages.get(index)?;
    let files: &[SourceSlot] = self
      .sources
      .get(slot.sources.start() as usize + 1..slot.sources.end() as usize)?;
    let key = wanted.key();
    let first = files
      .partition_point(|file: &SourceSlot| set::compare_paths(file.key(), key) == Ordering::Less);
    let at = first.checked_add(wanted.ordinal as usize)?;
    let found = files.get(at)?;
    if set::compare_paths(found.key(), key) != Ordering::Equal || found.len != wanted.len {
      return None;
    }
    package.files.get(at)?.text.text()
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
/// - a supplied file's text, when that file is in the packages given to [`SourceMap::bind`] with
///   the recorded path and the recorded length, and is UTF-8.
///
/// And `None`:
///
/// - for a supplied file that is missing, changed, or not UTF-8. A byte text that is not UTF-8
///   always carries a syntax diagnostic of its own, and is rendered without a snippet;
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
