//! What a caller supplies: packages of source files, and what each package declares.

use core::ops::RangeInclusive;
use std::{boxed::Box, vec::Vec};

/// One file of a [`Package`]: where it sits in the package, and its text.
///
/// Built with [`SourceFile::new`] and read through its getters; the fields are private so that a
/// field added later breaks no caller.
#[derive(Debug, Clone)]
pub struct SourceFile<S> {
  path: Box<[u8]>,
  text: S,
}

impl<S> SourceFile<S> {
  /// A file at `path` holding `text`.
  ///
  /// `path` is relative to the package's root and `/`-separated, as `media/vision.graphqlx` is.
  /// Nothing is checked here: a path that is absolute, uses `\`, is not UTF-8, or has an empty, `.`
  /// or `..` segment is reported by the analysis as `smear::graphqlx::invalid-file-path`, at the
  /// package as a whole.
  pub fn new(path: impl AsRef<[u8]>, text: S) -> Self {
    Self {
      path: path.as_ref().into(),
      text,
    }
  }

  /// The path, as it was given.
  #[inline]
  pub fn path(&self) -> &[u8] {
    &self.path
  }

  /// The text.
  #[inline]
  pub const fn text(&self) -> &S {
    &self.text
  }
}

/// How a package's files become its modules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Layout {
  /// Directory modules: a file's path is its module's path, so `media/vision.graphqlx` is the
  /// module `~::media::vision`.
  Directory,
  /// One module for the whole package, every file merged into it, as a GraphQL schema split across
  /// files is merged.
  Flat,
}

/// What a package may use, from the language's three built-in packages.
///
/// Ordered by what each admits: `Core < Alloc < Std`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Tier {
  /// `core` alone: no allocation and no operating system.
  Core,
  /// `core` and `alloc`: allocation, and no operating system.
  Alloc,
  /// All three: `core`, `alloc` and `std`.
  Std,
}

/// Where a package's integer widths beyond `core`'s own come from.
///
/// `core` provides `i8` to `i128` and `u8` to `u128`. Any other width a package writes needs that
/// package's provider, and must lie inside the range the provider declares for its signedness.
/// The provider changes no name: `i256` is spelled `i256` whatever provides it, and only the host
/// type a code generator chooses for it differs.
///
/// [`IntegerProvider::numo`] is the one provider v1 knows, and its only constructor.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerProvider {
  name: &'static str,
  signed: (u32, u32),
  unsigned: (u32, u32),
}

impl IntegerProvider {
  /// The `numo` crate: signed and unsigned integers of every width from 0 to 65536 bits, the range
  /// numo provides for its default storage.
  #[inline]
  pub const fn numo() -> Self {
    Self {
      name: "numo",
      signed: (0, 65536),
      unsigned: (0, 65536),
    }
  }

  /// The provider's name, as a manifest writes it: `numo`.
  #[inline]
  pub const fn name(&self) -> &str {
    self.name
  }

  /// The signed widths, in bits, the provider supplies.
  #[inline]
  pub const fn signed(&self) -> RangeInclusive<u32> {
    RangeInclusive::new(self.signed.0, self.signed.1)
  }

  /// The unsigned widths, in bits, the provider supplies.
  #[inline]
  pub const fn unsigned(&self) -> RangeInclusive<u32> {
    RangeInclusive::new(self.unsigned.0, self.unsigned.1)
  }

  /// Everything the provider holds, as one key the package order breaks ties with.
  #[inline]
  pub(crate) const fn key(&self) -> (&str, u32, u32, u32, u32) {
    (
      self.name,
      self.signed.0,
      self.signed.1,
      self.unsigned.0,
      self.unsigned.1,
    )
  }
}

/// One package: its name, how its files map to modules, its tier, its files, the packages it
/// depends on, and its integer provider.
///
/// Built with [`Package::new`] and the `with_*` setters, and read through its getters. The fields
/// are private so that a field added later breaks no caller. Nothing is checked while building;
/// the analysis reports what is wrong with a package, at that package as a whole.
///
/// The caller reads its manifest, walks its directories and reads its files: the analysis touches
/// no file system and no network. Every dependency is supplied up front, beside the root package.
#[derive(Debug, Clone)]
pub struct Package<S> {
  name: Box<[u8]>,
  layout: Layout,
  tier: Tier,
  files: Vec<SourceFile<S>>,
  dependencies: Vec<Box<[u8]>>,
  integers: Option<IntegerProvider>,
}

impl<S> Package<S> {
  /// A package named `name`, with no files, no dependencies and no integer provider.
  ///
  /// `name` is a GraphQLx Name, and not `core`, `alloc` or `std`, which are the built-in packages'.
  /// A name that breaks either rule is reported by the analysis, not refused here.
  pub fn new(name: impl AsRef<[u8]>, layout: Layout, tier: Tier) -> Self {
    Self {
      name: name.as_ref().into(),
      layout,
      tier,
      files: Vec::new(),
      dependencies: Vec::new(),
      integers: None,
    }
  }

  /// The same package with `files` as its files, in place of any it had.
  ///
  /// Their order does not matter: the analysis numbers a package's files by path.
  #[must_use]
  pub fn with_files<I>(mut self, files: I) -> Self
  where
    I: IntoIterator<Item = SourceFile<S>>,
  {
    self.files = files.into_iter().collect::<Vec<SourceFile<S>>>();
    self
  }

  /// The same package declaring `dependencies`, by name, in place of any it declared.
  ///
  /// Their order does not matter, and a name declared twice is declared once. A package reaches
  /// its declared dependencies and the built-in `core`, `alloc` and `std`, and no other; a declared
  /// dependency the analysis is not given is reported.
  #[must_use]
  pub fn with_dependencies<I>(mut self, dependencies: I) -> Self
  where
    I: IntoIterator,
    I::Item: AsRef<[u8]>,
  {
    let mut names: Vec<Box<[u8]>> = Vec::new();
    for name in dependencies {
      names.push(name.as_ref().into());
    }
    self.dependencies = names;
    self
  }

  /// The same package with `integers` as its integer provider.
  #[inline]
  #[must_use]
  pub fn with_integers(mut self, integers: IntegerProvider) -> Self {
    self.integers = Some(integers);
    self
  }

  /// The name, as it was given.
  #[inline]
  pub fn name(&self) -> &[u8] {
    &self.name
  }

  /// How the files become modules.
  #[inline]
  pub const fn layout(&self) -> Layout {
    self.layout
  }

  /// The tier.
  #[inline]
  pub const fn tier(&self) -> Tier {
    self.tier
  }

  /// The files, in the order they were given.
  #[inline]
  pub fn files(&self) -> &[SourceFile<S>] {
    &self.files
  }

  /// The declared dependencies' names, in the order they were given.
  #[inline]
  pub fn dependencies(&self) -> impl ExactSizeIterator<Item = &[u8]> {
    self.dependencies.iter().map(|name| name.as_ref())
  }

  /// The integer provider, if the package selected one.
  #[inline]
  pub const fn integers(&self) -> Option<&IntegerProvider> {
    self.integers.as_ref()
  }
}
