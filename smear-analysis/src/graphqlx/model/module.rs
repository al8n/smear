//! Modules: each package's root module, its directory modules, and its inline `mod`s.

use tokora::diagnostic::Location;

use crate::graphqlx::id::{DefId, IdRange, ModuleId, PackageId, Sym, dense};

use super::{Model, name::Description, table::ModuleRow};

/// How a module came to be.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ModuleKind {
  /// A package's root module: `~::` inside the package, and `^::name` from a package that reaches
  /// it.
  Root,
  /// A directory of a `Directory` package, by its path: the files of `media/vision/` are the
  /// module `~::media::vision`, and a directory that holds no file of its own, only directories, is
  /// an empty module on the way to them. Every file of one directory is one module.
  Directory,
  /// A module only an inline `mod name { … }` declares. A `mod` of a directory module's path
  /// declares that directory module again, which is refused, and leaves it a `Directory`.
  Inline,
}

impl Model {
  /// Every module id: package by package, each package's root module first, then by path,
  /// compared segment by segment, so a module comes before its descendants.
  #[inline]
  pub fn modules(&self) -> IdRange<ModuleId> {
    IdRange::new(0, dense(self.modules.len()))
  }

  /// The module `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of [`modules`](Self::modules): an id belongs to the analysis that issued
  /// it.
  #[inline]
  pub fn module(&self, id: ModuleId) -> Module<'_> {
    assert!(
      (id.get() as usize) < self.modules.len(),
      "module id {} is past the {} modules of this model",
      id.get(),
      self.modules.len()
    );
    Module { model: self, id }
  }
}

/// One module of the model.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct Module<'a> {
  model: &'a Model,
  id: ModuleId,
}

impl<'a> Module<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> ModuleId {
    self.id
  }

  /// The package it belongs to.
  #[inline]
  pub fn package(&self) -> PackageId {
    self.row().package
  }

  /// Its path from its package's root module, one name per segment: empty for a root module.
  #[inline]
  pub fn path(&self) -> &'a [Sym] {
    let model: &'a Model = self.model;
    self.row().path.of(&model.paths)
  }

  /// The module it is inside, or `None` for a root module.
  #[inline]
  pub fn parent(&self) -> Option<ModuleId> {
    self.row().parent
  }

  /// The modules directly inside it, in id order.
  #[inline]
  pub fn children(&self) -> &'a [ModuleId] {
    let model: &'a Model = self.model;
    self.row().children.of(&model.module_children)
  }

  /// How it came to be.
  #[inline]
  pub fn kind(&self) -> ModuleKind {
    self.row().kind
  }

  /// Where the inline `mod` that declares it names it: the first in source order, when two
  /// declare it. `None` for a root module and for a directory module.
  #[inline]
  pub fn location(&self) -> Option<Location> {
    self.row().location
  }

  /// Where the inline `mod` that declares it is written, in that `mod`'s source: from its
  /// description, or its `mod` when it has none, to its last token, the `}` that closes its body
  /// where one is written. The first in source order, when two declare it, as for
  /// [`location`](Self::location); `None` exactly when its location is, for a root module and for a
  /// directory module.
  ///
  /// In an analysis made through [`analyze_with`](crate::graphqlx::analyze_with) with a layer that
  /// keeps a tree, such as `smear-parser`'s lossless layer, the smallest node of its file's tree
  /// that covers the extent is the node of that `mod`, and
  /// [`Trees::node`](crate::graphqlx::Trees::node) answers it.
  #[inline]
  pub fn extent(&self) -> Option<Location> {
    self.row().extent
  }

  /// The description of the inline `mod` that declares it, if that `mod` writes one.
  #[inline]
  pub fn description(&self) -> Option<Description<'a>> {
    Description::of(self.model, self.row().description)
  }

  /// The definitions it holds itself, in id order; its child modules' are theirs.
  #[inline]
  pub fn defs(&self) -> IdRange<DefId> {
    self.row().defs
  }

  /// The model's record of this module.
  fn row(&self) -> &'a ModuleRow {
    let model: &'a Model = self.model;
    &model.modules[self.id.get() as usize]
  }
}

impl core::fmt::Debug for Module<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Module")
      .field("id", &self.id)
      .field("package", &self.package())
      .field("kind", &self.kind())
      .field("path", &self.path())
      .field("defs", &self.defs())
      .finish()
  }
}
