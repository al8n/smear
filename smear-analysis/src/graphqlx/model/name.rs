//! Names and text: the model's two arenas, read by symbol and by run, and the canonical names that
//! identify a definition across analyses.

use core::fmt;

use tokora::diagnostic::Location;

use crate::graphqlx::id::{DefId, ModuleId, Sym};

use super::{
  Model,
  def::DefKind,
  table::{AliasRow, DescriptionRow, PackageRow, UNSET_MODULE},
};

/// The two kinds of name a module gives: a type definition's and a directive definition's are
/// apart, so one module may hold a type and a directive with one name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Namespace {
  /// The name of every definition but a directive's.
  Type,
  /// The name of a directive definition.
  Directive,
}

impl Namespace {
  /// The namespace a definition of `kind` is named in.
  const fn of(kind: DefKind) -> Self {
    match kind {
      DefKind::Directive => Self::Directive,
      _ => Self::Type,
    }
  }
}

impl Model {
  /// The name `sym` interned.
  ///
  /// # Panics
  ///
  /// If `sym` is not one of this model's names: a symbol belongs to the analysis that issued it.
  #[inline]
  pub fn name(&self, sym: Sym) -> &str {
    assert!(
      (sym.get() as usize) < self.spans.len(),
      "name {} is past the {} names of this model",
      sym.get(),
      self.spans.len()
    );
    let (start, end) = self.spans[sym.get() as usize];
    self.names.get(start as usize..end as usize).unwrap_or("")
  }

  /// The canonical name of `def`: `^::package::module::path::Name` for a type, and
  /// `@^::package::module::path::name` for a directive. Its `Display` writes it.
  ///
  /// It is the definition's identity across analyses and versions, spelled as a dependency path:
  /// a valid path in every package that may reach the definition's package, and in none of that
  /// package's own modules, which say `~::`. A definition in a package's root module has no
  /// module segment: `^::core::i32`.
  ///
  /// # Panics
  ///
  /// If `def` is not one of [`defs`](Self::defs).
  #[inline]
  pub fn canonical_name(&self, def: DefId) -> CanonicalName<'_> {
    assert!(
      (def.get() as usize) < self.defs.len(),
      "definition id {} is past the {} definitions of this model",
      def.get(),
      self.defs.len()
    );
    CanonicalName { model: self, def }
  }

  /// The definition the canonical name `name` names in `namespace`, or `None` when none does.
  ///
  /// `name` is spelled as [`canonical_name`](Self::canonical_name) writes it, the `@` included
  /// for a directive, or as a second name a module gives a definition: `^::core::Int` finds
  /// `i32`. A name that is not UTF-8, or not a dependency path, finds nothing. When a module gives
  /// one name twice, which is an error, the first definition in id order is found.
  pub fn lookup(&self, namespace: Namespace, name: impl AsRef<[u8]>) -> Option<DefId> {
    let text = core::str::from_utf8(name.as_ref()).ok()?;
    let path = match namespace {
      Namespace::Type => text,
      Namespace::Directive => text.strip_prefix('@')?,
    };
    let (module, item) = path.strip_prefix("^::")?.rsplit_once("::")?;
    let module = self.find_module(module)?;
    self.named_in(module, namespace, item)
  }

  /// The module the dependency path `path` names: `^::package` for a package's root module, and
  /// `^::package::module::path` for the others. `None` when it names none, or is not UTF-8.
  pub fn module_by_path(&self, path: impl AsRef<[u8]>) -> Option<ModuleId> {
    let text = core::str::from_utf8(path.as_ref()).ok()?;
    self.find_module(text.strip_prefix("^::")?)
  }

  /// The module `path` names: a package's name, then the names of the modules down from its root
  /// module, `::`-separated. The first package in id order with that name is the one searched.
  fn find_module(&self, path: &str) -> Option<ModuleId> {
    let mut segments = path.split("::");
    let package = segments.next()?;
    let package = self
      .packages
      .iter()
      .find(|row: &&PackageRow| row.name.text(&self.text) == package)?;
    let mut module = package.root_module;
    for segment in segments {
      let row = self.modules.get(module.get() as usize)?;
      module = *row
        .children
        .of(&self.module_children)
        .iter()
        .find(|child: &&ModuleId| self.last_segment(**child) == Some(segment))?;
    }
    (module != UNSET_MODULE).then_some(module)
  }

  /// The last segment of `module`'s path: its own name, or `None` for a root module.
  fn last_segment(&self, module: ModuleId) -> Option<&str> {
    let row = self.modules.get(module.get() as usize)?;
    let last = row.path.of(&self.paths).last()?;
    Some(self.name(*last))
  }

  /// The definition named `item` in `namespace` that `module` holds, or else gives a second name.
  fn named_in(&self, module: ModuleId, namespace: Namespace, item: &str) -> Option<DefId> {
    let row = self.modules.get(module.get() as usize)?;
    let first = self
      .aliases
      .partition_point(|alias: &AliasRow| alias.module < module);
    row
      .defs
      .into_iter()
      .find(|id: &DefId| {
        let def = &self.defs[id.get() as usize];
        Namespace::of(def.kind) == namespace && self.name(def.name) == item
      })
      .or_else(|| {
        self.aliases[first..]
          .iter()
          .take_while(|alias: &&AliasRow| alias.module == module)
          .find(|alias: &&AliasRow| {
            self.name(alias.name) == item
              && Namespace::of(self.defs[alias.def.get() as usize].kind) == namespace
          })
          .map(|alias: &AliasRow| alias.def)
      })
  }
}

/// The canonical name of a definition, as [`Model::canonical_name`] answers it: its `Display`
/// writes `^::package::module::path::Name`, or `@^::package::module::path::name` for a directive.
///
/// `Copy`: it holds the model and the definition, and borrows its spelling from the model as it
/// writes it.
#[derive(Clone, Copy)]
pub struct CanonicalName<'a> {
  model: &'a Model,
  def: DefId,
}

impl fmt::Display for CanonicalName<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let model = self.model;
    let def = &model.defs[self.def.get() as usize];
    let module = &model.modules[def.module.get() as usize];
    if def.kind == DefKind::Directive {
      f.write_str("@")?;
    }
    f.write_str("^::")?;
    f.write_str(
      model.packages[module.package.get() as usize]
        .name
        .text(&model.text),
    )?;
    for segment in module.path.of(&model.paths) {
      f.write_str("::")?;
      f.write_str(model.name(*segment))?;
    }
    f.write_str("::")?;
    f.write_str(model.name(def.name))
  }
}

impl fmt::Debug for CanonicalName<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("CanonicalName(")?;
    fmt::Display::fmt(self, f)?;
    f.write_str(")")
  }
}

/// A description: its text, cooked, and where it is written.
///
/// An item with no description answers `None`, and one with an empty description answers a
/// `Description` whose text is `""`: the two are kept apart.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Description<'a> {
  text: &'a str,
  location: Location,
}

impl<'a> Description<'a> {
  /// The description `row` holds in `model`, if any.
  pub(super) fn of(model: &'a Model, row: Option<DescriptionRow>) -> Option<Self> {
    let row = row?;
    Some(Self {
      text: row.text.text(&model.text),
      location: row.location,
    })
  }

  /// Its text, cooked: escapes decoded, and a block string's indentation removed.
  #[inline]
  pub const fn text(&self) -> &'a str {
    self.text
  }

  /// Where it is written.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }
}
