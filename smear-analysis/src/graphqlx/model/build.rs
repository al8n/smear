//! The builder: the one way the analysis writes a model.
//!
//! # Who appends what, and in what order
//!
//! An id is the position its row is appended at, and the finished model is never renumbered, so
//! each table is appended in its ids' order:
//!
//! - packages, by `analyze`: `core`, `alloc`, `std`, the root, then the dependencies by name;
//! - modules, by the module tree: package by package, each package's root module first, then by
//!   path, compared segment by segment, each segment bytewise;
//! - definitions, by item collection: by package, module path, source id, then position, the
//!   materialised primitives first in `core`'s root module;
//! - contributions, with their definition: the definition, then each extension by source id and
//!   position; the schema's after every definition's;
//! - members, by item collection: owner by owner, the direct members by contribution and as
//!   written, then each field's arguments, field by field;
//! - type occurrences, values and applications, by lowering: in the order of the walk over every
//!   package, an occurrence before what it nests (`order` spells the walk out);
//! - types, by interning as resolution reaches each type occurrence: a type's arguments before it;
//! - names, by interning: by first occurrence, in source-id order, then position.
//!
//! The later phases write onto rows already appended: resolution gives occurrences their kinds and
//! types, and applications their directives; instantiation adds instantiations and satisfactions;
//! coercion gives values their shapes and types; the tier phase gives definitions and types their
//! tiers. What none of them wrote keeps the neutral value its row was appended with, which the
//! accessors document.
//!
//! # The checks
//!
//! Each list grows at the end of its pool or not at all, so it stays one run; appending to a list
//! that another list has followed fails an assertion at that append. When the model is finished,
//! `order` checks, under `debug_assertions`, that every table is in its ids' order and that the
//! walk reaches every occurrence in id order.

#[cfg(feature = "test-support")]
mod phases;

use std::{boxed::Box, collections::BTreeMap, string::String, vec, vec::Vec};

use smear_semantics::interner::{Interner, Symbol};
use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{DefId, IdRange, ModuleId, PackageId, Sym, TyId, dense},
  package::{IntegerProvider, Layout, Tier},
};

use super::{
  Model, Namespace, PackageKind,
  def::DefKind,
  module::ModuleKind,
  order,
  table::{
    AliasRow, BUILTIN_DIRECTIVES, BUILTIN_TYPES, BuiltinsRow, DefRow, KIND_UNRESOLVED, ModuleRow,
    NO_DEF, PackageRow, Run, TyRow, TypeExprRow, UNSET_MODULE, UNSET_TY, ValueRow,
  },
};

impl Symbol for Sym {
  #[inline]
  fn from_index(index: u32) -> Self {
    Self::new(index)
  }

  #[inline]
  fn index(self) -> u32 {
    self.get()
  }
}

/// A model being written: the tables, and what only writing them needs.
///
/// Crate-private in every build that ships: the test-support door is the one way outside the
/// crate to it.
pub struct ModelBuilder {
  model: Model,
  /// The name arena while it grows. The finished model keeps its spellings and spans.
  names: Interner<Sym>,
  /// Every type interned so far, keyed by its shape as words: see `intern_type`.
  types: BTreeMap<Box<[u32]>, TyId>,
  /// Every closed instantiation added, in any order: sorted into runs when the model is finished.
  instantiations: Vec<(DefId, TyId)>,
  /// Every location an error was reported at: applied when the model is finished.
  marks: Vec<Location>,
}

impl Default for ModelBuilder {
  /// A model with nothing in it yet.
  fn default() -> Self {
    Self {
      model: Model::empty(),
      names: Interner::default(),
      types: BTreeMap::new(),
      instantiations: Vec::new(),
      marks: Vec::new(),
    }
  }
}

impl ModelBuilder {
  /// Appends a package named `name`, of `kind`, `tier` and `layout`, numbered with the next
  /// `sources` source ids: its package source, then its files.
  ///
  /// Packages are appended in id order, and each takes the source ids after the previous one's.
  pub fn push_package(
    &mut self,
    name: &str,
    kind: PackageKind,
    tier: Tier,
    layout: Layout,
    sources: u32,
  ) -> PackageId {
    let id = PackageId::new(dense(self.model.packages.len()));
    let start = match self.model.packages.last() {
      Some(previous) => previous.sources.end(),
      None => 0,
    };
    let name = push_text(&mut self.model.text, name);
    self.model.packages.push(PackageRow {
      name,
      kind,
      tier,
      layout,
      sources: IdRange::new(start, start.saturating_add(sources)),
      dependencies: Run::default(),
      integers: None,
      root_module: UNSET_MODULE,
    });
    id
  }

  /// Adds `dependency` to `package`'s resolved dependencies, after those it has.
  ///
  /// A package's dependencies are added together, in id order, before the next package's.
  pub fn push_dependency(&mut self, package: PackageId, dependency: PackageId) {
    let row = &mut self.model.packages[package.get() as usize];
    grow(
      &mut self.model.dependencies,
      &mut row.dependencies,
      dependency,
    );
  }

  /// Gives `package` `integers` as its integer provider.
  pub fn set_integers(&mut self, package: PackageId, integers: IntegerProvider) {
    let index = dense(self.model.integers.len());
    self.model.integers.push(integers);
    self.model.packages[package.get() as usize].integers = Some(index);
  }

  /// Appends a module of `package`, of `kind`: its root module when `parent` and `name` are
  /// `None`, and otherwise the module `name` inside `parent`.
  ///
  /// Modules are appended in id order: package by package, each package's root module first, then
  /// by path, compared segment by segment, each segment bytewise, so a module comes before its
  /// descendants.
  pub fn push_module(
    &mut self,
    package: PackageId,
    parent: Option<ModuleId>,
    name: Option<&str>,
    kind: ModuleKind,
  ) -> ModuleId {
    let id = ModuleId::new(dense(self.model.modules.len()));
    debug_assert_eq!(
      parent.is_none(),
      kind == ModuleKind::Root,
      "a module has a parent unless it is its package's root module"
    );
    let path = match (parent, name) {
      (Some(parent), Some(name)) => {
        let segment = self.names.intern(name.as_bytes());
        let inherited = self.model.modules[parent.get() as usize].path;
        let start = dense(self.model.paths.len());
        self
          .model
          .paths
          .extend_from_within(inherited.start as usize..inherited.end() as usize);
        self.model.paths.push(segment);
        Run {
          start,
          len: inherited.len.saturating_add(1),
        }
      }
      _ => Run::default(),
    };
    if kind == ModuleKind::Root {
      let row = &mut self.model.packages[package.get() as usize];
      debug_assert_eq!(
        row.root_module, UNSET_MODULE,
        "a package has one root module"
      );
      row.root_module = id;
    }
    self.model.modules.push(ModuleRow {
      package,
      parent,
      path,
      kind,
      location: None,
      description: None,
      defs: IdRange::new(0, 0),
      children: Run::default(),
    });
    id
  }

  /// The finished model.
  ///
  /// It derives what is derived: each module's children and definitions, each definition's
  /// instantiations, the sorted lookup tables and the built-ins. It gives every type occurrence and
  /// value no phase gave a type the one unresolved type, and applies the marks. Under
  /// `debug_assertions` it then checks every table's order.
  pub fn finish(mut self) -> Model {
    self.fill_types();
    self.derive_modules();
    self.derive_instantiations();
    self.derive_lookups();
    self.derive_builtins();
    self.apply_marks();

    let ModelBuilder {
      mut model, names, ..
    } = self;
    let (bytes, spans, _) = names.into_parts();
    // Every spelling was interned from a `&str`, so the arena is UTF-8 and every span falls on a
    // character boundary.
    model.names = String::from_utf8(bytes).unwrap_or_default();
    model.spans = spans;

    if cfg!(debug_assertions) {
      order::check(&model);
    }
    model
  }

  /// The type `row` describes, whose arguments are `args`: the one already interned with the same
  /// shape, or a new one appended.
  ///
  /// A type's key is its shape as words: the kind, whether it is non-null, its definition, its
  /// parameter index and its element, then its arguments. A field a shape gives no meaning to holds
  /// the same placeholder in every row, so two types are one exactly when their keys are equal.
  fn intern_type(&mut self, mut row: TyRow, args: &[TyId]) -> TyId {
    let mut key: Vec<u32> = Vec::with_capacity(5 + args.len());
    key.push(u32::from(row.shape));
    key.push(u32::from(row.non_null));
    key.push(row.def.get());
    key.push(row.index);
    key.push(row.element.get());
    for arg in args {
      key.push(arg.get());
    }
    if let Some(found) = self.types.get(key.as_slice()) {
      return *found;
    }
    let id = TyId::new(dense(self.model.tys.len()));
    row.args = append(&mut self.model.ty_args, args);
    self.model.tys.push(row);
    self.types.insert(key.into_boxed_slice(), id);
    id
  }

  /// Gives every type occurrence and value no phase gave a type the one type of every unresolved
  /// occurrence, interning it if nothing has.
  fn fill_types(&mut self) {
    let exprs = self
      .model
      .type_exprs
      .iter()
      .any(|row: &TypeExprRow| row.ty == UNSET_TY);
    let values = self
      .model
      .values
      .iter()
      .any(|row: &ValueRow| row.ty == UNSET_TY);
    if !exprs && !values {
      return;
    }
    let unresolved = self.intern_type(unresolved_ty(), &[]);
    for row in &mut self.model.type_exprs {
      if row.ty == UNSET_TY {
        row.ty = unresolved;
      }
    }
    for row in &mut self.model.values {
      if row.ty == UNSET_TY {
        row.ty = unresolved;
      }
    }
  }

  /// Each module's child modules, and each module's definitions.
  ///
  /// A module's children are not adjacent in the module table (each child's descendants follow
  /// it), so they are counted per parent and then placed, one run per module, in id order.
  /// A module's definitions are adjacent, in module order, so one pass finds each module's run.
  fn derive_modules(&mut self) {
    let count = self.model.modules.len();
    let mut sizes: Vec<u32> = vec![0; count];
    for row in &self.model.modules {
      if let Some(parent) = row.parent {
        sizes[parent.get() as usize] += 1;
      }
    }
    let mut next: Vec<u32> = Vec::with_capacity(count);
    let mut start: u32 = 0;
    for (row, size) in self.model.modules.iter_mut().zip(&sizes) {
      row.children = Run { start, len: *size };
      next.push(start);
      start += *size;
    }
    let mut children: Vec<ModuleId> = vec![ModuleId::new(0); start as usize];
    for (index, row) in self.model.modules.iter().enumerate() {
      if let Some(parent) = row.parent {
        let slot = &mut next[parent.get() as usize];
        children[*slot as usize] = ModuleId::new(dense(index));
        *slot += 1;
      }
    }
    self.model.module_children = children;

    let defs = self.model.defs.len();
    let mut at: usize = 0;
    for (index, row) in self.model.modules.iter_mut().enumerate() {
      let first = at;
      while at < defs && self.model.defs[at].module.get() as usize == index {
        at += 1;
      }
      row.defs = IdRange::new(dense(first), dense(at));
    }
  }

  /// Each definition's closed instantiations, sorted and each kept once, as one run per
  /// definition.
  fn derive_instantiations(&mut self) {
    let mut pairs: Vec<(DefId, TyId)> = core::mem::take(&mut self.instantiations);
    pairs.sort_unstable();
    pairs.dedup();
    let mut at: usize = 0;
    for (index, row) in self.model.defs.iter_mut().enumerate() {
      let start = dense(self.model.instantiations.len());
      while at < pairs.len() && pairs[at].0.get() as usize == index {
        self.model.instantiations.push(pairs[at].1);
        at += 1;
      }
      row.instantiations = Run {
        start,
        len: dense(self.model.instantiations.len()) - start,
      };
    }
  }

  /// The sorted tables a lookup searches: the satisfactions, each once, and the aliases, by module,
  /// then the namespace of the definition each names, then name, the first added kept when a
  /// module gives one name twice in one namespace.
  fn derive_lookups(&mut self) {
    self.model.satisfactions.sort_unstable();
    self.model.satisfactions.dedup();
    let defs: &[DefRow] = &self.model.defs;
    let aliases: &mut Vec<AliasRow> = &mut self.model.aliases;
    // The sort is stable, so among aliases with one key the first added stays first, and is kept.
    aliases.sort_by_key(|row: &AliasRow| alias_key(defs, row));
    aliases.dedup_by_key(|row: &mut AliasRow| alias_key(defs, row));
  }

  /// The built-in definitions a consumer keys on, found by name among the built-in packages'
  /// definitions, the first in id order winning; and the materialised primitives, in id order.
  fn derive_builtins(&mut self) {
    let mut builtins = BuiltinsRow::default();
    for (index, row) in self.model.defs.iter().enumerate() {
      let package = self.model.modules[row.module.get() as usize].package;
      if self.model.packages[package.get() as usize].kind != PackageKind::BuiltIn {
        continue;
      }
      let id = DefId::new(dense(index));
      let name = self.names.bytes(row.name);
      if row.kind == DefKind::Directive {
        for (slot, wanted) in builtins.directives.iter_mut().zip(BUILTIN_DIRECTIVES) {
          if slot.is_none() && wanted.as_bytes() == name {
            *slot = Some(id);
          }
        }
      } else {
        for (slot, wanted) in builtins.types.iter_mut().zip(BUILTIN_TYPES) {
          if slot.is_none() && wanted.as_bytes() == name {
            *slot = Some(id);
          }
        }
      }
      if let Some(primitive) = row.intrinsic {
        self.model.primitives.push((primitive, id));
      }
    }
    self.model.builtins = builtins;
  }

  /// Marks every contribution and member whose extent holds the location of an error reported
  /// while the model was written.
  fn apply_marks(&mut self) {
    self.model.mark(&self.marks);
  }
}

impl Model {
  /// Marks every contribution and member whose extent holds one of `marks`, the locations errors
  /// were reported at; a definition one of whose contributions is marked has errors.
  ///
  /// A location that covers a whole source lies within no extent, and an extent that covers a
  /// whole source holds nothing: what has no position of its own marks nothing.
  pub(crate) fn mark(&mut self, marks: &[Location]) {
    let mut spans: Vec<(u32, usize, usize)> = Vec::with_capacity(marks.len());
    for at in marks {
      if let Some(span) = at.span() {
        spans.push((at.source(), span.start(), span.end()));
      }
    }
    if spans.is_empty() {
      return;
    }
    spans.sort_unstable();
    for row in &mut self.contributions {
      if holds_any(&spans, row.extent) {
        row.marked = true;
      }
    }
    for row in &mut self.members {
      if holds_any(&spans, row.extent) {
        row.marked = true;
      }
    }
  }
}

/// What the aliases are sorted and kept once by: the module, the namespace of the definition the
/// alias names, then the name. A module's second names for types come before its second names for
/// directives.
fn alias_key(defs: &[DefRow], row: &AliasRow) -> (ModuleId, u8, Sym) {
  let namespace = match Namespace::of(defs[row.def.get() as usize].kind) {
    Namespace::Type => 0,
    Namespace::Directive => 1,
  };
  (row.module, namespace, row.name)
}

/// The one type every unresolved occurrence has.
fn unresolved_ty() -> TyRow {
  TyRow {
    shape: KIND_UNRESOLVED,
    def: NO_DEF,
    index: 0,
    element: TyId::new(0),
    args: Run::default(),
    non_null: false,
    closed: true,
    tier: Tier::Core,
  }
}

/// Whether one of `marks`, sorted by source and then span, lies within `extent`.
///
/// A location that covers a whole source lies within no extent, and an extent that covers a whole
/// source holds no mark: what has no position of its own marks nothing.
fn holds_any(marks: &[(u32, usize, usize)], extent: Location) -> bool {
  let Some(span) = extent.span() else {
    return false;
  };
  let (source, start, end) = (extent.source(), span.start(), span.end());
  let first =
    marks.partition_point(|mark: &(u32, usize, usize)| (mark.0, mark.1) < (source, start));
  marks[first..]
    .iter()
    .take_while(|mark: &&(u32, usize, usize)| mark.0 == source && mark.1 <= end)
    .any(|mark: &(u32, usize, usize)| mark.2 <= end)
}

/// Appends `text` to `arena`: the run it fills.
fn push_text(arena: &mut String, text: &str) -> Run {
  let start = dense(arena.len());
  arena.push_str(text);
  Run {
    start,
    len: dense(text.len()),
  }
}

/// Appends `entries` to `pool` as one run.
fn append<T: Copy>(pool: &mut Vec<T>, entries: &[T]) -> Run {
  let start = dense(pool.len());
  pool.extend_from_slice(entries);
  Run {
    start,
    len: dense(entries.len()),
  }
}

/// Appends `entry` to `pool` as the next entry of `run`.
fn grow<T>(pool: &mut Vec<T>, run: &mut Run, entry: T) {
  extend(run, dense(pool.len()));
  pool.push(entry);
}

/// Makes the entry at `index` of `run`'s pool the next entry of `run`, which must end there: a
/// list grows only at the end of its pool, so that it stays one run.
fn extend(run: &mut Run, index: u32) {
  if run.len == 0 {
    run.start = index;
  }
  debug_assert_eq!(
    run.end(),
    index,
    "a list grows only at the end of its pool, and another list was appended after this one"
  );
  run.len = run.len.saturating_add(1);
}
