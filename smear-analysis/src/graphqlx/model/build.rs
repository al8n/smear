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
//! - contributions, by lowering, with their definition: the definition, then each extension by
//!   source id and position; the schema's after every definition's, its definitions before its
//!   extensions;
//! - members, by lowering: owner by owner, the direct members by contribution and as written, then
//!   each field's arguments, field by field;
//! - type occurrences, values and applications, by lowering: in the order of the walk over every
//!   package, an occurrence before what it nests (`order` spells the walk out);
//! - types, by interning as instantiation reaches each type occurrence: a type's arguments before
//!   it;
//! - names, by interning, in the order the analysis first interns them, phase by phase: the
//!   module tree, the declarations, the names `use` items bind, then lowering, each in its own
//!   order, which is a function of the program alone; it is no order over spellings.
//!
//! The later phases write onto rows already appended: resolution gives occurrences their kinds,
//! and applications their directives; instantiation gives occurrences their types, and adds
//! instantiations and satisfactions; coercion gives values their shapes and types; the tier phase
//! gives definitions and types their tiers. What none of them wrote keeps the neutral value its
//! row was appended with, which the accessors document.
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
  id::{DefId, IdRange, MemberId, ModuleId, PackageId, Sym, TyId, dense},
  package::{IntegerProvider, Layout, Tier},
};

use super::{
  Model, Namespace, PackageKind,
  def::{DefKind, DirectiveLocation, Origin, Primitive},
  member::MemberKind,
  module::ModuleKind,
  order,
  table::{
    AliasRow, BUILTIN_DIRECTIVES, BUILTIN_TYPES, BuiltinsRow, ContributionRow, DefRow,
    DescriptionRow, KIND_UNRESOLVED, MemberRow, ModuleRow, NO_DEF, PackageRow, Run, TyRow,
    TypeExprRow, TypeParamRow, UNSET_MODULE, UNSET_TY, ValueRow,
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
      extent: None,
      description: None,
      defs: IdRange::new(0, 0),
      children: Run::default(),
    });
    id
  }

  /// Records the inline `mod` that declares `module`: where it names the module, `location`, and
  /// its extent, from its description, or its `mod`, to its `}`.
  pub fn set_module_declaration(&mut self, module: ModuleId, location: Location, extent: Location) {
    let row = &mut self.model.modules[module.get() as usize];
    row.location = Some(location);
    row.extent = Some(extent);
  }

  /// Gives `module` the description `text`, cooked, written at `location`. An empty `text` is an
  /// empty description, which is not an absent one.
  pub fn set_module_description(&mut self, module: ModuleId, text: &str, location: Location) {
    let description = self.description(text, location);
    self.model.modules[module.get() as usize].description = Some(description);
  }

  /// Interns `name`: the symbol it already has, or a new one.
  ///
  /// Symbols are numbered in the order the analysis first interns them, phase by phase: the
  /// module tree, the declarations, the names `use` items bind, then lowering, each phase in its
  /// own order. That order is a function of the program alone, never of the order its files and
  /// dependencies are passed in, and it is not an order over spellings.
  pub fn intern(&mut self, name: &str) -> Sym {
    self.names.intern(name.as_bytes())
  }

  /// The symbol `name` was interned with, or `None` when no name so spelled was: a query that
  /// interns nothing.
  pub(crate) fn find(&self, name: &[u8]) -> Option<Sym> {
    self.names.lookup(name)
  }

  /// The spelling `sym` was interned with.
  pub(crate) fn spelling(&self, sym: Sym) -> &[u8] {
    self.names.bytes(sym)
  }

  /// Where the definition `def` is named.
  pub(crate) fn def_location(&self, def: DefId) -> Location {
    self.model.defs[def.get() as usize].location
  }

  /// Appends a definition: a `kind` named `name` in `module`, its name written at `location`, of
  /// `origin`.
  ///
  /// Definitions are appended in id order: by package, then module path, then source id, then
  /// position. A definition starts with no contribution, no list, no description, the tier `Core`
  /// and no built-in meaning; the methods below give it them.
  pub fn push_def(
    &mut self,
    module: ModuleId,
    kind: DefKind,
    name: Sym,
    location: Location,
    origin: Origin,
  ) -> DefId {
    debug_assert!(
      (module.get() as usize) < self.model.modules.len(),
      "a definition's module is appended before it"
    );
    let id = DefId::new(dense(self.model.defs.len()));
    self.model.defs.push(DefRow {
      kind,
      name,
      module,
      location,
      origin,
      description: None,
      contributions: Run::default(),
      type_params: Run::default(),
      bounds: Run::default(),
      implements: Run::default(),
      union_members: Run::default(),
      applications: Run::default(),
      members: IdRange::new(0, 0),
      locations: Run::default(),
      repeatable: false,
      tier: Tier::Core,
      intrinsic: None,
      marker: false,
      collection: None,
      externs: Run::default(),
      instantiations: Run::default(),
    });
    id
  }

  /// Appends `def`'s own contribution: its definition, named at `location` and spanning `extent`,
  /// from its description or first keyword to its last token. Answers its index among `def`'s
  /// contributions, which is 0.
  ///
  /// A definition's contributions are appended together, the definition first, then each
  /// extension by source id and position ([`push_extension`](Self::push_extension)), before the next
  /// definition's.
  pub fn push_contribution(&mut self, def: DefId, location: Location, extent: Location) -> u32 {
    let row = &mut self.model.defs[def.get() as usize];
    let index = row.contributions.len;
    grow(
      &mut self.model.contributions,
      &mut row.contributions,
      contribution_row(location, extent, false),
    );
    index
  }

  /// Appends an extension of `def` as its next contribution: the extension names `def` at
  /// `location`, its target path, and spans `extent`, from its `extend` to its last token. Answers
  /// its index among `def`'s contributions.
  ///
  /// It follows `def`'s own contribution ([`push_contribution`](Self::push_contribution)) and
  /// every extension of `def` before it by source id and position, and comes before the next
  /// definition's contributions.
  pub fn push_extension(&mut self, def: DefId, location: Location, extent: Location) -> u32 {
    let row = &mut self.model.defs[def.get() as usize];
    let index = row.contributions.len;
    grow(
      &mut self.model.contributions,
      &mut row.contributions,
      contribution_row(location, extent, true),
    );
    index
  }

  /// Gives `def` the description `text`, cooked, written at `location`.
  pub fn set_def_description(&mut self, def: DefId, text: &str, location: Location) {
    let description = self.description(text, location);
    self.model.defs[def.get() as usize].description = Some(description);
  }

  /// Appends a type parameter named `name`, written at `location`, to `def`. Answers its index.
  pub fn push_type_param(&mut self, def: DefId, name: Sym, location: Location) -> u32 {
    let row = &mut self.model.defs[def.get() as usize];
    let index = row.type_params.len;
    grow(
      &mut self.model.type_params,
      &mut row.type_params,
      TypeParamRow {
        name,
        location,
        default: None,
      },
    );
    index
  }

  /// Adds `location` to the locations of the directive definition `def`, after those it has.
  pub fn push_directive_location(&mut self, def: DefId, location: DirectiveLocation) {
    let row = &mut self.model.defs[def.get() as usize];
    grow(&mut self.model.locations, &mut row.locations, location);
  }

  /// Makes the directive definition `def` repeatable.
  pub fn set_repeatable(&mut self, def: DefId) {
    self.model.defs[def.get() as usize].repeatable = true;
  }

  /// Makes `def` the intrinsic primitive `primitive`.
  pub fn set_intrinsic(&mut self, def: DefId, primitive: Primitive) {
    self.model.defs[def.get() as usize].intrinsic = Some(primitive);
  }

  /// Gives `def` the tier it requires.
  pub fn set_def_tier(&mut self, def: DefId, tier: Tier) {
    self.model.defs[def.get() as usize].tier = tier;
  }

  /// Makes `name` in `module` a second name of `def`, as `Int` is of `i32`. Second names are added
  /// in any order. A module may give one name to a type and to a directive, which are named in two
  /// namespaces; when it gives one name twice in one namespace, the first added is kept.
  pub fn push_alias(&mut self, module: ModuleId, name: Sym, def: DefId) {
    self.model.aliases.push(AliasRow { module, name, def });
  }

  /// Appends a direct member of `owner`, contributed by its contribution `contribution`: a `kind`
  /// named `name`, its name written at `location`, spanning `extent`.
  ///
  /// Members are appended in id order: owner by owner; an owner's direct members first, by
  /// contribution and then as written; then each field's arguments, field by field, through
  /// [`push_argument`](Self::push_argument).
  pub fn push_member(
    &mut self,
    owner: DefId,
    contribution: u32,
    kind: MemberKind,
    name: Sym,
    location: Location,
    extent: Location,
  ) -> MemberId {
    let index = dense(self.model.members.len());
    let row = &mut self.model.defs[owner.get() as usize];
    debug_assert_eq!(
      row.kind.member_kind(),
      Some(kind),
      "a definition's direct members are of the one kind its own kind has"
    );
    row.members = extend_range(row.members, index);
    let part = slot(row.contributions, contribution);
    let part = &mut self.model.contributions[part];
    part.members = extend_range(part.members, index);
    self
      .model
      .members
      .push(member_row(kind, name, owner, None, location, extent));
    MemberId::new(index)
  }

  /// Appends an argument of `field`, named `name`, its name written at `location`, spanning
  /// `extent`.
  pub fn push_argument(
    &mut self,
    field: MemberId,
    name: Sym,
    location: Location,
    extent: Location,
  ) -> MemberId {
    let index = dense(self.model.members.len());
    let parent = &mut self.model.members[field.get() as usize];
    debug_assert_eq!(parent.kind, MemberKind::Field, "only a field has arguments");
    parent.arguments = extend_range(parent.arguments, index);
    let owner = parent.owner;
    self.model.members.push(member_row(
      MemberKind::Argument,
      name,
      owner,
      Some(field),
      location,
      extent,
    ));
    MemberId::new(index)
  }

  /// Gives `member` the description `text`, cooked, written at `location`.
  pub fn set_member_description(&mut self, member: MemberId, text: &str, location: Location) {
    let description = self.description(text, location);
    self.model.members[member.get() as usize].description = Some(description);
  }

  /// Appends a schema definition to the schema's contributions: at `location`, its `schema`, and
  /// spanning `extent`, from its description or its `schema` to its last token. Answers its index
  /// among the schema's contributions.
  ///
  /// The schema's contributions are appended after every definition's, its definitions before its
  /// extensions ([`push_schema_extension`](Self::push_schema_extension)).
  pub fn push_schema_contribution(&mut self, location: Location, extent: Location) -> u32 {
    let index = self.model.schema.contributions.len;
    grow(
      &mut self.model.contributions,
      &mut self.model.schema.contributions,
      contribution_row(location, extent, false),
    );
    index
  }

  /// Appends a schema extension to the schema's contributions: at `location`, its `extend`, and
  /// spanning `extent`, from its `extend` to its last token. Answers its index among the schema's
  /// contributions.
  ///
  /// It follows every schema definition, and every schema extension before it by source id and
  /// position.
  pub fn push_schema_extension(&mut self, location: Location, extent: Location) -> u32 {
    let index = self.model.schema.contributions.len;
    grow(
      &mut self.model.contributions,
      &mut self.model.schema.contributions,
      contribution_row(location, extent, true),
    );
    index
  }

  /// Gives the schema the description `text`, cooked, written at `location`.
  pub fn set_schema_description(&mut self, text: &str, location: Location) {
    let description = self.description(text, location);
    self.model.schema.description = Some(description);
  }

  /// Whether a spelling the model keeps was lost: the name arena refused one, as it does past the
  /// four gigabytes its `u32` offsets address, answering one shared placeholder for it; or the
  /// text arena holds more bytes than a `u32` addresses, so that a run of it saturates. Either way
  /// some spelling the model answers is not the one written.
  pub(crate) fn arenas_overflowed(&self) -> bool {
    self.names.refused().is_some() || u32::try_from(self.model.text.len()).is_err()
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

  /// A description of `text`, appended to the text arena, written at `location`.
  fn description(&mut self, text: &str, location: Location) -> DescriptionRow {
    DescriptionRow {
      text: push_text(&mut self.model.text, text),
      location,
    }
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

/// A contribution at `location`, spanning `extent`, that has contributed nothing yet: an
/// extension when `extension`, and otherwise a definition.
fn contribution_row(location: Location, extent: Location, extension: bool) -> ContributionRow {
  ContributionRow {
    location,
    extent,
    extension,
    members: IdRange::new(0, 0),
    implements: Run::default(),
    union_members: Run::default(),
    applications: Run::default(),
    marked: false,
  }
}

/// The position in its pool of the entry `index` of `run`.
fn slot(run: Run, index: u32) -> usize {
  debug_assert!(index < run.len, "the entry is one of the list's");
  run.start as usize + index as usize
}

/// `range` with the member `index` added at its end, which must be where it ends: an owner's
/// direct members, and a field's arguments, are each one run of the member table.
fn extend_range<Id>(range: IdRange<Id>, index: u32) -> IdRange<Id> {
  let next = index.saturating_add(1);
  if range.is_empty() {
    return IdRange::new(index, next);
  }
  debug_assert_eq!(
    range.end(),
    index,
    "a run of members grows only at the end of the member table"
  );
  IdRange::new(range.start(), next)
}

/// A member with no description, type, default, argument or application yet.
fn member_row(
  kind: MemberKind,
  name: Sym,
  owner: DefId,
  parent: Option<MemberId>,
  location: Location,
  extent: Location,
) -> MemberRow {
  MemberRow {
    kind,
    name,
    owner,
    parent,
    location,
    extent,
    description: None,
    ty: None,
    default: None,
    arguments: IdRange::new(0, 0),
    applications: Run::default(),
    marked: false,
  }
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
