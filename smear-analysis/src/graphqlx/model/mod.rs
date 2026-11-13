//! The model: what the packages declare, resolved, and read through views.
//!
//! The model is stored as `smear-schema`'s `repr` stores a schema: dense tables of `Copy` rows,
//! each child list a run of a sibling pool, one name arena and one text arena, and no allocation
//! per node (`table`). It is written once, through the crate's builder (`build`), and read through
//! views: each a `Copy` pair of the model and an id, answering borrows of the model.

mod application;
mod build;
mod def;
mod member;
mod module;
mod name;
mod order;
mod satisfy;
mod schema;
mod table;
mod ty;
mod value;

use std::{string::String, vec::Vec};

pub use application::{Application, ApplicationArgument, Resolution};
pub use build::ModelBuilder;
pub(crate) use build::{Occurrence, Shape, Written};
pub use def::{
  Collection, Contribution, Contributions, Def, DefKind, DirectiveLocation, Extern, Externs,
  Origin, Primitive, TypeBound, TypeParam, TypeParams,
};
pub use member::{Member, MemberKind};
pub use module::{Module, ModuleKind};
pub use name::{CanonicalName, Description, Namespace};
pub(crate) use satisfy::{Keys, Steps};
pub use schema::{Builtins, Root, RootOperation, Schema};
pub use ty::{Path, PathRoot, Ty, TyKind, TypeExpr, TypeExprKind};
pub use value::{FloatValue, IntValue, Literal, LiteralKind, ObjectEntry, Value, ValueKind};

use super::{
  id::{
    ApplicationId, DefId, IdRange, ModuleId, PackageId, SourceId, Sym, TyId, TypeExprId, ValueId,
    dense,
  },
  package::{IntegerProvider, Layout, Tier},
  set::ROOT,
};
use table::{
  AliasRow, ApplicationRow, BuiltinsRow, ContributionRow, DefRow, ExternRow, FloatRow, IntRow,
  LiteralRow, MemberRow, ModuleRow, PackageRow, SchemaRow, TyRow, TypeExprRow, TypeParamRow,
  ValueRow,
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
///
/// Two models are equal when every table holds equal rows in the same order and both arenas hold
/// the same spellings: when every id names the same thing in both. That is finer than every
/// accessor answering alike: a model built by another route, with the same content interned in
/// another order, is not equal. It is `PartialEq` and not `Eq` because a float value is compared
/// as an `f64` is. A float the model holds comes from a float literal, and no literal spells a
/// NaN, so a model is equal to itself.
#[derive(Debug, Clone, PartialEq)]
pub struct Model {
  packages: Vec<PackageRow>,
  /// Every package's resolved dependencies, one run per package.
  dependencies: Vec<PackageId>,
  /// The integer providers the packages selected.
  integers: Vec<IntegerProvider>,

  modules: Vec<ModuleRow>,
  /// Every module's child modules, one run per module.
  module_children: Vec<ModuleId>,

  defs: Vec<DefRow>,
  /// Every definition's contributions, then the schema's.
  contributions: Vec<ContributionRow>,
  /// The names each extension's own type parameter list writes, each with where it is written,
  /// one run per extension that writes a list, in the order of the contributions.
  written_params: Vec<(Sym, tokora::diagnostic::Location)>,
  type_params: Vec<TypeParamRow>,
  bounds: Vec<TypeBound>,
  implements: Vec<TypeExprId>,
  union_members: Vec<TypeExprId>,
  /// Every definition's applications, then the schema's.
  listed_applications: Vec<ApplicationId>,
  locations: Vec<DirectiveLocation>,
  externs: Vec<ExternRow>,
  /// Every generic definition's closed instantiations, one run per definition.
  instantiations: Vec<TyId>,
  /// Every second name, sorted by module, then the namespace of the definition it names, then
  /// name.
  aliases: Vec<AliasRow>,

  members: Vec<MemberRow>,
  member_applications: Vec<ApplicationId>,

  type_exprs: Vec<TypeExprRow>,
  type_expr_args: Vec<TypeExprId>,
  tys: Vec<TyRow>,
  ty_args: Vec<TyId>,

  values: Vec<ValueRow>,
  value_items: Vec<ValueId>,
  map_entries: Vec<(ValueId, ValueId)>,
  object_entries: Vec<ObjectEntry>,
  ints: Vec<IntRow>,
  floats: Vec<FloatRow>,
  literals: Vec<LiteralRow>,
  /// Every integer's magnitude, little-endian.
  magnitudes: Vec<u8>,

  applications: Vec<ApplicationRow>,
  application_type_args: Vec<TypeExprId>,
  arguments: Vec<ApplicationArgument>,

  schema: SchemaRow,
  builtins: BuiltinsRow,
  /// Every materialised primitive, in definition order.
  primitives: Vec<(Primitive, DefId)>,

  /// Every interned name, concatenated: the name arena.
  names: String,
  /// Each name's `(start, end)` in the name arena, indexed by [`Sym`].
  spans: Vec<(u32, u32)>,
  /// Every module path and every written path's segments.
  paths: Vec<Sym>,
  /// Every other spelling the model keeps: package names, descriptions, strings, literals and
  /// extern texts. The text arena.
  text: String,
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
  /// A model with no package and nothing in it: where the builder starts.
  fn empty() -> Self {
    Self {
      packages: Vec::new(),
      dependencies: Vec::new(),
      integers: Vec::new(),
      modules: Vec::new(),
      module_children: Vec::new(),
      defs: Vec::new(),
      contributions: Vec::new(),
      written_params: Vec::new(),
      type_params: Vec::new(),
      bounds: Vec::new(),
      implements: Vec::new(),
      union_members: Vec::new(),
      listed_applications: Vec::new(),
      locations: Vec::new(),
      externs: Vec::new(),
      instantiations: Vec::new(),
      aliases: Vec::new(),
      members: Vec::new(),
      member_applications: Vec::new(),
      type_exprs: Vec::new(),
      type_expr_args: Vec::new(),
      tys: Vec::new(),
      ty_args: Vec::new(),
      values: Vec::new(),
      value_items: Vec::new(),
      map_entries: Vec::new(),
      object_entries: Vec::new(),
      ints: Vec::new(),
      floats: Vec::new(),
      literals: Vec::new(),
      magnitudes: Vec::new(),
      applications: Vec::new(),
      application_type_args: Vec::new(),
      arguments: Vec::new(),
      schema: SchemaRow::default(),
      builtins: BuiltinsRow::default(),
      primitives: Vec::new(),
      names: String::new(),
      spans: Vec::new(),
      paths: Vec::new(),
      text: String::new(),
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

  /// How many members, type occurrences, values and applications it holds, in that order: the
  /// tables its public views answer no whole range of.
  #[cfg(feature = "test-support")]
  pub(crate) fn counts(&self) -> (u32, u32, u32, u32) {
    (
      dense(self.members.len()),
      dense(self.type_exprs.len()),
      dense(self.values.len()),
      dense(self.applications.len()),
    )
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
    let model: &'a Model = self.model;
    self.row().name.text(&model.text)
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
    self.row().dependencies.of(&model.dependencies)
  }

  /// The integer provider it selected, if any: where the host type of each integer width it writes
  /// beyond `core`'s comes from.
  #[inline]
  pub fn integers(&self) -> Option<&'a IntegerProvider> {
    let model: &'a Model = self.model;
    let index = self.row().integers?;
    model.integers.get(index as usize)
  }

  /// Its root module: `~::` inside it, and `^::name` from a package that reaches it.
  #[inline]
  pub fn root_module(&self) -> ModuleId {
    self.row().root_module
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

// What the whole design rests on, checked by the compiler rather than argued: the model and every
// view over it may cross threads. A `Cell`, an `Rc` or a raw pointer in any table would fail this.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<Model>;
  let _ = assert_send_sync::<PackageInfo<'static>>;
  let _ = assert_send_sync::<Module<'static>>;
  let _ = assert_send_sync::<Def<'static>>;
  let _ = assert_send_sync::<Contribution<'static>>;
  let _ = assert_send_sync::<Contributions<'static>>;
  let _ = assert_send_sync::<TypeParam<'static>>;
  let _ = assert_send_sync::<TypeParams<'static>>;
  let _ = assert_send_sync::<Extern<'static>>;
  let _ = assert_send_sync::<Externs<'static>>;
  let _ = assert_send_sync::<Member<'static>>;
  let _ = assert_send_sync::<Description<'static>>;
  let _ = assert_send_sync::<TypeExpr<'static>>;
  let _ = assert_send_sync::<TypeExprKind<'static>>;
  let _ = assert_send_sync::<Ty<'static>>;
  let _ = assert_send_sync::<TyKind<'static>>;
  let _ = assert_send_sync::<Path<'static>>;
  let _ = assert_send_sync::<Value<'static>>;
  let _ = assert_send_sync::<ValueKind<'static>>;
  let _ = assert_send_sync::<IntValue<'static>>;
  let _ = assert_send_sync::<FloatValue<'static>>;
  let _ = assert_send_sync::<Literal<'static>>;
  let _ = assert_send_sync::<Application<'static>>;
  let _ = assert_send_sync::<Schema<'static>>;
  let _ = assert_send_sync::<Builtins<'static>>;
  let _ = assert_send_sync::<CanonicalName<'static>>;
  let _ = assert_send_sync::<ModelBuilder>;
};
