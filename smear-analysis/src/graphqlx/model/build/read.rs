//! What a phase after lowering reads of the rows written so far.
//!
//! Every accessor answers by value, or by a borrow of the builder, and takes an id the builder
//! issued. For an id it did not issue it answers `None`, 0 or nothing: it never indexes past a
//! table.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{
    ApplicationId, DefId, IdRange, MemberId, ModuleId, PackageId, Sym, TyId, TypeExprId, ValueId,
    dense,
  },
  model::{
    ApplicationArgument, Collection, DefKind, Origin, Resolution, TypeBound,
    satisfy::{Keys, Steps, Wanted},
    table::{
      DefRow, ExternRow, KIND_LIST, KIND_NAMED, KIND_PARAM, MemberRow, PackageRow, TyRow, UNSET_TY,
      VALUE_ENUM, VALUE_NULL, VALUE_STRING,
    },
  },
  package::Tier,
};

use super::{ModelBuilder, Text, type_key};

/// What a value stores, as a phase after coercion reads it.
#[derive(Clone, Copy)]
pub(crate) enum Stored {
  /// `null`.
  Null,
  /// A string: its cooked text, the run coercion kept.
  String(Text),
  /// A value of an enum: the value.
  Enum(MemberId),
  /// Anything else, an unresolved value among them.
  Other,
}

/// What a type is, as a phase after instantiation reads it.
#[derive(Clone, Copy)]
pub(crate) enum Shape {
  /// A named type of this definition.
  Named(DefId),
  /// One of `def`'s type parameters, the one at `index`.
  Param { def: DefId, index: u32 },
  /// A list of this element.
  List(TyId),
  /// The one unresolved type.
  Unresolved,
}

/// What a type occurrence names, as lowering resolved it.
#[derive(Clone, Copy)]
pub(crate) enum Written {
  /// A definition: `core`'s `NonZero` for an alias such as `NonZeroU32`, which `non_zero_of`
  /// tells apart.
  Named(DefId),
  /// One of `def`'s type parameters: the one at `index`, which an extension's name past its
  /// target's parameters leaves past them.
  Param { def: DefId, index: u32 },
  /// A list of its element.
  List(TypeExprId),
  /// A path that did not resolve.
  Unresolved,
}

/// One type occurrence, as a phase after lowering reads it.
pub(crate) struct Occurrence<'b> {
  /// What it names.
  pub(crate) written: Written,
  /// The arguments written for it, in order, whatever it names.
  pub(crate) args: &'b [TypeExprId],
  /// Whether it is written non-null, with `!`.
  pub(crate) non_null: bool,
  /// Where it is written.
  pub(crate) location: Location,
  /// Its type: `None` until a phase gives it one.
  pub(crate) ty: Option<TyId>,
}

impl ModelBuilder {
  /// How many definitions the model holds.
  pub(crate) fn def_count(&self) -> u32 {
    dense(self.model.defs.len())
  }

  /// What kind of definition `def` is.
  pub(crate) fn def_kind(&self, def: DefId) -> Option<DefKind> {
    self
      .model
      .defs
      .get(def.get() as usize)
      .map(|row: &DefRow| row.kind)
  }

  /// The module `def` belongs to. A definition the root package replaces a GraphQL built-in with
  /// belongs to the built-in's module, though it is written in the root package's root module.
  pub(crate) fn def_module(&self, def: DefId) -> Option<ModuleId> {
    self
      .model
      .defs
      .get(def.get() as usize)
      .map(|row: &DefRow| row.module)
  }

  /// The name of `def`: nothing for an id the builder did not issue.
  pub(crate) fn def_name(&self, def: DefId) -> &[u8] {
    match self.model.defs.get(def.get() as usize) {
      Some(row) => self.names.bytes(row.name),
      None => &[],
    }
  }

  /// The collection `def` stands for, once built-in meaning is given.
  pub(crate) fn def_collection(&self, def: DefId) -> Option<Collection> {
    self
      .model
      .defs
      .get(def.get() as usize)
      .and_then(|row: &DefRow| row.collection)
  }

  /// The applications of `def`, every contribution's, in order.
  pub(crate) fn def_applications(&self, def: DefId) -> &[ApplicationId] {
    match self.model.defs.get(def.get() as usize) {
      Some(row) => row.applications.of(&self.model.listed_applications),
      None => &[],
    }
  }

  /// The direct members of `def`, in id order: a directive's are its arguments.
  pub(crate) fn def_members(&self, def: DefId) -> IdRange<MemberId> {
    self
      .model
      .defs
      .get(def.get() as usize)
      .map_or(IdRange::new(0, 0), |row: &DefRow| row.members)
  }

  /// The arguments of `member`, a field: none for a member of any other kind.
  pub(crate) fn member_arguments(&self, member: MemberId) -> IdRange<MemberId> {
    self
      .model
      .members
      .get(member.get() as usize)
      .map_or(IdRange::new(0, 0), |row: &MemberRow| row.arguments)
  }

  /// The name of `member`.
  pub(crate) fn member_name(&self, member: MemberId) -> Option<Sym> {
    self
      .model
      .members
      .get(member.get() as usize)
      .map(|row: &MemberRow| row.name)
  }

  /// Where `member`'s name is written.
  pub(crate) fn member_location(&self, member: MemberId) -> Option<Location> {
    self
      .model
      .members
      .get(member.get() as usize)
      .map(|row: &MemberRow| row.location)
  }

  /// How many values the model holds.
  pub(crate) fn value_count(&self) -> u32 {
    dense(self.model.values.len())
  }

  /// The type occurrence `member`'s type is written as: none for an enum value.
  pub(crate) fn member_ty(&self, member: MemberId) -> Option<TypeExprId> {
    self
      .model
      .members
      .get(member.get() as usize)
      .and_then(|row: &MemberRow| row.ty)
  }

  /// The applications of `member`, in order.
  pub(crate) fn member_applications(&self, member: MemberId) -> &[ApplicationId] {
    match self.model.members.get(member.get() as usize) {
      Some(row) => row.applications.of(&self.model.member_applications),
      None => &[],
    }
  }

  /// The schema's applications, every contribution's, in order.
  pub(crate) fn schema_applications(&self) -> &[ApplicationId] {
    self
      .model
      .schema
      .applications
      .of(&self.model.listed_applications)
  }

  /// How many type parameters `def` has: 0 for an id the builder did not issue.
  pub(crate) fn type_param_count(&self, def: DefId) -> u32 {
    self
      .model
      .defs
      .get(def.get() as usize)
      .map_or(0, |row: &DefRow| row.type_params.len)
  }

  /// `def`'s type parameter at `index`: its name, where its name is written, and the occurrence
  /// its default is, if it has one.
  pub(crate) fn type_param(
    &self,
    def: DefId,
    index: u32,
  ) -> Option<(Sym, Location, Option<TypeExprId>)> {
    let row = self.model.defs.get(def.get() as usize)?;
    if index >= row.type_params.len {
      return None;
    }
    let at = row.type_params.start.checked_add(index)?;
    let param = self.model.type_params.get(at as usize)?;
    Some((param.name, param.location, param.default))
  }

  /// How many directive applications the model holds.
  pub(crate) fn application_count(&self) -> u32 {
    dense(self.model.applications.len())
  }

  /// The application `id`: the directive it resolved to, how many type arguments it writes, and
  /// where its directive's path is written.
  pub(crate) fn applied(&self, id: ApplicationId) -> Option<(Resolution<DefId>, u32, Location)> {
    let row = self.model.applications.get(id.get() as usize)?;
    Some((row.directive, row.type_args.len, row.path.location))
  }

  /// The type of the application `id`, once a phase gave it one: its directive's instantiation,
  /// whose arguments are its complete type arguments, or the unresolved type.
  pub(crate) fn application_ty(&self, id: ApplicationId) -> Option<TyId> {
    let row = self.model.applications.get(id.get() as usize)?;
    (row.ty != UNSET_TY).then_some(row.ty)
  }

  /// The type arguments the application `id` writes, in order: none for an id the builder did not
  /// issue.
  pub(crate) fn application_type_args(&self, id: ApplicationId) -> &[TypeExprId] {
    match self.model.applications.get(id.get() as usize) {
      Some(row) => row.type_args.of(&self.model.application_type_args),
      None => &[],
    }
  }

  /// The arguments of the application `id`, in its directive's order: none for an id the builder
  /// did not issue.
  pub(crate) fn application_arguments(&self, id: ApplicationId) -> &[ApplicationArgument] {
    match self.model.applications.get(id.get() as usize) {
      Some(row) => row.arguments.of(&self.model.arguments),
      None => &[],
    }
  }

  /// What the value `value` stores, once coercion made it: `Other` for an id the builder did not
  /// issue.
  pub(crate) fn value_stored(&self, value: ValueId) -> Stored {
    match self.model.values.get(value.get() as usize) {
      Some(row) => match row.shape {
        VALUE_NULL => Stored::Null,
        VALUE_STRING => Stored::String(Text(row.run)),
        VALUE_ENUM => Stored::Enum(row.member),
        _ => Stored::Other,
      },
      None => Stored::Other,
    }
  }

  /// The tier `package` is declared at.
  pub(crate) fn package_tier(&self, package: PackageId) -> Option<Tier> {
    self
      .model
      .packages
      .get(package.get() as usize)
      .map(|row: &PackageRow| row.tier)
  }

  /// The tier `def` requires, as written so far: a built-in's tag, which no phase recomputes.
  pub(crate) fn def_tier(&self, def: DefId) -> Option<Tier> {
    self
      .model
      .defs
      .get(def.get() as usize)
      .map(|row: &DefRow| row.tier)
  }

  /// The greatest of the tiers of `def`'s externs: `None` when it has none.
  pub(crate) fn def_extern_tier(&self, def: DefId) -> Option<Tier> {
    let row = self.model.defs.get(def.get() as usize)?;
    row
      .externs
      .of(&self.model.externs)
      .iter()
      .map(|bound: &ExternRow| bound.tier)
      .max()
  }

  /// Whether `def` is a marker, once built-in meaning is given: `false` for an id the builder did
  /// not issue.
  pub(crate) fn def_marker(&self, def: DefId) -> bool {
    self
      .model
      .defs
      .get(def.get() as usize)
      .is_some_and(|row: &DefRow| row.marker)
  }

  /// The `implements` entries of `def`, every contribution's, in order: none for a definition that
  /// writes none, and for an id the builder did not issue.
  pub(crate) fn def_implements(&self, def: DefId) -> &[TypeExprId] {
    match self.model.defs.get(def.get() as usize) {
      Some(row) => row.implements.of(&self.model.implements),
      None => &[],
    }
  }

  /// The union members of `def`, every contribution's, in order: none for a definition of any
  /// other kind, and for an id the builder did not issue.
  pub(crate) fn def_union_members(&self, def: DefId) -> &[TypeExprId] {
    match self.model.defs.get(def.get() as usize) {
      Some(row) => row.union_members.of(&self.model.union_members),
      None => &[],
    }
  }

  /// Where `def` comes from.
  pub(crate) fn def_origin(&self, def: DefId) -> Option<Origin> {
    self
      .model
      .defs
      .get(def.get() as usize)
      .map(|row: &DefRow| row.origin)
  }

  /// How many type occurrences the model holds.
  pub(crate) fn type_expr_count(&self) -> u32 {
    dense(self.model.type_exprs.len())
  }

  /// The type occurrence `id`, as lowering wrote it, with the type a phase gave it.
  pub(crate) fn occurrence(&self, id: TypeExprId) -> Option<Occurrence<'_>> {
    let row = self.model.type_exprs.get(id.get() as usize)?;
    let written = match row.shape {
      KIND_NAMED => Written::Named(row.def),
      KIND_PARAM => Written::Param {
        def: row.def,
        index: row.index,
      },
      KIND_LIST => Written::List(row.element),
      _ => Written::Unresolved,
    };
    Some(Occurrence {
      written,
      args: row.args.of(&self.model.type_expr_args),
      non_null: row.non_null,
      location: row.location,
      ty: (row.ty != UNSET_TY).then_some(row.ty),
    })
  }

  /// The primitive whose `NonZero` the occurrence `id` names, when it is an alias such as
  /// `NonZeroU32`, as lowering noted it: one binary search of the notes.
  pub(crate) fn non_zero_of(&self, id: TypeExprId) -> Option<DefId> {
    let at = self
      .non_zero
      .binary_search_by_key(&id, |&(noted, _): &(TypeExprId, DefId)| noted)
      .ok()?;
    self
      .non_zero
      .get(at)
      .map(|&(_, primitive): &(TypeExprId, DefId)| primitive)
  }

  /// How many types the model holds.
  pub(crate) fn ty_count(&self) -> u32 {
    dense(self.model.tys.len())
  }

  /// How deeply `ty` nests: 0 for a type the builder did not issue.
  pub(crate) fn ty_depth(&self, ty: TyId) -> u32 {
    self
      .model
      .tys
      .get(ty.get() as usize)
      .map_or(0, |row: &TyRow| row.depth)
  }

  /// Whether `ty` names no type parameter.
  pub(crate) fn ty_closed(&self, ty: TyId) -> bool {
    self
      .model
      .tys
      .get(ty.get() as usize)
      .is_some_and(|row: &TyRow| row.closed)
  }

  /// What `ty` is: `None` for a type the builder did not issue.
  pub(crate) fn ty_shape(&self, ty: TyId) -> Option<Shape> {
    let row = self.model.tys.get(ty.get() as usize)?;
    Some(match row.shape {
      KIND_NAMED => Shape::Named(row.def),
      KIND_PARAM => Shape::Param {
        def: row.def,
        index: row.index,
      },
      KIND_LIST => Shape::List(row.element),
      _ => Shape::Unresolved,
    })
  }

  /// Every argument of `ty`, a named type: none for a type of any other shape, and for one the
  /// builder did not issue.
  pub(crate) fn ty_args(&self, ty: TyId) -> &[TyId] {
    match self.model.tys.get(ty.get() as usize) {
      Some(row) if row.shape == KIND_NAMED => row.args.of(&self.model.ty_args),
      _ => &[],
    }
  }

  /// Whether `ty` is non-null: `false` for a type the builder did not issue.
  pub(crate) fn ty_non_null(&self, ty: TyId) -> bool {
    self
      .model
      .tys
      .get(ty.get() as usize)
      .is_some_and(|row: &TyRow| row.non_null)
  }

  /// The bounds of `def`, in the order they are written: none for an id the builder did not
  /// issue.
  pub(crate) fn def_bounds(&self, def: DefId) -> &[TypeBound] {
    match self.model.defs.get(def.get() as usize) {
      Some(row) => row.bounds.of(&self.model.bounds),
      None => &[],
    }
  }

  /// Whether `ty` satisfies the interface type `interface`, read off the rows written so far, by
  /// the routine the finished model answers `Model::satisfies_ty` with, keyed on `keys`; `None`
  /// when `steps` ran out before it was known.
  pub(crate) fn judge(
    &self,
    keys: &Keys,
    ty: TyId,
    interface: TyId,
    steps: &mut Steps,
  ) -> Option<bool> {
    self.model.judge(keys, ty, Wanted::Type(interface), steps)
  }

  /// The named type of `def` with every argument `args`, non-null when `non_null`, when it is
  /// interned already: found by the key interning makes, and interning nothing.
  pub(crate) fn find_named(&self, def: DefId, args: &[TyId], non_null: bool) -> Option<TyId> {
    let key = type_key(KIND_NAMED, non_null, def, 0, TyId::new(0), args);
    self.types.get(key.as_slice()).copied()
  }
}
