//! What a phase after lowering reads of the rows written so far.
//!
//! Every accessor answers by value, or by a borrow of the builder, and takes an id the builder
//! issued. For an id it did not issue it answers `None`, 0 or nothing: it never indexes past a
//! table.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{ApplicationId, DefId, ModuleId, Sym, TyId, TypeExprId, dense},
  model::{
    Collection, DefKind, Resolution, TypeBound,
    satisfy::{Keys, Steps, Wanted},
    table::{DefRow, KIND_LIST, KIND_NAMED, KIND_PARAM, TyRow, UNSET_TY},
  },
};

use super::{ModelBuilder, type_key};

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
