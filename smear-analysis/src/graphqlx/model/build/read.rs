//! What a phase after lowering reads of the rows written so far.
//!
//! Every accessor answers by value, or by a borrow of the builder, and takes an id the builder
//! issued. For an id it did not issue it answers `None`, 0 or nothing: it never indexes past a
//! table.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{ApplicationId, DefId, ModuleId, Sym, TypeExprId, dense},
  model::{
    DefKind, Resolution,
    table::{DefRow, KIND_LIST, KIND_NAMED, KIND_PARAM},
  },
};

use super::ModelBuilder;

/// What a type occurrence names, as lowering resolved it.
#[derive(Clone, Copy)]
pub(crate) enum Written {
  /// A definition.
  Named,
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
  /// Where it is written.
  pub(crate) location: Location,
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

  /// The type occurrence `id`, as lowering wrote it.
  pub(crate) fn occurrence(&self, id: TypeExprId) -> Option<Occurrence<'_>> {
    let row = self.model.type_exprs.get(id.get() as usize)?;
    let written = match row.shape {
      KIND_NAMED => Written::Named,
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
      location: row.location,
    })
  }

  /// The application `id`: the directive it resolved to, how many type arguments it writes, and
  /// where its directive's path is written.
  pub(crate) fn applied(&self, id: ApplicationId) -> Option<(Resolution<DefId>, u32, Location)> {
    let row = self.model.applications.get(id.get() as usize)?;
    Some((row.directive, row.type_args.len, row.path.location))
  }
}
