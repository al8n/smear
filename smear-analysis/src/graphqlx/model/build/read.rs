//! What a phase after lowering reads of the rows written so far.
//!
//! Every accessor answers by value, or by a borrow of the builder, and takes an id the builder
//! issued. For an id it did not issue it answers `None`, 0 or nothing: it never indexes past a
//! table.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{ApplicationId, DefId, ModuleId, dense},
  model::{DefKind, Resolution, table::DefRow},
};

use super::ModelBuilder;

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

  /// The application `id`: the directive it resolved to, how many type arguments it writes, and
  /// where its directive's path is written.
  pub(crate) fn applied(&self, id: ApplicationId) -> Option<(Resolution<DefId>, u32, Location)> {
    let row = self.model.applications.get(id.get() as usize)?;
    Some((row.directive, row.type_args.len, row.path.location))
  }
}
