//! What a phase after lowering reads of the rows written so far.
//!
//! Every accessor answers by value, or by a borrow of the builder, and takes an id the builder
//! issued. For an id it did not issue it answers `None`, 0 or nothing: it never indexes past a
//! table.

use crate::graphqlx::{
  id::{DefId, ModuleId},
  model::table::DefRow,
};

use super::ModelBuilder;

impl ModelBuilder {
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

  /// How many type parameters `def` has: 0 for an id the builder did not issue.
  pub(crate) fn type_param_count(&self, def: DefId) -> u32 {
    self
      .model
      .defs
      .get(def.get() as usize)
      .map_or(0, |row: &DefRow| row.type_params.len)
  }
}
