//! The model's builder, and analyses of the models it builds, for tests outside this crate.
//!
//! Behind the `test-support` feature, and hidden even then. `pub` is forced only because `smear`'s
//! integration tests are separate crates: they build small models through the builder the analysis
//! writes its own with, and walk them as a consumer walks an analysis. It is not API, and it
//! changes with the builder.

pub use super::model::ModelBuilder;

use tokora::diagnostic::{Code, Location};

use super::{
  Visitor, analysis::Analysis, id::ModuleId, limits::Limits, model::Model, rules::Rules,
};

/// An analysis of the finished `model`: what [`conclude`] answers when no rule runs.
pub fn analysis_of(model: Model, limits: &Limits) -> Analysis {
  Analysis::of_model(model, limits, &mut ())
}

/// The analysis `analyze` concludes from the finished `model` when `rules` run in place of smear's
/// own rule set, under the diagnostic ceiling of `limits`, which [`visit`](super::visit) keeps to
/// as well.
///
/// The marks the builder applied are the phases': the rules read them as they walk every supplied
/// package, and the errors the rules report mark the model once the walk has ended. The analysis's
/// diagnostics are the rules'.
///
/// A model built here has no texts, so each source of the analysis's map has the package the model
/// gives it, its package source first, and every other one is a file with no path and no length.
pub fn conclude<V: Visitor + ?Sized>(model: Model, limits: &Limits, rules: &mut V) -> Analysis {
  Analysis::of_model(model, limits, rules)
}

/// smear's own rules, each by its type's name with the codes it reports: the table the liveness
/// test reads.
pub const RULES: &[(&str, &[Code])] = Rules::TABLE;

/// The extent of the inline `mod` that declares `module`, from its description, or its `mod`, to
/// its `}`: what the model keeps crate-private until extents are public, for the tests that check
/// what the module tree records.
pub fn module_extent(model: &Model, module: ModuleId) -> Option<Location> {
  model.module(module).extent()
}
