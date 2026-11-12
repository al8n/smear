//! The model's builder, and analyses of the models it builds, for tests outside this crate.
//!
//! Behind the `test-support` feature, and hidden even then. `pub` is forced only because `smear`'s
//! integration tests are separate crates: they build small models through the builder the analysis
//! writes its own with, and walk them as a consumer walks an analysis. It is not API, and it
//! changes with the builder.

pub use super::model::ModelBuilder;

use super::{analysis::Analysis, limits::Limits, model::Model};

/// An analysis of the finished `model`: no diagnostic, a source map of the model's packages'
/// sources, and the diagnostic ceiling of `limits`, which [`visit`](super::visit) keeps to.
///
/// A model built here has no texts, so each source of the map has the package the model gives it,
/// its package source first, and every other one is a file with no path and no length.
pub fn analysis_of(model: Model, limits: &Limits) -> Analysis {
  Analysis::of_model(model, limits)
}
