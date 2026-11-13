//! The model's builder, and analyses of the models it builds, for tests outside this crate.
//!
//! Behind the `test-support` feature, and hidden even then. `pub` is forced only because `smear`'s
//! integration tests are separate crates: they build small models through the builder the analysis
//! writes its own with, and walk them as a consumer walks an analysis. It is not API, and it
//! changes with the builder.

pub use super::model::ModelBuilder;

use std::{
  format,
  string::{String, ToString},
  vec::Vec,
};

use tokora::diagnostic::{Code, Location};

use super::{
  Visitor,
  analysis::{Analysis, resolve_door},
  id::ModuleId,
  limits::Limits,
  model::Model,
  package::Package,
  resolve::Resolved,
  rules::Rules,
  text::SourceText,
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

/// How many members, type occurrences, values and applications `model` holds, in that order: the
/// lengths of the four tables whose ids the public model answers no whole range of, for the tests
/// that check a walk reaches every row of each.
pub fn counts(model: &Model) -> (u32, u32, u32, u32) {
  model.counts()
}

/// The extent of every contribution of `model`, each definition's and then the schema's, and then
/// of every member, each from its description, or its first token, to its last token: what the
/// model keeps crate-private until extents are public, for the tests that check which extents
/// hold where a diagnostic points.
pub fn extents(model: &Model) -> Vec<Location> {
  model.extents()
}

/// What `path` names, written in the module `module` of `root` beside `dependencies`, where
/// `params` are the generic parameters in scope: the one door to the resolver the analysis builds,
/// asked as lowering asks it.
///
/// `module` is spelled as `Model::module_by_path` takes it, `^::package` then the names of the
/// modules down from its root. `path` is spelled as written, `~::`, `^::` and `..::` included; `@`
/// before it asks for a directive, `mod ` before it for a module, and anything else for a type.
///
/// The answer is a definition's canonical name, `^::core::NonZero<^::core::u32>` for a `NonZero`
/// alias, `mod ` and the module's path for a module, `param ` and its name for a generic
/// parameter, or `None` when `module` names no module or `path` resolves to nothing. Why it does
/// not resolve is reported nowhere: the analysis's diagnostics say that.
pub fn resolve<S: SourceText>(
  root: &Package<S>,
  dependencies: &[Package<S>],
  module: &str,
  params: &[&str],
  path: &str,
) -> Option<String> {
  let spelled: Vec<&[u8]> = params.iter().map(|param: &&str| param.as_bytes()).collect();
  let (model, answer) = resolve_door(root, dependencies, module, &spelled, path)?;
  match answer {
    Resolved::Def(def) => Some(model.canonical_name(def).to_string()),
    Resolved::NonZero {
      non_zero,
      primitive,
    } => Some(format!(
      "{}<{}>",
      model.canonical_name(non_zero),
      model.canonical_name(primitive)
    )),
    Resolved::Module(id) => {
      let module = model.module(id);
      let mut text = format!("mod ^::{}", model.package(module.package()).name());
      for segment in module.path() {
        text.push_str("::");
        text.push_str(model.name(*segment));
      }
      Some(text)
    }
    Resolved::Param(index) => params
      .get(index as usize)
      .map(|param: &&str| format!("param {param}")),
    Resolved::Unresolved => None,
  }
}
