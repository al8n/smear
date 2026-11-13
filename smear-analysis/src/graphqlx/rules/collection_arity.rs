//! A set has one type parameter, and a map two.
//!
//! `core`'s `@set` makes a scalar a set of the one type its type parameter is, and `@map` makes a
//! scalar a map from its first type parameter to its second. The applications judged are the ones
//! that give a scalar that meaning: those the package that wrote the scalar writes, in its
//! definition or in an extension of its own, told by the one test instantiation asks before it
//! gives the meaning, whether the package that wrote the application, `SourceMap::written_by` of
//! its path, is the package that wrote the definition, `SourceMap::written_by` of its own
//! contribution. So the root package's own `scalar String`, which replaces the built-in, is
//! judged with the `@set` the root package adds to it; an application another package writes
//! gives no meaning and is the orphan rule's alone; and a built-in package's scalar is the
//! language's, and not judged.
//!
//! Of those applications, in application order:
//!
//! - a scalar that applies both `@set` and `@map` is reported once, at the later of the first of
//!   each, and nothing is said of its type parameters;
//! - otherwise, the first `@set` of a scalar that has not exactly one type parameter, or the first
//!   `@map` of one that has not exactly two, is reported at its path, with a label at the scalar's
//!   name.
//!
//! Each is `smear::graphqlx::collection-arity`. Where `@set` and `@map` may stand, on a scalar
//! alone, is the directives' own locations, and not this rule's.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  ApplicationId, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, PackageKind, Resolution, Visitor,
  diagnostic::Excerpt,
};

use super::own_contribution;

/// Reports each scalar its own package makes a set or a map with a count of type parameters that
/// collection has not, and each that its own package makes both.
///
/// It reads a definition's contributions and their applications in the `enter_def` hook. It says
/// nothing about an application in a contribution in which a phase reported an error, and
/// nothing about a built-in package's scalar.
///
/// It costs one look at each definition the walk enters, and, for a scalar of a supplied
/// package, one at each application its contributions hold; it keeps nothing between two walks.
/// Its messages quote the scalar's name as an excerpt, so each is bounded whatever its length, and
/// give counts as numbers.
#[derive(Default)]
pub(super) struct CollectionArity;

/// The first application of a collection's directive, and whether a phase reported an error in
/// the contribution that holds it.
type First = Option<(ApplicationId, bool)>;

impl Visitor for CollectionArity {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let sources = cx.sources();
    let definition = model.def(def);
    if definition.kind() != DefKind::Scalar {
      return;
    }
    // The package that wrote the scalar.
    let Some((_, own)) = own_contribution(cx, def) else {
      return;
    };
    if model.package(own).kind() == PackageKind::BuiltIn {
      return;
    }
    let builtins = model.builtins();
    let (set, map) = (builtins.set(), builtins.map());
    let mut first_set: First = None;
    let mut first_map: First = None;
    // The later of the two firsts, once both are met.
    let mut later: First = None;
    for part in definition.contributions() {
      for &application in part.applications() {
        let applied = model.application(application);
        // An application another package wrote gives the scalar no meaning.
        if sources.written_by(applied.path_location()) != Some(own) {
          continue;
        }
        let Resolution::Resolved(directive) = applied.directive() else {
          continue;
        };
        let met = Some((application, part.has_errors()));
        if directive == set && first_set.is_none() {
          first_set = met;
          if first_map.is_some() {
            later = met;
          }
        } else if directive == map && first_map.is_none() {
          first_map = met;
          if first_set.is_some() {
            later = met;
          }
        }
      }
    }
    let params = definition.type_params().len();
    let (at, message, help) = match (later, first_set, first_map) {
      (Some(both), _, _) => (
        both,
        format!(
          "the scalar `{}` applies both `@set` and `@map`, and a scalar is a set or a map, not \
           both",
          Excerpt(definition.name().as_bytes())
        ),
        "remove `@set` or `@map`: a set of one type parameter, or a map of two",
      ),
      (None, Some(first), None) if params != 1 => (
        first,
        format!(
          "the scalar `{}` applies `@set`, which makes a scalar of 1 type parameter a set, and it \
           declares {params}",
          Excerpt(definition.name().as_bytes())
        ),
        "declare one type parameter, the set's element, as `scalar Tags<T> @set` does",
      ),
      (None, None, Some(first)) if params != 2 => (
        first,
        format!(
          "the scalar `{}` applies `@map`, which makes a scalar of 2 type parameters a map, and it \
           declares {params}",
          Excerpt(definition.name().as_bytes())
        ),
        "declare two type parameters, the map's key and its value, as `scalar Index<K, V> @map` \
         does",
      ),
      _ => return,
    };
    let (application, refused) = at;
    if refused {
      return;
    }
    let kind = DiagnosticKind::CollectionArity;
    let mut report =
      Diagnostic::builder(kind.code(), model.application(application).path_location())
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help(help);
    // A scalar a supplied package writes has a position of its own.
    if !definition.location().is_entire() {
      report = report.label(definition.location(), "the scalar");
    }
    cx.report(report.build());
  }
}
