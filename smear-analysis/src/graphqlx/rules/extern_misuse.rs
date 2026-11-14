//! `@extern` binds a scalar its own package defines, and a language binds it once.
//!
//! A scalar's externs are the `@extern`s its own package writes on it, as binding read them off
//! the applications that resolved to `core`'s `@extern`. Two that write one `lang` would give the
//! scalar two host types in one language: each extern whose `lang` an earlier one of its scalar
//! has, the cooked strings compared bytewise, is reported at its application's path, with a label
//! at the earlier one's, as `smear::graphqlx::extern-misuse`. `"rust"` and `"rust"` are one
//! language, and `"rust"` and `"Rust"` two. The earlier one is the first of its `lang` whether or
//! not a phase refused something in its contribution; a later one in a contribution a phase
//! refused something in is not reported.
//!
//! A GraphQL built-in the root package replaces keeps its built-in's identity, and binding binds it
//! to nothing: a built-in's host type is given by `@repr(for: …)` on the schema alone. So each
//! application of `@extern` on it that the root package writes, in its own definition or in an
//! extension of its own, is `smear::graphqlx::extern-misuse` too, at its path, unless a phase
//! refused something in the contribution that holds it. A dependency's extension of it is the
//! orphan rule's whole, and so is the root package's extension of a built-in it did not write.
//!
//! Nothing else is judged here: an `@extern` on what is no scalar stands where `@extern` may not,
//! which is not this rule's to say; and one whose values did not coerce binds nothing, coercion
//! having reported it within the contribution, where this rule says nothing.
//!
//! The rule reads the public model, the public `SourceMap::written_by` among it, the package that
//! wrote a contribution, and one crate-private accessor, `Extern::application`: the application
//! an extern is bound from.

use std::{format, vec::Vec};

use smear_semantics::duplicates::Duplicates;

use crate::graphqlx::{
  ApplicationId, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, Extern, Origin, PackageId,
  PackageKind, Resolution, Visitor, diagnostic::Excerpt,
};

use super::own_contribution;

/// Reports each extern of a scalar whose `lang` an earlier extern of the scalar has, and each
/// `@extern` the root package writes on a GraphQL built-in its own definition replaces.
///
/// It reads a scalar's externs, its contributions and their applications in the `enter_def` hook.
/// It says nothing about an extern or an application in a contribution a phase refused something
/// in, nor about a scalar a built-in package wrote.
///
/// It costs one look at each definition the walk enters; for a scalar of two externs or more, one
/// pass over its contributions' applications, which pairs each extern with the contribution that
/// holds its application, and one search of the `lang`s by the shared kernel, a scan up to
/// sixty-four and one sort above, in a list it makes for that scalar alone; and for a GraphQL
/// built-in the root package replaces, one look at each application its own package writes on it.
/// It keeps nothing between two walks. Its messages quote the scalar's name and a `lang` as
/// excerpts, so each is bounded whatever their lengths.
#[derive(Default)]
pub(super) struct ExternMisuse;

impl Visitor for ExternMisuse {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let definition = model.def(def);
    if definition.kind() != DefKind::Scalar {
      return;
    }
    // The package that wrote the scalar: the root package for a GraphQL built-in it replaces.
    let Some((_, own)) = own_contribution(cx, def) else {
      return;
    };
    if model.package(own).kind() == PackageKind::BuiltIn {
      return;
    }
    if definition.origin() == Origin::User {
      twice(cx, def);
    } else if definition.origin() == Origin::BuiltIn {
      replaced(cx, def, own);
    }
  }
}

/// Reports each extern of the scalar `def` whose `lang` an earlier one of its externs has, at its
/// application's path, with a label at the earlier one's.
fn twice(cx: &mut Cx<'_>, def: DefId) {
  let model = cx.model();
  let definition = model.def(def);
  if definition.externs().len() < 2 {
    return;
  }
  // Each extern bound from an application, with that application and whether a phase refused
  // something in the contribution that holds it. The externs are in application order, and so
  // are the contributions' applications, so one pass pairs them.
  let mut pending = definition.externs().peekable();
  let mut bound: Vec<(&str, ApplicationId, bool)> = Vec::with_capacity(definition.externs().len());
  for part in definition.contributions() {
    for &application in part.applications() {
      // An extern bound from no application, which only a hand-built model holds, is passed over.
      while pending
        .peek()
        .is_some_and(|next: &Extern<'_>| next.application().is_none())
      {
        pending.next();
      }
      if let Some(next) = pending.peek().copied()
        && next.application() == Some(application)
      {
        bound.push((next.lang(), application, part.has_errors()));
        pending.next();
      }
    }
  }
  let mut duplicates: Duplicates<&str> =
    Duplicates::over(bound.len(), |at: usize| lang_at(&bound, at));
  let kind = DiagnosticKind::ExternMisuse;
  for (at, &(lang, application, refused)) in bound.iter().enumerate() {
    // Every position is asked, in order, so that the kernel's scan and its sort agree.
    let Some(first) = duplicates.first(at, lang) else {
      continue;
    };
    let Some(&(_, earlier, _)) = bound.get(first) else {
      continue;
    };
    if refused {
      continue;
    }
    let name = Excerpt(definition.name().as_bytes());
    let spelled = Excerpt(lang.as_bytes());
    cx.report(
      Diagnostic::builder(kind.code(), model.application(application).path_location())
        .severity(kind.severity())
        .message(format!(
          "the scalar `{name}` is bound to a host type of `{spelled}` twice, and a language binds \
           a scalar once"
        ))
        .primary_label(kind.title())
        .label(
          model.application(earlier).path_location(),
          "first bound in this language here",
        )
        .help("write one `@extern` for each language: remove this one, or give it another `lang`")
        .build(),
    );
  }
}

/// The `lang` at `at` of `bound`, or `""` past its end.
fn lang_at<'k>(bound: &[(&'k str, ApplicationId, bool)], at: usize) -> &'k str {
  bound
    .get(at)
    .map_or("", |entry: &(&'k str, ApplicationId, bool)| entry.0)
}

/// Reports each `@extern` that `writer`, the root package, writes on `def`, a GraphQL built-in its
/// own definition replaces, in a contribution of its own, at its path.
fn replaced(cx: &mut Cx<'_>, def: DefId, writer: PackageId) {
  let model = cx.model();
  let sources = cx.sources();
  let definition = model.def(def);
  let host = model.builtins().r#extern();
  let kind = DiagnosticKind::ExternMisuse;
  for part in definition.contributions() {
    if part.has_errors() || sources.written_by(part.location()) != Some(writer) {
      continue;
    }
    for &application in part.applications() {
      let applied = model.application(application);
      if applied.directive() != Resolution::Resolved(host) {
        continue;
      }
      cx.report(
        Diagnostic::builder(kind.code(), applied.path_location())
          .severity(kind.severity())
          .message(format!(
            "this package's own definition replaces the GraphQL built-in `{}`, and `@extern` binds \
             a scalar its package defines: a built-in's host type is given by `@repr(for: …)` on \
             the schema",
            Excerpt(definition.name().as_bytes())
          ))
          .primary_label(kind.title())
          .help(
            "give the built-in its host type with `extend schema @repr(for: …, lang: …, type: \
             …)`, and remove this `@extern`",
          )
          .build(),
      );
    }
  }
}
