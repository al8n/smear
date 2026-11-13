//! A marker is used as a marker: it has no field, it is named only in `implements` and in bounds,
//! and it is what a scalar, an enum, an input object or a union implements.
//!
//! A marker is an interface its own package applies `core`'s `@marker` to. It says what a type
//! is and holds nothing, so:
//!
//! - it declares no field. One that declares any is reported once, at the first field its own
//!   package wrote, in the first of its contributions that writes one and that no phase refused,
//!   with a label at the marker's name. An extension another package wrote is the orphan rule's
//!   alone, and a built-in package's marker is the language's;
//! - it is named only as an `implements` entry or as a bound's interface, the outermost occurrence
//!   there. Named anywhere else it is reported where it is named: a member's type, a list's
//!   element, a type argument, of an `implements` entry or a bound's interface too, a union member,
//!   a type parameter's default, a root, an application's type argument;
//! - a scalar, an enum, an input object and a union implement markers alone: an entry of their
//!   `implements` that names what is no marker, a type parameter among them, is reported where it
//!   is written. An object or an interface may implement any interface, and whether what it names
//!   is one is GraphQL's own rule.
//!
//! A marker that takes type parameters is a marker like any other.
//!
//! Each is `smear::graphqlx::marker-misuse`. The rule reads the public model, and one crate-private
//! accessor through `Nesting`: `TypeExpr::written_args`, to tell an outermost occurrence.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, PackageKind, Site, TypeExprId,
  TypeExprKind, Visitor, diagnostic::Excerpt,
};

use super::{Nesting, Silence, own_contribution};

/// Reports each marker that declares a field, each occurrence of a marker that is no outermost
/// `implements` entry or bound's interface, and each `implements` entry of a scalar, an enum, an
/// input object or a union that names what is no marker, a type parameter among them.
///
/// It reads a definition in the `enter_def` hook and an occurrence in the `type_expr` hook. It
/// says nothing about a contribution in which a phase reported an error: of a marker's fields, it
/// reports only in a contribution no phase refused, and of an occurrence, only one whose
/// contribution, or for a root the schema's, no phase refused. It reports only what a supplied
/// package wrote: the walk reaches nothing else, and a built-in package's marker is not judged.
///
/// It costs one look at each definition the walk enters, and at each contribution of a marker, and
/// one at each occurrence, and one look at the schema's contributions for the whole walk; it keeps
/// a count and two flags between two occurrences, and nothing between two walks. Its messages
/// quote a definition's or a type parameter's name as an excerpt, so each is bounded whatever the
/// name's length.
#[derive(Default)]
pub(super) struct MarkerMisuse {
  /// Where each occurrence stands in its tree.
  nesting: Nesting,
  /// Whether it says nothing at an occurrence.
  silence: Silence,
}

impl Visitor for MarkerMisuse {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.nesting = Nesting::default();
    self.silence = Silence::of(cx);
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let sources = cx.sources();
    let definition = model.def(def);
    if !definition.is_marker() {
      return;
    }
    // The package that wrote the marker.
    let Some((_, own)) = own_contribution(cx, def) else {
      return;
    };
    if model.package(own).kind() == PackageKind::BuiltIn {
      return;
    }
    let first = definition
      .contributions()
      .filter(|part: &Contribution<'_>| {
        sources.written_by(part.location()) == Some(own)
          && !part.has_errors()
          && !part.members().is_empty()
      })
      .find_map(|part: Contribution<'_>| part.members().into_iter().next());
    let Some(first) = first else {
      return;
    };
    let kind = DiagnosticKind::MarkerMisuse;
    let mut report = Diagnostic::builder(kind.code(), model.member(first).location())
      .severity(kind.severity())
      .message(format!(
        "the marker `{}` declares a field, and a marker declares none: it says what a type is, \
         and holds nothing",
        Excerpt(definition.name().as_bytes())
      ))
      .primary_label(kind.title())
      .help("remove the marker's fields, or remove its `@marker` to make it an interface");
    if !definition.location().is_entire() {
      report = report.label(definition.location(), "the marker");
    }
    cx.report(report.build());
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    let model = cx.model();
    let root = self.nesting.reach(model, ty).root;
    if self.silence.silenced(cx) {
      return;
    }
    let occurrence = model.type_expr(ty);
    let site = cx.site();
    let kind = DiagnosticKind::MarkerMisuse;
    // The definition the occurrence names; none for a type parameter, which is no marker.
    let interface = match occurrence.kind() {
      TypeExprKind::Named { def: named, .. } => Some(model.def(named)),
      TypeExprKind::Param { .. } => None,
      _ => return,
    };
    if let Some(interface) = interface
      && interface.is_marker()
    {
      if root && matches!(site, Some(Site::Implements | Site::Bound)) {
        return;
      }
      cx.report(
        Diagnostic::builder(kind.code(), occurrence.location())
          .severity(kind.severity())
          .message(format!(
            "the marker `{}` is named here, and a marker is named only in an `implements` list \
             or as a bound's interface",
            Excerpt(interface.name().as_bytes())
          ))
          .primary_label(kind.title())
          .help(
            "a marker says what a type is, and holds no value: name it in an `implements` list or \
             in a `where` clause, and an interface or a type here",
          )
          .build(),
      );
      return;
    }
    // What a scalar, an enum, an input object or a union implements: markers, so neither another
    // interface nor a type parameter.
    if !root || site != Some(Site::Implements) {
      return;
    }
    let Some(owner) = cx.def() else {
      return;
    };
    let owner = model.def(owner);
    let (noun, any) = match owner.kind() {
      DefKind::Scalar => ("scalar", "a scalar"),
      DefKind::Enum => ("enum", "an enum"),
      DefKind::InputObject => ("input object", "an input object"),
      DefKind::Union => ("union", "a union"),
      _ => return,
    };
    let entry = match (interface, occurrence.kind()) {
      (Some(interface), _) => format!("`{}`", Excerpt(interface.name().as_bytes())),
      (None, TypeExprKind::Param { def, index }) => {
        let Some(param) = model.def(def).type_params().nth(index as usize) else {
          return;
        };
        format!("its type parameter `{}`", Excerpt(param.name().as_bytes()))
      }
      (None, _) => return,
    };
    cx.report(
      Diagnostic::builder(kind.code(), occurrence.location())
        .severity(kind.severity())
        .message(format!(
          "the {noun} `{}` implements {entry}, which is no marker, and {any} implements markers \
           alone",
          Excerpt(owner.name().as_bytes()),
        ))
        .primary_label(kind.title())
        .help(
          "implement a marker, an interface `@marker` makes one, or remove this entry from the \
           `implements` list",
        )
        .build(),
    );
  }
}
