//! An enum takes no type parameter.
//!
//! An enum's values are names, and none of them holds a type: a type parameter of an enum would be
//! named by nothing the enum writes, and each use of the enum would write a type argument that
//! changes nothing. So an enum that declares a type parameter is reported once, at its first type
//! parameter's name, as `smear::graphqlx::generic-enum`.
//!
//! The model keeps the enum's type parameters as written, and the phases treat the enum as any
//! generic definition: each use of it is counted and typed, so a use that leaves its argument out
//! is `generic-arity` where it is written, beside this rule's diagnostic at the definition.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, Visitor, diagnostic::Excerpt,
};

/// Reports each enum that declares a type parameter.
///
/// It reads a definition's kind and its type parameters in the `enter_def` hook. It says nothing
/// about an enum whose own contribution, its first, has its `has_errors()` set: a phase already
/// refused something in it, a default of one of its type parameters among what it holds, and a
/// second diagnostic within it would repeat the phase's.
///
/// It costs one look at each definition the walk enters, and keeps nothing between two walks. Its
/// message quotes the enum's name as an excerpt, so it is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct GenericEnum;

impl Visitor for GenericEnum {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let definition = model.def(def);
    if definition.kind() != DefKind::Enum {
      return;
    }
    let Some(first) = definition.type_params().next() else {
      return;
    };
    let refused = definition
      .contributions()
      .next()
      .is_some_and(|own: Contribution<'_>| own.has_errors());
    if refused {
      return;
    }
    let kind = DiagnosticKind::GenericEnum;
    cx.report(
      Diagnostic::builder(kind.code(), first.location())
        .severity(kind.severity())
        .message(format!(
          "the enum `{}` declares a type parameter, and an enum takes none: its values are names, \
           which hold no type",
          Excerpt(definition.name().as_bytes())
        ))
        .primary_label(kind.title())
        .help("remove the enum's type parameters")
        .build(),
    );
  }
}
