//! What an object or an interface implements: interfaces.
//!
//! GraphQL refuses an `implements` entry of an object or an interface that names a type which is
//! no interface. So each such entry is reported where it is written, under the code the same rule
//! has in GraphQL: `smear-schema`'s `implements-non-interface`, with its label. A marker is an
//! interface, with or without arguments, and an entry that names one is accepted. An entry that
//! names a type parameter of its definition is refused too, as a type parameter is no interface;
//! and an entry that names an alias such as `NonZeroU32` names the scalar it spells. An entry that
//! names nothing was refused by name resolution, and is passed by.
//!
//! What a scalar, an enum, an input object or a union implements is the marker rule's.
//!
//! A definition is judged when a supplied package wrote it, a GraphQL built-in the root package
//! replaces among them, and each entry whoever wrote it.
//!
//! The rule reads the public model and nothing else.

use std::{format, string::String};

use crate::graphqlx::{
  Cx, DefId, DefKind, Diagnostic, Path, TypeExprKind, Visitor, code::schema, diagnostic::Excerpt,
};

use super::supplied;

/// Reports each `implements` entry of an object or an interface a supplied package wrote that
/// names a definition which is no interface, or a type parameter.
///
/// It reads a definition's contributions and each one's `implements` entries in the `enter_def`
/// hook. It says nothing about an entry whose contribution has its `has_errors()` set: a phase
/// already refused something in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs one answer of who wrote each object and interface the walk enters, and one look at
/// each of its entries, and keeps nothing between two walks. Its messages quote names as excerpts,
/// so each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct ImplementsNonInterface;

impl Visitor for ImplementsNonInterface {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let definition = model.def(def);
    let noun = match definition.kind() {
      DefKind::Object => "type",
      DefKind::Interface => "interface",
      DefKind::Scalar
      | DefKind::Union
      | DefKind::Enum
      | DefKind::InputObject
      | DefKind::Directive => return,
    };
    if definition.implements().is_empty() || !supplied(cx, def) {
      return;
    }
    let owner = Excerpt(definition.name().as_bytes());
    for part in definition.contributions() {
      if part.has_errors() {
        continue;
      }
      for entry in part.implements() {
        let occurrence = model.type_expr(*entry);
        let what: String = match occurrence.kind() {
          TypeExprKind::Named { def: named, .. } => {
            let named = model.def(named);
            let kind = match named.kind() {
              DefKind::Interface => continue,
              DefKind::Object => "an object type",
              DefKind::Scalar => "a scalar",
              DefKind::Union => "a union",
              DefKind::Enum => "an enum",
              DefKind::InputObject => "an input object",
              DefKind::Directive => "a directive",
            };
            format!(
              "`{}`, which is {kind} and no interface",
              Excerpt(named.name().as_bytes())
            )
          }
          // The type parameter as the entry writes it: an extension's own name for its target's
          // parameter is its own.
          TypeExprKind::Param { .. } => {
            let Some(&name) = occurrence
              .path()
              .and_then(|path: Path<'_>| path.segments().last())
            else {
              continue;
            };
            format!(
              "its type parameter `{}`, and a type parameter is no interface",
              Excerpt(model.name(name).as_bytes())
            )
          }
          TypeExprKind::List { .. } | TypeExprKind::Unresolved => continue,
        };
        cx.report(
          Diagnostic::builder(schema::IMPLEMENTS_NON_INTERFACE, occurrence.location())
            .message(format!("the {noun} `{owner}` implements {what}"))
            .primary_label("implemented type is not an interface")
            .build(),
        );
      }
    }
  }
}
