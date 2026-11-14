//! A union's members are object types.
//!
//! GraphQL refuses a union member that is no object type: a union of interfaces, of scalars, of
//! enums, of input objects or of unions has no concrete member set. So each such member is
//! reported where it is written, under the code the same rule has in GraphQL: `smear-schema`'s
//! `union-member-not-object`, with its label and its help. An object is a member, with or without
//! arguments; a member that names an alias such as `NonZeroU32` names the scalar it spells. A
//! member that is a type parameter of a generic union is not judged here: each instantiation that
//! fills it is where it is written. A member that names nothing was refused by name resolution,
//! and is passed by.
//!
//! A union is judged when a supplied package wrote it, a GraphQL built-in the root package
//! replaces among them, and each member whoever wrote it.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Cx, DefId, DefKind, Diagnostic, TypeExprKind, Visitor, code::schema, diagnostic::Excerpt,
};

use super::supplied;

/// Reports each member of a union a supplied package wrote that names a definition which is no
/// object type.
///
/// It reads a union's contributions and each one's members in the `enter_def` hook. It says
/// nothing about a member whose contribution has its `has_errors()` set: a phase already refused
/// something in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs one answer of who wrote each union the walk enters, and one look at each of its
/// members, and keeps nothing between two walks. Its messages quote names as excerpts, so each is
/// bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct UnionMemberNotObject;

impl Visitor for UnionMemberNotObject {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let definition = model.def(def);
    if definition.kind() != DefKind::Union
      || definition.union_members().is_empty()
      || !supplied(cx, def)
    {
      return;
    }
    let union = Excerpt(definition.name().as_bytes());
    for part in definition.contributions() {
      if part.has_errors() {
        continue;
      }
      for member in part.union_members() {
        let occurrence = model.type_expr(*member);
        let TypeExprKind::Named { def: named, .. } = occurrence.kind() else {
          continue;
        };
        let named = model.def(named);
        let kind = match named.kind() {
          DefKind::Object => continue,
          DefKind::Interface => "an interface",
          DefKind::Scalar => "a scalar",
          DefKind::Union => "a union",
          DefKind::Enum => "an enum",
          DefKind::InputObject => "an input object",
          DefKind::Directive => "a directive",
        };
        cx.report(
          Diagnostic::builder(schema::UNION_MEMBER_NOT_OBJECT, occurrence.location())
            .message(format!(
              "the union `{union}` holds `{}`, which is {kind} and no object type",
              Excerpt(named.name().as_bytes())
            ))
            .primary_label("union member is not an object type")
            .help(
              "a union's members must be object types: a union of interfaces or of unions has no \
               concrete member set.",
            )
            .build(),
        );
      }
    }
  }
}
