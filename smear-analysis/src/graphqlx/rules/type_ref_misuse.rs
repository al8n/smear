//! `TypeRef` stands only in a directive's arguments, and no field's argument reaches one.
//!
//! `core`'s `TypeRef` is the type of a value that names a definition, and only a directive's
//! arguments hold such a value. So an occurrence that names it stands where it may exactly when:
//!
//! - every occurrence above it in its tree is a list: `TypeRef`, `[TypeRef!]`, `[[TypeRef]]`;
//! - its tree is a member's type;
//! - the member is an argument of a directive definition, or an input field, which only a
//!   directive's arguments may reach, as the second half says.
//!
//! Anywhere else it is reported, where it is named: a field's type, a field's argument, a type
//! argument of any generic definition, a collection's included, an application's type argument, a
//! union member, a type parameter's default, an `implements` entry, a bound, a root. It is told by
//! the definition an occurrence resolved to, never by a spelling. Each is
//! `smear::graphqlx::typeref-misuse`.
//!
//! And an argument of a field, which an operation sets, is refused when a value of its type can
//! hold an input field of type `TypeRef`, under lists alone: through lists, collections' arguments
//! and input objects, each input object's arguments for its type parameters that stand in its
//! input fields, over every package's input objects together, so that the root package's argument
//! of a dependency's input object that only the dependency's directives take is refused in the
//! root package. It is reported at the argument's type, labelled at the field of type `TypeRef` a
//! value can reach first, under the same code. A scalar that is no collection hides its arguments,
//! which no value of it holds. A `TypeRef` the argument's type names under lists alone is the first
//! half's refusal, and not this one's. A directive's arguments are metadata, and are not judged.
//!
//! The rule reads the public model and nothing else: through `Nesting`, the public
//! `TypeExpr::written_args`, to tell which occurrences of a tree are above another.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, Diagnostic, DiagnosticKind, MemberId, MemberKind, Site, TypeExprId,
  TypeExprKind, Visitor, diagnostic::Excerpt, positions::Positions, reach::Reach,
};

use super::{Nesting, Silence};

/// Reports each occurrence of `TypeRef` that is not, under lists alone, the type of a directive
/// definition's argument or of an input field; and each field's argument a value of whose type
/// can hold an input field of type `TypeRef`.
///
/// It finds what a value of each input object can reach in the `begin` hook, over every package's
/// input objects; reads an occurrence, where it sits and the member that holds it in the
/// `type_expr` hook; and each field's argument in the `enter_member` hook. It says nothing about
/// an occurrence or an argument whose contribution, or for a root the schema's, a phase refused
/// something in. It reports only what a supplied package wrote: the walk reaches nothing else.
///
/// It costs, once per walk, the table of where each type parameter stands and the graph of what a
/// value of each input object can reach, each linear in what the input objects and the members
/// hold, and one look at the schema's contributions; one look at each occurrence the walk reaches;
/// and for each field's argument, one answer of what its type can reach, each type answered once
/// for the walk. It keeps a count and two flags between two occurrences, and for the walk the
/// table and what each type can reach, keyed by the model's ids. Its messages quote names as
/// excerpts, so each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct TypeRefMisuse {
  /// Where each occurrence stands in its tree.
  nesting: Nesting,
  /// Whether it says nothing at an occurrence.
  silence: Silence,
  /// Where each type parameter stands.
  positions: Positions,
  /// What a value of each type can reach.
  reach: Reach,
}

impl Visitor for TypeRefMisuse {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    self.nesting = Nesting::default();
    self.silence = Silence::of(cx);
    self.positions = Positions::of(model);
    self.reach = Reach::of(model, &self.positions);
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    let model = cx.model();
    let argument = model.member(member);
    if argument.kind() != MemberKind::Argument {
      return;
    }
    // A directive definition's argument has no field above it.
    let Some(field) = argument.parent() else {
      return;
    };
    let Some(ty) = argument.ty() else {
      return;
    };
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let occurrence = model.type_expr(ty);
    let Some(held) = self.reach.type_ref(model, &self.positions, occurrence.ty()) else {
      return;
    };
    let held = model.member(held);
    let kind = DiagnosticKind::TypeRefMisuse;
    cx.report(
      Diagnostic::builder(kind.code(), occurrence.location())
        .severity(kind.severity())
        .message(format!(
          "the argument `{}` of the field `{}` of `{}` takes a value that can hold the input field \
           `{}` of `{}`, whose type is `TypeRef`, which only a directive's arguments may reach",
          Excerpt(argument.name().as_bytes()),
          Excerpt(model.member(field).name().as_bytes()),
          Excerpt(model.def(argument.owner()).name().as_bytes()),
          Excerpt(held.name().as_bytes()),
          Excerpt(model.def(held.owner()).name().as_bytes())
        ))
        .primary_label(kind.title())
        .label(held.location(), "a value of `TypeRef` stands here")
        .help(
          "an operation's argument never holds a type reference: take the input object in a \
           directive's argument alone, or drop the `TypeRef` field from what the argument reaches",
        )
        .build(),
    );
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    let model = cx.model();
    let place = self.nesting.reach(model, ty);
    if self.silence.silenced(cx) {
      return;
    }
    let occurrence = model.type_expr(ty);
    let TypeExprKind::Named { def, .. } = occurrence.kind() else {
      return;
    };
    if def != model.builtins().type_ref() {
      return;
    }
    // A directive definition's argument has no field above it; a field's argument does.
    let holds = |member: MemberId| -> bool {
      let member = model.member(member);
      match member.kind() {
        MemberKind::Argument => member.parent().is_none(),
        MemberKind::InputField => true,
        _ => false,
      }
    };
    if place.lists && cx.site() == Some(Site::MemberType) && cx.member().is_some_and(holds) {
      return;
    }
    let kind = DiagnosticKind::TypeRefMisuse;
    cx.report(
      Diagnostic::builder(kind.code(), occurrence.location())
        .severity(kind.severity())
        .message(
          "`TypeRef` names a definition, and stands only as the type of a directive definition's \
           argument or of an input field, a list of it included",
        )
        .primary_label(kind.title())
        .help(
          "pass the definition as a directive's argument of type `TypeRef`, several as \
           `[TypeRef!]`, and one with more beside it as an input object's field",
        )
        .build(),
    );
  }
}
