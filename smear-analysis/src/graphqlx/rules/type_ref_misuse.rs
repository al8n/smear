//! `TypeRef` stands only in a directive's arguments.
//!
//! `core`'s `TypeRef` is the type of a value that names a definition, and only a directive's
//! arguments hold such a value. So an occurrence that names it stands where it may exactly when:
//!
//! - every occurrence above it in its tree is a list: `TypeRef`, `[TypeRef!]`, `[[TypeRef]]`;
//! - its tree is a member's type;
//! - the member is an argument of a directive definition, or an input field, which only a
//!   directive's arguments may reach: which input objects those are is coercion's to judge.
//!
//! Anywhere else it is reported, where it is named: a field's type, a field's argument, a type
//! argument of any generic definition, a collection's included, an application's type argument, a
//! union member, a type parameter's default, an `implements` entry, a bound, a root. It is told by
//! the definition an occurrence resolved to, never by a spelling. Each is
//! `smear::graphqlx::typeref-misuse`.
//!
//! The rule reads the public model, and one crate-private accessor through `Nesting`:
//! `TypeExpr::written_args`, to tell which occurrences of a tree are above another.

use crate::graphqlx::{
  Cx, Diagnostic, DiagnosticKind, MemberId, MemberKind, Site, TypeExprId, TypeExprKind, Visitor,
};

use super::{Nesting, Silence};

/// Reports each occurrence of `TypeRef` that is not, under lists alone, the type of a directive
/// definition's argument or of an input field.
///
/// It reads an occurrence, where it sits and the member that holds it in the `type_expr` hook. It
/// says nothing about an occurrence whose contribution, or for a root the schema's, a phase
/// refused something in. It reports only what a supplied package wrote: the walk reaches nothing
/// else.
///
/// It costs one look at each occurrence the walk reaches, and one look at the schema's
/// contributions for the whole walk; it keeps a count and two flags between two occurrences, and
/// nothing between two walks. Its message is static text.
#[derive(Default)]
pub(super) struct TypeRefMisuse {
  /// Where each occurrence stands in its tree.
  nesting: Nesting,
  /// Whether it says nothing at an occurrence.
  silence: Silence,
}

impl Visitor for TypeRefMisuse {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.nesting = Nesting::default();
    self.silence = Silence::of(cx);
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
