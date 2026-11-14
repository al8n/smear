//! The root operations have distinct types.
//!
//! GraphQL refuses a schema whose query, mutation and subscription roots are not all different
//! types: the three are the entry points of three different operation kinds. A root takes part
//! when it is an object type: a presumed root, or a written root whose type names an object type
//! and resolved. Two roots are one type when both are written and their types are one, which for
//! two roots, each written with no `!`, is one nullable type; and when one is presumed and the
//! other is written as that same definition with no type argument. Two presumed roots are never
//! one: three different names presume them.
//!
//! In the order query, mutation, subscription, a root that is one type with an earlier one is
//! reported once, with a label at the first earlier one, under the code the same rule has in
//! GraphQL: `smear-schema`'s `shared-root-operation-type`, with its labels and its help. The
//! diagnostic stands at a written root, always: at the later one's type when it is written, and
//! otherwise at the earlier one's, labelled at the presumed root's definition. A root that is no
//! object type is the rule before's to report, and one that names nothing name resolution's:
//! neither takes part.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, Model, Root, RootOperation, TyId, TyKind,
  TypeExprId, TypeExprKind, Visitor, code::schema, diagnostic::Excerpt,
};

/// Reports each root operation type that is one type with an earlier root's.
///
/// It reads the schema's roots and its contributions in the `schema` hook. It says nothing when
/// any of the schema's contributions has its `has_errors()` set: the model records no contribution
/// for a root, and a phase that refused something in the schema may have refused it in the item
/// that writes the root.
///
/// It costs one look at each of the schema's contributions, and at each of its three roots and its
/// type, with three comparisons of two roots; it keeps nothing between two walks. Its messages
/// quote the definition's name as an excerpt, so each is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct SharedRootOperationType;

/// A root that takes part: an object type, written or presumed.
#[derive(Clone, Copy)]
enum Taking {
  /// Written: the occurrence, its type, and the object type it names.
  Written {
    occurrence: TypeExprId,
    ty: TyId,
    def: DefId,
  },
  /// Presumed: the object type.
  Presumed(DefId),
}

impl Taking {
  /// The object type the root names.
  const fn def(self) -> DefId {
    match self {
      Self::Written { def, .. } | Self::Presumed(def) => def,
    }
  }
}

impl Visitor for SharedRootOperationType {
  fn schema(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let roots = model.schema();
    if roots
      .contributions()
      .any(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let taking: [(&'static str, Option<Taking>); 3] = [
      ("query", takes_part(model, roots.root(RootOperation::Query))),
      (
        "mutation",
        takes_part(model, roots.root(RootOperation::Mutation)),
      ),
      (
        "subscription",
        takes_part(model, roots.root(RootOperation::Subscription)),
      ),
    ];
    for (index, &(operation, root)) in taking.iter().enumerate() {
      let Some(later) = root else {
        continue;
      };
      let first = taking
        .iter()
        .take(index)
        .find_map(|entry: &(&'static str, Option<Taking>)| {
          let (earlier, other) = *entry;
          other
            .filter(|other: &Taking| one(model, *other, later))
            .map(|other: Taking| (earlier, other))
        });
      let Some((earlier, first)) = first else {
        continue;
      };
      let (at, label) = match (later, first) {
        (
          Taking::Written { occurrence, .. },
          Taking::Written {
            occurrence: written,
            ..
          },
        ) => (
          model.type_expr(occurrence).location(),
          model.type_expr(written).location(),
        ),
        (Taking::Written { occurrence, .. }, Taking::Presumed(def))
        | (Taking::Presumed(def), Taking::Written { occurrence, .. }) => (
          model.type_expr(occurrence).location(),
          model.def(def).location(),
        ),
        (Taking::Presumed(_), Taking::Presumed(_)) => continue,
      };
      let mut report = Diagnostic::builder(schema::SHARED_ROOT_OPERATION_TYPE, at)
        .message(format!(
          "the {earlier} and {operation} root operations have one type, `{}`",
          Excerpt(model.def(later.def()).name().as_bytes())
        ))
        .primary_label("two root operations name the same type")
        .help(
          "the three roots are the entry points of three different operation kinds, so each \
           needs its own object type.",
        );
      if !label.is_entire() {
        report = report.label(label, "already a root operation type here");
      }
      cx.report(report.build());
    }
  }
}

/// The root `root`, of `model`, when it takes part: a presumed root, or a written one whose type
/// names an object type and resolved.
fn takes_part(model: &Model, root: Option<Root>) -> Option<Taking> {
  match root? {
    Root::Presumed(def) => Some(Taking::Presumed(def)),
    Root::Written(occurrence) => {
      let written = model.type_expr(occurrence);
      let TypeExprKind::Named { def, .. } = written.kind() else {
        return None;
      };
      let ty = written.ty();
      if model.def(def).kind() != DefKind::Object
        || matches!(model.ty(ty).kind(), TyKind::Unresolved)
      {
        return None;
      }
      Some(Taking::Written {
        occurrence,
        ty,
        def,
      })
    }
  }
}

/// Whether the roots `a` and `b`, of `model`, are one type: both written, of one type; or one
/// presumed, and the other written as that definition with no type argument.
fn one(model: &Model, a: Taking, b: Taking) -> bool {
  match (a, b) {
    (Taking::Written { ty: x, .. }, Taking::Written { ty: y, .. }) => x == y,
    (Taking::Presumed(def), Taking::Written { ty, .. })
    | (Taking::Written { ty, .. }, Taking::Presumed(def)) => matches!(
      model.ty(ty).kind(),
      TyKind::Named { def: named, args } if named == def && args.is_empty()
    ),
    (Taking::Presumed(_), Taking::Presumed(_)) => false,
  }
}
