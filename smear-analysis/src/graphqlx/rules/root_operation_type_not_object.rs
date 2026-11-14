//! A root operation type is an object type.
//!
//! GraphQL refuses a root operation type that is not an object type, for each of the three roots:
//! an interface, a marker among them, a union, a scalar, an enum or an input object has no field
//! set an operation could select from. Each written root whose type names a definition that is no
//! object type is reported at the type, under the code the same rule has in GraphQL:
//! `smear-schema`'s `root-operation-type-not-object`, with its label and its help. An alias such
//! as `NonZeroU32` names `core`'s `NonZero`, a scalar; an instantiation of a generic object type
//! is an object type.
//!
//! A presumed root is an object type by how it is presumed, and a root that names nothing is name
//! resolution's.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefKind, Diagnostic, Root, RootOperation, TypeExprKind, Visitor, code::schema,
  diagnostic::Excerpt,
};

/// Reports each written root operation type that names a definition that is no object type.
///
/// It reads the schema's roots and its contributions in the `schema` hook. It says nothing when
/// any of the schema's contributions has its `has_errors()` set: the model records no contribution
/// for a root, and a phase that refused something in the schema may have refused it in the item
/// that writes the root.
///
/// It costs one look at each of the schema's contributions and at each of its three roots, and
/// keeps nothing between two walks. Its messages quote the definition's name as an excerpt, so
/// each is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct RootOperationTypeNotObject;

impl Visitor for RootOperationTypeNotObject {
  fn schema(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let roots = model.schema();
    if roots
      .contributions()
      .any(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    for (operation, keyword) in [
      (RootOperation::Query, "query"),
      (RootOperation::Mutation, "mutation"),
      (RootOperation::Subscription, "subscription"),
    ] {
      let Some(Root::Written(ty)) = roots.root(operation) else {
        continue;
      };
      let occurrence = model.type_expr(ty);
      let TypeExprKind::Named { def, .. } = occurrence.kind() else {
        continue;
      };
      let definition = model.def(def);
      let what = match definition.kind() {
        DefKind::Object => continue,
        DefKind::Interface if definition.is_marker() => "a marker",
        DefKind::Interface => "an interface",
        DefKind::Union => "a union",
        DefKind::Scalar => "a scalar",
        DefKind::Enum => "an enum",
        DefKind::InputObject => "an input object",
        DefKind::Directive => "a directive",
      };
      cx.report(
        Diagnostic::builder(
          schema::ROOT_OPERATION_TYPE_NOT_OBJECT,
          occurrence.location(),
        )
        .message(format!(
          "the {keyword} root operation type `{}` is {what}, and a root operation type is an \
           object type",
          Excerpt(definition.name().as_bytes())
        ))
        .primary_label("root operation type is not an object type")
        .help(
          "a root operation type must be an object type: an interface or union has no concrete \
           field set to resolve.",
        )
        .build(),
      );
    }
  }
}
