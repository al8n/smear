//! A schema extension extends a schema that is defined.
//!
//! GraphQL refuses an `extend schema` when there is no schema to extend: the document writes no
//! `schema` definition, and no object type named `Query` is presumed its query root. Here the
//! schema is the root package's, and it is defined when the root package writes a schema
//! definition, or when its query root is presumed, which lowering decides before it reads any
//! extension's roots, as GraphQL decides it. When it is not, each schema extension the root package
//! writes is reported at its `extend`, under the code the same rule has in GraphQL:
//! `smear-schema`'s `schema-extension-without-schema`, with its label and its help. The extension
//! gives the schema what it writes all the same.
//!
//! A dependency's schema extension is the rule on schema items outside the root package's alone.
//!
//! The rule reads the public model and nothing else.

use crate::graphqlx::{Contribution, Cx, Diagnostic, Root, RootOperation, Visitor, code::schema};

/// Reports each schema extension the root package writes when it defines no schema to extend.
///
/// It reads the query root and the schema's contributions in the `schema` hook, and finds the
/// package that wrote each contribution from where it is written. It says nothing about an
/// extension whose `has_errors()` is set: a phase already refused something in it, and a second
/// diagnostic within it would repeat the phase's.
///
/// It costs at most two looks at each of the schema's contributions, and keeps nothing between two
/// walks. Its message quotes nothing of the input.
#[derive(Default)]
pub(super) struct SchemaExtensionWithoutSchema;

impl Visitor for SchemaExtensionWithoutSchema {
  fn schema(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let sources = cx.sources();
    let root = model.root_package();
    let roots = model.schema();
    if matches!(roots.root(RootOperation::Query), Some(Root::Presumed(_))) {
      return;
    }
    let ours =
      |part: &Contribution<'_>| -> bool { sources.written_by(part.location()) == Some(root) };
    if roots
      .contributions()
      .any(|part: Contribution<'_>| !part.is_extension() && ours(&part))
    {
      return;
    }
    for part in roots.contributions() {
      if !part.is_extension() || part.has_errors() || !ours(&part) {
        continue;
      }
      cx.report(
        Diagnostic::builder(schema::SCHEMA_EXTENSION_WITHOUT_SCHEMA, part.location())
          .message(
            "the root package extends the schema, and has none to extend: it writes no schema \
             definition, and no object type `Query` is presumed its query root",
          )
          .primary_label("schema extension has no schema to extend")
          .help(
            "`extend schema` adds to a schema the document defines: write a `schema` definition, \
             or define the `Query` type that omitting one presumes.",
          )
          .build(),
      );
    }
  }
}
