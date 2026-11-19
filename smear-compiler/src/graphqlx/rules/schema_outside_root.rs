//! The schema is the root package's: only the root package defines or extends it.
//!
//! A dependency is a library. The program a root package and its dependencies make has one
//! schema, the root package's, and a dependency that defined or extended it would give the same
//! root package another schema for each set of dependencies it is built with. So a schema
//! definition and a schema extension written in any other package are each reported, at the
//! item's first keyword, as `smear::graphqlx::schema-outside-root`.
//!
//! The model keeps such an item as a contribution of the schema that gives it nothing: lowering
//! looks for nothing the item writes, so this is the one diagnostic about it. A dependency's
//! schema definition is no second definition of the schema: the rule that counts schema
//! definitions counts the root package's alone.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{Cx, Diagnostic, DiagnosticKind, Visitor, diagnostic::Excerpt};

/// Reports each schema definition and each schema extension a package other than the root package
/// writes.
///
/// It reads the schema's contributions in the `schema` hook, and finds the package that wrote each
/// from where the contribution is written.
///
/// It says nothing about an item whose `has_errors()` is set: a phase already refused something in
/// it, and a second diagnostic within it would repeat the phase's.
///
/// It costs one visit of each of the schema's contributions, and keeps nothing between two walks.
/// Its message quotes the writing package's name as an excerpt, so it is bounded whatever the
/// name's length.
#[derive(Default)]
pub(super) struct SchemaOutsideRoot;

impl Visitor for SchemaOutsideRoot {
  fn schema(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let sources = cx.sources();
    let root = model.root_package();
    let kind = DiagnosticKind::SchemaOutsideRoot;
    for part in model.schema().contributions() {
      let Some(writer) = sources.written_by(part.location()) else {
        continue;
      };
      if writer == root || part.has_errors() {
        continue;
      }
      let does = if part.is_extension() {
        "extends"
      } else {
        "defines"
      };
      cx.report(
        Diagnostic::builder(kind.code(), part.location())
          .severity(kind.severity())
          .message(format!(
            "package `{}` {does} the schema, which only the root package defines and extends",
            Excerpt(model.package(writer).name().as_bytes())
          ))
          .primary_label(kind.title())
          .help(
            "a dependency is a library, and the schema is the root package's: remove this item, \
             or write it in the root package",
          )
          .build(),
      );
    }
  }
}
