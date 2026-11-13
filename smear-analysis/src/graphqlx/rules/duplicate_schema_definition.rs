//! One schema definition: a program defines its schema at most once.
//!
//! The schema is the root package's. The first schema definition the root package writes, by
//! source id and position, is the schema's: it gives the schema its description, its roots and its
//! applications. Each later one the root package writes gives it nothing, and is reported here, at
//! its `schema`, with a label at the first, under the code the same rule has in GraphQL:
//! `smear-schema`'s `duplicate-schema-definition`, with its label and its help.
//!
//! A schema definition a dependency writes is not counted here. It is refused once, as a schema
//! item outside the root package, by the rule of that name, whether or not the root package
//! defines the schema too.
//!
//! The rule reads the public model and nothing else.

use tokora::diagnostic::Location;

use crate::graphqlx::{Cx, Diagnostic, Visitor, code::schema};

/// Reports each schema definition the root package writes after its first.
///
/// It reads the schema's contributions in the `schema` hook, and tells a definition from an
/// extension, and the root package's from a dependency's, by what the model says of each, never
/// by its place among them.
///
/// It says nothing about a definition whose `has_errors()` is set: a phase already refused
/// something in it, and a second diagnostic within it would repeat the phase's. The first
/// definition is the schema's whether or not it has errors.
///
/// It costs one visit of each of the schema's contributions, and keeps nothing between two walks.
#[derive(Default)]
pub(super) struct DuplicateSchemaDefinition;

impl Visitor for DuplicateSchemaDefinition {
  fn schema(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let sources = cx.sources();
    let root = model.root_package();
    // Where the root package's first schema definition is.
    let mut first: Option<Location> = None;
    for part in model.schema().contributions() {
      if part.is_extension() || sources.written_by(part.location()) != Some(root) {
        continue;
      }
      let Some(earlier) = first else {
        first = Some(part.location());
        continue;
      };
      if part.has_errors() {
        continue;
      }
      cx.report(
        Diagnostic::builder(schema::DUPLICATE_SCHEMA_DEFINITION, part.location())
          .message("the root package defines the schema twice")
          .primary_label("duplicate schema definition")
          .label(earlier, "first defined here")
          .help(
            "a document holds at most one `schema` block; put the extra roots in `extend \
             schema`.",
          )
          .build(),
      );
    }
  }
}
