//! A schema has a query root operation type.
//!
//! GraphQL refuses a schema that provides no query root operation type: a query is the one
//! operation every schema answers, and its root is where every query begins. Here the schema is
//! the root package's, and it has a query root when lowering kept one: the `query:` that the root
//! package's first schema definition or one of its schema extensions writes, whatever it names, or,
//! with no schema definition of the root package's, an object type named `Query` that the root
//! package's root module itself declares, which GraphQL presumes the query root. A schema with none
//! is reported once, under the code the same rule has in GraphQL: `smear-schema`'s
//! `missing-query-root-operation-type`, with its label and its help.
//!
//! It is reported at the `schema` of the root package's first schema definition, and with none at
//! the whole of the root package's package source, which lies in no definition and so marks
//! nothing. A root that is written and names nothing, or names what is no object type, is a root
//! all the same: name resolution or another rule refuses it, and this rule says nothing. A
//! dependency has no schema, and nothing it writes is this rule's.
//!
//! A library has no schema of its own, so a consumer that analyzes one as the root package waives
//! this code by filtering it.
//!
//! The rule reads the public model and nothing else.

use tokora::diagnostic::Location;

use crate::graphqlx::{Contribution, Cx, Diagnostic, RootOperation, Visitor, code::schema};

/// Reports the schema when it has no query root operation type.
///
/// It reads the query root in the `schema` hook and, when there is none, the schema's
/// contributions up to the root package's first schema definition, telling the root package's from
/// a dependency's by where each is written. It says nothing when that definition's `has_errors()`
/// is set: a phase already refused something in it, and a second diagnostic within it would repeat
/// the phase's. With no schema definition of the root package's, its diagnostic lies in no
/// contribution, and nothing silences it.
///
/// It costs one look at the query root and, when there is none, at most one look at each of the
/// schema's contributions, and keeps nothing between two walks. Its message quotes nothing of the
/// input.
#[derive(Default)]
pub(super) struct MissingQueryRootOperationType;

impl Visitor for MissingQueryRootOperationType {
  fn schema(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let roots = model.schema();
    if roots.root(RootOperation::Query).is_some() {
      return;
    }
    let sources = cx.sources();
    let root = model.root_package();
    let first = roots.contributions().find(|part: &Contribution<'_>| {
      !part.is_extension() && sources.written_by(part.location()) == Some(root)
    });
    let at = match first {
      Some(part) if part.has_errors() => return,
      Some(part) => part.location(),
      // The root package's package source, which every package has, before its files.
      None => match model.package(root).sources().into_iter().next() {
        Some(source) => Location::entire(source.get()),
        None => return,
      },
    };
    cx.report(
      Diagnostic::builder(schema::MISSING_QUERY_ROOT_OPERATION_TYPE, at)
        .message("the schema has no query root operation type")
        .primary_label("no query root operation type")
        .help("define `type Query`, or name an existing object type with `schema { query: … }`.")
        .build(),
    );
  }
}
