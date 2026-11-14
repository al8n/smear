//! Members held: a definition that holds members holds one at least.
//!
//! GraphQL refuses an object or an interface that defines no field, a union that declares no
//! member type, an enum that defines no value and an input object that defines no field. What a
//! definition holds is everything it holds, its own contribution's and each extension's, whoever
//! wrote the extension, as GraphQL judges the merged type: a definition whose every member an
//! extension adds is not empty. An empty one is reported once, at its name, under the code the
//! same rule has in GraphQL: `smear-schema`'s `empty-fields-definition`, `empty-union-members`,
//! `empty-enum-values` or `empty-input-fields`, with its label and its help.
//!
//! GraphQLx differs from GraphQL in one case: an interface that is a marker, which says what a type
//! is and holds nothing, holds no field by its own rule. So the help for an empty interface says
//! that it may be a marker. A union whose one member names nothing has a member: name resolution
//! refused the member.
//!
//! A definition is judged when a supplied package wrote it, a GraphQL built-in the root package
//! replaces among them.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, Visitor, code::schema, diagnostic::Excerpt,
};

use super::supplied;

/// What GraphQL tells the author of an object or an interface that defines no field.
const FIELDS_HELP: &str = "an object or interface type must define at least one field.";

/// What the analyzer tells the author of an interface that defines no field and is no marker.
const INTERFACE_HELP: &str = "an object or interface type must define at least one field. An \
                              interface that says what a type is and holds nothing is a marker, \
                              and takes `@marker`.";

/// Reports each object or interface with no field, union with no member, enum with no value and
/// input object with no field, of those a supplied package wrote; a marker holds no field.
///
/// It reads a definition's kind, whether it is a marker, and the length of its members or its
/// union members in the `enter_def` hook. It says nothing about a definition whose own
/// contribution, its first, has its `has_errors()` set, where its name is: a phase already refused
/// something in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs one look at the length of what each definition the walk enters holds, and for an empty
/// one an answer of who wrote it, and keeps nothing between two walks. Its messages quote the
/// definition's name as an excerpt, so each is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct EmptyDefinition;

impl Visitor for EmptyDefinition {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let definition = model.def(def);
    let (empty, code, what, lacks, label, help) = match definition.kind() {
      DefKind::Object => (
        definition.fields().is_empty(),
        schema::EMPTY_FIELDS_DEFINITION,
        "type",
        "field",
        "type defines no fields",
        Some(FIELDS_HELP),
      ),
      DefKind::Interface => (
        !definition.is_marker() && definition.fields().is_empty(),
        schema::EMPTY_FIELDS_DEFINITION,
        "interface",
        "field",
        "type defines no fields",
        Some(INTERFACE_HELP),
      ),
      DefKind::Union => (
        definition.union_members().is_empty(),
        schema::EMPTY_UNION_MEMBERS,
        "union",
        "member type",
        "union declares no member types",
        None,
      ),
      DefKind::Enum => (
        definition.enum_values().is_empty(),
        schema::EMPTY_ENUM_VALUES,
        "enum",
        "value",
        "enum defines no values",
        None,
      ),
      DefKind::InputObject => (
        definition.input_fields().is_empty(),
        schema::EMPTY_INPUT_FIELDS,
        "input object",
        "field",
        "input object defines no fields",
        None,
      ),
      DefKind::Scalar | DefKind::Directive => return,
    };
    if !empty || !supplied(cx, def) {
      return;
    }
    // `supplied` answers `true` only for a definition whose first contribution is its own.
    let refused = definition
      .contributions()
      .next()
      .is_none_or(|own: Contribution<'_>| own.has_errors());
    if refused {
      return;
    }
    let name = Excerpt(definition.name().as_bytes());
    let mut report = Diagnostic::builder(code, definition.location())
      .message(format!("the {what} `{name}` holds no {lacks}"))
      .primary_label(label);
    if let Some(help) = help {
      report = report.help(help);
    }
    cx.report(report.build());
  }
}
