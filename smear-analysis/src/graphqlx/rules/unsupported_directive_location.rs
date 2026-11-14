//! A directive stands where its definition lists.
//!
//! A directive definition lists the locations its directive may be applied at, and GraphQL refuses
//! an application anywhere else: on the schema, on a definition of a kind, a directive definition
//! included, or on a field, an argument, an input field or an enum value, each its own location.
//! An argument is one location whether it is a field's or a directive definition's. Each
//! application at a location its directive's definition does not list is reported at its path,
//! with a label at the directive definition's name, under the code the same rule has in GraphQL:
//! `smear-schema`'s `unsupported-directive-location`, with its label and its help.
//!
//! The directive is the definition the application resolved to, `core`'s own among them, so
//! `@marker` on an object and `@deprecated` on a scalar are refused as a program's own directive
//! is. An application an extension writes stands where its target stands, on a built-in included.
//! An application whose directive did not resolve is name resolution's.
//!
//! The rule reads the public model and nothing else.

use std::{format, vec::Vec};

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, DefKind, Diagnostic, DirectiveLocation, MemberKind, Resolution,
  Visitor, code::schema, diagnostic::Excerpt,
};

/// Reports each application at a location its directive's definition does not list.
///
/// It reads every directive definition's locations in the `begin` hook, and each application in
/// the `application` hook. It says nothing about an application whose contribution has its
/// `has_errors()` set: a phase already refused something in it, and a second diagnostic within it
/// would repeat the phase's.
///
/// It costs, once per walk, one look at each definition and at each location each directive
/// definition writes, however many times it writes one; and for each application, one `AND`. It
/// keeps one word per definition for the walk: the locations it lists, one bit each. Its messages
/// quote the directive's name as an excerpt, so each is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct UnsupportedDirectiveLocation {
  /// For each definition, by index, the locations it lists, one bit each: none for a definition
  /// that is no directive.
  masks: Vec<u32>,
}

impl Visitor for UnsupportedDirectiveLocation {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    self.masks.clear();
    for def in model.defs() {
      let mut mask: u32 = 0;
      for location in model.def(def).locations() {
        mask |= bit(*location);
      }
      self.masks.push(mask);
    }
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let model = cx.model();
    let applied = model.application(application);
    let Resolution::Resolved(directive) = applied.directive() else {
      return;
    };
    let (location, what, keyword) = place(cx);
    let listed = usize::try_from(directive.get())
      .ok()
      .and_then(|index: usize| self.masks.get(index))
      .copied()
      .unwrap_or(0);
    if (listed & bit(location)) != 0 {
      return;
    }
    let definition = model.def(directive);
    let mut report = Diagnostic::builder(
      schema::UNSUPPORTED_DIRECTIVE_LOCATION,
      applied.path_location(),
    )
    .message(format!(
      "the directive `@{}` is applied to {what}, and its definition does not list `{keyword}`",
      Excerpt(definition.name().as_bytes())
    ))
    .primary_label("directive is not allowed here")
    .help("add the location to the directive's definition, or move the usage.");
    if !definition.location().is_entire() {
      report = report.label(definition.location(), "the directive's definition");
    }
    cx.report(report.build());
  }
}

/// The location the application the walk has reached stands at, `cx` saying where that is: what
/// it is applied to, for the message, and the location as a definition writes it.
///
/// An application stands on the innermost member the walk is in, a field's argument within it; on
/// the definition the walk is in when it is in no member; and on the schema when it is in neither.
fn place(cx: &Cx<'_>) -> (DirectiveLocation, &'static str, &'static str) {
  let model = cx.model();
  if let Some(member) = cx.member() {
    return match model.member(member).kind() {
      MemberKind::Field => (
        DirectiveLocation::FieldDefinition,
        "a field",
        "FIELD_DEFINITION",
      ),
      MemberKind::Argument => (
        DirectiveLocation::ArgumentDefinition,
        "an argument",
        "ARGUMENT_DEFINITION",
      ),
      MemberKind::InputField => (
        DirectiveLocation::InputFieldDefinition,
        "an input field",
        "INPUT_FIELD_DEFINITION",
      ),
      MemberKind::EnumValue => (DirectiveLocation::EnumValue, "an enum value", "ENUM_VALUE"),
    };
  }
  let Some(def) = cx.def() else {
    return (DirectiveLocation::Schema, "the schema", "SCHEMA");
  };
  match model.def(def).kind() {
    DefKind::Scalar => (DirectiveLocation::Scalar, "a scalar", "SCALAR"),
    DefKind::Object => (DirectiveLocation::Object, "an object type", "OBJECT"),
    DefKind::Interface => (DirectiveLocation::Interface, "an interface", "INTERFACE"),
    DefKind::Union => (DirectiveLocation::Union, "a union", "UNION"),
    DefKind::Enum => (DirectiveLocation::Enum, "an enum", "ENUM"),
    DefKind::InputObject => (
      DirectiveLocation::InputObject,
      "an input object",
      "INPUT_OBJECT",
    ),
    DefKind::Directive => (
      DirectiveLocation::DirectiveDefinition,
      "a directive definition",
      "DIRECTIVE_DEFINITION",
    ),
  }
}

/// The bit of `location` in a word of locations: one of twenty, each its own.
const fn bit(location: DirectiveLocation) -> u32 {
  let index: u32 = match location {
    DirectiveLocation::Query => 0,
    DirectiveLocation::Mutation => 1,
    DirectiveLocation::Subscription => 2,
    DirectiveLocation::Field => 3,
    DirectiveLocation::FragmentDefinition => 4,
    DirectiveLocation::FragmentSpread => 5,
    DirectiveLocation::InlineFragment => 6,
    DirectiveLocation::VariableDefinition => 7,
    DirectiveLocation::Schema => 8,
    DirectiveLocation::Scalar => 9,
    DirectiveLocation::Object => 10,
    DirectiveLocation::FieldDefinition => 11,
    DirectiveLocation::ArgumentDefinition => 12,
    DirectiveLocation::Interface => 13,
    DirectiveLocation::Union => 14,
    DirectiveLocation::Enum => 15,
    DirectiveLocation::EnumValue => 16,
    DirectiveLocation::InputObject => 17,
    DirectiveLocation::InputFieldDefinition => 18,
    DirectiveLocation::DirectiveDefinition => 19,
  };
  1 << index
}
