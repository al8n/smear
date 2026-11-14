//! A OneOf input object's fields have no default.
//!
//! A OneOf input object is given exactly one of its fields, and a field with a default would always
//! be given: GraphQL refuses a field of a `@oneOf` input object that declares a default value. Each
//! is reported at its name, under the code the same rule has in GraphQL: `smear-schema`'s
//! `one-of-field-has-default`, with its label and its help. What is read is whether a default is
//! written, and not the default's value.
//!
//! An input object is a OneOf input object when the package that wrote it applies `core`'s
//! `@oneOf` to it, in its definition or in an extension of its own, as `one_of` answers.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefId, Diagnostic, MemberId, MemberKind, Visitor, code::schema,
  diagnostic::Excerpt,
};

use super::one_of;

/// Reports each field of a OneOf input object that declares a default value.
///
/// It reads whether an input object is OneOf in the `enter_def` hook, and whether each input field
/// writes a default in the `enter_member` hook. It says nothing about a field whose contribution
/// has its `has_errors()` set: a phase already refused something in it, and a second diagnostic
/// within it would repeat the phase's.
///
/// It costs, for each input object the walk enters, one look at each of its applications, and
/// for each input field one look at whether it writes a default. It keeps one flag between two
/// hooks: whether the definition the walk is in is a OneOf input object. Its messages quote names
/// as excerpts, so each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct OneOfFieldHasDefault {
  /// Whether the definition the walk is in is a OneOf input object.
  one_of: bool,
}

impl Visitor for OneOfFieldHasDefault {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.one_of = false;
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    self.one_of = one_of(cx, def);
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    if !self.one_of {
      return;
    }
    let model = cx.model();
    let field = model.member(member);
    if field.kind() != MemberKind::InputField
      || field.default().is_none()
      || cx
        .contribution()
        .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    cx.report(
      Diagnostic::builder(schema::ONE_OF_FIELD_HAS_DEFAULT, field.location())
        .message(format!(
          "the field `{}` of the OneOf input object `{}` has a default value",
          Excerpt(field.name().as_bytes()),
          Excerpt(model.def(field.owner()).name().as_bytes())
        ))
        .primary_label("field of a @oneOf input object has a default value")
        .help("a defaulted field would always be present, which is the one thing `@oneOf` forbids.")
        .build(),
    );
  }
}
