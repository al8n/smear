//! A OneOf input object's fields are nullable.
//!
//! A OneOf input object is given exactly one of its fields, so each field is one of the
//! alternatives and none may be required: GraphQL refuses a field of a `@oneOf` input object that
//! is written non-null. Each is reported at its name, under the code the same rule has in GraphQL:
//! `smear-schema`'s `one-of-field-not-nullable`, with its label and its help. What is read is the
//! field's own outermost `!`: `[Int!]` is nullable. A field typed by a type parameter is judged as
//! it is written, here, and not at each instantiation.
//!
//! An input object is a OneOf input object when the package that wrote it applies `core`'s
//! `@oneOf` to it, in its definition or in an extension of its own, as `one_of` answers. So a
//! field is judged whichever contribution writes it, and another package's `@oneOf` makes nothing
//! OneOf.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefId, Diagnostic, MemberId, MemberKind, Visitor, code::schema,
  diagnostic::Excerpt,
};

use super::one_of;

/// Reports each field of a OneOf input object that is written non-null.
///
/// It reads whether an input object is OneOf in the `enter_def` hook, and each input field's
/// type in the `enter_member` hook. It says nothing about a field whose contribution has its
/// `has_errors()` set: a phase already refused something in it, and a second diagnostic within it
/// would repeat the phase's.
///
/// It costs, for each input object the walk enters, one look at each of its applications, and
/// for each input field one look at its type. It keeps one flag between two hooks: whether the
/// definition the walk is in is a OneOf input object. Its messages quote names as excerpts, so
/// each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct OneOfFieldNotNullable {
  /// Whether the definition the walk is in is a OneOf input object.
  one_of: bool,
}

impl Visitor for OneOfFieldNotNullable {
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
    if field.kind() != MemberKind::InputField {
      return;
    }
    let Some(ty) = field.ty() else {
      return;
    };
    if !model.type_expr(ty).non_null()
      || cx
        .contribution()
        .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    cx.report(
      Diagnostic::builder(schema::ONE_OF_FIELD_NOT_NULLABLE, field.location())
        .message(format!(
          "the field `{}` of the OneOf input object `{}` is non-null",
          Excerpt(field.name().as_bytes()),
          Excerpt(model.def(field.owner()).name().as_bytes())
        ))
        .primary_label("field of a @oneOf input object is non-null")
        .help(
          "every field of a `@oneOf` input object is one of the alternatives, so each one has to \
           be nullable.",
        )
        .build(),
    );
  }
}
