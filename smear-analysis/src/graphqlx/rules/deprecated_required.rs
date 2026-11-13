//! A required argument or input field is not deprecated.
//!
//! GraphQL refuses `@deprecated` on a required argument, a field's or a directive's, and on a
//! required input field: one that is written non-null and has no default, which a caller must
//! give, and so cannot stop giving. Each is reported at its name, with a label at the first
//! `@deprecated` it applies, under the code the same rule has in GraphQL: `smear-schema`'s
//! `deprecated-required-argument` or `deprecated-required-input-field`, with its label and its
//! help.
//!
//! `@deprecated` is told by the definition an application resolved to, `core`'s, never by how it is
//! spelled: a directive of another module or package that is named `deprecated` is another
//! directive, and the root package's own `@deprecated`, which replaces `core`'s, keeps its
//! identity. Whether a default is written is read, and not the default's value. A field is not
//! judged: deprecating a field that is non-null is GraphQL's to allow.
//!
//! A member is judged when a supplied package wrote its definition, a GraphQL built-in the root
//! package replaces among them.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, DefId, Diagnostic, MemberId, MemberKind, Resolution, Visitor,
  code::schema, diagnostic::Excerpt,
};

use super::supplied;

/// Reports each argument and input field of a definition a supplied package wrote that is written
/// non-null, has no default, and applies `core`'s `@deprecated`.
///
/// It reads who wrote each definition in the `enter_def` hook, and each argument's and input
/// field's default, type and applications in the `enter_member` hook. It says nothing about a
/// member whose contribution has its `has_errors()` set: a phase already refused something in it,
/// and a second diagnostic within it would repeat the phase's.
///
/// It costs one answer of who wrote each definition the walk enters, and for each argument and
/// input field one look at its default and its type and one at each of its own applications. It
/// keeps one flag between two hooks: whether a supplied package wrote the definition the walk is
/// in. Its messages quote names as excerpts, so each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct DeprecatedRequired {
  /// Whether a supplied package wrote the definition the walk is in.
  judged: bool,
}

impl Visitor for DeprecatedRequired {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.judged = false;
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    self.judged = supplied(cx, def);
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    if !self.judged {
      return;
    }
    let model = cx.model();
    let written = model.member(member);
    if !matches!(
      written.kind(),
      MemberKind::Argument | MemberKind::InputField
    ) || written.default().is_some()
    {
      return;
    }
    let Some(ty) = written.ty() else {
      return;
    };
    if !model.type_expr(ty).non_null() {
      return;
    }
    let deprecated = model.builtins().deprecated();
    let Some(application) = written.applications().iter().find(|id: &&ApplicationId| {
      matches!(
        model.application(**id).directive(),
        Resolution::Resolved(directive) if directive == deprecated
      )
    }) else {
      return;
    };
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let name = Excerpt(written.name().as_bytes());
    let owner = Excerpt(model.def(written.owner()).name().as_bytes());
    let (code, message, label, help) = match (written.kind(), written.parent()) {
      (MemberKind::InputField, _) => (
        schema::DEPRECATED_REQUIRED_INPUT_FIELD,
        format!("the input field `{name}` of `{owner}` is required and deprecated"),
        "required input field is deprecated",
        "to deprecate a required input field, first make it optional — give it a default, or drop \
         the `!`.",
      ),
      (_, Some(field)) => {
        let field = Excerpt(model.member(field).name().as_bytes());
        (
          schema::DEPRECATED_REQUIRED_ARGUMENT,
          format!(
            "the argument `{name}` of the field `{field}` of `{owner}` is required and deprecated"
          ),
          "required argument is deprecated",
          "to deprecate a required argument, first make it optional — give it a default, or drop \
           the `!`.",
        )
      }
      (_, None) => (
        schema::DEPRECATED_REQUIRED_ARGUMENT,
        format!("the argument `{name}` of the directive `@{owner}` is required and deprecated"),
        "required argument is deprecated",
        "to deprecate a required argument, first make it optional — give it a default, or drop \
         the `!`.",
      ),
    };
    cx.report(
      Diagnostic::builder(code, written.location())
        .message(message)
        .primary_label(label)
        .label(
          model.application(*application).path_location(),
          "deprecated here",
        )
        .help(help)
        .build(),
    );
  }
}
