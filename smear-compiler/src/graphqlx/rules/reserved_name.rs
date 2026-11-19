//! Reserved names: a member's name does not begin with two underscores.
//!
//! GraphQL reserves the names that begin with `__` for its introspection system, and the language
//! keeps the reservation for every member: a field, a field's argument, an enum value, an input
//! field and a directive's argument. So a member whose name begins with `__` is reported at its
//! name, under the code the same rule has in GraphQL, by what the member is: `smear-schema`'s
//! `reserved-field-name`, `reserved-argument-name`, `reserved-enum-value-name`,
//! `reserved-input-field-name` or `reserved-directive-argument-name`, with its label and its help.
//!
//! A member is judged when a supplied package wrote its definition, a GraphQL built-in the root
//! package replaces among them, whichever contribution of the definition writes the member. What a
//! package adds to a definition of `core`, `alloc` or `std` by an extension is the orphan rule's
//! alone, as GraphQL judges no member of a built-in type.
//!
//! The rule reads the public model and nothing else.

use std::{format, string::String};

use crate::graphqlx::{
  Contribution, Cx, DefId, Diagnostic, MemberId, MemberKind, Visitor, code::schema,
  diagnostic::Excerpt,
};

use super::supplied;

/// Reports each member whose name begins with `__`, of a definition a supplied package wrote.
///
/// It reads who wrote each definition in the `enter_def` hook, and each member's name in the
/// `enter_member` hook, a field's arguments among them. It says nothing about a member whose
/// contribution has its `has_errors()` set: a phase already refused something in it, and a second
/// diagnostic within it would repeat the phase's.
///
/// It costs one answer of who wrote each definition the walk enters and a look at the first two
/// bytes of each member's name, and keeps one flag between two hooks: whether a supplied package
/// wrote the definition the walk is in. Its messages quote the member's name and its owners' names
/// as excerpts, so each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct ReservedName {
  /// Whether a supplied package wrote the definition the walk is in.
  judged: bool,
}

impl Visitor for ReservedName {
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
    if !written.name().as_bytes().starts_with(b"__") {
      return;
    }
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let name = Excerpt(written.name().as_bytes());
    let owner = Excerpt(model.def(written.owner()).name().as_bytes());
    let (code, message, label): (_, String, &'static str) = match written.kind() {
      MemberKind::Field => (
        schema::RESERVED_FIELD_NAME,
        format!("the field `{name}` of `{owner}` begins with `__`, which introspection reserves"),
        "field name is reserved for introspection",
      ),
      MemberKind::Argument => match written.parent() {
        Some(field) => {
          let field = Excerpt(model.member(field).name().as_bytes());
          (
            schema::RESERVED_ARGUMENT_NAME,
            format!(
              "the argument `{name}` of the field `{field}` of `{owner}` begins with `__`, which \
               introspection reserves"
            ),
            "argument name is reserved for introspection",
          )
        }
        None => (
          schema::RESERVED_DIRECTIVE_ARGUMENT_NAME,
          format!(
            "the argument `{name}` of the directive `@{owner}` begins with `__`, which \
             introspection reserves"
          ),
          "directive argument name is reserved for introspection",
        ),
      },
      MemberKind::EnumValue => (
        schema::RESERVED_ENUM_VALUE_NAME,
        format!(
          "the value `{name}` of the enum `{owner}` begins with `__`, which introspection reserves"
        ),
        "enum value name is reserved for introspection",
      ),
      MemberKind::InputField => (
        schema::RESERVED_INPUT_FIELD_NAME,
        format!(
          "the input field `{name}` of `{owner}` begins with `__`, which introspection reserves"
        ),
        "input field name is reserved for introspection",
      ),
    };
    cx.report(
      Diagnostic::builder(code, written.location())
        .message(message)
        .primary_label(label)
        .help("a name beginning with `__` is reserved for introspection; rename it.")
        .build(),
    );
  }
}
