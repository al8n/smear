//! Types of the right kind: a member's type is of the kind its position takes.
//!
//! GraphQL refuses a field whose type is an input object, and an argument, a directive's argument
//! or an input field whose type is an object, an interface or a union: a field's type is an output
//! type and the others' an input type. What is judged is the base of the member's type, the named
//! type under its lists and its `!`. It is reported there, at the named type and not at the list
//! around it, under the code the same rule has in GraphQL: `smear-schema`'s
//! `field-type-not-output-type`, `argument-type-not-input-type`,
//! `directive-argument-type-not-input-type` or `input-field-type-not-input-type`, with its label
//! and its help.
//!
//! A scalar is a type of every position, whatever its type arguments: what an instantiation's
//! arguments may be is judged where the instantiation is written, and not here. So is a type
//! parameter, and a path that names nothing, which name resolution refused. A marker is an
//! interface: one as an argument's type is this rule's refusal, and the marker rule's beside it.
//!
//! A member is judged when a supplied package wrote its definition, a GraphQL built-in the root
//! package replaces among them.
//!
//! The rule reads the public model and nothing else.

use std::format;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, MemberId, MemberKind, TypeExprKind, Visitor,
  code::schema, diagnostic::Excerpt,
};

use super::supplied;

/// Reports each member of a definition a supplied package wrote whose type's base, under its
/// lists, names a definition its position does not take.
///
/// It reads who wrote each definition in the `enter_def` hook, and each member's type in the
/// `enter_member` hook, a field's arguments among them. It says nothing about a member whose
/// contribution has its `has_errors()` set: a phase already refused something in it, and a second
/// diagnostic within it would repeat the phase's.
///
/// It costs one answer of who wrote each definition the walk enters, and for each member one look
/// at its type and one step for each list its type writes, with no recursion. It keeps one flag
/// between two hooks: whether a supplied package wrote the definition the walk is in. Its messages
/// quote names as excerpts, so each is bounded whatever the names' lengths, and print no type.
#[derive(Default)]
pub(super) struct MemberTypeKind {
  /// Whether a supplied package wrote the definition the walk is in.
  judged: bool,
}

impl Visitor for MemberTypeKind {
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
    let Some(ty) = written.ty() else {
      return;
    };
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    // The base: under the member's own lists, one step for each `[` its type writes.
    let mut base = model.type_expr(ty);
    while let TypeExprKind::List { element } = base.kind() {
      base = model.type_expr(element);
    }
    let TypeExprKind::Named { def: named, .. } = base.kind() else {
      return;
    };
    let named = model.def(named);
    let output = named.kind() == DefKind::InputObject;
    let input = matches!(
      named.kind(),
      DefKind::Object | DefKind::Interface | DefKind::Union
    );
    let parent = written.parent();
    let (code, refused) = match written.kind() {
      MemberKind::Field => (schema::FIELD_TYPE_NOT_OUTPUT_TYPE, output),
      MemberKind::Argument if parent.is_some() => (schema::ARGUMENT_TYPE_NOT_INPUT_TYPE, input),
      MemberKind::Argument => (schema::DIRECTIVE_ARGUMENT_TYPE_NOT_INPUT_TYPE, input),
      MemberKind::InputField => (schema::INPUT_FIELD_TYPE_NOT_INPUT_TYPE, input),
      MemberKind::EnumValue => return,
    };
    if !refused {
      return;
    }
    let name = Excerpt(written.name().as_bytes());
    let owner = Excerpt(model.def(written.owner()).name().as_bytes());
    let (what, takes, label, help) = match (written.kind(), parent) {
      (MemberKind::Field, _) => (
        format!("the field `{name}` of `{owner}`"),
        "a field's type is an output type",
        "field type is not an output type",
        "a field's type must be a scalar, object, interface, union or enum; an input object \
         appears only in argument and input-field positions.",
      ),
      (MemberKind::Argument, Some(field)) => {
        let field = Excerpt(model.member(field).name().as_bytes());
        (
          format!("the argument `{name}` of the field `{field}` of `{owner}`"),
          "an argument's type is an input type",
          "argument type is not an input type",
          "an argument's type must be a scalar, enum or input object.",
        )
      }
      (MemberKind::Argument, None) => (
        format!("the argument `{name}` of the directive `@{owner}`"),
        "a directive's argument's type is an input type",
        "directive argument type is not an input type",
        "a directive argument's type must be a scalar, enum or input object.",
      ),
      (MemberKind::InputField, _) => (
        format!("the input field `{name}` of `{owner}`"),
        "an input field's type is an input type",
        "input field type is not an input type",
        "an input field's type must be a scalar, enum or input object.",
      ),
      (MemberKind::EnumValue, _) => return,
    };
    let kind = match named.kind() {
      DefKind::InputObject => "the input object",
      DefKind::Object => "the object type",
      DefKind::Interface => "the interface",
      DefKind::Union => "the union",
      DefKind::Scalar | DefKind::Enum | DefKind::Directive => return,
    };
    let base_name = Excerpt(named.name().as_bytes());
    cx.report(
      Diagnostic::builder(code, base.location())
        .message(format!(
          "{what} is typed by {kind} `{base_name}`, and {takes}"
        ))
        .primary_label(label)
        .help(help)
        .build(),
    );
  }
}
