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
//! A scalar that is no collection is a type of every position, whatever its type arguments. A
//! collection, a scalar `core`'s `@set` or `@map` makes one, holds values of its arguments, so they
//! stand where it stands and are judged here, its complete type's, through every collection and
//! list it holds: `HashMap<string, User>` is no input type. One of a kind the position does not
//! take is reported at the deepest occurrence that writes the way to it; one in a slot the
//! occurrence leaves to its default, at the occurrence, with a label at the default. A generic
//! definition that is no collection is judged here by its kind alone, whatever its arguments. A
//! type parameter is a type of every position, and so is a path that names nothing, which name
//! resolution refused. A marker is an interface: one as an argument's type is this rule's refusal,
//! and the marker rule's beside it.
//!
//! A member is judged when a supplied package wrote its definition, a GraphQL built-in the root
//! package replaces among them.
//!
//! The rule reads the public model and nothing else.

use std::{format, string::String};

use tokora::diagnostic::Code;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, Member, MemberId, MemberKind, Model, TypeExprKind,
  Visitor,
  code::schema,
  diagnostic::Excerpt,
  positions::{Class, Fits, default_of, named},
};

use super::supplied;

/// Reports each member of a definition a supplied package wrote whose type's base, under its
/// lists, names a definition its position does not take, or a collection that holds one.
///
/// It reads who wrote each definition in the `enter_def` hook, and each member's type in the
/// `enter_member` hook, a field's arguments among them. It says nothing about a member whose
/// contribution has its `has_errors()` set: a phase already refused something in it, and a second
/// diagnostic within it would repeat the phase's.
///
/// It costs one answer of who wrote each definition the walk enters, and for each member one look
/// at its type and one step for each list its type writes, with no recursion; for a member whose
/// base is a collection, one answer of whether its type fits, each type and class judged once for
/// the walk, and a report follows one path down the type. It keeps, between two hooks, whether a
/// supplied package wrote the definition the walk is in, and for the walk what it judged of each
/// type. Its messages quote names as excerpts, so each is bounded whatever the names' lengths, and
/// print no type.
#[derive(Default)]
pub(super) struct MemberTypeKind {
  /// Whether a supplied package wrote the definition the walk is in.
  judged: bool,
  /// Whether the types it asked about fit the classes their positions take.
  fits: Fits,
}

impl Visitor for MemberTypeKind {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.judged = false;
    self.fits = Fits::default();
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
    let Some(position) = Position::of(written) else {
      return;
    };
    // The base: under the member's own lists, one step for each `[` its type writes.
    let mut base = model.type_expr(ty);
    while let TypeExprKind::List { element } = base.kind() {
      base = model.type_expr(element);
    }
    let TypeExprKind::Named { def: named_def, .. } = base.kind() else {
      return;
    };
    let definition = model.def(named_def);
    if definition.collection().is_some() {
      // A collection is a scalar, which the base check never refuses: its arguments are judged.
      let Some(misfit) = self
        .fits
        .misfit(model, base.ty(), position.class(), base.id())
      else {
        return;
      };
      let leaf = model.def(misfit.leaf);
      let mut report = Diagnostic::builder(position.code(), model.type_expr(misfit.at).location())
        .message(format!(
          "{} is typed by the collection `{}`, whose argument is {} `{}`, and {}",
          what(model, written, position),
          Excerpt(definition.name().as_bytes()),
          named(leaf.kind()),
          Excerpt(leaf.name().as_bytes()),
          position.takes()
        ))
        .primary_label(position.label());
      if let Some(default) = misfit
        .filled
        .and_then(|(def, index): (DefId, u32)| default_of(model, def, index))
      {
        report = report.label(default, "filled from this default");
      }
      cx.report(report.help(position.help()).build());
      return;
    }
    let refused = match position.class() {
      Class::Output => definition.kind() == DefKind::InputObject,
      Class::Input => matches!(
        definition.kind(),
        DefKind::Object | DefKind::Interface | DefKind::Union
      ),
    };
    if !refused {
      return;
    }
    cx.report(
      Diagnostic::builder(position.code(), base.location())
        .message(format!(
          "{} is typed by {} `{}`, and {}",
          what(model, written, position),
          named(definition.kind()),
          Excerpt(definition.name().as_bytes()),
          position.takes()
        ))
        .primary_label(position.label())
        .help(position.help())
        .build(),
    );
  }
}

/// The four positions whose types the rule judges, each with its class and GraphQL's words.
#[derive(Clone, Copy)]
enum Position {
  /// A field's type.
  Field,
  /// A field's argument's type.
  Argument,
  /// A directive definition's argument's type.
  DirectiveArgument,
  /// An input field's type.
  InputField,
}

impl Position {
  /// The position `member`'s type stands in, or `None` for an enum value, which has no type.
  fn of(member: Member<'_>) -> Option<Self> {
    match member.kind() {
      MemberKind::Field => Some(Self::Field),
      MemberKind::Argument if member.parent().is_some() => Some(Self::Argument),
      MemberKind::Argument => Some(Self::DirectiveArgument),
      MemberKind::InputField => Some(Self::InputField),
      MemberKind::EnumValue => None,
    }
  }

  /// GraphQL's code for a type of a kind the position does not take.
  fn code(self) -> Code {
    match self {
      Self::Field => schema::FIELD_TYPE_NOT_OUTPUT_TYPE,
      Self::Argument => schema::ARGUMENT_TYPE_NOT_INPUT_TYPE,
      Self::DirectiveArgument => schema::DIRECTIVE_ARGUMENT_TYPE_NOT_INPUT_TYPE,
      Self::InputField => schema::INPUT_FIELD_TYPE_NOT_INPUT_TYPE,
    }
  }

  /// The class of type the position takes.
  fn class(self) -> Class {
    match self {
      Self::Field => Class::Output,
      Self::Argument | Self::DirectiveArgument | Self::InputField => Class::Input,
    }
  }

  /// What the position takes, the end of a message.
  fn takes(self) -> &'static str {
    match self {
      Self::Field => "a field's type is an output type",
      Self::Argument => "an argument's type is an input type",
      Self::DirectiveArgument => "a directive's argument's type is an input type",
      Self::InputField => "an input field's type is an input type",
    }
  }

  /// GraphQL's primary label.
  fn label(self) -> &'static str {
    match self {
      Self::Field => "field type is not an output type",
      Self::Argument => "argument type is not an input type",
      Self::DirectiveArgument => "directive argument type is not an input type",
      Self::InputField => "input field type is not an input type",
    }
  }

  /// GraphQL's help.
  fn help(self) -> &'static str {
    match self {
      Self::Field => {
        "a field's type must be a scalar, object, interface, union or enum; an input object \
         appears only in argument and input-field positions."
      }
      Self::Argument => "an argument's type must be a scalar, enum or input object.",
      Self::DirectiveArgument => {
        "a directive argument's type must be a scalar, enum or input object."
      }
      Self::InputField => "an input field's type must be a scalar, enum or input object.",
    }
  }
}

/// How a message names `member`, of `model`, whose type stands in `position`: its name and its
/// owner's, each quoted as an excerpt.
fn what(model: &Model, member: Member<'_>, position: Position) -> String {
  let name = Excerpt(member.name().as_bytes());
  let owner = Excerpt(model.def(member.owner()).name().as_bytes());
  match (position, member.parent()) {
    (Position::Field, _) => format!("the field `{name}` of `{owner}`"),
    (Position::Argument, Some(field)) => format!(
      "the argument `{name}` of the field `{}` of `{owner}`",
      Excerpt(model.member(field).name().as_bytes())
    ),
    (Position::Argument | Position::DirectiveArgument, _) => {
      format!("the argument `{name}` of the directive `@{owner}`")
    }
    (Position::InputField, _) => format!("the input field `{name}` of `{owner}`"),
  }
}
