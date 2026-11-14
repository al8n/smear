//! Distinct members: a definition's members have distinct names, and so have a field's arguments.
//!
//! GraphQL refuses a name written twice in one list of members: an object's or an interface's
//! fields, an input object's fields, an enum's values, a directive's arguments, and a field's
//! arguments. A definition's list is everything it holds, its own contribution's members and then
//! each extension's, so a member an extension repeats is the same mistake as one the definition
//! writes twice. Each member whose name an earlier member of its list has is reported at its name,
//! with a label at the first member of that name, under the code the same rule has in GraphQL:
//! `smear-schema`'s `duplicate-field-name`, `duplicate-input-field-name`, `duplicate-enum-value`,
//! `duplicate-directive-argument-name` or `duplicate-argument-name`, with its labels.
//!
//! Two definitions of one name are two definitions, each with its own members: the declarations
//! report the second, and its members are not compared with the first's.
//!
//! A definition is judged when a supplied package wrote it, a GraphQL built-in the root package
//! replaces among them. What a package adds to a definition of `core`, `alloc` or `std` is the
//! orphan rule's alone, as GraphQL judges no member of a built-in type.
//!
//! The rule reads one crate-private accessor, `IdRange::start`, to name the member at a position
//! of a list.

use std::format;

use smear_semantics::duplicates::Duplicates;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, IdRange, MemberId, MemberKind, Model, Sym, Visitor,
  code::schema, diagnostic::Excerpt, id::dense,
};

use super::supplied;

/// Reports each member whose name an earlier member of its list has, in a definition a supplied
/// package wrote.
///
/// It reads a definition's direct members and its contributions in the `enter_def` hook, and a
/// field's arguments in the `enter_member` hook. It says nothing about a member whose contribution
/// has its `has_errors()` set, the later member's, where it would point: a phase already refused
/// something in it, and a second diagnostic within it would repeat the phase's. A first member
/// inside such a contribution still labels a later one outside it. A field's arguments are all its
/// contribution's, so a refused contribution says nothing of them.
///
/// It costs, for each definition the walk enters, one answer of who wrote it and one scan of its
/// direct members, and for each field one scan of its arguments: a scan reads each name once,
/// compares it with the names before it while the list is at most sixty-four long, and sorts a
/// longer list once. A list of fewer than two is not scanned. It keeps one flag between two hooks:
/// whether a supplied package wrote the definition the walk is in. Its messages quote names as
/// excerpts, so each is bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct DuplicateMember {
  /// Whether a supplied package wrote the definition the walk is in.
  judged: bool,
}

/// Which list a member is in, for what its diagnostic says.
#[derive(Clone, Copy)]
enum List {
  /// An object's or an interface's fields.
  Fields,
  /// An input object's fields.
  InputFields,
  /// An enum's values.
  EnumValues,
  /// A directive definition's arguments.
  DirectiveArguments,
  /// A field's arguments.
  FieldArguments,
}

impl Visitor for DuplicateMember {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.judged = false;
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    self.judged = supplied(cx, def);
    if !self.judged {
      return;
    }
    let model = cx.model();
    let definition = model.def(def);
    let (members, list) = match definition.kind() {
      DefKind::Object | DefKind::Interface => (definition.fields(), List::Fields),
      DefKind::InputObject => (definition.input_fields(), List::InputFields),
      DefKind::Enum => (definition.enum_values(), List::EnumValues),
      DefKind::Directive => (definition.arguments(), List::DirectiveArguments),
      DefKind::Scalar | DefKind::Union => return,
    };
    let Some(mut seen) = scan(model, members) else {
      return;
    };
    // The contributions hold the direct members one after another, in contribution order, so
    // this reads the list in its order and says which contribution wrote each member.
    for part in definition.contributions() {
      for member in part.members() {
        let Some(first) = earlier(&mut seen, model, members, member) else {
          continue;
        };
        if !part.has_errors() {
          report(cx, list, member, first);
        }
      }
    }
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    if !self.judged {
      return;
    }
    let model = cx.model();
    let field = model.member(member);
    if field.kind() != MemberKind::Field {
      return;
    }
    let arguments = field.arguments();
    if arguments.len() < 2
      || cx
        .contribution()
        .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let Some(mut seen) = scan(model, arguments) else {
      return;
    };
    for argument in arguments {
      if let Some(first) = earlier(&mut seen, model, arguments, argument) {
        report(cx, List::FieldArguments, argument, first);
      }
    }
  }
}

/// The scan of the names of `members`, or `None` for a list of fewer than two, which repeats
/// none.
fn scan(model: &Model, members: IdRange<MemberId>) -> Option<Duplicates<Sym>> {
  if members.len() < 2 {
    return None;
  }
  let len = usize::try_from(members.len()).ok()?;
  let start = members.start();
  Some(Duplicates::over(len, |position: usize| {
    model
      .member(MemberId::new(start.saturating_add(dense(position))))
      .sym()
  }))
}

/// The earlier member of `members` whose name `member` has, `seen` being the scan of `members`
/// that has read every member before `member` and none after it.
fn earlier(
  seen: &mut Duplicates<Sym>,
  model: &Model,
  members: IdRange<MemberId>,
  member: MemberId,
) -> Option<MemberId> {
  if !members.contains(member) {
    return None;
  }
  let start = members.start();
  let position = usize::try_from(member.get().saturating_sub(start)).ok()?;
  let first = seen.first(position, model.member(member).sym())?;
  Some(MemberId::new(start.saturating_add(dense(first))))
}

/// Reports `member` of the list `list`, whose name the earlier member `first` has.
fn report(cx: &mut Cx<'_>, list: List, member: MemberId, first: MemberId) {
  let model = cx.model();
  let later = model.member(member);
  let name = Excerpt(later.name().as_bytes());
  let owner = Excerpt(model.def(later.owner()).name().as_bytes());
  let (code, message, label, related) = match list {
    List::Fields => (
      schema::DUPLICATE_FIELD_NAME,
      format!("`{owner}` declares the field `{name}` again"),
      "duplicate field",
      "first defined here",
    ),
    List::InputFields => (
      schema::DUPLICATE_INPUT_FIELD_NAME,
      format!("`{owner}` declares the input field `{name}` again"),
      "duplicate input field",
      "first defined here",
    ),
    List::EnumValues => (
      schema::DUPLICATE_ENUM_VALUE,
      format!("the enum `{owner}` declares the value `{name}` again"),
      "duplicate enum value",
      "first defined here",
    ),
    List::DirectiveArguments => (
      schema::DUPLICATE_DIRECTIVE_ARGUMENT_NAME,
      format!("the directive `@{owner}` declares the argument `{name}` again"),
      "duplicate directive argument",
      "first declared here",
    ),
    List::FieldArguments => {
      let field = later
        .parent()
        .map_or("", |parent: MemberId| model.member(parent).name());
      let field = Excerpt(field.as_bytes());
      (
        schema::DUPLICATE_ARGUMENT_NAME,
        format!("the field `{field}` of `{owner}` declares the argument `{name}` again"),
        "duplicate argument",
        "first declared here",
      )
    }
  };
  cx.report(
    Diagnostic::builder(code, later.location())
      .message(message)
      .primary_label(label)
      .label(model.member(first).location(), related)
      .build(),
  );
}
