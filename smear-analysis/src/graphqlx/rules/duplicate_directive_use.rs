//! A directive that is not repeatable is applied once.
//!
//! GraphQL refuses a directive that is not `repeatable` applied twice to one location. A location
//! is everything applied to one thing: a definition's applications, its own contribution's and
//! every extension's, which are one location; a member's, a field's argument being a member of
//! its own; and the schema's, its definition's and every extension's. Each application of a
//! directive that is not repeatable whose directive an earlier application of its location applies
//! is reported at its path, with a label at the first application of that directive there, under
//! the code the same rule has in GraphQL: `smear-schema`'s `duplicate-directive-use`, with its
//! labels and its help.
//!
//! The directive is the definition the application resolved to, whatever its type arguments: a
//! generic directive applied twice with other type arguments is one directive applied twice. An
//! application whose directive did not resolve is name resolution's. A definition of `core`,
//! `alloc` or `std` is one location with what a supplied package's extension applies to it, so
//! the root package's `extend interface MapKey @marker` applies `@marker` again; an application is
//! reported only when a supplied package wrote it.
//!
//! The rule reads the public model and nothing else.

use std::{format, string::String, vec::Vec};

use smear_semantics::duplicates::Duplicates;

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, DefId, DefKind, Diagnostic, MemberId, MemberKind, Model,
  PackageId, PackageKind, Resolution, Visitor, code::schema, diagnostic::Excerpt,
};

/// Reports each application of a directive that is not repeatable which an earlier application of
/// its location applies.
///
/// It reads a definition's contributions and their applications in the `enter_def` hook, a
/// member's applications in the `enter_member` hook, and the schema's contributions and their
/// applications in the `schema` hook: each is one location. It says nothing about an application
/// whose contribution has its `has_errors()` set, the later application's, where it would point: a
/// phase already refused something in it, and a second diagnostic within it would repeat the
/// phase's. A first application inside such a contribution still labels a later one outside it.
///
/// It costs, for each location, one look at each of its applications, and with two or more one
/// scan of the directives they apply: a scan compares each with the ones before it while the
/// location holds at most sixty-four, and sorts a larger one once. It keeps one buffer, which
/// holds one location's applications at a time. Its messages quote names as excerpts, so each is
/// bounded whatever the names' lengths.
#[derive(Default)]
pub(super) struct DuplicateDirectiveUse {
  /// The applications of the location being judged, in order.
  applied: Vec<Applied>,
}

/// One application of a location.
#[derive(Clone, Copy)]
struct Applied {
  /// The application.
  id: ApplicationId,
  /// The directive it resolved to, or `None` when it did not resolve.
  directive: Option<DefId>,
  /// Whether a phase reported an error within the contribution that wrote it.
  refused: bool,
}

/// What a location's applications are applied to, for what a diagnostic says.
#[derive(Clone, Copy)]
enum Holder {
  /// A definition, its own contribution and its extensions.
  Def(DefId),
  /// A member.
  Member(MemberId),
  /// The schema.
  Schema,
}

impl DuplicateDirectiveUse {
  /// Appends `applications`, of `model`, which a contribution whose errors `refused` says wrote,
  /// to the location being judged.
  fn push(&mut self, model: &Model, applications: &[ApplicationId], refused: bool) {
    for id in applications {
      let directive = match model.application(*id).directive() {
        Resolution::Resolved(directive) => Some(directive),
        Resolution::Unresolved => None,
      };
      self.applied.push(Applied {
        id: *id,
        directive,
        refused,
      });
    }
  }
}

impl Visitor for DuplicateDirectiveUse {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.applied.clear();
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    self.applied.clear();
    for part in model.def(def).contributions() {
      self.push(model, part.applications(), part.has_errors());
    }
    judge(cx, &self.applied, Holder::Def(def));
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    let model = cx.model();
    // A member's applications are all its contribution's.
    let refused = cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors());
    self.applied.clear();
    self.push(model, model.member(member).applications(), refused);
    judge(cx, &self.applied, Holder::Member(member));
  }

  fn schema(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    self.applied.clear();
    for part in model.schema().contributions() {
      self.push(model, part.applications(), part.has_errors());
    }
    judge(cx, &self.applied, Holder::Schema);
  }
}

/// Reports each application of `applied`, one location's in order, whose directive is not
/// repeatable and an earlier application of `applied` applies, `holder` being what they are
/// applied to.
fn judge(cx: &mut Cx<'_>, applied: &[Applied], holder: Holder) {
  if applied.len() < 2 {
    return;
  }
  let model = cx.model();
  let sources = cx.sources();
  let mut seen = Duplicates::over(applied.len(), |position: usize| {
    applied
      .get(position)
      .and_then(|one: &Applied| one.directive)
  });
  for (position, one) in applied.iter().enumerate() {
    let Some(directive) = one.directive else {
      continue;
    };
    if model.def(directive).is_repeatable() {
      continue;
    }
    let Some(first) = seen.first(position, Some(directive)) else {
      continue;
    };
    if one.refused {
      continue;
    }
    let later = model.application(one.id);
    let supplied = sources
      .written_by(later.path_location())
      .is_some_and(|writer: PackageId| model.package(writer).kind() != PackageKind::BuiltIn);
    let Some(earlier) = applied.get(first) else {
      continue;
    };
    if !supplied {
      continue;
    }
    cx.report(
      Diagnostic::builder(schema::DUPLICATE_DIRECTIVE_USE, later.path_location())
        .message(format!(
          "the directive `@{}` is applied to {} again, and it is not repeatable",
          Excerpt(model.def(directive).name().as_bytes()),
          described(model, holder)
        ))
        .primary_label("directive is not repeatable")
        .label(
          model.application(earlier.id).path_location(),
          "first applied here",
        )
        .help(
          "declare the directive `repeatable`, or apply it once; a definition and its extensions \
           are one location.",
        )
        .build(),
    );
  }
}

/// What `holder` is, as a diagnostic names it: the schema, or a definition or a member by its own
/// name, each through an excerpt.
fn described(model: &Model, holder: Holder) -> String {
  match holder {
    Holder::Schema => String::from("the schema"),
    Holder::Def(def) => {
      let definition = model.def(def);
      let name = Excerpt(definition.name().as_bytes());
      if definition.kind() == DefKind::Directive {
        format!("the directive `@{name}`")
      } else {
        format!("the type `{name}`")
      }
    }
    Holder::Member(member) => {
      let member = model.member(member);
      let name = Excerpt(member.name().as_bytes());
      let owner = Excerpt(model.def(member.owner()).name().as_bytes());
      match (member.kind(), member.parent()) {
        (MemberKind::Field, _) => format!("the field `{name}` of `{owner}`"),
        (MemberKind::Argument, Some(field)) => format!(
          "the argument `{name}` of the field `{}` of `{owner}`",
          Excerpt(model.member(field).name().as_bytes())
        ),
        (MemberKind::Argument, None) => {
          format!("the argument `{name}` of the directive `@{owner}`")
        }
        (MemberKind::InputField, _) => format!("the input field `{name}` of `{owner}`"),
        (MemberKind::EnumValue, _) => format!("the value `{name}` of the enum `{owner}`"),
      }
    }
  }
}
