//! `TypeRef` stands only in a directive's arguments, and no field's argument reaches one.
//!
//! `core`'s `TypeRef` is the type of a value that names a definition, and only a directive's
//! arguments hold such a value. So an occurrence that names it stands where it may exactly when:
//!
//! - every occurrence above it in its tree is a list: `TypeRef`, `[TypeRef!]`, `[[TypeRef]]`;
//! - its tree is a member's type;
//! - the member is an argument of a directive definition, or an input field, which only a
//!   directive's arguments may reach, as the second half says.
//!
//! Anywhere else it is reported, where it is named: a field's type, a field's argument, a type
//! argument of any generic definition, a collection's included, an application's type argument, a
//! union member, a type parameter's default, an `implements` entry, a bound, a root. It is told by
//! the definition an occurrence resolved to, never by a spelling. Each is
//! `smear::graphqlx::typeref-misuse`.
//!
//! And an argument of a field, which an operation sets, is refused when a value of its type can
//! hold an input field of type `TypeRef`, under lists alone: through lists, collections' arguments
//! and input objects, each input object's arguments that a value of it holds, which its input
//! fields alone say, over every package's input objects together, so that the root package's
//! argument of a dependency's input object that only the dependency's directives take is refused
//! in the root package. It is reported at the argument's type, labelled at the field of type
//! `TypeRef` a value can reach first, under the same code. A scalar that is no collection hides its
//! arguments, which no value of it holds. A `TypeRef` the argument's type names under lists alone
//! is the first half's refusal, and not this one's. A directive's arguments are metadata, and are
//! not judged.
//!
//! The same is met after substitution. A type parameter is reached from an operation's argument
//! where a field argument's type holds it, through lists, collections, the definitions it is
//! passed to, and the input objects whose values hold their arguments, as the table of where each
//! type parameter stands answers: in `type Q<T> { f(a: T): Int }` and in
//! `type D<T> { q: Q<Wrapped<T>> }`, with `input Wrapped<W> { v: W }`. So each instantiation, every
//! written type occurrence wherever it stands and every directive application, by its directive's
//! complete instantiation, gives such a parameter a type no value of which holds a field of type
//! `TypeRef`. The first argument, in order, that does is reported, at the argument as written, or
//! at the occurrence, an application at its path, for one a default fills; labelled where the
//! parameter is reached from an operation's argument, at the field of type `TypeRef`, and at the
//! default that fills it. Each type is judged once, and reported at each occurrence of it. Else,
//! what the slots an occurrence leaves to their defaults derive is judged the same way, at the
//! occurrence, a type the occurrence or the program writes being judged where it is written. A
//! directive's type parameter is reached from an operation's argument only through an object one
//! of its arguments' types holds, which is no input type: an application is judged, in practice,
//! for what its defaults derive.
//!
//! The rule reads the public model, and one crate-private accessor, `Application::ty`: an
//! application's complete type, its directive's instantiation. Through `Nesting` it reads the
//! public `TypeExpr::written_args`, to tell which occurrences of a tree are above another.

use std::{collections::BTreeMap, format};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, DefId, Diagnostic, DiagnosticKind, MemberId, MemberKind, Model,
  Site, TyId, TyKind, TypeExprId, TypeExprKind, Visitor,
  diagnostic::Excerpt,
  id::dense,
  positions::{Derived, Fill, Place, Positions, Taking, parameter, taking},
  reach::Reach,
};

use super::{Nesting, Silence};

/// Reports each occurrence of `TypeRef` that is not, under lists alone, the type of a directive
/// definition's argument or of an input field; each field's argument a value of whose type can
/// hold an input field of type `TypeRef`; and each occurrence and application whose type gives a
/// type parameter an operation's argument reaches, or whose fill gives one, such a type.
///
/// It finds where each type parameter stands and what a value of each input object can reach in
/// the `begin` hook, over every package's definitions; reads an occurrence, where it sits and the
/// member that holds it in the `type_expr` hook; each field's argument in the `enter_member` hook;
/// and each application in the `application` hook. It says nothing about an occurrence, an
/// argument or an application whose contribution, or for a root the schema's, a phase refused
/// something in. It reports only what a supplied package wrote: the walk reaches nothing else.
///
/// It costs, once per walk, the table of where each type parameter stands and the graph of what a
/// value of each input object can reach, each linear in what the definitions and the members hold,
/// and one look at the schema's contributions; one look at each occurrence the walk reaches; for
/// each field's argument, one answer of what its type can reach; and for each occurrence and
/// application, one look at its type and, once for each distinct type, a verdict that reads what
/// each argument at a slot an operation's argument reaches can reach, and once for each distinct
/// type and count of written arguments, the types its fill derives, each once. Each type's reach
/// is answered once for the walk. It keeps a count and two flags between two occurrences, and for
/// the walk the table, what each type can reach and its verdicts, keyed by the model's ids. Its
/// messages quote names as excerpts, so each is bounded whatever the names' lengths, and print no
/// type.
#[derive(Default)]
pub(super) struct TypeRefMisuse {
  /// Where each occurrence stands in its tree.
  nesting: Nesting,
  /// Whether it says nothing at an occurrence.
  silence: Silence,
  /// Where each type parameter stands.
  positions: Positions,
  /// What a value of each type can reach.
  reach: Reach,
  /// Each type's own verdict, judged once: the first of its arguments, at a slot an operation's
  /// argument reaches, a value of which can hold a field of type `TypeRef`.
  verdicts: BTreeMap<TyId, Option<Held>>,
  /// What each type's fills derive, judged once for each count of written arguments.
  derived: Derived<Held>,
}

/// A type's own failure: the first of its arguments, in order, at a slot an operation's argument
/// reaches, a value of which can hold a field of type `TypeRef`, with that field.
#[derive(Clone, Copy)]
struct Held {
  /// The slot of the argument.
  slot: u32,
  /// The input field of type `TypeRef` a value of the argument can hold.
  field: MemberId,
}

/// What an occurrence or an application is refused for.
#[derive(Clone, Copy)]
enum Found {
  /// Its type's own failure.
  Own(Held),
  /// A failure among what its fill derives.
  Derived(Fill<Held>),
}

impl Visitor for TypeRefMisuse {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    self.nesting = Nesting::default();
    self.silence = Silence::of(cx);
    self.positions = Positions::of(model, cx.sources());
    self.reach = Reach::of(model, &self.positions);
    self.verdicts = BTreeMap::new();
    self.derived = Derived::default();
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    let model = cx.model();
    let argument = model.member(member);
    if argument.kind() != MemberKind::Argument {
      return;
    }
    // A directive definition's argument has no field above it.
    let Some(field) = argument.parent() else {
      return;
    };
    let Some(ty) = argument.ty() else {
      return;
    };
    if cx
      .contribution()
      .is_some_and(|part: Contribution<'_>| part.has_errors())
    {
      return;
    }
    let occurrence = model.type_expr(ty);
    let Some(held) = self.reach.type_ref(model, &self.positions, occurrence.ty()) else {
      return;
    };
    let held = model.member(held);
    let kind = DiagnosticKind::TypeRefMisuse;
    cx.report(
      Diagnostic::builder(kind.code(), occurrence.location())
        .severity(kind.severity())
        .message(format!(
          "the argument `{}` of the field `{}` of `{}` takes a value that can hold the input field \
           `{}` of `{}`, whose type is `TypeRef`, which only a directive's arguments may reach",
          Excerpt(argument.name().as_bytes()),
          Excerpt(model.member(field).name().as_bytes()),
          Excerpt(model.def(argument.owner()).name().as_bytes()),
          Excerpt(held.name().as_bytes()),
          Excerpt(model.def(held.owner()).name().as_bytes())
        ))
        .primary_label(kind.title())
        .label(held.location(), "a value of `TypeRef` stands here")
        .help(HELP)
        .build(),
    );
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    let model = cx.model();
    let place = self.nesting.reach(model, ty);
    if self.silence.silenced(cx) {
      return;
    }
    let occurrence = model.type_expr(ty);
    let TypeExprKind::Named { def, args: written } = occurrence.kind() else {
      return;
    };
    if def != model.builtins().type_ref() {
      // `TypeRef` takes no type parameter, so only another definition's instantiation is judged.
      let complete = occurrence.ty();
      let Some(found) = self.judge(model, complete, dense(written.len())) else {
        return;
      };
      if let Some(report) = self.report(model, found, complete, written, occurrence.location()) {
        cx.report(report);
      }
      return;
    }
    // A directive definition's argument has no field above it; a field's argument does.
    let holds = |member: MemberId| -> bool {
      let member = model.member(member);
      match member.kind() {
        MemberKind::Argument => member.parent().is_none(),
        MemberKind::InputField => true,
        _ => false,
      }
    };
    if place.lists && cx.site() == Some(Site::MemberType) && cx.member().is_some_and(holds) {
      return;
    }
    let kind = DiagnosticKind::TypeRefMisuse;
    cx.report(
      Diagnostic::builder(kind.code(), occurrence.location())
        .severity(kind.severity())
        .message(
          "`TypeRef` names a definition, and stands only as the type of a directive definition's \
           argument or of an input field, a list of it included",
        )
        .primary_label(kind.title())
        .help(
          "pass the definition as a directive's argument of type `TypeRef`, several as \
           `[TypeRef!]`, and one with more beside it as an input object's field",
        )
        .build(),
    );
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
    let Some(complete) = applied.ty() else {
      return;
    };
    let written = applied.type_args();
    let Some(found) = self.judge(model, complete, dense(written.len())) else {
      return;
    };
    if let Some(report) = self.report(model, found, complete, written, applied.path_location()) {
      cx.report(report);
    }
  }
}

/// What the rule tells the author of an operation's argument that reaches a `TypeRef`.
const HELP: &str = "an operation's argument never holds a type reference: take the input object \
                    in a directive's argument alone, or drop the `TypeRef` field from what the \
                    argument reaches";

impl TypeRefMisuse {
  /// What an occurrence or an application whose type is `ty`, of `model`, and that writes
  /// `written` type arguments is refused for: its type's own failure, else one among what its
  /// fill derives.
  fn judge(&mut self, model: &Model, ty: TyId, written: u32) -> Option<Found> {
    let TyKind::Named { args, .. } = model.ty(ty).kind() else {
      return None;
    };
    if args.is_empty() {
      return None;
    }
    let Self {
      positions,
      reach,
      verdicts,
      derived,
      ..
    } = self;
    if let Some(own) = verdict(model, positions, reach, verdicts, ty) {
      return Some(Found::Own(own));
    }
    derived
      .verdict(model, ty, written, |next: TyId| {
        verdict(model, positions, reach, verdicts, next)
      })
      .map(Found::Derived)
  }

  /// The diagnostic of `found`, for an occurrence or an application whose type is `ty`, of `model`,
  /// that writes the type arguments `written`, `whole` being where it is written: the occurrence,
  /// or an application's path.
  fn report(
    &self,
    model: &Model,
    found: Found,
    ty: TyId,
    written: &[TypeExprId],
    whole: Location,
  ) -> Option<Diagnostic> {
    let (slot, field, derived) = match found {
      Found::Own(Held { slot, field }) => (slot, field, None),
      Found::Derived(Fill {
        slot: filled,
        ty: derived,
        verdict: Held { slot, field },
      }) => (slot, field, Some((filled, derived))),
    };
    let Taking {
      def: taker,
      slot,
      argument,
      lead,
      at,
      filled,
    } = taking(model, ty, written, whole, slot, derived)?;
    let field = model.member(field);
    let name = Excerpt(parameter(model, taker, slot).as_bytes());
    let taken = Excerpt(
      model
        .def(named_under_lists(model, argument)?)
        .name()
        .as_bytes(),
    );
    let mut report = Diagnostic::builder(DiagnosticKind::TypeRefMisuse.code(), at)
      .severity(DiagnosticKind::TypeRefMisuse.severity())
      .message(format!(
        "{lead} `{taken}` for its type parameter `{name}`, which an operation's argument reaches, \
         and a value of `{taken}` can hold the input field `{}` of `{}`, whose type is `TypeRef`",
        Excerpt(field.name().as_bytes()),
        Excerpt(model.def(field.owner()).name().as_bytes())
      ))
      .primary_label(DiagnosticKind::TypeRefMisuse.title());
    if let Some(witness) = self.positions.witness(taker, slot, Place::Operation) {
      report = report.label(
        witness,
        format!("`{name}` is reached from an operation's argument here"),
      );
    }
    report = report.label(field.location(), "a value of `TypeRef` stands here");
    if let Some(default) = filled {
      report = report.label(default, "filled from this default");
    }
    Some(report.help(HELP).build())
  }
}

/// The first argument of `ty`, of `model`, in order, at a slot an operation's argument reaches by
/// `positions`, a value of which can hold a field of type `TypeRef` by `reach`, with that field;
/// `verdicts` keeps each type's answer.
fn verdict(
  model: &Model,
  positions: &Positions,
  reach: &mut Reach,
  verdicts: &mut BTreeMap<TyId, Option<Held>>,
  ty: TyId,
) -> Option<Held> {
  if let Some(kept) = verdicts.get(&ty) {
    return *kept;
  }
  let mut found: Option<Held> = None;
  if let TyKind::Named { def, args } = model.ty(ty).kind() {
    for (slot, argument) in args.iter().enumerate() {
      let slot = dense(slot);
      if positions.witness(def, slot, Place::Operation).is_some()
        && let Some(field) = reach.type_ref(model, positions, *argument)
      {
        found = Some(Held { slot, field });
        break;
      }
    }
  }
  verdicts.insert(ty, found);
  found
}

/// The definition `ty`, of `model`, names under its lists, when it names one.
fn named_under_lists(model: &Model, ty: TyId) -> Option<DefId> {
  let mut ty = ty;
  // Each step goes one level down the type, so the loop ends.
  loop {
    match model.ty(ty).kind() {
      TyKind::List { element } => ty = element,
      TyKind::Named { def, .. } => return Some(def),
      TyKind::Param { .. } | TyKind::Unresolved => return None,
    }
  }
}
