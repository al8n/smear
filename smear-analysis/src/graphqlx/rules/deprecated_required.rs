//! A required argument or input field is not deprecated, nor made required by the type arguments an
//! instantiation gives.
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
//! The same is met after substitution. A deprecated argument or input field with no default whose
//! type is its definition's type parameter, written nullable, is required in an instantiation that
//! gives the parameter a non-null type: `I<Int!>` of `input I<T> { a: T @deprecated }`. Such a
//! parameter stands as the member's whole type where its definition puts it, and is passed on to
//! the definitions it is given to, as the table of where each type parameter stands answers, but
//! not through a list or a collection, which stand there instead. So each instantiation, every
//! written type occurrence wherever it stands and every directive application, by its directive's
//! complete instantiation, gives such a parameter a nullable type. The first argument, in order,
//! that is non-null is reported, under the argument's code or the input field's, at the argument
//! as written, or at the occurrence, an application at its path, for one a default fills;
//! labelled where the parameter stands, and at the default that fills it. Each type is judged
//! once, and reported at each occurrence of it. Else, what the slots an occurrence leaves to their
//! defaults derive is judged the same way, at the occurrence, a type the occurrence or the program
//! writes being judged where it is written.
//!
//! The rule reads the public model, and one crate-private accessor, `Application::ty`: an
//! application's complete type, its directive's instantiation.

use std::{collections::BTreeMap, format};

use tokora::diagnostic::{Code, Location};

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, DefId, Diagnostic, MemberId, MemberKind, Model, Resolution,
  TyId, TyKind, TypeExprId, TypeExprKind, Visitor,
  code::schema,
  diagnostic::Excerpt,
  id::dense,
  positions::{Derived, Fill, Place, Positions, Taking, parameter, taking},
};

use super::{Silence, supplied};

/// What GraphQL tells the author of a required argument that is deprecated.
const ARGUMENT_HELP: &str = "to deprecate a required argument, first make it optional — give it a \
                             default, or drop the `!`.";

/// What GraphQL tells the author of a required input field that is deprecated.
const INPUT_FIELD_HELP: &str = "to deprecate a required input field, first make it optional — \
                                give it a default, or drop the `!`.";

/// GraphQL's label for a required argument that is deprecated.
const ARGUMENT_LABEL: &str = "required argument is deprecated";

/// GraphQL's label for a required input field that is deprecated.
const INPUT_FIELD_LABEL: &str = "required input field is deprecated";

/// The places this rule judges an instantiation's arguments at, in the order it reads them.
const PLACES: [Place; 2] = [Place::DeprecatedArgument, Place::DeprecatedInputField];

/// Reports each argument and input field of a definition a supplied package wrote that is written
/// non-null, has no default, and applies `core`'s `@deprecated`; and each occurrence and
/// application whose type gives a type parameter that stands as such a member's whole type,
/// written nullable, or whose fill gives one, a non-null type.
///
/// It finds where each type parameter stands in the `begin` hook, reads who wrote each definition
/// in the `enter_def` hook, each argument's and input field's default, type and applications in
/// the `enter_member` hook, each occurrence in the `type_expr` hook and each application in the
/// `application` hook. It says nothing about a member, an occurrence or an application whose
/// contribution has its `has_errors()` set, or for a written root one of the schema's: a phase
/// already refused something in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs, once per walk, the table of where each type parameter stands, which is linear in the
/// slots, the open types the members, union members and applications reach and their arguments,
/// and one look at the schema's contributions; one answer of who wrote each definition the walk
/// enters, and for each argument and input field one look at its default and its type and one at
/// each of its own applications; for each occurrence and application, one look at its type and,
/// once for each distinct type, a verdict that reads each argument at a slot that stands as such a
/// member's type, and once for each distinct type and count of written arguments, the types its
/// fill derives, each once. It keeps, between two hooks, whether a supplied package wrote the
/// definition the walk is in, and for the walk the table, the silence and its verdicts, keyed by
/// the model's ids. Its messages quote names as excerpts, so each is bounded whatever the names'
/// lengths, and print no type.
#[derive(Default)]
pub(super) struct DeprecatedRequired {
  /// Whether a supplied package wrote the definition the walk is in.
  judged: bool,
  /// Whether it says nothing at an occurrence.
  silence: Silence,
  /// Where each type parameter stands.
  positions: Positions,
  /// Each type's own verdict, judged once: the first of its arguments, at a slot that stands as a
  /// deprecated member's whole type, that is non-null, with that place.
  verdicts: BTreeMap<TyId, Option<Own>>,
  /// What each type's fills derive, judged once for each count of written arguments.
  derived: Derived<Own>,
}

/// A type's own failure: the first of its arguments, in order, that is non-null at a slot that
/// stands as a deprecated member's whole type, with that place.
#[derive(Clone, Copy)]
struct Own {
  /// The slot of the argument.
  slot: u32,
  /// The place: a deprecated argument's type, or a deprecated input field's.
  place: Place,
}

/// What an occurrence or an application is refused for.
#[derive(Clone, Copy)]
enum Found {
  /// Its type's own failure.
  Own(Own),
  /// A failure among what its fill derives.
  Derived(Fill<Own>),
}

impl Visitor for DeprecatedRequired {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.judged = false;
    self.silence = Silence::of(cx);
    self.positions = Positions::of(cx.model(), cx.sources());
    self.verdicts = BTreeMap::new();
    self.derived = Derived::default();
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
        INPUT_FIELD_LABEL,
        INPUT_FIELD_HELP,
      ),
      (_, Some(field)) => {
        let field = Excerpt(model.member(field).name().as_bytes());
        (
          schema::DEPRECATED_REQUIRED_ARGUMENT,
          format!(
            "the argument `{name}` of the field `{field}` of `{owner}` is required and deprecated"
          ),
          ARGUMENT_LABEL,
          ARGUMENT_HELP,
        )
      }
      (_, None) => (
        schema::DEPRECATED_REQUIRED_ARGUMENT,
        format!("the argument `{name}` of the directive `@{owner}` is required and deprecated"),
        ARGUMENT_LABEL,
        ARGUMENT_HELP,
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

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    if self.silence.silenced(cx) {
      return;
    }
    let model = cx.model();
    let occurrence = model.type_expr(ty);
    let TypeExprKind::Named { args: written, .. } = occurrence.kind() else {
      return;
    };
    let complete = occurrence.ty();
    let Some(found) = self.judge(model, complete, dense(written.len())) else {
      return;
    };
    if let Some(report) = self.report(model, found, complete, written, occurrence.location()) {
      cx.report(report);
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

impl DeprecatedRequired {
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
      verdicts,
      derived,
      ..
    } = self;
    if let Some(own) = verdict(model, positions, verdicts, ty) {
      return Some(Found::Own(own));
    }
    derived
      .verdict(model, ty, written, |next: TyId| {
        verdict(model, positions, verdicts, next)
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
    let (Own { slot, place }, derived) = match found {
      Found::Own(own) => (own, None),
      Found::Derived(Fill {
        slot: filled,
        ty: derived,
        verdict,
      }) => (verdict, Some((filled, derived))),
    };
    let Taking {
      def: taker,
      slot,
      lead,
      at,
      filled,
      ..
    } = taking(model, ty, written, whole, slot, derived)?;
    let (code, label, help, member): (Code, &str, &str, &str) = match place {
      Place::DeprecatedInputField => (
        schema::DEPRECATED_REQUIRED_INPUT_FIELD,
        INPUT_FIELD_LABEL,
        INPUT_FIELD_HELP,
        "input field",
      ),
      _ => (
        schema::DEPRECATED_REQUIRED_ARGUMENT,
        ARGUMENT_LABEL,
        ARGUMENT_HELP,
        "argument",
      ),
    };
    let name = Excerpt(parameter(model, taker, slot).as_bytes());
    let mut report = Diagnostic::builder(code, at)
      .message(format!(
        "{lead} a non-null type for its type parameter `{name}`, which makes a deprecated \
         {member} required"
      ))
      .primary_label(label);
    if let Some(witness) = self.positions.witness(taker, slot, place) {
      report = report.label(witness, format!("`{name}` stands here"));
    }
    if let Some(default) = filled {
      report = report.label(default, "filled from this default");
    }
    Some(report.help(help).build())
  }
}

/// The first argument of `ty`, of `model`, in order, that is non-null at a slot that stands as a
/// deprecated member's whole type by `positions`, the places read in [`PLACES`]' order;
/// `verdicts` keeps each type's answer.
fn verdict(
  model: &Model,
  positions: &Positions,
  verdicts: &mut BTreeMap<TyId, Option<Own>>,
  ty: TyId,
) -> Option<Own> {
  if let Some(kept) = verdicts.get(&ty) {
    return *kept;
  }
  let mut found: Option<Own> = None;
  if let TyKind::Named { def, args } = model.ty(ty).kind() {
    'arguments: for (slot, argument) in args.iter().enumerate() {
      if !model.ty(*argument).non_null() {
        continue;
      }
      let slot = dense(slot);
      for place in PLACES {
        if positions.witness(def, slot, place).is_some() {
          found = Some(Own { slot, place });
          break 'arguments;
        }
      }
    }
  }
  verdicts.insert(ty, found);
  found
}
