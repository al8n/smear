//! A OneOf input object's fields are nullable, and stay nullable under the type arguments an
//! instantiation gives.
//!
//! A OneOf input object is given exactly one of its fields, so each field is one of the
//! alternatives and none may be required: GraphQL refuses a field of a `@oneOf` input object that
//! is written non-null. Each is reported at its name, under the code the same rule has in GraphQL:
//! `smear-schema`'s `one-of-field-not-nullable`, with its label and its help. What is read is the
//! field's own outermost `!`: `[Int!]` is nullable.
//!
//! The same is met after substitution. A field whose type is its OneOf's own type parameter,
//! written nullable, is non-null in an instantiation that gives the parameter a non-null type:
//! `Choice<Int!>` of `input Choice<T> @oneOf { x: T }`. Such a parameter stands as the field's
//! whole type where its definition puts it, and is passed on to the definitions it is given to,
//! as the table of where each type parameter stands answers, but not through a list or a
//! collection, which stand there instead: `input L<T> @oneOf { x: [T] }` holds a nullable `[Int!]`
//! under `L<Int!>`. So each instantiation, every written type occurrence wherever it stands and
//! every directive application, by its directive's complete instantiation, gives such a parameter
//! a nullable type. The first argument, in order, that is non-null is reported, at the argument as
//! written, or at the occurrence, an application at its path, for one a default fills; labelled
//! where the parameter stands, and at the default that fills it. Each type is judged once, and
//! reported at each occurrence of it. Else, what the slots an occurrence leaves to their defaults
//! derive is judged the same way, at the occurrence, a type the occurrence or the program writes
//! being judged where it is written.
//!
//! A set's element and a map's key are non-null in every instantiation, however they are written.
//! So a collection's own definition that gives its key or element such a field's whole type makes
//! the field non-null in every instantiation: `scalar S<T> @set @d<T>` with
//! `directive @d<X>(v: Choice<X>)`. That is the definition's mistake, reported once, where the
//! definition gives the place: at the type argument or the `implements` entry, or at an
//! application's path for an argument a default fills; with a label at the field. No use of the
//! collection repeats it: the table keeps the place apart from the key or element, and passes it
//! on to nothing.
//!
//! An input object is a OneOf input object when the package that wrote it applies `core`'s
//! `@oneOf` to it, in its definition or in an extension of its own, as `one_of` answers. So a
//! field is judged whichever contribution writes it, and another package's `@oneOf` makes nothing
//! OneOf.
//!
//! The rule reads the public model, and one crate-private accessor, `Application::ty`: an
//! application's complete type, its directive's instantiation.

use std::{collections::BTreeMap, format};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationId, Collection, Contribution, Cx, DefId, Diagnostic, MemberId, MemberKind, Model,
  TyId, TyKind, TypeExprId, TypeExprKind, Visitor,
  code::schema,
  diagnostic::Excerpt,
  id::dense,
  positions::{Derived, Fill, Mistake, Place, Positions, Taking, parameter, taking},
};

use super::{Silence, one_of};

/// What GraphQL tells the author of a non-null field of a OneOf input object.
const HELP: &str = "every field of a `@oneOf` input object is one of the alternatives, so each one has to be \
   nullable.";

/// What the rule tells the author of a collection whose key or element a OneOf's field takes.
const FORCED_HELP: &str = "a set's element and a map's key are non-null however they are \
                           written, and every field of a `@oneOf` input object has to be \
                           nullable: do not pass the key or element to one.";

/// GraphQL's label for a non-null field of a OneOf input object.
const LABEL: &str = "field of a @oneOf input object is non-null";

/// Reports each field of a OneOf input object that is written non-null; each occurrence and
/// application whose type gives a type parameter that stands as such a field's whole type, or
/// whose fill gives one, a non-null type; and each place where a collection's own definition gives
/// its key or element such a field's whole type.
///
/// It finds where each type parameter stands in the `begin` hook, reads whether an input object is
/// OneOf in the `enter_def` hook, each input field's type in the `enter_member` hook, each
/// occurrence in the `type_expr` hook and each application in the `application` hook. It says
/// nothing about a field, an occurrence or an application whose contribution has its
/// `has_errors()` set, or for a written root one of the schema's: a phase already refused something
/// in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs, once per walk, the table of where each type parameter stands, which is linear in the
/// slots, the open types the members, union members, `implements` entries and applications reach
/// and their arguments, and one look at the schema's contributions; for each input object the walk
/// enters, one look at each of its applications, and for each input field one look at its type;
/// for each occurrence and application, one look at its type and, once for each distinct type, a
/// verdict that reads each argument at a slot that stands as a OneOf's field, and once for each
/// distinct type and count of written arguments, the types its fill derives, each once; and in a
/// collection's definition, for each occurrence and application, a lookup of what its key or
/// element was given there. It keeps, between two hooks, whether the definition the walk is in is
/// a OneOf input object, and for the walk the table, the silence and its verdicts, keyed by the
/// model's ids. Its messages quote names as excerpts, so each is bounded whatever the names'
/// lengths, and print no type.
#[derive(Default)]
pub(super) struct OneOfFieldNotNullable {
  /// Whether the definition the walk is in is a OneOf input object.
  one_of: bool,
  /// Whether it says nothing at an occurrence.
  silence: Silence,
  /// Where each type parameter stands.
  positions: Positions,
  /// Each type's own verdict, judged once: the slot of the first of its arguments, at a slot that
  /// stands as a OneOf's field, that is non-null.
  verdicts: BTreeMap<TyId, Option<u32>>,
  /// What each type's fills derive, judged once for each count of written arguments.
  derived: Derived<u32>,
}

/// What an occurrence or an application is refused for.
#[derive(Clone, Copy)]
enum Found {
  /// Its type's own failure, at this slot.
  Own(u32),
  /// A failure among what its fill derives.
  Derived(Fill<u32>),
}

impl Visitor for OneOfFieldNotNullable {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.one_of = false;
    self.silence = Silence::of(cx);
    self.positions = Positions::of(cx.model(), cx.sources());
    self.verdicts = BTreeMap::new();
    self.derived = Derived::default();
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
        .primary_label(LABEL)
        .help(HELP)
        .build(),
    );
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    if self.silence.silenced(cx) {
      return;
    }
    let model = cx.model();
    let occurrence = model.type_expr(ty);
    if let Some(report) = self.forced(model, cx.def(), occurrence.location()) {
      cx.report(report);
    }
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
    if let Some(report) = self.forced(model, cx.def(), applied.path_location()) {
      cx.report(report);
    }
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

impl OneOfFieldNotNullable {
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
    if let Some(slot) = verdict(model, positions, verdicts, ty) {
      return Some(Found::Own(slot));
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
    let (slot, derived) = match found {
      Found::Own(slot) => (slot, None),
      Found::Derived(Fill {
        slot: filled,
        ty: derived,
        verdict: slot,
      }) => (slot, Some((filled, derived))),
    };
    let Taking {
      def: taker,
      slot,
      lead,
      at,
      filled,
      ..
    } = taking(model, ty, written, whole, slot, derived)?;
    let field = model.member(self.positions.origin(taker, slot, Place::OneOfField)?);
    let name = Excerpt(parameter(model, taker, slot).as_bytes());
    let mut report = Diagnostic::builder(schema::ONE_OF_FIELD_NOT_NULLABLE, at)
      .message(format!(
        "{lead} a non-null type for its type parameter `{name}`, which is the type of the field \
         `{}` of the OneOf input object `{}`",
        Excerpt(field.name().as_bytes()),
        Excerpt(model.def(field.owner()).name().as_bytes())
      ))
      .primary_label(LABEL);
    if let Some(witness) = self.positions.witness(taker, slot, Place::OneOfField) {
      report = report.label(witness, format!("`{name}` stands here"));
    }
    if let Some(default) = filled {
      report = report.label(default, "filled from this default");
    }
    Some(report.help(HELP).build())
  }

  /// The diagnostic of the mistake of `def`'s definition, of `model`, at `at`, where the walk has
  /// reached a type argument, an application or an `implements` entry of the definition: the key
  /// or element of `def`, a collection, which every instantiation makes non-null, given there a
  /// type parameter that stands as a OneOf's field's whole type, which it makes non-null in every
  /// instantiation. `None` anywhere else: a definition's mistake is reported once, where it gives
  /// the key or element that place. One lookup.
  fn forced(&self, model: &Model, def: Option<DefId>, at: Location) -> Option<Diagnostic> {
    let def = def?;
    let collection = model.def(def).collection()?;
    let Mistake {
      at: witness,
      member,
      ..
    } = self.positions.forced(def, Place::OneOfField)?;
    if witness != at {
      return None;
    }
    let field = model.member(member?);
    let (kind, part) = match collection {
      Collection::Set => ("set", "element"),
      Collection::Map => ("map", "key"),
    };
    Some(
      Diagnostic::builder(schema::ONE_OF_FIELD_NOT_NULLABLE, at)
        .message(format!(
          "`{}`, the {part} of the {kind} `{}`, is non-null in every instantiation, and here it is \
           the type of the field `{}` of the OneOf input object `{}`",
          Excerpt(parameter(model, def, 0).as_bytes()),
          Excerpt(model.def(def).name().as_bytes()),
          Excerpt(field.name().as_bytes()),
          Excerpt(model.def(field.owner()).name().as_bytes())
        ))
        .primary_label(LABEL)
        .label(field.location(), "the field it is the type of")
        .help(FORCED_HELP)
        .build(),
    )
  }
}

/// The slot of the first argument of `ty`, of `model`, in order, at a slot that stands as a OneOf's
/// field by `positions`, that is non-null; `verdicts` keeps each type's answer.
fn verdict(
  model: &Model,
  positions: &Positions,
  verdicts: &mut BTreeMap<TyId, Option<u32>>,
  ty: TyId,
) -> Option<u32> {
  if let Some(kept) = verdicts.get(&ty) {
    return *kept;
  }
  let mut found: Option<u32> = None;
  if let TyKind::Named { def, args } = model.ty(ty).kind() {
    for (slot, argument) in args.iter().enumerate() {
      let slot = dense(slot);
      if model.ty(*argument).non_null() && positions.witness(def, slot, Place::OneOfField).is_some()
      {
        found = Some(slot);
        break;
      }
    }
  }
  verdicts.insert(ty, found);
  found
}
