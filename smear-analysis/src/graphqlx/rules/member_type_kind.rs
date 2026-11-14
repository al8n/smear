//! Types of the right kind: a member's type is of the kind its position takes, and so is what an
//! instantiation gives a type parameter where the parameter stands.
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
//! occurrence leaves to its default, at the occurrence, with a label at the default. A type
//! parameter is a type of every position, and so is a path that names nothing, which name
//! resolution refused. A marker is an interface: one as an argument's type is this rule's refusal,
//! and the marker rule's beside it.
//!
//! The same rule is met after substitution. A type parameter stands where its definition puts it,
//! through lists, collections and the definitions it is passed to, as the table of where each
//! type parameter stands answers; so each instantiation, every written type occurrence wherever it
//! stands, a type parameter's default, a bound and an `implements` entry among them, and every
//! directive application, by its directive's complete instantiation, gives each type parameter a
//! type of the class each place it stands in takes. The first argument, in order, that does not
//! fit a place its parameter stands in, the places taken as a field's type, a field argument's,
//! an input field's and a directive argument's, is reported under that place's code: at the
//! argument as written, down to the leaf of a collection, or at the occurrence, an application at
//! its path, for an argument a default fills; with a label where the parameter stands, and one at
//! the default that fills it. Each type is judged once, and reported at each occurrence of it.
//! Else, what the slots an occurrence leaves to their defaults derive is judged the same way, at
//! the occurrence: `Conn<In>` of `type Conn<N, E = Edge<N>>`, whose `E` is `Edge<In>`, whose
//! `node: N` is then an input object in a field. A type the occurrence writes, or that an
//! occurrence of the program writes, is passed over there: it is judged where it is written. And a
//! fill that leaves a slot to a default the rule refuses derives nothing to judge: the default is
//! refused where it is written, `Pair<N, Obj>` of `scalar Ref<N, E = Pair<N, Obj>>` at `Obj` when
//! `Pair`'s second parameter stands in an input field, and no occurrence of `Ref` that leaves `E`
//! out repeats it, though each derives a `Pair` of its own.
//!
//! A member is judged when a supplied package wrote its definition, a GraphQL built-in the root
//! package replaces among them, and so is a member's type occurrence; every other occurrence and
//! application the walk reaches is judged where it is written.
//!
//! The rule reads the public model, the public `Application::ty` among it: an application's
//! complete type, its directive's instantiation. Through `positions::Derived` it reads one
//! crate-private accessor, `Model::type_exprs`: every type occurrence of the model, whose types
//! are what the program writes.

use std::{collections::BTreeMap, format, string::String};

use tokora::diagnostic::{Code, Location};

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, DefId, DefKind, Diagnostic, Member, MemberId, MemberKind, Model,
  Site, TyId, TyKind, TypeExprId, TypeExprKind, Visitor,
  code::schema,
  diagnostic::Excerpt,
  id::dense,
  positions::{Class, Derived, Fill, Fits, Place, Positions, default_of, named, parameter},
};

use super::{Silence, supplied};

/// The positions whose places this rule judges an instantiation's arguments at, in the order it
/// reads them.
const POSITIONS: [Position; 4] = [
  Position::Field,
  Position::Argument,
  Position::InputField,
  Position::DirectiveArgument,
];

/// Reports each member of a definition a supplied package wrote whose type's base, under its
/// lists, names a definition its position does not take, or a collection that holds one; and each
/// occurrence and application whose type gives a type parameter, or whose fill gives one, a type
/// of a kind where the parameter stands does not take.
///
/// It finds where each type parameter stands in the `begin` hook, reads who wrote each definition
/// in the `enter_def` hook, each member's type in the `enter_member` hook, a field's arguments
/// among them, each occurrence in the `type_expr` hook and each application in the `application`
/// hook. It says nothing about a member, an occurrence or an application whose contribution has its
/// `has_errors()` set, or for a written root one of the schema's: a phase already refused something
/// in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs, once per walk, the table of where each type parameter stands, which is linear in the
/// slots, the open types the members, union members, `implements` entries and applications reach
/// and their arguments, and one look at the schema's contributions. For each member, one look at
/// its type and one step for each list its type writes; for each occurrence and application, one
/// look at its type and, once for each distinct type, a verdict that reads each argument against
/// each place its slot stands in, each type and class judged once for the walk; and once for each
/// distinct type and count of written arguments, the types its fill derives, each once, and once
/// for its definition, the types of its defaults. A report reads one answer kept for its written
/// occurrence: where under it what does not fit lies, made once for the walk for each occurrence
/// and class, from the answer of the child it writes on the way, or from its own type's where it
/// stops writing the way. So a written tree is read once, however many reports nest in it, and a
/// path a default fills is walked once, when its types are judged, and never for a report. Nothing
/// recurses. It keeps, between two hooks, whether a supplied package wrote the definition the walk
/// is in, and for the walk the table, the silence, its verdicts and those answers, each keyed by
/// the model's ids. Its messages quote names as excerpts, so each is bounded whatever the names'
/// lengths, and print no type.
#[derive(Default)]
pub(super) struct MemberTypeKind {
  /// Whether a supplied package wrote the definition the walk is in.
  judged: bool,
  /// Whether it says nothing at an occurrence.
  silence: Silence,
  /// Where each type parameter stands.
  positions: Positions,
  /// Whether the types it asked about fit the classes their positions take, and where, under each
  /// written occurrence a report asked about, what does not fit lies.
  fits: Fits,
  /// Each type's own verdict, judged once: the first of its arguments that does not fit a place
  /// its slot stands in.
  verdicts: BTreeMap<TyId, Option<Own>>,
  /// What each type's fills derive, judged once for each count of written arguments.
  derived: Derived<Own>,
}

/// A type's own failure: the first of its arguments, in order, that does not fit a place its slot
/// stands in, with that place's position.
#[derive(Clone, Copy)]
struct Own {
  /// The slot of the argument.
  slot: u32,
  /// The position whose place it does not fit.
  position: Position,
}

/// What an occurrence or an application is refused for.
#[derive(Clone, Copy)]
enum Found {
  /// Its type's own failure.
  Own(Own),
  /// A failure among what its fill derives.
  Derived(Fill<Own>),
}

impl Visitor for MemberTypeKind {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.judged = false;
    self.silence = Silence::of(cx);
    self.positions = Positions::of(cx.model(), cx.sources());
    self.fits = Fits::default();
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
        .misfit(model, base.ty(), position.class(), Some(base.id()))
      else {
        return;
      };
      let leaf = model.def(misfit.leaf);
      let at = misfit.at.unwrap_or(base.id());
      let mut report = Diagnostic::builder(position.code(), model.type_expr(at).location())
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

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    // A member's type is judged as its member is.
    if cx.site() == Some(Site::MemberType) && !self.judged {
      return;
    }
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

impl MemberTypeKind {
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
      fits,
      verdicts,
      derived,
      ..
    } = self;
    if let Some(own) = verdict(model, positions, fits, verdicts, ty) {
      return Some(Found::Own(own));
    }
    derived
      .verdict(model, ty, written, |next: TyId| {
        verdict(model, positions, fits, verdicts, next)
      })
      .map(Found::Derived)
  }

  /// The diagnostic of `found`, for an occurrence or an application whose type is `ty`, of `model`,
  /// that writes the type arguments `written`, `whole` being where it is written: the occurrence,
  /// or an application's path.
  fn report(
    &mut self,
    model: &Model,
    found: Found,
    ty: TyId,
    written: &[TypeExprId],
    whole: Location,
  ) -> Option<Diagnostic> {
    let TyKind::Named { def, args } = model.ty(ty).kind() else {
      return None;
    };
    let definition = Excerpt(model.def(def).name().as_bytes());
    // A directive is named as it is applied.
    let sign = if model.def(def).kind() == DefKind::Directive {
      "@"
    } else {
      ""
    };
    match found {
      Found::Own(Own { slot, position }) => {
        let given = written.get(slot as usize).copied();
        let argument = *args.get(slot as usize)?;
        let misfit = self.fits.misfit(model, argument, position.class(), given)?;
        let name = Excerpt(parameter(model, def, slot).as_bytes());
        let lead = match given {
          Some(_) => format!("`{sign}{definition}` takes"),
          None => format!("`{sign}{definition}` takes, from the default of `{name}`,"),
        };
        let message = phrase(
          model,
          &lead,
          misfit.leaf,
          misfit.nested && given.is_some(),
          misfit.nested,
          &name,
          position,
        );
        let at = match (given, misfit.at) {
          (Some(_), Some(at)) => model.type_expr(at).location(),
          _ => whole,
        };
        let mut report = Diagnostic::builder(position.code(), at)
          .message(message)
          .primary_label(position.label());
        if let Some(witness) = self.positions.witness(def, slot, position.place()) {
          report = report.label(witness, format!("`{name}` stands here"));
        }
        if given.is_none()
          && let Some(default) = default_of(model, def, slot)
        {
          report = report.label(default, "filled from this default");
        }
        if let Some(default) = misfit
          .filled
          .and_then(|(collection, index): (DefId, u32)| default_of(model, collection, index))
        {
          report = report.label(default, "filled from this default");
        }
        Some(report.help(position.help()).build())
      }
      Found::Derived(Fill {
        slot: filled,
        ty: derived,
        verdict: Own { slot, position },
      }) => {
        let TyKind::Named {
          def: derived_def,
          args: derived_args,
        } = model.ty(derived).kind()
        else {
          return None;
        };
        let argument = *derived_args.get(slot as usize)?;
        let misfit = self.fits.misfit(model, argument, position.class(), None)?;
        let name = Excerpt(parameter(model, derived_def, slot).as_bytes());
        let lead = format!(
          "the default of `{}` of `{sign}{definition}` makes `{}` take",
          Excerpt(parameter(model, def, filled).as_bytes()),
          Excerpt(model.def(derived_def).name().as_bytes())
        );
        let message = phrase(
          model,
          &lead,
          misfit.leaf,
          false,
          misfit.nested,
          &name,
          position,
        );
        let mut report = Diagnostic::builder(position.code(), whole)
          .message(message)
          .primary_label(position.label());
        if let Some(witness) = self.positions.witness(derived_def, slot, position.place()) {
          report = report.label(witness, format!("`{name}` stands here"));
        }
        if let Some(default) = default_of(model, def, filled) {
          report = report.label(default, "filled from this default");
        }
        Some(report.help(position.help()).build())
      }
    }
  }
}

/// The first argument of `ty`, of `model`, in order, that does not fit a place its slot stands in
/// by `positions`, places read in [`POSITIONS`]' order, `fits` judging each type once and
/// `verdicts` keeping each type's answer.
fn verdict(
  model: &Model,
  positions: &Positions,
  fits: &mut Fits,
  verdicts: &mut BTreeMap<TyId, Option<Own>>,
  ty: TyId,
) -> Option<Own> {
  if let Some(kept) = verdicts.get(&ty) {
    return *kept;
  }
  let mut found: Option<Own> = None;
  if let TyKind::Named { def, args } = model.ty(ty).kind() {
    'arguments: for (slot, argument) in args.iter().enumerate() {
      let slot = dense(slot);
      for position in POSITIONS {
        if positions.witness(def, slot, position.place()).is_some()
          && !fits.fits(model, *argument, position.class())
        {
          found = Some(Own { slot, position });
          break 'arguments;
        }
      }
    }
  }
  verdicts.insert(ty, found);
  found
}

/// The message for an argument a type parameter `name` takes that does not fit where it stands in
/// `position`, of `model`: `lead`, which names who takes it, then what does not fit, of the
/// definition `leaf`, named before the parameter, or after it when `after` is set, as lying in a
/// collection when `nested` is set.
fn phrase(
  model: &Model,
  lead: &str,
  leaf: DefId,
  after: bool,
  nested: bool,
  name: &Excerpt<'_>,
  position: Position,
) -> String {
  let leaf = model.def(leaf);
  let kind = named(leaf.kind());
  let leaf = Excerpt(leaf.name().as_bytes());
  let thing = if nested {
    format!("a collection whose argument is {kind} `{leaf}`")
  } else {
    format!("{kind} `{leaf}`")
  };
  let stands = position.stands();
  let takes = position.takes();
  if after {
    format!(
      "{lead}, for its type parameter `{name}`, which stands as {stands}, {thing}, and {takes}"
    )
  } else {
    format!("{lead} {thing} for its type parameter `{name}`, which stands as {stands}, and {takes}")
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

  /// The place a type parameter stands in when it stands in this position.
  const fn place(self) -> Place {
    match self {
      Self::Field => Place::Field,
      Self::Argument => Place::Argument,
      Self::DirectiveArgument => Place::DirectiveArgument,
      Self::InputField => Place::InputField,
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

  /// What a type parameter that stands there stands as, in a message.
  fn stands(self) -> &'static str {
    match self {
      Self::Field => "a field's type",
      Self::Argument => "a field argument's type",
      Self::DirectiveArgument => "a directive argument's type",
      Self::InputField => "an input field's type",
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
