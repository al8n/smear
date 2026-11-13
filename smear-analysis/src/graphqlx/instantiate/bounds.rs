//! The instantiation phase's bounds: those of every instantiation the program writes, and those of
//! every directive at each of its applications, judged exactly.
//!
//! The occurrences' are judged once every occurrence has its type and every application its count:
//! judging an occurrence reads the types of its definition's bounds and of its argument's
//! `implements` entries, which are typed later than the occurrence whenever they are written
//! later, and a definition's defaults are typed before its bounds. The applications' are judged
//! last, once their type arguments are complete.
//!
//! # What is judged
//!
//! Every written occurrence of a definition that has a bound, by its complete type, wherever it
//! stands: a member's type, an argument of another occurrence, a default, a bound's interface, an
//! `implements` entry, a union member, a root, an application's type argument. A slot a default
//! filled is judged like a written one. An occurrence that names a parameter of the definition it
//! is written in is judged through that parameter's bounds, so a generic definition is judged once,
//! where it is written, for all its instantiations.
//!
//! Then every application of a directive that has a bound, by its type, the directive's
//! instantiation with the application's complete type arguments, a slot a default filled
//! included: an application is an instantiation of its directive. One inside a generic definition
//! whose type arguments name that definition's parameters is judged through their bounds, as an
//! occurrence there is.
//!
//! An instantiation that only a fill derives is not judged: the default's own occurrence is judged
//! where it is written. That misses nothing given that every type declares what its declared
//! interfaces declare, under substitution: a derived instantiation then holds whenever the
//! occurrence it comes from holds and the use's own bounds hold, since substitution keeps identity.
//! Without that premise, one step lets an ill-bounded derived instantiation through: the one by
//! which a parameter satisfies what the interface of one of its bounds declares.
//!
//! # A bound, under an instantiation's arguments
//!
//! For each bound of the definition, in the order they are written: its interface type, with the
//! instantiation's arguments in place of the definition's type parameters, each taken as it is,
//! its `!` included; and the argument for the bounded parameter, which must satisfy that interface
//! type, read off what the argument declares by the routine `Model::satisfies_ty` answers with.
//! The first bound that fails is the verdict, and no other is looked at. A map's or a set's bound
//! on its key that a `NonZero` fails is not one: `NonZero` of an integer is a key, and the rule
//! that refuses a `NonZero` of anything else says so where it is written.
//!
//! A bound's interface type that names a parameter is rebuilt from its interned type, node by node,
//! its arguments before it, each distinct node once; a type it derives is completed as an
//! occurrence's is, its depth checked and its count kept, and is no instantiation the program
//! writes. The types the pass derives for the occurrences are interned after every occurrence's
//! type, in the order the pass reaches the occurrences; those it derives for the applications,
//! after every application's own type, in application order.
//!
//! # What it costs
//!
//! Each distinct type of an occurrence or an application is judged once, whatever nullness it is
//! written with: a type's two nullnesses share one verdict, which reads the instantiation's
//! definition and its arguments alone, and every application of one type shares it too. Each pair
//! of an argument and an interface type is asked once. Every step beyond one lookup per occurrence
//! and per application is paid from `Limits::max_bound_steps`: a bound taken under an
//! instantiation's arguments, a node of a bound rebuilt, an `implements` entry or a parameter's
//! bound looked at, a pair of nodes or an argument compared. What the rebuild derives also counts
//! toward `Limits::max_instantiations` and `Limits::max_generic_depth`. Nothing draws on
//! `Limits::max_substitutions`, which counts filling alone.
//!
//! # What is reported
//!
//! At each occurrence of an instantiation whose verdict fails, `unsatisfied-bound`, naming the
//! definition, the parameter and the bound's interface, with a label at the bound and one at the
//! type argument, where the occurrence writes it or at the default that filled it. At each
//! occurrence of one a limit stopped judging, `resource-limit`. The occurrence keeps its type
//! either way. An application is reported so at its path, its directive named with its `@`, the
//! type argument labelled where the application writes it or at the directive's default that
//! filled it.

use std::{collections::BTreeMap, format, string::String};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  code::DiagnosticKind,
  diagnostic::{Diagnostic, Excerpt},
  id::{ApplicationId, DefId, TyId, TypeExprId, dense},
  model::{DefKind, Keys, Occurrence, Resolution, Shape, Steps, TypeBound, Written},
  types::{Limit, Stop},
};

use super::Typing;

/// What an instantiation's bounds come to.
#[derive(Clone, Copy)]
enum Verdict {
  /// Every bound holds.
  Holds,
  /// The bound at this index of its definition's, the first in their order that does not hold.
  Fails { bound: u32 },
  /// A limit stopped the judging before it was known: `Limits::max_bound_steps`, or a limit on a
  /// type a bound rebuilt under the instantiation's arguments would derive.
  Stopped(Stop),
}

/// What the pass keeps from one instantiation to the next, the occurrences' and then the
/// applications', beside the walk's own state and the type services, whose budget of bound steps
/// it spends.
pub(super) struct Judging {
  /// The built-in definitions satisfaction is keyed on.
  keys: Keys,
  /// Each type an occurrence of a definition with a bound has, and the nullable form of each that
  /// is written non-null only, with what its bounds come to. Keyed by the type, which the loop
  /// looks up for each occurrence.
  verdicts: BTreeMap<TyId, Verdict>,
  /// Each pair of an argument and an interface type asked, with whether the argument satisfies
  /// it. Keyed by the pair, which a verdict looks up before it asks the routine.
  pairs: BTreeMap<(TyId, TyId), bool>,
}

impl Judging {
  /// The pass's state before any instantiation is judged, keyed on `keys`.
  pub(super) const fn new(keys: Keys) -> Self {
    Self {
      keys,
      verdicts: BTreeMap::new(),
      pairs: BTreeMap::new(),
    }
  }
}

/// What a bound's report tells the author of a named type argument.
const NAMED_HELP: &str = "declare the interface on the type argument with exactly these type \
                          arguments, or pass a type that declares it";

/// What a bound's report tells the author of a type argument that is a type parameter.
const PARAM_HELP: &str = "bound this type parameter by the interface in this definition's `where` \
                          clause";

/// What a bound's report tells the author of a type argument that is a list.
const LIST_HELP: &str = "a list satisfies no bound: pass a type that declares the interface";

impl Typing<'_> {
  /// Judges the bounds of every instantiation the program writes, within the budget of bound
  /// steps, and reports each occurrence of one that fails or that a limit stopped.
  pub(super) fn bounds(&mut self, judging: &mut Judging) {
    for index in 0..self.builder.type_expr_count() {
      self.judge_occurrence(judging, TypeExprId::new(index));
    }
  }

  /// Judges the bounds of every application's directive, by the application's type, within the
  /// same budget, and reports each application whose type fails one or that a limit stopped. It
  /// runs once the applications' type arguments are complete.
  pub(super) fn application_bounds(&mut self, judging: &mut Judging) {
    for index in 0..self.builder.application_count() {
      self.judge_application(judging, ApplicationId::new(index));
    }
  }

  /// Judges the application `application`, when its directive has a bound and its type is that
  /// directive's: one lookup of its type's verdict, which is made the first time it is asked, and
  /// shared with every other application of the type.
  fn judge_application(&mut self, judging: &mut Judging, application: ApplicationId) {
    let Some((Resolution::Resolved(directive), _, at)) = self.builder.applied(application) else {
      return;
    };
    // A directive with no type parameter, an application whose count was refused, and one that
    // could not be completed have no type of the directive: whoever left it so reported it.
    let Some(ty) = self.builder.application_ty(application) else {
      return;
    };
    if self.builder.def_bounds(directive).is_empty() {
      return;
    }
    match self.builder.ty_shape(ty) {
      Some(Shape::Named(named)) if named == directive => {}
      _ => return,
    }
    match self.verdict(judging, ty, directive, at) {
      Verdict::Holds => {}
      Verdict::Fails { bound } => {
        let written = self.written_argument(
          self.builder.application_type_args(application),
          directive,
          bound,
        );
        self.report_unsatisfied(at, written, "@", ty, directive, bound);
      }
      Verdict::Stopped(stop) => self.report_unjudged(at, stop),
    }
  }

  /// Judges the occurrence `id`, when it names a definition that has a bound and its type is that
  /// definition's: one lookup of its type's verdict, which is made the first time it is asked.
  fn judge_occurrence(&mut self, judging: &mut Judging, id: TypeExprId) {
    let Some(occurrence) = self.builder.occurrence(id) else {
      return;
    };
    let Written::Named(def) = occurrence.written else {
      return;
    };
    // Every occurrence has a type once the fourth step has ended: `None`, no type yet, is not met.
    let Some(ty) = occurrence.ty else {
      return;
    };
    let location = occurrence.location;
    if self.builder.def_bounds(def).is_empty() {
      return;
    }
    // An occurrence whose type is the unresolved one is skipped: whoever left it so reported it.
    match self.builder.ty_shape(ty) {
      Some(Shape::Named(named)) if named == def => {}
      _ => return,
    }
    match self.verdict(judging, ty, def, location) {
      Verdict::Holds => {}
      Verdict::Fails { bound } => {
        let written = self
          .builder
          .occurrence(id)
          .and_then(|occurrence: Occurrence<'_>| {
            self.written_argument(occurrence.args, def, bound)
          });
        self.report_unsatisfied(location, written, "", ty, def, bound);
      }
      Verdict::Stopped(stop) => self.report_unjudged(location, stop),
    }
  }

  /// The type argument of `args`, the type arguments an occurrence or an application of `def`
  /// writes, for the parameter `def`'s bound at `bound` bounds: none when it is left to a default.
  fn written_argument(&self, args: &[TypeExprId], def: DefId, bound: u32) -> Option<TypeExprId> {
    let bounded = self.builder.def_bounds(def).get(bound as usize)?;
    args.get(bounded.parameter() as usize).copied()
  }

  /// The verdict of `ty`, an instantiation of `def` an occurrence written at `location` has: kept
  /// once made, and shared by the type's two nullnesses.
  fn verdict(
    &mut self,
    judging: &mut Judging,
    ty: TyId,
    def: DefId,
    location: Location,
  ) -> Verdict {
    if let Some(&verdict) = judging.verdicts.get(&ty) {
      return verdict;
    }
    // The nullable form of a non-null instantiation was interned before it, and is found, once
    // for each distinct non-null type.
    let nullable = if self.builder.ty_non_null(ty) {
      self.builder.intern_nullness(ty, false)
    } else {
      ty
    };
    let verdict = match judging.verdicts.get(&nullable).copied() {
      Some(verdict) => verdict,
      None => {
        // What the rebuild completes is checked and counted as the occurrence's own tree would be.
        self.library = location.source() < self.supplied;
        debug_assert!(
          !self.library,
          "the built-in library writes no instantiation of a definition with a bound"
        );
        let verdict = self.judged(judging, nullable, def);
        judging.verdicts.insert(nullable, verdict);
        verdict
      }
    };
    judging.verdicts.insert(ty, verdict);
    verdict
  }

  /// What the bounds of `ty`, a nullable instantiation of `def`, come to: each in the definition's
  /// order, one step for each taken under `ty`'s arguments, until one fails.
  fn judged(&mut self, judging: &mut Judging, ty: TyId, def: DefId) -> Verdict {
    let count = dense(self.builder.def_bounds(def).len());
    for index in 0..count {
      if !self.types.bound_steps.charge(1) {
        return Verdict::Stopped(Stop::Steps);
      }
      let Some(bound) = self.builder.def_bounds(def).get(index as usize).copied() else {
        break;
      };
      let Some((interface, written)) = self.interface_of(bound) else {
        continue;
      };
      // The interface type the argument must satisfy: the bound's own when it names no parameter.
      let goal = if self.builder.ty_closed(written) {
        written
      } else {
        let types = &mut *self.types;
        let rebuilt = types.derived.rebuild(
          self.builder,
          &mut types.bound_steps,
          self.library,
          written,
          ty,
          def,
        );
        match rebuilt {
          Ok(Some(goal)) => goal,
          Ok(None) => continue,
          Err(stop) => return Verdict::Stopped(stop),
        }
      };
      let Some(argument) = self
        .builder
        .ty_args(ty)
        .get(bound.parameter() as usize)
        .copied()
      else {
        continue;
      };
      let holds = match judging.pairs.get(&(argument, goal)).copied() {
        Some(holds) => holds,
        None => {
          let left = self.types.bound_steps.left();
          let mut steps = Steps::counted(left);
          let answer = self
            .builder
            .judge(&judging.keys, argument, goal, &mut steps);
          // What the routine took, which the budget had left: it never takes more.
          self
            .types
            .bound_steps
            .take(left.saturating_sub(steps.left()));
          let Some(holds) = answer else {
            return Verdict::Stopped(Stop::Steps);
          };
          judging.pairs.insert((argument, goal), holds);
          holds
        }
      };
      if holds {
        continue;
      }
      // A `NonZero` that is no key holds something other than an integer, which the rule on
      // `NonZero`'s argument refuses where it is written: with an integer in its place this bound
      // holds.
      let non_zero = matches!(
        self.builder.ty_shape(argument),
        Some(Shape::Named(of)) if judging.keys.non_zero == Some(of)
      );
      if non_zero && judging.keys.map_key == Some(interface) {
        continue;
      }
      return Verdict::Fails { bound: index };
    }
    Verdict::Holds
  }

  /// The interface `bound`'s right side names, and its type, when it names an interface and its
  /// type is a named type. A right side that names none was refused where it is written, and one
  /// that could not be typed was reported where it is written: either way the bound is passed over.
  fn interface_of(&self, bound: TypeBound) -> Option<(DefId, TyId)> {
    let occurrence = self.builder.occurrence(bound.interface())?;
    let Written::Named(interface) = occurrence.written else {
      return None;
    };
    let written = occurrence.ty?;
    if self.builder.def_kind(interface) != Some(DefKind::Interface) {
      return None;
    }
    match self.builder.ty_shape(written) {
      Some(Shape::Named(_)) => Some((interface, written)),
      _ => None,
    }
  }

  /// Reports `unsatisfied-bound` at `at`, where an occurrence of `def` is written, or an
  /// application of the directive `def`, `sigil` being what names `def` there, whose type `ty`, an
  /// instantiation of `def`, fails `def`'s bound at `bound`: naming the definition, the parameter
  /// and the bound's interface, labelled at the bound and at the type argument, at `argument` where
  /// it is written, or else at the default that filled it.
  fn report_unsatisfied(
    &mut self,
    at: Location,
    argument: Option<TypeExprId>,
    sigil: &str,
    ty: TyId,
    def: DefId,
    bound: u32,
  ) {
    let Some(written) = self.builder.def_bounds(def).get(bound as usize).copied() else {
      return;
    };
    let parameter = written.parameter();
    let Some(bounding) = self.builder.occurrence(written.interface()) else {
      return;
    };
    let Written::Named(interface) = bounding.written else {
      return;
    };
    let bounding = bounding.location;
    let Some((name, _, default)) = self.builder.type_param(def, parameter) else {
      return;
    };
    // The type argument where it is written, or else the default that filled it.
    let argument = match argument {
      Some(argument) => self
        .builder
        .occurrence(argument)
        .map(|typed: Occurrence<'_>| (typed.location, "the type argument")),
      None => default
        .and_then(|root: TypeExprId| self.builder.occurrence(root))
        .map(|filled: Occurrence<'_>| (filled.location, "filled from this default")),
    };
    let help = match self
      .builder
      .ty_args(ty)
      .get(parameter as usize)
      .and_then(|argument: &TyId| self.builder.ty_shape(*argument))
    {
      Some(Shape::Param { .. }) => PARAM_HELP,
      Some(Shape::List(_)) => LIST_HELP,
      Some(Shape::Named(_) | Shape::Unresolved) | None => NAMED_HELP,
    };
    let message = {
      let spelled = Excerpt(self.builder.spelling(name));
      format!(
        "`{sigil}{}` bounds its type parameter `{spelled}` by `{}`, and the type argument for \
         `{spelled}` here does not satisfy that bound",
        Excerpt(self.builder.def_name(def)),
        Excerpt(self.builder.def_name(interface)),
      )
    };
    let kind = DiagnosticKind::UnsatisfiedBound;
    let mut report = Diagnostic::builder(kind.code(), at)
      .severity(kind.severity())
      .message(message)
      .primary_label(kind.title())
      .label(bounding, "the bound")
      .help(help);
    if let Some((written_at, text)) = argument {
      report = report.label(written_at, text);
    }
    self.reporter.report(report.build());
  }

  /// Reports, at `at`, an occurrence of an instantiation whose bounds `stop` kept from being
  /// judged.
  fn report_unjudged(&mut self, at: Location, stop: Stop) {
    let max_steps = self.types.bound_steps.max();
    let limits = self.types.limits();
    let (message, help): (String, &'static str) = match stop {
      Stop::Steps => (
        format!(
          "the bounds of this instantiation could not be judged within `Limits::max_bound_steps`, \
           {max_steps}, the steps judging bounds may take"
        ),
        "raise the bound with `Limits::with_max_bound_steps`, or write fewer instantiations of \
         definitions with bounds",
      ),
      Stop::Built(Limit::Depth) => (
        format!(
          "the bounds of this instantiation could not be judged: a bound put under its type \
           arguments nests deeper than `Limits::max_generic_depth`, {}, admits",
          limits.max_generic_depth()
        ),
        "raise the bound with `Limits::with_max_generic_depth`, or nest types less deeply",
      ),
      Stop::Built(Limit::Instantiations) => (
        format!(
          "the bounds of this instantiation could not be judged: a bound put under its type \
           arguments is one more instantiation of a generic definition than \
           `Limits::max_instantiations`, {}, admits",
          limits.max_instantiations()
        ),
        "raise the bound with `Limits::with_max_instantiations`, or instantiate fewer types",
      ),
      // The match's arm alone: `instantiation()` refuses a type for its depth or for the count of
      // instantiations, and never for this limit, which counts filling and which judging never
      // draws on. No program reaches it.
      Stop::Built(Limit::Substitutions) => (
        format!(
          "the bounds of this instantiation could not be judged within \
           `Limits::max_substitutions`, {}",
          limits.max_substitutions()
        ),
        "raise the bound with `Limits::with_max_substitutions`",
      ),
    };
    let kind = DiagnosticKind::ResourceLimit;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help(help)
        .build(),
    );
  }
}
