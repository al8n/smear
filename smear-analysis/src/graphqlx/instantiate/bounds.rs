//! The instantiation phase's last step: the bounds of every instantiation the program writes,
//! judged exactly.
//!
//! It runs once every occurrence has its type and every application its count: judging an
//! occurrence reads the types of its definition's bounds and of its argument's `implements`
//! entries, which are typed later than the occurrence whenever they are written later, and a
//! definition's defaults are typed before its bounds.
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
//! An instantiation that only a fill derives is not judged: the default's own occurrence is judged
//! where it is written. That misses nothing given that every type declares what its declared
//! interfaces declare, under substitution: a derived instantiation then holds whenever the
//! occurrence it comes from holds and the use's own bounds hold, since substitution keeps identity.
//! Without that premise, one step lets an ill-bounded derived instantiation through: the one by
//! which a parameter satisfies what the interface of one of its bounds declares. An application's
//! own bounds are not judged here: its complete type arguments are built where its values are
//! coerced.
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
//! writes. The types the pass derives are interned after every occurrence's type, in the order the
//! pass reaches the occurrences.
//!
//! # What it costs
//!
//! Each distinct type of an occurrence is judged once, whatever nullness it is written with: a
//! type's two nullnesses share one verdict, which reads the instantiation's definition and its
//! arguments alone. Each pair of an argument and an interface type is asked once. Every step beyond
//! one lookup per occurrence is paid from `Limits::max_bound_steps`: a bound taken under an
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
//! either way.

use std::{collections::BTreeMap, format, string::String, vec::Vec};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  code::DiagnosticKind,
  diagnostic::{Diagnostic, Excerpt},
  id::{DefId, TyId, TypeExprId, dense},
  model::{DefKind, Keys, Occurrence, Shape, Steps, TypeBound, Written},
};

use super::{Limit, Typing};

/// What kept an instantiation's bounds from being judged.
#[derive(Clone, Copy)]
enum Stop {
  /// `Limits::max_bound_steps`.
  Steps,
  /// A type a bound rebuilt under the instantiation's arguments would derive passes this limit.
  Built(Limit),
}

/// What an instantiation's bounds come to.
#[derive(Clone, Copy)]
enum Verdict {
  /// Every bound holds.
  Holds,
  /// The bound at this index of its definition's, the first in their order that does not hold.
  Fails { bound: u32 },
  /// A limit stopped the judging before it was known.
  Stopped(Stop),
}

/// What the pass keeps from one occurrence to the next, beside the walk's own state.
struct Judging {
  /// The built-in definitions satisfaction is keyed on.
  keys: Keys,
  /// `Limits::max_bound_steps`.
  max: u32,
  /// The steps spent.
  spent: u32,
  /// Each type an occurrence of a definition with a bound has, and the nullable form of each that
  /// is written non-null only, with what its bounds come to. Keyed by the type, which the loop
  /// looks up for each occurrence.
  verdicts: BTreeMap<TyId, Verdict>,
  /// Each pair of an argument and an interface type asked, with whether the argument satisfies
  /// it. Keyed by the pair, which a verdict looks up before it asks the routine.
  pairs: BTreeMap<(TyId, TyId), bool>,
  /// While one bound is rebuilt: each open node of its type rebuilt, with what it became. Keyed by
  /// the node, which the rebuild looks up for each child it meets; cleared for each bound.
  rebuilt: BTreeMap<TyId, TyId>,
  /// While one bound is rebuilt: the nodes being rebuilt, each with its next child to look at.
  frames: Vec<(TyId, u32)>,
}

impl Judging {
  /// Spends `steps`, or spends none and answers that the budget cannot pay them.
  fn charge(&mut self, steps: u32) -> bool {
    match self.spent.checked_add(steps) {
      Some(spent) if spent <= self.max => {
        self.spent = spent;
        true
      }
      _ => false,
    }
  }

  /// How many steps the budget has left.
  fn left(&self) -> u32 {
    self.max.saturating_sub(self.spent)
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
  /// Judges the bounds of every instantiation the program writes, keyed on `keys`, within
  /// `max_steps` steps over the whole pass, and reports each occurrence of one that fails or that
  /// a limit stopped.
  pub(super) fn bounds(&mut self, keys: Keys, max_steps: u32) {
    let mut judging = Judging {
      keys,
      max: max_steps,
      spent: 0,
      verdicts: BTreeMap::new(),
      pairs: BTreeMap::new(),
      rebuilt: BTreeMap::new(),
      frames: Vec::new(),
    };
    for index in 0..self.builder.type_expr_count() {
      self.judge_occurrence(&mut judging, TypeExprId::new(index));
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
      Verdict::Fails { bound } => self.report_unsatisfied(id, ty, def, bound),
      Verdict::Stopped(stop) => self.report_unjudged(location, stop, judging.max),
    }
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
      if !judging.charge(1) {
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
        match self.rebuild(judging, written, ty, def) {
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
          let left = judging.left();
          let mut steps = Steps::counted(left);
          let answer = self
            .builder
            .judge(&judging.keys, argument, goal, &mut steps);
          // What the routine took, which the budget had left: it never takes more.
          let taken = left.saturating_sub(steps.left());
          judging.spent = judging.spent.saturating_add(taken);
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

  /// `bound`, the interface type of a bound of `def`, with the arguments of `ty`, an instantiation
  /// of `def`, in place of `def`'s type parameters, each as it is: interned, a node's arguments or
  /// element before it, each distinct node of `bound` once. One step for the root, and one for each
  /// argument or element of a node rebuilt, as it is looked at.
  ///
  /// `Err` when a limit stopped it; `Ok(None)` for a type parameter of another definition, which a
  /// bound cannot name, and which passes the bound over.
  fn rebuild(
    &mut self,
    judging: &mut Judging,
    bound: TyId,
    ty: TyId,
    def: DefId,
  ) -> Result<Option<TyId>, Stop> {
    judging.rebuilt.clear();
    judging.frames.clear();
    if !judging.charge(1) {
      return Err(Stop::Steps);
    }
    judging.frames.push((bound, 0));
    while let Some(&(node, next)) = judging.frames.last() {
      let child = match self.builder.ty_shape(node) {
        Some(Shape::Named(_)) => self.builder.ty_args(node).get(next as usize).copied(),
        Some(Shape::List(element)) => (next == 0).then_some(element),
        Some(Shape::Param { .. } | Shape::Unresolved) | None => None,
      };
      if let Some(child) = child {
        if let Some(frame) = judging.frames.last_mut() {
          frame.1 = next.saturating_add(1);
        }
        // Looked at, whether it is closed, rebuilt already or still to do.
        if !judging.charge(1) {
          return Err(Stop::Steps);
        }
        if !self.builder.ty_closed(child) && !judging.rebuilt.contains_key(&child) {
          judging.frames.push((child, 0));
        }
        continue;
      }
      judging.frames.pop();
      let Some(made) = self.rebuilt_node(&judging.rebuilt, node, ty, def)? else {
        return Ok(None);
      };
      if judging.frames.is_empty() {
        return Ok(Some(made));
      }
      judging.rebuilt.insert(node, made);
    }
    Ok(None)
  }

  /// What `node`, an open node of a bound of `def` whose arguments or element `rebuilt` holds
  /// already, becomes with the arguments of `ty`, an instantiation of `def`, in place of `def`'s
  /// type parameters.
  fn rebuilt_node(
    &mut self,
    rebuilt: &BTreeMap<TyId, TyId>,
    node: TyId,
    ty: TyId,
    def: DefId,
  ) -> Result<Option<TyId>, Stop> {
    let non_null = self.builder.ty_non_null(node);
    match self.builder.ty_shape(node) {
      // The argument for the parameter, as it is, its `!` included; written `T!`, its non-null
      // form, which a collection's key, already non-null, is.
      Some(Shape::Param { def: owner, index }) => {
        debug_assert!(
          owner == def,
          "a bound names its own definition's type parameters"
        );
        if owner != def {
          return Ok(None);
        }
        let Some(argument) = self.builder.ty_args(ty).get(index as usize).copied() else {
          return Ok(None);
        };
        Ok(Some(if non_null {
          self.non_null_of(argument)
        } else {
          argument
        }))
      }
      Some(Shape::List(element)) => {
        let element = if self.builder.ty_closed(element) {
          element
        } else {
          match rebuilt.get(&element) {
            Some(&made) => made,
            None => return Ok(None),
          }
        };
        let depth = self.builder.ty_depth(element).saturating_add(1);
        if !self.library && depth > self.max_depth {
          return Err(Stop::Built(Limit::Depth));
        }
        Ok(Some(self.builder.intern_list(element, non_null)))
      }
      // Completed as an occurrence's type is from its slots, and recorded as no instantiation:
      // the program does not write it.
      Some(Shape::Named(named)) => {
        self.slots.clear();
        let args: &[TyId] = self.builder.ty_args(node);
        for &arg in args {
          let slot = if self.builder.ty_closed(arg) {
            arg
          } else {
            match rebuilt.get(&arg) {
              Some(&made) => made,
              None => return Ok(None),
            }
          };
          self.slots.push(slot);
        }
        let nullable = self.instantiation(named).map_err(Stop::Built)?;
        Ok(Some(if non_null {
          self.non_null_of(nullable)
        } else {
          nullable
        }))
      }
      Some(Shape::Unresolved) | None => Ok(None),
    }
  }

  /// Reports `unsatisfied-bound` at the occurrence `id`, whose type `ty`, an instantiation of
  /// `def`, fails `def`'s bound at `bound`: naming the definition, the parameter and the bound's
  /// interface, labelled at the bound and at the type argument, where the occurrence writes it or
  /// at the default that filled it.
  fn report_unsatisfied(&mut self, id: TypeExprId, ty: TyId, def: DefId, bound: u32) {
    let Some(written) = self.builder.def_bounds(def).get(bound as usize).copied() else {
      return;
    };
    let parameter = written.parameter();
    let Some(occurrence) = self.builder.occurrence(id) else {
      return;
    };
    let at = occurrence.location;
    let argument = occurrence.args.get(parameter as usize).copied();
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
    // The type argument where this occurrence writes it, or else the default that filled it.
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
        "`{}` bounds its type parameter `{spelled}` by `{}`, and the type argument for `{spelled}` \
         here does not satisfy that bound",
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
  /// judged, `max_steps` being `Limits::max_bound_steps`.
  fn report_unjudged(&mut self, at: Location, stop: Stop, max_steps: u32) {
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
          self.max_depth
        ),
        "raise the bound with `Limits::with_max_generic_depth`, or nest types less deeply",
      ),
      Stop::Built(Limit::Instantiations) => (
        format!(
          "the bounds of this instantiation could not be judged: a bound put under its type \
           arguments is one more instantiation of a generic definition than \
           `Limits::max_instantiations`, {}, admits",
          self.max_instantiations
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
          self.max_substitutions
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
