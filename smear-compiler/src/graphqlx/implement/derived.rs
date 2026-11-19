//! Derived instantiations, design Z's D11: an instantiation's entries and members, and the
//! arguments it makes required, at each instantiation the closure coercion built derives, reported
//! at the written occurrence that derives it.
//!
//! Each instantiation the closure derives whose definition does not grow is judged as an
//! occurrence's type is, by the same verdicts, kept by type and paid from the phase's budget: its
//! distinct written entries and members compared under its arguments, when its definition is
//! listed; and the arguments its definition's fields add beside their interfaces' fields, typed by
//! its own bare nullable type parameter, which it makes required where its argument for that
//! parameter is non-null. Each occurrence of a root a supplied package writes, and each application
//! of one, in id order, whose own verdict holds, reports the failing instantiation of least index
//! its expansion reaches, as the closure's blame finds it: two entries, or two members, of one type
//! there, `duplicate-implements-interface` or `duplicate-union-member`, at most one of each,
//! labelled at the type or the application that derives the instantiation and at the two; an
//! argument made required, `unexpected-required-argument`, labelled there, at the argument and at
//! the interface's field; or a stop, `resource-limit`. A derived instantiation of a definition that
//! grows is not judged, nor is what only it derives. The report pass reads each occurrence and
//! application once, finding its root by its type, whose nullable form is found once for each type
//! and kept: so repeated occurrences of a wide type cost a lookup each.
//!
//! What was reported already is not reported again. A comparison stopped by the rebuild that
//! stopped the closure is coercion's to report, which it did once. And a failure passes from an
//! instantiation to the one that derives it only where it was derived there: not where the
//! deriver's own arguments hold it, which whatever derived the deriver reaches too; nor where the
//! type at that place holds an open instantiation that failed the same way where it is written,
//! which reported it, and that the deriver's arguments make the failing one. What closing a type
//! alone introduces is reported at each root that reaches it. Reading the deriver's arguments and
//! the type at a place is a step of the phase's budget for each type, each read once.

use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec::Vec,
};

use tokora::diagnostic::Location;

use super::{Comparison, Implementing, Latent, List, Made, Twin, Verdict, labelled};
use crate::graphqlx::{
  code::schema,
  diagnostic::{Diagnostic, Excerpt},
  id::{ApplicationId, DefId, PoolRange, TyId, TypeExprId, dense},
  model::{Applied, DefKind, ModelBuilder, Occurrence, Resolution, Shape, Written},
  set::ROOT,
  types::closure::{Blame, Closure, DERIVES, Place},
};

/// One of the phase's two checks at a derived instantiation.
#[derive(Clone, Copy)]
enum Check {
  /// Two entries, or two members, of one type.
  Meeting,
  /// An argument made required.
  Making,
}

/// What the blame of the closure's edges reads, each read once and kept: the instantiations each
/// deriver's own arguments hold, and the open instantiations each position's type holds.
#[derive(Default)]
struct Paths {
  /// The nullable forms of the generic named types each deriver's arguments hold, by its node.
  arguments: BTreeMap<u32, BTreeSet<TyId>>,
  /// Each position's type, with its run of `instances`: the open generic named types it holds, by
  /// their definitions.
  positions: BTreeMap<TyId, PoolRange>,
  /// The runs `positions` names.
  instances: Vec<(DefId, TyId)>,
  /// The types still to read.
  stack: Vec<TyId>,
  /// The types the reading being made has met.
  seen: BTreeSet<TyId>,
}

/// What each node of the closure reaches that fails, for each of the phase's two checks.
struct Blamed {
  /// Two entries or two members of one type, or a stop.
  met: Vec<Option<Blame>>,
  /// An argument made required, or a stop.
  made: Vec<Option<Blame>>,
}

impl Implementing<'_> {
  /// Judges each instantiation the closure derives, and reports, at each occurrence and each
  /// application of a root whose own verdict holds, the failing instantiation its expansion reaches
  /// first. The closure is the type services' no longer: nothing after this phase reads it.
  pub(super) fn derived(&mut self) {
    let closure = core::mem::take(&mut self.types.closure);
    if closure.is_empty() {
      return;
    }
    let mut meeting: Vec<u32> = Vec::new();
    let mut making: Vec<u32> = Vec::new();
    for at in 0..closure.len() {
      let Some(node) = closure.node(at) else {
        break;
      };
      if node.written || node.grows {
        continue;
      }
      if let Some(listed) = self.listed(node.def) {
        match self.verdict(node.ty, node.def, listed) {
          Verdict::Compared(Comparison {
            entries: None,
            members: None,
          }) => {}
          // The rebuild that stopped the closure, which coercion reported: not again.
          Verdict::Stopped(_) if self.repeats(node.ty) => {}
          Verdict::Compared(_) | Verdict::Stopped(_) => meeting.push(at),
        }
      }
      if !matches!(self.made_by(node.ty, node.def), Made::Nothing) {
        making.push(at);
      }
    }
    if meeting.is_empty() && making.is_empty() {
      return;
    }
    let mut paths = Paths::default();
    let blamed = Blamed {
      met: closure.blame_where(&meeting, |failed: u32, parent: u32, place: Place| {
        self.carries(&mut paths, &closure, failed, parent, place, Check::Meeting)
      }),
      made: closure.blame_where(&making, |failed: u32, parent: u32, place: Place| {
        self.carries(&mut paths, &closure, failed, parent, place, Check::Making)
      }),
    };
    let supplied = self.sources.package_sources(ROOT).start();
    // The root of each type an occurrence or an application has, by that type, either nullness.
    let mut roots: BTreeMap<TyId, Option<u32>> = BTreeMap::new();
    for index in 0..self.builder.type_expr_count() {
      let Some((written, ty, at)) = self
        .builder
        .occurrence(TypeExprId::new(index))
        .map(|occurrence: Occurrence<'_>| (occurrence.written, occurrence.ty, occurrence.location))
      else {
        continue;
      };
      let (Written::Named(def), Some(ty)) = (written, ty) else {
        continue;
      };
      if at.source() >= supplied {
        self.report_derived(&closure, &blamed, &mut roots, at, def, ty);
      }
    }
    for index in 0..self.builder.application_count() {
      let application = ApplicationId::new(index);
      let (
        Some(Applied {
          directive: Resolution::Resolved(def),
          at,
          ..
        }),
        Some(ty),
      ) = (
        self.builder.applied(application),
        self.builder.application_ty(application),
      )
      else {
        continue;
      };
      if at.source() >= supplied {
        self.report_derived(&closure, &blamed, &mut roots, at, def, ty);
      }
    }
  }

  /// What `ty`, an instantiation of `def`, makes required of the arguments `def`'s fields add
  /// beside their interfaces' fields: kept by the type, made the first time it is asked, as an
  /// occurrence's is.
  fn made_by(&mut self, ty: TyId, def: DefId) -> Made {
    let from = self
      .latent
      .partition_point(|latent: &Latent| latent.def < def);
    let to = self
      .latent
      .partition_point(|latent: &Latent| latent.def <= def);
    if from == to {
      return Made::Nothing;
    }
    if let Some(&known) = self.made.get(&ty) {
      return known;
    }
    let found = self.make(ty, from, to);
    self.made.insert(ty, found);
    found
  }

  /// Whether the edge from the deriver `parent` to the failing instantiation `failed`, at `place`,
  /// passes on what `failed` fails of `check`. Not when `failed` is held in the deriver's own
  /// arguments: whatever derived the deriver reached it too, by the edge where it was derived,
  /// which says. Nor when the type at `place` holds an open instantiation that failed the same way
  /// where it is written, which reported it, and that the deriver's arguments make `failed`:
  /// closing the type changed neither the failure nor where it comes from. What closing alone
  /// introduces passes on. Each reading is a step of the phase's budget, read once and kept; one
  /// the budget cannot pay for passes the failure on.
  fn carries(
    &mut self,
    paths: &mut Paths,
    closure: &Closure,
    failed: u32,
    parent: u32,
    place: Place,
    check: Check,
  ) -> bool {
    let (Some(child), Some(deriver)) = (closure.node(failed), closure.node(parent)) else {
      return true;
    };
    if !deriver.written {
      match self.held_by_arguments(paths, parent, deriver.ty, child.ty) {
        Some(true) => return false,
        Some(false) => {}
        None => return true,
      }
    }
    let open = match place {
      Place::Type(id) => self
        .builder
        .occurrence(id)
        .and_then(|occurrence: Occurrence<'_>| occurrence.ty),
      Place::Application(id) => self.builder.application_ty(id),
    };
    let Some(open) = open else {
      return true;
    };
    let Some(run) = self.open_instances(paths, open) else {
      return true;
    };
    let candidates = paths
      .instances
      .get(run.start as usize..run.end as usize)
      .unwrap_or_default();
    let from = candidates.partition_point(|&(def, _): &(DefId, TyId)| def < child.def);
    let to = candidates.partition_point(|&(def, _): &(DefId, TyId)| def <= child.def);
    for index in from..to {
      let Some(&(_, instance)) = paths.instances.get(run.start as usize + index) else {
        break;
      };
      if !self.reported_open(instance, child.ty, check) {
        continue;
      }
      if let Ok(Some(made)) = self.rebuilt(instance, deriver.ty, deriver.def)
        && self.same_instance(made, child.ty)
      {
        return false;
      }
    }
    true
  }

  /// Whether the arguments of `ty`, the type of the deriver `node`, hold the instantiation whose
  /// nullable form is `child`, at any depth: the nullable forms they hold read the first time the
  /// deriver is asked about, each type a step of the phase's budget. `None` when the budget cannot
  /// pay.
  fn held_by_arguments(
    &mut self,
    paths: &mut Paths,
    node: u32,
    ty: TyId,
    child: TyId,
  ) -> Option<bool> {
    if !paths.arguments.contains_key(&node) {
      let mut held: BTreeSet<TyId> = BTreeSet::new();
      paths.stack.clear();
      paths.seen.clear();
      paths.stack.extend_from_slice(self.builder.ty_args(ty));
      while let Some(at) = paths.stack.pop() {
        if !paths.seen.insert(at) {
          continue;
        }
        if !self.steps.charge(1) {
          return None;
        }
        let builder: &ModelBuilder = &*self.builder;
        match builder.ty_shape(at) {
          Some(Shape::Named(def)) => {
            if builder.type_param_count(def) > 0 {
              let nullable = if builder.ty_non_null(at) {
                builder
                  .find_named(def, builder.ty_args(at), false)
                  .unwrap_or(at)
              } else {
                at
              };
              held.insert(nullable);
            }
            paths.stack.extend_from_slice(builder.ty_args(at));
          }
          Some(Shape::List(element)) => paths.stack.push(element),
          Some(Shape::Param(_) | Shape::Unresolved) | None => {}
        }
      }
      paths.arguments.insert(node, held);
    }
    Some(
      paths
        .arguments
        .get(&node)
        .is_some_and(|held: &BTreeSet<TyId>| held.contains(&child)),
    )
  }

  /// The run of `paths.instances` that holds the open generic named types `open`, a position's
  /// type, holds, itself among them, by their definitions: read the first time it is asked, each
  /// type a step of the phase's budget. `None` when the budget cannot pay.
  fn open_instances(&mut self, paths: &mut Paths, open: TyId) -> Option<PoolRange> {
    if let Some(&run) = paths.positions.get(&open) {
      return Some(run);
    }
    let start = paths.instances.len();
    paths.stack.clear();
    paths.seen.clear();
    paths.stack.push(open);
    while let Some(at) = paths.stack.pop() {
      if self.builder.ty_closed(at) || !paths.seen.insert(at) {
        continue;
      }
      if !self.steps.charge(1) {
        paths.instances.truncate(start);
        return None;
      }
      let builder: &ModelBuilder = &*self.builder;
      match builder.ty_shape(at) {
        Some(Shape::Named(def)) => {
          if builder.type_param_count(def) > 0 {
            paths.instances.push((def, at));
          }
          paths.stack.extend_from_slice(builder.ty_args(at));
        }
        Some(Shape::List(element)) => paths.stack.push(element),
        Some(Shape::Param(_) | Shape::Unresolved) | None => {}
      }
    }
    if let Some(run) = paths.instances.get_mut(start..) {
      run.sort_unstable();
    }
    let run = PoolRange {
      start: dense(start),
      end: dense(paths.instances.len()),
    };
    paths.positions.insert(open, run);
    Some(run)
  }

  /// Whether `instance`, an open type an occurrence has, failed `check` where it is written, as
  /// `child`, an instantiation the closure derives, fails it: two entries or two members that are
  /// the same two, or the same argument made required. The occurrences' pass reported it there.
  fn reported_open(&self, instance: TyId, child: TyId, check: Check) -> bool {
    match check {
      Check::Meeting => match (self.verdicts.get(&instance), self.verdicts.get(&child)) {
        (Some(Verdict::Compared(open)), Some(Verdict::Compared(closed))) => {
          (open.entries.is_some() || open.members.is_some()) && open == closed
        }
        _ => false,
      },
      Check::Making => matches!(
        (self.made.get(&instance), self.made.get(&child)),
        (Some(Made::Required(open)), Some(Made::Required(closed))) if open == closed
      ),
    }
  }

  /// Whether `made` and `child` are one named type, whatever the nullness of either.
  fn same_instance(&self, made: TyId, child: TyId) -> bool {
    match (self.builder.ty_shape(made), self.builder.ty_shape(child)) {
      (Some(Shape::Named(one)), Some(Shape::Named(other))) => {
        one == other && self.builder.ty_args(made) == self.builder.ty_args(child)
      }
      _ => false,
    }
  }

  /// Reports at `at`, an occurrence or an application of `ty`, an instantiation of `user`, what its
  /// expansion reaches first that fails, for each check whose own verdict at `ty` holds, when `ty`
  /// is a closed named type of `user` and a root of `closure`: found through `roots`, which keeps
  /// each type's root the first time it is asked.
  fn report_derived(
    &mut self,
    closure: &Closure,
    blamed: &Blamed,
    roots: &mut BTreeMap<TyId, Option<u32>>,
    at: Location,
    user: DefId,
    ty: TyId,
  ) {
    if self.builder.type_param_count(user) == 0
      || !self.builder.ty_closed(ty)
      || !matches!(self.builder.ty_shape(ty), Some(Shape::Named(named)) if named == user)
    {
      return;
    }
    let found = if let Some(&known) = roots.get(&ty) {
      known
    } else {
      let nullable = if self.builder.ty_non_null(ty) {
        self
          .builder
          .find_named(user, self.builder.ty_args(ty), false)
          .unwrap_or(ty)
      } else {
        ty
      };
      let found = closure.find(nullable);
      roots.insert(ty, found);
      found
    };
    let Some(root) = found else {
      return;
    };
    // Its own verdicts, made where it is written: one that fails was reported there.
    let compared = self.verdicts.get(&ty).is_none_or(|verdict: &Verdict| {
      matches!(
        verdict,
        Verdict::Compared(Comparison {
          entries: None,
          members: None
        })
      )
    });
    if compared && let Some(&Some(blame)) = blamed.met.get(root as usize) {
      self.report_derived_meeting(closure, at, user, blame);
    }
    let required = self
      .made
      .get(&ty)
      .is_some_and(|made: &Made| !matches!(made, Made::Nothing));
    if !required && let Some(&Some(blame)) = blamed.made.get(root as usize) {
      self.report_derived_made(closure, at, user, blame);
    }
  }

  /// How a report at a use of an instantiation of `def` begins: a type instantiated there, or a
  /// directive applied there.
  fn used(&self, def: DefId) -> String {
    let name = Excerpt(self.builder.def_name(def));
    if self.builder.def_kind(def) == Some(DefKind::Directive) {
      format!("`@{name}` is applied here")
    } else {
      format!("`{name}` is instantiated here")
    }
  }

  /// Where `place` is written, when it is somewhere a label can point.
  fn place_location(&self, place: Option<Place>) -> Option<Location> {
    let at = match place? {
      Place::Type(written) => self.located(written)?,
      Place::Application(application) => self.builder.applied(application)?.at,
    };
    (!at.is_entire()).then_some(at)
  }

  /// Reports at `at`, a use of an instantiation of `user`, the instantiation `blame` names, whose
  /// entries or members meet under its arguments, or whose comparison a limit stopped.
  fn report_derived_meeting(&mut self, closure: &Closure, at: Location, user: DefId, blame: Blame) {
    let Some(failed) = closure.node(blame.node) else {
      return;
    };
    match self.verdicts.get(&failed.ty).copied() {
      Some(Verdict::Compared(Comparison { entries, members })) => {
        let derives = self.place_location(blame.place);
        if let Some(twin) = entries {
          self.report_derived_twin(at, user, failed.def, derives, List::Implements, twin);
        }
        if let Some(twin) = members {
          self.report_derived_twin(at, user, failed.def, derives, List::Members, twin);
        }
      }
      Some(Verdict::Stopped(stop)) => {
        let subject = format!(
          "the entries and members of an instantiation of `{}` that `{}` derives here",
          Excerpt(self.builder.def_name(failed.def)),
          Excerpt(self.builder.def_name(user))
        );
        self.report_limit(at, &subject, stop);
      }
      None => {}
    }
  }

  /// Reports at `at`, a use of an instantiation of `user` that derives an instantiation of `def`
  /// at `derives`, that `twin`, two items of `def`'s list `list`, are one type under its arguments.
  fn report_derived_twin(
    &mut self,
    at: Location,
    user: DefId,
    def: DefId,
    derives: Option<Location>,
    list: List,
    twin: Twin,
  ) {
    let (Some(first), Some(later)) = (self.located(twin.earlier), self.located(twin.later)) else {
      return;
    };
    let used = self.used(user);
    let named = Excerpt(self.builder.def_name(def));
    let (code, message, title, labels) = match list {
      List::Implements => (
        schema::DUPLICATE_IMPLEMENTS_INTERFACE,
        format!(
          "{used} with type arguments that derive an instantiation of `{named}` under which two \
           of its `implements` entries are one interface type"
        ),
        "duplicate interface",
        ("this entry", "the same interface type here"),
      ),
      List::Members => (
        schema::DUPLICATE_UNION_MEMBER,
        format!(
          "{used} with type arguments that derive an instantiation of `{named}` under which two \
           of its members are one type"
        ),
        "duplicate union member",
        ("this member", "the same type here"),
      ),
    };
    let mut report = Diagnostic::builder(code, at)
      .message(message)
      .primary_label(title)
      .help(
        "instantiate it with type arguments that derive no instantiation under which they meet",
      );
    if let Some(derives) = derives {
      report = report.label(derives, DERIVES);
    }
    let report = labelled(labelled(report, first, labels.0), later, labels.1);
    self.reporter.report(report.build());
  }

  /// Reports at `at`, a use of an instantiation of `user`, the instantiation `blame` names, which
  /// makes an argument required, or whose judging a limit stopped.
  fn report_derived_made(&mut self, closure: &Closure, at: Location, user: DefId, blame: Blame) {
    let Some(failed) = closure.node(blame.node) else {
      return;
    };
    match self.made.get(&failed.ty).copied() {
      Some(Made::Required(index)) => {
        let derives = self.place_location(blame.place);
        self.report_derived_required(at, user, index, derives);
      }
      Some(Made::Stopped(stop)) => {
        let subject = format!(
          "the arguments the fields of an instantiation of `{}` that `{}` derives here require",
          Excerpt(self.builder.def_name(failed.def)),
          Excerpt(self.builder.def_name(user))
        );
        self.report_limit(at, &subject, stop);
      }
      Some(Made::Nothing) | None => {}
    }
  }

  /// Reports at `at`, a use of an instantiation of `user` that derives, at `derives`, an
  /// instantiation making the latent argument at `index` required: labelled there, at the argument
  /// and at the interface's field that does not declare it.
  fn report_derived_required(
    &mut self,
    at: Location,
    user: DefId,
    index: u32,
    derives: Option<Location>,
  ) {
    let Some(&Latent {
      def,
      argument,
      field,
      interface,
      ..
    }) = self.latent.get(index as usize)
    else {
      return;
    };
    let (Some(written), Some(declared)) = (
      self.builder.member_location(argument),
      self.builder.member_location(field),
    ) else {
      return;
    };
    let used = self.used(user);
    let owner = Excerpt(self.builder.def_name(def));
    let interface = Excerpt(self.builder.def_name(interface));
    let named = Excerpt(self.member_spelling(field));
    let name = Excerpt(self.member_spelling(argument));
    let mut report = Diagnostic::builder(schema::UNEXPECTED_REQUIRED_ARGUMENT, at)
      .message(format!(
        "{used} with type arguments that derive an instantiation of `{owner}` under which its \
         field `{named}` requires the argument `{name}`, which `{interface}`'s field `{named}` \
         does not declare"
      ))
      .primary_label("unexpected required argument")
      .help(
        "make the argument nullable, give it a default, or declare it on the interface's field",
      );
    if let Some(derives) = derives {
      report = report.label(derives, DERIVES);
    }
    report = report.label(written, "required under these type arguments");
    self
      .reporter
      .report(labelled(report, declared, "the interface's field").build());
  }
}
