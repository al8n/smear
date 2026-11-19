//! Derived instantiations, design Z's D11: the closure of what each written instantiation derives,
//! which coercion builds once its walk has ended, and the check of a generic definition's values,
//! and of the arguments its applications leave out, at each instantiation of it the closure
//! derives.
//!
//! # The closure
//!
//! Its roots are the closed instantiations of generic definitions a supplied package writes: each
//! type occurrence's type, in id order, then each application's complete type, in id order, each
//! instantiation once, at where it is first written, by its nullable form. Each root is expanded in
//! turn, and then, breadth first, each instantiation its expansion derives that is new to the
//! closure, no supplied package writes and whose definition does not grow, so each instantiation
//! is expanded once.
//!
//! An instantiation's expansion reads what a copy of it needs under its arguments: each of its
//! definition's members' types, `implements` entries and union members, and the complete type of
//! each application on the definition or on one of its members, rebuilt as the check at a written
//! instantiation rebuilds a member's type, through the same remembered rebuild. Each instantiation
//! of a generic definition the rebuilt types hold, at any depth, is derived, with the type or the
//! application that derives it; a written one is no child, being judged where it is written and
//! expanded as a root, and what it holds is its own. A root's expansion reads its arguments too,
//! each one a type parameter's default may fill: what a default fills is derived by that default.
//!
//! The expansion reads only toward what can be judged: an instantiation is derived only of a
//! definition that is relevant, one that holds something an instantiation of it is judged for, or
//! one of whose positions holds an open named type of a relevant one; and a position is read only
//! when it holds such a type. What an instantiation is judged for is a value recorded under its
//! definition, here, and in the implementation check an `implements` entry or a union member that
//! names its type parameters, or, where an object or an interface implements one, a field argument
//! typed by its own bare nullable type parameter with no default, which may be required. So a
//! program whose generic definitions hold nothing to judge derives nothing, and pays nothing.
//!
//! # What it costs, and what ends it
//!
//! A root is expanded once, and a derived instantiation only when its definition does not grow,
//! where a type argument is passed on and never wrapped on a cycle: so the closure is finite, and
//! each instantiation it derives is new to the model, counted toward `Limits::max_instantiations`
//! and depth-checked against `Limits::max_generic_depth` as every rebuild is. Each rebuild is paid
//! from `Limits::max_value_steps`, as the check at a written instantiation pays for its own, and a
//! rebuild remembered costs nothing; so is each instantiation an expansion derives, once for each
//! instantiation that derives it. Reading what a position holds before any rebuild reads the types
//! instantiation built, and costs nothing. A limit that stops an expansion is `resource-limit`,
//! once, at where the root whose expansion was running is first written, and the closure ends
//! there: what it did not reach is not judged.
//!
//! # The check, and its report
//!
//! Each instantiation the closure derives whose definition does not grow and records rows is
//! judged as a written instantiation is, by the same verdict, made once and shared by both
//! nullnesses, a step for each pair of the instantiation and a row. Each occurrence of a root,
//! and each application of one, in id order, whose own verdict holds, reports the failing
//! instantiation of least index its expansion reaches, as `Closure::blame` finds it: with the code
//! the same failure has at a written instantiation, labelled at the type or the application that
//! derives the failing instantiation and at the value or the application that fails; or, stopped
//! by a limit, `resource-limit`. A root whose own verdict fails was reported as written, and says
//! nothing more. A derived instantiation of a definition that grows is not judged, nor is what
//! only it derives.

use std::{
  borrow::Cow,
  collections::{BTreeMap, BTreeSet},
  format, vec,
  vec::Vec,
};

use tokora::diagnostic::Location;

use super::{Checked, Coercion, OmittedArgument, Owed, OwedValue, Recorded};
use crate::graphqlx::{
  code::schema,
  diagnostic::{Diagnostic, Excerpt, Message},
  id::{ApplicationId, DefId, MemberId, PoolRange, TyId, TypeExprId, dense},
  model::{Applied, DefKind, Occurrence, Resolution, Shape, Written, WrittenParam},
  raw::RawTree,
  types::{
    Limit, Stop,
    closure::{Blame, Closure, DERIVES, Node, Place},
  },
};

/// An open position of a generic definition: a type a copy of an instantiation of it reads under
/// its arguments.
#[derive(Clone, Copy)]
struct Position {
  /// The definition.
  def: DefId,
  /// Where it is written: a type occurrence, or an application by its complete type.
  place: Place,
  /// Its type, which names the definition's type parameters.
  ty: TyId,
  /// Whether it holds an open named type of a relevant definition: whether reading it under an
  /// instantiation's arguments can derive one with something to judge.
  useful: bool,
}

/// What building the closure keeps while it expands.
struct Building {
  /// The closure so far.
  closure: Closure,
  /// Every generic definition's open positions, each definition's a run.
  positions: Vec<Position>,
  /// Each definition's run of `positions`, by `DefId`.
  runs: Vec<PoolRange>,
  /// Whether each definition is relevant, by `DefId`.
  relevant: Vec<bool>,
  /// The types the expansion being made has walked.
  seen: BTreeSet<TyId>,
  /// The children the expansion being made has derived.
  derived: BTreeSet<u32>,
  /// The types still to walk.
  stack: Vec<TyId>,
  /// The instantiations still to expand from the root being expanded, in order, and those already
  /// expanded before them.
  queue: Vec<u32>,
}

impl Coercion<'_, '_, '_> {
  /// Builds the closure of derived instantiations, judges what it derives, and keeps it in the type
  /// services for the implementation check.
  pub(super) fn derived(&mut self) {
    let closure = self.derive_closure();
    self.judge_derived(&closure);
    self.types.closure = closure;
  }

  /// The closure: every root, and what each derives through the definitions that do not grow, as
  /// the module says. A limit that stops an expansion is reported once, and ends it.
  fn derive_closure(&mut self) -> Closure {
    let mut closure = Closure::default();
    // The roots, each with where it is first written.
    let mut roots: Vec<(u32, Location, DefId)> = Vec::new();
    for index in 0..self.builder.type_expr_count() {
      let Some(occurrence) = self.builder.occurrence(TypeExprId::new(index)) else {
        continue;
      };
      let (Written::Named(def), Some(ty)) = (occurrence.written, occurrence.ty) else {
        continue;
      };
      let at = occurrence.location;
      self.root(&mut closure, &mut roots, at, def, ty);
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
      self.root(&mut closure, &mut roots, at, def, ty);
    }
    if roots.is_empty() {
      return closure;
    }
    let mut building = Building {
      closure,
      positions: Vec::new(),
      runs: Vec::new(),
      relevant: Vec::new(),
      seen: BTreeSet::new(),
      derived: BTreeSet::new(),
      stack: Vec::new(),
      queue: Vec::new(),
    };
    self.relevance(&mut building);
    if !building.relevant.iter().any(|relevant: &bool| *relevant) {
      return building.closure;
    }
    for &(root, at, def) in &roots {
      building.queue.clear();
      building.queue.push(root);
      let mut next: usize = 0;
      while let Some(&expanding) = building.queue.get(next) {
        next += 1;
        if let Err(stop) = self.expand(&mut building, expanding) {
          self.report_unexpanded(at, def, stop);
          return building.closure;
        }
      }
    }
    building.closure
  }

  /// Joins `ty`, written at `at`, to `closure` as a root, and to `roots` when it is new: when a
  /// supplied package writes it and it is a closed named type of `def`, a generic definition.
  fn root(
    &mut self,
    closure: &mut Closure,
    roots: &mut Vec<(u32, Location, DefId)>,
    at: Location,
    def: DefId,
    ty: TyId,
  ) {
    if at.source() < self.supplied
      || self.builder.type_param_count(def) == 0
      || !self.builder.ty_closed(ty)
      || !self.is_named(ty, def)
    {
      return;
    }
    let nullable = self.nullable(ty, def);
    let grows = self.builder.def_grows(def);
    let (node, new) = closure.join(nullable, def, true, grows);
    if new {
      roots.push((node, at, def));
    }
  }

  /// Finds every generic definition's open positions, and which definitions are relevant, as the
  /// module says: each definition with something to judge, and then, back through the positions
  /// that hold their open named types, each definition one of whose positions holds a relevant one,
  /// each such position marked useful. Reading the types instantiation built costs nothing.
  fn relevance(&mut self, building: &mut Building) {
    let count = self.builder.def_count();
    building.runs = vec![PoolRange { start: 0, end: 0 }; count as usize];
    building.relevant = vec![false; count as usize];
    for index in 0..count {
      let def = DefId::new(index);
      if self.builder.type_param_count(def) == 0 {
        continue;
      }
      let start = dense(building.positions.len());
      self.push_positions(def, &mut building.positions);
      if let Some(run) = building.runs.get_mut(index as usize) {
        *run = PoolRange {
          start,
          end: dense(building.positions.len()),
        };
      }
    }
    // The definitions of the open named types each distinct position type holds, a run of `holds`
    // each, walked once however many positions have the type: many applications of one directive
    // share one complete type.
    let mut walked: BTreeMap<TyId, PoolRange> = BTreeMap::new();
    let mut holds: Vec<DefId> = Vec::new();
    // Each open named type a position holds, by its definition, with the position.
    let mut held: Vec<(DefId, u32)> = Vec::new();
    for (at, position) in building.positions.iter().enumerate() {
      let run = if let Some(&run) = walked.get(&position.ty) {
        run
      } else {
        let start = dense(holds.len());
        building.seen.clear();
        building.stack.clear();
        building.stack.push(position.ty);
        while let Some(ty) = building.stack.pop() {
          if self.builder.ty_closed(ty) || !building.seen.insert(ty) {
            continue;
          }
          match self.builder.ty_shape(ty) {
            Some(Shape::Named(def)) => {
              holds.push(def);
              building.stack.extend_from_slice(self.builder.ty_args(ty));
            }
            Some(Shape::List(element)) => building.stack.push(element),
            Some(Shape::Param(_) | Shape::Unresolved) | None => {}
          }
        }
        let run = PoolRange {
          start,
          end: dense(holds.len()),
        };
        walked.insert(position.ty, run);
        run
      };
      for &def in holds
        .get(run.start as usize..run.end as usize)
        .unwrap_or_default()
      {
        held.push((def, dense(at)));
      }
    }
    held.sort_unstable();
    held.dedup();
    // What is judged: a value recorded under a definition, or what the implementation check
    // judges at an instantiation.
    let mut work: Vec<DefId> = Vec::new();
    for row in &self.recorded {
      if let Some(slot) = building.relevant.get_mut(row.def.get() as usize)
        && !*slot
      {
        *slot = true;
        work.push(row.def);
      }
    }
    for index in 0..count {
      let def = DefId::new(index);
      if self.builder.type_param_count(def) > 0
        && self.judged_when_implemented(def)
        && let Some(slot) = building.relevant.get_mut(index as usize)
        && !*slot
      {
        *slot = true;
        work.push(def);
      }
    }
    while let Some(def) = work.pop() {
      let from = held.partition_point(|&(holder, _): &(DefId, u32)| holder < def);
      for &(holder, at) in held.get(from..).unwrap_or_default() {
        if holder != def {
          break;
        }
        let Some(position) = building.positions.get_mut(at as usize) else {
          continue;
        };
        position.useful = true;
        let owner = position.def;
        if let Some(slot) = building.relevant.get_mut(owner.get() as usize)
          && !*slot
        {
          *slot = true;
          work.push(owner);
        }
      }
    }
  }

  /// Appends to `into` each open position of `def`: its `implements` entries, its union members,
  /// the complete types of its applications, and each member's type and its applications' complete
  /// types, a field's arguments' too.
  fn push_positions(&self, def: DefId, into: &mut Vec<Position>) {
    for &entry in self
      .builder
      .def_implements(def)
      .iter()
      .chain(self.builder.def_union_members(def))
    {
      self.push_type(def, entry, into);
    }
    for &application in self.builder.def_applications(def) {
      self.push_application(def, application, into);
    }
    for member in self.builder.def_members(def) {
      self.push_member(def, member, into);
      for argument in self.builder.member_arguments(member) {
        self.push_member(def, argument, into);
      }
    }
  }

  /// Appends to `into` the type and the applications of `member`, of `def`, that are open.
  fn push_member(&self, def: DefId, member: MemberId, into: &mut Vec<Position>) {
    if let Some(written) = self.builder.member_ty(member) {
      self.push_type(def, written, into);
    }
    for &application in self.builder.member_applications(member) {
      self.push_application(def, application, into);
    }
  }

  /// Appends to `into` the type occurrence `written`, of `def`, when its type is open.
  fn push_type(&self, def: DefId, written: TypeExprId, into: &mut Vec<Position>) {
    if let Some(ty) = self
      .builder
      .occurrence(written)
      .and_then(|occurrence: Occurrence<'_>| occurrence.ty)
      && !self.builder.ty_closed(ty)
    {
      into.push(Position {
        def,
        place: Place::Type(written),
        ty,
        useful: false,
      });
    }
  }

  /// Appends to `into` the complete type of `application`, on `def`, when it is open.
  fn push_application(&self, def: DefId, application: ApplicationId, into: &mut Vec<Position>) {
    if let Some(ty) = self.builder.application_ty(application)
      && !self.builder.ty_closed(ty)
    {
      into.push(Position {
        def,
        place: Place::Application(application),
        ty,
        useful: false,
      });
    }
  }

  /// Whether the implementation check judges something at an instantiation of `def`, read
  /// generously: an `implements` entry or a union member whose type names a type parameter, or, for
  /// an object or an interface that writes an `implements` entry, a field argument typed by its own
  /// bare nullable type parameter with no default.
  fn judged_when_implemented(&self, def: DefId) -> bool {
    for &item in self
      .builder
      .def_implements(def)
      .iter()
      .chain(self.builder.def_union_members(def))
    {
      if self
        .builder
        .occurrence(item)
        .and_then(|occurrence: Occurrence<'_>| occurrence.ty)
        .is_some_and(|ty: TyId| !self.builder.ty_closed(ty))
      {
        return true;
      }
    }
    // An argument the interface's field does not declare is the check's only where the definition
    // implements one.
    if !matches!(
      self.builder.def_kind(def),
      Some(DefKind::Object | DefKind::Interface)
    ) || self.builder.def_implements(def).is_empty()
    {
      return false;
    }
    self
      .builder
      .def_members(def)
      .into_iter()
      .any(|field: MemberId| {
        self
          .builder
          .member_arguments(field)
          .into_iter()
          .any(|argument: MemberId| {
            self.bare_param(argument, def) && !self.raw.defaults.contains_key(&argument)
          })
      })
  }

  /// Expands the node `at` of the closure: each useful position of its definition, rebuilt under
  /// its type, and, for a root, each argument a type parameter's default may fill, each walked for
  /// the instantiations it derives. `Err` when a limit stops a rebuild, or the budget a child.
  fn expand(&mut self, building: &mut Building, at: u32) -> Result<(), Stop> {
    let Some(node) = building.closure.node(at) else {
      return Ok(());
    };
    let from = building.closure.next_edge();
    building.seen.clear();
    building.derived.clear();
    // A definition that is not relevant has no useful position.
    let relevant = building
      .relevant
      .get(node.def.get() as usize)
      .copied()
      .unwrap_or(false);
    let run = building
      .runs
      .get(node.def.get() as usize)
      .copied()
      .filter(|_: &PoolRange| relevant)
      .unwrap_or(PoolRange { start: 0, end: 0 });
    for index in run.start..run.end {
      let Some(&Position {
        place, ty, useful, ..
      }) = building.positions.get(index as usize)
      else {
        break;
      };
      if !useful {
        continue;
      }
      if let Some(under) = self.rebuilt(ty, node.ty, node.def, false)? {
        self.collect(building, under, place)?;
      }
    }
    if node.written {
      // A root's arguments: what a type parameter's default fills is derived by that default.
      let count = self.builder.ty_args(node.ty).len();
      for index in 0..count {
        let Some(&argument) = self.builder.ty_args(node.ty).get(index) else {
          break;
        };
        let Some(default) = self
          .builder
          .type_param(node.def, dense(index))
          .and_then(|param: WrittenParam| param.default)
        else {
          continue;
        };
        self.collect(building, argument, Place::Type(default))?;
      }
    }
    building.closure.expanded(at, from);
    Ok(())
  }

  /// Walks `ty`, closed, for the instantiations it derives at `place`, with an explicit stack: each
  /// instantiation of a relevant generic definition no supplied package writes is a child of the
  /// node being expanded, once, a step of `Limits::max_value_steps`, and one new to the closure
  /// whose definition does not grow is queued to be expanded. A written one is no child, and its
  /// arguments are its own. `Err` when the budget cannot pay for a child.
  fn collect(&mut self, building: &mut Building, ty: TyId, place: Place) -> Result<(), Stop> {
    building.stack.clear();
    building.stack.push(ty);
    while let Some(held) = building.stack.pop() {
      if !building.seen.insert(held) {
        continue;
      }
      match self.builder.ty_shape(held) {
        Some(Shape::Named(def)) => {
          if self.builder.type_param_count(def) == 0 {
            continue;
          }
          let nullable = self.nullable(held, def);
          if let Some(known) = building.closure.find(nullable)
            && building
              .closure
              .node(known)
              .is_some_and(|node: Node| node.written)
          {
            continue;
          }
          if building
            .relevant
            .get(def.get() as usize)
            .copied()
            .unwrap_or(false)
          {
            let grows = self.builder.def_grows(def);
            let (child, new) = building.closure.join(nullable, def, false, grows);
            if building.derived.insert(child) {
              if !self.charge(1) {
                return Err(Stop::Steps);
              }
              building.closure.derive(child, place);
              if new && !grows {
                building.queue.push(child);
              }
            }
          }
          building.stack.extend_from_slice(self.builder.ty_args(held));
        }
        Some(Shape::List(element)) => building.stack.push(element),
        Some(Shape::Param(_) | Shape::Unresolved) | None => {}
      }
    }
    Ok(())
  }

  /// Judges each instantiation `closure` derives whose definition does not grow and records rows,
  /// and reports, at each occurrence and each application of a root whose own verdict holds, the
  /// failing instantiation its expansion reaches first.
  fn judge_derived(&mut self, closure: &Closure) {
    if self.recorded.is_empty() {
      return;
    }
    let mut failing: Vec<u32> = Vec::new();
    for at in 0..closure.len() {
      let Some(node) = closure.node(at) else {
        break;
      };
      if node.written || node.grows {
        continue;
      }
      if !matches!(self.derived_verdict(node.def, node.ty), Checked::Fits) {
        failing.push(at);
      }
    }
    if failing.is_empty() {
      return;
    }
    let blamed = closure.blame(&failing);
    for index in 0..self.builder.type_expr_count() {
      let Some(occurrence) = self.builder.occurrence(TypeExprId::new(index)) else {
        continue;
      };
      let (Written::Named(def), Some(ty)) = (occurrence.written, occurrence.ty) else {
        continue;
      };
      let at = occurrence.location;
      self.report_derived(closure, &blamed, at, def, ty);
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
      self.report_derived(closure, &blamed, at, def, ty);
    }
  }

  /// The verdict of `ty`, the nullable form of an instantiation of `def` the closure derives: made
  /// the first time it is asked, as a written instantiation's is, and kept; `Fits` when `def`
  /// records nothing.
  fn derived_verdict(&mut self, def: DefId, ty: TyId) -> Checked {
    let start = self
      .recorded
      .partition_point(|row: &Recorded| row.def < def);
    let end = self
      .recorded
      .partition_point(|row: &Recorded| row.def <= def);
    if start >= end {
      return Checked::Fits;
    }
    if let Some(&verdict) = self.checked.get(&ty) {
      return verdict;
    }
    let verdict = self.check(def, ty, start, end);
    self.checked.insert(ty, verdict);
    verdict
  }

  /// Reports at `at`, an occurrence or an application of `ty`, an instantiation of `user`, the
  /// failing instantiation its expansion reaches first, as `blamed` keeps it, when a supplied
  /// package writes it there, `ty` is a root, and its own verdict holds.
  fn report_derived(
    &mut self,
    closure: &Closure,
    blamed: &[Option<Blame>],
    at: Location,
    user: DefId,
    ty: TyId,
  ) {
    if at.source() < self.supplied
      || self.builder.type_param_count(user) == 0
      || !self.builder.ty_closed(ty)
      || !self.is_named(ty, user)
    {
      return;
    }
    let nullable = self.nullable(ty, user);
    // Its own verdict, made where it is written: one that fails was reported there.
    if self
      .checked
      .get(&nullable)
      .is_some_and(|verdict: &Checked| !matches!(verdict, Checked::Fits))
    {
      return;
    }
    let Some(root) = closure.find(nullable) else {
      return;
    };
    let Some(&Some(Blame { node, place })) = blamed.get(root as usize) else {
      return;
    };
    let Some(failed) = closure.node(node) else {
      return;
    };
    match self.checked.get(&failed.ty).copied() {
      Some(Checked::Unfit(row)) => self.report_derived_unfit(at, user, failed.def, row, place),
      Some(Checked::Stopped(stop)) => self.report_derived_unchecked(at, user, failed.def, stop),
      Some(Checked::Fits) | None => {}
    }
  }

  /// Where `place` is written, when it is somewhere a label can point.
  fn place_location(&self, place: Option<Place>) -> Option<Location> {
    let at = match place? {
      Place::Type(written) => self.builder.occurrence(written)?.location,
      Place::Application(application) => self.builder.applied(application)?.at,
    };
    (!at.is_entire()).then_some(at)
  }

  /// Reports, at `at`, a use of an instantiation of `user` whose expansion derives an instantiation
  /// of `def`, derived at `place`, under whose arguments the row recorded at `row` does not hold: a
  /// value that does not fit the type they give it, with the code of the site it is written at,
  /// labelled at `place` and at the value; or an argument an application leaves out that they make
  /// required, labelled at `place`, at the application and at the argument's declaration.
  fn report_derived_unfit(
    &mut self,
    at: Location,
    user: DefId,
    def: DefId,
    row: u32,
    place: Option<Place>,
  ) {
    let Some(recorded) = self.recorded.get(row as usize).copied() else {
      return;
    };
    let used = self.used(user);
    let derives = self.place_location(place);
    match recorded.owed {
      Owed::Value(OwedValue { tree, site }) => {
        let Some(RawTree { root, source, .. }) = self.raw.trees.get(tree as usize).copied() else {
          return;
        };
        let Some(node) = self.raw.nodes.get(root as usize).copied() else {
          return;
        };
        let message = format!(
          "{used} with type arguments that derive an instantiation of `{}` under which `{}`, a \
           value it holds, does not fit the type it takes",
          Excerpt(self.builder.def_name(def)),
          Excerpt(self.raw.text(source, node.span))
        );
        let (code, label) = site.refusal();
        let mut report = Diagnostic::builder(code, at)
          .message(message)
          .primary_label(label);
        if let Some(derives) = derives {
          report = report.label(derives, DERIVES);
        }
        self.reporter.report(
          report
            .label(Location::new(source, node.span), "this value")
            .build(),
        );
      }
      Owed::Omitted(OmittedArgument {
        application,
        argument,
      }) => {
        let Some(Applied {
          directive: resolution,
          at: path,
          ..
        }) = self.builder.applied(application)
        else {
          return;
        };
        let directive: &[u8] = match resolution {
          Resolution::Resolved(directive) => self.builder.def_name(directive),
          _ => b"",
        };
        let name: &[u8] = match self.builder.member_name(argument) {
          Some(sym) => self.builder.spelling(sym),
          None => b"",
        };
        let message = format!(
          "{used} with type arguments that derive an instantiation of `{}` under which `@{}` \
           requires its argument `{}`, and an application it holds leaves it out",
          Excerpt(self.builder.def_name(def)),
          Excerpt(directive),
          Excerpt(name)
        );
        let mut report = Diagnostic::builder(schema::MISSING_REQUIRED_DIRECTIVE_ARGUMENT, at)
          .message(message)
          .primary_label("missing required directive argument");
        if let Some(derives) = derives {
          report = report.label(derives, DERIVES);
        }
        report = report.label(path, "this application leaves it out");
        if let Some(declared) = self.builder.member_location(argument)
          && !declared.is_entire()
        {
          report = report.label(declared, "required here");
        }
        self.reporter.report(report.build());
      }
    }
  }

  /// Reports, at `at`, a use of an instantiation of `user` whose expansion derives an instantiation
  /// of `def`, that `stop` kept what `def` holds from being checked under its arguments.
  fn report_derived_unchecked(&mut self, at: Location, user: DefId, def: DefId, stop: Stop) {
    let sigil = if self.builder.def_kind(user) == Some(DefKind::Directive) {
      "@"
    } else {
      ""
    };
    let what = format!(
      "an instantiation of `{}` that `{sigil}{}` derives here",
      Excerpt(self.builder.def_name(def)),
      Excerpt(self.builder.def_name(user))
    );
    let message = match stop {
      Stop::Steps => Message {
        text: format!(
          "the values {what} holds could not be checked within `Limits::max_value_steps`, {}, the \
           steps the analysis may take to complete values",
          self.values.max()
        ),
        help: Cow::Borrowed(
          "raise the limit with `Limits::with_max_value_steps`, or instantiate fewer types",
        ),
      },
      Stop::Built(_) => self.stopped(stop, &format!("a value {what} holds takes")),
    };
    self.limit(at, message);
  }

  /// Reports, at `at`, where the root `user` whose expansion was running is first written, that
  /// `stop` kept what it derives from being read: once, and the closure ends.
  fn report_unexpanded(&mut self, at: Location, user: DefId, stop: Stop) {
    let (sigil, verb) = if self.builder.def_kind(user) == Some(DefKind::Directive) {
      ("@", "applied")
    } else {
      ("", "instantiated")
    };
    let what = format!(
      "what `{sigil}{}` derives under the type arguments it is {verb} with here could not be read",
      Excerpt(self.builder.def_name(user))
    );
    let limits = self.types.limits();
    let message = match stop {
      Stop::Steps => Message {
        text: format!(
          "{what} within `Limits::max_value_steps`, {}, the steps the analysis may take to \
           complete values",
          self.values.max()
        ),
        help: Cow::Borrowed(
          "raise the limit with `Limits::with_max_value_steps`, or instantiate fewer types",
        ),
      },
      Stop::Built(Limit::Depth) => Message {
        text: format!(
          "{what}: a type it derives nests deeper than `Limits::max_generic_depth`, {}, admits",
          limits.max_generic_depth()
        ),
        help: Cow::Borrowed(
          "raise the bound with `Limits::with_max_generic_depth`, or nest types less deeply",
        ),
      },
      Stop::Built(Limit::Instantiations) => Message {
        text: format!(
          "{what}: it derives more instantiations of generic definitions than \
           `Limits::max_instantiations`, {}, admits",
          limits.max_instantiations()
        ),
        help: Cow::Borrowed(
          "raise the bound with `Limits::with_max_instantiations`, or instantiate fewer types",
        ),
      },
      // The match's arm alone: a rebuild fills nothing, and never draws on this limit.
      Stop::Built(Limit::Substitutions) => Message {
        text: format!(
          "{what} within `Limits::max_substitutions`, {}",
          limits.max_substitutions()
        ),
        help: Cow::Borrowed("raise the bound with `Limits::with_max_substitutions`"),
      },
    };
    self.limit(at, message);
  }
}
