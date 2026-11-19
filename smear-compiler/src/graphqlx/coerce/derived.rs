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
//! expanded as a root, and what it holds is its own. A root's expansion reads the arguments its
//! type parameters' defaults fill too, past the most any occurrence of it writes: what a default
//! fills is derived by that default.
//!
//! The expansion reads only toward what can be judged. What an instantiation is judged for is a
//! value recorded under its definition, here, and in the implementation check an `implements` entry
//! or a union member that names its type parameters, or, where an object or an interface implements
//! one, a field argument typed by its own bare nullable type parameter with no default, which may
//! be required. A definition is relevant when it has something judged, or when one of its positions
//! holds, at any depth and through any definition, a named type of a relevant definition that does
//! not grow and that no supplied package writes, open or closed, a type a default fills among them,
//! or a type whose arguments hold one; and a position is read only when it holds such a type. A
//! derived instantiation of a definition that grows is neither judged nor expanded, so its own
//! definition makes it reach nothing: it reaches through what its arguments hold alone. That is a
//! least fixpoint, and so transitive: a definition is relevant exactly when an expansion of an
//! instantiation of it can reach an instantiation of a definition with something judged, since the
//! walk that finds it reads below each position what an expansion walks below its rebuild, a
//! written instantiation ending both, and what a rebuild puts in a type parameter's place is
//! written or was met above. So a program whose generic definitions hold nothing to judge derives
//! nothing, and pays nothing.
//!
//! # What it costs, and what ends it
//!
//! A root is expanded once, and a derived instantiation only when its definition does not grow,
//! where a type argument is passed on and never wrapped on a cycle: so the closure is finite, and
//! each instantiation it derives is new to the model, counted toward `Limits::max_instantiations`
//! and depth-checked against `Limits::max_generic_depth` as every rebuild is. Each turn of each of
//! its loops is a step of `Limits::max_value_steps`, or is bounded by the model's size:
//!
//! - finding the roots, what is relevant, and which positions and fills an expansion reads, reads
//!   the types instantiation built, each distinct type below a position or a root's fill once, with
//!   its arguments: linear in the model but for sorting, and charged nothing. Each definition keeps
//!   its useful positions alone, each distinct type once, and each root the fills, past what any
//!   occurrence of it writes, that can derive something judged;
//! - each position an expansion reads is a rebuild, a step for its root and one for each argument
//!   or element it looks at, as the check at a written instantiation pays for its own; a position
//!   whose rebuild the check of its instantiation made already is put on the walk's stack, a step;
//! - a walk's stack holds generic named types and lists alone, each a step as it is put on it. A
//!   type's distinct such arguments, or its element, are read the first time a walk meets it and
//!   kept: so a type of no type parameter costs nothing past that one reading, and a type many
//!   walks meet is read once;
//! - each derived instantiation's check is a step for each pair of it and a row, made once, as
//!   soon as it is derived;
//! - blaming the failing instantiations reads each instantiation and each edge once, each edge
//!   made by a step its walk paid; each report pass reads each occurrence and application once,
//!   finding its root by its type's nullable form, found once for each type and kept.
//!
//! So the closure does work linear in the model's size and in the steps it pays, but for sorting.
//!
//! A limit that stops an expansion is `resource-limit`, once, at where the root whose expansion was
//! running is first written, and the closure ends there. The stopped expansion keeps what it
//! derived before the stop, and what the closure derived is judged, each instantiation as it was
//! derived. The stopped root absorbs each check a limit stopped that it reaches, its one
//! `resource-limit` standing for them, and reports the first failure that is confirmed; a check a
//! limit stopped while an earlier root's expansion was running is reported at that root's
//! occurrences. The rebuild that stopped the closure is handed to the implementation check, which
//! says nothing of it again. What the closure did not reach is not judged.
//!
//! # The check, and its report
//!
//! Each instantiation the closure derives whose definition does not grow and records rows is judged
//! as a written instantiation is, by the same verdict, made once and shared by both nullnesses, a
//! step for each pair of the instantiation and a row. Each mistake has one owner (W:979): a row
//! whose value's count, against the type it takes there, a report already owns is that report's,
//! and the check passes over it to the next row. A written object literal that reports its copy of
//! a default owns that copy's count, and each count beneath it that does not fit, as the count kept
//! it. An application that reports its copy of a default owns each count beneath the default's,
//! whether a copy is made or not, but not the default's own: a row that holds that default is
//! another application's omission, a mistake of its own. Each occurrence of a root, and each
//! application of one, in id order, whose own verdict holds, reports the failing instantiation of
//! least index its expansion reaches, as `Closure::blame` finds it: with the code the same failure
//! has at a written instantiation, labelled at the type or the application that derives the failing
//! instantiation and at the value or the application that fails; or, stopped by a limit,
//! `resource-limit`. A root whose own verdict fails was reported as written, and says nothing more.
//! A derived instantiation of a definition that grows is not judged, nor is what only it derives.

use std::{
  borrow::Cow,
  collections::{BTreeMap, BTreeSet},
  format, vec,
  vec::Vec,
};

use tokora::diagnostic::Location;

use super::{Checked, Coercion, CountKey, OmittedArgument, Owed, OwedValue, Recorded};
use crate::graphqlx::{
  code::schema,
  diagnostic::{Diagnostic, Excerpt, Message},
  id::{ApplicationId, DefId, MemberId, PoolRange, TyId, TypeExprId, dense},
  model::{Applied, DefKind, Occurrence, Resolution, Shape, Written, WrittenParam},
  raw::RawTree,
  types::{
    Limit, Stop,
    closure::{Blame, Closure, DERIVES, Halt, Node, Place},
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
  /// Whether it holds, at any depth, a named type of a relevant definition that no supplied package
  /// writes, open or closed: whether reading it under an instantiation's arguments can derive one
  /// with something to judge.
  useful: bool,
}

/// What building the closure keeps while it expands.
struct Building {
  /// The closure so far.
  closure: Closure,
  /// Every generic definition's open positions, each definition's a run; once what is relevant is
  /// found, each definition's useful ones alone, each distinct type once, where first written.
  positions: Vec<Position>,
  /// Each definition's run of `positions`, by `DefId`.
  runs: Vec<PoolRange>,
  /// Whether each definition is relevant, by `DefId`.
  relevant: Vec<bool>,
  /// The arguments the roots' defaults fill that can derive something judged, each with the
  /// default that fills it, a run for each root.
  fills: Vec<(TyId, TypeExprId)>,
  /// Each root's run of `fills`, by its node.
  filled: BTreeMap<u32, PoolRange>,
  /// Each type a walk met, with its run of `children`: its distinct arguments, or its element,
  /// that are generic named types or lists, in order, read the first time a walk meets it.
  kept: BTreeMap<TyId, PoolRange>,
  /// The runs `kept` names.
  children: Vec<TyId>,
  /// The types a type's children being read have given already.
  distinct: BTreeSet<TyId>,
  /// The types the expansion being made has walked.
  seen: BTreeSet<TyId>,
  /// The children the expansion being made has derived.
  derived: BTreeSet<u32>,
  /// The types still to walk.
  stack: Vec<TyId>,
  /// The instantiations still to expand from the root being expanded, in order, and those already
  /// expanded before them.
  queue: Vec<u32>,
  /// The root whose expansion is running.
  root: u32,
  /// Each derived instantiation whose check a limit stopped, by the root whose expansion judged it.
  stalled: BTreeMap<u32, u32>,
  /// The rebuild that stopped the expansion, when a rebuild did.
  halted: Option<Halt>,
}

/// The roots of the closure, as the type occurrences and the applications are read.
#[derive(Default)]
struct Roots {
  /// Each root, with where it is first written and its definition, in the order first written.
  list: Vec<(u32, Location, DefId)>,
  /// The most type arguments any occurrence or application of each root writes, by its node: the
  /// arguments past them are filled, by their defaults, wherever it is written.
  spelled: BTreeMap<u32, u32>,
}

/// The closure, with what stopped it.
struct Derivation {
  /// The closure.
  closure: Closure,
  /// The root whose expansion a limit stopped, whose `resource-limit` is reported; `None` when no
  /// limit stopped one.
  stopped: Option<u32>,
  /// Each derived instantiation whose check a limit stopped, by the root whose expansion judged it.
  stalled: BTreeMap<u32, u32>,
  /// The rebuild that stopped the closure, when a rebuild did.
  halted: Option<Halt>,
}

impl Derivation {
  /// Whether the check of the derived instantiation `at`, which a limit stopped, is one the
  /// closure's `resource-limit` stands for: it was judged while the expansion that report names was
  /// running.
  fn covers(&self, at: u32) -> bool {
    self
      .stopped
      .is_some_and(|root: u32| self.stalled.get(&at) == Some(&root))
  }
}

impl Coercion<'_, '_, '_> {
  /// Builds the closure of derived instantiations, judges what it derives, and keeps it in the type
  /// services for the implementation check.
  pub(super) fn derived(&mut self) {
    let derivation = self.derive_closure();
    self.judge_derived(&derivation);
    self.types.halted = derivation.halted;
    self.types.closure = derivation.closure;
  }

  /// The closure: every root, and what each derives through the definitions that do not grow, as
  /// the module says, with what stopped it. A limit that stops an expansion is reported once, and
  /// ends it.
  fn derive_closure(&mut self) -> Derivation {
    let mut closure = Closure::default();
    let mut roots = Roots::default();
    for index in 0..self.builder.type_expr_count() {
      let Some(occurrence) = self.builder.occurrence(TypeExprId::new(index)) else {
        continue;
      };
      let (Written::Named(def), Some(ty)) = (occurrence.written, occurrence.ty) else {
        continue;
      };
      let at = occurrence.location;
      let written = dense(occurrence.args.len());
      self.root(&mut closure, &mut roots, at, def, ty, written);
    }
    for index in 0..self.builder.application_count() {
      let application = ApplicationId::new(index);
      let (
        Some(Applied {
          directive: Resolution::Resolved(def),
          written,
          at,
        }),
        Some(ty),
      ) = (
        self.builder.applied(application),
        self.builder.application_ty(application),
      )
      else {
        continue;
      };
      self.root(&mut closure, &mut roots, at, def, ty, written);
    }
    if roots.list.is_empty() {
      return Derivation {
        closure,
        stopped: None,
        stalled: BTreeMap::new(),
        halted: None,
      };
    }
    let mut building = Building {
      closure,
      positions: Vec::new(),
      runs: Vec::new(),
      relevant: Vec::new(),
      fills: Vec::new(),
      filled: BTreeMap::new(),
      kept: BTreeMap::new(),
      children: Vec::new(),
      distinct: BTreeSet::new(),
      seen: BTreeSet::new(),
      derived: BTreeSet::new(),
      stack: Vec::new(),
      queue: Vec::new(),
      root: 0,
      stalled: BTreeMap::new(),
      halted: None,
    };
    self.relevance(&mut building, &roots);
    if building.relevant.iter().any(|relevant: &bool| *relevant) {
      for &(root, at, def) in &roots.list {
        building.root = root;
        building.queue.clear();
        building.queue.push(root);
        let mut next: usize = 0;
        while let Some(&expanding) = building.queue.get(next) {
          next += 1;
          if let Err(stop) = self.expand(&mut building, expanding) {
            self.report_unexpanded(at, def, stop);
            return Derivation {
              closure: building.closure,
              stopped: Some(root),
              stalled: building.stalled,
              halted: building.halted,
            };
          }
        }
      }
    }
    Derivation {
      closure: building.closure,
      stopped: None,
      stalled: building.stalled,
      halted: None,
    }
  }

  /// Joins `ty`, written at `at` with `written` type arguments, to `closure` as a root, and to
  /// `roots` when it is new: when a supplied package writes it and it is a closed named type of
  /// `def`, a generic definition.
  fn root(
    &mut self,
    closure: &mut Closure,
    roots: &mut Roots,
    at: Location,
    def: DefId,
    ty: TyId,
    written: u32,
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
      roots.list.push((node, at, def));
    }
    let most = roots.spelled.entry(node).or_insert(0);
    *most = (*most).max(written);
  }

  /// Finds every generic definition's open positions, which definitions are relevant and which
  /// positions are useful, as the module says, in time linear in the types below the positions but
  /// for one sort, reading the types instantiation built for nothing.
  ///
  /// It is a least fixpoint, found once with a worklist, and so transitive. A definition with
  /// something judged is relevant. Below the positions, each named type of a relevant definition
  /// that no supplied package writes, open or closed, reaches; a type that holds one that reaches,
  /// as an argument or as its element, reaches, whatever its own definition; and a position whose
  /// type reaches is useful, and its definition relevant. So a definition is relevant exactly when
  /// an expansion of an instantiation of it can reach an instantiation of a definition with
  /// something judged. Below a rebuilt position `collect` meets what this walk met below the
  /// position, a type of no type parameter and a written instantiation ending both; and, in a type
  /// parameter's place, an argument of the instantiation being expanded, which is written, ending
  /// the walk, or was met already: by the walk that derived that instantiation, or by a root's walk
  /// of the arguments its defaults fill.
  ///
  /// The arguments a root's defaults fill are read by the same walk, those past the most any
  /// occurrence of the root writes: a root's expansion walks the ones that reach alone. Then each
  /// definition's run of positions keeps its useful ones alone, each distinct type once, at its
  /// first place, so that an expansion reads nothing it cannot use.
  fn relevance(&mut self, building: &mut Building, roots: &Roots) {
    let count = self.builder.def_count();
    building.runs = vec![PoolRange { start: 0, end: 0 }; count as usize];
    building.relevant = vec![false; count as usize];
    // What is judged: a value recorded under a definition, or what the implementation check
    // judges at an instantiation. With nothing judged, nothing is relevant, and nothing is read.
    let mut defs: Vec<DefId> = Vec::new();
    for row in &self.recorded {
      if let Some(slot) = building.relevant.get_mut(row.def.get() as usize)
        && !*slot
      {
        *slot = true;
        defs.push(row.def);
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
        defs.push(def);
      }
    }
    if defs.is_empty() {
      return;
    }
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
    // Each argument a root's default fills, which no occurrence of the root writes, with the
    // default, each root's a run.
    let mut candidates: Vec<(TyId, TypeExprId)> = Vec::new();
    let mut runs: Vec<(u32, PoolRange)> = Vec::new();
    for &(root, _, def) in &roots.list {
      let Some(node) = building.closure.node(root) else {
        continue;
      };
      let start = dense(candidates.len());
      let written = roots.spelled.get(&root).copied().unwrap_or(0);
      let count = dense(self.builder.ty_args(node.ty).len());
      for slot in written..count {
        let Some(&argument) = self.builder.ty_args(node.ty).get(slot as usize) else {
          break;
        };
        if let Some(default) = self
          .builder
          .type_param(def, slot)
          .and_then(|param: WrittenParam| param.default)
        {
          candidates.push((argument, default));
        }
      }
      runs.push((
        root,
        PoolRange {
          start,
          end: dense(candidates.len()),
        },
      ));
    }
    // Every distinct type below a position or a filled argument, once, by its index: each generic
    // named type no supplied package writes, and each list. A written instantiation ends the walk,
    // as it ends an expansion's, and a type of no type parameter holds nothing.
    let mut index: BTreeMap<TyId, u32> = BTreeMap::new();
    let mut types: Vec<TyId> = Vec::new();
    building.stack.clear();
    for position in &building.positions {
      building.stack.push(position.ty);
    }
    for &(argument, _) in &candidates {
      building.stack.push(argument);
    }
    while let Some(ty) = building.stack.pop() {
      if index.contains_key(&ty) {
        continue;
      }
      match self.builder.ty_shape(ty) {
        Some(Shape::Named(def)) => {
          if self.builder.type_param_count(def) == 0 {
            continue;
          }
          let nullable = self.nullable(ty, def);
          if building.closure.find(nullable).is_some() {
            continue;
          }
          index.insert(ty, dense(types.len()));
          types.push(ty);
          building.stack.extend_from_slice(self.builder.ty_args(ty));
        }
        Some(Shape::List(element)) => {
          index.insert(ty, dense(types.len()));
          types.push(ty);
          building.stack.push(element);
        }
        Some(Shape::Param(_) | Shape::Unresolved) | None => {}
      }
    }
    // Each type's parents, by the type: what holds it as an argument or as its element. Each named
    // type, by its definition. Each position, by its type.
    let mut parents: Vec<(u32, u32)> = Vec::new();
    let mut instances: Vec<(DefId, u32)> = Vec::new();
    for (at, &ty) in types.iter().enumerate() {
      let at = dense(at);
      match self.builder.ty_shape(ty) {
        Some(Shape::Named(def)) => {
          instances.push((def, at));
          for argument in self.builder.ty_args(ty) {
            if let Some(&child) = index.get(argument) {
              parents.push((child, at));
            }
          }
        }
        Some(Shape::List(element)) => {
          if let Some(&child) = index.get(&element) {
            parents.push((child, at));
          }
        }
        Some(Shape::Param(_) | Shape::Unresolved) | None => {}
      }
    }
    parents.sort_unstable();
    parents.dedup();
    instances.sort_unstable();
    let mut placed: Vec<(u32, u32)> = Vec::new();
    for (at, position) in building.positions.iter().enumerate() {
      if let Some(&ty) = index.get(&position.ty) {
        placed.push((ty, dense(at)));
      }
    }
    placed.sort_unstable();
    // The worklist: each definition made relevant makes its named types reach; each type that
    // reaches makes its parents reach, and each position of it useful, its definition relevant.
    // Each definition, type, parent and position is taken once.
    let mut reaching: Vec<bool> = vec![false; types.len()];
    let mut work: Vec<u32> = Vec::new();
    while let Some(def) = defs.pop() {
      // A derived instantiation of a definition that grows is neither judged nor expanded: its
      // definition makes it reach nothing, and it reaches through what its arguments hold alone.
      let from = if self.builder.def_grows(def) {
        instances.len()
      } else {
        instances.partition_point(|&(of, _): &(DefId, u32)| of < def)
      };
      for &(of, at) in instances.get(from..).unwrap_or_default() {
        if of != def {
          break;
        }
        if let Some(slot) = reaching.get_mut(at as usize)
          && !*slot
        {
          *slot = true;
          work.push(at);
        }
      }
      while let Some(at) = work.pop() {
        let from = parents.partition_point(|&(child, _): &(u32, u32)| child < at);
        for &(child, parent) in parents.get(from..).unwrap_or_default() {
          if child != at {
            break;
          }
          if let Some(slot) = reaching.get_mut(parent as usize)
            && !*slot
          {
            *slot = true;
            work.push(parent);
          }
        }
        let from = placed.partition_point(|&(ty, _): &(u32, u32)| ty < at);
        for &(ty, held) in placed.get(from..).unwrap_or_default() {
          if ty != at {
            break;
          }
          let Some(position) = building.positions.get_mut(held as usize) else {
            continue;
          };
          position.useful = true;
          let owner = position.def;
          if let Some(slot) = building.relevant.get_mut(owner.get() as usize)
            && !*slot
          {
            *slot = true;
            defs.push(owner);
          }
        }
      }
    }
    // Each root's filled arguments that reach, in order.
    let reaches = |ty: TyId| -> bool {
      index
        .get(&ty)
        .and_then(|at: &u32| reaching.get(*at as usize))
        .copied()
        .unwrap_or(false)
    };
    for &(root, run) in &runs {
      let start = dense(building.fills.len());
      for &(argument, default) in candidates
        .get(run.start as usize..run.end as usize)
        .unwrap_or_default()
      {
        if reaches(argument) {
          building.fills.push((argument, default));
        }
      }
      let end = dense(building.fills.len());
      if end > start {
        building.filled.insert(root, PoolRange { start, end });
      }
    }
    // Each definition's useful positions alone, each distinct type once, at its first place.
    let mut compact: Vec<Position> = Vec::new();
    for slot in &mut building.runs {
      let start = dense(compact.len());
      building.distinct.clear();
      for &position in building
        .positions
        .get(slot.start as usize..slot.end as usize)
        .unwrap_or_default()
      {
        if position.useful && building.distinct.insert(position.ty) {
          compact.push(position);
        }
      }
      *slot = PoolRange {
        start,
        end: dense(compact.len()),
      };
    }
    building.positions = compact;
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
  /// its type, and, for a root, each argument its defaults fill that can derive something judged,
  /// each walked for the instantiations it derives. `Err` when a limit stops a rebuild, or the
  /// budget a type the walk meets. Whatever stops it, the node keeps the children it derived before
  /// the stop, so that what it reached is blamed on it.
  fn expand(&mut self, building: &mut Building, at: u32) -> Result<(), Stop> {
    let from = building.closure.next_edge();
    building.seen.clear();
    building.derived.clear();
    let read = self.derive_from(building, at);
    building.closure.expanded(at, from);
    read
  }

  /// What `expand` reads of the node `at`.
  fn derive_from(&mut self, building: &mut Building, at: u32) -> Result<(), Stop> {
    let Some(node) = building.closure.node(at) else {
      return Ok(());
    };
    let none = PoolRange { start: 0, end: 0 };
    let run = building
      .runs
      .get(node.def.get() as usize)
      .copied()
      .unwrap_or(none);
    for index in run.start..run.end {
      let Some(&Position { place, ty, .. }) = building.positions.get(index as usize) else {
        break;
      };
      let rebuilt = match self.rebuilt(ty, node.ty, node.def, false) {
        Ok(rebuilt) => rebuilt,
        Err(stop) => {
          building.halted = Some(Halt {
            instantiation: node.ty,
            written: ty,
            stop,
          });
          return Err(stop);
        }
      };
      if let Some(under) = rebuilt {
        self.collect(building, under, place)?;
      }
    }
    if node.written {
      // What a type parameter's default fills is derived by that default.
      let run = building.filled.get(&at).copied().unwrap_or(none);
      for index in run.start..run.end {
        let Some(&(argument, default)) = building.fills.get(index as usize) else {
          break;
        };
        self.collect(building, argument, Place::Type(default))?;
      }
    }
    Ok(())
  }

  /// Walks `ty`, closed, for the instantiations it derives at `place`, with an explicit stack that
  /// holds generic named types and lists alone, each a step of `Limits::max_value_steps` as it is
  /// put on it: a type's distinct such arguments, or its element, are read the first time a walk
  /// meets it and kept, so a type of no type parameter is never put on the stack. Each
  /// instantiation of a relevant generic definition no supplied package writes is a child of the
  /// node being expanded, once; one new to the closure whose definition does not grow is judged at
  /// once, so that a limit that stops the closure later leaves it judged, and queued to be
  /// expanded. A written one is no child, and its arguments are its own. `Err` when the budget
  /// cannot pay for a type.
  fn collect(&mut self, building: &mut Building, ty: TyId, place: Place) -> Result<(), Stop> {
    building.stack.clear();
    if !self.walked(ty) {
      return Ok(());
    }
    if !self.charge(1) {
      return Err(Stop::Steps);
    }
    building.stack.push(ty);
    while let Some(held) = building.stack.pop() {
      if !building.seen.insert(held) {
        continue;
      }
      if let Some(Shape::Named(def)) = self.builder.ty_shape(held) {
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
            building.closure.derive(child, place);
            if new && !grows {
              if matches!(self.derived_verdict(def, nullable), Checked::Stopped(_)) {
                building.stalled.insert(child, building.root);
              }
              building.queue.push(child);
            }
          }
        }
      }
      let run = self.children_of(building, held);
      for index in run.start..run.end {
        let Some(&child) = building.children.get(index as usize) else {
          break;
        };
        if !self.charge(1) {
          return Err(Stop::Steps);
        }
        building.stack.push(child);
      }
    }
    Ok(())
  }

  /// The run of `building.children` that holds the distinct arguments of `ty`, or its element, that
  /// are generic named types or lists, in order: read the first time a walk meets `ty`, and kept.
  fn children_of(&self, building: &mut Building, ty: TyId) -> PoolRange {
    if let Some(&run) = building.kept.get(&ty) {
      return run;
    }
    let start = dense(building.children.len());
    building.distinct.clear();
    match self.builder.ty_shape(ty) {
      Some(Shape::Named(_)) => {
        for &argument in self.builder.ty_args(ty) {
          if self.walked(argument) && building.distinct.insert(argument) {
            building.children.push(argument);
          }
        }
      }
      Some(Shape::List(element)) => {
        if self.walked(element) {
          building.children.push(element);
        }
      }
      Some(Shape::Param(_) | Shape::Unresolved) | None => {}
    }
    let run = PoolRange {
      start,
      end: dense(building.children.len()),
    };
    building.kept.insert(ty, run);
    run
  }

  /// Whether a walk puts `ty` on its stack: a generic named type or a list. A type of no type
  /// parameter, a type parameter and the unresolved type hold nothing a walk derives.
  fn walked(&self, ty: TyId) -> bool {
    match self.builder.ty_shape(ty) {
      Some(Shape::Named(def)) => self.builder.type_param_count(def) > 0,
      Some(Shape::List(_)) => true,
      Some(Shape::Param(_) | Shape::Unresolved) | None => false,
    }
  }

  /// Reports, at each occurrence and each application of a root whose own verdict holds, the
  /// failing instantiation its expansion reaches first, of those the closure derives whose
  /// definition does not grow, each judged when it was derived. A check a limit stopped that the
  /// closure's one `resource-limit` stands for says nothing more.
  fn judge_derived(&mut self, derivation: &Derivation) {
    if self.recorded.is_empty() {
      return;
    }
    let closure = &derivation.closure;
    let mut failing: Vec<u32> = Vec::new();
    let mut confirmed: Vec<u32> = Vec::new();
    for at in 0..closure.len() {
      let Some(node) = closure.node(at) else {
        break;
      };
      if node.written || node.grows {
        continue;
      }
      match self.derived_verdict(node.def, node.ty) {
        Checked::Fits => {}
        Checked::Unfit(_) => {
          failing.push(at);
          confirmed.push(at);
        }
        Checked::Stopped(_) if derivation.covers(at) => {}
        Checked::Stopped(_) => failing.push(at),
      }
    }
    if failing.is_empty() {
      return;
    }
    let blamed = closure.blame(&failing);
    // The root whose expansion a limit stopped absorbs each check a limit stopped, its one
    // `resource-limit` standing for them: it reports the first failure that is confirmed.
    let absorbed = derivation.stopped.map(|_: u32| closure.blame(&confirmed));
    let absorbed = absorbed.as_deref();
    for index in 0..self.builder.type_expr_count() {
      let Some(occurrence) = self.builder.occurrence(TypeExprId::new(index)) else {
        continue;
      };
      let (Written::Named(def), Some(ty)) = (occurrence.written, occurrence.ty) else {
        continue;
      };
      let at = occurrence.location;
      self.report_derived(derivation, &blamed, absorbed, at, def, ty);
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
      self.report_derived(derivation, &blamed, absorbed, at, def, ty);
    }
  }

  /// The verdict of `ty`, the nullable form of an instantiation of `def` the closure derives: made
  /// the first time it is asked, as a written instantiation's is, and kept; `Fits` when `def`
  /// records nothing. A row whose value's count, against the type it takes under `ty`, a report
  /// owns already is that report's mistake (W:979): the check passes over it to the next row, so
  /// each row is still checked once.
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
    let mut from = start;
    let verdict = loop {
      let verdict = self.check(def, ty, from, end);
      let Checked::Unfit(row) = verdict else {
        break verdict;
      };
      if !self.owned_row(def, ty, row) {
        break verdict;
      }
      from = (row as usize).saturating_add(1);
    };
    self.checked.insert(ty, verdict);
    verdict
  }

  /// Whether the row recorded at `row`, of `def`, is a value whose count against the type it takes
  /// under `ty` a report owns already. That type was rebuilt, and kept, when the row was checked,
  /// and is read again for nothing.
  fn owned_row(&mut self, def: DefId, ty: TyId, row: u32) -> bool {
    let Some(Recorded {
      ty: written,
      owed: Owed::Value(OwedValue { tree, .. }),
      ..
    }) = self.recorded.get(row as usize).copied()
    else {
      return false;
    };
    match self.rebuilt(written, ty, def, false) {
      Ok(Some(under)) => self.owned.contains(&CountKey {
        tree,
        ty: Some(under),
      }),
      Ok(None) | Err(_) => false,
    }
  }

  /// Reports at `at`, an occurrence or an application of `ty`, an instantiation of `user`, the
  /// failing instantiation its expansion reaches first, as `blamed` keeps it, when a supplied
  /// package writes it there, `ty` is a root, and its own verdict holds. The root whose expansion a
  /// limit stopped reads `absorbed`, the blame of the confirmed failures alone: its
  /// `resource-limit` stands for every check a limit stopped that it reaches, and what it reports
  /// is the first failure that is confirmed.
  fn report_derived(
    &mut self,
    derivation: &Derivation,
    blamed: &[Option<Blame>],
    absorbed: Option<&[Option<Blame>]>,
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
    let Some(root) = derivation.closure.find(nullable) else {
      return;
    };
    let blamed = match absorbed {
      Some(confirmed) if derivation.stopped == Some(root) => confirmed,
      _ => blamed,
    };
    let Some(&Some(Blame { node, place })) = blamed.get(root as usize) else {
      return;
    };
    let Some(failed) = derivation.closure.node(node) else {
      return;
    };
    match self.checked.get(&failed.ty).copied() {
      Some(Checked::Unfit(row)) => self.report_derived_unfit(at, user, failed.def, row, place),
      Some(Checked::Stopped(_)) if derivation.stopped == Some(root) => {}
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
