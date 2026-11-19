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
//! definition's members' types, its `implements` entries and union members, and the complete type
//! of each application on the definition or on one of its members, rebuilt as the check at a
//! written instantiation rebuilds a member's type, through the same remembered rebuild. Each
//! instantiation of a generic definition the rebuilt types hold, at any depth, is derived, with the
//! type or the application that derives it; a written one is no child, being judged where it is
//! written and expanded as a root, and what it holds is its own. A root's expansion reads the
//! arguments its type parameters' defaults fill too, past the most any occurrence of it writes:
//! what a default fills is derived by that default. A contribution the orphan rule refuses, which
//! another package writes into a definition, derives nothing, grows nothing and makes nothing
//! relevant: every walk over a definition's contributions, here, in the growth graph and where
//! coercion records a value, reads those the rule accepts alone, as `rules::accepted` answers, the
//! rule's own test. A type written inside a refused contribution is a written occurrence like any
//! other, judged and expanded where it is written; a refused contribution adds nothing these walks
//! read to the definition it extends.
//!
//! What this module keeps a refused contribution out of is its own walks, relevance, expansion and
//! growth, and what coercion records. A derived instantiation is judged against its definition's
//! shape as the written check reads it, every contribution's members included, a refused
//! extension's among them: an input object's fields, by name, required or defaulted, and an enum's
//! values, by name, are read off the indexes coercion builds once from every contribution, which
//! the check where a value is written reads too. So a derived instantiation and a written one are
//! judged against one shape.
//!
//! The expansion reads only toward what can be judged. What an instantiation is judged for is a
//! value recorded under its definition, here, and in the implementation check an `implements` entry
//! or a union member that names its type parameters, or an argument typed by its own bare nullable
//! type parameter with no default, which may be required, that a field of an object or an interface
//! adds beside the field of its name of an interface, no marker, that one of the definition's
//! entries names. A definition is relevant when it has something judged, or when one of its
//! positions holds, at any depth and through any definition, a named type of a relevant definition
//! that does not grow and that no supplied package writes, open or closed, a type a default fills
//! among them, or a type whose arguments hold one; and a position is read only when it holds such a
//! type. A derived instantiation of a definition that grows is neither judged nor expanded, so its
//! own definition makes it reach nothing: it reaches through what its arguments hold alone. That is
//! a least fixpoint, and so transitive: a definition is relevant exactly when an expansion of an
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
//!   occurrence of it writes, that can derive something judged. Whether a field adds an argument
//!   beside an interface's field reads each definition's own fields once, the first of each name,
//!   as the implementation check does, and looks up every interface's fields and arguments, indexed
//!   once by name: the lookups across the definitions' entries are four at most for each type
//!   occurrence and definition the model holds, and a definition asked about past them is relevant,
//!   read generously, what the closure then reads of it being charged as everything it reads is;
//! - each position an expansion reads is a rebuild, a step for its root and one for each argument
//!   or element it looks at, as the check at a written instantiation pays for its own; a position
//!   whose rebuild the check of its instantiation made already is met by the walk, a step;
//! - a walk meets generic named types and lists alone, each a step as it is met, and each reached
//!   as soon as it is paid for, before the next is charged, so that a budget that stops the walk
//!   loses nothing it paid for. A type's distinct such arguments, or its element, are read the
//!   first time a walk meets it and kept: so a type of no type parameter costs nothing past that
//!   one reading, and a type many walks meet is read once;
//! - each derived instantiation's check is a step for each pair of it and a row, made once, as
//!   soon as it is derived;
//! - blaming the failing instantiations reads each instantiation and each edge once, each edge
//!   made by a step its walk paid; each report pass reads each occurrence and application once,
//!   finding its root by its type's nullable form, found once for each type and kept.
//!
//! So the closure does work linear in the model's size and in the steps it pays, but for sorting.
//!
//! A limit that stops an expansion is `resource-limit` at where the root whose expansion was
//! running is first written, and the closure ends there. As every `resource-limit`, it is said once
//! for each written occurrence and kind of limit, whichever phase or path meets it there first,
//! each phase's step budget a kind of its own and a limit on types, which every phase shares, one
//! kind: so the same limit met at the same place by the written check of that root, by an
//! application's omitted argument, or later by the implementation check, is said once. The stopped
//! expansion keeps what it derived before the stop, and what the closure derived is judged, each
//! instantiation as it was derived. Each occurrence of a root reports the first failure its
//! expansion reaches, a check a limit stopped among them, which is `resource-limit` there; but
//! where a `resource-limit` of that kind was said already, at the root's first occurrence when the
//! same limit stopped its expansion among them, the occurrence reports its first confirmed failure
//! instead, so that a stop said once hides nothing confirmed. What the closure did not reach is not
//! judged.
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
//! Likewise, where a limit on types stops a rebuild at a position, the expansion ends there with
//! its `resource-limit`: what fails inside the types that rebuild had built in part is not
//! guaranteed a report, and raising the limit shows it. A derived instantiation of a definition
//! that grows is not judged, nor is what only it derives.

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
  id::{ApplicationId, DefId, MemberId, PoolRange, Sym, TyId, TypeExprId, dense},
  model::{Applied, DefKind, Occurrence, Resolution, Shape, Written, WrittenParam},
  raw::RawTree,
  rules::{Accepted, accepted, accepts_application},
  types::{
    Limit, Spent, Stop,
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

/// Every interface's fields, and their arguments, by name, for the question whether a field adds
/// an argument beside the field of its name of an interface its definition implements: built the
/// first time relevance asks it, so that each answer is a few lookups, with what the lookups may
/// read in all.
#[derive(Default)]
struct Declared {
  /// Whether `fields`, `arguments` and `left` are set.
  built: bool,
  /// Each interface's fields, by the interface, then name, then id.
  fields: Vec<(DefId, Sym, MemberId)>,
  /// Each interface field's arguments' names, by the field, then name.
  arguments: Vec<(MemberId, Sym)>,
  /// The lookups the questions may still make across the definitions' entries: four for each type
  /// occurrence and definition the model holds. Past them, a definition that adds such an argument
  /// is relevant, read generously.
  left: u32,
  /// The own fields of the definition being asked about, the first of each name, by name.
  own: Vec<(Sym, MemberId)>,
  /// Those of them that add an argument the implementation check may judge, by name, each with its
  /// run of `names`.
  adding: Vec<(Sym, MemberId, PoolRange)>,
  /// The runs `adding` names: the names of the arguments each field adds, sorted, each once.
  names: Vec<Sym>,
  /// The names of the arguments the field being read adds, before they are sorted and kept once.
  added: Vec<Sym>,
  /// The interfaces the definition being asked about's own entries name, each once.
  interfaces: Vec<DefId>,
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
  /// the module says. A limit that stops an expansion is reported, and ends it.
  fn derive_closure(&mut self) -> Closure {
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
      return closure;
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
    };
    self.relevance(&mut building, &roots);
    if building.relevant.iter().any(|relevant: &bool| *relevant) {
      for &(root, at, def) in &roots.list {
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
    }
    building.closure
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
    let mut declared = Declared::default();
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
        && self.judged_when_implemented(def, &mut declared)
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

  /// Appends to `into` each open position of `def` that a contribution the orphan rule accepts
  /// writes, as `rules::accepted` answers: its `implements` entries and, for a union, its members,
  /// those whose types resolved, as the implementation check reads them; the complete types of its
  /// applications the rule accepts; and each of its members' types and their applications' complete
  /// types, a field's arguments' too. What a contribution the rule refuses writes derives nothing.
  fn push_positions(&self, def: DefId, into: &mut Vec<Position>) {
    let model = self.builder.so_far();
    let union = self.builder.def_kind(def) == Some(DefKind::Union);
    for part in model.def(def).contributions() {
      let judged = accepted(model, self.sources, def, &part);
      if judged == Accepted::Refused {
        continue;
      }
      let members: &[TypeExprId] = if union { part.union_members() } else { &[] };
      for &item in part.implements().iter().chain(members) {
        if self.resolved(item).is_some() {
          self.push_type(def, item, into);
        }
      }
      for &application in part.applications() {
        if accepts_application(model, self.meanings, judged, application) {
          self.push_application(def, application, into);
        }
      }
      for member in part.members() {
        self.push_member(def, member, into);
        for argument in self.builder.member_arguments(member) {
          self.push_member(def, argument, into);
        }
      }
    }
  }

  /// Appends to `into` the type and the applications of `member`, of `def`, that are open: what the
  /// contribution that wrote the member wrote with it, which `push_positions` read as accepted.
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
  /// an object or an interface, an argument one of its fields adds beside the field of its name of
  /// an interface one of its entries names, but a marker, whose fields the check does not compare,
  /// typed by its own bare nullable type parameter with no default, which may be required, each
  /// name once. An argument of a field that implements no interface's field, or that the
  /// interface's field declares, is the check's never. Entries, members and fields are those of the
  /// contributions the orphan rule accepts, as `rules::accepted` answers, entries and members whose
  /// types resolved, as the check reads them: what a contribution the rule refuses writes makes
  /// nothing relevant.
  ///
  /// Of the definition's own fields, the first of each name is read, the one the implementation
  /// check finds to implement an interface's field of that name. Then, for each interface its
  /// entries name, the fewer of the fields that add such an argument and of the interface's own
  /// fields is read, and the other side looked up in `declared`, and so for the arguments of each
  /// field of one name on both sides, each lookup paid from what `declared` has left: so the
  /// questions read the model's definitions once each, and, across their entries, no more than four
  /// lookups for each type occurrence and definition the model holds. A definition asked about past
  /// them is relevant.
  fn judged_when_implemented(&self, def: DefId, declared: &mut Declared) -> bool {
    let model = self.builder.so_far();
    let kind = self.builder.def_kind(def);
    let union = kind == Some(DefKind::Union);
    let wide = matches!(kind, Some(DefKind::Object | DefKind::Interface));
    // An argument the interface's field does not declare is the check's only where the definition
    // implements one: the interfaces its entries name, each once, but a marker, whose fields the
    // check does not compare; and its own fields, the first of each name, as the check indexes
    // them. Each from the contributions the orphan rule accepts.
    declared.interfaces.clear();
    declared.own.clear();
    for part in model.def(def).contributions() {
      if accepted(model, self.sources, def, &part) == Accepted::Refused {
        continue;
      }
      let members: &[TypeExprId] = if union { part.union_members() } else { &[] };
      for &item in part.implements().iter().chain(members) {
        let Some(ty) = self.resolved(item) else {
          continue;
        };
        if !self.builder.ty_closed(ty) {
          return true;
        }
        if wide
          && let Some(Shape::Named(interface)) = self.builder.ty_shape(ty)
          && interface != def
          && self.builder.def_kind(interface) == Some(DefKind::Interface)
          && !self.builder.def_marker(interface)
        {
          declared.interfaces.push(interface);
        }
      }
      if wide {
        for field in part.members() {
          if let Some(name) = self.builder.member_name(field) {
            declared.own.push((name, field));
          }
        }
      }
    }
    if declared.interfaces.is_empty() {
      return false;
    }
    declared.own.sort_unstable();
    declared
      .own
      .dedup_by_key(|&mut (name, _): &mut (Sym, MemberId)| name);
    declared.adding.clear();
    declared.names.clear();
    for &(name, field) in &declared.own {
      declared.added.clear();
      for argument in self.builder.member_arguments(field) {
        if self.bare_param(argument, def)
          && !self.raw.defaults.contains_key(&argument)
          && let Some(added) = self.builder.member_name(argument)
        {
          declared.added.push(added);
        }
      }
      if declared.added.is_empty() {
        continue;
      }
      // Each name once: the check passes over every argument of a name its interface's field
      // declares, however often it is written.
      declared.added.sort_unstable();
      declared.added.dedup();
      let start = dense(declared.names.len());
      declared.names.extend_from_slice(&declared.added);
      let end = dense(declared.names.len());
      declared
        .adding
        .push((name, field, PoolRange { start, end }));
    }
    if declared.adding.is_empty() {
      return false;
    }
    declared.interfaces.sort_unstable();
    declared.interfaces.dedup();
    self.declare(declared);
    let Declared {
      fields,
      arguments,
      left,
      adding,
      names,
      interfaces,
      ..
    } = declared;
    for &interface in interfaces.iter() {
      let from =
        fields.partition_point(|&(owner, _, _): &(DefId, Sym, MemberId)| owner < interface);
      let to = fields.partition_point(|&(owner, _, _): &(DefId, Sym, MemberId)| owner <= interface);
      let theirs = fields.get(from..to).unwrap_or(&[]);
      // The fewer side is read, and the other looked up by name.
      if adding.len() <= theirs.len() {
        for &(name, _, run) in adding.iter() {
          if !spend(left, 1) {
            return true;
          }
          for &(_, _, field) in
            same_name(theirs, name, |&(_, written, _): &(DefId, Sym, MemberId)| {
              written
            })
          {
            if adds_beside(names.as_slice(), run, arguments.as_slice(), field, left) {
              return true;
            }
          }
        }
      } else {
        for &(_, name, field) in theirs {
          if !spend(left, 1) {
            return true;
          }
          for &(_, _, run) in same_name(
            adding.as_slice(),
            name,
            |&(written, _, _): &(Sym, MemberId, PoolRange)| written,
          ) {
            if adds_beside(names.as_slice(), run, arguments.as_slice(), field, left) {
              return true;
            }
          }
        }
      }
    }
    false
  }

  /// The type of `item`, an `implements` entry or a union member, when it resolved: the items the
  /// implementation check reads, as it gathers them, of a contribution the orphan rule accepts.
  fn resolved(&self, item: TypeExprId) -> Option<TyId> {
    let ty = self.builder.occurrence(item)?.ty?;
    (!matches!(self.builder.ty_shape(ty), Some(Shape::Unresolved) | None)).then_some(ty)
  }

  /// Fills `declared` with every interface's fields that a contribution the orphan rule accepts
  /// writes, those the implementation check compares, and each one's arguments, by name, and with
  /// what its lookups may read: once.
  fn declare(&self, declared: &mut Declared) {
    if declared.built {
      return;
    }
    declared.built = true;
    declared.left = self
      .builder
      .type_expr_count()
      .saturating_add(self.builder.def_count())
      .saturating_mul(4);
    let model = self.builder.so_far();
    for index in 0..self.builder.def_count() {
      let interface = DefId::new(index);
      if self.builder.def_kind(interface) != Some(DefKind::Interface) {
        continue;
      }
      for part in model.def(interface).contributions() {
        if accepted(model, self.sources, interface, &part) == Accepted::Refused {
          continue;
        }
        for field in part.members() {
          let Some(name) = self.builder.member_name(field) else {
            continue;
          };
          declared.fields.push((interface, name, field));
          for argument in self.builder.member_arguments(field) {
            if let Some(name) = self.builder.member_name(argument) {
              declared.arguments.push((field, name));
            }
          }
        }
      }
    }
    declared.fields.sort_unstable();
    declared.arguments.sort_unstable();
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
      if let Some(under) = self.rebuilt(ty, node.ty, node.def, false)? {
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
  /// met: a type's distinct such arguments, or its element, are read the first time a walk meets it
  /// and kept, so a type of no type parameter is never met. Each type is reached as soon as it is
  /// paid for, before the next is charged, so that a budget that stops the walk loses nothing it
  /// paid for. `Err` when the budget cannot pay for a type.
  fn collect(&mut self, building: &mut Building, ty: TyId, place: Place) -> Result<(), Stop> {
    building.stack.clear();
    if !self.walked(ty) {
      return Ok(());
    }
    if !self.charge(1) {
      return Err(Stop::Steps);
    }
    if self.reach(building, ty, place) {
      building.stack.push(ty);
    }
    while let Some(held) = building.stack.pop() {
      let run = self.children_of(building, held);
      for index in run.start..run.end {
        let Some(&child) = building.children.get(index as usize) else {
          break;
        };
        if !self.charge(1) {
          return Err(Stop::Steps);
        }
        if self.reach(building, child, place) {
          building.stack.push(child);
        }
      }
    }
    Ok(())
  }

  /// Reaches `ty`, a type the walk deriving at `place` paid for, and whether the walk reads below
  /// it: not when the expansion being made has met it already, nor when it is a written
  /// instantiation, whose arguments are its own. An instantiation of a relevant generic definition
  /// no supplied package writes is a child of the node being expanded, once, with its edge; one
  /// new to the closure whose definition does not grow is judged at once, so that a limit that
  /// stops the closure later leaves it judged, and queued to be expanded.
  fn reach(&mut self, building: &mut Building, ty: TyId, place: Place) -> bool {
    if !building.seen.insert(ty) {
      return false;
    }
    let Some(Shape::Named(def)) = self.builder.ty_shape(ty) else {
      return true;
    };
    let nullable = self.nullable(ty, def);
    if let Some(known) = building.closure.find(nullable)
      && building
        .closure
        .node(known)
        .is_some_and(|node: Node| node.written)
    {
      return false;
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
          self.derived_verdict(def, nullable);
          building.queue.push(child);
        }
      }
    }
    true
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
  /// failing instantiation its expansion reaches first, of those `closure` derives whose definition
  /// does not grow, each judged when it was derived, a check a limit stopped among them.
  fn judge_derived(&mut self, closure: &Closure) {
    if self.recorded.is_empty() {
      return;
    }
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
        Checked::Stopped(_) => failing.push(at),
      }
    }
    if failing.is_empty() {
      return;
    }
    let blamed = closure.blame(&failing);
    // An occurrence whose first failure is a check a limit stopped, of a kind said where it is
    // already, reports its first confirmed failure instead.
    let confirmed = (confirmed.len() < failing.len()).then(|| closure.blame(&confirmed));
    let confirmed = confirmed.as_deref();
    for index in 0..self.builder.type_expr_count() {
      let Some(occurrence) = self.builder.occurrence(TypeExprId::new(index)) else {
        continue;
      };
      let (Written::Named(def), Some(ty)) = (occurrence.written, occurrence.ty) else {
        continue;
      };
      let at = occurrence.location;
      self.report_derived(closure, &blamed, confirmed, at, def, ty);
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
      self.report_derived(closure, &blamed, confirmed, at, def, ty);
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
  /// failing instantiation its expansion reaches first in `closure`, as `blamed` keeps it, when a
  /// supplied package writes it there, `ty` is a root, and its own verdict holds. A check a limit
  /// stopped is `resource-limit` there, but when one of its kind was said there already: then the
  /// occurrence reads `confirmed`, the blame of the confirmed failures alone, and reports its first
  /// confirmed failure instead, so that a stop said once at a place never hides a failure that is
  /// confirmed there.
  fn report_derived(
    &mut self,
    closure: &Closure,
    blamed: &[Option<Blame>],
    confirmed: Option<&[Option<Blame>]>,
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
    let mut chosen = blamed.get(root as usize).copied().flatten();
    if let Some(confirmed) = confirmed {
      let said = chosen
        .and_then(|blame: Blame| closure.node(blame.node))
        .and_then(|failed: Node| match self.checked.get(&failed.ty) {
          Some(&Checked::Stopped(stop)) => Some(stop),
          _ => None,
        })
        .is_some_and(|stop: Stop| self.types.limit_said(at, stop.spent(Spent::Values)));
      if said {
        chosen = confirmed.get(root as usize).copied().flatten();
      }
    }
    let Some(Blame { node, place }) = chosen else {
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
    self.limit(at, stop, message);
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
    self.limit(at, stop, message);
  }
}

/// The run of `sorted`, sorted by the name `name_of` reads, whose name is `name`.
fn same_name<T>(sorted: &[T], name: Sym, name_of: impl Fn(&T) -> Sym) -> &[T] {
  let from = sorted.partition_point(|item: &T| name_of(item) < name);
  let to = sorted.partition_point(|item: &T| name_of(item) <= name);
  sorted.get(from..to).unwrap_or(&[])
}

/// Whether one of the names of `run` of `names`, the arguments a field adds, sorted, is not the
/// name of an argument of `field`, an interface's field, as `arguments` lists them: surely when it
/// adds more than the field declares, and otherwise looked up, the fewer side read, each lookup
/// paid from `left`; and when `left` cannot pay, as read generously.
fn adds_beside(
  names: &[Sym],
  run: PoolRange,
  arguments: &[(MemberId, Sym)],
  field: MemberId,
  left: &mut u32,
) -> bool {
  if !spend(left, 1) {
    return true;
  }
  let added = names
    .get(run.start as usize..run.end as usize)
    .unwrap_or(&[]);
  let from = arguments.partition_point(|&(owner, _): &(MemberId, Sym)| owner < field);
  let to = arguments.partition_point(|&(owner, _): &(MemberId, Sym)| owner <= field);
  let declared = arguments.get(from..to).unwrap_or(&[]);
  if added.len() > declared.len() {
    return true;
  }
  for &name in added {
    if !spend(left, 1) {
      return true;
    }
    if declared
      .binary_search_by(|&(_, written): &(MemberId, Sym)| written.cmp(&name))
      .is_err()
    {
      return true;
    }
  }
  false
}

/// Pays `cost` from `left`, when it can.
fn spend(left: &mut u32, cost: u32) -> bool {
  match left.checked_sub(cost) {
    Some(rest) => {
      *left = rest;
      true
    }
    None => {
      *left = 0;
      false
    }
  }
}
