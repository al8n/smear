//! Which generic definitions grow: the one graph that says it, which the analysis builds once after
//! instantiation to answer `Def::grows`, and which `UnboundedInstantiation` builds in its `begin`
//! to place its reports.
//!
//! The graph is built over the model's public views, as a rule reads them, so the same routine
//! serves the finished model and the model as instantiation left it: what it reads, every
//! definition's type parameters, members, `implements` entries, union members and applications, and
//! every occurrence's and application's type, is written by then, and no later phase changes it.
//!
//! Its nodes are every type parameter of every definition, numbered by definition, and every open
//! type, other than a bare parameter, that a followed position writes, that an application written
//! there has, or that such a type holds: each written occurrence's own type is entered, since a
//! collection's key or element is non-null in its holder's type and so is not the type of the
//! occurrence that writes it. Its edges:
//!
//! - for a named type and each of its open arguments, an edge from the argument into the named
//!   definition's parameter at that position, which grows unless the argument is a bare parameter;
//! - for each open argument or element of an open type, an edge from it into the type, which does
//!   not grow;
//! - for a directive application, the edges of its type, its directive's instantiation with every
//!   type argument, written or filled, which is entered as a named type is. A count of type
//!   arguments that was refused gives the application no type, and no edge.
//!
//! A definition grows when one of its type parameters lies in a strongly connected component that
//! holds a growing edge, both of whose ends lie in it: the parameter then reaches itself through
//! that edge, with a type argument that wraps what it was given. Such a growing edge also marks the
//! type that holds it, which keeps the least index of such an argument. One ascending pass over the
//! types then says which hold a marked one, since a type's arguments are interned before it, and
//! each keeps the default that fills the argument it is reported for: what the rule reports by.
//!
//! The followed positions are what a copy of an instantiation needs: the types of a definition's
//! fields, of their arguments, of its input fields and of a directive definition's arguments; its
//! `implements` entries; its union members; and the directive applications written on it and on
//! its members, each followed by its type arguments as written and by its complete type arguments,
//! defaults filled. Not a bound, a type parameter's default as it is written, a root or a value: a
//! default is followed where it fills, since an occurrence's type, and an application's, holds what
//! it filled. Nor an interface's instantiation into the types that implement it.
//!
//! A contribution the orphan rule refuses, which another package writes into a definition, grows
//! nothing: each followed position is read from the contributions the rule accepts, as
//! `rules::accepted` answers, the one test `coerce`'s closure of derived instantiations reads too.
//! So what the rule refuses changes no definition's growth, nor what the closure derives.
//!
//! It costs time linear in the model's definitions, members, applications and types, and has no
//! limit of its own. When its graph has more nodes than a `u32` numbers, which only a model of tens
//! of gigabytes reaches, it is not built, and nothing is found to grow.

use core::num::TryFromIntError;
use std::{vec, vec::Vec};

use crate::{
  graphqlx::{
    ApplicationId, DefId, MemberId, Model, NamedTy, NamedTypeExpr, SourceMap, Ty, TyId, TyKind,
    TypeExprId, TypeExprKind, TypeParam, TypeParamRef,
    rules::{Accepted, accepted, accepts_application},
  },
  kernel::scc,
};

/// What the graph over one model found.
pub(crate) struct Grown {
  /// Each definition that grows, in id order.
  pub(crate) defs: Vec<DefId>,
  /// What it found of each type, by id, up to the last it entered, when the marks were asked for:
  /// empty otherwise.
  pub(crate) types: Vec<Found>,
}

/// The graph over `model`, whose files `sources` holds, and what it finds there: the definitions
/// that grow, and, when `marks`, which types are marked and which hold a marked one. `Err` when the
/// graph cannot be numbered in a `u32`.
pub(crate) fn grown(model: &Model, sources: &SourceMap, marks: bool) -> Result<Grown, Overflow> {
  let mut build = Build::new(model, sources)?;
  build.roots()?;
  if build.growing.is_empty() {
    // No edge grows, so no definition does, and nothing is marked.
    return Ok(Grown {
      defs: Vec::new(),
      types: if marks { build.found } else { Vec::new() },
    });
  }
  let component = build.components()?;
  let defs = build.growing_defs(&component);
  if !marks {
    return Ok(Grown {
      defs,
      types: Vec::new(),
    });
  }
  build.mark(&component);
  build.spread();
  Ok(Grown {
    defs,
    types: build.found,
  })
}

/// Whether the type `ty` holds a marked type, as `found` records each type by id.
pub(crate) fn holds(found: &[Found], ty: TyId) -> bool {
  found
    .get(ty.get() as usize)
    .is_some_and(|record: &Found| record.holds)
}

/// What the graph found of one type.
#[derive(Clone, Copy, Default)]
pub(crate) struct Found {
  /// Whether it holds a marked type: it is one, or an argument or its element holds one.
  pub(crate) holds: bool,
  /// Whether it is marked: a named type with an argument whose growing edge lies on a cycle.
  pub(crate) marked: bool,
  /// The argument it is reported for: for a marked type, the least index of such an argument; for
  /// one that holds and is not marked, the least index of an argument that holds.
  pub(crate) index: u32,
  /// The default of its definition's type parameter at `index`, which fills that argument for an
  /// occurrence or an application that leaves it out.
  pub(crate) default: Option<TypeExprId>,
}

/// The graph cannot be numbered in a `u32`.
pub(crate) struct Overflow;

/// `value` as a node number, or the overflow that leaves the graph unbuilt.
fn number(value: usize) -> Result<u32, Overflow> {
  u32::try_from(value).map_err(|_: TryFromIntError| Overflow)
}

/// Counts one more edge out of the node `from`, in the slot past `from`'s own.
fn count(starts: &mut [usize], from: u32) {
  let Some(at) = (from as usize).checked_add(1) else {
    return;
  };
  if let Some(slot) = starts.get_mut(at) {
    *slot = slot.saturating_add(1);
  }
}

/// Writes the edge from `from` into `to` in the next free place of `from`'s row.
fn place(cursors: &mut [usize], targets: &mut [u32], from: u32, to: u32) {
  let Some(cursor) = cursors.get_mut(from as usize) else {
    return;
  };
  if let Some(target) = targets.get_mut(*cursor) {
    *target = to;
  }
  *cursor = cursor.saturating_add(1);
}

/// One type as the build enters it.
#[derive(Clone, Copy, Default)]
struct Entry {
  /// Whether the build entered it.
  entered: bool,
  /// Its node, given the first time it was named; only an open type that is no bare parameter
  /// has one.
  node: Option<u32>,
}

/// An edge from an open type that is no bare parameter into a type parameter.
#[derive(Clone, Copy)]
struct Growing {
  /// The type's node.
  from: u32,
  /// The type parameter's node.
  to: u32,
  /// The named type one of whose arguments it leaves: the type it marks when it lies on a cycle.
  holder: TyId,
  /// The position of the argument it leaves.
  index: u32,
}

/// The node an edge from one type leaves, by what the type is.
#[derive(Clone, Copy)]
enum Tail {
  /// A bare parameter, with or without `!`: its parameter's node. Its edge into a type parameter
  /// does not grow.
  Param(u32),
  /// An open type that is no bare parameter: its own node. Its edge into a type parameter grows.
  Type(u32),
}

/// An edge that does not grow: from its first node into its second.
#[derive(Clone, Copy)]
struct Edge {
  /// The node it leaves.
  from: u32,
  /// The node it enters.
  to: u32,
}

/// The graph built over one model, and what it finds there.
struct Build<'a> {
  /// The model it is built over.
  model: &'a Model,
  /// The model's files: which package wrote each contribution, which the orphan rule judges.
  sources: &'a SourceMap,
  /// The node of each definition's first type parameter, by definition, then one past the last.
  prefix: Vec<u32>,
  /// The node the next type to be named is given, which is also the count of nodes so far.
  next: u32,
  /// Each type as the build entered it, by id.
  entries: Vec<Entry>,
  /// What the build found of each type, by id.
  found: Vec<Found>,
  /// The edges that do not grow, each from its first node into its second.
  plain: Vec<Edge>,
  /// The edges that grow.
  growing: Vec<Growing>,
  /// The occurrences still to walk.
  occurrences: Vec<TypeExprId>,
  /// The types still to enter.
  pending: Vec<TyId>,
}

impl<'a> Build<'a> {
  /// The build over `model`, its type parameters numbered: one pass over the definitions. The
  /// kernel numbers its discoveries from one in a `u32`, so the count of nodes stays below
  /// `u32::MAX`.
  fn new(model: &'a Model, sources: &'a SourceMap) -> Result<Self, Overflow> {
    let defs = model.defs();
    let mut prefix: Vec<u32> = Vec::with_capacity((defs.len() as usize).saturating_add(1));
    let mut next: u32 = 0;
    for id in defs {
      prefix.push(next);
      let params = number(model.def(id).type_params().len())?;
      next = next
        .checked_add(params)
        .filter(|nodes: &u32| *nodes < u32::MAX)
        .ok_or(Overflow)?;
    }
    prefix.push(next);
    Ok(Self {
      model,
      sources,
      prefix,
      next,
      entries: Vec::new(),
      found: Vec::new(),
      plain: Vec::new(),
      growing: Vec::new(),
      occurrences: Vec::new(),
      pending: Vec::new(),
    })
  }

  /// The node of `def`'s type parameter `index`, or `None` when `def` has none at `index`. Every
  /// type parameter's node the graph names is this function's.
  fn param(&self, def: DefId, index: u32) -> Option<u32> {
    let at = def.get() as usize;
    let start = *self.prefix.get(at)?;
    let end = *self.prefix.get(at.checked_add(1)?)?;
    start.checked_add(index).filter(|node: &u32| *node < end)
  }

  /// The place of `ty` in the records by id, which grow to hold it.
  fn grow(&mut self, ty: TyId) -> Result<usize, Overflow> {
    let at = ty.get() as usize;
    if at >= self.entries.len() {
      let len = at.checked_add(1).ok_or(Overflow)?;
      self.entries.resize(len, Entry::default());
      self.found.resize(len, Found::default());
    }
    Ok(at)
  }

  /// The node of `ty`, an open type that is no bare parameter: the one it was given when it was
  /// first named, or the next, the count of nodes staying below `u32::MAX`.
  fn node(&mut self, ty: TyId) -> Result<u32, Overflow> {
    let at = self.grow(ty)?;
    let next = self.next;
    let Some(entry) = self.entries.get_mut(at) else {
      return Err(Overflow);
    };
    if let Some(node) = entry.node {
      return Ok(node);
    }
    let after = next
      .checked_add(1)
      .filter(|nodes: &u32| *nodes < u32::MAX)
      .ok_or(Overflow)?;
    entry.node = Some(next);
    self.next = after;
    Ok(next)
  }

  /// The node an edge from `ty` leaves, or `None` for a closed type, which gives no edge.
  fn tail(&mut self, ty: TyId) -> Result<Option<Tail>, Overflow> {
    let model: &'a Model = self.model;
    let view: Ty<'a> = model.ty(ty);
    if view.is_closed() {
      return Ok(None);
    }
    match view.kind() {
      TyKind::Param(TypeParamRef { def, index }) => Ok(self.param(def, index).map(Tail::Param)),
      TyKind::Named(_) | TyKind::List(_) => self.node(ty).map(|node: u32| Some(Tail::Type(node))),
      TyKind::Unresolved => Ok(None),
    }
  }

  /// The edge from what `tail` leaves into the type whose node is `holder`, which holds it as an
  /// argument or as its element. It does not grow.
  fn contain(&mut self, tail: Tail, holder: u32) {
    let (Tail::Param(from) | Tail::Type(from)) = tail;
    self.plain.push(Edge { from, to: holder });
  }

  /// The edge from what `tail` leaves into the type parameter whose node is `to`, held by `holder`
  /// at `index`. It grows when `tail` is an open type that is no bare parameter.
  fn pass(&mut self, tail: Tail, to: u32, holder: TyId, index: u32) {
    match tail {
      Tail::Param(from) => self.plain.push(Edge { from, to }),
      Tail::Type(from) => self.growing.push(Growing {
        from,
        to,
        holder,
        index,
      }),
    }
  }

  /// Enters `ty` and every type it holds, each once, with an explicit stack: an open type that is
  /// no bare parameter is given its node and the edges of its arguments or its element. A closed
  /// type and a bare parameter are left at once.
  fn enter(&mut self, ty: TyId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    self.pending.push(ty);
    while let Some(ty) = self.pending.pop() {
      let at = self.grow(ty)?;
      let Some(entry) = self.entries.get_mut(at) else {
        continue;
      };
      if entry.entered {
        continue;
      }
      entry.entered = true;
      let view: Ty<'a> = model.ty(ty);
      if view.is_closed() {
        continue;
      }
      match view.kind() {
        TyKind::List(element) => {
          let holder = self.node(ty)?;
          if let Some(tail) = self.tail(element)? {
            self.contain(tail, holder);
          }
          self.pending.push(element);
        }
        TyKind::Named(NamedTy { def, args }) => {
          let holder = self.node(ty)?;
          for (position, &argument) in args.iter().enumerate() {
            let index = number(position)?;
            let Some(tail) = self.tail(argument)? else {
              continue;
            };
            self.contain(tail, holder);
            if let Some(to) = self.param(def, index) {
              self.pass(tail, to, ty, index);
            }
            self.pending.push(argument);
          }
        }
        TyKind::Param(_) | TyKind::Unresolved => {}
      }
    }
    Ok(())
  }

  /// Walks `root` and every occurrence nested in it, with an explicit stack, through what each
  /// occurrence's kind answers, and enters each occurrence's own type. What is written on a type
  /// parameter or on a path that did not resolve is refused where it is written, and not walked.
  fn walk(&mut self, root: TypeExprId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    self.occurrences.push(root);
    while let Some(id) = self.occurrences.pop() {
      let occurrence = model.type_expr(id);
      self.enter(occurrence.ty())?;
      match occurrence.kind() {
        TypeExprKind::Named(NamedTypeExpr { args, .. }) => self.occurrences.extend_from_slice(args),
        TypeExprKind::List(element) => self.occurrences.push(element),
        TypeExprKind::Param(_) | TypeExprKind::Unresolved => {}
      }
    }
    Ok(())
  }

  /// The followed positions of every definition that has a type parameter: each field's type and
  /// its arguments' types, each input field's and each directive argument's type, each
  /// `implements` entry and union member, and the type arguments and the type of each application
  /// written on the definition and on each of its members, an enum's values among them. Each is
  /// read from the contributions the orphan rule accepts, as `rules::accepted` answers, alone: what
  /// a contribution it refuses writes grows nothing.
  fn roots(&mut self) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    let sources: &'a SourceMap = self.sources;
    for id in model.defs() {
      if self.param(id, 0).is_none() {
        continue;
      }
      for part in model.def(id).contributions() {
        let judged = accepted(model, sources, id, &part);
        if judged == Accepted::Refused {
          continue;
        }
        for &item in part.implements().iter().chain(part.union_members()) {
          self.walk(item)?;
        }
        for &application in part.applications() {
          if accepts_application(model, judged, application) {
            self.applied(application)?;
          }
        }
        for member in part.members() {
          self.member(member)?;
          for argument in model.member(member).arguments() {
            self.member(argument)?;
          }
        }
      }
    }
    Ok(())
  }

  /// A member's type, and the type arguments and the type of each of its applications: what the
  /// contribution that wrote the member wrote with it, which `roots` read as accepted.
  fn member(&mut self, id: MemberId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    let member = model.member(id);
    if let Some(ty) = member.ty() {
      self.walk(ty)?;
    }
    for application in member.applications() {
      self.applied(*application)?;
    }
    Ok(())
  }

  /// An application's type arguments as written, each walked whatever their count; then its type,
  /// its directive's instantiation with every type argument, written or filled, entered as a named
  /// type is. An application whose count of type arguments was refused has no type. Each is one an
  /// accepted contribution wrote, as `roots` and `member` find them.
  fn applied(&mut self, id: ApplicationId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    let application = model.application(id);
    for argument in application.type_args() {
      self.walk(*argument)?;
    }
    if let Some(instantiation) = application.ty() {
      self.enter(instantiation)?;
    }
    Ok(())
  }

  /// The strongly connected component of each node, named by its least member, over the graph's
  /// own compressed rows: each node's edges counted, the counts summed into where each row starts,
  /// then the rows filled.
  fn components(&self) -> Result<Vec<u32>, Overflow> {
    let nodes = self.next as usize;
    // Node `n`'s row starts at `starts[n]` and ends at `starts[n + 1]`.
    let mut starts: Vec<usize> = vec![0; nodes.checked_add(1).ok_or(Overflow)?];
    for &Edge { from, .. } in &self.plain {
      count(&mut starts, from);
    }
    for edge in &self.growing {
      count(&mut starts, edge.from);
    }
    let mut total: usize = 0;
    for start in &mut starts {
      total = total.saturating_add(*start);
      *start = total;
    }
    let mut targets: Vec<u32> = vec![0; total];
    let mut cursors: Vec<usize> = starts.clone();
    for &Edge { from, to } in &self.plain {
      place(&mut cursors, &mut targets, from, to);
    }
    for edge in &self.growing {
      place(&mut cursors, &mut targets, edge.from, edge.to);
    }
    Ok(scc::components(
      nodes,
      |node: usize, successors: &mut Vec<u32>| {
        let start = starts.get(node).copied().unwrap_or(0);
        let end = node
          .checked_add(1)
          .and_then(|after: usize| starts.get(after))
          .copied()
          .unwrap_or(start);
        successors.extend_from_slice(targets.get(start..end).unwrap_or_default());
      },
    ))
  }

  /// The definitions one of whose type parameters lies in a component that holds a growing edge
  /// both of whose ends lie in it, in id order: one pass over the growing edges, then one over the
  /// definitions' parameters. A growing edge leaves a type and enters a parameter, two nodes, so
  /// its ends share a component only on a cycle of more than one node.
  fn growing_defs(&self, component: &[u32]) -> Vec<DefId> {
    // Whether each component holds such an edge, by the least node that names it.
    let mut cyclic: Vec<bool> = vec![false; component.len()];
    for edge in &self.growing {
      let (Some(&from), Some(&to)) = (
        component.get(edge.from as usize),
        component.get(edge.to as usize),
      ) else {
        continue;
      };
      if from != to {
        continue;
      }
      if let Some(slot) = cyclic.get_mut(to as usize) {
        *slot = true;
      }
    }
    let in_cycle = |node: u32| -> bool {
      component
        .get(node as usize)
        .and_then(|named: &u32| cyclic.get(*named as usize))
        .copied()
        .unwrap_or(false)
    };
    let mut defs: Vec<DefId> = Vec::new();
    for id in self.model.defs() {
      let at = id.get() as usize;
      let (Some(&start), Some(&end)) = (
        self.prefix.get(at),
        at.checked_add(1)
          .and_then(|after: usize| self.prefix.get(after)),
      ) else {
        continue;
      };
      if (start..end).any(in_cycle) {
        defs.push(id);
      }
    }
    defs
  }

  /// Marks the holder of each growing edge whose two ends share a component, each type keeping the
  /// least index of such an argument.
  fn mark(&mut self, component: &[u32]) {
    for edge in &self.growing {
      let (Some(from), Some(to)) = (
        component.get(edge.from as usize),
        component.get(edge.to as usize),
      ) else {
        continue;
      };
      if from != to {
        continue;
      }
      if let Some(found) = self.found.get_mut(edge.holder.get() as usize) {
        found.index = if found.marked {
          found.index.min(edge.index)
        } else {
          edge.index
        };
        found.marked = true;
      }
    }
  }

  /// Says which types hold a marked one, in one ascending pass over the types by id: a type's
  /// arguments and element are interned before it, so each is decided first. Each type that
  /// holds keeps the argument it is reported for and, for a named type, the default of its
  /// definition's type parameter there: the one search of a parameter list, once a type.
  fn spread(&mut self) {
    let model: &'a Model = self.model;
    for at in 0..self.found.len() {
      // Only an open type that is no bare parameter has a node, and may hold.
      if self
        .entries
        .get(at)
        .and_then(|entry: &Entry| entry.node)
        .is_none()
      {
        continue;
      }
      let Ok(id) = u32::try_from(at) else {
        break;
      };
      let ty: Ty<'a> = model.ty(TyId::new(id));
      let Some(&found) = self.found.get(at) else {
        continue;
      };
      let index = if found.marked {
        Some(found.index)
      } else {
        self.held(ty)
      };
      let Some(index) = index else {
        continue;
      };
      let default = match ty.kind() {
        TyKind::Named(NamedTy { def, .. }) => model
          .def(def)
          .type_params()
          .nth(index as usize)
          .and_then(|param: TypeParam<'a>| param.default()),
        TyKind::Param(_) | TyKind::List(_) | TyKind::Unresolved => None,
      };
      if let Some(record) = self.found.get_mut(at) {
        record.holds = true;
        record.index = index;
        record.default = default;
      }
    }
  }

  /// For `ty`, which is not marked, the least index of an argument that holds a marked type, or
  /// `0` for a list whose element holds one; `None` when nothing it holds does.
  fn held(&self, ty: Ty<'_>) -> Option<u32> {
    match ty.kind() {
      TyKind::Named(NamedTy { args, .. }) => args
        .iter()
        .position(|argument: &TyId| holds(&self.found, *argument))
        .and_then(|position: usize| u32::try_from(position).ok()),
      TyKind::List(element) => holds(&self.found, element).then_some(0),
      TyKind::Param(_) | TyKind::Unresolved => None,
    }
  }
}
