//! What a value can reach: which input objects a value of a type can hold, through which
//! arguments and fields, over every package together, and so whether it can hold a `TypeRef`.
//!
//! `core`'s `TypeRef` names a definition, and only a directive's arguments may hold one: an input
//! object one of whose fields is, under lists alone, a `TypeRef` may be reached from directives'
//! arguments and from nothing an operation writes. [`Reach`] answers, for a type, the field of type
//! `TypeRef` a value of it can hold, through lists, collections' arguments and input objects, each
//! input object's arguments that a value of it holds ([`Positions::holds`]), which its input
//! fields alone say: a directive applied to it, and that directive's arguments, are metadata,
//! which no value holds. A scalar that is no collection hides its arguments, which no value of it
//! holds. A `TypeRef` itself, or a list of it, answers nothing: where it stands is judged where it
//! is written.
//!
//! # The graph
//!
//! Its nodes are every input object of the model, every package's, and each interned type the
//! input objects' fields reach, each `TyId` once. An input object leads to the complete type of
//! each of its fields, every contribution's, in member order; a list to its element; a collection
//! to each argument; a named type of an input object to that input object, and to each argument a
//! value of it holds. Nothing leads out of any other type. The input objects one of whose fields
//! is, under lists alone, `TypeRef` are the seeds, each with the first such field in member order;
//! a breadth-first walk back along the edges from them taints every node that reaches one, with
//! the field of the node it was reached from. What is kept is each input object tainted, with its
//! field; the graph is dropped.
//!
//! # Why the whole program
//!
//! An input object of one package may be reached from a field argument of another: a dependency
//! may hold `input Binding { type: TypeRef! }` and take it in a directive's argument alone, which
//! is metadata, and the root package write `f(b: ^::geo::Binding)`, which an operation sets. A
//! pass over one package at a time would judge the dependency alone and find nothing; this graph
//! holds every input object of every package. The next pull request can reuse the numbering of
//! types and definitions, and the walk; its edges and its roots are its own design's to settle,
//! since this graph follows what a value can hold, and a tier counts a type argument a value
//! cannot hold.
//!
//! # What it costs
//!
//! [`Reach::of`] looks once at each input object's fields; enters each type in the closure of
//! their complete types once, reading its arguments once; and walks the reversed edges once. A
//! query is answered once for each type asked, with an explicit stack, each type under it once,
//! and kept. Every table is keyed by the model's ids, in ordered maps and sorted lists, with a
//! logarithm for each lookup. Nothing recurses.

use std::{collections::BTreeMap, vec::Vec};

use crate::graphqlx::{
  DefId, DefKind, MemberId, Model, TyId, TyKind, id::dense, positions::Positions,
};

/// Which input objects can hold a `TypeRef`, each with the field it can reach first, and the
/// answer for each type asked.
///
/// A rule keeps one for its walk and makes it in `begin`: what it holds is keyed by the model's
/// ids.
#[derive(Default)]
pub(crate) struct Reach {
  /// Each input object a value of which can hold a `TypeRef`, with the field of that type the walk
  /// found first.
  tainted: BTreeMap<DefId, MemberId>,
  /// Each type asked about, with its answer.
  asked: BTreeMap<TyId, Option<MemberId>>,
  /// The types still to answer, each with whether its children were pushed above it.
  stack: Vec<(TyId, bool)>,
}

impl Reach {
  /// What can reach a `TypeRef` in `model`, every package's input objects together, `positions`
  /// saying which arguments of an input object a value of it holds.
  pub(crate) fn of(model: &Model, positions: &Positions) -> Self {
    let type_ref = model.builtins().type_ref();
    let inputs: Vec<DefId> = model
      .defs()
      .into_iter()
      .filter(|def: &DefId| model.def(*def).kind() == DefKind::InputObject)
      .collect();
    if inputs.is_empty() {
      return Self::default();
    }
    // The nodes: the input objects first, then each type their fields reach.
    let mut types: BTreeMap<TyId, u32> = BTreeMap::new();
    let mut edges: Vec<(u32, u32)> = Vec::new();
    let mut seeds: Vec<(u32, MemberId)> = Vec::new();
    let mut stack: Vec<TyId> = Vec::new();
    let first = dense(inputs.len());
    let node_of = |types: &mut BTreeMap<TyId, u32>, stack: &mut Vec<TyId>, ty: TyId| -> u32 {
      if let Some(node) = types.get(&ty) {
        return *node;
      }
      let node = first.saturating_add(dense(types.len()));
      types.insert(ty, node);
      stack.push(ty);
      node
    };
    for (index, &input) in inputs.iter().enumerate() {
      let node = dense(index);
      let mut seeded = false;
      for field in model.def(input).input_fields() {
        let Some(occurrence) = model.member(field).ty() else {
          continue;
        };
        let ty = model.type_expr(occurrence).ty();
        if !seeded && holds_type_ref(model, ty, type_ref) {
          seeds.push((node, field));
          seeded = true;
        }
        let to = node_of(&mut types, &mut stack, ty);
        edges.push((node, to));
      }
    }
    while let Some(ty) = stack.pop() {
      let Some(&from) = types.get(&ty) else {
        continue;
      };
      match model.ty(ty).kind() {
        TyKind::List { element } => {
          let to = node_of(&mut types, &mut stack, element);
          edges.push((from, to));
        }
        TyKind::Named { def, args } => {
          let definition = model.def(def);
          if definition.collection().is_some() {
            for arg in args {
              let to = node_of(&mut types, &mut stack, *arg);
              edges.push((from, to));
            }
          } else if definition.kind() == DefKind::InputObject {
            if let Ok(index) = inputs.binary_search(&def) {
              edges.push((from, dense(index)));
            }
            for (index, arg) in args.iter().enumerate() {
              if positions.holds(def, dense(index)) {
                let to = node_of(&mut types, &mut stack, *arg);
                edges.push((from, to));
              }
            }
          }
        }
        TyKind::Param { .. } | TyKind::Unresolved => {}
      }
    }
    // The edges reversed, by their target, and the walk back from the seeds.
    let count = (first as usize).saturating_add(types.len());
    let mut reversed: Vec<(u32, u32)> = edges
      .iter()
      .map(|&(from, to): &(u32, u32)| (to, from))
      .collect();
    reversed.sort_unstable();
    let mut witness: Vec<Option<MemberId>> = Vec::with_capacity(count);
    witness.resize(count, None);
    let mut queue: Vec<u32> = Vec::new();
    let mut at = 0usize;
    for &(node, field) in &seeds {
      if let Some(slot) = witness.get_mut(node as usize) {
        *slot = Some(field);
        queue.push(node);
      }
    }
    while let Some(&node) = queue.get(at) {
      at = at.saturating_add(1);
      let Some(field) = witness.get(node as usize).copied().flatten() else {
        continue;
      };
      let from = reversed.partition_point(|&(to, _): &(u32, u32)| to < node);
      let to = reversed.partition_point(|&(to, _): &(u32, u32)| to <= node);
      for &(_, source) in reversed.get(from..to).unwrap_or(&[]) {
        if let Some(slot) = witness.get_mut(source as usize)
          && slot.is_none()
        {
          *slot = Some(field);
          queue.push(source);
        }
      }
    }
    let mut tainted: BTreeMap<DefId, MemberId> = BTreeMap::new();
    for (index, &input) in inputs.iter().enumerate() {
      if let Some(field) = witness.get(index).copied().flatten() {
        tainted.insert(input, field);
      }
    }
    Self {
      tainted,
      asked: BTreeMap::new(),
      stack: Vec::new(),
    }
  }

  /// The input field of type `TypeRef` a value of `ty`, of `model`, can hold, through lists,
  /// collections' arguments and input objects, each input object's arguments that a value of it
  /// holds by `positions`, the first one the walk found; `None` when it can hold none. A `TypeRef`
  /// itself, and a list of it, answer `None`.
  pub(crate) fn type_ref(
    &mut self,
    model: &Model,
    positions: &Positions,
    ty: TyId,
  ) -> Option<MemberId> {
    if let Some(answer) = self.asked.get(&ty) {
      return *answer;
    }
    self.stack.clear();
    self.stack.push((ty, false));
    while let Some((next, pushed)) = self.stack.pop() {
      if self.asked.contains_key(&next) {
        continue;
      }
      let answer = match model.ty(next).kind() {
        TyKind::List { element } => {
          if !pushed {
            self.stack.push((next, true));
            self.stack.push((element, false));
            continue;
          }
          self.asked.get(&element).copied().flatten()
        }
        TyKind::Named { def, args } => {
          let definition = model.def(def);
          let input = definition.kind() == DefKind::InputObject;
          if input && let Some(field) = self.tainted.get(&def) {
            Some(*field)
          } else if definition.collection().is_some() || input {
            // What a value of it holds: each argument of a collection, and each argument of an
            // input object that a value of it holds.
            let held = |index: usize| -> bool { !input || positions.holds(def, dense(index)) };
            if !pushed {
              self.stack.push((next, true));
              for (index, arg) in args.iter().enumerate().rev() {
                if held(index) {
                  self.stack.push((*arg, false));
                }
              }
              continue;
            }
            args
              .iter()
              .enumerate()
              .filter(|&(index, _): &(usize, &TyId)| held(index))
              .find_map(|(_, arg): (usize, &TyId)| self.asked.get(arg).copied().flatten())
          } else {
            None
          }
        }
        TyKind::Param { .. } | TyKind::Unresolved => None,
      };
      self.asked.insert(next, answer);
    }
    self.asked.get(&ty).copied().flatten()
  }
}

/// Whether `ty`, of `model`, is, under lists alone, `type_ref`.
fn holds_type_ref(model: &Model, ty: TyId, type_ref: DefId) -> bool {
  let mut ty = ty;
  loop {
    match model.ty(ty).kind() {
      TyKind::List { element } => ty = element,
      TyKind::Named { def, .. } => return def == type_ref,
      TyKind::Param { .. } | TyKind::Unresolved => return false,
    }
  }
}
