//! The tier phase: the last phase, which gives every definition and every type the tier it
//! requires, the least of `core`, `alloc` and `std` that holds everything it uses.
//!
//! - A definition of `core`, `alloc` or `std`, a meta-type and a GraphQL built-in the root package
//!   replaces among them, keeps the tag its package gave it, and is never computed again.
//! - A scalar a supplied package defines requires the greatest of its externs' tiers, and `core`
//!   with none: a custom scalar, and a collection that declares no host type, need no allocator.
//! - An enum and a directive a supplied package defines require `core`: a directive's arguments
//!   count nowhere.
//! - An object or an interface a supplied package defines requires the greatest of the tiers of the
//!   types of its fields and of their arguments, an input object of its input fields' types, and a
//!   union of its members' types; a position that applies `core`'s `@heap`, told by the definition
//!   the application resolved to, requires `alloc` at least.
//! - A named type requires the greatest of its definition's tier and its arguments'; a list
//!   requires `alloc` at least, and its element's; a type parameter and the unresolved type require
//!   `core`. So for a type that names a parameter it is a lower bound.
//!
//! Definitions and types that name one another in a cycle share the greatest tier any of them
//! reaches: the tiers are the least fixed point of those rules. It is found with one worklist over
//! the definitions and the types together, each a node, and an edge from each node to each node
//! whose tier is at least its own: from a definition to each of its named types, from a type to
//! each named type it is an argument of and each list it is the element of, and from a type to
//! each definition one of whose positions or members it is the type of. The edges are made in two
//! passes, a count and then a fill, into one vector of offsets and one of edges. Every node above
//! `core` is pushed; a popped node raises each node its edges reach that it is above and that is no
//! fixed definition, and pushes it. A node rises at most twice and is pushed at most three times,
//! so the work is linear in the nodes and the edges, whatever order the worklist pops in, and the
//! answer is the one least fixed point. Nothing recurses, and nothing is interned.
//!
//! It runs last: after binding, whose externs it reads, and after every phase that interns a type,
//! so that every type has its tier but the unresolved type the model's finishing may intern, whose
//! `core` is its tier. A phase that interns types goes before it. It reports nothing, and writes
//! only the tiers above `core`, onto rows already appended.

use std::{vec, vec::Vec};

use crate::graphqlx::{
  builtin::{CORE, DIRECTIVE},
  declare::Declarations,
  id::{ApplicationId, DefId, MemberId, TyId, TypeExprId, dense},
  instantiate::binding,
  model::{DefKind, ModelBuilder, Occurrence, Origin, Resolution, Shape},
  package::Tier,
};

/// Gives every definition and every type of `builder`, whose declarations are `declarations`, the
/// tier it requires.
pub(crate) fn tiers(declarations: &Declarations<'_, '_>, builder: &mut ModelBuilder) {
  let heap = binding(declarations, builder, CORE, DIRECTIVE, b"heap");
  let defs = builder.def_count() as usize;
  let nodes = defs.saturating_add(builder.ty_count() as usize);

  // Each node's tier so far, and whether it is a definition no edge raises.
  let mut tiers: Vec<Tier> = vec![Tier::Core; nodes];
  let mut fixed: Vec<bool> = vec![false; nodes];
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    let (tier, holds) = initial(builder, def, heap);
    if let Some(slot) = tiers.get_mut(index as usize) {
      *slot = tier;
    }
    if let Some(slot) = fixed.get_mut(index as usize) {
      *slot = !holds;
    }
  }
  for index in 0..builder.ty_count() {
    if matches!(builder.ty_shape(TyId::new(index)), Some(Shape::List(_)))
      && let Some(slot) = tiers.get_mut(defs.saturating_add(index as usize))
    {
      *slot = Tier::Alloc;
    }
  }

  // The edges, each from a node to one whose tier is at least its own, by their first node.
  let mut pairs: Vec<(u32, u32)> = Vec::new();
  edges(builder, defs, &mut pairs);
  let mut offsets: Vec<u32> = vec![0; nodes.saturating_add(1)];
  for &(from, _) in &pairs {
    if let Some(count) = offsets.get_mut((from as usize).saturating_add(1)) {
      *count = count.saturating_add(1);
    }
  }
  for at in 1..offsets.len() {
    let before = offsets.get(at - 1).copied().unwrap_or(0);
    if let Some(count) = offsets.get_mut(at) {
      *count = count.saturating_add(before);
    }
  }
  let mut next: Vec<u32> = offsets.clone();
  let mut targets: Vec<u32> = vec![0; pairs.len()];
  for &(from, to) in &pairs {
    if let Some(slot) = next.get_mut(from as usize) {
      if let Some(target) = targets.get_mut(*slot as usize) {
        *target = to;
      }
      *slot = slot.saturating_add(1);
    }
  }
  drop(pairs);
  drop(next);

  // The worklist: every node above `core`, then every node a pop raises.
  let mut work: Vec<u32> = Vec::new();
  for (node, tier) in tiers.iter().enumerate() {
    if *tier > Tier::Core {
      work.push(dense(node));
    }
  }
  while let Some(node) = work.pop() {
    let Some(&reached) = tiers.get(node as usize) else {
      continue;
    };
    let start = offsets.get(node as usize).copied().unwrap_or(0) as usize;
    let end = offsets
      .get((node as usize).saturating_add(1))
      .copied()
      .unwrap_or(0) as usize;
    for &target in targets.get(start..end).unwrap_or_default() {
      if fixed.get(target as usize).copied().unwrap_or(true) {
        continue;
      }
      if let Some(tier) = tiers.get_mut(target as usize)
        && *tier < reached
      {
        *tier = reached;
        work.push(target);
      }
    }
  }

  // What is above `core` is written: a supplied package's definition, and a type.
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    if let Some(&tier) = tiers.get(index as usize)
      && tier > Tier::Core
      && builder.def_origin(def) == Some(Origin::User)
    {
      builder.set_def_tier(def, tier);
    }
  }
  for index in 0..builder.ty_count() {
    if let Some(&tier) = tiers.get(defs.saturating_add(index as usize))
      && tier > Tier::Core
    {
      builder.set_ty_tier(TyId::new(index), tier);
    }
  }
}

/// The tier `def` starts at, and whether edges reach it: a built-in's tag; a supplied package's
/// scalar's externs' greatest tier, `core` with none, and `core` for its enum and its directive,
/// none of which edges reach; and for its object, interface, input object or union, `alloc` when
/// one of its positions applies `heap`, `core`'s `@heap`, and `core` otherwise, its types' edges
/// reaching it.
fn initial(builder: &ModelBuilder, def: DefId, heap: Option<DefId>) -> (Tier, bool) {
  if builder.def_origin(def) != Some(Origin::User) {
    return (builder.def_tier(def).unwrap_or(Tier::Core), false);
  }
  match builder.def_kind(def) {
    Some(DefKind::Scalar) => (builder.def_extern_tier(def).unwrap_or(Tier::Core), false),
    Some(DefKind::Object | DefKind::Interface | DefKind::InputObject | DefKind::Union) => {
      let mut on_heap = false;
      for member in builder.def_members(def) {
        on_heap |= applies(builder, member, heap);
        for argument in builder.member_arguments(member) {
          on_heap |= applies(builder, argument, heap);
        }
      }
      let tier = if on_heap { Tier::Alloc } else { Tier::Core };
      (tier, true)
    }
    _ => (Tier::Core, false),
  }
}

/// Whether `member` applies `heap`, `core`'s `@heap`.
fn applies(builder: &ModelBuilder, member: MemberId, heap: Option<DefId>) -> bool {
  let Some(heap) = heap else {
    return false;
  };
  builder
    .member_applications(member)
    .iter()
    .any(|application: &ApplicationId| {
      matches!(
        builder.applied(*application),
        Some((Resolution::Resolved(directive), _, _)) if directive == heap
      )
    })
}

/// Appends to `pairs` every edge, as its first node and its second: from a type's definition and
/// from each of its arguments to a named type, from its element to a list, and to an object, an
/// interface, an input object or a union a supplied package defines from the type of each of its
/// positions, a field's arguments among them, and of each of its union members. A definition `d`
/// is the node `d`, and a type `t` the node `defs + t`.
fn edges(builder: &ModelBuilder, defs: usize, pairs: &mut Vec<(u32, u32)>) {
  let node = |ty: TyId| -> u32 { dense(defs.saturating_add(ty.get() as usize)) };
  for index in 0..builder.ty_count() {
    let ty = TyId::new(index);
    match builder.ty_shape(ty) {
      Some(Shape::Named(def)) => {
        pairs.push((def.get(), node(ty)));
        for &argument in builder.ty_args(ty) {
          pairs.push((node(argument), node(ty)));
        }
      }
      Some(Shape::List(element)) => pairs.push((node(element), node(ty))),
      _ => {}
    }
  }
  // The type of the occurrence `written`, once a phase gave it one.
  let typed = |written: TypeExprId| -> Option<TyId> {
    builder
      .occurrence(written)
      .and_then(|occurrence: Occurrence<'_>| occurrence.ty)
  };
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    if builder.def_origin(def) != Some(Origin::User) {
      continue;
    }
    match builder.def_kind(def) {
      Some(DefKind::Object | DefKind::Interface | DefKind::InputObject) => {
        for member in builder.def_members(def) {
          if let Some(ty) = builder.member_ty(member).and_then(typed) {
            pairs.push((node(ty), index));
          }
          for argument in builder.member_arguments(member) {
            if let Some(ty) = builder.member_ty(argument).and_then(typed) {
              pairs.push((node(ty), index));
            }
          }
        }
      }
      Some(DefKind::Union) => {
        for &member in builder.def_union_members(def) {
          if let Some(ty) = typed(member) {
            pairs.push((node(ty), index));
          }
        }
      }
      _ => {}
    }
  }
}
