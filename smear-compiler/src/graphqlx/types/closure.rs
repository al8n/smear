//! The closure of derived instantiations, design Z's D11: what each written instantiation derives,
//! through the definitions that do not grow, so that each phase judges what it judges at a written
//! instantiation at a derived one too, and reports it at the written occurrence that derives it.
//!
//! Coercion builds it, once its walk has ended, and the implementation check reads it. Its nodes
//! are instantiations, each by its nullable form, so that a type's two nullnesses are one node:
//!
//! - the roots, each closed instantiation of a generic definition that a supplied package writes,
//!   a type occurrence's type or an application's complete type;
//! - and each closed instantiation of a generic definition a node's expansion derives that no
//!   supplied package writes, once.
//!
//! A node's expansion reads what a copy of it needs under its arguments: its definition's members'
//! types, `implements` entries and union members, and the complete types of the applications on
//! the definition and on its members, each a rebuild; and, for a root, the arguments its type
//! parameters' defaults fill. Its children are the instantiations those types hold that no supplied
//! package writes, each once, with the place that derives it. A root is expanded; a derived node
//! only when its definition does not grow. A written instantiation an expansion meets is a root of
//! its own: it is judged where it is written, and what it derives is its own, so the expansion
//! goes no further through it.
//!
//! Which nodes fail is each phase's to say. [`Closure::blame`] then gives each node the failing
//! node of least index that its expansion reaches without passing through a root, with the place
//! that derives it: so a root is blamed for the first failing instantiation the closure met among
//! those it derives, and a written occurrence of it reports that one.

use std::{collections::BTreeMap, vec, vec::Vec};

use super::Stop;
use crate::graphqlx::id::{ApplicationId, DefId, PoolRange, TyId, TypeExprId, dense};

/// The label at the type or the application that derives the instantiation a report names, in
/// coercion's reports and in the implementation check's.
pub(crate) const DERIVES: &str = "this derives that instantiation";

/// Where an instantiation of the closure is derived: a type its holder's definition writes, read
/// under the holder's arguments.
#[derive(Clone, Copy)]
pub(crate) enum Place {
  /// A type occurrence: a member's type, an `implements` entry or a union member; or the default
  /// of a type parameter, which fills a root's argument.
  Type(TypeExprId),
  /// An application on the holder's definition or on one of its members, by its complete type.
  Application(ApplicationId),
}

/// One instantiation of the closure.
#[derive(Clone, Copy)]
pub(crate) struct Node {
  /// Its nullable form.
  pub(crate) ty: TyId,
  /// Its definition.
  pub(crate) def: DefId,
  /// Whether a supplied package writes it: a root.
  pub(crate) written: bool,
  /// Whether its definition grows: derived, it is neither judged nor expanded.
  pub(crate) grows: bool,
  /// Its children, a run of the closure's edges: what its expansion derived, each once.
  children: PoolRange,
}

/// A child of a node: the node it derived, and the place that derives it.
#[derive(Clone, Copy)]
struct Edge {
  child: u32,
  place: Place,
}

/// A rebuild that stopped the closure: the instantiation being expanded, by its nullable form, the
/// written type rebuilt under its arguments, and what stopped it.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct Halt {
  /// The instantiation being expanded.
  pub(crate) instantiation: TyId,
  /// The type of the position whose rebuild stopped.
  pub(crate) written: TyId,
  /// What stopped it.
  pub(crate) stop: Stop,
}

/// What a node's expansion reaches that fails: the failing node, and the place that derives it on
/// the way there, `None` for the failing node itself.
#[derive(Clone, Copy)]
pub(crate) struct Blame {
  /// The failing node.
  pub(crate) node: u32,
  /// The place that derives it, from the node that holds it on the way.
  pub(crate) place: Option<Place>,
}

/// The closure: its nodes in the order the expansion met them, roots first, and their children.
#[derive(Default)]
pub(crate) struct Closure {
  nodes: Vec<Node>,
  edges: Vec<Edge>,
  /// Each node, by its nullable form.
  index: BTreeMap<TyId, u32>,
}

impl Closure {
  /// How many nodes it holds.
  pub(crate) fn len(&self) -> u32 {
    dense(self.nodes.len())
  }

  /// Whether it holds no node: no supplied package writes a closed instantiation.
  pub(crate) fn is_empty(&self) -> bool {
    self.nodes.is_empty()
  }

  /// The node at `at`.
  pub(crate) fn node(&self, at: u32) -> Option<Node> {
    self.nodes.get(at as usize).copied()
  }

  /// The node whose nullable form is `ty`.
  pub(crate) fn find(&self, ty: TyId) -> Option<u32> {
    self.index.get(&ty).copied()
  }

  /// The node whose nullable form is `ty`, of `def`, joined at the end when it is new: written when
  /// `written`, and growing when `grows`. Answers it, and whether it is new.
  pub(crate) fn join(&mut self, ty: TyId, def: DefId, written: bool, grows: bool) -> (u32, bool) {
    if let Some(&at) = self.index.get(&ty) {
      return (at, false);
    }
    let at = dense(self.nodes.len());
    self.nodes.push(Node {
      ty,
      def,
      written,
      grows,
      children: PoolRange { start: 0, end: 0 },
    });
    self.index.insert(ty, at);
    (at, true)
  }

  /// Where the next node's children begin: the end of the edges so far.
  pub(crate) fn next_edge(&self) -> u32 {
    dense(self.edges.len())
  }

  /// Gives the node being expanded one more child, `child`, derived at `place`.
  pub(crate) fn derive(&mut self, child: u32, place: Place) {
    self.edges.push(Edge { child, place });
  }

  /// Ends the expansion of the node at `at`, whose children are the edges since `from`.
  pub(crate) fn expanded(&mut self, at: u32, from: u32) {
    let end = dense(self.edges.len());
    if let Some(node) = self.nodes.get_mut(at as usize) {
      node.children = PoolRange { start: from, end };
    }
  }

  /// For each node, the failing node of least index its expansion reaches, `failing` being every
  /// failing node in ascending order, none of them a root: the failing node itself, and each node
  /// that derives it, directly or through derived nodes, but never through a root, which owns what
  /// it derives. Each node keeps the place that derives its failing node, on the way it was first
  /// reached.
  ///
  /// The failing nodes are walked in ascending order, each back through the nodes that derive it,
  /// breadth first, and a node is given its answer the first time it is reached and never again:
  /// what a node reaches is reached by every node that derives it, so a node already answered is
  /// one whose own derivers are answered no worse. Each node and each edge is read once: linear in
  /// the closure, and charged nothing, its edges having been paid for when they were derived.
  pub(crate) fn blame(&self, failing: &[u32]) -> Vec<Option<Blame>> {
    self.blame_where(failing, |_: u32, _: u32, _: Place| true)
  }

  /// As [`blame`](Self::blame), but an edge into a failing node passes its failure on to the node
  /// that derives it only when `carries` says so, given the failing node, that node and the place:
  /// a failure a phase knows to be reported already where it is derived goes no further that way.
  /// What `carries` costs is the caller's.
  pub(crate) fn blame_where(
    &self,
    failing: &[u32],
    mut carries: impl FnMut(u32, u32, Place) -> bool,
  ) -> Vec<Option<Blame>> {
    let mut blamed: Vec<Option<Blame>> = vec![None; self.nodes.len()];
    if failing.is_empty() {
      return blamed;
    }
    // Every edge backwards, by the child it derives: each child's derivers are one run.
    let mut parents: Vec<(u32, u32, Place)> = Vec::with_capacity(self.edges.len());
    for (at, node) in self.nodes.iter().enumerate() {
      let start = node.children.start as usize;
      let end = node.children.end as usize;
      for edge in self.edges.get(start..end).unwrap_or_default() {
        parents.push((edge.child, dense(at), edge.place));
      }
    }
    parents.sort_by_key(|&(child, parent, _): &(u32, u32, Place)| (child, parent));
    let mut queue: Vec<u32> = Vec::new();
    for &failed in failing {
      if blamed
        .get(failed as usize)
        .is_none_or(|known: &Option<Blame>| known.is_some())
      {
        continue;
      }
      if let Some(slot) = blamed.get_mut(failed as usize) {
        *slot = Some(Blame {
          node: failed,
          place: None,
        });
      }
      queue.clear();
      queue.push(failed);
      let mut next: usize = 0;
      while let Some(&below) = queue.get(next) {
        next += 1;
        let via = blamed
          .get(below as usize)
          .copied()
          .flatten()
          .and_then(|blame: Blame| blame.place);
        let from = parents.partition_point(|&(child, ..): &(u32, u32, Place)| child < below);
        for &(child, parent, place) in parents.get(from..).unwrap_or_default() {
          if child != below {
            break;
          }
          if below == failed && !carries(failed, parent, place) {
            continue;
          }
          let Some(slot) = blamed.get_mut(parent as usize) else {
            continue;
          };
          if slot.is_some() {
            continue;
          }
          *slot = Some(Blame {
            node: failed,
            place: Some(via.unwrap_or(place)),
          });
          // A root owns what it derives: it is blamed, and what derives it is not, through it.
          if self
            .nodes
            .get(parent as usize)
            .is_some_and(|node: &Node| !node.written)
          {
            queue.push(parent);
          }
        }
      }
    }
    blamed
  }
}
