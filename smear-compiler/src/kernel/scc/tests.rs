//! Which nodes lie on a cycle, and which component each node is in, over graphs small enough to
//! read whole, and over one chain long enough that a walk holding a frame per node would hold five
//! thousand of them.

use super::*;

/// [`on_cycle`] over a graph written as one row of successors per node.
fn cycles(graph: &[&[u32]], marked: usize) -> Vec<bool> {
  on_cycle(graph.len(), marked, |node, reaches| {
    reaches.extend_from_slice(graph[node]);
  })
}

/// No nodes, or no node marked, is an empty answer.
#[test]
fn nothing_marked_is_an_empty_answer() {
  assert!(
    on_cycle(0, 0, |_, _| {}).is_empty(),
    "a graph with no nodes"
  );
  assert!(
    cycles(&[&[1], &[0]], 0).is_empty(),
    "a cycle with no marked member"
  );
}

/// An edge from a node to itself puts the node on a cycle, and two nodes that reach each other are
/// both on one.
#[test]
fn a_self_edge_and_a_pair_that_reach_each_other_are_on_cycles() {
  assert_eq!(cycles(&[&[0], &[]], 2), [true, false]);
  assert_eq!(cycles(&[&[1], &[0], &[]], 3), [true, true, false]);
}

/// A node that reaches a cycle without being reached back is not on it.
#[test]
fn reaching_a_cycle_is_not_being_on_one() {
  assert_eq!(cycles(&[&[1], &[1]], 2), [false, true]);
  assert_eq!(cycles(&[&[1], &[2], &[1]], 3), [false, true, true]);
}

/// A cycle through nodes at or past `marked` still marks its members below `marked`, and the
/// answer is `marked` long however many nodes the graph has.
#[test]
fn a_cycle_through_unmarked_nodes_marks_only_its_marked_members() {
  // 0 → 2 → 3 → 0 is a cycle with one marked member, 1 → 2 only reaches it, and 4 ↔ 5 is a cycle
  // with no marked member at all.
  let graph: [&[u32]; 6] = [&[2], &[2], &[3], &[0], &[5], &[4]];
  assert_eq!(cycles(&graph, 2), [true, false]);
}

/// A chain of five thousand nodes has no cycle, and one edge back from its last node to its first
/// puts every node on one.
///
/// The length is what makes this the guard on the walk being iterative: a recursive one would hold
/// a frame per node of the chain at once.
#[test]
fn a_long_chain_is_acyclic_until_its_end_reaches_its_start() {
  const LENGTH: usize = 5_000;
  let chain = |back: bool| {
    on_cycle(LENGTH, LENGTH, |node, reaches| {
      if node + 1 < LENGTH {
        reaches.push(node as u32 + 1);
      } else if back {
        reaches.push(0);
      }
    })
  };
  assert_eq!(chain(false), vec![false; LENGTH], "a chain has no cycle");
  assert_eq!(
    chain(true),
    vec![true; LENGTH],
    "a chain closed at its end is one cycle"
  );
}

/// [`components`] over a graph written as one row of successors per node.
fn groups(graph: &[&[u32]]) -> Vec<u32> {
  components(graph.len(), |node, reaches| {
    reaches.extend_from_slice(graph[node]);
  })
}

/// A graph with no nodes has no component.
#[test]
fn no_nodes_is_an_empty_answer() {
  assert!(components(0, |_, _| {}).is_empty(), "a graph with no nodes");
}

/// A node that shares a component with no other node is named by itself, with an edge to itself
/// or without one, and two nodes that reach each other share one, named by the lesser.
#[test]
fn a_self_edge_is_its_own_component_and_a_pair_that_reach_each_other_share_one() {
  assert_eq!(groups(&[&[0], &[]]), [0, 1]);
  assert_eq!(groups(&[&[1], &[0], &[]]), [0, 0, 2]);
}

/// Parallel edges are one edge, and a node's edge to itself inside a larger component changes
/// nothing of the component.
#[test]
fn parallel_edges_and_a_self_edge_inside_a_component_change_nothing() {
  // 0 → 1 twice and 1 → 0 twice: one component, beside 2 alone.
  assert_eq!(groups(&[&[1, 1], &[0, 0], &[]]), [0, 0, 2]);
  // 0 → 1 → 2 → 0, with 1 → 1 too: one component, named by 0.
  assert_eq!(groups(&[&[1], &[1, 2], &[0]]), [0, 0, 0]);
  // The same with the self-edge on the least node, and a node with an edge to itself that reaches
  // the component without being reached back.
  assert_eq!(groups(&[&[0, 1], &[2], &[0], &[3, 1]]), [0, 0, 0, 3]);
}

/// A node that reaches a component without being reached back is not in it.
#[test]
fn reaching_a_component_is_not_being_in_it() {
  assert_eq!(groups(&[&[1], &[1]]), [0, 1]);
  assert_eq!(groups(&[&[1], &[2], &[1]]), [0, 1, 1]);
}

/// A component is named by its least node, whichever node the walk enters it by, and two cycles
/// that do not reach each other are two components.
#[test]
fn each_component_is_named_by_its_least_node() {
  // 0 reaches the cycle 3 → 2 → 3 at 3, its greater node; 1 stands alone.
  assert_eq!(groups(&[&[3], &[], &[3], &[2]]), [0, 1, 2, 2]);
  // 0 ↔ 3 and 1 ↔ 2 are two cycles, so two components.
  assert_eq!(groups(&[&[3], &[2], &[1], &[0]]), [0, 1, 1, 0]);
}

/// A chain of five thousand nodes is a component a node, and one edge back from its last node to
/// its first makes it one component.
///
/// The length is what makes this the guard on the walk being iterative: a recursive one would hold
/// a frame per node of the chain at once.
#[test]
fn a_long_chain_is_one_component_once_its_end_reaches_its_start() {
  const LENGTH: usize = 5_000;
  let chain = |back: bool| {
    components(LENGTH, |node, reaches| {
      if node + 1 < LENGTH {
        reaches.push(node as u32 + 1);
      } else if back {
        reaches.push(0);
      }
    })
  };
  let apart: Vec<u32> = (0..LENGTH).map(|node| node as u32).collect();
  assert_eq!(chain(false), apart, "a chain is a component a node");
  assert_eq!(
    chain(true),
    vec![0; LENGTH],
    "a chain closed at its end is one component"
  );
}
