//! Which nodes lie on a cycle, over graphs small enough to read whole, and over one chain long
//! enough that a walk holding a frame per node would hold five thousand of them.

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
