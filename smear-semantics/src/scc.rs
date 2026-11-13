//! Which nodes of a directed graph lie on a cycle, and which strongly connected component each
//! node is in, each found in one walk over compressed rows.

use std::{vec, vec::Vec};

/// Which of the first `marked` nodes of a directed graph lie on a cycle.
///
/// The nodes are `0..nodes`. `successors` is handed a node and an empty vector, and writes into
/// the vector the nodes that node reaches in one step. It is called exactly twice per node — a
/// counting pass, then a filling pass — and must give the same answer both times. Every successor
/// is below `nodes`, and `marked <= nodes`.
///
/// The answer has length `marked`. Entry `n` is true when node `n` has an edge to itself or
/// shares a strongly connected component with another node; a node that merely reaches a cycle is
/// not on one.
///
/// # Compressed rows, and why the edges are counted before they are written
///
/// `successors` is asked twice — once to size each row, once to fill it — rather than pushing
/// into a vector per node, because one row per node is `nodes` allocations and this is two. The
/// scratch it writes through is reused across both passes and every node.
pub fn on_cycle(
  nodes: usize,
  marked: usize,
  mut successors: impl FnMut(usize, &mut Vec<u32>),
) -> Vec<bool> {
  let mut reaches: Vec<u32> = Vec::new();
  let mut offset: Vec<usize> = vec![0; nodes + 1];
  for node in 0..nodes {
    reaches.clear();
    successors(node, &mut reaches);
    offset[node + 1] = reaches.len();
  }
  for node in 0..nodes {
    offset[node + 1] += offset[node];
  }
  let mut edges: Vec<u32> = vec![0; offset[nodes]];
  for node in 0..nodes {
    reaches.clear();
    successors(node, &mut reaches);
    edges[offset[node]..offset[node + 1]].copy_from_slice(&reaches);
  }

  // Tarjan, iterative for the reason the walk it replaces was iterative: the type graph's depth
  // is bounded by nothing but the document, and a recursive walk would put an SDL's
  // input-object chain on the call stack.
  //
  // Discovery indices start at one so that zero means "not yet visited". A sentinel at the top
  // of the range would be a second thing the counter must never reach; this way the counter has
  // only its own width to respect.
  const UNVISITED: u32 = 0;
  let mut discovered: Vec<u32> = vec![UNVISITED; nodes];
  let mut low: Vec<u32> = vec![0; nodes];
  let mut stacked: Vec<bool> = vec![false; nodes];
  let mut component: Vec<u32> = Vec::new();
  /// A node and the edge of its row to read next.
  type Descent = (u32, usize);
  let mut descent: Vec<Descent> = Vec::new();
  let mut next_discovery: u32 = 1;
  let mut cyclic = vec![false; marked];

  for root in 0..nodes {
    if discovered[root] != UNVISITED {
      continue;
    }
    discovered[root] = next_discovery;
    low[root] = next_discovery;
    next_discovery += 1;
    component.push(root as u32);
    stacked[root] = true;
    descent.push((root as u32, offset[root]));

    while let Some(&(node, cursor)) = descent.last() {
      let node = node as usize;
      if cursor < offset[node + 1] {
        descent
          .last_mut()
          .expect("the frame just read is still on the descent")
          .1 = cursor + 1;
        let next = edges[cursor] as usize;
        if next == node && node < marked {
          // A component of one, which nothing below can tell from an acyclic node.
          cyclic[node] = true;
        }
        if discovered[next] == UNVISITED {
          discovered[next] = next_discovery;
          low[next] = next_discovery;
          next_discovery += 1;
          component.push(next as u32);
          stacked[next] = true;
          descent.push((next as u32, offset[next]));
        } else if stacked[next] {
          low[node] = low[node].min(discovered[next]);
        }
        continue;
      }

      descent.pop();
      if let Some(&(parent, _)) = descent.last() {
        low[parent as usize] = low[parent as usize].min(low[node]);
      }
      if low[node] != discovered[node] {
        continue;
      }
      // The component is the tail of the stack above this node, which is its root.
      let at = component
        .iter()
        .rposition(|&member| member as usize == node)
        .expect("a component's root is on the component stack");
      // Two members mean every member reaches every other, so every marked node among them
      // reaches itself.
      let closed = component.len() - at > 1;
      for &member in &component[at..] {
        stacked[member as usize] = false;
        if closed && (member as usize) < marked {
          cyclic[member as usize] = true;
        }
      }
      component.truncate(at);
    }
  }

  cyclic
}

/// The strongly connected component of each node of a directed graph, each named by its least
/// node.
///
/// The nodes are `0..nodes`. `successors` is handed a node and an empty vector, and writes into
/// the vector the nodes that node reaches in one step. It is called exactly twice per node — a
/// counting pass, then a filling pass — and must give the same answer both times. Every successor
/// is below `nodes`.
///
/// The answer has length `nodes`. Entry `n` is the least node of the component that holds `n`, so
/// two nodes share a component exactly when their entries are equal, and an entry equals its own
/// index exactly for the least node of its component. A node no cycle passes through is a
/// component of its own, whether or not it has an edge to itself: a caller that needs to tell the
/// two apart reads it off its edges.
///
/// The rows and the walk are [`on_cycle`]'s: the edges are counted, then written into compressed
/// rows, and Tarjan's algorithm runs over them with an explicit descent, so no graph, however
/// deep, puts a frame per node on the call stack. Each component is named once it is complete, in
/// one pass over its members.
pub fn components(nodes: usize, mut successors: impl FnMut(usize, &mut Vec<u32>)) -> Vec<u32> {
  let mut reaches: Vec<u32> = Vec::new();
  let mut offset: Vec<usize> = vec![0; nodes + 1];
  for node in 0..nodes {
    reaches.clear();
    successors(node, &mut reaches);
    offset[node + 1] = reaches.len();
  }
  for node in 0..nodes {
    offset[node + 1] += offset[node];
  }
  let mut edges: Vec<u32> = vec![0; offset[nodes]];
  for node in 0..nodes {
    reaches.clear();
    successors(node, &mut reaches);
    edges[offset[node]..offset[node + 1]].copy_from_slice(&reaches);
  }

  // Tarjan, iterative as `on_cycle`'s is, with discovery indices from one so that zero means "not
  // yet visited".
  const UNVISITED: u32 = 0;
  let mut discovered: Vec<u32> = vec![UNVISITED; nodes];
  let mut low: Vec<u32> = vec![0; nodes];
  let mut stacked: Vec<bool> = vec![false; nodes];
  let mut component: Vec<u32> = Vec::new();
  /// A node and the edge of its row to read next.
  type Descent = (u32, usize);
  let mut descent: Vec<Descent> = Vec::new();
  let mut next_discovery: u32 = 1;
  let mut least: Vec<u32> = vec![0; nodes];

  for root in 0..nodes {
    if discovered[root] != UNVISITED {
      continue;
    }
    discovered[root] = next_discovery;
    low[root] = next_discovery;
    next_discovery += 1;
    component.push(root as u32);
    stacked[root] = true;
    descent.push((root as u32, offset[root]));

    while let Some(&(node, cursor)) = descent.last() {
      let node = node as usize;
      if cursor < offset[node + 1] {
        descent
          .last_mut()
          .expect("the frame just read is still on the descent")
          .1 = cursor + 1;
        let next = edges[cursor] as usize;
        if discovered[next] == UNVISITED {
          discovered[next] = next_discovery;
          low[next] = next_discovery;
          next_discovery += 1;
          component.push(next as u32);
          stacked[next] = true;
          descent.push((next as u32, offset[next]));
        } else if stacked[next] {
          low[node] = low[node].min(discovered[next]);
        }
        continue;
      }

      descent.pop();
      if let Some(&(parent, _)) = descent.last() {
        low[parent as usize] = low[parent as usize].min(low[node]);
      }
      if low[node] != discovered[node] {
        continue;
      }
      // The component is the tail of the stack above this node, which is its root; its name is
      // its least member, which need not be the node the walk entered it by.
      let at = component
        .iter()
        .rposition(|&member| member as usize == node)
        .expect("a component's root is on the component stack");
      let members = &component[at..];
      let name = members.iter().copied().min().unwrap_or(node as u32);
      for &member in members {
        stacked[member as usize] = false;
        least[member as usize] = name;
      }
      component.truncate(at);
    }
  }

  least
}

#[cfg(test)]
mod tests;
