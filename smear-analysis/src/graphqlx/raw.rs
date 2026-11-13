//! The values as written: what lowering reads of each written value for coercion, which writes the
//! model's values.
//!
//! Each default and each argument an application writes is a tree of nodes, kept with the source
//! it is written in. The trees, the table of every source's text the literals are read from, which
//! default each tree is and which argument, are the analysis's own: lowering builds them,
//! coercion reads them, and they are dropped when coercion ends.

use std::{collections::BTreeMap, vec::Vec};

use tokora::{SimpleSpan, diagnostic::Location};

use super::{
  id::{ApplicationId, MemberId, Sym, dense},
  model::PathRoot,
};

/// What a node is, as written.
#[derive(Clone, Copy)]
pub(crate) enum RawKind {
  Null,
  Boolean,
  Int,
  Float,
  String,
  /// A path: its row of the table's paths.
  Path(u32),
  List,
  Set,
  Map,
  Object,
}

/// One written value.
#[derive(Clone, Copy)]
pub(crate) struct RawNode {
  pub(crate) kind: RawKind,
  /// Where it is written, as the parser spans it, in its tree's source.
  pub(crate) span: SimpleSpan,
}

/// A path a value writes, as written.
#[derive(Clone, Copy)]
pub(crate) struct RawPath {
  /// Its root as the model records it: `None` for a root of a kind lowering does not know, which
  /// lowering reports.
  pub(crate) written: Option<PathRoot>,
  /// Its segments: where its run of the table's segments starts, and how long it is.
  pub(crate) segments: (u32, u32),
}

/// One written value: the node it is, and the source it is written in.
#[derive(Clone, Copy)]
pub(crate) struct RawTree {
  pub(crate) root: u32,
  pub(crate) source: u32,
}

/// One argument an application writes that its directive declares.
#[derive(Clone, Copy)]
pub(crate) struct RawArgument {
  pub(crate) application: ApplicationId,
  /// The directive's argument it sets.
  pub(crate) argument: MemberId,
  /// Where its name is written.
  pub(crate) name: Location,
  /// Its value.
  pub(crate) tree: u32,
}

/// Every written value, as lowering reads it.
pub(crate) struct RawValues<'p> {
  /// Every node.
  pub(crate) nodes: Vec<RawNode>,
  /// Every path a node is.
  pub(crate) paths: Vec<RawPath>,
  /// Every path's segments, each path's one run.
  pub(crate) segments: Vec<Sym>,
  /// Every written value.
  pub(crate) trees: Vec<RawTree>,
  /// Each member's default, by the member: what the walk of the members looks a default up by.
  pub(crate) defaults: BTreeMap<MemberId, u32>,
  /// Every argument the applications write, in the order lowering appends them: application by
  /// application in id order, and each one's in the order of its directive's arguments, an
  /// argument written twice twice, in written order.
  pub(crate) arguments: Vec<RawArgument>,
  /// Each source's text, by `SourceId`: a package source's is empty.
  pub(crate) texts: Vec<&'p [u8]>,
}

impl<'p> RawValues<'p> {
  /// No value yet, each written in one of the sources whose texts `texts` holds.
  pub(crate) fn new(texts: Vec<&'p [u8]>) -> Self {
    Self {
      nodes: Vec::new(),
      paths: Vec::new(),
      segments: Vec::new(),
      trees: Vec::new(),
      defaults: BTreeMap::new(),
      arguments: Vec::new(),
      texts,
    }
  }

  /// Appends the path written from `written` through `segments`: its row of the paths.
  pub(crate) fn push_path(&mut self, written: Option<PathRoot>, segments: &[Sym]) -> u32 {
    let row = dense(self.paths.len());
    let start = dense(self.segments.len());
    self.segments.extend_from_slice(segments);
    self.paths.push(RawPath {
      written,
      segments: (start, dense(segments.len())),
    });
    row
  }

  /// Appends the value `node`, written in the source `source`: its tree.
  pub(crate) fn push_tree(&mut self, source: u32, node: RawNode) -> u32 {
    let tree = dense(self.trees.len());
    let root = dense(self.nodes.len());
    self.nodes.push(node);
    self.trees.push(RawTree { root, source });
    tree
  }

  /// The text written at `span` in the source `source`: none for a span that does not fit its
  /// source, which the parser never makes.
  pub(crate) fn text(&self, source: u32, span: SimpleSpan) -> &'p [u8] {
    let file: &'p [u8] = self.texts.get(source as usize).copied().unwrap_or_default();
    file.get(span.start()..span.end()).unwrap_or_default()
  }

  /// The segments of `path`: none for a run past the table, which no path has.
  pub(crate) fn path_segments(&self, path: RawPath) -> &[Sym] {
    let (start, len) = path.segments;
    let start = start as usize;
    self
      .segments
      .get(start..start.saturating_add(len as usize))
      .unwrap_or_default()
  }
}
