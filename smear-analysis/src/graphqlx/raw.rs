//! The values as written: what lowering reads of each written value for coercion, which writes the
//! model's values.
//!
//! Each default and each argument an application writes is a tree: one fixed-size node for each
//! literal it writes, the value itself first and then what it holds, in pre-order, a list's and a
//! set's elements in order, a map's entries each key and then its value, an object's fields'
//! values in written order. An object's node keeps where its fields' names are: one row of the
//! table's fields for each field it writes, in written order, each the name's symbol, found among
//! the names interned and never interned, and where the name is written. The trees, the table of
//! every source's text the literals are read from, which default each tree is and which argument,
//! are the analysis's own: lowering builds them, coercion reads them, and they are dropped when
//! coercion ends. What coercion reads of a node is kept outside the node, in coercion's own tables,
//! which the node's one slot indexes.

use std::{collections::BTreeMap, vec::Vec};

use tokora::{SimpleSpan, diagnostic::Location};

use super::{
  id::{ApplicationId, MemberId, ModuleId, Sym, dense},
  model::{PathRoot, Segments},
  resolve::{Resolved, Root},
};

/// The radix an integer literal is written in.
#[derive(Clone, Copy)]
pub(crate) enum Radix {
  Decimal,
  Hex,
  Octal,
  Binary,
}

/// What a node is, as written.
#[derive(Clone, Copy)]
pub(crate) enum RawKind {
  Null,
  Boolean(bool),
  Int(Radix),
  /// A float literal, hexadecimal when `hex`.
  Float {
    hex: bool,
  },
  String,
  /// A path: its row of the table's paths.
  Path(u32),
  List,
  Set,
  Map,
  /// An object: its first row of the table's fields, the row of its first field, each later field
  /// the next row.
  Object(u32),
}

/// One literal a value writes: 32 bytes at most.
#[derive(Clone, Copy)]
pub(crate) struct RawNode {
  pub(crate) kind: RawKind,
  /// Where it is written, as the parser spans it, in its tree's source.
  pub(crate) span: SimpleSpan,
  /// One past the last node of its subtree: its children are the node after it, the node at that
  /// one's `after`, and so on up to its own.
  pub(crate) after: u32,
  /// Its row of what coercion has read of it: [`UNREAD`] until something is.
  pub(crate) kept: u32,
}

/// A node's `kept` slot before coercion has read anything of it.
pub(crate) const UNREAD: u32 = u32::MAX;

const _: () = assert!(core::mem::size_of::<RawNode>() <= 32);

/// A path a value writes, as written.
#[derive(Clone, Copy)]
pub(crate) struct RawPath {
  /// Its root as the model records it: `None` for a root of a kind lowering does not know, which
  /// lowering reports.
  pub(crate) written: Option<PathRoot>,
  /// Its root as the resolver reads it.
  pub(crate) root: Root,
  /// Its segments, each a symbol and where it is written: where its run of the table's segments
  /// starts, and how long it is.
  pub(crate) segments: (u32, u32),
  /// The generic parameter in scope that its first segment names, when it has no root: a
  /// parameter shadows every module name, and only a first segment can be one.
  pub(crate) param: Option<u32>,
  /// Its segments in the model's path pool, once coercion has kept them.
  pub(crate) kept: Option<Segments>,
  /// What its segments but the last name, once coercion has asked.
  pub(crate) prefix: Option<Resolved>,
}

/// One field an object writes: its name, and where the name is written. The object's i-th child
/// is the value of its i-th field.
#[derive(Clone, Copy)]
pub(crate) struct RawField {
  /// The name, when a member's name is spelled so: a name never interned is no member's.
  pub(crate) name: Option<Sym>,
  /// Where the name is written, in its tree's source.
  pub(crate) at: SimpleSpan,
}

/// One written value: its first node, and the module and the source it is written in.
#[derive(Clone, Copy)]
pub(crate) struct RawTree {
  pub(crate) root: u32,
  pub(crate) source: u32,
  pub(crate) module: ModuleId,
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
  /// Every node, each tree's in pre-order.
  pub(crate) nodes: Vec<RawNode>,
  /// Every path a node is.
  pub(crate) paths: Vec<RawPath>,
  /// Every path's segments, each a symbol and where it is written, each path's one run.
  pub(crate) segments: Vec<(Sym, SimpleSpan)>,
  /// Every field an object writes, each object's one run, in written order.
  pub(crate) fields: Vec<RawField>,
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
      fields: Vec::new(),
      trees: Vec::new(),
      defaults: BTreeMap::new(),
      arguments: Vec::new(),
      texts,
    }
  }

  /// Appends the path written from `written`, which the resolver reads as `root`, through
  /// `segments`, whose first names the generic parameter `param` in scope: its row of the paths.
  pub(crate) fn push_path(
    &mut self,
    written: Option<PathRoot>,
    root: Root,
    param: Option<u32>,
    segments: impl Iterator<Item = (Sym, SimpleSpan)>,
  ) -> u32 {
    let row = dense(self.paths.len());
    let start = self.segments.len();
    self.segments.extend(segments);
    self.paths.push(RawPath {
      written,
      root,
      segments: (dense(start), dense(self.segments.len() - start)),
      param,
      kept: None,
      prefix: None,
    });
    row
  }

  /// Appends a node of `kind`, written at `span`, with nothing under it yet: its index.
  pub(crate) fn push_node(&mut self, kind: RawKind, span: SimpleSpan) -> u32 {
    let at = dense(self.nodes.len());
    self.nodes.push(RawNode {
      kind,
      span,
      after: at.saturating_add(1),
      kept: UNREAD,
    });
    at
  }

  /// Closes the node at `at`: every node appended since it is under it.
  pub(crate) fn close_node(&mut self, at: u32) {
    let after = dense(self.nodes.len());
    if let Some(node) = self.nodes.get_mut(at as usize) {
      node.after = after;
    }
  }

  /// Appends the tree whose first node is `root`, written in the source `source` and in the module
  /// `module`: its index.
  pub(crate) fn push_tree(&mut self, root: u32, source: u32, module: ModuleId) -> u32 {
    let tree = dense(self.trees.len());
    self.trees.push(RawTree {
      root,
      source,
      module,
    });
    tree
  }

  /// The text written at `span` in the source `source`: none for a span that does not fit its
  /// source, which the parser never makes.
  pub(crate) fn text(&self, source: u32, span: SimpleSpan) -> &'p [u8] {
    let file: &'p [u8] = self.texts.get(source as usize).copied().unwrap_or_default();
    file.get(span.start()..span.end()).unwrap_or_default()
  }

  /// The segments of `path`, each with where it is written: none for a run past the table, which
  /// no path has.
  pub(crate) fn path_segments(&self, path: RawPath) -> &[(Sym, SimpleSpan)] {
    let (start, len) = path.segments;
    let start = start as usize;
    self
      .segments
      .get(start..start.saturating_add(len as usize))
      .unwrap_or_default()
  }
}
