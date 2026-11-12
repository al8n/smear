//! The module tree: every package's modules, found from its directories and its inline `mod`s, and
//! numbered in [`ModuleId`] order once all are found.
//!
//! A package's root module is its root directory's files, or, in the `Flat` layout, all of its
//! files. In the `Directory` layout every directory that holds a file, and every directory on the
//! way to one, is a module whose path is the directory's: the files of `media/vision/` are
//! `~::media::vision`. An inline `mod name { … }` declares a child of the module it is written in,
//! in either layout. One path is one module: a directory module, or else the first `mod` in source
//! order, declares it, and the walk reports every later declaration.
//!
//! A module costs the same at any depth: it holds its parent, its own name and its depth, never its
//! path, and it is found by its parent and its name.
//!
//! # Numbering
//!
//! [`Tree::push`] numbers the packages in id order, and each package's modules in pre-order: a
//! module, then each of its children with all of that child's descendants, the children in bytewise
//! order of their names. That is the `ModuleId` order, which compares two modules' paths segment by
//! segment, each segment bytewise. Of two modules of one package, either one is the other's
//! ancestor, whose path is a proper prefix of the other's, and the ancestor comes first in both
//! orders; or their paths part at one segment, under the module whose path they share, and both
//! orders put first the one whose segment there is the lesser bytewise: the paths compare as that
//! segment does, and pre-order numbers the whole of the lesser child's subtree before the other
//! child.

use std::{collections::BTreeMap, string::String, vec, vec::Vec};

use smear_lexer::decode_string_value;
use tokora::diagnostic::Location;

use super::{
  id::{ModuleId, PackageId, dense},
  model::{ModelBuilder, ModuleKind},
};

/// A module the walk found, before the tree numbers it.
pub(crate) struct Node<'d> {
  /// The index of its package, in id order.
  pub(crate) package: usize,
  /// Its name, the last segment of its path; empty for a root module.
  pub(crate) name: &'d [u8],
  /// The node of the module it is inside; `None` for a root module.
  pub(crate) parent: Option<u32>,
  /// How many modules it sits below its package's root module, whose depth is 0.
  pub(crate) depth: u32,
  pub(crate) kind: ModuleKind,
  /// For a directory module, its path as the first file under it spells it: what a diagnostic
  /// quotes.
  pub(crate) directory: Option<&'d [u8]>,
  /// For a module only `mod`s declare, the first of them.
  pub(crate) declared: Option<Declared<'d>>,
}

/// An inline `mod`'s declaration of the module it names.
#[derive(Clone, Copy)]
pub(crate) struct Declared<'d> {
  /// Where it names the module.
  pub(crate) location: Location,
  /// From its description, or its `mod`, to its `}`.
  pub(crate) extent: Location,
  /// Its description, as written, quotes included, and where it is written.
  pub(crate) description: Option<(&'d [u8], Location)>,
}

/// Every module found so far, and the key each is found by.
pub(crate) struct Tree<'d> {
  nodes: Vec<Node<'d>>,
  /// Every module but a root module, by its parent's node and its name: what two declarations of
  /// one module share, and, in the map's order, each module's children in bytewise order of their
  /// names, which numbering reads.
  children: BTreeMap<(u32, &'d [u8]), u32>,
  /// Each package's root module, by the package's index.
  roots: Vec<u32>,
}

impl<'d> Tree<'d> {
  /// A tree with no module.
  pub(crate) fn new() -> Self {
    Self {
      nodes: Vec::new(),
      children: BTreeMap::new(),
      roots: Vec::new(),
    }
  }

  /// Adds the root module of the package at `package`, the next package in id order; answers its
  /// node.
  pub(crate) fn root(&mut self, package: usize) -> u32 {
    let node = dense(self.nodes.len());
    self.nodes.push(Node {
      package,
      name: b"",
      parent: None,
      depth: 0,
      kind: ModuleKind::Root,
      directory: None,
      declared: None,
    });
    self.roots.push(node);
    node
  }

  /// The root module of the package at `package`.
  pub(crate) fn root_of(&self, package: usize) -> u32 {
    self.roots[package]
  }

  /// The directory module `name` inside `parent`, which the file paths spell `directory`: the one
  /// the tree has, or a new one.
  ///
  /// Every directory module is added before any `mod` is walked, so a child the tree already has
  /// here is a directory module too.
  pub(crate) fn directory(&mut self, parent: u32, name: &'d [u8], directory: &'d [u8]) -> u32 {
    if let Some(found) = self.child(parent, name) {
      return found;
    }
    self.insert(parent, name, ModuleKind::Directory, Some(directory), None)
  }

  /// Adds the module `name` inside `parent`, which only the `mod` `declared` declares so far, and
  /// which the tree must not have yet.
  pub(crate) fn declare(&mut self, parent: u32, name: &'d [u8], declared: Declared<'d>) -> u32 {
    self.insert(parent, name, ModuleKind::Inline, None, Some(declared))
  }

  /// The module `name` inside `parent`, when the tree has it.
  pub(crate) fn child(&self, parent: u32, name: &'d [u8]) -> Option<u32> {
    self.children.get(&(parent, name)).copied()
  }

  /// The module `node` names.
  pub(crate) fn node(&self, node: u32) -> &Node<'d> {
    &self.nodes[node as usize]
  }

  /// Every module, by node.
  pub(crate) fn nodes(&self) -> &[Node<'d>] {
    &self.nodes
  }

  /// Appends every module to `builder` in `ModuleId` order: the packages in id order, and each
  /// package's modules in pre-order, each module's children in bytewise order of their names, with
  /// an explicit stack; answers each node's id. The builder must hold every package already, and no
  /// module yet.
  pub(crate) fn push(&self, builder: &mut ModelBuilder) -> Vec<ModuleId> {
    // The children map is ordered by parent, then name, so each node's children are one run of
    // `order`, from `starts[node]` to `starts[node + 1]`, in bytewise order of their names.
    let mut starts: Vec<u32> = vec![0; self.nodes.len() + 1];
    for &(parent, _) in self.children.keys() {
      starts[parent as usize] += 1;
    }
    let mut total: u32 = 0;
    for start in &mut starts {
      let count = *start;
      *start = total;
      total += count;
    }
    let order: Vec<u32> = self.children.values().copied().collect();

    let mut ids: Vec<ModuleId> = vec![ModuleId::new(0); self.nodes.len()];
    let mut stack: Vec<u32> = Vec::new();
    let mut text = String::new();
    for &root in &self.roots {
      stack.push(root);
      while let Some(node) = stack.pop() {
        let row = &self.nodes[node as usize];
        let package = PackageId::new(dense(row.package));
        let parent: Option<ModuleId> = row.parent.map(|parent: u32| ids[parent as usize]);
        let name = row.parent.map(|_: u32| String::from_utf8_lossy(row.name));
        let id = builder.push_module(package, parent, name.as_deref(), row.kind);
        ids[node as usize] = id;
        if let Some(declared) = row.declared {
          builder.set_module_declaration(id, declared.location, declared.extent);
          if let Some((spelling, location)) = declared.description {
            text.clear();
            // A description the lexer read always decodes; one that did not is left absent.
            if decode_string_value(spelling, &mut text).is_ok() {
              builder.set_module_description(id, &text, location);
            }
          }
        }
        // Its children, the last first, so that the first in bytewise order is numbered next, and
        // all of its descendants before its next sibling.
        let index = node as usize;
        let children: &[u32] = &order[starts[index] as usize..starts[index + 1] as usize];
        stack.extend(children.iter().rev());
      }
    }
    ids
  }

  /// Adds the module `name` of `kind` inside `parent`; answers its node.
  fn insert(
    &mut self,
    parent: u32,
    name: &'d [u8],
    kind: ModuleKind,
    directory: Option<&'d [u8]>,
    declared: Option<Declared<'d>>,
  ) -> u32 {
    let up = &self.nodes[parent as usize];
    let node = Node {
      package: up.package,
      name,
      parent: Some(parent),
      depth: up.depth.saturating_add(1),
      kind,
      directory,
      declared,
    };
    let index = dense(self.nodes.len());
    self.children.insert((parent, name), index);
    self.nodes.push(node);
    index
  }
}
