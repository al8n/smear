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
//! The tree is keyed by package, then path, the path compared segment by segment and each segment
//! bytewise: exactly what the `ModuleId` order compares. Numbering walks the keys in order, so a
//! package's root module comes first and a module before its descendants.

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
  /// Its path from its package's root module, one name per segment.
  pub(crate) path: Vec<&'d [u8]>,
  /// The node of the module it is inside; `None` for a root module.
  pub(crate) parent: Option<u32>,
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
  /// Each module's package and path, mapped to its node. The key is what the `ModuleId` order
  /// compares, and what two declarations of one module share.
  keys: BTreeMap<(usize, Vec<&'d [u8]>), u32>,
  /// Each package's root module, by the package's index.
  roots: Vec<u32>,
}

impl<'d> Tree<'d> {
  /// A tree with no module.
  pub(crate) fn new() -> Self {
    Self {
      nodes: Vec::new(),
      keys: BTreeMap::new(),
      roots: Vec::new(),
    }
  }

  /// Adds the root module of the package at `package`, the next package in id order; answers its
  /// node.
  pub(crate) fn root(&mut self, package: usize) -> u32 {
    let node = self.insert(Node {
      package,
      path: Vec::new(),
      parent: None,
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
    let up = &self.nodes[parent as usize];
    let package = up.package;
    let path = extended(&up.path, name);
    self.insert(Node {
      package,
      path,
      parent: Some(parent),
      kind: ModuleKind::Directory,
      directory: Some(directory),
      declared: None,
    })
  }

  /// Adds the module `name` inside `parent`, which only the `mod` `declared` declares so far, and
  /// which the tree must not have yet.
  pub(crate) fn declare(&mut self, parent: u32, name: &'d [u8], declared: Declared<'d>) -> u32 {
    let up = &self.nodes[parent as usize];
    let package = up.package;
    let path = extended(&up.path, name);
    self.insert(Node {
      package,
      path,
      parent: Some(parent),
      kind: ModuleKind::Inline,
      directory: None,
      declared: Some(declared),
    })
  }

  /// The module `name` inside `parent`, when the tree has it.
  pub(crate) fn child(&self, parent: u32, name: &'d [u8]) -> Option<u32> {
    let up = &self.nodes[parent as usize];
    self
      .keys
      .get(&(up.package, extended(&up.path, name)))
      .copied()
  }

  /// The module `node` names.
  pub(crate) fn node(&self, node: u32) -> &Node<'d> {
    &self.nodes[node as usize]
  }

  /// Every module, by node.
  pub(crate) fn nodes(&self) -> &[Node<'d>] {
    &self.nodes
  }

  /// Appends every module to `builder` in `ModuleId` order: package by package, and within one by
  /// path; answers each node's id. The builder must hold every package already, and no module yet.
  pub(crate) fn push(&self, builder: &mut ModelBuilder) -> Vec<ModuleId> {
    let mut ids: Vec<ModuleId> = vec![ModuleId::new(0); self.nodes.len()];
    let mut text = String::new();
    for &node in self.keys.values() {
      let row = &self.nodes[node as usize];
      let package = PackageId::new(dense(row.package));
      let parent: Option<ModuleId> = row.parent.map(|parent: u32| ids[parent as usize]);
      let name = row.path.last().copied().map(String::from_utf8_lossy);
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
    }
    ids
  }

  /// Adds `node`, under its key; answers its index.
  fn insert(&mut self, node: Node<'d>) -> u32 {
    let index = dense(self.nodes.len());
    self.keys.insert((node.package, node.path.clone()), index);
    self.nodes.push(node);
    index
  }
}

/// `path`, and then `name`.
fn extended<'d>(path: &[&'d [u8]], name: &'d [u8]) -> Vec<&'d [u8]> {
  let mut extended: Vec<&'d [u8]> = Vec::with_capacity(path.len() + 1);
  extended.extend_from_slice(path);
  extended.push(name);
  extended
}
