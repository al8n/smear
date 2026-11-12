//! Item collection: one walk over the parsed documents, which finds every package's modules and
//! everything each module declares, and hands both on, so that no later phase walks a document's
//! items again.
//!
//! Each file is placed in a module first: its package's root module in the `Flat` layout, and its
//! directory's module in the `Directory` layout. Then each placed file's items are walked in
//! source order, the items of an inline `mod` in the module it declares, with an explicit stack,
//! so that modules nested however deep cost no native stack. Each item is kept, by its index among
//! its source's items in that order: each definition with its kind, name, location, extent and
//! description; each extension, `use` item, and schema definition or extension with its module.
//! The same walk reads every item's type positions for the widths it writes ([`Widths`]).
//!
//! A file is placed nowhere when the package set refused its path (an invalid path, or one that
//! two files share, which nothing but their texts tells apart), when a directory on its way is not
//! a Name, and when a directory on its way sits deeper than `Limits::max_module_depth`. Such a file
//! is parsed all the same, so its syntax errors are reported, but nothing it declares is collected.
//!
//! The walk is written once, generic over the slice type of a parsed document, and called alike
//! for a document of text and a document of bytes: nothing it does depends on which parser made a
//! document.

use std::{collections::BTreeMap, format, vec::Vec};

use smear_parser::graphqlx::ast::{
  DescribedTypeSystemDefinition, ModDefinition, Name, StringValue, TypeDefinition,
  TypeSystemDefinition, TypeSystemDocument, TypeSystemExtension, TypeSystemItem,
};
use tokora::{diagnostic::Location, span::AsSpan};

use super::{
  check,
  code::DiagnosticKind,
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{SourceId, dense},
  limits::Limits,
  model::{DefKind, Primitive},
  package::Layout,
  parse::{Documents, Parsed},
  set::{FileView, PackageView},
  source::SourceMap,
  tree::{Declared, Tree},
  widths::Widths,
};

/// What the walk found: every module, everything each module declares, and every primitive the
/// program writes. The declarations number what it found ([`declarations`](super::declare)).
pub(crate) struct Collected<'d, 'a> {
  /// Every module, before it is numbered.
  pub(crate) tree: Tree<'d>,
  /// Each source's items, flattened in the order the walk reads them, which is source order with a
  /// `mod`'s items right after it: what [`Placed::item`] indexes. `None` for a source the walk did
  /// not read.
  pub(crate) items: Vec<Option<Items<'d, 'a>>>,
  /// Every type and directive definition, in the order the walk met them: by package, then
  /// source, then position.
  pub(crate) definitions: Vec<Found<'d>>,
  /// Every `use` item, in the same order.
  pub(crate) uses: Vec<Placed>,
  /// Every type and directive extension, in the same order.
  pub(crate) extensions: Vec<Placed>,
  /// Every schema definition and extension, in the same order.
  pub(crate) schemas: Vec<Placed>,
  /// Every primitive a placed file writes where its package may have it, keyed by its place among
  /// `core`'s materialised definitions.
  pub(crate) written: BTreeMap<(u8, u32), Primitive>,
}

/// One document's items, flattened, of the parser that made the document.
// Read by name resolution and lowering, the next parts of the analyzer, through the items the
// declarations address; until they land nothing reads it, and `dead_code` would say so of each
// variant's field.
#[allow(dead_code)]
pub(crate) enum Items<'d, 'a> {
  /// A document of text.
  Text(Vec<&'d TypeSystemItem<&'a str>>),
  /// A document of bytes that are not UTF-8.
  Bytes(Vec<&'d TypeSystemItem<&'a [u8]>>),
}

/// An item the walk kept: the module it is in, its source, and its index among that source's
/// items.
#[derive(Clone, Copy)]
pub(crate) struct Placed {
  /// The module's node in the tree.
  pub(crate) module: u32,
  pub(crate) source: SourceId,
  /// Its index among its source's items, in [`Collected::items`].
  pub(crate) item: u32,
}

/// A type or directive definition the walk found.
pub(crate) struct Found<'d> {
  /// Where it is.
  pub(crate) placed: Placed,
  pub(crate) kind: DefKind,
  /// Its name, as written.
  pub(crate) name: &'d [u8],
  /// Where its name is written.
  pub(crate) location: Location,
  /// From its description, or its first keyword, to its last token.
  pub(crate) extent: Location,
  /// Its description, as written, quotes included, and where it is written.
  pub(crate) description: Option<(&'d [u8], Location)>,
}

/// Finds every module of `packages`, which are in id order and numbered by `sources`, and
/// everything each declares: each package's root module, its directory modules and the modules
/// its files' inline `mod`s declare, reporting what is refused on the way, and each module's
/// items.
pub(crate) fn items<'d, 'a: 'd>(
  packages: &'d [PackageView<'a>],
  sources: &SourceMap,
  documents: &'d Documents<'a>,
  limits: &Limits,
  reporter: &mut Reporter,
) -> Collected<'d, 'a> {
  let mut walk = Walk {
    tree: Tree::new(),
    reporter,
    max_depth: limits.max_module_depth(),
    package_name: b"",
    package_source: Location::entire(0),
    widths: Widths::new(),
    definitions: Vec::new(),
    uses: Vec::new(),
    extensions: Vec::new(),
    schemas: Vec::new(),
    unknown: 0,
  };
  let mut items: Vec<Option<Items<'d, 'a>>> = Vec::new();
  items.resize_with(sources.len() as usize, || None);
  for (index, package) in packages.iter().enumerate() {
    let range = sources.package_sources(index);
    walk.package_name = package.name;
    walk.package_source = Location::entire(range.start());
    walk.widths.package(package.integers);
    let root = walk.tree.root(index);

    // Every file is placed before any is walked, so that a `mod` meets every directory module of
    // its package, wherever the directory's files are in source order.
    let mut placed: Vec<(SourceId, u32)> = Vec::with_capacity(package.files.len());
    let mut reported: Option<&'d [u8]> = None;
    for (offset, file) in package.files.iter().enumerate() {
      if refused(&package.files, offset) {
        continue;
      }
      let module = match package.layout {
        Layout::Directory => match walk.directory(root, file.path, &mut reported) {
          Some(module) => module,
          None => continue,
        },
        Layout::Flat => root,
      };
      placed.push((SourceId::new(range.start() + 1 + dense(offset)), module));
    }

    for (source, module) in placed {
      let slot = source.get() as usize;
      match documents.get(source) {
        Some(Parsed::Text(document)) => {
          items[slot] = Some(Items::Text(walk.document(document, source, module)));
        }
        Some(Parsed::Bytes(document)) => {
          items[slot] = Some(Items::Bytes(walk.document(document, source, module)));
        }
        None => {}
      }
    }
  }
  let (written, unknown) = walk.widths.finish();
  debug_assert_eq!(
    walk.unknown.saturating_add(unknown),
    0,
    "a parsed document holds an item of a kind the walk does not know, so what it declares is lost"
  );
  Collected {
    tree: walk.tree,
    items,
    definitions: walk.definitions,
    uses: walk.uses,
    extensions: walk.extensions,
    schemas: walk.schemas,
    written,
  }
}

/// Whether the package set refused the path of the file at `at` of `files`, which are in source
/// order: a path the path rule refuses, or a path another file of the package has.
fn refused(files: &[FileView<'_>], at: usize) -> bool {
  let path = files[at].path;
  check::path_problem(path).is_some()
    || (at > 0 && files[at - 1].path == path)
    || files
      .get(at + 1)
      .is_some_and(|next: &FileView<'_>| next.path == path)
}

/// One list of items being walked, a document's or a `mod`'s, and the module they are in.
struct Frame<'d, S> {
  items: &'d [TypeSystemItem<S>],
  /// The index of the next item to walk.
  next: usize,
  /// The module's node.
  module: u32,
}

/// What the walk keeps: the tree it grows, where it reports, the package it is in, and what it has
/// found.
struct Walk<'d, 'r> {
  tree: Tree<'d>,
  reporter: &'r mut Reporter,
  /// `Limits::max_module_depth`.
  max_depth: u32,
  /// The name of the package being walked, which messages quote.
  package_name: &'d [u8],
  /// The whole of that package's package source, where its directories are reported.
  package_source: Location,
  /// The widths the walked items write.
  widths: Widths<'d>,
  definitions: Vec<Found<'d>>,
  uses: Vec<Placed>,
  extensions: Vec<Placed>,
  schemas: Vec<Placed>,
  /// How many items of a kind it does not know the walk met.
  unknown: u32,
}

impl<'d> Walk<'d, '_> {
  /// The module of a `Directory` package's file at `path`: the module of the directory it is in,
  /// added to the tree with every directory on the way.
  ///
  /// `None`, and reported at the package source, when a directory on the way is not a Name or sits
  /// deeper than the limit. The package's files are in source order, which keeps the files under one
  /// directory together, so `reported`, the directory reported last, is what reports each once.
  fn directory(
    &mut self,
    root: u32,
    path: &'d [u8],
    reported: &mut Option<&'d [u8]>,
  ) -> Option<u32> {
    let mut module = root;
    let mut start: usize = 0;
    let mut depth: u32 = 0;
    // Every segment before the last `/` is a directory, and the last segment names the file.
    while let Some(offset) = path[start..].iter().position(|byte: &u8| *byte == b'/') {
      let end = start + offset;
      let name: &'d [u8] = &path[start..end];
      let directory: &'d [u8] = &path[..end];
      depth = depth.saturating_add(1);
      let named = check::is_name(name);
      if !named || depth > self.max_depth {
        if *reported != Some(directory) {
          *reported = Some(directory);
          if named {
            self.deep_directory(directory, depth);
          } else {
            self.unnamed_directory(directory, name);
          }
        }
        return None;
      }
      module = self.tree.directory(module, name, directory);
      start = end + 1;
    }
    Some(module)
  }

  /// Walks the items of `document`, the file `source`, which is placed in the module `module`;
  /// answers them, flattened in the order walked.
  fn document<S: AsRef<[u8]>>(
    &mut self,
    document: &'d TypeSystemDocument<S>,
    source: SourceId,
    module: u32,
  ) -> Vec<&'d TypeSystemItem<S>> {
    let mut walked: Vec<&'d TypeSystemItem<S>> = Vec::new();
    let mut stack: Vec<Frame<'d, S>> = Vec::new();
    stack.push(Frame {
      items: document.definitions(),
      next: 0,
      module,
    });
    loop {
      let Some(frame) = stack.last_mut() else {
        break;
      };
      let items: &'d [TypeSystemItem<S>] = frame.items;
      let next: usize = frame.next;
      let module: u32 = frame.module;
      let Some(item) = items.get(next) else {
        stack.pop();
        continue;
      };
      frame.next = next + 1;
      let placed = Placed {
        module,
        source,
        item: dense(walked.len()),
      };
      walked.push(item);
      match item {
        TypeSystemItem::Mod(declaration) => {
          if let Some(child) = self.declare(declaration, source, module) {
            stack.push(Frame {
              items: declaration.items(),
              next: 0,
              module: child,
            });
          }
        }
        TypeSystemItem::Definition(described) => self.definition(described, placed),
        TypeSystemItem::Extension(extension) => {
          self.widths.extension(extension);
          match extension {
            TypeSystemExtension::Schema(_) => self.schemas.push(placed),
            TypeSystemExtension::Type(_) | TypeSystemExtension::Directive(_) => {
              self.extensions.push(placed);
            }
          }
        }
        TypeSystemItem::Use(declaration) => {
          self.widths.use_item(declaration);
          self.uses.push(placed);
        }
        // `TypeSystemItem` is `#[non_exhaustive]`, and an item of a kind this walk does not know
        // may declare what it cannot place. It is counted, and a debug build refuses the count once
        // the walk ends, so a parser that grows a kind fails every analysis test until the walk
        // places it.
        _ => self.unknown = self.unknown.saturating_add(1),
      }
    }
    walked
  }

  /// Keeps the definition `described`, at `placed`: a type or a directive, or the schema.
  fn definition<S: AsRef<[u8]>>(
    &mut self,
    described: &'d DescribedTypeSystemDefinition<S>,
    placed: Placed,
  ) {
    self.widths.definition(described.node());
    let (kind, name): (DefKind, &'d Name<S>) = match described.node() {
      TypeSystemDefinition::Type(definition) => match definition {
        TypeDefinition::Scalar(scalar) => (DefKind::Scalar, scalar.name().name()),
        TypeDefinition::Object(object) => (DefKind::Object, object.name().name()),
        TypeDefinition::Interface(interface) => (DefKind::Interface, interface.name().name()),
        TypeDefinition::Union(union) => (DefKind::Union, union.name().name()),
        TypeDefinition::Enum(enumeration) => (DefKind::Enum, enumeration.name().name()),
        TypeDefinition::InputObject(input) => (DefKind::InputObject, input.name().name()),
        // `TypeDefinition` is `#[non_exhaustive]`: a kind of definition this walk does not know is
        // counted, as an unknown item is, and a debug build refuses the count.
        _ => {
          self.unknown = self.unknown.saturating_add(1);
          return;
        }
      },
      TypeSystemDefinition::Directive(directive) => (DefKind::Directive, directive.name().name()),
      TypeSystemDefinition::Schema(_) => {
        self.schemas.push(placed);
        return;
      }
    };
    let source = placed.source.get();
    let description: Option<(&'d [u8], Location)> =
      described
        .description()
        .map(|text: &'d StringValue<S>| -> (&'d [u8], Location) {
          (text.source().as_ref(), Location::new(source, *text.span()))
        });
    self.definitions.push(Found {
      placed,
      kind,
      name: name.source().as_ref(),
      location: Location::new(source, *name.as_span()),
      extent: Location::new(source, *described.span()),
      description,
    });
  }

  /// The module the inline `mod` `declaration`, written in the file `source` inside the module
  /// `parent`, declares.
  ///
  /// `None` when that module would sit deeper than the limit: it is reported, and nothing inside it
  /// is walked. A module the package already has is declared again, which is reported, and the
  /// items are walked into that one module all the same.
  fn declare<S: AsRef<[u8]>>(
    &mut self,
    declaration: &'d ModDefinition<S>,
    source: SourceId,
    parent: u32,
  ) -> Option<u32> {
    let name: &'d [u8] = declaration.name().source().as_ref();
    let at = Location::new(source.get(), *declaration.name().as_span());
    let depth = self.tree.node(parent).path.len().saturating_add(1);
    if depth > self.max_depth as usize {
      self.deep_module(at, name, depth);
      return None;
    }
    if let Some(found) = self.tree.child(parent, name) {
      self.declared_twice(at, name, found);
      return Some(found);
    }
    let description: Option<(&'d [u8], Location)> =
      declaration
        .description()
        .map(|text: &'d StringValue<S>| -> (&'d [u8], Location) {
          (
            text.source().as_ref(),
            Location::new(source.get(), *text.span()),
          )
        });
    let declared = Declared {
      location: at,
      extent: Location::new(source.get(), *declaration.span()),
      description,
    };
    Some(self.tree.declare(parent, name, declared))
  }

  /// Reports the directory `directory`, whose last segment `name` is not a Name.
  fn unnamed_directory(&mut self, directory: &[u8], name: &[u8]) {
    let kind = DiagnosticKind::InvalidDirectoryName;
    self.reporter.report(
      Diagnostic::builder(kind.code(), self.package_source)
        .severity(kind.severity())
        .message(format!(
          "package `{}` holds the directory `{}`, which cannot be a module: `{}` is not a GraphQLx \
           name",
          Excerpt(self.package_name),
          Excerpt(directory),
          Excerpt(name)
        ))
        .primary_label(kind.title())
        .help(
          "a directory that is a module is named as a name is: a letter or `_`, then letters, \
           digits or `_`; rename it, or give the package the `Flat` layout",
        )
        .build(),
    );
  }

  /// Reports the directory `directory`, whose module would sit `depth` deep, past the limit.
  fn deep_directory(&mut self, directory: &[u8], depth: u32) {
    let kind = DiagnosticKind::ResourceLimit;
    let max = self.max_depth;
    self.reporter.report(
      Diagnostic::builder(kind.code(), self.package_source)
        .severity(kind.severity())
        .message(format!(
          "package `{}` holds the directory `{}`, whose module would sit {depth} deep, past the \
           limit of {max}",
          Excerpt(self.package_name),
          Excerpt(directory)
        ))
        .primary_label(kind.title())
        .help("raise `Limits::max_module_depth`, or nest the directories less deeply")
        .build(),
    );
  }

  /// Reports the `mod` named `name` at `at`, whose module would sit `depth` deep, past the limit.
  fn deep_module(&mut self, at: Location, name: &[u8], depth: usize) {
    let kind = DiagnosticKind::ResourceLimit;
    let max = self.max_depth;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(format!(
          "`mod {}` declares a module that would sit {depth} deep, past the limit of {max}",
          Excerpt(name)
        ))
        .primary_label(kind.title())
        .help("raise `Limits::max_module_depth`, or nest the modules less deeply")
        .build(),
    );
  }

  /// Reports the `mod` named `name` at `at`, which declares the module `first` again.
  fn declared_twice(&mut self, at: Location, name: &[u8], first: u32) {
    let kind = DiagnosticKind::ModuleDefinedTwice;
    let node = self.tree.node(first);
    let (declared, directory) = (node.declared, node.directory);
    let mut builder = Diagnostic::builder(kind.code(), at)
      .severity(kind.severity())
      .message(format!(
        "`mod {}` declares a module its package already has",
        Excerpt(name)
      ))
      .primary_label(kind.title());
    builder = match (declared, directory) {
      (Some(declared), _) => builder.label(declared.location, "first declared here"),
      (None, Some(directory)) => builder.label(
        self.package_source,
        format!("the directory `{}` is this module", Excerpt(directory)),
      ),
      // A root module has no name for a `mod` to repeat.
      (None, None) => builder,
    };
    self.reporter.report(
      builder
        .help(
          "a module is declared once, by its directory or by one `mod`; move these items there \
           and remove this `mod`, or rename it",
        )
        .build(),
    );
  }
}
