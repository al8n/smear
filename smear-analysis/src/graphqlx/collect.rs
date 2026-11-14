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
//! a Name, and when a directory on its way sits deeper than `Limits::max_module_depth` or would
//! add a module past `Limits::max_modules`. Such a file is parsed all the same, so its syntax
//! errors are reported, but nothing it declares is collected.
//!
//! Every module the model will hold counts against `Limits::max_modules`: every package's root
//! module, all of which are in the tree before the walk starts and are held whatever the limit, and
//! each directory module and each `mod`'s module as the walk adds it. The first module a supplied
//! package's directory or `mod` would add past the limit is reported, and the walk adds no module
//! after it, refusing it as it refuses one past the depth limit; what sits in a module already held
//! is still collected. A `mod` of a module the package already has adds none.
//!
//! The walk counts every definition the model will hold as it meets it, against
//! `Limits::max_definitions`: each type and directive definition but one that takes a GraphQL
//! built-in's place, and each primitive the first time a package writes it, or a root package's
//! definition makes it. The built-in library counts, and is always collected whole. The first
//! definition a supplied package would add past the limit is reported, and the walk collects
//! nothing after it: no item of the rest of its file, nor of any file after it.
//!
//! An item of a kind the walk does not know, or one holding a part the width pass does not know,
//! which only a parser newer than the walk can make, is reported once per file, at the first such
//! item, in every build; a debug build also refuses it once the walk ends.
//!
//! The walk is written generic over the slice type of a parsed document, and nothing it does
//! depends on which parser made a document. Every document it is handed is one of text: a byte text
//! that is not UTF-8 has none.

use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  vec::Vec,
};

use smear_parser::graphqlx::ast::{
  DescribedTypeSystemDefinition, ModDefinition, Name, StringValue, TypeDefinition,
  TypeSystemDefinition, TypeSystemDocument, TypeSystemExtension, TypeSystemItem,
};
use tokora::{diagnostic::Location, span::AsSpan};

use super::{
  builtin::{Replaced, namespace, replaceable, replaced, spelled},
  check,
  code::DiagnosticKind,
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{SourceId, dense},
  limits::Limits,
  model::{DefKind, Primitive},
  package::Layout,
  parse::Documents,
  set::{FileView, PackageView, ROOT},
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
  /// Every primitive a placed file writes where its package may have it, or a root package's
  /// definition makes, keyed by its place among `core`'s materialised definitions.
  pub(crate) written: BTreeMap<(u8, u32), Primitive>,
}

/// One document's items, flattened. Every document is one of text: a byte text that is not UTF-8
/// has none.
pub(crate) enum Items<'d, 'a> {
  /// A document of text.
  Text(Vec<&'d TypeSystemItem<&'a str>>),
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
  /// The GraphQL built-in whose place it takes, when it is the root package's definition of that
  /// built-in's kind and name, the first of its name in its namespace in the root package's root
  /// module.
  pub(crate) replaces: Option<Replaced>,
}

/// A definition the definition limit counts.
enum Counted<'n> {
  /// A type or a directive, of its kind, spelled so.
  Definition(DefKind, &'n [u8]),
  /// A primitive's, which `core` makes.
  Primitive(Primitive),
}

/// What would add a module the module limit refuses.
enum Adding<'n> {
  /// A directory of the package being walked, as the file paths spell it.
  Directory(&'n [u8]),
  /// A `mod` named so, naming its module at the location.
  Mod(Location, &'n [u8]),
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
    max_modules: limits.max_modules(),
    modules_full: false,
    package: 0,
    package_name: b"",
    package_source: Location::entire(0),
    root: 0,
    graphql_names: BTreeSet::new(),
    defined: 0,
    max_definitions: limits.max_definitions(),
    full: false,
    widths: Widths::new(),
    definitions: Vec::new(),
    uses: Vec::new(),
    extensions: Vec::new(),
    schemas: Vec::new(),
    unknown: 0,
  };
  let mut items: Vec<Option<Items<'d, 'a>>> = Vec::new();
  items.resize_with(sources.len() as usize, || None);
  // Every package's root module is held whatever the module limit, so all of them are in the tree,
  // and counted, before the first module the limit may refuse.
  for index in 0..packages.len() {
    walk.tree.root(index);
  }
  for (index, package) in packages.iter().enumerate() {
    let range = sources.package_sources(index);
    let root = walk.tree.root_of(index);
    walk.package = index;
    walk.package_name = package.name;
    walk.package_source = Location::entire(range.start());
    walk.root = root;
    walk.widths.package(package.integers);

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
      // Past the definition limit nothing more is collected.
      if walk.full {
        break;
      }
      let slot = source.get() as usize;
      if let Some(document) = documents.get(source) {
        items[slot] = Some(Items::Text(walk.document(document, source, module)));
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
/// order: a path the path rule refuses, or a path another file of the package has. Name
/// resolution asks it too, so that its file table holds the files this walk places and no other.
pub(crate) fn refused(files: &[FileView<'_>], at: usize) -> bool {
  let path = files[at].path;
  check::path_problem(path).is_some()
    || (at > 0 && files[at - 1].path == path)
    || files
      .get(at + 1)
      .is_some_and(|next: &FileView<'_>| next.path == path)
}

/// Reports that a file holds an item, or a part of one, of a kind this analyzer does not know, the
/// first of them at `at`: only a parser newer than the analyzer makes one, and what it holds is in
/// no model. The walk reports it once per file, and so does lowering.
pub(crate) fn unknown_item(reporter: &mut Reporter, at: Location) {
  let kind = DiagnosticKind::UnknownItem;
  reporter.report(
    Diagnostic::builder(kind.code(), at)
      .severity(kind.severity())
      .message(
        "this file holds an item, or a part of one, of a kind this analyzer does not know, so \
         what it declares is not in the model",
      )
      .primary_label(kind.title())
      .help(
        "the parser that read this file is newer than this analyzer; analyze it with a release \
         of the analyzer that knows every kind of item the parser reads",
      )
      .build(),
  );
}

/// One list of items being walked, a document's or a `mod`'s, and the module they are in.
struct Frame<'d, S> {
  items: &'d [TypeSystemItem<S>],
  /// The index of the next item to walk.
  next: usize,
  /// The module's node.
  module: u32,
}

/// What the walk keeps: the tree it grows, where it reports, the package it is in, what it counts
/// against the definition limit, and what it has found.
struct Walk<'d, 'r> {
  tree: Tree<'d>,
  reporter: &'r mut Reporter,
  /// `Limits::max_module_depth`.
  max_depth: u32,
  /// `Limits::max_modules`.
  max_modules: u32,
  /// Whether the walk refused a module past the module limit, after which it adds none.
  modules_full: bool,
  /// The index of the package being walked.
  package: usize,
  /// The name of the package being walked, which messages quote.
  package_name: &'d [u8],
  /// The whole of that package's package source, where its directories are reported.
  package_source: Location,
  /// That package's root module's node.
  root: u32,
  /// The names of GraphQL's built-ins that the root package's root module has defined so far, each
  /// in its namespace: only the first definition of each such name replaces a built-in.
  graphql_names: BTreeSet<(u8, &'d [u8])>,
  /// How many definitions the model will hold so far.
  defined: u32,
  /// `Limits::max_definitions`.
  max_definitions: u32,
  /// Whether the walk refused a definition past the limit, after which it collects nothing.
  full: bool,
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
  /// `None` too when a directory on the way would add a module past the module limit.
  ///
  /// Every directory module is added before any `mod` is walked, so a child the tree already has
  /// here is a directory module too.
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
      module = match self.tree.child(module, name) {
        Some(found) => found,
        None => {
          if !self.module_room(Adding::Directory(directory)) {
            return None;
          }
          self.tree.directory(module, name, directory)
        }
      };
      start = end + 1;
    }
    Some(module)
  }

  /// Whether the tree may take one more module, which the package being walked would add: always
  /// for a built-in package, and for a supplied package while the tree holds fewer modules than the
  /// limit and none was refused before. The first module refused is reported, at what would have
  /// added it, and every module after it is refused unreported.
  fn module_room(&mut self, adding: Adding<'_>) -> bool {
    if self.package < ROOT {
      return true;
    }
    if !self.modules_full && self.tree.count() < self.max_modules {
      return true;
    }
    if !self.modules_full {
      self.modules_full = true;
      self.past_module_limit(adding);
    }
    false
  }

  /// Walks the items of `document`, the file `source`, which is placed in the module `module`;
  /// answers them, flattened in the order walked.
  fn document<S: AsRef<[u8]>>(
    &mut self,
    document: &'d TypeSystemDocument<S>,
    source: SourceId,
    module: u32,
  ) -> Vec<&'d TypeSystemItem<S>> {
    self.widths.source(source.get());
    let mut walked: Vec<&'d TypeSystemItem<S>> = Vec::new();
    let mut stack: Vec<Frame<'d, S>> = Vec::new();
    stack.push(Frame {
      items: document.definitions(),
      next: 0,
      module,
    });
    // Where the first item of this file that holds a node of a kind the walk does not know is.
    let mut unknown_at: Option<Location> = None;
    while let Some(frame) = stack.last_mut() {
      // Past the definition limit nothing more is collected.
      if self.full {
        break;
      }
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
      let before = self.unknown_nodes();
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
          self.admit_written();
          match extension {
            TypeSystemExtension::Schema(_) => self.schemas.push(placed),
            TypeSystemExtension::Type(_) | TypeSystemExtension::Directive(_) => {
              self.extensions.push(placed);
            }
          }
        }
        TypeSystemItem::Use(declaration) => {
          self.widths.use_item(declaration);
          self.admit_written();
          self.uses.push(placed);
        }
        // `TypeSystemItem` is `#[non_exhaustive]`, and an item of a kind this walk does not know
        // may declare what it cannot place. It is counted, and reported below, once per file; and a
        // debug build refuses the count once the walk ends, so a parser that grows a kind fails
        // every analysis test until the walk places it.
        _ => self.unknown = self.unknown.saturating_add(1),
      }
      // An item's span is the parser's to give whatever its kind, so it is where the report goes.
      if unknown_at.is_none() && self.unknown_nodes() != before {
        unknown_at = Some(Location::new(source.get(), *item.span()));
      }
    }
    if let Some(at) = unknown_at {
      unknown_item(self.reporter, at);
    }
    walked
  }

  /// How many nodes of a kind it does not know the walk, and its width pass, have met so far.
  fn unknown_nodes(&self) -> u32 {
    self.unknown.saturating_add(self.widths.unknown())
  }

  /// Keeps the definition `described`, at `placed`: a type or a directive, when the model may hold
  /// it, or the schema.
  fn definition<S: AsRef<[u8]>>(
    &mut self,
    described: &'d DescribedTypeSystemDefinition<S>,
    placed: Placed,
  ) {
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
        self.widths.definition(described.node());
        self.admit_written();
        self.schemas.push(placed);
        return;
      }
    };
    let source = placed.source.get();
    let spelling: &'d [u8] = name.source().as_ref();
    let location = Location::new(source, *name.as_span());
    let replaces = self.replaces(kind, spelling, placed.module);
    if !self.admit(kind, spelling, location, replaces) {
      return;
    }
    self.widths.definition(described.node());
    self.admit_written();
    let description: Option<(&'d [u8], Location)> =
      described
        .description()
        .map(|text: &'d StringValue<S>| -> (&'d [u8], Location) {
          (text.source().as_ref(), Location::new(source, *text.span()))
        });
    self.definitions.push(Found {
      placed,
      kind,
      name: spelling,
      location,
      extent: Location::new(source, *described.span()),
      description,
      replaces,
    });
  }

  /// What the definition `spelling` of `kind`, in the module `module`, replaces: a GraphQL
  /// built-in, when it is the first definition of its name in its namespace in the root package's
  /// root module, and of the built-in's kind.
  fn replaces(&mut self, kind: DefKind, spelling: &'d [u8], module: u32) -> Option<Replaced> {
    let space = namespace(kind);
    if self.package != ROOT
      || module != self.root
      || !replaceable(space, spelling)
      || !self.graphql_names.insert((space, spelling))
    {
      return None;
    }
    replaced(kind, spelling)
  }

  /// Counts the definition `spelling` of `kind`, named at `location`, which replaces `replaces`:
  /// `true` when the model may hold it. A definition that takes a built-in's place adds no
  /// definition to the model, unless the built-in is a primitive no package has written yet, which
  /// it makes.
  fn admit(
    &mut self,
    kind: DefKind,
    spelling: &[u8],
    location: Location,
    replaces: Option<Replaced>,
  ) -> bool {
    match replaces {
      Some(Replaced::Built(..)) => true,
      Some(Replaced::Primitive(primitive)) => {
        if self.widths.wrote(primitive) {
          return true;
        }
        let made = self.room(location, Counted::Definition(kind, spelling));
        if made {
          self.widths.make(primitive);
        }
        made
      }
      None => self.room(location, Counted::Definition(kind, spelling)),
    }
  }

  /// Counts each primitive the item just read wrote first, in the order it wrote them, as the
  /// definition `core` makes for it; one the model may not hold is taken back.
  fn admit_written(&mut self) {
    for (primitive, location) in self.widths.fresh() {
      if !self.room(location, Counted::Primitive(primitive)) {
        self.widths.unwrite(primitive);
      }
    }
  }

  /// Counts one more definition the model will hold, named at `location`: `true` when the model may
  /// hold it. A built-in package's definitions always may, as the built-in library is collected
  /// whole; a supplied package's may while the model holds fewer than the limit. The first that may
  /// not is reported, and after it the walk collects nothing.
  fn room(&mut self, location: Location, counted: Counted<'_>) -> bool {
    if self.full {
      return false;
    }
    if self.package >= ROOT && self.defined >= self.max_definitions {
      self.full = true;
      self.past_limit(location, counted);
      return false;
    }
    self.defined = self.defined.saturating_add(1);
    true
  }

  /// The module the inline `mod` `declaration`, written in the file `source` inside the module
  /// `parent`, declares.
  ///
  /// `None` when that module would sit deeper than the depth limit, or be one module past the
  /// module limit: nothing inside it is walked. A module the package already has is declared
  /// again, which is reported, and adds no module: the items are walked into that one module all
  /// the same.
  fn declare<S: AsRef<[u8]>>(
    &mut self,
    declaration: &'d ModDefinition<S>,
    source: SourceId,
    parent: u32,
  ) -> Option<u32> {
    let name: &'d [u8] = declaration.name().source().as_ref();
    let at = Location::new(source.get(), *declaration.name().as_span());
    let depth = self.tree.node(parent).depth.saturating_add(1);
    if depth > self.max_depth {
      self.deep_module(at, name, depth);
      return None;
    }
    if let Some(found) = self.tree.child(parent, name) {
      self.declared_twice(at, name, found);
      return Some(found);
    }
    if !self.module_room(Adding::Mod(at, name)) {
      return None;
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

  /// Reports the first module past the module limit, which `adding` would have added: a directory
  /// at its package's package source, a `mod` where it names its module.
  fn past_module_limit(&mut self, adding: Adding<'_>) {
    let kind = DiagnosticKind::ResourceLimit;
    let max = self.max_modules;
    let (at, message) = match adding {
      Adding::Directory(directory) => (
        self.package_source,
        format!(
          "package `{}` holds the directory `{}`, whose module would pass the limit of {max} \
           modules the model may hold, so the files under it are placed in no module, and no \
           module after it is declared",
          Excerpt(self.package_name),
          Excerpt(directory)
        ),
      ),
      Adding::Mod(at, name) => (
        at,
        format!(
          "`mod {}` declares a module that would pass the limit of {max} modules the model may \
           hold, so nothing inside it is collected, and no module after it is declared",
          Excerpt(name)
        ),
      ),
    };
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help("raise `Limits::max_modules`, or use fewer directories and `mod`s")
        .build(),
    );
  }

  /// Reports `counted`, named at `at`, the first definition past the definition limit.
  fn past_limit(&mut self, at: Location, counted: Counted<'_>) {
    let kind = DiagnosticKind::ResourceLimit;
    let max = self.max_definitions;
    let what = match counted {
      Counted::Definition(DefKind::Directive, name) => {
        format!("the directive `@{}`", Excerpt(name))
      }
      Counted::Definition(_, name) => format!("the type `{}`", Excerpt(name)),
      Counted::Primitive(primitive) => {
        format!("the primitive `{}` written here", spelled(primitive))
      }
    };
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(format!(
          "{what} would pass the limit of {max} definitions the model may hold, so neither it nor \
           anything after it is declared"
        ))
        .primary_label(kind.title())
        .help("raise `Limits::max_definitions`, or declare fewer definitions")
        .build(),
    );
  }

  /// Reports the `mod` named `name` at `at`, whose module would sit `depth` deep, past the limit.
  fn deep_module(&mut self, at: Location, name: &[u8], depth: u32) {
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
