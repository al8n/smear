//! Name resolution: every module's scope, and the one call that resolves a path where it stands.
//!
//! # A module's scope
//!
//! The first segment of a path with no root is looked up in this order, and the first hit decides:
//!
//! 1. the generic parameters of the definition the path stands in, which the caller passes, where
//!    the segment is wanted as a type or a module, never alone as a directive's name: a parameter
//!    is a type, so it answers a lone segment where a type is wanted, and refuses one where a
//!    module is wanted or segments follow it, a directive's path included;
//! 2. the module's own declarations and its child modules;
//! 3. the module's explicit `use` bindings: named items and aliases;
//! 4. the module's glob `use` bindings: each glob gives the declarations and the child modules of
//!    the module it names, and a name two globs give to two different items is an error at the
//!    path that meets it;
//! 5. the prelude: the declarations of the root modules of `core`, `alloc` and `std`, whatever the
//!    package's tier, which hold the primitives the program writes and the second names `Int`,
//!    `Float`, `Boolean`, `String` and `ID`; and `NonZeroIN` and `NonZeroUN`, recognised by
//!    pattern, which name `core`'s `NonZero` of that primitive whatever `NonZero` names where they
//!    are written;
//! 6. last, and only where a module is wanted, which is where more segments follow or where the
//!    path is the whole of a `use` item, the names of the packages the module's package may reach:
//!    `core`, `alloc` and `std`, and its declared dependencies, never a dependency's own dependency
//!    and never its own name.
//!
//! Types and modules share one namespace, and directives have their own: a type parameter, a type
//! and a module of one name stand in each other's way, and a directive stands in none's. A later
//! segment is looked up among the declarations and the child modules of the module the segments
//! before it name, and nowhere else: what a module's `use` items bind is the module's own, and is
//! never re-exported. How the `use` items are resolved is [`uses`]'s.
//!
//! A primitive resolves only in a package that may have its width, `core`'s widths 8 to 128 or a
//! width its integer provider gives, however the path reaches it.
//!
//! # Roots
//!
//! `~::` starts at the root module of the package the path is written in; `^::name` at the root
//! module of a package that package may reach, which no name of any scope shadows; and each `..::`
//! one module up, where `..::` above a root module names nothing.
//!
//! # What resolution costs
//!
//! Nothing of a scope is copied. A module's scope is its own declarations and child modules,
//! found by binary search in the tables the declarations hand on; its explicit `use` bindings and
//! its globs, found by range in two ordered maps keyed by the module first; and the prelude's and
//! the packages' tables, which every module shares. So a module costs its entry and its row among
//! its parent's children, each name a `use` binds one row, each module its globs name one row,
//! however many globs name it, and nothing about it grows with how deep it sits or what its parent
//! holds. A lookup costs a logarithm of those tables, and for the globs one search in each module
//! the module's globs name, which `Limits::max_globs` bounds; a path costs one lookup per segment.
//!
//! # Near misses
//!
//! A name that does not resolve gets the nearest name visible where it is written, of the
//! namespace wanted, as a "did you mean" in its help, within one budget for the whole analysis:
//! see [`near`]. [`Scopes::nearest`] answers lowering the near miss of a name among candidates it
//! holds, under the same budget and the same rules.

mod near;
mod uses;

use core::cell::Cell;
use std::{boxed::Box, collections::BTreeMap, format, string::String, vec, vec::Vec};

use smear_parser::graphqlx::ast::{Path, PathRoot};
use tokora::{
  SimpleSpan,
  diagnostic::{Code, Location},
  span::AsSpan,
};

use super::{
  builtin::{CORE, DIRECTIVE, TYPE, available, intrinsic, spelled, written},
  code::{DiagnosticKind, schema},
  declare::{Binding, Child, Declarations, ModuleEntry},
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{DefId, ModuleId, PackageId, SourceId, Sym, dense},
  limits::Limits,
  model::{ModelBuilder, ModuleKind, Primitive},
  package::{IntegerProvider, Layout},
  set::{PackageView, ROOT},
  source::SourceMap,
};

/// Every module's scope, and what resolves a path written in one.
pub(crate) struct Scopes<'x> {
  /// Each module's declarations: the hand-over's bindings, sorted by module, namespace and name.
  bindings: &'x [Binding],
  /// Each module, by id.
  modules: &'x [ModuleEntry],
  /// Each module but a root module, sorted by its parent and its name.
  children: &'x [Child],
  /// Each package's root module, by the package's index.
  roots: &'x [ModuleId],
  /// Each primitive `core` materialises, with its definition, in `DefId` order, which is the
  /// order of their ranks.
  primitives: &'x [(Primitive, DefId)],
  /// Each package, by index.
  packages: Vec<PackageScope<'x>>,
  /// `core`'s `NonZero`, which `NonZeroIN` and `NonZeroUN` name.
  non_zero: Option<DefId>,
  /// Every name an explicit `use` binding binds: by module, namespace and name, then by the `use`
  /// item, in the declarations' order, and the order the item binds its names in. The first of a
  /// key's run is the one a lookup answers.
  explicit: BTreeMap<ExplicitKey, Bound>,
  /// Every glob, one row for each module and module its globs name, however many globs name it:
  /// at most `Limits::max_globs` rows a module.
  globs: BTreeMap<(ModuleId, ModuleId), Glob>,
  /// The directory module of each file of a `Directory` package, by source id: where a string
  /// source written in the file starts. `None` for a package source, a file of a `Flat` package,
  /// and a file placed in no module.
  directories: Vec<Option<ModuleId>>,
  /// Each file of a `Directory` package that sits in a directory module, by that module, then by
  /// the file's name: what tells a string source that names a file from one that names nothing.
  files: Vec<FileName<'x>>,
  /// `Limits::max_path_segments`.
  max_segments: u32,
  /// `Limits::max_use_depth`.
  max_use_depth: u32,
  /// The near-miss comparisons the analysis may still spend, of
  /// `Limits::max_near_miss_comparisons`: spent by every search, the `use` items' and lowering's.
  near: Cell<u32>,
}

/// What orders the explicit `use` bindings: module, namespace, name, `use` item, and the order the
/// item binds its names in.
type ExplicitKey = (ModuleId, u8, Sym, u32, u32);

/// What an explicit `use` binding binds, and where the name it binds is written.
#[derive(Clone, Copy)]
struct Bound {
  target: Target,
  at: Location,
}

/// The globs of one module that name one module: where the first of them is written, the `use`
/// item that made it, and whether another item made one too. The row answers every lookup but
/// those of an item that is its only maker, whose own globs never answer its own head.
#[derive(Clone, Copy)]
struct Glob {
  at: Location,
  item: u32,
  shared: bool,
}

/// One package, as name resolution reads it.
struct PackageScope<'x> {
  /// Its name, as given.
  name: &'x [u8],
  /// How its files become modules: a string source names a directory only in the `Directory`
  /// layout.
  layout: Layout,
  /// Its integer provider, which decides the widths beyond `core`'s it may have.
  integers: Option<&'x IntegerProvider>,
  /// The supplied packages it declares and may reach, by name, each with its root module. The
  /// built-in packages it reaches without declaring them.
  dependencies: Vec<(&'x [u8], ModuleId)>,
  /// Its package source's id, which its files' ids follow.
  source: u32,
}

/// A file of a `Directory` package, as a string source that names it is told by: the module of
/// its directory, its name, the bytes after its path's last `/`, and its directory's path, the
/// bytes before that `/`, which the help quotes. Both are slices of the file's own path.
#[derive(Clone, Copy)]
struct FileName<'x> {
  module: ModuleId,
  name: &'x [u8],
  directory: &'x [u8],
}

/// What a path is wanted to name.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Wanted {
  /// A type: a field's, an argument's or an input field's type, a type argument, a union member,
  /// an `implements` entry, a root operation, an extension's target. A type parameter answers it.
  Type,
  /// A directive: an application's name, or a directive extension's target.
  Directive,
  /// A module: every segment but the last.
  Module,
}

impl Wanted {
  /// The namespace what is wanted is named in: a module's is the type namespace.
  fn namespace(self) -> u8 {
    match self {
      Self::Directive => DIRECTIVE,
      Self::Type | Self::Module => TYPE,
    }
  }

  /// What is wanted, as a message names it.
  fn noun(self) -> &'static str {
    match self {
      Self::Type => "type",
      Self::Directive => "directive",
      Self::Module => "module",
    }
  }
}

/// Where a path is written, which decides the code it is refused with when it does not resolve: a
/// single segment with no root, where GraphQL has a place of this kind, keeps GraphQL's code.
// Lowering, the next part of the analyzer, passes each of these; until it lands only the
// test-support door passes one.
#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Position {
  /// A field's, an argument's or an input field's type, a type argument, a default or a bound:
  /// `smear::schema::undefined-type`.
  Type,
  /// A directive application's name: `smear::schema::undefined-directive`.
  Directive,
  /// An extension's target: `smear::schema::undefined-extension-target`.
  ExtensionTarget,
  /// An `implements` entry: `smear::schema::undefined-implements-interface`.
  Implements,
  /// A union member: `smear::schema::undefined-union-member`.
  UnionMember,
  /// A root operation's type: `smear::schema::undefined-root-operation-type`.
  RootOperation,
  /// A place GraphQL does not have: `smear::graphqlx::unresolved-path` however the path is written.
  Other,
}

/// One question to the resolver: where a path is written, and what is wanted of it.
// Lowering, the next part of the analyzer, asks one at each path it lowers; until it lands only the
// test-support door does.
#[allow(dead_code)]
pub(crate) struct Request<'r> {
  /// The module the path stands in.
  pub(crate) module: ModuleId,
  /// The source the path is written in.
  pub(crate) source: SourceId,
  pub(crate) wanted: Wanted,
  pub(crate) position: Position,
  /// The generic parameters in scope where the path stands, its definition's or the names an
  /// extension gives them, as [`params`] tables them: each spelling with its index, sorted by
  /// spelling, one row a spelling.
  pub(crate) params: &'r [(&'r [u8], u32)],
}

/// The generic parameters spelled `written`, in order, as a [`Request`] carries them: each
/// spelling with its index, sorted by spelling, one row a spelling, the lowest index of a spelling
/// written twice. Lowering builds it once for each definition or extension, and the resolver finds
/// a path's first segment in it by binary search.
// Lowering, the next part of the analyzer, calls this for each definition and extension; until it
// lands only the test-support door does.
#[allow(dead_code)]
pub(crate) fn params<'r>(written: &[&'r [u8]]) -> Vec<(&'r [u8], u32)> {
  let mut table: Vec<(&'r [u8], u32)> = written
    .iter()
    .enumerate()
    .map(|(index, &spelling): (usize, &&'r [u8])| (spelling, dense(index)))
    .collect();
  table.sort_unstable();
  table.dedup_by_key(|row: &mut (&'r [u8], u32)| row.0);
  table
}

/// What a path names.
// Lowering, the next part of the analyzer, reads each answer; until it lands only the test-support
// door does.
#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Resolved {
  /// A type or a directive.
  Def(DefId),
  /// `NonZeroIN` or `NonZeroUN`: `core`'s `NonZero`, of the primitive whose definition is
  /// `primitive`.
  NonZero { non_zero: DefId, primitive: DefId },
  /// A module.
  Module(ModuleId),
  /// The generic parameter in scope at this index.
  Param(u32),
  /// Nothing: why has been reported.
  Unresolved,
}

/// What a name names in the type namespace, which types and modules share, or in the directive
/// namespace.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Target {
  /// A definition.
  Def(DefId),
  /// A module.
  Module(ModuleId),
  /// `NonZeroIN` or `NonZeroUN`: `core`'s `NonZero` of the primitive whose definition this is.
  NonZero(DefId),
}

/// Which level of a module's scope answered a lookup.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Level {
  /// The module's own declarations and child modules.
  Declared,
  /// Its explicit `use` bindings.
  Explicit,
  /// Its globs.
  Glob,
  /// The prelude.
  Prelude,
  /// The packages its package may reach.
  Package,
}

/// What a lookup of a first segment in a module's scope found.
#[derive(Clone, Copy)]
enum Lookup {
  /// One item, at one level.
  Found(Target, Level),
  /// Two different items two globs give, each with where its glob is written.
  Ambiguous((Target, Location), (Target, Location)),
  /// Nothing.
  Missing,
}

impl Lookup {
  /// Whether `self` and `other` found the same thing, at whatever level.
  fn same(self, other: Self) -> bool {
    match (self, other) {
      (Self::Found(a, _), Self::Found(b, _)) => a == b,
      (Self::Ambiguous(a, b), Self::Ambiguous(c, d)) => a.0 == c.0 && b.0 == d.0,
      (Self::Missing, Self::Missing) => true,
      _ => false,
    }
  }
}

/// One lookup of a first segment in a module's scope.
#[derive(Clone, Copy)]
struct Query<'q> {
  module: ModuleId,
  name: &'q [u8],
  /// `name`'s symbol, if it has one: a name no declaration, module or `use` has is in no table.
  sym: Option<Sym>,
  namespace: u8,
  /// Whether a child module answers before a type of its name, which only an error leaves beside
  /// each other.
  module_first: bool,
  /// Whether the packages the module's package may reach answer, last.
  packages: bool,
  /// Whether only the module's declarations, its child modules and its explicit `use` bindings
  /// answer: the explicit fixpoint's lookups, which nothing resolved later can change.
  explicit_only: bool,
  /// The `use` item whose own bindings and globs do not answer: the one whose path is looked up.
  exclude: Option<u32>,
}

impl<'q> Query<'q> {
  /// A lookup of `name`, whose symbol is `sym`, in the scope of `module`, wanting `wanted`, as a
  /// path that is not a `use` item's asks it.
  fn wanting(module: ModuleId, name: &'q [u8], sym: Option<Sym>, wanted: Wanted) -> Self {
    Self {
      module,
      name,
      sym,
      namespace: wanted.namespace(),
      module_first: wanted == Wanted::Module,
      packages: wanted == Wanted::Module,
      explicit_only: false,
      exclude: None,
    }
  }
}

/// Where a path starts, as resolution reads it.
#[derive(Clone, Copy)]
pub(crate) enum Root {
  /// No root: the scope of the module the path stands in.
  Relative,
  /// `~::`.
  Package,
  /// `^::`.
  Dependency,
  /// `..::`, this many times, written here.
  Parent(usize, Location),
  /// A root of a kind this analyzer does not know, the whole path written here.
  Unknown(Location),
}

/// One segment of a path: its spelling and where it is written.
#[derive(Clone, Copy)]
pub(crate) struct Segment<'p> {
  pub(crate) name: &'p [u8],
  pub(crate) at: Location,
}

/// What a later segment is looked up in, as a message names it.
#[derive(Clone, Copy)]
enum Within<'p> {
  /// The module the segment before names, spelled so.
  After(&'p [u8]),
  /// The root module of the package the path is written in, which `~::` names.
  Own,
  /// The module `..::` names.
  Parent,
}

/// Why a path does not resolve.
#[derive(Clone, Copy)]
enum Why<'p> {
  /// Nothing of the name is in the scope of `module`, where the path stands.
  NotInScope { module: ModuleId, wanted: Wanted },
  /// Nothing of the name, in either namespace, is in the scope of `module`, nor is a package it
  /// may reach named so: a `use` item's path of one segment.
  NothingInScope { module: ModuleId },
  /// `module`, which the path names `within`, declares nothing of the name.
  NotIn {
    module: ModuleId,
    within: Within<'p>,
    wanted: Wanted,
  },
  /// `module`, which the path names `within`, declares nothing of the name in either namespace:
  /// the last segment of a `use` item's path.
  NotDeclared {
    module: ModuleId,
    within: Within<'p>,
  },
  /// The name names `found`, which is not what is wanted.
  WrongKind { found: Target, wanted: Wanted },
  /// The name is a generic parameter, and segments follow it.
  ParamNotModule,
  /// The name is a generic parameter, alone where a module is wanted.
  ParamAsModule,
  /// `..::` above a root module.
  AboveRoot,
  /// `^::name`, or a first segment where a module is wanted, names no package the path's package
  /// may reach.
  UnknownPackage,
  /// A primitive whose width the path's package may not have.
  Unavailable(Primitive),
  /// Two globs give the name two different items, each with where its glob is written.
  Ambiguous {
    first: (Target, Location),
    second: (Target, Location),
  },
  /// More segments than `Limits::max_path_segments`: this many.
  TooLong(usize),
  /// A `use` item's groups nested deeper than `Limits::max_use_depth`: this deep.
  TooDeep(u32),
  /// `^::*`, which names no module.
  PackageGlob,
  /// A string source that is a URL.
  Url,
  /// A string source in a package of the `Flat` layout, whose one module is its root.
  Flat,
  /// A string source that neither starts with `./` or `../` nor is a URL.
  NotRelative,
  /// A string source that climbs out of its package's root directory.
  Outside,
  /// A string source that names no directory of its package that is a module.
  NoDirectory,
  /// A string source that names a file of its package, which is in this directory, relative to
  /// the package's root.
  File { directory: &'p [u8] },
  /// A string source that names its package's root module and binds no name with `as`.
  Unnamed,
  /// A root, a prefix or a tree of a kind this analyzer does not know.
  Unknown,
}

/// A path that does not resolve: where, what is written there, and why.
struct Failure<'p> {
  at: Location,
  name: &'p [u8],
  why: Why<'p>,
  /// Whether the path is one segment with no root, which keeps GraphQL's code where GraphQL has
  /// its place.
  single: bool,
}

/// The scopes of every module of `declarations`, whose packages are `packages`, in id order and
/// numbered by `sources`, with each package's resolved `dependencies`: every module's declarations
/// and child modules, and every `use` item resolved, reporting what does not resolve and what
/// collides. Reports too, as a warning, every type a supplied package declares with the name of a
/// primitive of the prelude.
pub(crate) fn scopes<'x>(
  packages: &'x [PackageView<'_>],
  sources: &SourceMap,
  dependencies: &[Vec<PackageId>],
  declarations: &'x Declarations<'_, '_>,
  limits: &Limits,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) -> Scopes<'x> {
  let roots: &'x [ModuleId] = declarations.roots.as_slice();
  let mut scoped: Vec<PackageScope<'x>> = Vec::with_capacity(packages.len());
  for (index, view) in packages.iter().enumerate() {
    let mut reached: Vec<(&'x [u8], ModuleId)> = Vec::new();
    for dependency in &dependencies[index] {
      let at = dependency.get() as usize;
      // A built-in package is reached by its name whether or not it is declared.
      if at >= ROOT {
        reached.push((packages[at].name, roots[at]));
      }
    }
    reached.sort_unstable_by(|a: &(&[u8], ModuleId), b: &(&[u8], ModuleId)| a.0.cmp(b.0));
    scoped.push(PackageScope {
      name: view.name,
      layout: view.layout,
      integers: view.integers,
      dependencies: reached,
      source: sources.package_sources(index).start(),
    });
  }

  let mut scopes = Scopes {
    bindings: declarations.names.as_slice(),
    modules: declarations.modules.as_slice(),
    children: declarations.children.as_slice(),
    roots,
    primitives: declarations.primitives.as_slice(),
    packages: scoped,
    non_zero: None,
    explicit: BTreeMap::new(),
    globs: BTreeMap::new(),
    directories: Vec::new(),
    files: Vec::new(),
    max_segments: limits.max_path_segments(),
    max_use_depth: limits.max_use_depth(),
    near: Cell::new(limits.max_near_miss_comparisons()),
  };
  let non_zero: Option<DefId> = builder
    .find(b"NonZero")
    .and_then(|sym: Sym| scopes.def(roots[CORE], TYPE, sym));
  scopes.non_zero = non_zero;
  let (directories, files) = scopes.file_tables(packages, sources, builder);
  scopes.directories = directories;
  scopes.files = files;
  scopes.shadowed(builder, reporter);
  uses::resolve(&mut scopes, declarations, limits, builder, reporter);
  scopes
}

impl<'x> Scopes<'x> {
  /// The directory module of every file of a `Directory` package, by source id, and those files by
  /// module and name: the tables a string source is resolved against. Each file's path is read
  /// once, split at its last `/`, and its directories are walked down from its package's root
  /// module, one child module per name, as the module tree placed the file.
  ///
  /// A package's files are in source order, which keeps the files of one directory in the bytewise
  /// order of their names. So a stable sort by module alone leaves the rows in module, then name
  /// order, and reads no name.
  fn file_tables(
    &self,
    packages: &'x [PackageView<'_>],
    sources: &SourceMap,
    names: &ModelBuilder,
  ) -> (Vec<Option<ModuleId>>, Vec<FileName<'x>>) {
    let mut directories: Vec<Option<ModuleId>> = vec![None; sources.len() as usize];
    let mut files: Vec<FileName<'x>> = Vec::new();
    for (index, view) in packages.iter().enumerate() {
      if view.layout != Layout::Directory {
        continue;
      }
      let first = sources.package_sources(index).start() as usize + 1;
      for (offset, file) in view.files.iter().enumerate() {
        let path: &'x [u8] = file.path;
        let (directory, name) = match path.iter().rposition(|byte: &u8| *byte == b'/') {
          Some(cut) => (&path[..cut], &path[cut + 1..]),
          None => (&path[..0], path),
        };
        let Some(module) = self.directory_module(names, index, directory) else {
          continue;
        };
        if let Some(slot) = directories.get_mut(first + offset) {
          *slot = Some(module);
        }
        files.push(FileName {
          module,
          name,
          directory,
        });
      }
    }
    files.sort_by_key(|file: &FileName<'x>| file.module);
    (directories, files)
  }

  /// The path, relative to its package's root, of the directory of the file named `name` in the
  /// directory whose module is `module`: `None` when no file of that directory is named so.
  fn named_file(&self, module: ModuleId, name: &[u8]) -> Option<&'x [u8]> {
    let at = self
      .files
      .binary_search_by(|file: &FileName<'x>| {
        file.module.cmp(&module).then_with(|| file.name.cmp(name))
      })
      .ok()?;
    Some(self.files[at].directory)
  }

  /// The module of `directory`, a directory of the package at `package`: its names joined by `/`,
  /// none for the package's root directory, walked down from its root module. `None` when a name
  /// names no child module, or one that only an inline `mod` declares, where no file is placed.
  fn directory_module(
    &self,
    names: &ModelBuilder,
    package: usize,
    directory: &[u8],
  ) -> Option<ModuleId> {
    let mut module = self.roots[package];
    if directory.is_empty() {
      return Some(module);
    }
    for name in directory.split(|byte: &u8| *byte == b'/') {
      module = self.child(module, names.find(name)?)?;
    }
    (self.modules[module.get() as usize].kind == ModuleKind::Directory).then_some(module)
  }

  /// The directory module of the file `source`, when it is a file of a `Directory` package placed
  /// in one.
  fn directory_of(&self, source: SourceId) -> Option<ModuleId> {
    self
      .directories
      .get(source.get() as usize)
      .copied()
      .flatten()
  }
}

impl Scopes<'_> {
  /// Resolves `path`, written where `request` says: what it names, or `Resolved::Unresolved` once
  /// why is reported through `reporter`. A single segment with no root that names nothing, or names
  /// what its place does not want, is reported with GraphQL's code for `request`'s position where
  /// GraphQL has one, and every other path with `smear::graphqlx::unresolved-path`, or
  /// `unknown-package` for a package it may not reach.
  // Lowering, the next part of the analyzer, calls this at every path it lowers; until it lands
  // nothing does.
  #[allow(dead_code)]
  pub(crate) fn resolve<S: AsRef<[u8]>>(
    &self,
    names: &ModelBuilder,
    reporter: &mut Reporter,
    request: &Request<'_>,
    path: &Path<S>,
  ) -> Resolved {
    let mut segments: Vec<Segment<'_>> = Vec::with_capacity(path.segments().len());
    let root = digest(path, request.source, &mut segments);
    self.resolve_segments(names, reporter, request, root, &segments)
  }

  /// Resolves the path `root` and `segments` spell, as [`resolve`](Self::resolve) does.
  pub(crate) fn resolve_segments(
    &self,
    names: &ModelBuilder,
    reporter: &mut Reporter,
    request: &Request<'_>,
    root: Root,
    segments: &[Segment<'_>],
  ) -> Resolved {
    match self.walk(names, request, root, segments) {
      Ok(resolved) => resolved,
      Err(failure) => {
        self.report(
          names,
          reporter,
          request.module,
          request.position,
          request.params,
          &failure,
        );
        Resolved::Unresolved
      }
    }
  }

  /// What the path `root` and `segments` spell names where `request` says, or why it names nothing.
  ///
  /// A failure travels boxed, so a path that resolves moves no more than its answer and one that
  /// does not costs one allocation.
  fn walk<'p>(
    &self,
    names: &ModelBuilder,
    request: &Request<'_>,
    root: Root,
    segments: &[Segment<'p>],
  ) -> Result<Resolved, Box<Failure<'p>>> {
    let package = self.package_of(request.module);
    let (Some(&first), Some(&last)) = (segments.first(), segments.last()) else {
      // A parsed path holds a segment at least, so a path of none is never asked about.
      return Ok(Resolved::Unresolved);
    };
    if segments.len() > self.max_segments as usize {
      let at = joined(first.at, last.at);
      return Err(Box::new(Failure {
        at,
        name: first.name,
        why: Why::TooLong(segments.len()),
        single: false,
      }));
    }
    let rest = &segments[1..];
    let (start, after, within): (ModuleId, &[Segment<'p>], Within<'p>) = match root {
      Root::Relative => {
        // What the first segment is wanted as: what the request wants when it is the only one,
        // and a module when segments follow it, whatever the last of them is wanted as.
        let wanted = if rest.is_empty() {
          request.wanted
        } else {
          Wanted::Module
        };
        // A generic parameter is a type: in scope where a type or a module is wanted, and never
        // for a lone directive's name.
        if wanted != Wanted::Directive
          && let Some(index) = param(request.params, first.name)
        {
          return if !rest.is_empty() {
            Err(failure(first, Why::ParamNotModule, false))
          } else if wanted == Wanted::Type {
            Ok(Resolved::Param(index))
          } else {
            Err(failure(first, Why::ParamAsModule, false))
          };
        }
        let query = Query::wanting(request.module, first.name, names.find(first.name), wanted);
        match self.lookup(query) {
          Lookup::Found(target, _) if rest.is_empty() => {
            return self
              .finish(target, wanted, package)
              .map_err(|why: Why<'static>| failure(first, why, true));
          }
          Lookup::Found(Target::Module(module), _) => (module, rest, Within::After(first.name)),
          Lookup::Found(found, _) => {
            let why = Why::WrongKind {
              found,
              wanted: Wanted::Module,
            };
            return Err(failure(first, why, false));
          }
          Lookup::Ambiguous(one, other) => {
            let why = Why::Ambiguous {
              first: one,
              second: other,
            };
            return Err(failure(first, why, rest.is_empty()));
          }
          Lookup::Missing => {
            let why = Why::NotInScope {
              module: request.module,
              wanted,
            };
            return Err(failure(first, why, rest.is_empty()));
          }
        }
      }
      Root::Package => (self.roots[package], segments, Within::Own),
      Root::Dependency => {
        let Some(module) = self.package_root(package, first.name) else {
          return Err(failure(first, Why::UnknownPackage, false));
        };
        if rest.is_empty() {
          return self
            .finish(Target::Module(module), request.wanted, package)
            .map_err(|why: Why<'static>| failure(first, why, false));
        }
        (module, rest, Within::After(first.name))
      }
      Root::Parent(count, at) => match self.up(request.module, count) {
        Some(module) => (module, segments, Within::Parent),
        None => {
          return Err(Box::new(Failure {
            at,
            name: b"..",
            why: Why::AboveRoot,
            single: false,
          }));
        }
      },
      Root::Unknown(at) => {
        return Err(Box::new(Failure {
          at,
          name: first.name,
          why: Why::Unknown,
          single: false,
        }));
      }
    };
    self.later(names, request.wanted, package, start, after, within)
  }

  /// What the segments `after` name below `module`, which the path names `within`: each but the
  /// last a child module of the one before, and the last what `wanted` asks for, among the
  /// declarations and the child modules of the module before it. A failure travels boxed, as
  /// [`walk`](Self::walk)'s does.
  fn later<'p>(
    &self,
    names: &ModelBuilder,
    wanted: Wanted,
    package: usize,
    mut module: ModuleId,
    after: &[Segment<'p>],
    mut within: Within<'p>,
  ) -> Result<Resolved, Box<Failure<'p>>> {
    for (offset, &segment) in after.iter().enumerate() {
      let sym = names.find(segment.name);
      if offset + 1 == after.len() {
        let found = sym.and_then(|sym: Sym| {
          self.declared(module, wanted.namespace(), sym, wanted == Wanted::Module)
        });
        return match found {
          Some(target) => self
            .finish(target, wanted, package)
            .map_err(|why: Why<'static>| failure(segment, why, false)),
          None => {
            let why = Why::NotIn {
              module,
              within,
              wanted,
            };
            Err(failure(segment, why, false))
          }
        };
      }
      match sym.and_then(|sym: Sym| self.declared(module, TYPE, sym, true)) {
        Some(Target::Module(child)) => {
          module = child;
          within = Within::After(segment.name);
        }
        Some(found) => {
          let why = Why::WrongKind {
            found,
            wanted: Wanted::Module,
          };
          return Err(failure(segment, why, false));
        }
        None => {
          let why = Why::NotIn {
            module,
            within,
            wanted: Wanted::Module,
          };
          return Err(failure(segment, why, false));
        }
      }
    }
    // Only a path of no segment after its root reaches here, and a parsed path has one.
    Ok(Resolved::Unresolved)
  }

  /// `target`, which a path names in `package`, as `wanted` wants it, or why it is not: of
  /// another kind, or a primitive whose width the package may not have.
  fn finish(
    &self,
    target: Target,
    wanted: Wanted,
    package: usize,
  ) -> Result<Resolved, Why<'static>> {
    match (target, wanted) {
      (Target::Def(def), Wanted::Type | Wanted::Directive) => {
        self.usable(def, package)?;
        Ok(Resolved::Def(def))
      }
      (Target::NonZero(primitive), Wanted::Type) => {
        self.usable(primitive, package)?;
        match self.non_zero {
          Some(non_zero) => Ok(Resolved::NonZero {
            non_zero,
            primitive,
          }),
          // The prelude answers `NonZeroIN` only while `core` has `NonZero`, as it always does.
          None => Err(Why::WrongKind {
            found: target,
            wanted,
          }),
        }
      }
      (Target::Module(module), Wanted::Module) => Ok(Resolved::Module(module)),
      (found, wanted) => Err(Why::WrongKind { found, wanted }),
    }
  }

  /// Whether the package at `package` may have `def`: any definition but a primitive whose width
  /// neither `core` nor the package's integer provider gives.
  fn usable(&self, def: DefId, package: usize) -> Result<(), Why<'static>> {
    match self.primitive_of(def) {
      Some(primitive) if !available(primitive, self.packages[package].integers) => {
        Err(Why::Unavailable(primitive))
      }
      _ => Ok(()),
    }
  }

  /// What `query`'s name names in its module's scope, level by level, the first that has it
  /// deciding: the module's own declarations and child modules, its explicit `use` bindings, its
  /// globs, the prelude, and, where the query asks, the packages its package may reach.
  fn lookup(&self, query: Query<'_>) -> Lookup {
    if let Some(sym) = query.sym {
      if let Some(target) = self.declared(query.module, query.namespace, sym, query.module_first) {
        return Lookup::Found(target, Level::Declared);
      }
      if let Some(bound) = self.bound(query.module, query.namespace, sym, query.exclude) {
        return Lookup::Found(bound.target, Level::Explicit);
      }
    }
    if query.explicit_only {
      return Lookup::Missing;
    }
    if let Some(sym) = query.sym {
      let globbed = self.globbed(
        query.module,
        query.namespace,
        sym,
        query.module_first,
        query.exclude,
      );
      if !matches!(globbed, Lookup::Missing) {
        return globbed;
      }
    }
    if let Some(target) = self.prelude(query.namespace, query.sym, query.name) {
      return Lookup::Found(target, Level::Prelude);
    }
    if query.packages
      && query.namespace == TYPE
      && let Some(root) = self.package_root(self.package_of(query.module), query.name)
    {
      return Lookup::Found(Target::Module(root), Level::Package);
    }
    Lookup::Missing
  }

  /// The first explicit `use` binding of `module` that binds `sym` in `namespace`, but those of the
  /// `use` item `exclude`.
  fn bound(
    &self,
    module: ModuleId,
    namespace: u8,
    sym: Sym,
    exclude: Option<u32>,
  ) -> Option<Bound> {
    self
      .explicit
      .range((module, namespace, sym, 0, 0)..=(module, namespace, sym, u32::MAX, u32::MAX))
      .find(|(key, _): &(&ExplicitKey, &Bound)| Some(key.3) != exclude)
      .map(|(_, bound): (&ExplicitKey, &Bound)| *bound)
  }

  /// What the globs of `module`, but those only the `use` item `exclude` made, give `sym` in
  /// `namespace`: what each module they name declares so, as `declared` answers it. A module has
  /// one row for each module its globs name, at most `Limits::max_globs`, so a lookup searches at
  /// most that many modules.
  fn globbed(
    &self,
    module: ModuleId,
    namespace: u8,
    sym: Sym,
    module_first: bool,
    exclude: Option<u32>,
  ) -> Lookup {
    let mut first: Option<(Target, Location)> = None;
    let from = (module, ModuleId::new(0));
    let to = (module, ModuleId::new(u32::MAX));
    for (&(_, target), glob) in self.globs.range(from..=to) {
      if !glob.shared && Some(glob.item) == exclude {
        continue;
      }
      let Some(found) = self.declared(target, namespace, sym, module_first) else {
        continue;
      };
      match first {
        None => first = Some((found, glob.at)),
        Some((seen, seen_at)) if seen != found => {
          return Lookup::Ambiguous((seen, seen_at), (found, glob.at));
        }
        Some(_) => {}
      }
    }
    match first {
      Some((target, _)) => Lookup::Found(target, Level::Glob),
      None => Lookup::Missing,
    }
  }

  /// What `module` itself declares as `sym` in `namespace`: a definition, or, in the type
  /// namespace, a child module. A module and a type of one name in one module are refused where
  /// they are declared, and the one that fits is answered: the module where `module_wanted`, and
  /// otherwise the type.
  fn declared(
    &self,
    module: ModuleId,
    namespace: u8,
    sym: Sym,
    module_wanted: bool,
  ) -> Option<Target> {
    let def = self.def(module, namespace, sym).map(Target::Def);
    if namespace != TYPE {
      return def;
    }
    match (def, self.child(module, sym).map(Target::Module)) {
      (Some(_), Some(child)) if module_wanted => Some(child),
      (def, child) => def.or(child),
    }
  }

  /// The definition `module` declares as `sym` in `namespace`.
  fn def(&self, module: ModuleId, namespace: u8, sym: Sym) -> Option<DefId> {
    let at = self
      .bindings
      .binary_search_by_key(&(module, namespace, sym), |binding: &Binding| {
        (binding.module, binding.namespace, binding.name)
      })
      .ok()?;
    Some(self.bindings[at].def)
  }

  /// The child module of `parent` named `sym`.
  fn child(&self, parent: ModuleId, sym: Sym) -> Option<ModuleId> {
    let at = self
      .children
      .binary_search_by_key(&(parent, sym), |child: &Child| (child.parent, child.name))
      .ok()?;
    Some(self.children[at].module)
  }

  /// What `module` itself declares, in both namespaces: its run of the bindings, found by two
  /// binary searches.
  fn bindings_of(&self, module: ModuleId) -> &[Binding] {
    let from = self
      .bindings
      .partition_point(|binding: &Binding| binding.module < module);
    let to = self
      .bindings
      .partition_point(|binding: &Binding| binding.module <= module);
    &self.bindings[from..to]
  }

  /// The child modules of `module`: its run of the children, found by two binary searches.
  fn children_of(&self, module: ModuleId) -> &[Child] {
    let from = self
      .children
      .partition_point(|child: &Child| child.parent < module);
    let to = self
      .children
      .partition_point(|child: &Child| child.parent <= module);
    &self.children[from..to]
  }

  /// What the prelude names `name`, whose symbol is `sym`, in `namespace`: a declaration of the
  /// root module of `core`, `alloc` or `std`, in that order, or `NonZeroIN` or `NonZeroUN`, which
  /// name `core`'s `NonZero` of a materialised primitive.
  fn prelude(&self, namespace: u8, sym: Option<Sym>, name: &[u8]) -> Option<Target> {
    if let Some(sym) = sym {
      for &root in self.roots.iter().take(ROOT) {
        if let Some(def) = self.def(root, namespace, sym) {
          return Some(Target::Def(def));
        }
      }
    }
    if namespace != TYPE || self.non_zero.is_none() || !name.starts_with(b"NonZero") {
      return None;
    }
    // Only the `NonZero` pattern writes a primitive with a name that begins so.
    let primitive = written(name)?;
    self.primitive(primitive).map(Target::NonZero)
  }

  /// The root module of the package `name` names from the package at `package`: `core`, `alloc`
  /// or `std`, or one of its declared dependencies.
  fn package_root(&self, package: usize, name: &[u8]) -> Option<ModuleId> {
    if let Some(index) = self
      .packages
      .iter()
      .take(ROOT)
      .position(|built: &PackageScope<'_>| built.name == name)
    {
      return Some(self.roots[index]);
    }
    let reached = &self.packages[package].dependencies;
    let at = reached
      .binary_search_by(|(spelling, _): &(&[u8], ModuleId)| (*spelling).cmp(name))
      .ok()?;
    Some(reached[at].1)
  }

  /// The module `count` modules up from `module`, or `None` when that is above its package's root
  /// module. The walk stops at the root, however many `..` are written.
  fn up(&self, module: ModuleId, count: usize) -> Option<ModuleId> {
    let mut at = module;
    for _ in 0..count {
      at = self.modules[at.get() as usize].parent?;
    }
    Some(at)
  }

  /// The index of the package `module` belongs to.
  fn package_of(&self, module: ModuleId) -> usize {
    self.modules[module.get() as usize].package
  }

  /// The primitive `def` is, when it is one of those `core` materialises.
  fn primitive_of(&self, def: DefId) -> Option<Primitive> {
    let (primitive, id) = *self.primitives.get(def.get() as usize)?;
    (id == def).then_some(primitive)
  }

  /// The definition `core` materialised for `primitive`, if the program writes it.
  fn primitive(&self, primitive: Primitive) -> Option<DefId> {
    let at = self
      .primitives
      .binary_search_by_key(&primitive.rank(), |row: &(Primitive, DefId)| row.0.rank())
      .ok()?;
    Some(self.primitives[at].1)
  }

  /// Reports `failure`, a path written in `module` at `position`, where `params` are the generic
  /// parameters in scope, as [`params`] tables them, that does not resolve: with the nearest name
  /// visible there, of the namespace wanted, in its help, when one is within reach and the budget
  /// lasts.
  fn report(
    &self,
    names: &ModelBuilder,
    reporter: &mut Reporter,
    module: ModuleId,
    position: Position,
    params: &[(&[u8], u32)],
    failure: &Failure<'_>,
  ) {
    let (code, title): (Code, &'static str) = match failure.why {
      Why::TooLong(_) | Why::TooDeep(_) => titled(DiagnosticKind::ResourceLimit),
      Why::Unknown => titled(DiagnosticKind::UnknownItem),
      Why::UnknownPackage => titled(DiagnosticKind::UnknownPackage),
      Why::Ambiguous { .. } => titled(DiagnosticKind::AmbiguousGlob),
      Why::Url => titled(DiagnosticKind::UrlUnsupported),
      Why::File { .. } => titled(DiagnosticKind::FilePathUse),
      Why::NotInScope { .. } | Why::WrongKind { .. } | Why::Unavailable(_) if failure.single => {
        graphql(position).unwrap_or_else(|| titled(DiagnosticKind::UnresolvedPath))
      }
      _ => titled(DiagnosticKind::UnresolvedPath),
    };
    let (message, help) = self.explain(names, module, position, failure);
    let help = match self.suggest(names, module, failure, params) {
      Some(suggestion) => format!("did you mean {suggestion}? {help}"),
      None => help,
    };
    let mut builder = Diagnostic::builder(code, failure.at)
      .message(message)
      .primary_label(title)
      .help(help);
    if let Why::Ambiguous { first, second } = failure.why {
      builder = builder
        .label(first.1, "one glob gives it here")
        .label(second.1, "another gives it here");
    }
    reporter.report(builder.build());
  }

  /// The message and the help of `failure`, a path written in `module` at `position`.
  fn explain(
    &self,
    names: &ModelBuilder,
    module: ModuleId,
    position: Position,
    failure: &Failure<'_>,
  ) -> (String, String) {
    let package = self.package_of(module);
    let name = failure.name;
    match failure.why {
      Why::NotInScope {
        module,
        wanted: Wanted::Module,
      } => (
        format!(
          "no module named `{}` is in scope here, and no package this one reaches is named so",
          Excerpt(name)
        ),
        self.hint(names, Some(module), None, Wanted::Module, name, position),
      ),
      Why::NotInScope { module, wanted } => (
        format!(
          "no {} named {} is in scope here",
          wanted.noun(),
          quoted(wanted, name)
        ),
        self.hint(names, Some(module), None, wanted, name, position),
      ),
      Why::NothingInScope { module } => (
        format!(
          "nothing named `{}` is in scope here, and no package this one reaches is named so",
          Excerpt(name)
        ),
        self.hint(names, Some(module), None, Wanted::Module, name, position),
      ),
      Why::NotIn {
        module: holder,
        within,
        wanted,
      } => (
        format!(
          "{} declares no {} named {}",
          place(within),
          wanted.noun(),
          quoted(wanted, name)
        ),
        self.hint(names, None, Some(holder), wanted, name, position),
      ),
      Why::NotDeclared {
        module: holder,
        within,
      } => (
        format!(
          "{} declares nothing named `{}`",
          place(within),
          Excerpt(name)
        ),
        self.hint(names, None, Some(holder), Wanted::Module, name, position),
      ),
      Why::WrongKind { found, wanted } => (
        format!(
          "`{}` names {}, where a {} is wanted",
          Excerpt(name),
          kind_of(found),
          wanted.noun()
        ),
        String::from(
          "every segment of a path but the last names a module, and the last names what its \
           place wants",
        ),
      ),
      Why::ParamNotModule => (
        format!(
          "`{}` names a type parameter here, and nothing is named through a type parameter",
          Excerpt(name)
        ),
        String::from(
          "a definition's type parameters shadow every name of its module within it: rename the \
           parameter, or write the path from a root",
        ),
      ),
      Why::ParamAsModule => (
        format!(
          "`{}` names a type parameter here, where a module is wanted",
          Excerpt(name)
        ),
        String::from(
          "a definition's type parameters shadow every name of its module within it: rename the \
           parameter, or write the path from a root",
        ),
      ),
      Why::AboveRoot => (
        format!(
          "`..` climbs above the root module of package `{}`, which has no parent",
          Excerpt(self.packages[package].name)
        ),
        String::from("write fewer `..`, or start the path at the package's root with `~::`"),
      ),
      Why::UnknownPackage => self.unknown_package(package, name),
      Why::Unavailable(primitive) => (
        format!(
          "`{}` is a primitive whose width package `{}` may not have",
          Excerpt(name),
          Excerpt(self.packages[package].name)
        ),
        unavailable(primitive),
      ),
      Why::Ambiguous { .. } => (
        format!(
          "`{}` is brought into scope by two globs, as two different items",
          Excerpt(name)
        ),
        String::from(
          "name the item with a path, or bind it with an explicit `use`, which goes before every \
           glob",
        ),
      ),
      Why::TooLong(count) => (
        format!(
          "this path has {count} segments, past the limit of {}",
          self.max_segments
        ),
        String::from("raise `Limits::max_path_segments`, or write a shorter path"),
      ),
      Why::TooDeep(depth) => (
        format!(
          "this `use` item nests its groups {depth} deep, past the limit of {}",
          self.max_use_depth
        ),
        String::from("raise `Limits::max_use_depth`, or nest the groups less deeply"),
      ),
      Why::PackageGlob => (
        String::from("`^::*` names no module: `^::` is followed by the name of a package"),
        String::from("glob a package's root module with `^::name::*`"),
      ),
      Why::Url => (
        format!(
          "the string source `{}` is a URL, and a URL source is parsed but not supported",
          Excerpt(name)
        ),
        String::from(
          "declare the package in the manifest under a name, supply it to the analysis, and \
           reach it through `^::name`",
        ),
      ),
      Why::Flat => (
        format!(
          "the string source `{}` names a directory, and package `{}` has the `Flat` layout, \
           whose files are all its root module",
          Excerpt(name),
          Excerpt(self.packages[package].name)
        ),
        String::from("name the items with paths: in a `Flat` package `~::` reaches every one"),
      ),
      Why::NotRelative => (
        format!(
          "the string source `{}` names neither a directory relative to its file, starting with \
           `./` or `../`, nor a URL",
          Excerpt(name)
        ),
        String::from(
          "write the directory relative to the file the `use` item is written in, as \
           `\"./vision\"` or `\"../shared\"`",
        ),
      ),
      Why::Outside => (
        format!(
          "the string source `{}` climbs out of the root directory of package `{}`",
          Excerpt(name),
          Excerpt(self.packages[package].name)
        ),
        String::from(
          "a string source stays inside its package; reach another package through `^::name`",
        ),
      ),
      Why::NoDirectory => (
        format!(
          "the string source `{}` names no directory of package `{}` that is a module",
          Excerpt(name),
          Excerpt(self.packages[package].name)
        ),
        String::from(
          "a directory is a module when it or a directory below it holds a file of the package, \
           and its name is a GraphQLx name",
        ),
      ),
      Why::File { directory } => (
        format!(
          "the string source `{}` names a file, and a string source names a directory, whose \
           files are one module",
          Excerpt(name)
        ),
        if directory.is_empty() {
          String::from(
            "name the directory the file is in, the package's root directory, whose module `~::` \
             names",
          )
        } else {
          format!(
            "name the directory the file is in, `{}`, whose files are its module",
            Excerpt(directory)
          )
        },
      ),
      Why::Unnamed => (
        format!(
          "the string source `{}` names the root module of package `{}`, which has no name to \
           bind",
          Excerpt(name),
          Excerpt(self.packages[package].name)
        ),
        String::from("bind it under a name with `as`, or name its items with `~::`"),
      ),
      Why::Unknown => (
        String::from(
          "this path, or the `use` item it is part of, is of a kind this analyzer does not know, \
           so what it names is not in the model",
        ),
        String::from(
          "the parser that read this file is newer than this analyzer; analyze it with a release \
           of the analyzer that knows every kind the parser reads",
        ),
      ),
    }
  }

  /// The help of a name that names nothing where `wanted` is wanted: in the scope of `scope`, or
  /// among the declarations of `holder`. It names what a reader most likely missed: a width the
  /// package may not have, or a name the other namespace holds.
  fn hint(
    &self,
    names: &ModelBuilder,
    scope: Option<ModuleId>,
    holder: Option<ModuleId>,
    wanted: Wanted,
    name: &[u8],
    position: Position,
  ) -> String {
    let module = scope.or(holder);
    if let Some(module) = module
      && wanted == Wanted::Type
      && let Some(primitive) = written(name)
      && !available(primitive, self.packages[self.package_of(module)].integers)
    {
      return unavailable(primitive);
    }
    let other = match wanted {
      Wanted::Type => Some(Wanted::Directive),
      Wanted::Directive => Some(Wanted::Type),
      Wanted::Module => None,
    };
    if let Some(other) = other {
      let held = match (scope, holder) {
        (Some(scope), _) => {
          let query = Query::wanting(scope, name, names.find(name), other);
          !matches!(self.lookup(query), Lookup::Missing)
        }
        (None, Some(holder)) => names
          .find(name)
          .and_then(|sym: Sym| self.declared(holder, other.namespace(), sym, false))
          .is_some(),
        (None, None) => false,
      };
      if held {
        return format!(
          "{} names a {}, and types and directives are named apart: a {} needs a name of its own",
          quoted(other, name),
          other.noun(),
          wanted.noun()
        );
      }
    }
    match (position, scope) {
      (Position::ExtensionTarget, Some(_)) => String::from(
        "`extend` adds to a type or directive the document defines; define it, or drop the \
         `extend`.",
      ),
      (_, Some(_)) => String::from("declare it, or bring it into scope with a `use` item"),
      (_, None) => String::from(
        "every segment of a path but the last names a module, and the last names one of that \
         module's declarations or child modules",
      ),
    }
  }

  /// The message and the help of `^::name`, or of a first segment wanting a module, that names no
  /// package the package at `package` may reach.
  fn unknown_package(&self, package: usize, name: &[u8]) -> (String, String) {
    let own = self.packages[package].name;
    if own == name {
      return (
        format!(
          "package `{}` is the package this path is written in, which `^::` does not name",
          Excerpt(name)
        ),
        String::from("a package reaches its own items through `~::`"),
      );
    }
    if self.supplied(name) {
      (
        format!(
          "package `{}` does not declare the dependency `{}`, so it cannot reach it",
          Excerpt(own),
          Excerpt(name)
        ),
        String::from(
          "declare the package among this package's dependencies: a package reaches its own \
           dependencies, and never theirs",
        ),
      )
    } else {
      (
        format!(
          "no package named `{}` is supplied, and it is not `core`, `alloc` or `std`",
          Excerpt(name)
        ),
        String::from("supply the package, and declare it among this package's dependencies"),
      )
    }
  }

  /// Whether a supplied package is named `name`: the root package, or one of the dependencies
  /// after it, which the package set numbers in name order, so a binary search finds it.
  fn supplied(&self, name: &[u8]) -> bool {
    let Some(root) = self.packages.get(ROOT) else {
      return false;
    };
    root.name == name
      || self.packages[ROOT + 1..]
        .binary_search_by(|scope: &PackageScope<'_>| scope.name.cmp(name))
        .is_ok()
  }

  /// The near miss of `wanted` among `candidates`, under the analysis's one near-miss budget and
  /// the rules every search keeps: the nearest within reach, or `None` when none is, when `wanted`
  /// is not searched for, or when the budget runs out before the candidates do. Each candidate
  /// looked at spends one of the budget's comparisons, and none is looked at once it is spent.
  ///
  /// Lowering asks it what an unknown argument of a directive application may have meant, among
  /// the directive's arguments.
  // Lowering, the next part of the analyzer, calls this; until it lands only the tests do.
  #[allow(dead_code)]
  pub(crate) fn nearest<'c>(
    &self,
    wanted: &'c [u8],
    candidates: &mut dyn Iterator<Item = &'c [u8]>,
  ) -> Option<&'c [u8]> {
    if self.near.get() == 0 {
      return None;
    }
    let mut search = near::Search::new(wanted);
    for candidate in candidates {
      if !search.open() {
        break;
      }
      search.offer(&self.near, candidate);
    }
    search.found()
  }

  /// The near miss of `failure`'s name, written in `module` where `params` are the generic
  /// parameters in scope, quoted as a message quotes it: the nearest name visible there, of the
  /// namespace wanted, when one is within reach and the budget lasts.
  ///
  /// A first segment's candidates are its scope's names: the generic parameters where a type or a
  /// module is wanted, the module's declarations, child modules, explicit `use` bindings and
  /// globs, the prelude, and the packages its package may reach where a module is wanted. A later
  /// segment's are the declarations and the child modules of the module before it; a package's
  /// after `^::`, the packages its package may reach. Once the budget is spent, nothing is looked
  /// at.
  fn suggest(
    &self,
    names: &ModelBuilder,
    module: ModuleId,
    failure: &Failure<'_>,
    params: &[(&[u8], u32)],
  ) -> Option<String> {
    if self.near.get() == 0 {
      return None;
    }
    let mut search = near::Search::new(failure.name);
    if !search.open() {
      return None;
    }
    // What the suggestion is quoted as, as `explain` quotes the name: with `@` only where a
    // directive alone is wanted, and bare in a `use` item's path, which names either namespace.
    let quoting = match failure.why {
      Why::NotInScope { module, wanted } => {
        if wanted != Wanted::Directive {
          for &(spelling, _) in params {
            search.offer(&self.near, spelling);
          }
        }
        let packages = wanted == Wanted::Module;
        self.offer_scope(&mut search, names, module, wanted.namespace(), packages);
        wanted
      }
      Why::NothingInScope { module } => {
        self.offer_scope(&mut search, names, module, TYPE, true);
        self.offer_scope(&mut search, names, module, DIRECTIVE, false);
        Wanted::Module
      }
      Why::NotIn { module, wanted, .. } => {
        self.offer_declared(&mut search, names, module, wanted.namespace());
        wanted
      }
      Why::NotDeclared { module, .. } => {
        self.offer_declared(&mut search, names, module, TYPE);
        self.offer_declared(&mut search, names, module, DIRECTIVE);
        Wanted::Module
      }
      Why::UnknownPackage => {
        self.offer_packages(&mut search, self.package_of(module));
        Wanted::Module
      }
      _ => return None,
    };
    search
      .found()
      .map(|spelled: &[u8]| quoted(quoting, spelled))
  }

  /// Offers `search` the names the scope of `module` has in `namespace`, the packages its package
  /// may reach last when `packages`.
  fn offer_scope<'c>(
    &'c self,
    search: &mut near::Search<'c>,
    names: &'c ModelBuilder,
    module: ModuleId,
    namespace: u8,
    packages: bool,
  ) {
    self.offer_declared(search, names, module, namespace);
    let from: ExplicitKey = (module, namespace, Sym::new(0), 0, 0);
    let to: ExplicitKey = (module, namespace, Sym::new(u32::MAX), u32::MAX, u32::MAX);
    for (key, _) in self.explicit.range(from..=to) {
      if !search.open() {
        return;
      }
      search.offer(&self.near, names.spelling(key.2));
    }
    let first = (module, ModuleId::new(0));
    let end = (module, ModuleId::new(u32::MAX));
    for (&(_, target), _) in self.globs.range(first..=end) {
      // A module a glob names costs a comparison of its own, whatever it declares.
      search.visit(&self.near);
      if !search.open() {
        return;
      }
      self.offer_declared(search, names, target, namespace);
    }
    for &root in self.roots.iter().take(ROOT) {
      self.offer_bindings(search, names, root, namespace);
    }
    if packages && namespace == TYPE {
      self.offer_packages(search, self.package_of(module));
    }
  }

  /// Offers `search` what `module` declares in `namespace`, its child modules included in the type
  /// namespace.
  fn offer_declared<'c>(
    &'c self,
    search: &mut near::Search<'c>,
    names: &'c ModelBuilder,
    module: ModuleId,
    namespace: u8,
  ) {
    self.offer_bindings(search, names, module, namespace);
    if namespace != TYPE {
      return;
    }
    let from = self
      .children
      .partition_point(|child: &Child| child.parent < module);
    for child in self.children[from..]
      .iter()
      .take_while(|child: &&Child| child.parent == module)
    {
      if !search.open() {
        return;
      }
      search.offer(&self.near, names.spelling(child.name));
    }
  }

  /// Offers `search` the definitions `module` declares in `namespace`.
  fn offer_bindings<'c>(
    &'c self,
    search: &mut near::Search<'c>,
    names: &'c ModelBuilder,
    module: ModuleId,
    namespace: u8,
  ) {
    let from = self.bindings.partition_point(|binding: &Binding| {
      (binding.module, binding.namespace) < (module, namespace)
    });
    for binding in self.bindings[from..]
      .iter()
      .take_while(|binding: &&Binding| binding.module == module && binding.namespace == namespace)
    {
      if !search.open() {
        return;
      }
      search.offer(&self.near, names.spelling(binding.name));
    }
  }

  /// Offers `search` the names of the packages the package at `package` may reach.
  fn offer_packages<'c>(&'c self, search: &mut near::Search<'c>, package: usize) {
    for built in self.packages.iter().take(ROOT) {
      search.offer(&self.near, built.name);
    }
    for &(name, _) in &self.packages[package].dependencies {
      search.offer(&self.near, name);
    }
  }

  /// Warns of every type a supplied package's module declares with the name of a primitive of the
  /// prelude, which it shadows in that module; a root package's definition that replaces a GraphQL
  /// built-in shadows nothing.
  fn shadowed(&self, names: &ModelBuilder, reporter: &mut Reporter) {
    let kind = DiagnosticKind::ShadowedPrimitive;
    for binding in self.bindings {
      if binding.namespace != TYPE || binding.replaces || self.package_of(binding.module) < ROOT {
        continue;
      }
      let name = names.spelling(binding.name);
      let Some(help) = shadowing(name) else {
        continue;
      };
      reporter.report(
        Diagnostic::builder(kind.code(), names.def_location(binding.def))
          .severity(kind.severity())
          .message(format!(
            "the type `{}` takes the name of a primitive of the prelude, which it shadows in its \
             module",
            Excerpt(name)
          ))
          .primary_label(kind.title())
          .help(help)
          .build(),
      );
    }
  }

  /// The module `path` names, spelled as `Model::module_by_path` takes it: `^::package`, then the
  /// names of the modules down from its root module.
  #[cfg(feature = "test-support")]
  pub(crate) fn module_at(&self, names: &ModelBuilder, path: &str) -> Option<ModuleId> {
    let mut segments = path.strip_prefix("^::")?.split("::");
    let package = segments.next()?;
    let index = self
      .packages
      .iter()
      .position(|scope: &PackageScope<'_>| scope.name == package.as_bytes())?;
    let mut module = self.roots[index];
    for segment in segments {
      module = self.child(module, names.find(segment.as_bytes())?)?;
    }
    Some(module)
  }

  /// What `path` names in `module`, where the generic parameters spelled `written`, in order, are
  /// in scope: the test-support door's one call, which tables them as lowering does. `path` is
  /// spelled as written, `~::`, `^::` and `..::` included; `@` before it asks for a directive, and
  /// `mod ` before it for a module. What it does not resolve to is reported nowhere.
  #[cfg(feature = "test-support")]
  pub(crate) fn door(
    &self,
    names: &ModelBuilder,
    module: ModuleId,
    written: &[&[u8]],
    path: &str,
  ) -> Resolved {
    let (wanted, path) = match (path.strip_prefix('@'), path.strip_prefix("mod ")) {
      (Some(rest), _) => (Wanted::Directive, rest),
      (None, Some(rest)) => (Wanted::Module, rest),
      (None, None) => (Wanted::Type, path),
    };
    let at = Location::entire(0);
    let (root, rest) = if let Some(rest) = path.strip_prefix("~::") {
      (Root::Package, rest)
    } else if let Some(rest) = path.strip_prefix("^::") {
      (Root::Dependency, rest)
    } else {
      let mut count: usize = 0;
      let mut rest = path;
      while let Some(next) = rest.strip_prefix("..::") {
        count += 1;
        rest = next;
      }
      if count == 0 {
        (Root::Relative, rest)
      } else {
        (Root::Parent(count, at), rest)
      }
    };
    let segments: Vec<Segment<'_>> = rest
      .split("::")
      .map(|name: &str| Segment {
        name: name.as_bytes(),
        at,
      })
      .collect();
    let table = params(written);
    let request = Request {
      module,
      source: SourceId::new(0),
      wanted,
      position: Position::Other,
      params: &table,
    };
    let mut reporter = Reporter::without_marks(0);
    self.resolve_segments(names, &mut reporter, &request, root, &segments)
  }
}

/// The root of `path`, and each of its segments, written in `source`, onto `segments`.
fn digest<'p, S: AsRef<[u8]>>(
  path: &'p Path<S>,
  source: SourceId,
  segments: &mut Vec<Segment<'p>>,
) -> Root {
  for name in path.segments() {
    segments.push(Segment {
      name: name.source().as_ref(),
      at: Location::new(source.get(), *name.as_span()),
    });
  }
  match path.root() {
    PathRoot::Relative => Root::Relative,
    PathRoot::Package(_) => Root::Package,
    PathRoot::Dependency(_) => Root::Dependency,
    PathRoot::Parent { span, count } => {
      Root::Parent(count.get(), Location::new(source.get(), *span))
    }
    // `PathRoot` is `#[non_exhaustive]`: a root of a kind this analyzer does not know names no
    // module it can find, and is reported as an item of an unknown kind is.
    _ => Root::Unknown(Location::new(source.get(), *path.span())),
  }
}

/// The index of the generic parameter spelled `name` in `params`, a table [`params`] built: a
/// binary search.
fn param(params: &[(&[u8], u32)], name: &[u8]) -> Option<u32> {
  let at = params
    .binary_search_by(|(spelling, _): &(&[u8], u32)| (*spelling).cmp(name))
    .ok()?;
  Some(params[at].1)
}

/// A failure at `segment`, boxed as a resolution's `Err` carries it.
fn failure<'p>(segment: Segment<'p>, why: Why<'p>, single: bool) -> Box<Failure<'p>> {
  Box::new(Failure {
    at: segment.at,
    name: segment.name,
    why,
    single,
  })
}

/// From the start of `first` to the end of `last`, both in one source.
fn joined(first: Location, last: Location) -> Location {
  match (first.span(), last.span()) {
    (Some(a), Some(b)) => Location::new(first.source(), SimpleSpan::new(a.start(), b.end())),
    _ => first,
  }
}

/// `kind`'s code and title.
fn titled(kind: DiagnosticKind) -> (Code, &'static str) {
  (kind.code(), kind.title())
}

/// GraphQL's code, and `smear-schema`'s phrase for it, for a name of one segment with no root that
/// names nothing at `position`, where GraphQL has such a place.
fn graphql(position: Position) -> Option<(Code, &'static str)> {
  match position {
    Position::Type => Some((schema::UNDEFINED_TYPE, "undefined type")),
    Position::Directive => Some((schema::UNDEFINED_DIRECTIVE, "undefined directive")),
    Position::ExtensionTarget => Some((
      schema::UNDEFINED_EXTENSION_TARGET,
      "extension target is not defined",
    )),
    Position::Implements => Some((
      schema::UNDEFINED_IMPLEMENTS_INTERFACE,
      "implemented interface is not defined",
    )),
    Position::UnionMember => Some((
      schema::UNDEFINED_UNION_MEMBER,
      "union member is not defined",
    )),
    Position::RootOperation => Some((
      schema::UNDEFINED_ROOT_OPERATION_TYPE,
      "undefined root operation type",
    )),
    Position::Other => None,
  }
}

/// `name`, quoted as a message quotes what `wanted` names: a directive with its `@`.
fn quoted(wanted: Wanted, name: &[u8]) -> String {
  match wanted {
    Wanted::Directive => format!("`@{}`", Excerpt(name)),
    Wanted::Type | Wanted::Module => format!("`{}`", Excerpt(name)),
  }
}

/// What a later segment is looked up in, as a message names it.
fn place(within: Within<'_>) -> String {
  match within {
    Within::After(module) => format!("`{}`", Excerpt(module)),
    Within::Own => String::from("the root module of this package"),
    Within::Parent => String::from("the module `..` names"),
  }
}

/// What `target` is, as a message names it.
fn kind_of(target: Target) -> &'static str {
  match target {
    Target::Def(_) | Target::NonZero(_) => "a type",
    Target::Module(_) => "a module",
  }
}

/// The help of a primitive whose width a package may not have.
fn unavailable(primitive: Primitive) -> String {
  let width = match primitive {
    Primitive::Int { width, .. } => width,
    Primitive::Bool | Primitive::F32 | Primitive::F64 => 0,
  };
  format!(
    "`core` gives the integer widths 8, 16, 32, 64 and 128; a width of {width} needs an integer \
     provider whose range holds it, such as `IntegerProvider::numo()`, selected for the package"
  )
}

/// The help of a type named `name` that shadows a primitive of the prelude, which says how the
/// primitive is still reached; `None` when `name` names no primitive: neither `bool`, `f32`, `f64`,
/// `iN` or `uN` of `core`, nor `string` or `bytes` of `alloc`, nor `NonZeroIN` or `NonZeroUN`.
fn shadowing(name: &[u8]) -> Option<String> {
  let shown = Excerpt(name);
  let package = if intrinsic(name).is_some() {
    "core"
  } else if name == b"string".as_slice() || name == b"bytes".as_slice() {
    "alloc"
  } else {
    if !name.starts_with(b"NonZero") {
      return None;
    }
    // Only the `NonZero` pattern writes a primitive with a name that begins so.
    let primitive = written(name)?;
    return Some(format!(
      "within its module `{shown}` names this type, and `^::core::NonZero<{}>` still names the \
       primitive's; rename the type to leave the name to the primitive",
      spelled(primitive)
    ));
  };
  Some(format!(
    "within its module `{shown}` names this type, and `^::{package}::{shown}` still names the \
     primitive; rename the type to leave the name to the primitive"
  ))
}

#[cfg(test)]
mod tests;
