//! Name resolution: every module's scope, and the one call that resolves a path where it stands.
//!
//! # A module's scope
//!
//! The first segment of a path with no root is looked up in this order, and the first hit decides:
//!
//! 1. the generic parameters of the definition the path stands in, which the caller passes;
//! 2. the module's own declarations and its child modules;
//! 3. the prelude: the declarations of the root modules of `core`, `alloc` and `std`, whatever the
//!    package's tier, which hold the primitives the program writes and the second names `Int`,
//!    `Float`, `Boolean`, `String` and `ID`; and `NonZeroIN` and `NonZeroUN`, recognised by
//!    pattern, which name `core`'s `NonZero` of that primitive whatever `NonZero` names where they
//!    are written;
//! 4. last, and only where a module is wanted, which is where more segments follow, the names of
//!    the packages the module's package may reach: `core`, `alloc` and `std`, and its declared
//!    dependencies, never a dependency's own dependency and never its own name.
//!
//! Types and modules share one namespace, and directives have their own: a type parameter, a type
//! and a module of one name stand in each other's way, and a directive stands in none's. A later
//! segment is looked up among the declarations and the child modules of the module the segments
//! before it name, and nowhere else.
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
//! found by binary search in the tables the declarations hand on, and the prelude's and the
//! packages' tables, which every module shares. So a module costs its entry and its row among its
//! parent's children, and nothing about it grows with how deep it sits or what its parent holds. A
//! lookup costs a logarithm of those tables, and a path one lookup per segment.

use std::{format, string::String, vec::Vec};

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
  model::{ModelBuilder, Primitive},
  package::IntegerProvider,
  set::{PackageView, ROOT},
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
  /// `Limits::max_path_segments`.
  max_segments: u32,
}

/// One package, as name resolution reads it.
struct PackageScope<'x> {
  /// Its name, as given.
  name: &'x [u8],
  /// Its integer provider, which decides the widths beyond `core`'s it may have.
  integers: Option<&'x IntegerProvider>,
  /// The supplied packages it declares and may reach, by name, each with its root module. The
  /// built-in packages it reaches without declaring them.
  dependencies: Vec<(&'x [u8], ModuleId)>,
}

/// What a path is wanted to name.
// Lowering, the next part of the analyzer, asks for a type or a directive at each path it lowers;
// until it lands only the test-support door does.
#[allow(dead_code)]
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
  /// The generic parameters in scope where the path stands, by spelling and in order: its
  /// definition's, or the names an extension gives them.
  pub(crate) params: &'r [&'r [u8]],
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
  /// `module`, which the path names `within`, declares nothing of the name.
  NotIn {
    module: ModuleId,
    within: Within<'p>,
    wanted: Wanted,
  },
  /// The name names `found`, which is not what is wanted.
  WrongKind { found: Target, wanted: Wanted },
  /// The name is a generic parameter, and segments follow it.
  ParamNotModule,
  /// `..::` above a root module.
  AboveRoot,
  /// `^::name`, or a first segment where a module is wanted, names no package the path's package
  /// may reach.
  UnknownPackage,
  /// A primitive whose width the path's package may not have.
  Unavailable(Primitive),
  /// More segments than `Limits::max_path_segments`: this many.
  TooLong(usize),
  /// A root of a kind this analyzer does not know.
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

/// The scopes of every module of `declarations`, whose packages are `packages`, in id order, with
/// each package's resolved `dependencies`; reports, as a warning, every type a supplied package
/// declares with the name of a primitive of the prelude.
pub(crate) fn scopes<'x>(
  packages: &'x [PackageView<'_>],
  dependencies: &[Vec<PackageId>],
  declarations: &'x Declarations<'_, '_>,
  limits: &Limits,
  builder: &ModelBuilder,
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
      integers: view.integers,
      dependencies: reached,
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
    max_segments: limits.max_path_segments(),
  };
  let non_zero: Option<DefId> = builder
    .find(b"NonZero")
    .and_then(|sym: Sym| scopes.def(roots[CORE], TYPE, sym));
  scopes.non_zero = non_zero;
  scopes.shadowed(builder, reporter);
  scopes
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
        self.report(names, reporter, request.module, request.position, &failure);
        Resolved::Unresolved
      }
    }
  }

  /// What the path `root` and `segments` spell names where `request` says, or why it names nothing.
  fn walk<'p>(
    &self,
    names: &ModelBuilder,
    request: &Request<'_>,
    root: Root,
    segments: &[Segment<'p>],
  ) -> Result<Resolved, Failure<'p>> {
    let package = self.package_of(request.module);
    let (Some(&first), Some(&last)) = (segments.first(), segments.last()) else {
      // A parsed path holds a segment at least, so a path of none is never asked about.
      return Ok(Resolved::Unresolved);
    };
    if segments.len() > self.max_segments as usize {
      let at = joined(first.at, last.at);
      return Err(Failure {
        at,
        name: first.name,
        why: Why::TooLong(segments.len()),
        single: false,
      });
    }
    let rest = &segments[1..];
    let (start, after, within): (ModuleId, &[Segment<'p>], Within<'p>) = match root {
      Root::Relative => {
        if request.wanted != Wanted::Directive
          && let Some(index) = request
            .params
            .iter()
            .position(|param: &&[u8]| *param == first.name)
        {
          return if rest.is_empty() {
            Ok(Resolved::Param(dense(index)))
          } else {
            Err(failure(first, Why::ParamNotModule, false))
          };
        }
        let wanted = if rest.is_empty() {
          request.wanted
        } else {
          Wanted::Module
        };
        match self.lookup(names, request.module, wanted, first.name) {
          Some(target) if rest.is_empty() => {
            return self
              .finish(target, wanted, package)
              .map_err(|why: Why<'static>| failure(first, why, true));
          }
          Some(Target::Module(module)) => (module, rest, Within::After(first.name)),
          Some(found) => {
            let why = Why::WrongKind {
              found,
              wanted: Wanted::Module,
            };
            return Err(failure(first, why, false));
          }
          None => {
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
          return Err(Failure {
            at,
            name: b"..",
            why: Why::AboveRoot,
            single: false,
          });
        }
      },
      Root::Unknown(at) => {
        return Err(Failure {
          at,
          name: first.name,
          why: Why::Unknown,
          single: false,
        });
      }
    };
    self.later(names, request.wanted, package, start, after, within)
  }

  /// What the segments `after` name below `module`, which the path names `within`: each but the
  /// last a child module of the one before, and the last what `wanted` asks for, among the
  /// declarations and the child modules of the module before it.
  fn later<'p>(
    &self,
    names: &ModelBuilder,
    wanted: Wanted,
    package: usize,
    mut module: ModuleId,
    after: &[Segment<'p>],
    mut within: Within<'p>,
  ) -> Result<Resolved, Failure<'p>> {
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

  /// What `name` names in the scope of `module`, where `wanted` is wanted: the module's own
  /// declarations and child modules, then the prelude, then, where a module is wanted, the
  /// packages its package may reach.
  fn lookup(
    &self,
    names: &ModelBuilder,
    module: ModuleId,
    wanted: Wanted,
    name: &[u8],
  ) -> Option<Target> {
    let namespace = wanted.namespace();
    let sym = names.find(name);
    if let Some(sym) = sym
      && let Some(target) = self.declared(module, namespace, sym, wanted == Wanted::Module)
    {
      return Some(target);
    }
    if let Some(target) = self.prelude(namespace, sym, name) {
      return Some(target);
    }
    if wanted == Wanted::Module {
      return self
        .package_root(self.package_of(module), name)
        .map(Target::Module);
    }
    None
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

  /// Reports `failure`, a path written in `module` at `position` that does not resolve.
  fn report(
    &self,
    names: &ModelBuilder,
    reporter: &mut Reporter,
    module: ModuleId,
    position: Position,
    failure: &Failure<'_>,
  ) {
    let (code, title): (Code, &'static str) = match failure.why {
      Why::TooLong(_) => titled(DiagnosticKind::ResourceLimit),
      Why::Unknown => titled(DiagnosticKind::UnknownItem),
      Why::UnknownPackage => titled(DiagnosticKind::UnknownPackage),
      Why::NotInScope { .. } | Why::WrongKind { .. } | Why::Unavailable(_) if failure.single => {
        graphql(position).unwrap_or_else(|| titled(DiagnosticKind::UnresolvedPath))
      }
      _ => titled(DiagnosticKind::UnresolvedPath),
    };
    let (message, help) = self.explain(names, module, position, failure);
    reporter.report(
      Diagnostic::builder(code, failure.at)
        .message(message)
        .primary_label(title)
        .help(help)
        .build(),
    );
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
      Why::TooLong(count) => (
        format!(
          "this path has {count} segments, past the limit of {}",
          self.max_segments
        ),
        String::from("raise `Limits::max_path_segments`, or write a shorter path"),
      ),
      Why::Unknown => (
        String::from(
          "this path starts at a root of a kind this analyzer does not know, so what it names is \
           not in the model",
        ),
        String::from(
          "the parser that read this file is newer than this analyzer; analyze it with a release \
           of the analyzer that knows every kind of root the parser reads",
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
        (Some(scope), _) => self.lookup(names, scope, other, name).is_some(),
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
    let supplied = self
      .packages
      .iter()
      .skip(ROOT)
      .any(|scope: &PackageScope<'_>| scope.name == name);
    if supplied {
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

  /// What `path` names in `module`, where `params` are the generic parameters in scope: the
  /// test-support door's one call. `path` is spelled as written, `~::`, `^::` and `..::` included;
  /// `@` before it asks for a directive, and `mod ` before it for a module. What it does not
  /// resolve to is reported nowhere.
  #[cfg(feature = "test-support")]
  pub(crate) fn door(
    &self,
    names: &ModelBuilder,
    module: ModuleId,
    params: &[&[u8]],
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
    let request = Request {
      module,
      source: SourceId::new(0),
      wanted,
      position: Position::Other,
      params,
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

/// A failure at `segment`.
fn failure<'p>(segment: Segment<'p>, why: Why<'p>, single: bool) -> Failure<'p> {
  Failure {
    at: segment.at,
    name: segment.name,
    why,
    single,
  }
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
