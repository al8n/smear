//! `use` items: each module's resolved to a fixpoint, binding what they name in the module's
//! scope, then checked once every one of them is.
//!
//! # The algorithm
//!
//! A `use` item's tree depends on its module's scope through one name at most: the first segment
//! of its path, its head, when the path has no root. Every later segment is looked up among the
//! declarations and the child modules of the module before it, which are known, and a root or a
//! string source names a known module. A module's scope depends on its own `use` items alone, since
//! nothing is re-exported, so each module is resolved apart:
//!
//! 1. The declarations, the child modules and the roots are known before any item is read.
//! 2. The explicit fixpoint. An item whose path starts at a root or at a string source is resolved
//!    at once. An item with a head is resolved when its head is found among the module's
//!    declarations, its child modules and the explicit bindings made so far, which nothing made
//!    later can change; a path of the head alone needs it found in both namespaces. An item that
//!    waits is woken when its head's name is first bound, in either namespace.
//! 3. The glob fixpoint, in rounds. A round looks each waiting head up in the whole scope as it
//!    stood when the round began, the globs made so far, the prelude and the packages included, and
//!    resolves every item that finds something, as one step; the next round looks up again only
//!    the items whose head a name the round bound first, or a module a glob of the round named
//!    first, can answer. It ends when a round resolves nothing. Needing more rounds than
//!    `Limits::max_glob_iterations` is `resource-limit`, once for the module, and the items left
//!    are reported no further.
//! 4. Every item with a head is looked up again in the final scope. One that finds something else
//!    than what it was resolved to, a name a glob, the prelude or a package gave it that a binding
//!    made after it now shadows, is `use-resolution-changed`, and what its tree failed to find is
//!    not reported.
//! 5. A name an explicit binding binds that the module itself declares in that namespace, as a
//!    definition or a child module, is `use-declaration-collision`; a name two explicit bindings
//!    give two different items is `use-name-collision`. One item bound twice is no error.
//! 6. An item whose head never resolves is reported once, at its head; a segment of a resolved tree
//!    that names nothing, once, where it is written.
//!
//! An item's own bindings and globs never answer its own head.
//!
//! # Termination and cost
//!
//! A step only adds bindings and globs, an item is resolved at most once, and a round that resolves
//! nothing ends its fixpoint, so both fixpoints end: the explicit one after one look per item and
//! at most two wakes, one per namespace its head's name is first bound in; the glob one after at
//! most `Limits::max_glob_iterations` rounds, and after at most one round per item, since every
//! round but the last resolves one. In a module of `u` items, `b` names bound and `g` distinct
//! modules its globs name, a lookup costs a logarithm of the tables, and `g` more for its globs; an
//! item is looked up once by the explicit fixpoint, once by step 4, and by the glob fixpoint once
//! and again only when a name or a glob of the round before can answer it, which each binding of
//! its name and each glob of a module holding it does once. So a module costs `O((u + b) · g)`
//! lookups at worst, and the analysis the sum over its modules, which share nothing but the tables
//! they read.

use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec,
  vec::Vec,
};

use smear_lexer::decode_string_value;
use smear_parser::graphqlx::ast::{
  Name, PathRoot, TypeSystemItem, UsePrefix, UseTree, UseTreeKind,
};
use tokora::{SimpleSpan, diagnostic::Location, span::AsSpan};

use crate::graphqlx::{
  builtin::{DIRECTIVE, TYPE, available},
  code::DiagnosticKind,
  collect::Items,
  declare::{Declarations, Entry},
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{ModuleId, SourceId, Sym, dense},
  limits::Limits,
  model::{ModelBuilder, ModuleKind, Primitive},
  package::Layout,
};

use super::{
  Bound, ExplicitKey, Failure, Level, Lookup, Position, Query, Scopes, Target, Wanted, Why, Within,
  quoted,
};

/// Resolves every `use` item of `declarations` into `scopes`, module by module, and reports what
/// does not resolve and what collides; each name an item binds is interned into `names`.
pub(super) fn resolve<'x>(
  scopes: &mut Scopes<'x>,
  declarations: &'x Declarations<'_, '_>,
  limits: &Limits,
  names: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  let entries: &'x [Entry] = declarations.uses.as_slice();
  let mut start = 0;
  while start < entries.len() {
    let module = entries[start].module;
    let end = start
      + entries[start..]
        .iter()
        .take_while(|entry: &&Entry| entry.module == module)
        .count();
    let mut items: Vec<Item<'x>> = Vec::with_capacity(end - start);
    for index in start..end {
      if let Some(item) = item_at(declarations, index) {
        items.push(item);
      }
    }
    let package = scopes.package_of(module);
    let mut run = Run {
      scopes: &mut *scopes,
      names: &mut *names,
      reporter: &mut *reporter,
      module,
      package,
      items,
      failures: Vec::new(),
      fresh: Vec::new(),
      targets: Vec::new(),
      text: String::new(),
      max_rounds: limits.max_glob_iterations(),
    };
    run.resolve(declarations);
    start = end;
  }
}

/// A `use` item, as its module's fixpoints read it.
struct Item<'x> {
  /// Its index among the declarations' `use` items, which orders every item of the analysis.
  entry: usize,
  /// The first segment of its path, which its module's scope resolves; `None` for a tree that
  /// starts at a root or at a string source.
  head: Option<Head<'x>>,
  state: State,
}

/// The first segment of a `use` item's path, written with no root.
#[derive(Clone, Copy)]
struct Head<'x> {
  name: &'x [u8],
  at: Location,
  /// Whether the path is the head alone, `use name` with or without `as`, which binds the name in
  /// each namespace the scope has it in; otherwise a module is wanted.
  whole: bool,
}

/// Where a `use` item is in its module's resolution.
#[derive(Clone, Copy)]
enum State {
  /// Not resolved yet.
  Pending,
  /// Resolved: with what its head found, when it has one.
  Resolved(Option<Found>),
  /// Resolved to one thing, then found to name another once every item was resolved: reported,
  /// and what its tree failed to find is not.
  Changed,
  /// Never resolved: reported at its head.
  Failed,
  /// Left unresolved when its module's glob fixpoint passed its limit, which is reported once.
  Stopped,
}

/// What a head found: in the type namespace, and, for a path of the head alone, in the directive
/// namespace.
#[derive(Clone, Copy)]
struct Found {
  ty: Lookup,
  directive: Lookup,
}

/// The tree of a `use` item, of the parser that read its file.
enum Tree<'x, 'a> {
  Text(&'x UseTree<&'a str>),
  Bytes(&'x UseTree<&'a [u8]>),
}

/// What a tree's path starts at, before its own segments are read.
#[derive(Clone, Copy)]
enum Start<'x> {
  /// The item's head: the path's first segment, which the module's scope resolved.
  Head,
  /// A module, which the path names `within`.
  Module(ModuleId, Within<'x>),
  /// `^::`: the first segment names a package.
  Packages,
}

/// One tree of a `use` item being walked.
struct Frame<'x, S> {
  tree: &'x UseTree<S>,
  /// What a relative path of the tree starts at: `None` for the item's own tree, whose relative
  /// path starts at its head.
  base: Option<Start<'x>>,
  /// How many groups enclose it.
  depth: u32,
}

/// The item whose tree is being walked.
struct Cursor {
  /// Its index among its module's items.
  at: usize,
  /// The file it is written in.
  source: SourceId,
  /// Its key among every binding and glob: its index among the declarations' `use` items.
  item: u32,
  /// How many names it has bound so far, which orders its bindings.
  seq: u32,
  /// Whether a group of its tree nested past `Limits::max_use_depth`, which is reported once.
  deep: bool,
}

/// One module's `use` items being resolved.
struct Run<'r, 'x> {
  scopes: &'r mut Scopes<'x>,
  names: &'r mut ModelBuilder,
  reporter: &'r mut Reporter,
  module: ModuleId,
  /// The index of the module's package.
  package: usize,
  items: Vec<Item<'x>>,
  /// What the items' trees failed to find, each with its item's index among `items`: reported
  /// once every item is checked, but for an item whose resolution changed.
  failures: Vec<(usize, Failure<'x>)>,
  /// The names bound first in their namespace of the module since they were last taken.
  fresh: Vec<Sym>,
  /// The modules a glob of the module named first since they were last taken.
  targets: Vec<ModuleId>,
  /// A string source's text, its escapes decoded.
  text: String,
  /// `Limits::max_glob_iterations`.
  max_rounds: u32,
}

impl<'x> Run<'_, 'x> {
  /// Resolves the module's items, checks what they bind, and reports.
  fn resolve(&mut self, declarations: &'x Declarations<'_, '_>) {
    self.explicit(declarations);
    self.globs(declarations);
    self.recheck();
    self.collisions();
    self.report();
  }

  /// The explicit fixpoint: each item whose head the module's declarations, child modules and
  /// explicit bindings answer, or which has no head, resolved as soon as it can be.
  fn explicit(&mut self, declarations: &'x Declarations<'_, '_>) {
    // Popped from the end, so the items are first tried in their order.
    let mut queue: Vec<usize> = (0..self.items.len()).rev().collect();
    let mut waiting: BTreeMap<&'x [u8], Vec<usize>> = BTreeMap::new();
    while let Some(at) = queue.pop() {
      let item = &self.items[at];
      if !matches!(item.state, State::Pending) {
        continue;
      }
      let found = match item.head {
        None => None,
        Some(head) => {
          let found = self.head(head, true, key(item.entry));
          if !settled(found, head.whole) {
            waiting.entry(head.name).or_default().push(at);
            continue;
          }
          Some(found)
        }
      };
      self.apply(declarations, at, found);
      for sym in core::mem::take(&mut self.fresh) {
        if let Some(woken) = waiting.remove(self.names.spelling(sym)) {
          queue.extend(woken);
        }
      }
    }
    // The globs made so far give nothing the explicit fixpoint reads, and the glob fixpoint's first
    // round looks every waiting item up whatever they give.
    self.targets.clear();
  }

  /// The glob fixpoint: rounds that each resolve every waiting item whose head the scope, as it
  /// stood when the round began, answers.
  fn globs(&mut self, declarations: &'x Declarations<'_, '_>) {
    let mut rounds: u32 = 0;
    // What the round before bound and globbed first; `None` before the first round, which looks
    // every waiting item up.
    let mut woken: Option<(BTreeSet<Sym>, Vec<ModuleId>)> = None;
    loop {
      let mut decided: Vec<(usize, Found)> = Vec::new();
      for (at, item) in self.items.iter().enumerate() {
        let (State::Pending, Some(head)) = (item.state, item.head) else {
          continue;
        };
        if let Some((fresh, targets)) = &woken
          && !self.wakes(head, fresh, targets)
        {
          continue;
        }
        let found = self.head(head, false, key(item.entry));
        if answered(found, head.whole) {
          decided.push((at, found));
        }
      }
      let Some(&(first, _)) = decided.first() else {
        break;
      };
      if rounds == self.max_rounds {
        self.stop(first);
        break;
      }
      rounds += 1;
      for (at, found) in decided {
        self.apply(declarations, at, Some(found));
      }
      let fresh: BTreeSet<Sym> = core::mem::take(&mut self.fresh).into_iter().collect();
      let targets: Vec<ModuleId> = core::mem::take(&mut self.targets);
      woken = Some((fresh, targets));
    }
    for item in &mut self.items {
      if matches!(item.state, State::Pending) {
        item.state = State::Failed;
      }
    }
  }

  /// Whether a round that bound `fresh` first and globbed `targets` first can change what `head`
  /// finds: by a binding of its name, or by a glob of a module that has it.
  fn wakes(&self, head: Head<'x>, fresh: &BTreeSet<Sym>, targets: &[ModuleId]) -> bool {
    let Some(sym) = self.names.find(head.name) else {
      return false;
    };
    fresh.contains(&sym)
      || targets.iter().any(|&target: &ModuleId| {
        self
          .scopes
          .declared(target, TYPE, sym, !head.whole)
          .is_some()
          || (head.whole
            && self
              .scopes
              .declared(target, DIRECTIVE, sym, false)
              .is_some())
      })
  }

  /// Reports that the module's glob fixpoint would pass its limit, at the head of the item at
  /// `at`, the first the next round would resolve, and leaves every waiting item unresolved.
  fn stop(&mut self, at: usize) {
    if let Some(head) = self.items[at].head {
      let kind = DiagnosticKind::ResourceLimit;
      self.reporter.report(
        Diagnostic::builder(kind.code(), head.at)
          .severity(kind.severity())
          .message(format!(
            "resolving the `use` items of this module would take more than {} rounds of the glob \
             fixpoint",
            self.max_rounds
          ))
          .primary_label(kind.title())
          .help(
            "raise `Limits::max_glob_iterations`, or write from a root the paths that wait on \
             what this module's globs give",
          )
          .build(),
      );
    }
    for item in &mut self.items {
      if matches!(item.state, State::Pending) {
        item.state = State::Stopped;
      }
    }
  }

  /// Step 4: looks every resolved head up again in the final scope, and reports each that finds
  /// something else.
  fn recheck(&mut self) {
    let resolved: Vec<(usize, Head<'x>, Found)> = self
      .items
      .iter()
      .enumerate()
      .filter_map(
        |(at, item): (usize, &Item<'x>)| match (item.state, item.head) {
          (State::Resolved(Some(found)), Some(head)) => Some((at, head, found)),
          _ => None,
        },
      )
      .collect();
    for (at, head, was) in resolved {
      let now = self.head(head, false, key(self.items[at].entry));
      if was.ty.same(now.ty) && was.directive.same(now.directive) {
        continue;
      }
      self.items[at].state = State::Changed;
      self.changed(at, head, was, now);
    }
  }

  /// Reports the item at `at`, whose head found `was` when it was resolved and finds `now`.
  fn changed(&mut self, at: usize, head: Head<'x>, was: Found, now: Found) {
    let (namespace, before, after) = if was.ty.same(now.ty) {
      (DIRECTIVE, was.directive, now.directive)
    } else {
      (TYPE, was.ty, now.ty)
    };
    let kind = DiagnosticKind::UseResolutionChanged;
    let mut builder = Diagnostic::builder(kind.code(), head.at)
      .severity(kind.severity())
      .message(format!(
        "`{}` named {} when this `use` was resolved, and names {} once every `use` of its module \
         is",
        Excerpt(head.name),
        provenance(before),
        provenance(after)
      ))
      .primary_label(kind.title())
      .help(
        "a `use` may not hang on the order its module's `use` items are resolved in: write its \
         path from a root, `~::`, `^::` or `..::`, or bind the name it starts with otherwise",
      );
    if let Lookup::Found(_, Level::Explicit) = after
      && let Some(sym) = self.names.find(head.name)
      && let Some(bound) =
        self
          .scopes
          .bound(self.module, namespace, sym, Some(key(self.items[at].entry)))
    {
      builder = builder.label(bound.at, "this `use` binds it once every `use` is resolved");
    }
    self.reporter.report(builder.build());
  }

  /// Step 5: reports every explicit binding of the module that collides with a declaration of its
  /// name, and every one that gives a name another item than the first binding of it does.
  fn collisions(&mut self) {
    let module = self.module;
    let from: ExplicitKey = (module, 0, Sym::new(0), 0, 0);
    let to: ExplicitKey = (module, u8::MAX, Sym::new(u32::MAX), u32::MAX, u32::MAX);
    let bound: Vec<(u8, Sym, Bound)> = self
      .scopes
      .explicit
      .range(from..=to)
      .map(|(key, bound): (&ExplicitKey, &Bound)| (key.1, key.2, *bound))
      .collect();
    let mut first: Option<(u8, Sym, Bound)> = None;
    for (namespace, sym, binding) in bound {
      if let Some(declared) = self.scopes.declared(module, namespace, sym, false) {
        self.declaration_collision(namespace, sym, binding, declared);
      }
      match first {
        Some((space, name, earlier)) if space == namespace && name == sym => {
          if earlier.target != binding.target {
            self.name_collision(namespace, sym, earlier, binding);
          }
        }
        _ => first = Some((namespace, sym, binding)),
      }
    }
  }

  /// Reports `binding`, of `sym` in `namespace`, which the module declares as `declared` too.
  fn declaration_collision(&mut self, namespace: u8, sym: Sym, binding: Bound, declared: Target) {
    let kind = DiagnosticKind::UseDeclarationCollision;
    let message = format!(
      "this `use` binds {}, which its module declares as well",
      quoted(spoken(namespace), self.names.spelling(sym))
    );
    let mut builder = Diagnostic::builder(kind.code(), binding.at)
      .severity(kind.severity())
      .message(message)
      .primary_label(kind.title());
    builder = match declared {
      Target::Def(def) => builder.label(self.names.def_location(def), "declared here"),
      Target::Module(child) => match self.scopes.modules[child.get() as usize].declared {
        Some(location) => builder.label(location, "the module is declared here"),
        None => builder.label(
          Location::entire(self.scopes.packages[self.package].source),
          "a directory of this package is the module of that name",
        ),
      },
      // A module declares no `NonZeroIN`: only the prelude answers it.
      Target::NonZero(_) => builder,
    };
    self.reporter.report(
      builder
        .help(
          "a name in a module's scope names one thing: rename the declaration, or bind the item \
           under another name with `as`",
        )
        .build(),
    );
  }

  /// Reports `later`, a binding of `sym` in `namespace` to another item than `first` gives it.
  fn name_collision(&mut self, namespace: u8, sym: Sym, first: Bound, later: Bound) {
    let kind = DiagnosticKind::UseNameCollision;
    let message = format!(
      "two `use` items bind {} to two different items",
      quoted(spoken(namespace), self.names.spelling(sym))
    );
    self.reporter.report(
      Diagnostic::builder(kind.code(), later.at)
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .label(first.at, "first bound here")
        .help(
          "a name in a module's scope names one thing: bind one of the items under another name \
           with `as`",
        )
        .build(),
    );
  }

  /// Step 6: reports each item that never resolved, at its head, and what each tree failed to
  /// find, but for an item whose resolution changed.
  fn report(&mut self) {
    let items = core::mem::take(&mut self.items);
    let failures = core::mem::take(&mut self.failures);
    for item in &items {
      if let (State::Failed, Some(head)) = (item.state, item.head) {
        let why = if head.whole {
          Why::NothingInScope {
            module: self.module,
          }
        } else {
          Why::NotInScope {
            module: self.module,
            wanted: Wanted::Module,
          }
        };
        let failure = Failure {
          at: head.at,
          name: head.name,
          why,
          single: false,
        };
        self.fail_now(&failure);
      }
    }
    for (at, failure) in &failures {
      if !matches!(items[*at].state, State::Changed) {
        self.fail_now(failure);
      }
    }
  }

  /// Reports `failure`, a path of a `use` item of the module.
  fn fail_now(&mut self, failure: &Failure<'_>) {
    self.scopes.report(
      &*self.names,
      &mut *self.reporter,
      self.module,
      Position::Other,
      &[],
      failure,
    );
  }

  /// What `head` finds in the module's scope, the bindings and the globs of the item `exclude`
  /// left out: in the type namespace, where a module is wanted unless the path is the head alone,
  /// and for a path of the head alone in the directive namespace as well. With `explicit_only`,
  /// only the module's declarations, child modules and explicit bindings answer.
  fn head(&self, head: Head<'x>, explicit_only: bool, exclude: u32) -> Found {
    let query = Query {
      module: self.module,
      name: head.name,
      sym: self.names.find(head.name),
      namespace: TYPE,
      module_first: !head.whole,
      packages: true,
      explicit_only,
      exclude: Some(exclude),
    };
    let ty = self.scopes.lookup(query);
    let directive = if head.whole {
      self.scopes.lookup(Query {
        namespace: DIRECTIVE,
        module_first: false,
        packages: false,
        ..query
      })
    } else {
      Lookup::Missing
    };
    Found { ty, directive }
  }

  /// Resolves the item at `at`, whose head found `found` when it has one: walks its tree, binding
  /// each name it binds and making each glob it makes, and keeps what it fails to find.
  fn apply(&mut self, declarations: &'x Declarations<'_, '_>, at: usize, found: Option<Found>) {
    self.items[at].state = State::Resolved(found);
    let entry = &declarations.uses[self.items[at].entry];
    let mut cursor = Cursor {
      at,
      source: entry.item.source,
      item: key(self.items[at].entry),
      seq: 0,
      deep: false,
    };
    match tree_of(declarations, entry) {
      Some(Tree::Text(tree)) => self.walk(&mut cursor, tree, found),
      Some(Tree::Bytes(tree)) => self.walk(&mut cursor, tree, found),
      // Every item is made from a `use` item's tree.
      None => {}
    }
  }

  /// Walks `tree`, the tree of the item `cursor` names, with an explicit stack of its groups.
  fn walk<S: AsRef<[u8]>>(
    &mut self,
    cursor: &mut Cursor,
    tree: &'x UseTree<S>,
    found: Option<Found>,
  ) {
    let source = cursor.source;
    let mut stack: Vec<Frame<'x, S>> = vec![Frame {
      tree,
      base: None,
      depth: 0,
    }];
    while let Some(frame) = stack.pop() {
      let whole = location(source, *frame.tree.span());
      let Some((start, segments)) = self.start(cursor, &frame) else {
        continue;
      };
      match frame.tree.kind() {
        UseTreeKind::Simple { alias } => {
          self.simple(cursor, start, segments, alias.as_ref(), whole, found);
        }
        UseTreeKind::Glob => match self.reach(cursor, start, segments, found) {
          Some(Start::Module(module, _)) => self.glob(cursor, module, whole),
          Some(Start::Packages) => self.fail(
            cursor.at,
            Failure {
              at: whole,
              name: b"^::*",
              why: Why::PackageGlob,
              single: false,
            },
          ),
          Some(Start::Head) | None => {}
        },
        UseTreeKind::Group(trees) => {
          let Some(base) = self.reach(cursor, start, segments, found) else {
            continue;
          };
          let depth = frame.depth.saturating_add(1);
          if depth > self.scopes.max_use_depth {
            if !cursor.deep {
              cursor.deep = true;
              self.fail(
                cursor.at,
                Failure {
                  at: whole,
                  name: b"",
                  why: Why::TooDeep(depth),
                  single: false,
                },
              );
            }
            continue;
          }
          // The last first, so that the members are walked, and bind, in their order.
          for member in trees.as_slice().iter().rev() {
            stack.push(Frame {
              tree: member,
              base: Some(base),
              depth,
            });
          }
        }
        // `UseTreeKind` is `#[non_exhaustive]`: a tree of a kind this analyzer does not know binds
        // nothing it can find, and is reported as an item of an unknown kind is.
        _ => self.fail(
          cursor.at,
          Failure {
            at: whole,
            name: b"",
            why: Why::Unknown,
            single: false,
          },
        ),
      }
    }
  }

  /// What `frame`'s path starts at, and the segments to read from there: its root's module, the
  /// module its string source names, its group's module, or the item's head. `None` once what
  /// fails is kept.
  fn start<S: AsRef<[u8]>>(
    &mut self,
    cursor: &Cursor,
    frame: &Frame<'x, S>,
  ) -> Option<(Start<'x>, &'x [Name<S>])> {
    let source = cursor.source;
    let tree: &'x UseTree<S> = frame.tree;
    let whole = location(source, *tree.span());
    let none: &'x [Name<S>] = Default::default();
    match tree.prefix() {
      UsePrefix::Root(root) => {
        let start = self.root(cursor, root, whole)?;
        Some((start, none))
      }
      UsePrefix::Path(path) => {
        let segments: &'x [Name<S>] = path.segments();
        if segments.len() > self.scopes.max_segments as usize {
          let failure = Failure {
            at: location(source, *path.span()),
            name: b"",
            why: Why::TooLong(segments.len()),
            single: false,
          };
          self.fail(cursor.at, failure);
          return None;
        }
        let start = match path.root() {
          PathRoot::Relative => frame.base.unwrap_or(Start::Head),
          root => self.root(cursor, root, whole)?,
        };
        Some((start, segments))
      }
      UsePrefix::Source {
        source: string,
        segments,
      } => {
        let raw: &'x [u8] = string.source().as_ref();
        let at = location(source, *string.span());
        let module = match self.directory(source, raw) {
          Ok(module) => module,
          Err(why) => {
            let failure = Failure {
              at,
              name: raw,
              why,
              single: false,
            };
            self.fail(cursor.at, failure);
            return None;
          }
        };
        let segments: &'x [Name<S>] = segments.as_slice();
        if segments.len() > self.scopes.max_segments as usize {
          let failure = Failure {
            at,
            name: b"",
            why: Why::TooLong(segments.len()),
            single: false,
          };
          self.fail(cursor.at, failure);
          return None;
        }
        Some((Start::Module(module, Within::After(raw)), segments))
      }
      // `UsePrefix` is `#[non_exhaustive]`: a prefix of a kind this analyzer does not know names no
      // module it can find, and is reported as an item of an unknown kind is.
      _ => {
        let failure = Failure {
          at: whole,
          name: b"",
          why: Why::Unknown,
          single: false,
        };
        self.fail(cursor.at, failure);
        None
      }
    }
  }

  /// Where the root `root` of a path of the tree written at `whole` starts. `None` once what fails
  /// is kept.
  fn root(&mut self, cursor: &Cursor, root: &PathRoot, whole: Location) -> Option<Start<'x>> {
    match root {
      PathRoot::Package(_) => Some(Start::Module(self.scopes.roots[self.package], Within::Own)),
      PathRoot::Dependency(_) => Some(Start::Packages),
      PathRoot::Parent { span, count } => match self.scopes.up(self.module, count.get()) {
        Some(module) => Some(Start::Module(module, Within::Parent)),
        None => {
          let failure = Failure {
            at: location(cursor.source, *span),
            name: b"..",
            why: Why::AboveRoot,
            single: false,
          };
          self.fail(cursor.at, failure);
          None
        }
      },
      // A relative path has no root to start at. `PathRoot` is `#[non_exhaustive]`: a root of a
      // kind this analyzer does not know names no module it can find, and is reported as an item
      // of an unknown kind is.
      _ => {
        let failure = Failure {
          at: whole,
          name: b"",
          why: Why::Unknown,
          single: false,
        };
        self.fail(cursor.at, failure);
        None
      }
    }
  }

  /// The module `segments` name from `start`, each a child module of the module before it, or
  /// `Start::Packages` when `start` is `^::` and no segment follows. `None` once what fails is
  /// kept.
  fn reach<S: AsRef<[u8]>>(
    &mut self,
    cursor: &Cursor,
    start: Start<'x>,
    segments: &'x [Name<S>],
    found: Option<Found>,
  ) -> Option<Start<'x>> {
    let source = cursor.source;
    let (mut module, mut within, rest): (ModuleId, Within<'x>, &'x [Name<S>]) = match start {
      Start::Module(module, within) => (module, within, segments),
      Start::Packages => {
        let Some((first, rest)) = segments.split_first() else {
          return Some(Start::Packages);
        };
        let name: &'x [u8] = first.source().as_ref();
        let Some(root) = self.scopes.package_root(self.package, name) else {
          self.fail(cursor.at, failure_at(source, first, Why::UnknownPackage));
          return None;
        };
        (root, Within::After(name), rest)
      }
      Start::Head => {
        let (first, rest) = segments.split_first()?;
        let name: &'x [u8] = first.source().as_ref();
        let ty = found.map_or(Lookup::Missing, |found: Found| found.ty);
        let module = match ty {
          Lookup::Found(Target::Module(module), _) => module,
          Lookup::Found(target, _) => {
            let why = Why::WrongKind {
              found: target,
              wanted: Wanted::Module,
            };
            self.fail(cursor.at, failure_at(source, first, why));
            return None;
          }
          Lookup::Ambiguous(one, other) => {
            let why = Why::Ambiguous {
              first: one,
              second: other,
            };
            self.fail(cursor.at, failure_at(source, first, why));
            return None;
          }
          Lookup::Missing => {
            let why = Why::NotInScope {
              module: self.module,
              wanted: Wanted::Module,
            };
            self.fail(cursor.at, failure_at(source, first, why));
            return None;
          }
        };
        (module, Within::After(name), rest)
      }
    };
    for segment in rest {
      let name: &'x [u8] = segment.source().as_ref();
      let named = self
        .names
        .find(name)
        .and_then(|sym: Sym| self.scopes.declared(module, TYPE, sym, true));
      match named {
        Some(Target::Module(child)) => {
          module = child;
          within = Within::After(name);
        }
        Some(target) => {
          let why = Why::WrongKind {
            found: target,
            wanted: Wanted::Module,
          };
          self.fail(cursor.at, failure_at(source, segment, why));
          return None;
        }
        None => {
          let why = Why::NotIn {
            module,
            within,
            wanted: Wanted::Module,
          };
          self.fail(cursor.at, failure_at(source, segment, why));
          return None;
        }
      }
    }
    Some(Start::Module(module, within))
  }

  /// Binds what a simple tree, written at `whole`, names from `start` through `segments`: under
  /// `alias`, or the name of the last segment, or, with no segment, the name of the module its
  /// string source names.
  fn simple<S: AsRef<[u8]>>(
    &mut self,
    cursor: &mut Cursor,
    start: Start<'x>,
    segments: &'x [Name<S>],
    alias: Option<&'x Name<S>>,
    whole: Location,
    found: Option<Found>,
  ) {
    let source = cursor.source;
    let aliased: Option<(&'x [u8], Location)> =
      alias.map(|name: &'x Name<S>| (name.source().as_ref(), location(source, *name.as_span())));
    let Some((last, before)) = segments.split_last() else {
      // A string source alone, which binds the module of its directory.
      let Start::Module(module, within) = start else {
        return;
      };
      match aliased {
        Some((name, at)) => self.bind_name(cursor, TYPE, name, at, Target::Module(module)),
        None => match self.scopes.modules[module.get() as usize].name {
          Some(name) => self.bind(cursor, TYPE, name, whole, Target::Module(module)),
          None => {
            let raw: &'x [u8] = match within {
              Within::After(raw) => raw,
              Within::Own | Within::Parent => b"",
            };
            let failure = Failure {
              at: whole,
              name: raw,
              why: Why::Unnamed,
              single: false,
            };
            self.fail(cursor.at, failure);
          }
        },
      }
      return;
    };
    let last_name: &'x [u8] = last.source().as_ref();
    let (name, at) = aliased.unwrap_or_else(|| (last_name, location(source, *last.as_span())));
    if before.is_empty() && matches!(start, Start::Head) {
      if let Some(found) = found {
        self.whole(cursor, name, at, last, found);
      }
      return;
    }
    match self.reach(cursor, start, before, found) {
      Some(Start::Packages) => match self.scopes.package_root(self.package, last_name) {
        Some(root) => self.bind_name(cursor, TYPE, name, at, Target::Module(root)),
        None => self.fail(cursor.at, failure_at(source, last, Why::UnknownPackage)),
      },
      Some(Start::Module(module, within)) => {
        let sym = self.names.find(last_name);
        let ty = sym.and_then(|sym: Sym| self.scopes.declared(module, TYPE, sym, false));
        let directive =
          sym.and_then(|sym: Sym| self.scopes.declared(module, DIRECTIVE, sym, false));
        let mut bound = false;
        let mut unavailable: Option<Primitive> = None;
        if let Some(target) = ty {
          match self.unavailable(target) {
            None => {
              self.bind_name(cursor, TYPE, name, at, target);
              bound = true;
            }
            Some(primitive) => unavailable = Some(primitive),
          }
        }
        if let Some(target) = directive {
          self.bind_name(cursor, DIRECTIVE, name, at, target);
          bound = true;
        }
        if !bound {
          let why = match unavailable {
            Some(primitive) => Why::Unavailable(primitive),
            None => Why::NotDeclared { module, within },
          };
          self.fail(cursor.at, failure_at(source, last, why));
        }
      }
      Some(Start::Head) | None => {}
    }
  }

  /// Binds `name`, written at `at`, to what the head `head`, a path's one segment, found in each
  /// namespace.
  fn whole<S: AsRef<[u8]>>(
    &mut self,
    cursor: &mut Cursor,
    name: &'x [u8],
    at: Location,
    head: &'x Name<S>,
    found: Found,
  ) {
    let mut bound = false;
    let mut unavailable: Option<Primitive> = None;
    if let Lookup::Found(target, _) = found.ty {
      match self.unavailable(target) {
        None => {
          self.bind_name(cursor, TYPE, name, at, target);
          bound = true;
        }
        Some(primitive) => unavailable = Some(primitive),
      }
    }
    if let Lookup::Found(target, _) = found.directive {
      self.bind_name(cursor, DIRECTIVE, name, at, target);
      bound = true;
    }
    let why = match (found.ty, found.directive, unavailable) {
      (Lookup::Ambiguous(first, second), _, _) | (_, Lookup::Ambiguous(first, second), _) => {
        Some(Why::Ambiguous { first, second })
      }
      _ if bound => None,
      (_, _, Some(primitive)) => Some(Why::Unavailable(primitive)),
      _ => Some(Why::NothingInScope {
        module: self.module,
      }),
    };
    if let Some(why) = why {
      self.fail(cursor.at, failure_at(cursor.source, head, why));
    }
  }

  /// The primitive `target` is, when the module's package may not have its width.
  fn unavailable(&self, target: Target) -> Option<Primitive> {
    let (Target::Def(def) | Target::NonZero(def)) = target else {
      return None;
    };
    let primitive = self.scopes.primitive_of(def)?;
    let integers = self.scopes.packages[self.package].integers;
    (!available(primitive, integers)).then_some(primitive)
  }

  /// Interns `name` and binds it, written at `at`, in `namespace` of the module to `target`.
  fn bind_name(
    &mut self,
    cursor: &mut Cursor,
    namespace: u8,
    name: &[u8],
    at: Location,
    target: Target,
  ) {
    let sym = self.names.intern(&String::from_utf8_lossy(name));
    self.bind(cursor, namespace, sym, at, target);
  }

  /// Binds `sym`, written at `at`, in `namespace` of the module to `target`, for the item `cursor`
  /// names.
  fn bind(&mut self, cursor: &mut Cursor, namespace: u8, sym: Sym, at: Location, target: Target) {
    let module = self.module;
    let first = self
      .scopes
      .explicit
      .range((module, namespace, sym, 0, 0)..=(module, namespace, sym, u32::MAX, u32::MAX))
      .next()
      .is_none();
    self.scopes.explicit.insert(
      (module, namespace, sym, cursor.item, cursor.seq),
      Bound { target, at },
    );
    cursor.seq = cursor.seq.saturating_add(1);
    if first {
      self.fresh.push(sym);
    }
  }

  /// Makes a glob, written at `at`, of the module `target`, for the item `cursor` names.
  fn glob(&mut self, cursor: &Cursor, target: ModuleId, at: Location) {
    let module = self.module;
    let first = self
      .scopes
      .globs
      .range((module, target, 0)..=(module, target, u32::MAX))
      .next()
      .is_none();
    self.scopes.globs.insert((module, target, cursor.item), at);
    if first {
      self.targets.push(target);
    }
  }

  /// Keeps `failure` of the item at `at`, to report once every item is checked.
  fn fail(&mut self, at: usize, failure: Failure<'x>) {
    self.failures.push((at, failure));
  }

  /// The module the string source `raw`, written in the file `source`, names: its directory's.
  ///
  /// The source is decoded, then walked from the directory module of `source`, the module of the
  /// directory the file is in, whatever `mod` the item is written in: it starts with `.` or `..`,
  /// each `/`-separated segment after that is `.`, `..` or a name, and one `/` may end it. `.`
  /// stays, `..` goes up to the parent module, which a root module has none of, and a name goes down
  /// to the child module of that name; a name that is no child module, but a file of the directory
  /// reached, names that file. The module reached must be a directory's. A package of the `Flat`
  /// layout has no directory module, and a URL is not supported.
  ///
  /// Each segment costs a lookup of its own spelling, in the child modules and in the files of one
  /// directory, which `scopes` tabled once: nothing read here is proportional to the length of the
  /// file's path.
  fn directory(&mut self, source: SourceId, raw: &'x [u8]) -> Result<ModuleId, Why<'x>> {
    if self.scopes.packages[self.package].layout == Layout::Flat {
      return Err(Why::Flat);
    }
    self.text.clear();
    if decode_string_value(raw, &mut self.text).is_err() {
      return Err(Why::NotRelative);
    }
    if is_url(&self.text) {
      return Err(Why::Url);
    }
    let mut module = self.scopes.directory_of(source).ok_or(Why::NoDirectory)?;
    let text: &str = self.text.strip_suffix('/').unwrap_or(&self.text);
    for (index, part) in text.split('/').enumerate() {
      match part {
        "." => {}
        ".." => match self.scopes.modules[module.get() as usize].parent {
          Some(parent) => module = parent,
          None => return Err(Why::Outside),
        },
        _ if index == 0 || part.is_empty() => return Err(Why::NotRelative),
        name => {
          let name = name.as_bytes();
          let child = self
            .names
            .find(name)
            .and_then(|sym: Sym| self.scopes.child(module, sym));
          module = match child {
            Some(child) => child,
            None => {
              return Err(match self.scopes.named_file(module, name) {
                Some(directory) => Why::File { directory },
                None => Why::NoDirectory,
              });
            }
          };
        }
      }
    }
    match self.scopes.modules[module.get() as usize].kind {
      ModuleKind::Root | ModuleKind::Directory => Ok(module),
      ModuleKind::Inline => Err(Why::NoDirectory),
    }
  }
}

/// The `use` item at `index` among the declarations' `use` items, with its head.
fn item_at<'x>(declarations: &'x Declarations<'_, '_>, index: usize) -> Option<Item<'x>> {
  let entry = declarations.uses.get(index)?;
  let source = entry.item.source;
  let head = match tree_of(declarations, entry)? {
    Tree::Text(tree) => head_of(tree, source),
    Tree::Bytes(tree) => head_of(tree, source),
  };
  Some(Item {
    entry: index,
    head,
    state: State::Pending,
  })
}

/// The tree of the `use` item `entry` addresses.
fn tree_of<'x, 'a>(declarations: &'x Declarations<'_, 'a>, entry: &Entry) -> Option<Tree<'x, 'a>> {
  let index = entry.item.index as usize;
  match declarations.items.get(entry.item.source.get() as usize)? {
    Some(Items::Text(items)) => match items.get(index)? {
      TypeSystemItem::Use(definition) => Some(Tree::Text(definition.tree())),
      // Item collection addresses only `use` items among the declarations' `use` items.
      _ => None,
    },
    Some(Items::Bytes(items)) => match items.get(index)? {
      TypeSystemItem::Use(definition) => Some(Tree::Bytes(definition.tree())),
      _ => None,
    },
    None => None,
  }
}

/// The head of `tree`, a `use` item's tree in the file `source`: its path's first segment, when
/// the path has no root.
fn head_of<S: AsRef<[u8]>>(tree: &UseTree<S>, source: SourceId) -> Option<Head<'_>> {
  let UsePrefix::Path(path) = tree.prefix() else {
    return None;
  };
  if !path.root().is_relative() {
    return None;
  }
  let segments = path.segments();
  let first = segments.first()?;
  Some(Head {
    name: first.source().as_ref(),
    at: location(source, *first.as_span()),
    whole: segments.len() == 1 && matches!(tree.kind(), UseTreeKind::Simple { .. }),
  })
}

/// The key of the `use` item at `entry` among the declarations' `use` items.
fn key(entry: usize) -> u32 {
  dense(entry)
}

/// Whether `found`, which the explicit fixpoint looked up, is final: what nothing resolved later
/// can change, which is the type namespace's answer, and the directive namespace's too for a path
/// of the head alone.
fn settled(found: Found, whole: bool) -> bool {
  let ty = matches!(found.ty, Lookup::Found(..));
  if whole {
    ty && matches!(found.directive, Lookup::Found(..))
  } else {
    ty
  }
}

/// Whether `found` resolves its item: something in the type namespace, or, for a path of the head
/// alone, in either.
fn answered(found: Found, whole: bool) -> bool {
  !matches!(found.ty, Lookup::Missing) || (whole && !matches!(found.directive, Lookup::Missing))
}

/// Where a lookup's answer came from, as a message names it.
fn provenance(lookup: Lookup) -> &'static str {
  match lookup {
    Lookup::Found(_, Level::Declared) => "a declaration of its module",
    Lookup::Found(_, Level::Explicit) => "what a `use` item binds",
    Lookup::Found(_, Level::Glob) => "what a glob gives",
    Lookup::Found(_, Level::Prelude) => "a name of the prelude",
    Lookup::Found(_, Level::Package) => "a package",
    Lookup::Ambiguous(..) => "two items two globs give",
    Lookup::Missing => "nothing",
  }
}

/// What a binding in `namespace` names, as a message quotes it.
fn spoken(namespace: u8) -> Wanted {
  if namespace == DIRECTIVE {
    Wanted::Directive
  } else {
    Wanted::Type
  }
}

/// A failure at the segment `name` of a `use` item in the file `source`.
fn failure_at<'x, S: AsRef<[u8]>>(
  source: SourceId,
  name: &'x Name<S>,
  why: Why<'x>,
) -> Failure<'x> {
  Failure {
    at: location(source, *name.as_span()),
    name: name.source().as_ref(),
    why,
    single: false,
  }
}

/// `span` of the file `source`.
fn location(source: SourceId, span: SimpleSpan) -> Location {
  Location::new(source.get(), span)
}

/// Whether `text` is a URL: a scheme, a letter then letters, digits, `+`, `-` or `.`, then `:`.
fn is_url(text: &str) -> bool {
  let Some((scheme, _)) = text.split_once(':') else {
    return false;
  };
  let mut bytes = scheme.bytes();
  bytes
    .next()
    .is_some_and(|byte: u8| byte.is_ascii_alphabetic())
    && bytes.all(|byte: u8| byte.is_ascii_alphanumeric() || matches!(byte, b'+' | b'-' | b'.'))
}
