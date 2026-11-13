//! Instantiation: the phase after lowering, which gives each definition its built-in meaning,
//! checks each definition's defaults where they are written, and gives every type occurrence its
//! type.
//!
//! It reads no document: it reads the rows lowering wrote, through the builder, and writes onto
//! them. It does four things, in this order: built-in meaning; each definition's defaults; every
//! occurrence's type; every application's count of type arguments.
//!
//! # Built-in meaning
//!
//! `core`'s `@marker`, `@set` and `@map` are found once, each by its name among the directives
//! `core`'s root module declares. An application means one of them when it resolved to that
//! definition, whatever path it is written with; a directive of the program's own means nothing
//! here, whatever its name. Each definition's applications, every contribution's, are then looked
//! at once: an interface that applies `@marker` is a marker, and a scalar that applies `@set` or
//! `@map` stands for that collection, the first of the two in application order when it applies
//! both. A definition of another kind gets neither: where a directive may stand is a rule's to
//! judge.
//!
//! # Defaults, where they are written
//!
//! Each definition's type parameters are read once. The least number of type arguments it takes is
//! one past its last parameter with no default, so every parameter from there on has a default and
//! a use leaves out a suffix of them. Defaults are trailing: the first parameter with no default
//! that follows one with a default is `generic-default`, since that default could never fill.
//!
//! A default may name any parameter of its definition, written before it or after it, but no
//! cycle among a definition's own defaults: a use that leaves out the lowest parameter on a cycle
//! leaves out the whole cycle, and could never be filled. The graph has a vertex for each
//! parameter from the least number on, and an edge from one to each such parameter its default's
//! written tree names, anywhere in it; a parameter below the least number is always written, and
//! is no vertex. Its strongly connected components are its groups, and a group holds a cycle when
//! an edge joins two of its members, or one to itself. Each such group is `generic-default` once,
//! at the default of its lowest parameter, which can never fill: a use that leaves that parameter
//! out leaves out the whole group. A default that only leads into a group says nothing, since a
//! use that writes the group's parameters fills it.
//!
//! The check costs one walk of each default's written tree, an edge for each parameter it names,
//! and one run of Tarjan's algorithm over them, all of it written by the definition, and holds its
//! tables only while the definition is checked.
//!
//! # Types
//!
//! Every type occurrence is then given its type, tree by tree in id order: an occurrence no tree
//! has typed yet is the root of the next one, which is a default, a bound's interface, an
//! `implements` entry, a union member, an application's type argument, a member's type or a root.
//! A tree is typed with explicit stacks, each occurrence after its written arguments, or after its
//! element, so a type is interned where the model's walk first needs it, its arguments before it.
//!
//! - A path that names nothing is the one unresolved type, and so is everything that holds one:
//!   name resolution reported it, and nothing more is said.
//! - A list is the list of its element's type.
//! - A type parameter is that parameter of its definition, in a default's own tree too, whatever
//!   its index; written with type arguments, it is `generic-arity`.
//! - An alias such as `NonZeroU32` is `NonZero` of the primitive it spells, told by what lowering
//!   noted of it and never by its spelling; written with type arguments, it is `generic-arity`.
//! - An occurrence of a definition writes no more type arguments than the definition has type
//!   parameters, and no fewer than it takes, three numbers compared, or it is `generic-arity`.
//!   Its written arguments' types are its first slots, a collection's key or element made non-null
//!   there however it is written, and the slots it leaves out are filled from their defaults. Its
//!   type is the named type of all its slots, in its own nullness, and when it is typed for its own
//!   sake and names no parameter, its type is listed among the definition's instantiations.
//!
//! # Filling what is left out
//!
//! An occurrence that leaves arguments out asks the fill table first: for each definition and the
//! types of the slots its occurrences write, the complete type, or why it could not be built. On a
//! miss it pays a step of substitution for each slot it leaves out, then fills its empty slots in
//! index order. Filling a slot types the written tree of that parameter's default, substituted for
//! the occurrence: every node of the tree costs a step, and a parameter in it reads the
//! occurrence's slot, filling an empty one first, there and then. A fill whose definition, written
//! count and parameter are those of a fill in progress is a cycle, and fails. A substituted node
//! writes no row and reports nothing: it fails at its first failure, and its failure takes with it
//! the occurrence whose slot its tree fills, which records why in the table, at the slot its own
//! order had reached. The occurrence typed for its own sake at the bottom reports: past a limit,
//! `resource-limit`; a cycle, inside a default's own tree, `generic-default`; anything else,
//! `generic-arity`, with a label at the default that cannot fill it.
//!
//! # What bounds it
//!
//! Nothing follows a type into its definition's members, so a definition that names itself with a
//! growing argument costs its written occurrences and no more. What a program's bytes do not pay
//! for is filling, and each fill is paid from `Limits::max_substitutions`: each distinct set of
//! written slots is filled once, every slot and every node its fills walk charged, and the table
//! answers every later occurrence with the same slots in a logarithm, each type's non-null form
//! made once. A type that nests deeper than `Limits::max_generic_depth`, a list counting a level,
//! and an instantiation of a generic definition past `Limits::max_instantiations`, both nullnesses
//! counting once, are refused; an occurrence that cannot be built for a limit is `resource-limit`
//! where it is written. The built-in library is typed whatever the limits, and fills nothing.
//!
//! # Applications
//!
//! An application whose directive resolved writes no more type arguments than the directive has
//! type parameters, and no fewer than it takes, or it is `generic-arity`, at its path. Its type
//! arguments are occurrences the walk types.

use std::{
  boxed::Box,
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec::Vec,
};

use smear_semantics::scc;
use tokora::diagnostic::Location;

use super::{
  builtin::{CORE, DIRECTIVE},
  code::DiagnosticKind,
  declare::{Binding, Declarations},
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{ApplicationId, DefId, Sym, TyId, TypeExprId, dense},
  limits::Limits,
  model::{Collection, DefKind, ModelBuilder, Resolution, Written},
  set::ROOT,
  source::SourceMap,
};

/// Instantiates what lowering left in `builder`, whose declarations are `declarations` and whose
/// sources `sources` numbers, within `limits`, reporting into `reporter`: each definition's
/// built-in meaning, then its defaults, checked where they are written, then every occurrence's
/// type, then every application's count of type arguments.
pub(crate) fn instantiate(
  declarations: &Declarations<'_, '_>,
  sources: &SourceMap,
  limits: &Limits,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  meaning(declarations, builder);
  let Defaults { least, roots } = defaults(builder, reporter);
  let mut typing = Typing {
    builder,
    reporter,
    least,
    roots,
    max_depth: limits.max_generic_depth(),
    max_instantiations: limits.max_instantiations(),
    max_substitutions: limits.max_substitutions(),
    supplied: sources.package_sources(ROOT).start(),
    table: BTreeMap::new(),
    progress: BTreeSet::new(),
    non_null: BTreeMap::new(),
    unresolved: None,
    counted: 0,
    spent: 0,
    frames: Vec::new(),
    values: Vec::new(),
    key: Vec::new(),
    slots: Vec::new(),
    library: false,
    in_default: false,
  };
  typing.trees();
  typing.applications();
}

/// Gives each definition its built-in meaning: an interface that applies `core`'s `@marker` is a
/// marker, and a scalar that applies `core`'s `@set` or `@map` stands for that collection.
fn meaning(declarations: &Declarations<'_, '_>, builder: &mut ModelBuilder) {
  let marker = directive(declarations, builder, b"marker");
  let set = directive(declarations, builder, b"set");
  let map = directive(declarations, builder, b"map");
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    match builder.def_kind(def) {
      Some(DefKind::Interface) => {
        if marker.is_some_and(|wanted: DefId| applies(builder, def, wanted)) {
          builder.set_marker(def);
        }
      }
      Some(DefKind::Scalar) => {
        let collection = builder
          .def_applications(def)
          .iter()
          .find_map(|application: &ApplicationId| stands_for(builder, *application, set, map));
        if let Some(collection) = collection {
          builder.set_collection(def, collection);
        }
      }
      _ => {}
    }
  }
}

/// The collection `application` makes the scalar that applies it stand for: a set when it applies
/// `set`, `core`'s `@set`, and a map when it applies `map`, `core`'s `@map`.
fn stands_for(
  builder: &ModelBuilder,
  application: ApplicationId,
  set: Option<DefId>,
  map: Option<DefId>,
) -> Option<Collection> {
  let Some((Resolution::Resolved(directive), _, _)) = builder.applied(application) else {
    return None;
  };
  if Some(directive) == set {
    Some(Collection::Set)
  } else if Some(directive) == map {
    Some(Collection::Map)
  } else {
    None
  }
}

/// Whether `def` applies `directive`, in any of its contributions.
fn applies(builder: &ModelBuilder, def: DefId, directive: DefId) -> bool {
  builder
    .def_applications(def)
    .iter()
    .any(|application: &ApplicationId| {
      matches!(
        builder.applied(*application),
        Some((Resolution::Resolved(found), _, _)) if found == directive
      )
    })
}

/// `core`'s directive named `name`: the one `core`'s root module declares, read off its binding
/// there. `None` when no name so spelled was ever interned, which an analysis's library rules out.
fn directive(
  declarations: &Declarations<'_, '_>,
  builder: &ModelBuilder,
  name: &[u8],
) -> Option<DefId> {
  let module = *declarations.roots.get(CORE)?;
  let sym = builder.find(name)?;
  let key = (module, DIRECTIVE, sym);
  let at = declarations
    .names
    .binary_search_by(|binding: &Binding| {
      (binding.module, binding.namespace, binding.name).cmp(&key)
    })
    .ok()?;
  declarations
    .names
    .get(at)
    .map(|binding: &Binding| binding.def)
}

/// What a group's report tells its author to do. It does not promise that the definition is legal
/// then: a group can hold a cycle that avoids its lowest parameter, which the next analysis
/// reports.
const GROUP_HELP: &str =
  "give this type parameter no default, and none to the type parameters written before it";

/// One type parameter, as the check of its definition's defaults reads it: its name, where its
/// name is written, and its default.
type Param = (Sym, Location, Option<TypeExprId>);

/// An edge of a definition's graph of defaults: the default of `from` names `to` at `at`, both
/// parameters counted from the least number of type arguments the definition takes.
#[derive(Clone, Copy)]
struct Edge {
  from: u32,
  to: u32,
  at: Location,
}

/// What the check of one definition's defaults uses, kept from one definition to the next so that
/// it is allocated once.
#[derive(Default)]
struct Scratch {
  /// The definition's type parameters, in order.
  params: Vec<Param>,
  /// The edges of its graph: by the parameter whose default holds each, then in pre-order of that
  /// default's tree.
  edges: Vec<Edge>,
  /// The occurrences of the default being walked that are still to visit.
  stack: Vec<TypeExprId>,
  /// For each group, at its lowest member, whether an edge joins two of its members, or one to
  /// itself.
  cyclic: Vec<bool>,
  /// For each group, at its lowest member, where that member is first named in the default of the
  /// lowest other member that names it.
  label: Vec<Option<Location>>,
}

/// What the check of the definitions' defaults hands the types.
struct Defaults {
  /// Each definition's least number of type arguments, by `DefId`, which a count compares with.
  least: Vec<u32>,
  /// The root of every default's tree, ascending, which tells the walk that a tree is a default.
  roots: Vec<TypeExprId>,
}

/// Checks every definition's defaults where they are written, reporting into `reporter`.
fn defaults(builder: &ModelBuilder, reporter: &mut Reporter) -> Defaults {
  let mut scratch = Scratch::default();
  let count = builder.def_count();
  let mut found = Defaults {
    least: Vec::with_capacity(count as usize),
    roots: Vec::new(),
  };
  for index in 0..count {
    let least = definition(builder, reporter, DefId::new(index), &mut scratch);
    found.least.push(least);
    // Defaults are appended in definition order, then parameter order, so their roots ascend.
    found
      .roots
      .extend(scratch.params.iter().filter_map(|param: &Param| param.2));
  }
  found
}

/// Checks the defaults of `def`: that they are trailing, and that no group of them needs each
/// other. Answers the least number of type arguments `def` takes.
fn definition(
  builder: &ModelBuilder,
  reporter: &mut Reporter,
  def: DefId,
  scratch: &mut Scratch,
) -> u32 {
  scratch.params.clear();
  for index in 0..builder.type_param_count(def) {
    if let Some(param) = builder.type_param(def, index) {
      scratch.params.push(param);
    }
  }
  let count = dense(scratch.params.len());
  // One past the last parameter with no default: every parameter from it on has one.
  let least = scratch
    .params
    .iter()
    .rposition(|param: &Param| param.2.is_none())
    .map_or(0, |at: usize| dense(at).saturating_add(1));
  trailing(builder, reporter, &scratch.params);
  if least < count {
    groups(builder, reporter, def, least, count, scratch);
  }
  least
}

/// Reports the first of `params` with no default that follows one with a default: type arguments
/// bind by position, so the default before it could never fill.
fn trailing(builder: &ModelBuilder, reporter: &mut Reporter, params: &[Param]) {
  let mut defaulted = false;
  let mut before: Option<TypeExprId> = None;
  for &(name, at, default) in params {
    match default {
      Some(root) => {
        defaulted = true;
        before = Some(root);
      }
      None if defaulted => {
        let kind = DiagnosticKind::GenericDefault;
        let mut report = Diagnostic::builder(kind.code(), at)
          .severity(kind.severity())
          .message(format!(
            "the type parameter `{}` has no default, and follows one that has: type arguments \
             bind by position, so that default could never fill",
            Excerpt(builder.spelling(name))
          ))
          .primary_label(kind.title())
          .help("give this type parameter a default, or remove the defaults written before it");
        // The parameter before it has a default: it is the first with none to follow one.
        if let Some(root) = before
          && let Some(written) = builder.occurrence(root)
        {
          report = report.label(written.location, "the default before it");
        }
        reporter.report(report.build());
        return;
      }
      None => before = None,
    }
  }
}

/// Reports each group of `def`'s defaults that need each other, once, at the default of its lowest
/// parameter. The graph's vertices are the parameters `least..count`, numbered from `least`.
fn groups(
  builder: &ModelBuilder,
  reporter: &mut Reporter,
  def: DefId,
  least: u32,
  count: u32,
  scratch: &mut Scratch,
) {
  // The edges, default by default, each default's tree walked once, in pre-order.
  scratch.edges.clear();
  for (from, param) in (0u32..).zip(scratch.params.iter().skip(least as usize)) {
    let Some(root) = param.2 else {
      continue;
    };
    scratch.stack.clear();
    scratch.stack.push(root);
    while let Some(id) = scratch.stack.pop() {
      let Some(occurrence) = builder.occurrence(id) else {
        continue;
      };
      match occurrence.written {
        Written::Param { def: owner, index } if owner == def && index >= least && index < count => {
          scratch.edges.push(Edge {
            from,
            to: index - least,
            at: occurrence.location,
          });
        }
        Written::List(element) => scratch.stack.push(element),
        _ => {}
      }
      // Popped first to last, so the tree is walked in pre-order.
      scratch.stack.extend(occurrence.args.iter().rev().copied());
    }
  }

  let vertices = count.saturating_sub(least) as usize;
  let edges: &[Edge] = &scratch.edges;
  let group = scc::components(vertices, |node: usize, successors: &mut Vec<u32>| {
    let first = edges.partition_point(|edge: &Edge| (edge.from as usize) < node);
    let row: &[Edge] = edges.get(first..).unwrap_or_default();
    successors.extend(
      row
        .iter()
        .take_while(|edge: &&Edge| edge.from as usize == node)
        .map(|edge: &Edge| edge.to),
    );
  });

  // One pass over the edges: which groups hold a cycle, and where each group's lowest member is
  // first named by another member, the lowest such member's default first, each in pre-order.
  scratch.cyclic.clear();
  scratch.cyclic.resize(vertices, false);
  scratch.label.clear();
  scratch.label.resize(vertices, None);
  for edge in edges {
    let (Some(&source), Some(&target)) =
      (group.get(edge.from as usize), group.get(edge.to as usize))
    else {
      continue;
    };
    if source != target {
      continue;
    }
    if let Some(cyclic) = scratch.cyclic.get_mut(target as usize) {
      *cyclic = true;
    }
    if edge.to == target
      && edge.from != edge.to
      && let Some(label) = scratch.label.get_mut(target as usize)
      && label.is_none()
    {
      *label = Some(edge.at);
    }
  }

  // One report a group that holds a cycle, in the order of its lowest parameter.
  let kind = DiagnosticKind::GenericDefault;
  for (node, &name) in group.iter().enumerate() {
    if name != dense(node) || !scratch.cyclic.get(node).copied().unwrap_or(false) {
      continue;
    }
    let Some(&(sym, _, Some(root))) = scratch.params.get(node + least as usize) else {
      continue;
    };
    let Some(default) = builder.occurrence(root) else {
      continue;
    };
    let spelled = Excerpt(builder.spelling(sym));
    // A group of more than one has an edge into its lowest member from another; one of a single
    // parameter is a default that names its own parameter.
    let label = scratch.label.get(node).copied().flatten();
    let message = match label {
      Some(_) => format!(
        "the default of `{spelled}` can never fill: a use that leaves `{spelled}` out leaves out \
         the whole group of type parameters whose defaults need each other"
      ),
      None => format!("the default of `{spelled}` names `{spelled}` itself, so it can never fill"),
    };
    let mut report = Diagnostic::builder(kind.code(), default.location)
      .severity(kind.severity())
      .message(message)
      .primary_label(kind.title())
      .help(GROUP_HELP);
    if let Some(at) = label {
      report = report.label(at, "named here, by a default of the same group");
    }
    reporter.report(report.build());
  }
}

/// What a type argument count's report tells its author.
const COUNT_HELP: &str =
  "write a type argument for each type parameter, leaving out only trailing defaulted ones";

/// What a type parameter written with type arguments is told.
const PARAM_ARGUMENTS: &str = "a type parameter takes no type argument";

/// What an alias written with type arguments is told.
const ALIAS_ARGUMENTS: &str =
  "an alias such as `NonZeroU32` names one `NonZero`, and takes no type argument";

/// The limit a type passed, which its report names.
#[derive(Clone, Copy)]
enum Limit {
  /// `Limits::max_generic_depth`.
  Depth,
  /// `Limits::max_instantiations`.
  Instantiations,
  /// `Limits::max_substitutions`.
  Substitutions,
}

/// Why a type could not be built, carried unchanged from where it arose to the occurrence typed
/// for its own sake that reports it.
#[derive(Clone, Copy)]
enum Reason {
  /// Filling a slot led back to a fill in progress.
  Cycle,
  /// What a default writes cannot be built as it is written: a path that names nothing, a count of
  /// type arguments, type arguments on a type parameter or an alias.
  Unusable,
  /// A limit.
  Limit(Limit),
}

/// Why an occurrence that leaves arguments out could not be filled: the reason, and the slot its
/// order had reached, whose default a report labels.
#[derive(Clone, Copy)]
struct Failure {
  reason: Reason,
  param: u32,
}

/// One step of the walk that types a tree.
#[derive(Clone, Copy)]
enum Frame {
  /// An occurrence being typed.
  Visit(Visit),
  /// A slot being filled from its parameter's default, whose tree is typed above it.
  Fill(Fill),
}

/// An occurrence being typed.
#[derive(Clone, Copy)]
struct Visit {
  /// The occurrence.
  id: TypeExprId,
  /// Where its results start on the value stack: one for each written child typed, and, once an
  /// occurrence of a definition misses the fill table, one slot for each type parameter.
  start: u32,
  /// Its next written child to type.
  next: u32,
  /// `None` when it is typed for its own sake; otherwise it is a node of a default substituted for
  /// the occurrence at this position on the frame stack, whose slots its parameters read.
  owner: Option<u32>,
  /// What it is doing.
  step: Step,
}

/// What an occurrence being typed is doing.
#[derive(Clone, Copy)]
enum Step {
  /// Typing its written children.
  Children,
  /// An occurrence of a definition that missed the fill table, filling its empty slots in index
  /// order: the slot its order has reached.
  Filling(u32),
  /// A type parameter whose slot was empty, waiting for that slot's fill to end.
  Waiting(u32),
}

/// A slot being filled.
#[derive(Clone, Copy)]
struct Fill {
  /// The position on the frame stack of the occurrence whose slot it fills.
  frame: u32,
  /// The slot.
  slot: u32,
  /// Its key in the set of fills in progress: the occurrence's definition, how many arguments the
  /// occurrence writes, and the slot.
  key: (DefId, u32, u32),
}

/// What typing reads of one occurrence's row.
#[derive(Clone, Copy)]
struct Seen {
  /// What it names.
  written: Written,
  /// How many arguments it writes.
  args: u32,
  /// Whether it is written non-null.
  non_null: bool,
  /// Where it is written.
  location: Location,
}

/// The walk that types every occurrence, and what it keeps from one tree to the next.
struct Typing<'p> {
  builder: &'p mut ModelBuilder,
  reporter: &'p mut Reporter,
  /// Each definition's least number of type arguments, by `DefId`.
  least: Vec<u32>,
  /// The root of every default's tree, ascending.
  roots: Vec<TypeExprId>,
  /// `Limits::max_generic_depth`.
  max_depth: u32,
  /// `Limits::max_instantiations`.
  max_instantiations: u32,
  /// `Limits::max_substitutions`.
  max_substitutions: u32,
  /// The first source of a supplied package: every source below it is the built-in library's.
  supplied: u32,
  /// The fill table: by a definition and the types of the slots an occurrence of it writes, the
  /// complete nullable type, or why it could not be built. Every occurrence that misses it records
  /// what it came to, so each key is filled once.
  table: BTreeMap<Box<[u32]>, Result<TyId, Failure>>,
  /// The fills in progress, each as its definition, how many arguments the occurrence being filled
  /// writes, and the slot: exactly the keys of the fill frames on the stack.
  progress: BTreeSet<(DefId, u32, u32)>,
  /// Each type's non-null form, once made: every non-null occurrence, non-null parameter read from
  /// a slot and collection key asks through it, so a type's arguments are read once for it.
  non_null: BTreeMap<TyId, TyId>,
  /// The one unresolved type, once interned.
  unresolved: Option<TyId>,
  /// The instantiations of generic definitions interned outside the built-in library.
  counted: u32,
  /// The steps of substitution spent.
  spent: u32,
  /// The frames of the tree being typed.
  frames: Vec<Frame>,
  /// The results of the occurrences being typed, and the slots of those being filled.
  values: Vec<Option<TyId>>,
  /// A fill table key being made: one buffer for every key.
  key: Vec<u32>,
  /// The slots of the type being completed: one buffer for every type.
  slots: Vec<TyId>,
  /// Whether the tree being typed is written in the built-in library.
  library: bool,
  /// Whether the tree being typed is a type parameter's default.
  in_default: bool,
}

impl Typing<'_> {
  /// Types every tree, in id order: an occurrence no tree has typed yet is the root of the next.
  fn trees(&mut self) {
    for index in 0..self.builder.type_expr_count() {
      let root = TypeExprId::new(index);
      let Some(occurrence) = self.builder.occurrence(root) else {
        continue;
      };
      if occurrence.ty.is_some() {
        continue;
      }
      self.library = occurrence.location.source() < self.supplied;
      self.in_default = self.roots.binary_search(&root).is_ok();
      self.tree(root);
    }
  }

  /// Types the tree rooted at `root`, every occurrence of it for its own sake.
  fn tree(&mut self, root: TypeExprId) {
    self.frames.clear();
    self.values.clear();
    self.push(root, None);
    while let Some(&frame) = self.frames.last() {
      match frame {
        Frame::Visit(visit) => self.visit(visit),
        Frame::Fill(fill) => self.filled(fill),
      }
    }
    debug_assert!(
      self.progress.is_empty(),
      "every fill a tree starts ends with it"
    );
  }

  /// Pushes the frame of `id`: typed for its own sake when `owner` is `None`, and otherwise a node
  /// of a default substituted for the occurrence at `owner` on the frame stack.
  fn push(&mut self, id: TypeExprId, owner: Option<u32>) {
    let start = dense(self.values.len());
    self.frames.push(Frame::Visit(Visit {
      id,
      start,
      next: 0,
      owner,
      step: Step::Children,
    }));
  }

  /// Enters `id`, a node of a default substituted for the occurrence at `owner` on the frame stack:
  /// one step of substitution, then what can be judged of it before its children are typed.
  fn enter(&mut self, id: TypeExprId, owner: u32) -> Result<(), Reason> {
    self.charge(1)?;
    self.admit(id, owner)?;
    self.push(id, Some(owner));
    Ok(())
  }

  /// Spends `steps` of substitution, or spends none and answers that the budget cannot pay them.
  fn charge(&mut self, steps: u32) -> Result<(), Reason> {
    match self.spent.checked_add(steps) {
      Some(spent) if spent <= self.max_substitutions => {
        self.spent = spent;
        Ok(())
      }
      _ => Err(Reason::Limit(Limit::Substitutions)),
    }
  }

  /// Whether `id`, substituted for the occurrence at `owner`, can be built as it is written, as far
  /// as that is known before its children are typed: it fails on what typing it for its own sake
  /// would report.
  fn admit(&self, id: TypeExprId, owner: u32) -> Result<(), Reason> {
    let seen = self.seen(id).ok_or(Reason::Unusable)?;
    let usable = match seen.written {
      Written::Unresolved => false,
      Written::List(_) => true,
      // A parameter of the definition whose slots it reads.
      Written::Param { def, index } => {
        seen.args == 0
          && self.filled_def(owner) == Some(def)
          && index < self.builder.type_param_count(def)
      }
      Written::Named(def) => match self.builder.non_zero_of(id) {
        Some(_) => seen.args == 0,
        None => seen.args <= self.builder.type_param_count(def) && seen.args >= self.least(def),
      },
    };
    if usable {
      Ok(())
    } else {
      Err(Reason::Unusable)
    }
  }

  /// The definition named by the occurrence at `at` on the frame stack.
  fn filled_def(&self, at: u32) -> Option<DefId> {
    let Some(Frame::Visit(visit)) = self.frames.get(at as usize) else {
      return None;
    };
    match self.seen(visit.id)?.written {
      Written::Named(def) => Some(def),
      Written::Param { .. } | Written::List(_) | Written::Unresolved => None,
    }
  }

  /// What typing reads of the occurrence `id`.
  fn seen(&self, id: TypeExprId) -> Option<Seen> {
    let occurrence = self.builder.occurrence(id)?;
    Some(Seen {
      written: occurrence.written,
      args: dense(occurrence.args.len()),
      non_null: occurrence.non_null,
      location: occurrence.location,
    })
  }

  /// The written child of `id` at `next`: its argument there, or, for a list, its element.
  fn child(&self, id: TypeExprId, next: u32) -> Option<TypeExprId> {
    let occurrence = self.builder.occurrence(id)?;
    match occurrence.written {
      Written::List(element) => (next == 0).then_some(element),
      Written::Named(_) | Written::Param { .. } | Written::Unresolved => {
        occurrence.args.get(next as usize).copied()
      }
    }
  }

  /// Takes the next step of `visit`, the frame on top of the stack.
  fn visit(&mut self, visit: Visit) {
    match visit.step {
      Step::Children => match self.child(visit.id, visit.next) {
        Some(child) => {
          self.set_top(Visit {
            next: visit.next.saturating_add(1),
            ..visit
          });
          match visit.owner {
            None => self.push(child, None),
            // A node of a default fails at once, and takes the occurrence its tree fills with it.
            Some(owner) => {
              if let Err(reason) = self.enter(child, owner) {
                self.fail(owner, reason);
              }
            }
          }
        }
        None => self.finish(visit),
      },
      Step::Filling(order) => self.order(visit, order),
      Step::Waiting(slot) => self.read(visit, slot),
    }
  }

  /// Replaces the frame on top of the stack, an occurrence's, with `visit`.
  fn set_top(&mut self, visit: Visit) {
    if let Some(top) = self.frames.last_mut() {
      *top = Frame::Visit(visit);
    }
  }

  /// Completes `visit`, every written child of which is typed, by what it names.
  fn finish(&mut self, visit: Visit) {
    let Some(seen) = self.seen(visit.id) else {
      return self.unusable(visit);
    };
    match seen.written {
      // Name resolution reported it, and typed for its own sake it says nothing more. A default's
      // node never gets here: it fails when it is entered.
      Written::Unresolved => self.unusable(visit),
      Written::List(_) => self.list(visit, seen),
      Written::Param { def, index } => match visit.owner {
        None => self.param(visit, seen, def, index),
        Some(_) => self.read(visit, index),
      },
      Written::Named(def) => match self.builder.non_zero_of(visit.id) {
        Some(primitive) => self.alias(visit, seen, def, primitive),
        None => self.named(visit, seen, def),
      },
    }
  }

  /// `visit` cannot be built as it is written. Typed for its own sake it is the unresolved type,
  /// what is wrong with it reported already, here or by an earlier phase; substituted, it fails,
  /// and takes with it the occurrence its tree fills.
  fn unusable(&mut self, visit: Visit) {
    match visit.owner {
      None => {
        let ty = self.unresolved();
        self.done(ty);
      }
      Some(owner) => self.fail(owner, Reason::Unusable),
    }
  }

  /// `visit`'s own type, written as `seen` says, passes `limit`. Typed for its own sake it is
  /// reported where it is written, and is the unresolved type; substituted, it fails.
  fn limited(&mut self, visit: Visit, seen: Seen, limit: Limit) {
    match visit.owner {
      None => {
        self.report_limit(seen.location, limit);
        let ty = self.unresolved();
        self.done(ty);
      }
      Some(owner) => self.fail(owner, Reason::Limit(limit)),
    }
  }

  /// Completes `visit`, a list whose element is typed: one level deeper than its element.
  fn list(&mut self, visit: Visit, seen: Seen) {
    let unresolved = self.unresolved;
    let element = self
      .values
      .get(visit.start as usize)
      .copied()
      .flatten()
      .filter(|ty: &TyId| Some(*ty) != unresolved);
    // An element that is unresolved makes the list so, with nothing more to say.
    let Some(element) = element else {
      return self.unusable(visit);
    };
    let depth = self.builder.ty_depth(element).saturating_add(1);
    if !self.library && depth > self.max_depth {
      return self.limited(visit, seen, Limit::Depth);
    }
    let ty = self.builder.intern_list(element, seen.non_null);
    self.done(ty);
  }

  /// Completes `visit`, `def`'s type parameter `index` typed for its own sake: in a default's own
  /// tree too, the parameter itself, whatever its index.
  fn param(&mut self, visit: Visit, seen: Seen, def: DefId, index: u32) {
    if seen.args != 0 {
      self.report_arguments(seen.location, PARAM_ARGUMENTS, "remove the type arguments");
      return self.unusable(visit);
    }
    // A position past the definition's parameters is an extension's name past its target's list,
    // or a name a replaced built-in's list gave: lowering reported either.
    if index >= self.builder.type_param_count(def) {
      return self.unusable(visit);
    }
    let ty = self.builder.intern_param(def, index, seen.non_null);
    self.done(ty);
  }

  /// Completes `visit`, a type parameter substituted for the occurrence its tree fills: the type in
  /// that occurrence's slot `index`, in its non-null form when the parameter is written non-null.
  /// An empty slot is filled first, there and then, and this frame reads it once that fill ends.
  fn read(&mut self, visit: Visit, index: u32) {
    let Some(owner) = visit.owner else {
      return self.unusable(visit);
    };
    match (self.slot(owner, index), visit.step) {
      (Some(ty), _) => {
        let non_null = self.seen(visit.id).is_some_and(|seen: Seen| seen.non_null);
        let ty = if non_null { self.non_null_of(ty) } else { ty };
        self.done(ty);
      }
      (None, Step::Children) => {
        self.set_top(Visit {
          step: Step::Waiting(index),
          ..visit
        });
        if let Err(reason) = self.fill(owner, index) {
          self.fail(owner, reason);
        }
      }
      // A fill that ends has filled its slot, and one that fails takes this frame with it.
      (None, Step::Filling(_) | Step::Waiting(_)) => self.fail(owner, Reason::Unusable),
    }
  }

  /// The type in slot `index` of the occurrence at `at` on the frame stack, once it is filled.
  fn slot(&self, at: u32, index: u32) -> Option<TyId> {
    let Some(Frame::Visit(visit)) = self.frames.get(at as usize) else {
      return None;
    };
    let position = (visit.start as usize).saturating_add(index as usize);
    self.values.get(position).copied().flatten()
  }

  /// Completes `visit`, an alias such as `NonZeroU32`: `non_zero`, `core`'s `NonZero`, of the
  /// primitive whose definition is `primitive`, interned here, before the alias's own type.
  fn alias(&mut self, visit: Visit, seen: Seen, non_zero: DefId, primitive: DefId) {
    if seen.args != 0 {
      if visit.owner.is_none() {
        self.report_arguments(
          seen.location,
          ALIAS_ARGUMENTS,
          "write the alias alone, or `NonZero` with its one type argument",
        );
      }
      return self.unusable(visit);
    }
    // `core`'s `NonZero` has the one parameter the alias fills.
    if self.builder.type_param_count(non_zero) != 1 {
      return self.unusable(visit);
    }
    let slot = self.builder.intern_named(primitive, &[], false);
    self.values.truncate(visit.start as usize);
    self.values.push(Some(slot));
    self.complete(visit, seen, non_zero, false);
  }

  /// Completes `visit`, an occurrence of `def` whose written arguments are typed: its count, its
  /// written slots, then the fill of those it leaves out, then its type.
  fn named(&mut self, visit: Visit, seen: Seen, def: DefId) {
    let count = self.builder.type_param_count(def);
    let least = self.least(def);
    // Three numbers: no parameter row is read.
    if seen.args > count || seen.args < least {
      if visit.owner.is_none() {
        self.report_count(seen, def, count, least);
      }
      return self.unusable(visit);
    }
    let start = visit.start as usize;
    // A written argument that is unresolved makes the occurrence so, with nothing more to say. A
    // default's node fails rather than be unresolved, so only one typed for its own sake meets one.
    if let Some(unresolved) = self.unresolved
      && self
        .values
        .get(start..)
        .unwrap_or_default()
        .contains(&Some(unresolved))
    {
      return self.unusable(visit);
    }
    // A collection's key or element is non-null however it is written, before the fill table is
    // asked and before any default reads it.
    if seen.args != 0
      && self.builder.def_collection(def).is_some()
      && let Some(Some(key)) = self.values.get(start).copied()
    {
      let key = self.non_null_of(key);
      if let Some(slot) = self.values.get_mut(start) {
        *slot = Some(key);
      }
    }
    if seen.args < count {
      return self.lookup(visit, seen, def, count);
    }
    self.complete(visit, seen, def, false);
  }

  /// The slots `visit`, an occurrence of `def` with `count` type parameters, leaves out: the fill
  /// table first; on a miss, the charge, then the empty slots, which its order fills.
  fn lookup(&mut self, visit: Visit, seen: Seen, def: DefId, count: u32) {
    debug_assert!(
      !self.library,
      "a tree of the built-in library reads no fill table"
    );
    self.key_of(def, visit.start, seen.args);
    match self.table.get(self.key.as_slice()).copied() {
      Some(Ok(nullable)) => self.typed(visit, seen, def, nullable),
      Some(Err(failure)) => self.failed(visit, seen, def, failure),
      None => {
        // The charge pays for the slots before they are made, so a fill that fails at once leaves
        // none unpaid for.
        let missing = count.saturating_sub(seen.args);
        if let Err(reason) = self.charge(missing) {
          let failure = Failure {
            reason,
            param: seen.args,
          };
          self.store(def, visit.start, seen.args, Err(failure));
          return self.failed(visit, seen, def, failure);
        }
        let slots = self.values.len().saturating_add(missing as usize);
        self.values.resize(slots, None);
        self.set_top(Visit {
          step: Step::Filling(seen.args),
          ..visit
        });
      }
    }
  }

  /// Fills the next empty slot of `visit`, which missed the fill table, from `order` on in index
  /// order; completes it once every slot is filled.
  fn order(&mut self, visit: Visit, order: u32) {
    let Some(seen) = self.seen(visit.id) else {
      return self.unusable(visit);
    };
    let Written::Named(def) = seen.written else {
      return self.unusable(visit);
    };
    let count = self.builder.type_param_count(def);
    let at = dense(self.frames.len().saturating_sub(1));
    let mut slot = order;
    // A slot a demand filled already is passed over.
    while slot < count && self.slot(at, slot).is_some() {
      slot = slot.saturating_add(1);
    }
    if slot >= count {
      return self.complete(visit, seen, def, true);
    }
    self.set_top(Visit {
      step: Step::Filling(slot),
      ..visit
    });
    if let Err(reason) = self.fill(at, slot) {
      self.fail(at, reason);
    }
  }

  /// Starts filling slot `slot` of the occurrence at `at` on the frame stack: the written tree of
  /// that parameter's default is typed above a fill frame, substituted for the occurrence.
  fn fill(&mut self, at: u32, slot: u32) -> Result<(), Reason> {
    let Some(Frame::Visit(visit)) = self.frames.get(at as usize).copied() else {
      return Err(Reason::Unusable);
    };
    let seen = self.seen(visit.id).ok_or(Reason::Unusable)?;
    let Written::Named(def) = seen.written else {
      return Err(Reason::Unusable);
    };
    let key = (def, seen.args, slot);
    // A key met twice on one chain would walk the same defaults to a third meeting, and on.
    if self.progress.contains(&key) {
      return Err(Reason::Cycle);
    }
    // Every parameter an occurrence leaves out has a default.
    let Some((_, _, Some(default))) = self.builder.type_param(def, slot) else {
      return Err(Reason::Unusable);
    };
    self.progress.insert(key);
    self.frames.push(Frame::Fill(Fill {
      frame: at,
      slot,
      key,
    }));
    self.enter(default, at)
  }

  /// Ends `fill`, the frame on top of the stack, whose default's tree is typed: that type, on top
  /// of the value stack, goes into the slot.
  fn filled(&mut self, fill: Fill) {
    let ty = self.values.pop().flatten();
    self.frames.pop();
    self.progress.remove(&fill.key);
    let Some(mut ty) = ty else {
      return self.fail(fill.frame, Reason::Unusable);
    };
    // A collection's key or element filled from a default is non-null before anything reads it.
    if fill.slot == 0 && self.builder.def_collection(fill.key.0).is_some() {
      ty = self.non_null_of(ty);
    }
    let Some(Frame::Visit(visit)) = self.frames.get(fill.frame as usize).copied() else {
      return;
    };
    let position = (visit.start as usize).saturating_add(fill.slot as usize);
    if let Some(slot) = self.values.get_mut(position) {
      *slot = Some(ty);
    }
  }

  /// The type of `visit`, an occurrence of `def` whose slots are all on the value stack: its depth
  /// checked, its instantiation counted and interned, then in the occurrence's nullness. `filled`
  /// when it missed the fill table, which then records what it came to.
  fn complete(&mut self, visit: Visit, seen: Seen, def: DefId, filled: bool) {
    let count = self.builder.type_param_count(def);
    let start = visit.start as usize;
    let end = start.saturating_add(count as usize);
    let slots: &[Option<TyId>] = self.values.get(start..end).unwrap_or_default();
    self.slots.clear();
    self.slots.extend(slots.iter().flatten().copied());
    // Every slot is written or filled before this.
    if self.slots.len() != count as usize {
      return self.unusable(visit);
    }
    if count == 0 {
      let ty = self.builder.intern_named(def, &[], seen.non_null);
      return self.done(ty);
    }
    match self.instantiation(def) {
      Ok(nullable) => {
        if filled {
          self.store(def, visit.start, seen.args, Ok(nullable));
        }
        self.typed(visit, seen, def, nullable);
      }
      Err(limit) => {
        if filled {
          let failure = Failure {
            reason: Reason::Limit(limit),
            param: count,
          };
          self.store(def, visit.start, seen.args, Err(failure));
        }
        self.limited(visit, seen, limit);
      }
    }
  }

  /// The nullable named type of `def` with the slots `slots` holds, one level deeper than its
  /// deepest slot: one interned already, or one more instantiation, counted, when the limits admit
  /// it. The built-in library's are neither checked nor counted.
  fn instantiation(&mut self, def: DefId) -> Result<TyId, Limit> {
    let mut depth: u32 = 0;
    for slot in &self.slots {
      depth = depth.max(self.builder.ty_depth(*slot).saturating_add(1));
    }
    if !self.library && depth > self.max_depth {
      return Err(Limit::Depth);
    }
    if let Some(found) = self.builder.find_named(def, &self.slots, false) {
      return Ok(found);
    }
    if !self.library {
      if self.counted >= self.max_instantiations {
        return Err(Limit::Instantiations);
      }
      self.counted = self.counted.saturating_add(1);
    }
    Ok(self.builder.intern_named(def, &self.slots, false))
  }

  /// Completes `visit`, an occurrence of the generic definition `def` whose instantiation is
  /// `nullable`: its own type is that, or that type's non-null form, and when it is typed for its
  /// own sake and names no type parameter, that type is one of `def`'s instantiations.
  fn typed(&mut self, visit: Visit, seen: Seen, def: DefId, nullable: TyId) {
    let ty = if seen.non_null {
      self.non_null_of(nullable)
    } else {
      nullable
    };
    if visit.owner.is_none() && self.builder.ty_closed(ty) {
      self.builder.push_instantiation(def, ty);
    }
    self.done(ty);
  }

  /// `visit`, an occurrence of `def` that leaves arguments out, cannot be filled, for `failure`.
  /// Typed for its own sake it reports why and is the unresolved type; substituted, it fails, and
  /// takes with it the occurrence its tree fills.
  fn failed(&mut self, visit: Visit, seen: Seen, def: DefId, failure: Failure) {
    match visit.owner {
      None => {
        self.report_failure(seen, def, failure);
        let ty = self.unresolved();
        self.done(ty);
      }
      Some(owner) => self.fail(owner, failure.reason),
    }
  }

  /// The occurrence at `at` on the frame stack fails for `reason`, which arose in the tree filling
  /// one of its slots, or in starting a fill of its own. Every frame above it ends, each fill frame
  /// taking its key out of the set; the occurrence, which was filling, records why in the fill
  /// table, at the slot its order had reached; then one typed for its own sake reports, and one
  /// substituted fails in turn, taking with it the occurrence its own tree fills.
  fn fail(&mut self, at: u32, reason: Reason) {
    let mut at = at as usize;
    loop {
      while self.frames.len() > at.saturating_add(1) {
        if let Some(Frame::Fill(fill)) = self.frames.pop() {
          self.progress.remove(&fill.key);
        }
      }
      let Some(Frame::Visit(visit)) = self.frames.last().copied() else {
        return;
      };
      let seen = self.seen(visit.id);
      let def = match seen {
        Some(Seen {
          written: Written::Named(def),
          ..
        }) => Some(def),
        _ => None,
      };
      let failure = Failure {
        reason,
        param: match visit.step {
          Step::Filling(order) => order,
          Step::Children | Step::Waiting(_) => 0,
        },
      };
      if let (Step::Filling(_), Some(def), Some(seen)) = (visit.step, def, seen) {
        self.store(def, visit.start, seen.args, Err(failure));
      }
      match visit.owner {
        None => {
          if let (Some(def), Some(seen)) = (def, seen) {
            self.report_failure(seen, def, failure);
          }
          let ty = self.unresolved();
          self.done(ty);
          return;
        }
        Some(owner) => {
          self.frames.pop();
          self.values.truncate(visit.start as usize);
          at = owner as usize;
        }
      }
    }
  }

  /// Records in the fill table what the occurrence of `def` came to whose slots start at `start` on
  /// the value stack, `written` of them written.
  fn store(&mut self, def: DefId, start: u32, written: u32, outcome: Result<TyId, Failure>) {
    debug_assert!(
      !self.library,
      "a tree of the built-in library writes no fill table"
    );
    self.key_of(def, start, written);
    self.table.insert(Box::from(self.key.as_slice()), outcome);
  }

  /// Makes `key` the fill table's key of an occurrence of `def` whose `written` slots start at
  /// `start` on the value stack: the definition, then each written slot's type.
  fn key_of(&mut self, def: DefId, start: u32, written: u32) {
    self.key.clear();
    self.key.push(def.get());
    let from = start as usize;
    let slots: &[Option<TyId>] = self
      .values
      .get(from..from.saturating_add(written as usize))
      .unwrap_or_default();
    for slot in slots {
      self.key.push(slot.map_or(u32::MAX, |ty: TyId| ty.get()));
    }
  }

  /// Completes the occurrence on top of the stack with `ty`: its row is given it when it is typed
  /// for its own sake, and `ty` is left on the value stack for what holds it, or for the fill whose
  /// default it is.
  fn done(&mut self, ty: TyId) {
    let Some(Frame::Visit(visit)) = self.frames.pop() else {
      return;
    };
    if visit.owner.is_none() {
      self.builder.set_type_expr_ty(visit.id, ty);
    }
    self.values.truncate(visit.start as usize);
    self.values.push(Some(ty));
  }

  /// `ty`'s non-null form, made the first time it is asked for and remembered.
  fn non_null_of(&mut self, ty: TyId) -> TyId {
    if let Some(&made) = self.non_null.get(&ty) {
      return made;
    }
    let made = self.builder.intern_nullness(ty, true);
    self.non_null.insert(ty, made);
    made
  }

  /// The one unresolved type, interned the first time it is needed.
  fn unresolved(&mut self) -> TyId {
    if let Some(ty) = self.unresolved {
      return ty;
    }
    let ty = self.builder.intern_unresolved();
    self.unresolved = Some(ty);
    ty
  }

  /// The least number of type arguments `def` takes.
  fn least(&self, def: DefId) -> u32 {
    self.least.get(def.get() as usize).copied().unwrap_or(0)
  }

  /// Checks every application's count of type arguments against its directive's type parameters.
  fn applications(&mut self) {
    for index in 0..self.builder.application_count() {
      let Some((Resolution::Resolved(directive), written, at)) =
        self.builder.applied(ApplicationId::new(index))
      else {
        continue;
      };
      let count = self.builder.type_param_count(directive);
      let least = self.least(directive);
      if written > count || written < least {
        let message = counted("@", self.builder.def_name(directive), count, least, written);
        let defined = self.builder.def_location(directive);
        self.report_arity(at, message, defined);
      }
    }
  }

  /// Reports a count of type arguments, written as `seen` says, that `def`, with `count` type
  /// parameters of which every one from `least` on has a default, does not take.
  fn report_count(&mut self, seen: Seen, def: DefId, count: u32, least: u32) {
    let message = counted("", self.builder.def_name(def), count, least, seen.args);
    let defined = self.builder.def_location(def);
    self.report_arity(seen.location, message, defined);
  }

  /// Reports `generic-arity` at `at`, saying `message`, with a label at `defined`, the definition's
  /// name, when it has a position.
  fn report_arity(&mut self, at: Location, message: String, defined: Location) {
    let kind = DiagnosticKind::GenericArity;
    let mut report = Diagnostic::builder(kind.code(), at)
      .severity(kind.severity())
      .message(message)
      .primary_label(kind.title())
      .help(COUNT_HELP);
    // A primitive `core` materialises has no position of its own to point at.
    if !defined.is_entire() {
      report = report.label(defined, "defined here");
    }
    self.reporter.report(report.build());
  }

  /// Reports type arguments written at `at` on what takes none: `message` says what, and `help`
  /// what to write instead.
  fn report_arguments(&mut self, at: Location, message: &'static str, help: &'static str) {
    let kind = DiagnosticKind::GenericArity;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help(help)
        .build(),
    );
  }

  /// Reports an occurrence of `def`, written as `seen` says, that leaves a type argument out which
  /// cannot be filled, for `failure`: past a limit, `resource-limit`; a cycle met inside a
  /// default's own tree, `generic-default`; anything else, `generic-arity`, with a label at the
  /// default of the slot its order had reached.
  fn report_failure(&mut self, seen: Seen, def: DefId, failure: Failure) {
    let cycle = match failure.reason {
      Reason::Limit(limit) => return self.report_limit(seen.location, limit),
      Reason::Cycle => true,
      Reason::Unusable => false,
    };
    let param = self.builder.type_param(def, failure.param);
    let name = Excerpt(self.builder.def_name(def));
    let spelled = Excerpt(match param {
      Some((sym, _, _)) => self.builder.spelling(sym),
      None => &[],
    });
    if cycle && self.in_default {
      let message = format!(
        "`{name}` is written here without a type argument for `{spelled}`, and filling it from \
         its default leads back to a default being filled: the default this is written in can \
         never fill"
      );
      let kind = DiagnosticKind::GenericDefault;
      self.reporter.report(
        Diagnostic::builder(kind.code(), seen.location)
          .severity(kind.severity())
          .message(message)
          .primary_label(kind.title())
          .help("write every type argument here, or remove the default this is written in")
          .build(),
      );
      return;
    }
    let message = format!(
      "`{name}` is written without a type argument for `{spelled}`, and the default of \
       `{spelled}` cannot fill it here"
    );
    let default = param
      .and_then(|(_, _, default): (Sym, Location, Option<TypeExprId>)| default)
      .and_then(|root: TypeExprId| self.builder.occurrence(root))
      .map(|occurrence| occurrence.location);
    let kind = DiagnosticKind::GenericArity;
    let mut report = Diagnostic::builder(kind.code(), seen.location)
      .severity(kind.severity())
      .message(message)
      .primary_label(kind.title())
      .help("write the type argument, or correct the default that cannot fill it");
    if let Some(at) = default {
      report = report.label(at, "this default cannot fill it");
    }
    self.reporter.report(report.build());
  }

  /// Reports, at `at`, a type that cannot be built because it passes `limit`.
  fn report_limit(&mut self, at: Location, limit: Limit) {
    let (message, help) = match limit {
      Limit::Depth => (
        format!(
          "this type nests deeper than `Limits::max_generic_depth`, {}, admits",
          self.max_depth
        ),
        "raise the bound with `Limits::with_max_generic_depth`, or nest types less deeply",
      ),
      Limit::Instantiations => (
        format!(
          "this type is one more instantiation of a generic definition than \
           `Limits::max_instantiations`, {}, admits",
          self.max_instantiations
        ),
        "raise the bound with `Limits::with_max_instantiations`, or instantiate fewer types",
      ),
      Limit::Substitutions => (
        format!(
          "filling the type arguments this type leaves out takes more steps of substitution than \
           `Limits::max_substitutions`, {}, admits",
          self.max_substitutions
        ),
        "raise the bound with `Limits::with_max_substitutions`, or write the type arguments out",
      ),
    };
    let kind = DiagnosticKind::ResourceLimit;
    self.reporter.report(
      Diagnostic::builder(kind.code(), at)
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help(help)
        .build(),
    );
  }
}

/// What a count of type arguments says: how many type parameters `name`, written after `sigil`,
/// has, how many of them have a default, every one from `least` on, and how many type arguments are
/// written.
fn counted(sigil: &str, name: &[u8], count: u32, least: u32, written: u32) -> String {
  let name = Excerpt(name);
  let arguments = if written == 1 {
    String::from("1 type argument is")
  } else {
    format!("{written} type arguments are")
  };
  if count == 0 {
    return format!("`{sigil}{name}` has no type parameter, and {arguments} written");
  }
  let parameters = if count == 1 {
    String::from("1 type parameter")
  } else {
    format!("{count} type parameters")
  };
  let defaulted = count.saturating_sub(least);
  format!("`{sigil}{name}` has {parameters}, {defaulted} with a default, and {arguments} written")
}
