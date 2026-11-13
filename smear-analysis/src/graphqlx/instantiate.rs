//! Instantiation: the phase after lowering, which gives each definition its built-in meaning and
//! checks each definition's defaults where they are written.
//!
//! It reads no document: it reads the rows lowering wrote, through the builder, and writes onto
//! them.
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

use std::{format, vec::Vec};

use smear_semantics::scc;
use tokora::diagnostic::Location;

use super::{
  builtin::{CORE, DIRECTIVE},
  code::DiagnosticKind,
  declare::{Binding, Declarations},
  diagnostic::{Diagnostic, Excerpt, Reporter},
  id::{ApplicationId, DefId, Sym, TypeExprId, dense},
  model::{Collection, DefKind, ModelBuilder, Resolution, Written},
};

/// Instantiates what lowering left in `builder`, whose declarations are `declarations`, reporting
/// into `reporter`: each definition's built-in meaning, then its defaults, checked where they are
/// written.
pub(crate) fn instantiate(
  declarations: &Declarations<'_, '_>,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  meaning(declarations, builder);
  defaults(builder, reporter);
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

/// Checks every definition's defaults where they are written, reporting into `reporter`.
fn defaults(builder: &ModelBuilder, reporter: &mut Reporter) {
  let mut scratch = Scratch::default();
  for index in 0..builder.def_count() {
    definition(builder, reporter, DefId::new(index), &mut scratch);
  }
}

/// Checks the defaults of `def`: that they are trailing, and that no group of them needs each
/// other.
fn definition(builder: &ModelBuilder, reporter: &mut Reporter, def: DefId, scratch: &mut Scratch) {
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
