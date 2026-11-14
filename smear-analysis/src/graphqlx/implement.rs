//! Implementation: the phase after binding, which checks what each definition says it is with its
//! `implements` entries, and a union with its members.
//!
//! It reads no document, writes no row of the model and no text, and interns no name: it reads
//! the rows the phases before it wrote, through the builder, interns the types it reads under type
//! arguments through the rebuild alone, and reports.
//!
//! # What a definition is, here
//!
//! What its own package writes: the package that wrote the definition. An `implements` entry and a
//! union member are read when that package wrote them, in the definition or in one of its own
//! extensions, wherever the file. What another package writes into a definition is the orphan
//! rule's mistake, and is passed by here, for the definition checked and for every interface the
//! check reads. An entry or a member whose type is the unresolved type is passed by everywhere:
//! the resolver reported it.
//!
//! # What is checked
//!
//! Each definition, in id order. An `implements` entry of a type an earlier entry of the
//! definition has is `smear::schema::duplicate-implements-interface`, at it, with a label at the
//! first of that type, and so is every later one: `implements I & I & I` is two. A union's member of
//! a type an earlier member has is `smear::schema::duplicate-union-member`, likewise. Two entries
//! are told apart by their types, so `Cmp<Int>` and `Cmp<String>` are two, and an unresolved one is
//! compared with nothing.
//!
//! Then its roots, each entry, in order, whose type is a named type of an interface that is not
//! the definition itself, and of a marker when the definition is no object and no interface, the
//! first of its type: `implements I & I` checks `I` once. What an entry names besides is another
//! rule's: an object or a type parameter, refused by the rules on `implements` entries; a
//! non-marker on a scalar, an enum, an input object or a union, `marker-misuse`'s; the definition
//! itself, the rule on interfaces that implement themselves.
//!
//! For each root, every interface the root's interface implements must be one of the definition's
//! own entries too, compared as a type: the interface's own entries, each with the root's type
//! arguments in place of the interface's type parameters. One that is not is
//! `smear::schema::missing-transitive-interface`, at the root's entry, with a label at the entry
//! that implements it, and what it implements is owed in turn: its definition's entries are read
//! under its arguments, once for each definition the check of one definition reaches. So
//! `D <- C <- B <- A`, each implementing its parent alone, is three reports, as in `smear-schema`,
//! and a ring of definitions that grows its arguments ends at once. An interface the definition is
//! found to miss is reported once, through whichever root reaches it first; one that is the
//! definition itself is the self-implementing rule's, and passed by; and a definition that is no
//! object and no interface owes the markers alone. With `interface Ord<X> implements
//! Comparable<X>`, `type Lazy implements Ord<Lazy>` must write `Comparable<Lazy>` too.
//!
//! # What it costs, and what bounds it
//!
//! A definition's own entries and members are written, and each is read once: one scan of the
//! shared kernel, or one sort when the list is wide, finds the duplicates and the roots. Every
//! other step is paid from `Limits::max_implementation_steps`, a budget of its own, since the
//! check is needed on every accepted program and costs it the square of a definition's entries:
//! each entry looked at of an interface the check reads, whoever wrote it, and each node of a type
//! rebuilt under an entry's arguments, a rebuild remembered by the type and the arguments' named
//! type costing nothing more. On a program the check accepts, each root costs its interface's
//! entries, and no more. What a rebuild derives is checked against `Limits::max_generic_depth` and
//! counted toward `Limits::max_instantiations`; nothing draws on `Limits::max_substitutions` or
//! `Limits::max_bound_steps`. A check a limit stops is `resource-limit` once, at the root it was
//! checking, naming the limit, and nothing more is said of that definition.
//!
//! No step recurses: the interfaces still to read are an explicit stack. Each table is keyed by
//! ids, and the walk reads definitions and entries in id order, so no id and no diagnostic depends
//! on the order of the files or the dependencies.

use std::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::String,
  vec::Vec,
};

use smear_semantics::duplicates::Duplicates;
use tokora::diagnostic::Location;

use super::{
  code::{DiagnosticKind, schema},
  declare::Declarations,
  diagnostic::{Diagnostic, DiagnosticBuilder, Excerpt, Reporter},
  id::{DefId, PackageId, TyId, TypeExprId},
  instantiate::own_package,
  model::{DefKind, ModelBuilder, Occurrence, Shape},
  source::SourceMap,
  types::{Counter, Limit, Stop, Types},
};

/// Checks what each definition `builder` holds says it is, whose declarations are `declarations`
/// and whose sources `sources` numbers, with the type services of `types`, reporting into
/// `reporter`: that no two of its `implements` entries, nor of a union's members, are one type, and
/// that it implements what the interfaces it implements implement.
pub(crate) fn implement(
  declarations: &Declarations<'_, '_>,
  sources: &SourceMap,
  types: &mut Types,
  builder: &mut ModelBuilder,
  reporter: &mut Reporter,
) {
  let count = builder.def_count();
  let mut owners: Vec<Option<PackageId>> = Vec::with_capacity(count as usize);
  for index in 0..count {
    owners.push(own_package(declarations, DefId::new(index)));
  }
  let steps = Counter::new(types.limits().max_implementation_steps());
  let mut implementing = Implementing {
    builder,
    reporter,
    types,
    sources,
    owners,
    steps,
    rebuilds: BTreeMap::new(),
    entries: Vec::new(),
    members: Vec::new(),
    twice: Vec::new(),
    roots: Vec::new(),
    rooted: BTreeSet::new(),
    declared: Vec::new(),
    reached: BTreeSet::new(),
    expanded: BTreeSet::new(),
    frontier: Vec::new(),
  };
  implementing.definitions();
}

/// One of a definition's lists of types.
#[derive(Clone, Copy)]
enum List {
  /// Its `implements` entries.
  Implements,
  /// A union's members.
  Members,
}

/// A root of the definition being checked: an entry of its own that names an interface it is
/// checked against.
#[derive(Clone, Copy)]
struct Root {
  /// The definition being checked.
  def: DefId,
  /// Where the entry is written.
  at: Location,
  /// The entry's type, a named type of `interface`.
  ty: TyId,
  /// The interface.
  interface: DefId,
}

/// What the phase keeps from one definition to the next, beside the type services, whose rebuild
/// it pays for from its own budget.
struct Implementing<'p> {
  builder: &'p mut ModelBuilder,
  reporter: &'p mut Reporter,
  types: &'p mut Types,
  sources: &'p SourceMap,
  /// Each definition's own package, by `DefId`: the package that wrote it.
  owners: Vec<Option<PackageId>>,
  /// `Limits::max_implementation_steps`: every step of the phase.
  steps: Counter,
  /// Each type rebuilt under a named type's arguments, by the two, with what it became, what
  /// stopped it included: a second asks nothing.
  rebuilds: BTreeMap<(TyId, TyId), Result<Option<TyId>, Stop>>,
  /// The definition being checked's own entries whose types resolved, in order, each with its
  /// type.
  entries: Vec<(TypeExprId, TyId)>,
  /// The union being checked's own members whose types resolved, in order, each with its type.
  members: Vec<(TypeExprId, TyId)>,
  /// The written duplicates of the list being checked: each later item, the first of its type,
  /// and the type.
  twice: Vec<(TypeExprId, TypeExprId, TyId)>,
  /// The definition being checked's roots, in order.
  roots: Vec<Root>,
  /// The types of its roots, each once.
  rooted: BTreeSet<TyId>,
  /// The types of its own entries that resolved, sorted, each once.
  declared: Vec<TyId>,
  /// The interface types it was found to miss, each reported once.
  reached: BTreeSet<TyId>,
  /// The interface definitions whose entries its check has read.
  expanded: BTreeSet<DefId>,
  /// The interface types whose entries its check has still to read.
  frontier: Vec<TyId>,
}

impl Implementing<'_> {
  /// Checks every definition, in id order. One no package wrote, which no analysis holds, is
  /// passed by.
  fn definitions(&mut self) {
    for index in 0..self.builder.def_count() {
      let def = DefId::new(index);
      let Some(own) = self.owner(def) else {
        continue;
      };
      let Some(kind) = self.builder.def_kind(def) else {
        continue;
      };
      if kind == DefKind::Union {
        own_items(
          self.builder,
          self.sources,
          def,
          own,
          List::Members,
          &mut self.members,
        );
        self.written_duplicates(def, List::Members);
      }
      own_items(
        self.builder,
        self.sources,
        def,
        own,
        List::Implements,
        &mut self.entries,
      );
      self.written_duplicates(def, List::Implements);
      self.check(def, kind);
    }
  }

  /// The package that wrote `def`.
  fn owner(&self, def: DefId) -> Option<PackageId> {
    self.owners.get(def.get() as usize).copied().flatten()
  }

  /// Reports each item of `def`'s list `list`, as gathered, whose type an earlier item has: at
  /// it, with a label at the first of that type.
  fn written_duplicates(&mut self, def: DefId, list: List) {
    self.twice.clear();
    let items: &[(TypeExprId, TyId)] = match list {
      List::Implements => &self.entries,
      List::Members => &self.members,
    };
    let mut seen: Duplicates<TyId> = Duplicates::over(items.len(), |at: usize| {
      items
        .get(at)
        .map_or(TyId::new(0), |&(_, ty): &(TypeExprId, TyId)| ty)
    });
    for (position, &(later, ty)) in items.iter().enumerate() {
      if let Some(earlier) = seen.first(position, ty)
        && let Some(&(first, _)) = items.get(earlier)
      {
        self.twice.push((later, first, ty));
      }
    }
    for index in 0..self.twice.len() {
      let Some(&(later, first, ty)) = self.twice.get(index) else {
        break;
      };
      self.report_written(def, list, later, first, ty);
    }
  }

  /// Checks `def`, of the kind `kind`, whose own entries are gathered, against each of its roots in
  /// turn, until a limit stops one.
  fn check(&mut self, def: DefId, kind: DefKind) {
    // An object or an interface implements interfaces; any other kind markers alone.
    let wide = matches!(kind, DefKind::Object | DefKind::Interface);
    self.roots(def, wide);
    if self.roots.is_empty() {
      return;
    }
    self.declared.clear();
    self
      .declared
      .extend(self.entries.iter().map(|&(_, ty): &(TypeExprId, TyId)| ty));
    self.declared.sort_unstable();
    self.declared.dedup();
    self.reached.clear();
    self.expanded.clear();
    for index in 0..self.roots.len() {
      let Some(&root) = self.roots.get(index) else {
        break;
      };
      if let Err(stop) = self.transitive(root, wide) {
        self.report_stopped(root, stop);
        return;
      }
    }
  }

  /// Gathers the roots of `def`, `wide` when it is an object or an interface: each of its own
  /// entries, in order, that names an interface other than `def`, a marker unless `wide`, the
  /// first of its type.
  fn roots(&mut self, def: DefId, wide: bool) {
    self.roots.clear();
    self.rooted.clear();
    for index in 0..self.entries.len() {
      let Some(&(entry, ty)) = self.entries.get(index) else {
        break;
      };
      let Some(Shape::Named(interface)) = self.builder.ty_shape(ty) else {
        continue;
      };
      if interface == def
        || self.builder.def_kind(interface) != Some(DefKind::Interface)
        || (!wide && !self.builder.def_marker(interface))
      {
        continue;
      }
      let Some(at) = self.located(entry) else {
        continue;
      };
      if self.rooted.insert(ty) {
        self.roots.push(Root {
          def,
          at,
          ty,
          interface,
        });
      }
    }
  }

  /// Checks that the definition `root` is a root of implements what its interface implements,
  /// `wide` when the definition is an object or an interface: each interface reached, through the
  /// root's interface and through each one found missing, under the arguments that reach it, is
  /// one of the definition's own entries, or is reported.
  fn transitive(&mut self, root: Root, wide: bool) -> Result<(), Stop> {
    self.frontier.clear();
    self.expanded.insert(root.interface);
    self.frontier.push(root.ty);
    while let Some(current) = self.frontier.pop() {
      let Some(Shape::Named(holder)) = self.builder.ty_shape(current) else {
        continue;
      };
      let own = self.owner(holder);
      let count = self.builder.def_implements(holder).len();
      for at in 0..count {
        // Each entry looked at is a step, whoever wrote it.
        if !self.steps.charge(1) {
          return Err(Stop::Steps);
        }
        let Some(&entry) = self.builder.def_implements(holder).get(at) else {
          break;
        };
        let Some((location, written)) = self
          .builder
          .occurrence(entry)
          .map(|occurrence: Occurrence<'_>| (occurrence.location, occurrence.ty))
        else {
          continue;
        };
        if own.is_none() || self.sources.written_by(location) != own {
          continue;
        }
        let Some(written) = written else {
          continue;
        };
        let Some(needed) = self.rebuilt(written, current, holder)? else {
          continue;
        };
        let Some(Shape::Named(interface)) = self.builder.ty_shape(needed) else {
          continue;
        };
        if interface == root.def
          || self.builder.def_kind(interface) != Some(DefKind::Interface)
          || (!wide && !self.builder.def_marker(interface))
          || self.reached.contains(&needed)
          || self.declared.binary_search(&needed).is_ok()
        {
          continue;
        }
        self.report_missing(root, location, needed);
        self.reached.insert(needed);
        // What a missing interface implements is owed too, read once for each definition.
        if self.expanded.insert(interface) {
          self.frontier.push(needed);
        }
      }
    }
    Ok(())
  }

  /// `open`, a type written in `def` that may name its type parameters, under `ty`, a named type
  /// of `def`: itself when it names none; otherwise rebuilt with `ty`'s arguments in their place,
  /// the first time it is asked, its nodes paid from the phase's budget, and remembered by the two,
  /// what stopped it included. `None` when it cannot be rebuilt there.
  fn rebuilt(&mut self, open: TyId, ty: TyId, def: DefId) -> Result<Option<TyId>, Stop> {
    if self.builder.ty_closed(open) {
      return Ok(Some(open));
    }
    if let Some(&made) = self.rebuilds.get(&(open, ty)) {
      return made;
    }
    // The built-in library writes no `implements` entry, no union and no interface with a field,
    // so nothing rebuilt here is its own: every type it derives is checked and counted.
    let made = self
      .types
      .derived
      .rebuild(self.builder, &mut self.steps, false, open, ty, def);
    self.rebuilds.insert((open, ty), made);
    made
  }

  /// Reports `later`, an item of `def`'s list `list` of the type `ty`, which `first`, an earlier
  /// one, has.
  fn report_written(
    &mut self,
    def: DefId,
    list: List,
    later: TypeExprId,
    first: TypeExprId,
    ty: TyId,
  ) {
    let (Some(at), Some(earlier)) = (self.located(later), self.located(first)) else {
      return;
    };
    let owner = Excerpt(self.builder.def_name(def));
    let named = Excerpt(type_name(self.builder, ty));
    let (code, message, title, label) = match list {
      List::Implements => (
        schema::DUPLICATE_IMPLEMENTS_INTERFACE,
        format!("`{owner}` implements `{named}` twice"),
        "duplicate interface",
        "first implemented here",
      ),
      List::Members => (
        schema::DUPLICATE_UNION_MEMBER,
        format!("the union `{owner}` has `{named}` as a member twice"),
        "duplicate union member",
        "first a member here",
      ),
    };
    let report = Diagnostic::builder(code, at)
      .message(message)
      .primary_label(title)
      .help("remove the later one");
    self
      .reporter
      .report(labelled(report, earlier, label).build());
  }

  /// Reports that the definition `root` is a root of does not implement `needed`, an interface
  /// type the entry written at `from` implements: at the root's entry, labelled at `from`.
  fn report_missing(&mut self, root: Root, from: Location, needed: TyId) {
    let owner = Excerpt(self.builder.def_name(root.def));
    let through = Excerpt(self.builder.def_name(root.interface));
    let missing = Excerpt(type_name(self.builder, needed));
    let report = Diagnostic::builder(schema::MISSING_TRANSITIVE_INTERFACE, root.at)
      .message(format!(
        "`{owner}` implements `{through}`, which implements `{missing}`, so `{owner}` must \
         implement `{missing}` too"
      ))
      .primary_label("missing transitive interface")
      .help(
        "add it to this definition's `implements` entries, with the type arguments the labelled \
         entry gives it",
      );
    self.reporter.report(
      labelled(
        report,
        from,
        "implemented here, by an interface it implements",
      )
      .build(),
    );
  }

  /// Reports, at the root `root`, that `stop` stopped the check of its definition there.
  fn report_stopped(&mut self, root: Root, stop: Stop) {
    let subject = format!(
      "`{}`'s implementation of `{}`",
      Excerpt(self.builder.def_name(root.def)),
      Excerpt(self.builder.def_name(root.interface))
    );
    self.stopped(root.at, &subject, stop);
  }

  /// Reports `resource-limit` at `at`, where `stop` stopped the check of `subject`, naming the
  /// limit: one message for each way to stop.
  fn stopped(&mut self, at: Location, subject: &str, stop: Stop) {
    let limits = self.types.limits();
    let (message, help): (String, &'static str) = match stop {
      Stop::Steps => (
        format!(
          "{subject} could not be checked within `Limits::max_implementation_steps`, {}, the \
           steps checking implementations may take",
          self.steps.max()
        ),
        "raise the bound with `Limits::with_max_implementation_steps`",
      ),
      Stop::Built(Limit::Depth) => (
        format!(
          "{subject} could not be checked: a type it reads under its type arguments nests deeper \
           than `Limits::max_generic_depth`, {}, admits",
          limits.max_generic_depth()
        ),
        "raise the bound with `Limits::with_max_generic_depth`, or nest types less deeply",
      ),
      Stop::Built(Limit::Instantiations) => (
        format!(
          "{subject} could not be checked: a type it reads under its type arguments is one more \
           instantiation of a generic definition than `Limits::max_instantiations`, {}, admits",
          limits.max_instantiations()
        ),
        "raise the bound with `Limits::with_max_instantiations`, or instantiate fewer types",
      ),
      // The match's arm alone: `instantiation()` refuses a type for its depth or for the count of
      // instantiations, and never for this limit, which counts filling and which checking an
      // implementation never draws on. No program reaches it.
      Stop::Built(Limit::Substitutions) => (
        format!(
          "{subject} could not be checked within `Limits::max_substitutions`, {}",
          limits.max_substitutions()
        ),
        "raise the bound with `Limits::with_max_substitutions`",
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

  /// Where the type occurrence `id` is written.
  fn located(&self, id: TypeExprId) -> Option<Location> {
    self
      .builder
      .occurrence(id)
      .map(|occurrence: Occurrence<'_>| occurrence.location)
  }
}

/// Gathers into `into` the items of `def`'s list `list` that `own`, its own package, wrote and
/// whose types resolved, in order, each with its type, `sources` telling who wrote each.
fn own_items(
  builder: &ModelBuilder,
  sources: &SourceMap,
  def: DefId,
  own: PackageId,
  list: List,
  into: &mut Vec<(TypeExprId, TyId)>,
) {
  into.clear();
  let items: &[TypeExprId] = match list {
    List::Implements => builder.def_implements(def),
    List::Members => builder.def_union_members(def),
  };
  for &item in items {
    let Some(occurrence) = builder.occurrence(item) else {
      continue;
    };
    if sources.written_by(occurrence.location) != Some(own) {
      continue;
    }
    let Some(ty) = occurrence.ty else {
      continue;
    };
    if matches!(builder.ty_shape(ty), Some(Shape::Unresolved) | None) {
      continue;
    }
    into.push((item, ty));
  }
}

/// The name an entry or a member of the type `ty` is written with: its definition's, or its type
/// parameter's.
fn type_name(builder: &ModelBuilder, ty: TyId) -> &[u8] {
  match builder.ty_shape(ty) {
    Some(Shape::Named(def)) => builder.def_name(def),
    Some(Shape::Param { def, index }) => match builder.type_param(def, index) {
      Some((name, _, _)) => builder.spelling(name),
      None => &[],
    },
    Some(Shape::List(_) | Shape::Unresolved) | None => &[],
  }
}

/// `report` with a label saying `text` at `at`, unless `at` is a whole source, which no label
/// points at.
fn labelled(report: DiagnosticBuilder, at: Location, text: &'static str) -> DiagnosticBuilder {
  if at.is_entire() {
    report
  } else {
    report.label(at, text)
  }
}
