//! A generic definition that reaches itself with a type argument that grows is refused at the
//! occurrence that grows it: the rule a backend composes, which `analyze` never runs.
//!
//! `begin` builds a graph over the whole model. Its nodes are every type parameter of every
//! definition, numbered by definition, and every open type, other than a bare parameter, that a
//! followed position writes, that an application written there has, or that such a type holds:
//! each written occurrence's own type is entered, since a collection's key or element is non-null
//! in its holder's type and so is not the type of the occurrence that writes it. Its edges:
//!
//! - for a named type and each of its open arguments, an edge from the argument into the named
//!   definition's parameter at that position, which grows unless the argument is a bare parameter;
//! - for each open argument or element of an open type, an edge from it into the type, which does
//!   not grow;
//! - for a directive application, the edges of its type, its directive's instantiation with every
//!   type argument, written or filled, which is entered as a named type is. A count of type
//!   arguments that was refused gives the application no type, and no edge.
//!
//! A growing edge whose two ends share a strongly connected component is the refusal: it marks the
//! type that holds it, which keeps the least index of such an argument. One ascending pass over the
//! types then says which hold a marked one, since a type's arguments are interned before it, and
//! each keeps the default that fills the argument it is reported for.
//!
//! The hooks report at a written occurrence in a followed position whose own type is marked, or
//! holds a marked type that none of its written children's own types holds, and at the path of an
//! application whose type is marked, or holds a marked type that none of its written type
//! arguments' own types holds.
//!
//! The rule reads the public model and nothing else: the walk's context and the model's views. Its
//! crate-private names are smear's wording alone, the excerpt a message quotes a name through and
//! the kind's title.

use core::num::TryFromIntError;
use std::{format, string::String, vec, vec::Vec};

use smear_semantics::scc;
use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationId, Cx, DefId, Diagnostic, DiagnosticKind, MemberId, Model, Resolution, Site, Ty,
  TyId, TyKind, TypeExprId, TypeExprKind, TypeParam, Visitor, diagnostic::Excerpt,
};

/// The label at a written type argument that grows.
const WRITTEN: &str = "this type argument wraps a type parameter that it reaches again";

/// The label at the default that fills a type argument that grows.
const FILLED: &str = "this default fills a type argument that wraps a type parameter it reaches \
                      again";

/// The label at the default that holds a type argument that grows.
const HELD: &str = "this default holds a type argument that wraps a type parameter it reaches \
                    again";

/// Refuses a generic definition that reaches itself with a type argument that grows, at the
/// occurrence that grows it: the rule a backend composes when it makes one copy of a generic
/// definition for each instantiation, and so needs that set to be finite.
///
/// ```text
/// type Nest<T> {
///   value: T
///   deeper: Nest<[T]>
/// }
/// type Query { nest: Nest<i32> }
/// ```
///
/// `Nest<i32>` needs `Nest<[i32]>`, which needs `Nest<[[i32]]>`, and so on without end. The test is
/// a cycle test on the graph of generic definitions: a type parameter that receives, through the
/// definitions' bodies, a type that wraps it. `deeper: Nest<T>`, the same instantiation again, and
/// `Swap<U, T>` inside `type Swap<T, U>`, which passes the parameters along, are no growth. Each
/// definition is judged whatever the program instantiates.
///
/// [`analyze`](crate::graphqlx::analyze) never runs it, so
/// [`Analysis::is_ok`](crate::graphqlx::Analysis::is_ok) means what it meant. A backend composes it
/// into its own [`visit`](crate::graphqlx::visit), and its verdict is its own conjunction of the
/// two, as the example below shows.
///
/// Under [`Packages::Root`](crate::graphqlx::Packages::Root) it says nothing of a growth a
/// dependency wrote, even one the root instantiates. A backend that monomorphises any generic its
/// root uses, a dependency's included, walks
/// [`Packages::Supplied`](crate::graphqlx::Packages::Supplied).
///
/// Each diagnostic is an error of the kind
/// [`UnboundedInstantiation`](crate::graphqlx::DiagnosticKind::UnboundedInstantiation), reported at
/// one of three places:
///
/// - a written type occurrence whose type is an instantiation that grows: a member's type, an
///   `implements` entry, a union member, a directive application's type argument, or an occurrence
///   nested in one. It is labelled at the type argument that grows, where the occurrence writes
///   it, or at the default that fills it;
/// - a written occurrence whose type holds such an instantiation only in what defaults filled,
///   labelled at the default that filled the first argument that holds one;
/// - a directive application whose type is such an instantiation, labelled at the type argument
///   that grows where the application writes it, and otherwise at the default that fills it; or
///   whose type holds one only in what its directive's defaults filled, labelled at the default
///   that filled the first argument that holds one. Both at its directive's path.
///
/// One written occurrence or application gives one diagnostic at most. A message names the
/// definition or the directive, never a type, through a bounded excerpt of its name.
///
/// It follows what a copy of an instantiation needs: the types of a definition's fields, of their
/// arguments, of its input fields and of a directive definition's arguments; its `implements`
/// entries; its union members; and the directive applications written on it and on its members,
/// each followed by its type arguments as written and by its complete type arguments, defaults
/// filled ([`Application::ty`](crate::graphqlx::Application::ty)). It does not follow a bound, a
/// type parameter's default as it is written, a root or a value. A default is followed where it
/// fills, since an occurrence's type, and an application's, holds what it filled.
///
/// It costs time linear in the model's definitions, members, applications and types. Until the
/// next walk begins it keeps a fixed number of words for each of the model's types up to the last
/// it entered. It has no limit of its own. When its graph has more nodes than a `u32` numbers,
/// which only a model of tens of gigabytes reaches, it says nothing in that walk.
///
/// # Examples
///
/// ```
/// use smear_analysis::graphqlx::{
///   Layout, Limits, Package, Packages, SourceFile, Tier, UnboundedInstantiation, analyze, visit,
/// };
///
/// let root = Package::new("app", Layout::Directory, Tier::Std).with_files([SourceFile::new(
///   "schema.graphqlx",
///   "type Nest<T> { value: T deeper: Nest<[T]> } type Query { nest: Nest<i32> }",
/// )]);
/// let analysis = analyze(&root, &[], &Limits::new());
/// assert!(analysis.is_ok());
///
/// let mut mine = visit(&analysis, Packages::Supplied, &mut UnboundedInstantiation::new());
/// assert_eq!(mine.len(), 1);
///
/// // The verdict is the caller's conjunction, and one ordered list goes to the renderer.
/// let ok = analysis.is_ok() && !mine.has_errors();
/// assert!(!ok);
/// mine.merge(analysis.diagnostics().clone());
/// ```
pub struct UnboundedInstantiation {
  /// What the last `begin` found of each type, by id.
  types: Vec<Found>,
}

impl UnboundedInstantiation {
  /// The rule, ready to walk. It finds what it reports in each walk's `begin`, so one value serves
  /// many walks.
  #[inline]
  pub const fn new() -> Self {
    Self { types: Vec::new() }
  }
}

impl Default for UnboundedInstantiation {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

impl core::fmt::Debug for UnboundedInstantiation {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("UnboundedInstantiation")
      .finish_non_exhaustive()
  }
}

impl Visitor for UnboundedInstantiation {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    self.types = Vec::new();
    // Only a model of tens of gigabytes has more nodes than a `u32` numbers: the rule then keeps
    // nothing, and says nothing in this walk.
    if let Ok(build) = Build::run(cx.model()) {
      self.types = build.found;
    }
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    if !matches!(
      cx.site(),
      Some(Site::MemberType | Site::Implements | Site::UnionMember | Site::ApplicationTypeArgument)
    ) {
      return;
    }
    let model = cx.model();
    let occurrence = model.type_expr(ty);
    let own = occurrence.ty();
    let Some(&found) = self.types.get(own.get() as usize) else {
      return;
    };
    if !found.holds {
      return;
    }
    // A list is never marked, and holds only what its element, which is written, holds.
    let TyKind::Named { def, .. } = model.ty(own).kind() else {
      return;
    };
    // An occurrence whose own type is a named type is a named occurrence.
    let written: &[TypeExprId] = match occurrence.kind() {
      TypeExprKind::Named { args, .. } => args,
      TypeExprKind::Param { .. } | TypeExprKind::List { .. } | TypeExprKind::Unresolved => &[],
    };
    let name = Excerpt(model.def(def).name().as_bytes());
    if found.marked {
      let label = match written.get(found.index as usize) {
        Some(&argument) => Some((model.type_expr(argument).location(), WRITTEN)),
        None => found
          .default
          .map(|default: TypeExprId| (model.type_expr(default).location(), FILLED)),
      };
      let message = format!("an instantiation of `{name}` that grows without end");
      let help = format!(
        "every instantiation of `{name}` needs this one, with a larger type argument, so a backend \
         that makes one copy per instantiation cannot finish; pass the type parameter itself, or a \
         type that names none"
      );
      report(cx, occurrence.location(), message, help, label);
      return;
    }
    // The report is a written child's, or one below it, when a written child's own type holds.
    if written
      .iter()
      .any(|argument: &TypeExprId| holds(&self.types, model.type_expr(*argument).ty()))
    {
      return;
    }
    let label = found
      .default
      .map(|default: TypeExprId| (model.type_expr(default).location(), HELD));
    let message = format!("an instantiation of `{name}` whose default grows without end");
    // Not the marked case's help: the growth is an instantiation the default holds, which need not
    // be of this occurrence's definition, and the argument that holds it is one the occurrence
    // leaves out.
    let help = format!(
      "this occurrence leaves out a type argument of `{name}` whose default holds an instantiation \
       that grows without end, so a backend that makes one copy per instantiation cannot finish; \
       write the type argument the default fills, or change the default"
    );
    report(cx, occurrence.location(), message, help, label);
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    let model = cx.model();
    let applied = model.application(application);
    let Some(own) = applied.ty() else {
      return;
    };
    let Some(&found) = self.types.get(own.get() as usize) else {
      return;
    };
    if !found.holds {
      return;
    }
    let Resolution::Resolved(directive) = applied.directive() else {
      return;
    };
    let written = applied.type_args();
    let name = Excerpt(model.def(directive).name().as_bytes());
    if found.marked {
      let label = match written.get(found.index as usize) {
        Some(&argument) => Some((model.type_expr(argument).location(), WRITTEN)),
        None => found
          .default
          .map(|default: TypeExprId| (model.type_expr(default).location(), FILLED)),
      };
      let message = format!("an application of `@{name}` that grows without end");
      let help = format!(
        "every instantiation of the definition this application stands on needs `@{name}` with a \
         larger type argument, so a backend that makes one copy per instantiation cannot finish; \
         pass the type parameter itself, or a type that names none"
      );
      report(cx, applied.path_location(), message, help, label);
      return;
    }
    // The report is a written type argument's, or one below it, when a written type argument's own
    // type holds.
    if written
      .iter()
      .any(|argument: &TypeExprId| holds(&self.types, model.type_expr(*argument).ty()))
    {
      return;
    }
    let label = found
      .default
      .map(|default: TypeExprId| (model.type_expr(default).location(), HELD));
    let message = format!("an application of `@{name}` whose default grows without end");
    // Not the marked case's help, for the same reason: the argument that holds the growth is one
    // the application leaves out.
    let help = format!(
      "this application leaves out a type argument of `@{name}` whose default holds an \
       instantiation that grows without end, so a backend that makes one copy per instantiation \
       cannot finish; write the type argument the default fills, or change the default"
    );
    report(cx, applied.path_location(), message, help, label);
  }
}

/// Reports one growth at `at`, saying `message` and `help`, with a label when one has a place.
fn report(
  cx: &mut Cx<'_>,
  at: Location,
  message: String,
  help: String,
  label: Option<(Location, &'static str)>,
) {
  let kind = DiagnosticKind::UnboundedInstantiation;
  let mut diagnostic = Diagnostic::builder(kind.code(), at)
    .severity(kind.severity())
    .message(message)
    .primary_label(kind.title())
    .help(help);
  if let Some((place, text)) = label {
    diagnostic = diagnostic.label(place, text);
  }
  cx.report(diagnostic.build());
}

/// Whether the type `ty` holds a marked type, as `found` records each type by id.
fn holds(found: &[Found], ty: TyId) -> bool {
  found
    .get(ty.get() as usize)
    .is_some_and(|record: &Found| record.holds)
}

/// `value` as a node number, or the overflow that leaves the graph unbuilt.
fn number(value: usize) -> Result<u32, Overflow> {
  u32::try_from(value).map_err(|_: TryFromIntError| Overflow)
}

/// Counts one more edge out of the node `from`, in the slot past `from`'s own.
fn count(starts: &mut [usize], from: u32) {
  let Some(at) = (from as usize).checked_add(1) else {
    return;
  };
  if let Some(slot) = starts.get_mut(at) {
    *slot = slot.saturating_add(1);
  }
}

/// Writes the edge from `from` into `to` in the next free place of `from`'s row.
fn place(cursors: &mut [usize], targets: &mut [u32], from: u32, to: u32) {
  let Some(cursor) = cursors.get_mut(from as usize) else {
    return;
  };
  if let Some(target) = targets.get_mut(*cursor) {
    *target = to;
  }
  *cursor = cursor.saturating_add(1);
}

/// What `begin` found of one type.
#[derive(Clone, Copy, Default)]
struct Found {
  /// Whether it holds a marked type: it is one, or an argument or its element holds one.
  holds: bool,
  /// Whether it is marked: a named type with an argument whose growing edge lies on a cycle.
  marked: bool,
  /// The argument it is reported for: for a marked type, the least index of such an argument; for
  /// one that holds and is not marked, the least index of an argument that holds.
  index: u32,
  /// The default of its definition's type parameter at `index`, which fills that argument for an
  /// occurrence or an application that leaves it out.
  default: Option<TypeExprId>,
}

/// One type as the build enters it.
#[derive(Clone, Copy, Default)]
struct Entry {
  /// Whether the build entered it.
  entered: bool,
  /// Its node, given the first time it was named; only an open type that is no bare parameter
  /// has one.
  node: Option<u32>,
}

/// An edge from an open type that is no bare parameter into a type parameter.
#[derive(Clone, Copy)]
struct Growing {
  /// The type's node.
  from: u32,
  /// The type parameter's node.
  to: u32,
  /// The named type one of whose arguments it leaves: the type it marks when it lies on a cycle.
  holder: TyId,
  /// The position of the argument it leaves.
  index: u32,
}

/// The node an edge from one type leaves, by what the type is.
#[derive(Clone, Copy)]
enum Tail {
  /// A bare parameter, with or without `!`: its parameter's node. Its edge into a type parameter
  /// does not grow.
  Param(u32),
  /// An open type that is no bare parameter: its own node. Its edge into a type parameter grows.
  Type(u32),
}

/// The graph cannot be numbered in a `u32`.
struct Overflow;

/// The graph `begin` builds over one model, and what it finds there.
struct Build<'a> {
  /// The model it is built over.
  model: &'a Model,
  /// The node of each definition's first type parameter, by definition, then one past the last.
  prefix: Vec<u32>,
  /// The node the next type to be named is given, which is also the count of nodes so far.
  next: u32,
  /// Each type as the build entered it, by id.
  entries: Vec<Entry>,
  /// What the build found of each type, by id.
  found: Vec<Found>,
  /// The edges that do not grow, each from its first node into its second.
  plain: Vec<(u32, u32)>,
  /// The edges that grow.
  growing: Vec<Growing>,
  /// The occurrences still to walk.
  occurrences: Vec<TypeExprId>,
  /// The types still to enter.
  pending: Vec<TyId>,
}

impl<'a> Build<'a> {
  /// The graph over `model` and what it finds there, or `Overflow` when it cannot be numbered.
  fn run(model: &'a Model) -> Result<Self, Overflow> {
    let mut build = Self::new(model)?;
    build.roots()?;
    if build.growing.is_empty() {
      return Ok(build);
    }
    let component = build.components()?;
    build.mark(&component);
    build.spread();
    Ok(build)
  }

  /// The build over `model`, its type parameters numbered: one pass over the definitions. The
  /// kernel numbers its discoveries from one in a `u32`, so the count of nodes stays below
  /// `u32::MAX`.
  fn new(model: &'a Model) -> Result<Self, Overflow> {
    let defs = model.defs();
    let mut prefix: Vec<u32> = Vec::with_capacity((defs.len() as usize).saturating_add(1));
    let mut next: u32 = 0;
    for id in defs {
      prefix.push(next);
      let params = number(model.def(id).type_params().len())?;
      next = next
        .checked_add(params)
        .filter(|nodes: &u32| *nodes < u32::MAX)
        .ok_or(Overflow)?;
    }
    prefix.push(next);
    Ok(Self {
      model,
      prefix,
      next,
      entries: Vec::new(),
      found: Vec::new(),
      plain: Vec::new(),
      growing: Vec::new(),
      occurrences: Vec::new(),
      pending: Vec::new(),
    })
  }

  /// The node of `def`'s type parameter `index`, or `None` when `def` has none at `index`. Every
  /// type parameter's node the rule names is this function's.
  fn param(&self, def: DefId, index: u32) -> Option<u32> {
    let at = def.get() as usize;
    let start = *self.prefix.get(at)?;
    let end = *self.prefix.get(at.checked_add(1)?)?;
    start.checked_add(index).filter(|node: &u32| *node < end)
  }

  /// The place of `ty` in the records by id, which grow to hold it.
  fn grow(&mut self, ty: TyId) -> Result<usize, Overflow> {
    let at = ty.get() as usize;
    if at >= self.entries.len() {
      let len = at.checked_add(1).ok_or(Overflow)?;
      self.entries.resize(len, Entry::default());
      self.found.resize(len, Found::default());
    }
    Ok(at)
  }

  /// The node of `ty`, an open type that is no bare parameter: the one it was given when it was
  /// first named, or the next, the count of nodes staying below `u32::MAX`.
  fn node(&mut self, ty: TyId) -> Result<u32, Overflow> {
    let at = self.grow(ty)?;
    let next = self.next;
    let Some(entry) = self.entries.get_mut(at) else {
      return Err(Overflow);
    };
    if let Some(node) = entry.node {
      return Ok(node);
    }
    let after = next
      .checked_add(1)
      .filter(|nodes: &u32| *nodes < u32::MAX)
      .ok_or(Overflow)?;
    entry.node = Some(next);
    self.next = after;
    Ok(next)
  }

  /// The node an edge from `ty` leaves, or `None` for a closed type, which gives no edge.
  fn tail(&mut self, ty: TyId) -> Result<Option<Tail>, Overflow> {
    let model: &'a Model = self.model;
    let view: Ty<'a> = model.ty(ty);
    if view.is_closed() {
      return Ok(None);
    }
    match view.kind() {
      TyKind::Param { def, index } => Ok(self.param(def, index).map(Tail::Param)),
      TyKind::Named { .. } | TyKind::List { .. } => {
        self.node(ty).map(|node: u32| Some(Tail::Type(node)))
      }
      TyKind::Unresolved => Ok(None),
    }
  }

  /// The edge from what `tail` leaves into the type whose node is `holder`, which holds it as an
  /// argument or as its element. It does not grow.
  fn contain(&mut self, tail: Tail, holder: u32) {
    let (Tail::Param(from) | Tail::Type(from)) = tail;
    self.plain.push((from, holder));
  }

  /// The edge from what `tail` leaves into the type parameter whose node is `to`, held by `holder`
  /// at `index`. It grows when `tail` is an open type that is no bare parameter.
  fn pass(&mut self, tail: Tail, to: u32, holder: TyId, index: u32) {
    match tail {
      Tail::Param(from) => self.plain.push((from, to)),
      Tail::Type(from) => self.growing.push(Growing {
        from,
        to,
        holder,
        index,
      }),
    }
  }

  /// Enters `ty` and every type it holds, each once, with an explicit stack: an open type that is
  /// no bare parameter is given its node and the edges of its arguments or its element. A closed
  /// type and a bare parameter are left at once.
  fn enter(&mut self, ty: TyId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    self.pending.push(ty);
    while let Some(ty) = self.pending.pop() {
      let at = self.grow(ty)?;
      let Some(entry) = self.entries.get_mut(at) else {
        continue;
      };
      if entry.entered {
        continue;
      }
      entry.entered = true;
      let view: Ty<'a> = model.ty(ty);
      if view.is_closed() {
        continue;
      }
      match view.kind() {
        TyKind::List { element } => {
          let holder = self.node(ty)?;
          if let Some(tail) = self.tail(element)? {
            self.contain(tail, holder);
          }
          self.pending.push(element);
        }
        TyKind::Named { def, args } => {
          let holder = self.node(ty)?;
          for (position, &argument) in args.iter().enumerate() {
            let index = number(position)?;
            let Some(tail) = self.tail(argument)? else {
              continue;
            };
            self.contain(tail, holder);
            if let Some(to) = self.param(def, index) {
              self.pass(tail, to, ty, index);
            }
            self.pending.push(argument);
          }
        }
        TyKind::Param { .. } | TyKind::Unresolved => {}
      }
    }
    Ok(())
  }

  /// Walks `root` and every occurrence nested in it, with an explicit stack, through what each
  /// occurrence's kind answers, and enters each occurrence's own type. What is written on a type
  /// parameter or on a path that did not resolve is refused where it is written, and not walked.
  fn walk(&mut self, root: TypeExprId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    self.occurrences.push(root);
    while let Some(id) = self.occurrences.pop() {
      let occurrence = model.type_expr(id);
      self.enter(occurrence.ty())?;
      match occurrence.kind() {
        TypeExprKind::Named { args, .. } => self.occurrences.extend_from_slice(args),
        TypeExprKind::List { element } => self.occurrences.push(element),
        TypeExprKind::Param { .. } | TypeExprKind::Unresolved => {}
      }
    }
    Ok(())
  }

  /// The followed positions of every definition that has a type parameter: each field's type and
  /// its arguments' types, each input field's and each directive argument's type, each
  /// `implements` entry and union member, and the type arguments and the type of each application
  /// written on the definition and on each of its members, an enum's values among them.
  fn roots(&mut self) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    for id in model.defs() {
      if self.param(id, 0).is_none() {
        continue;
      }
      let def = model.def(id);
      for entry in def.implements() {
        self.walk(*entry)?;
      }
      for member in def.union_members() {
        self.walk(*member)?;
      }
      for application in def.applications() {
        self.applied(*application)?;
      }
      for field in def.fields() {
        self.member(field)?;
        for argument in model.member(field).arguments() {
          self.member(argument)?;
        }
      }
      for members in [def.input_fields(), def.enum_values(), def.arguments()] {
        for member in members {
          self.member(member)?;
        }
      }
    }
    Ok(())
  }

  /// A member's type, and the type arguments and the type of each of its applications.
  fn member(&mut self, id: MemberId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    let member = model.member(id);
    if let Some(ty) = member.ty() {
      self.walk(ty)?;
    }
    for application in member.applications() {
      self.applied(*application)?;
    }
    Ok(())
  }

  /// An application's type arguments as written, each walked whatever their count; then its type,
  /// its directive's instantiation with every type argument, written or filled, entered as a named
  /// type is. An application whose count of type arguments was refused has no type.
  fn applied(&mut self, id: ApplicationId) -> Result<(), Overflow> {
    let model: &'a Model = self.model;
    let application = model.application(id);
    for argument in application.type_args() {
      self.walk(*argument)?;
    }
    if let Some(instantiation) = application.ty() {
      self.enter(instantiation)?;
    }
    Ok(())
  }

  /// The strongly connected component of each node, named by its least member, over the rule's
  /// own compressed rows: each node's edges counted, the counts summed into where each row starts,
  /// then the rows filled.
  fn components(&self) -> Result<Vec<u32>, Overflow> {
    let nodes = self.next as usize;
    // Node `n`'s row starts at `starts[n]` and ends at `starts[n + 1]`.
    let mut starts: Vec<usize> = vec![0; nodes.checked_add(1).ok_or(Overflow)?];
    for &(from, _) in &self.plain {
      count(&mut starts, from);
    }
    for edge in &self.growing {
      count(&mut starts, edge.from);
    }
    let mut total: usize = 0;
    for start in &mut starts {
      total = total.saturating_add(*start);
      *start = total;
    }
    let mut targets: Vec<u32> = vec![0; total];
    let mut cursors: Vec<usize> = starts.clone();
    for &(from, to) in &self.plain {
      place(&mut cursors, &mut targets, from, to);
    }
    for edge in &self.growing {
      place(&mut cursors, &mut targets, edge.from, edge.to);
    }
    Ok(scc::components(
      nodes,
      |node: usize, successors: &mut Vec<u32>| {
        let start = starts.get(node).copied().unwrap_or(0);
        let end = node
          .checked_add(1)
          .and_then(|after: usize| starts.get(after))
          .copied()
          .unwrap_or(start);
        successors.extend_from_slice(targets.get(start..end).unwrap_or_default());
      },
    ))
  }

  /// Marks the holder of each growing edge whose two ends share a component, each type keeping the
  /// least index of such an argument.
  fn mark(&mut self, component: &[u32]) {
    for edge in &self.growing {
      let (Some(from), Some(to)) = (
        component.get(edge.from as usize),
        component.get(edge.to as usize),
      ) else {
        continue;
      };
      if from != to {
        continue;
      }
      if let Some(found) = self.found.get_mut(edge.holder.get() as usize) {
        found.index = if found.marked {
          found.index.min(edge.index)
        } else {
          edge.index
        };
        found.marked = true;
      }
    }
  }

  /// Says which types hold a marked one, in one ascending pass over the types by id: a type's
  /// arguments and element are interned before it, so each is decided first. Each type that
  /// holds keeps the argument it is reported for and, for a named type, the default of its
  /// definition's type parameter there: the one search of a parameter list, once a type.
  fn spread(&mut self) {
    let model: &'a Model = self.model;
    for at in 0..self.found.len() {
      // Only an open type that is no bare parameter has a node, and may hold.
      if self
        .entries
        .get(at)
        .and_then(|entry: &Entry| entry.node)
        .is_none()
      {
        continue;
      }
      let Ok(id) = u32::try_from(at) else {
        break;
      };
      let ty: Ty<'a> = model.ty(TyId::new(id));
      let Some(&found) = self.found.get(at) else {
        continue;
      };
      let index = if found.marked {
        Some(found.index)
      } else {
        self.held(ty)
      };
      let Some(index) = index else {
        continue;
      };
      let default = match ty.kind() {
        TyKind::Named { def, .. } => model
          .def(def)
          .type_params()
          .nth(index as usize)
          .and_then(|param: TypeParam<'a>| param.default()),
        TyKind::Param { .. } | TyKind::List { .. } | TyKind::Unresolved => None,
      };
      if let Some(record) = self.found.get_mut(at) {
        record.holds = true;
        record.index = index;
        record.default = default;
      }
    }
  }

  /// For `ty`, which is not marked, the least index of an argument that holds a marked type, or
  /// `0` for a list whose element holds one; `None` when nothing it holds does.
  fn held(&self, ty: Ty<'_>) -> Option<u32> {
    match ty.kind() {
      TyKind::Named { args, .. } => args
        .iter()
        .position(|argument: &TyId| holds(&self.found, *argument))
        .and_then(|position: usize| u32::try_from(position).ok()),
      TyKind::List { element } => holds(&self.found, element).then_some(0),
      TyKind::Param { .. } | TyKind::Unresolved => None,
    }
  }
}

// The rule may cross threads with the analysis it walks.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<UnboundedInstantiation>;
};
