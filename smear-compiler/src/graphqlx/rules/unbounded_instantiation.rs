//! A generic definition that reaches itself with a type argument that grows is refused at the
//! occurrence that grows it: the rule a backend composes, which `analyze` never runs.
//!
//! `begin` builds the growth graph over the whole model, the one `types::growth` builds: the graph
//! the analysis builds once more after instantiation to answer `Def::grows`, so that which
//! definitions grow is one fact, and a test holds the two to one answer over the corpus. A growing
//! edge whose two ends share a strongly connected component is the refusal: it marks the type that
//! holds it, which keeps the least index of such an argument, and each type that holds a marked one
//! keeps the default that fills the argument it is reported for.
//!
//! The hooks report at a written occurrence in a followed position whose own type is marked, or
//! holds a marked type that none of its written children's own types holds, and at the path of an
//! application whose type is marked, or holds a marked type that none of its written type
//! arguments' own types holds.
//!
//! The rule reads the public model and nothing else: the walk's context and the model's views,
//! through the graph, which reads the views alone. Its crate-private names are smear's wording
//! alone, the excerpt a message quotes a name through and the kind's title, and the graph's.

use std::{format, string::String, vec::Vec};

use tokora::diagnostic::Location;

use crate::graphqlx::{
  ApplicationId, Cx, Diagnostic, DiagnosticKind, Model, NamedTy, NamedTypeExpr, Resolution, Site,
  SourceMap, TyKind, TypeExprId, TypeExprKind, Visitor,
  diagnostic::Excerpt,
  rules::Meanings,
  types::growth::{self, Found, holds},
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
/// It does not follow an interface's instantiation into the types that implement it, so a backend
/// that emits the implementors of each interface instantiation checks for growth through them
/// itself.
///
/// Its graph is the one the analysis reads [`Def::grows`](crate::graphqlx::Def::grows) off: a
/// definition grows when one of its type parameters lies on a cycle of it that holds a growing
/// edge, and the rule reports where such an edge is written. So an analysis in which no
/// definition grows gives the rule nothing to report, and one it reports in holds a definition
/// that grows.
///
/// It costs time linear in the model's definitions, members, applications and types. Until the
/// next walk begins it keeps a fixed number of words for each of the model's types up to the last
/// it entered. It has no limit of its own. When its graph has more nodes than a `u32` numbers,
/// which only a model of tens of gigabytes reaches, it says nothing in that walk.
///
/// # Examples
///
/// ```
/// use smear_compiler::graphqlx::{
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

  /// What the rule's `begin` finds over `model`, whose files `sources` holds: the graph, with its
  /// marks, or `Err` when it cannot be numbered. The test-support door reads the definitions it
  /// finds to grow from here.
  pub(crate) fn graph(
    model: &Model,
    sources: &SourceMap,
  ) -> Result<growth::Grown, growth::Overflow> {
    growth::grown(model, sources, Meanings::of(model), true)
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
    if let Ok(grown) = Self::graph(cx.model(), cx.sources()) {
      self.types = grown.types;
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
    let TyKind::Named(NamedTy { def, .. }) = model.ty(own).kind() else {
      return;
    };
    // An occurrence whose own type is a named type is a named occurrence.
    let written: &[TypeExprId] = match occurrence.kind() {
      TypeExprKind::Named(NamedTypeExpr { args, .. }) => args,
      TypeExprKind::Param(_) | TypeExprKind::List(_) | TypeExprKind::Unresolved => &[],
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

// The rule may cross threads with the analysis it walks.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<UnboundedInstantiation>;
};
