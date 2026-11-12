//! Rules are visitors: the [`Visitor`] trait, the context [`Cx`] each hook is handed, and
//! [`visit`], the one driver that walks a model for a visitor.
//!
//! The walk reads the finished model and writes nothing: a visitor sees the model through its
//! views, and reports through the context. Visitors compose at compile time.

mod compose;
mod walk;

pub(crate) use walk::drive;

use super::{
  analysis::Analysis,
  diagnostic::{Diagnostic, Diagnostics, Reporter},
  id::{ApplicationId, DefId, MemberId, ModuleId, PackageId, TypeExprId, ValueId},
  model::{Contribution, Model, PackageKind},
  source::SourceMap,
};

/// Which packages' writing a walk covers.
///
/// A walk covers what each package WROTE. The package that wrote a contribution is the package of
/// the source it is written in: for a definition's own contribution that is the definition's
/// package, except for a GraphQL built-in the root package replaces, whose own contribution the
/// root writes; for an extension, the package whose source holds the extension. So `extend type
/// Point { … }` in the root package is the root's writing, although `Point` is a dependency's.
///
/// The walk enters every definition with at least one covered contribution, and the package and
/// the module that hold it, even when that package is not covered. Inside, it reaches only what the
/// covered contributions wrote; [`visit`] lists what that is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Packages {
  /// The root package.
  Root,
  /// The root package and every supplied dependency, whether the root reaches it or not: what
  /// [`analyze`](super::analyze) validates.
  Supplied,
  /// Every package: the supplied ones, and `core`, `alloc` and `std`.
  All,
}

impl Packages {
  /// Whether a package of `kind` is covered.
  fn holds(self, kind: PackageKind) -> bool {
    match self {
      Self::Root => kind == PackageKind::Root,
      Self::Supplied => matches!(kind, PackageKind::Root | PackageKind::Dependency),
      Self::All => true,
    }
  }
}

/// Where an occurrence sits: what holds the outermost type occurrence or value of the one the walk
/// is reaching.
///
/// [`Cx::site`] answers it for the occurrence the walk is reaching and for every occurrence nested
/// in it: an argument of `Page<User>` sits where `Page<User>` sits. A rule that needs more reads the
/// outermost occurrence, which the walk reaches first, and its children from it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Site {
  /// A member's type: a field's, an argument's or an input field's.
  MemberType,
  /// A member's default value: an argument's or an input field's.
  Default,
  /// An entry of an object's or an interface's `implements`.
  Implements,
  /// A member of a union.
  UnionMember,
  /// The interface or marker a bound on a type parameter names.
  Bound,
  /// A type parameter's default.
  TypeParamDefault,
  /// The value of a directive application's argument.
  ApplicationArgument,
  /// A type argument of a directive application.
  ApplicationTypeArgument,
  /// A root operation's type, written in a schema definition or extension.
  RootOperation,
}

/// A rule, or any other reader of a model: what the walk calls as it reaches each entity.
///
/// Every hook has an empty default body, so a visitor implements the hooks it needs and keeps its
/// own state in its fields. [`begin`](Self::begin) is where it resets that state, so one value
/// serves many walks. Hooks return nothing: a visitor reports through [`Cx::report`], and the walk
/// always reaches everything it covers.
///
/// The trait is object-safe and stays so, and a hook added later arrives with a default body, so it
/// breaks no implementation.
///
/// # Composition
///
/// Visitors compose by their types, at compile time:
///
/// - a tuple of up to twelve visitors is a visitor whose every hook calls each element's, first to
///   last;
/// - `Option<V>` is a visitor switched at run time, and `None` does no work;
/// - `&mut V` and `Box<V>` are visitors, `&mut dyn Visitor` and `Box<dyn Visitor>` included, so a
///   rule unknown at compile time is one element of a composition;
/// - `()` is the visitor that does nothing.
///
/// A rule's parameters are its fields, and a choice between two sets of rules is a branch between
/// two values. Rules do not see one another and their diagnostics are sorted, so the order of
/// composition never changes what a walk answers.
///
/// # Examples
///
/// A rule that refuses every definition named `Legacy`, holding the definition's view while it
/// reports:
///
/// ```
/// use smear_analysis::graphqlx::{
///   Cx, DefId, Diagnostic, Layout, Limits, Package, Packages, SourceFile, Tier, Visitor, analyze,
///   visit,
/// };
/// use tokora::diagnostic::Code;
///
/// struct NoLegacy;
///
/// impl Visitor for NoLegacy {
///   fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
///     let def = cx.model().def(def);
///     if def.name() == "Legacy" {
///       cx.report(
///         Diagnostic::builder(Code::new("app::no-legacy"), def.location())
///           .message("`Legacy` is retired")
///           .build(),
///       );
///     }
///   }
/// }
///
/// let root = Package::new("app", Layout::Directory, Tier::Std)
///   .with_files([SourceFile::new("schema.graphqlx", "type Query { ok: Int }")]);
/// let analysis = analyze(&root, &[], &Limits::new());
/// let diagnostics = visit(&analysis, Packages::Root, &mut NoLegacy);
/// assert!(diagnostics.is_empty());
///
/// // The same rule, composed with one switched off and one chosen at run time.
/// let chosen: Box<dyn Visitor> = Box::new(NoLegacy);
/// let mut rules = (NoLegacy, None::<NoLegacy>, chosen);
/// assert!(visit(&analysis, Packages::Root, &mut rules).is_empty());
/// ```
pub trait Visitor {
  /// The walk begins: nothing has been reached, and every position [`Cx`] answers is `None`.
  fn begin(&mut self, _cx: &mut Cx<'_>) {}

  /// The walk enters `package`: the modules it enters follow, then
  /// [`exit_package`](Self::exit_package).
  fn enter_package(&mut self, _cx: &mut Cx<'_>, _package: PackageId) {}

  /// The walk leaves `package`.
  fn exit_package(&mut self, _cx: &mut Cx<'_>, _package: PackageId) {}

  /// The walk enters `module`: the definitions it enters that the module holds itself follow, and
  /// not its child modules', then [`exit_module`](Self::exit_module).
  fn enter_module(&mut self, _cx: &mut Cx<'_>, _module: ModuleId) {}

  /// The walk leaves `module`.
  fn exit_module(&mut self, _cx: &mut Cx<'_>, _module: ModuleId) {}

  /// The walk enters `def`: what its covered contributions wrote follows, then
  /// [`exit_def`](Self::exit_def).
  fn enter_def(&mut self, _cx: &mut Cx<'_>, _def: DefId) {}

  /// The walk leaves `def`.
  fn exit_def(&mut self, _cx: &mut Cx<'_>, _def: DefId) {}

  /// The walk enters `member`: its type, its default and its applications follow, and for a field
  /// each of its arguments, entered and left in turn; then [`exit_member`](Self::exit_member).
  fn enter_member(&mut self, _cx: &mut Cx<'_>, _member: MemberId) {}

  /// The walk leaves `member`.
  fn exit_member(&mut self, _cx: &mut Cx<'_>, _member: MemberId) {}

  /// The walk reaches the type occurrence `ty`. It reaches every written one: an outermost
  /// occurrence first, then each of its arguments or its element, nested in pre-order.
  /// [`Cx::site`] says where the outermost one sits.
  fn type_expr(&mut self, _cx: &mut Cx<'_>, _ty: TypeExprId) {}

  /// The walk reaches the directive application `application`: its type arguments follow, then
  /// each argument's value.
  fn application(&mut self, _cx: &mut Cx<'_>, _application: ApplicationId) {}

  /// The walk reaches the value `value`. It reaches every one: an outermost value first, then each
  /// list or set element, each map entry's key and then its value, and each object field's value,
  /// nested in pre-order. [`Cx::site`] says where the outermost one sits.
  fn value(&mut self, _cx: &mut Cx<'_>, _value: ValueId) {}

  /// The walk reaches the schema, after every package: each written root's type follows, then the
  /// schema's applications.
  fn schema(&mut self, _cx: &mut Cx<'_>) {}

  /// The walk ends: everything it covers has been reached, and every position [`Cx`] answers is
  /// `None`.
  fn end(&mut self, _cx: &mut Cx<'_>) {}
}

/// Where the walk is, and where a visitor reports: what every hook is handed.
///
/// The model and the source map it answers are the analysis's, borrowed for as long as the
/// analysis is and not for as long as a hook borrows the context, so a visitor may hold a view of
/// the model while it reports.
pub struct Cx<'a> {
  model: &'a Model,
  sources: &'a SourceMap,
  reporter: Reporter,
  package: Option<PackageId>,
  module: Option<ModuleId>,
  def: Option<DefId>,
  member: Option<MemberId>,
  contribution: Option<Contribution<'a>>,
  site: Option<Site>,
  application: Option<ApplicationId>,
}

impl<'a> Cx<'a> {
  /// The context of a walk over `model`, whose sources `sources` numbers, reporting into
  /// `reporter`: at no entity yet.
  fn new(model: &'a Model, sources: &'a SourceMap, reporter: Reporter) -> Self {
    Self {
      model,
      sources,
      reporter,
      package: None,
      module: None,
      def: None,
      member: None,
      contribution: None,
      site: None,
      application: None,
    }
  }

  /// The model the walk reads.
  #[inline]
  pub fn model(&self) -> &'a Model {
    self.model
  }

  /// The analysis's sources: each one's package, path and length. A visitor never needs a
  /// source's text, because the model owns every spelling it answers with.
  #[inline]
  pub fn sources(&self) -> &'a SourceMap {
    self.sources
  }

  /// The package the walk is in: between [`enter_package`](Visitor::enter_package) and
  /// [`exit_package`](Visitor::exit_package), that package; in the schema's walk, the root package;
  /// `None` in [`begin`](Visitor::begin) and [`end`](Visitor::end).
  #[inline]
  pub fn package(&self) -> Option<PackageId> {
    self.package
  }

  /// The module the walk is in, between [`enter_module`](Visitor::enter_module) and
  /// [`exit_module`](Visitor::exit_module); `None` outside every module, the schema's walk
  /// included.
  #[inline]
  pub fn module(&self) -> Option<ModuleId> {
    self.module
  }

  /// The definition the walk is in, between [`enter_def`](Visitor::enter_def) and
  /// [`exit_def`](Visitor::exit_def); `None` outside every definition.
  #[inline]
  pub fn def(&self) -> Option<DefId> {
    self.def
  }

  /// The innermost member the walk is in, between [`enter_member`](Visitor::enter_member) and
  /// [`exit_member`](Visitor::exit_member): within a field's argument, the argument, and within the
  /// rest of the field, the field. `None` outside every member.
  #[inline]
  pub fn member(&self) -> Option<MemberId> {
    self.member
  }

  /// The contribution whose writing the walk is reaching. Its package, the package of the source
  /// it is written in, is where the current occurrence was written.
  ///
  /// Inside a definition it is the definition's own contribution while the walk reaches the type
  /// parameters' defaults and the bounds, and otherwise the contribution that wrote each
  /// `implements` entry, union member, application and member; a field's arguments are their
  /// field's contribution's. In the schema's walk it is the schema contribution that wrote each
  /// application. It is `None` everywhere else: in [`begin`](Visitor::begin) and
  /// [`end`](Visitor::end), in the package and module hooks, in
  /// [`enter_def`](Visitor::enter_def) and [`exit_def`](Visitor::exit_def), in
  /// [`schema`](Visitor::schema), and for a written root's type, since the model records no
  /// contribution for a root.
  #[inline]
  pub fn contribution(&self) -> Option<Contribution<'a>> {
    self.contribution
  }

  /// Where the outermost occurrence of the current one sits: `Some` exactly in
  /// [`type_expr`](Visitor::type_expr) and [`value`](Visitor::value), for the occurrence handed to
  /// the hook, and `None` in every other hook.
  #[inline]
  pub fn site(&self) -> Option<Site> {
    self.site
  }

  /// The directive application the walk is in: in [`application`](Visitor::application), and in
  /// the [`type_expr`](Visitor::type_expr) and [`value`](Visitor::value) hooks of its type
  /// arguments and its arguments' values. `None` everywhere else.
  #[inline]
  pub fn application(&self) -> Option<ApplicationId> {
    self.application
  }

  /// Reports `diagnostic`, built with [`Diagnostic::builder`]: with a code of the visitor's own,
  /// at any source, the analysis's or one of the consumer's.
  ///
  /// The walk's list keeps the least diagnostics, in their one total order, up to the ceiling the
  /// analysis was made with, so which are kept depends on the diagnostics alone. Every error counts
  /// toward [`Diagnostics::error_count`], kept or dropped.
  ///
  /// In [`analyze`](super::analyze)'s own walk, an error also marks the contributions and members
  /// that hold where it points, kept or dropped, once the walk has ended. A consumer's walk marks
  /// nothing.
  #[inline]
  pub fn report(&mut self, diagnostic: Diagnostic) {
    self.reporter.report(diagnostic);
  }
}

impl core::fmt::Debug for Cx<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Cx")
      .field("package", &self.package)
      .field("module", &self.module)
      .field("def", &self.def)
      .field("member", &self.member)
      .field("contribution", &self.contribution)
      .field("site", &self.site)
      .field("application", &self.application)
      .finish_non_exhaustive()
  }
}

/// Walks `analysis`'s model for `visitor`, over what `packages` wrote, and answers what the
/// visitor reported.
///
/// # The walk
///
/// It is the one driver: [`analyze`](super::analyze) runs smear's own rules through it too, over
/// [`Packages::Supplied`]. It reaches each entity it covers exactly once, in id order:
///
/// 1. [`begin`](Visitor::begin);
/// 2. each package it enters: [`enter_package`](Visitor::enter_package), the modules it enters,
///    [`exit_package`](Visitor::exit_package);
/// 3. each module it enters, flat: [`enter_module`](Visitor::enter_module), the definitions it
///    enters that the module holds itself, [`exit_module`](Visitor::exit_module);
/// 4. each definition it enters: [`enter_def`](Visitor::enter_def); its type parameters' defaults,
///    then its bounds' interfaces; its `implements` entries; its union members; its applications;
///    its direct members; [`exit_def`](Visitor::exit_def);
/// 5. each member: [`enter_member`](Visitor::enter_member); its type; its default; its
///    applications; a field's arguments, each a member nested in the field;
///    [`exit_member`](Visitor::exit_member);
/// 6. each application: [`application`](Visitor::application); its type arguments; each
///    argument's value;
/// 7. [`schema`](Visitor::schema), when the root package is covered: the type of each written
///    root, query, mutation, then subscription; then the schema's applications;
/// 8. [`end`](Visitor::end).
///
/// [`type_expr`](Visitor::type_expr) is called for every written type occurrence, the outermost
/// first and then its arguments or its element, and [`value`](Visitor::value) for every value, the
/// outermost first and then what it holds. The walk does that nesting itself, with explicit stacks:
/// a visitor cannot miss an inner occurrence, and no input deepens the call stack.
///
/// # What it covers
///
/// What `packages` wrote, as [`Packages`] says. The walk enters every definition with at least one
/// covered contribution, and the package and the module that hold it, even when that package is
/// not covered; and every module of a covered package, whether it holds a definition or not.
/// Inside a definition it reaches the covered contributions' `implements` entries, union members,
/// applications and direct members, and the definition's type parameters, their defaults and its
/// bounds when its own contribution is covered.
///
/// # What it answers
///
/// The diagnostics the visitor reported, in their one total order, under the diagnostic ceiling
/// the analysis was made with: [`Diagnostics::truncated`] says whether the ceiling dropped any, and
/// [`Diagnostics::error_count`] counts every error, kept or dropped. The analysis's own diagnostics
/// are not in it, and [`Diagnostics::merge`] makes one list of the two.
///
/// The analysis is not changed: a consumer's walk marks nothing, so every `has_errors()` answers
/// what [`analyze`](super::analyze) found. A visitor may walk an analysis that is not ok. It then
/// meets the `Unresolved` shapes and entities whose `has_errors()` is set, which the walk reaches as
/// it reaches everything else.
pub fn visit<V: Visitor + ?Sized>(
  analysis: &Analysis,
  packages: Packages,
  visitor: &mut V,
) -> Diagnostics {
  let reporter = Reporter::new(analysis.ceiling());
  // A consumer's walk marks nothing: the analysis is not its to change, so the marks its errors
  // would make are dropped.
  let (diagnostics, _) = drive(
    analysis.model(),
    analysis.sources(),
    packages,
    reporter,
    visitor,
  )
  .finish();
  diagnostics
}

// The selection and the occurrence sites may cross threads with the results they describe.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<Packages>;
  let _ = assert_send_sync::<Site>;
};
