//! W §13's invariants of an analysis that is ok, held by a census: one visitor, `Invariants`, walks
//! every checked analysis of the GraphQLx corpus and of the GraphQL schema fixtures, and every
//! program copied here from a test that asserts it ok, under `Packages::All`, and checks what the
//! public surface shows of each invariant:
//!
//! - nothing is unresolved: no type occurrence, value or application is, and no type the walk
//!   reaches, each walked whole, is the unresolved type;
//! - member names are unique within their owner, per kind;
//! - every named type holds one argument for each of its definition's type parameters, and every
//!   named occurrence writes no more than that;
//! - every application's arguments are its directive's, in its order, and where the directive
//!   takes no type parameter a required one is present and each is coerced to its type;
//! - every definition's tier is at most its package's, a meta-type aside.
//!
//! An analysis is checked when its diagnostics were not truncated and its every error is GraphQL's
//! missing query root. Most corpus files are parts of a schema and write none, and the query root
//! is a rule's code, so no phase spoke and no other rule did: a consumer that waives that code
//! reads such an analysis as ok, and is promised no `Unresolved` there. The opt-in rule
//! `UnboundedInstantiation` walks beside the visitor over every checked analysis, and alone over
//! every other corpus file, so that it meets every corpus file and is shown not to panic.
//!
//! What it cannot show: that every bound holds, which the bounds pass's tests hold; W §13's last
//! line, the rules of L §10 that GraphQLx shares with GraphQL, which the differential test holds on
//! the GraphQL corpora; and what only a fill derives. It is about what a model means, not about the
//! names it has: it holds however the surface is renamed.
#![cfg(all(feature = "std", feature = "analysis"))]

use std::path::{Path, PathBuf};

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationArgument, ApplicationId, Cx, DefId, Diagnostic, Diagnostics, IdRange,
    Layout, Limits, MemberId, Model, Origin, Package, Packages, Resolution, SourceFile, Tier, TyId,
    TyKind, TypeExprId, TypeExprKind, UnboundedInstantiation, ValueId, ValueKind, Visitor, analyze,
    visit,
  },
  diagnostic::Diagnose,
};

/// The opt-in rule's code.
const UNBOUNDED: &str = "smear::graphqlx::unbounded-instantiation";

/// Every file of the GraphQLx corpus and of the GraphQL schema fixtures, by path, each with its
/// bytes.
fn corpus() -> Vec<(PathBuf, Vec<u8>)> {
  let tests = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
  let mut paths: Vec<PathBuf> = Vec::new();
  for directory in [
    tests.join("corpusx"),
    tests.join("fixtures").join("schemas"),
  ] {
    let before = paths.len();
    let entries = std::fs::read_dir(&directory).unwrap_or_else(|error: std::io::Error| {
      panic!(
        "the corpus at {} is unreadable: {error}",
        directory.display()
      )
    });
    for entry in entries {
      let path = entry.expect("a corpus directory entry").path();
      if path.is_file() {
        paths.push(path);
      }
    }
    assert!(
      paths.len() > before,
      "the corpus at {} holds no file",
      directory.display()
    );
  }
  paths.sort();
  let mut files: Vec<(PathBuf, Vec<u8>)> = Vec::with_capacity(paths.len());
  for path in paths {
    let text = std::fs::read(&path)
      .unwrap_or_else(|error: std::io::Error| panic!("{} is unreadable: {error}", path.display()));
    files.push((path, text));
  }
  files
}

/// An analysis of the corpus file at `path`, whose bytes are `text`, as the one file of the `Flat`
/// root package `app`, at tier `Std`. A file the type-system parser refuses is an analysis too, of
/// the built-in library alone.
///
/// The path is printed first: a panic inside the analysis cannot say which file it was reading,
/// and the last line a failing test printed then does.
fn analyzed(path: &Path, text: &[u8]) -> Analysis {
  println!("{}", path.display());
  let root: Package<&[u8]> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], &Limits::new())
}

/// GraphQL's code for a schema with no query root. Most files of the corpora are parts of a schema
/// and write none, so the census reads a file whose only errors are this code as ok.
const NO_QUERY_ROOT: &str = "smear::schema::missing-query-root-operation-type";

/// Whether `analysis` is ok but for its program having no query root: every error it holds, if
/// any, is that code.
fn ok_but_for_the_root(analysis: &Analysis) -> bool {
  let diagnostics = analysis.diagnostics();
  let waived = diagnostics
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() == NO_QUERY_ROOT)
    .count();
  diagnostics.error_count() == waived
}

/// Whether the census checks `analysis`: its diagnostics were not truncated, so every error is
/// among them, and it is ok but for its program having no query root.
fn checked(analysis: &Analysis) -> bool {
  !analysis.diagnostics().truncated() && ok_but_for_the_root(analysis)
}

/// Files of the schema fixtures that the census must read as ok: whole schemas, each with its
/// query root, that both dialects accept, and that write defaults of every shape. A census that
/// reads none of them, or no file at all, checks nothing, and fails.
const KNOWN_OK: [&str; 3] = [
  "countries.graphql",
  "introspection_agreement.graphql",
  "introspection_defaults.graphql",
];

/// The file of the GraphQLx corpus that the census must read as ok: a generic definition with a
/// default, `type T<A, B = Int> { f: A }`, which writes no query root.
const KNOWN_OK_X: &str = "valid_x_definition_generics.graphqlx";

/// Asserts that the census read each of [`KNOWN_OK`] among `checked`, the files it read as ok, and
/// prints how many it read.
fn checked_the_known_ok(checked: &[PathBuf]) {
  println!("{} corpus files read as ok", checked.len());
  for name in KNOWN_OK {
    assert!(
      checked
        .iter()
        .any(|path: &PathBuf| path.file_name() == Some(std::ffi::OsStr::new(name))),
      "`{name}` is not read as ok: {} files are",
      checked.len()
    );
  }
}

/// Asserts that every diagnostic of `reported`, which the opt-in rule answered walking `name`, has
/// the rule's code.
fn the_rules_own(name: &str, reported: &Diagnostics) {
  for diagnostic in reported.iter() {
    assert_eq!(
      diagnostic.code().as_str(),
      UNBOUNDED,
      "{name}: {diagnostic:#?}"
    );
  }
}

/// Walks `analysis`, which the census checks, with `Invariants` and the opt-in rule beside it,
/// under `Packages::All`: every breach met, each naming `name`. The census is composed by
/// reference, so that what it met is read once the walk has ended.
fn census(name: &str, analysis: &Analysis) -> Vec<String> {
  let mut invariants = Invariants::new(name);
  let reported = visit(
    analysis,
    Packages::All,
    &mut (&mut invariants, UnboundedInstantiation::new()),
  );
  the_rules_own(name, &reported);
  invariants.failures
}

/// The census's visitor: each breach of W §13 it meets, as one line naming the file or the program
/// and the entity.
struct Invariants<'n> {
  /// The corpus file or the program walked, named on each breach.
  name: &'n str,
  /// Whether each type has been walked, by id, grown on demand.
  walked: Vec<bool>,
  /// The types still to walk.
  pending: Vec<TyId>,
  /// Each breach met.
  failures: Vec<String>,
}

impl<'n> Invariants<'n> {
  /// The census of a walk of `name`, which has met nothing yet.
  fn new(name: &'n str) -> Self {
    Self {
      name,
      walked: Vec::new(),
      pending: Vec::new(),
      failures: Vec::new(),
    }
  }

  /// Records one breach, `what`.
  fn fail(&mut self, what: String) {
    self.failures.push(format!("{}: {what}", self.name));
  }

  /// Walks `root` and every type it holds, each type once over the whole walk, with an explicit
  /// stack: none is the unresolved type, and each named type holds one argument for each of its
  /// definition's type parameters. `from` names what `root` was read off, as its kind and its id.
  fn walk(&mut self, model: &Model, root: TyId, from: (&str, u32)) {
    self.pending.push(root);
    while let Some(id) = self.pending.pop() {
      let index = id.get() as usize;
      if index >= self.walked.len() {
        self.walked.resize(index + 1, false);
      }
      if self.walked[index] {
        continue;
      }
      self.walked[index] = true;
      match model.ty(id).kind() {
        TyKind::Named { def, args, .. } => {
          let params = model.def(def).type_params().len();
          if args.len() != params {
            self.fail(format!(
              "type {} of the {} {}, a `{}`, holds {} arguments for {params} type parameters",
              id.get(),
              from.0,
              from.1,
              model.def(def).name(),
              args.len()
            ));
          }
          self.pending.extend_from_slice(args);
        }
        TyKind::List { element, .. } => self.pending.push(element),
        TyKind::Unresolved => self.fail(format!(
          "type {} of the {} {} is the unresolved type",
          id.get(),
          from.0,
          from.1
        )),
        _ => {}
      }
    }
  }

  /// Asserts that no two of `members`, the `kind` of the definition `owner`, share a name.
  fn unique(&mut self, model: &Model, owner: DefId, kind: &str, members: IdRange<MemberId>) {
    let mut names: Vec<&str> = members
      .into_iter()
      .map(|member: MemberId| model.member(member).name())
      .collect();
    names.sort_unstable();
    for pair in names.windows(2) {
      if pair[0] == pair[1] {
        self.fail(format!(
          "two {kind} of `{}`, definition {}, are named `{}`",
          model.def(owner).name(),
          owner.get(),
          pair[0]
        ));
      }
    }
  }
}

impl Visitor for Invariants<'_> {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let definition = model.def(def);
    self.unique(model, def, "fields", definition.fields());
    self.unique(model, def, "input fields", definition.input_fields());
    self.unique(model, def, "enum values", definition.enum_values());
    self.unique(model, def, "arguments", definition.arguments());
    for field in definition.fields() {
      self.unique(
        model,
        def,
        "arguments of one field",
        model.member(field).arguments(),
      );
    }
    if matches!(definition.origin(), Origin::Compat) {
      return;
    }
    let package = model.package(model.module(definition.module()).package());
    if definition.tier() > package.tier() {
      self.fail(format!(
        "`{}`, definition {}, requires {:?}, above its package `{}`'s {:?}",
        definition.name(),
        def.get(),
        definition.tier(),
        package.name(),
        package.tier()
      ));
    }
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    let model = cx.model();
    let occurrence = model.type_expr(ty);
    match occurrence.kind() {
      TypeExprKind::Unresolved => self.fail(format!("occurrence {} is unresolved", ty.get())),
      TypeExprKind::Named { def, args, .. } => {
        let params = model.def(def).type_params().len();
        if args.len() > params {
          self.fail(format!(
            "occurrence {} writes {} arguments of `{}`, which takes {params}",
            ty.get(),
            args.len(),
            model.def(def).name()
          ));
        }
      }
      _ => {}
    }
    self.walk(model, occurrence.ty(), ("occurrence", ty.get()));
  }

  fn value(&mut self, cx: &mut Cx<'_>, value: ValueId) {
    let model = cx.model();
    let reached = model.value(value);
    if matches!(reached.kind(), ValueKind::Unresolved) {
      self.fail(format!("value {} is unresolved", value.get()));
    }
    self.walk(model, reached.ty(), ("value", value.get()));
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    let model = cx.model();
    let applied = model.application(application);
    let id = application.get();
    if let Some(ty) = applied.ty() {
      self.walk(model, ty, ("application", id));
    }
    for argument in applied.ty_args() {
      self.walk(model, *argument, ("application", id));
    }
    let Resolution::Resolved(directive) = applied.directive() else {
      self.fail(format!("application {id} did not resolve"));
      return;
    };
    let definition = model.def(directive);
    let given = applied.arguments();
    for argument in given {
      if !definition.arguments().contains(argument.argument()) {
        self.fail(format!(
          "application {id} sets member {}, no argument of `@{}`",
          argument.argument().get(),
          definition.name()
        ));
      }
    }
    for pair in given.windows(2) {
      if pair[0].argument().get() >= pair[1].argument().get() {
        self.fail(format!(
          "application {id} holds member {} at or after member {}",
          pair[1].argument().get(),
          pair[0].argument().get()
        ));
      }
    }
    // Under type arguments an argument's type is its occurrence's rebuilt, and what is required
    // is read there: both are checked only where the directive takes no type parameter.
    if definition.type_params().next().is_some() {
      return;
    }
    for member in definition.arguments() {
      let declared = model.member(member);
      let Some(written) = declared.ty() else {
        continue;
      };
      let occurrence = model.type_expr(written);
      let set = given
        .iter()
        .find(|argument: &&ApplicationArgument| argument.argument() == member);
      match set {
        Some(argument) => {
          if model.value(argument.value()).ty() != occurrence.ty() {
            self.fail(format!(
              "application {id}'s `{}` is not coerced to the argument's type",
              declared.name()
            ));
          }
        }
        None => {
          if occurrence.non_null() && declared.default().is_none() {
            self.fail(format!(
              "application {id} leaves out the required `{}`",
              declared.name()
            ));
          }
        }
      }
    }
  }
}

/// A package named `name`, `Flat`, at `tier`, holding `files`, `(path, text)`.
fn package(
  name: &str,
  tier: Tier,
  files: &[(&'static str, &'static str)],
) -> Package<&'static str> {
  Package::new(name, Layout::Flat, tier).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// One program: the test it is copied from, its root package and its dependencies.
type Program = (
  &'static str,
  Package<&'static str>,
  Vec<Package<&'static str>>,
);

/// The root package `app` at `tier`, holding the one file `schema.graphqlx` of `text`, with no
/// dependency: the program `origin` names.
fn one_file(origin: &'static str, tier: Tier, text: &'static str) -> Program {
  (
    origin,
    package("app", tier, &[("schema.graphqlx", text)]),
    Vec::new(),
  )
}

/// Programs copied from tests that assert them ok, each analyzed here under the default limits:
/// together they write generic definitions with defaults and their instantiations, a generic
/// directive applied with written and filled type arguments and with defaulted arguments, object
/// literals and their copies, enum values, collections and lone values in values, `@extern`, the
/// implementation of interfaces, and packages at `core` and `alloc`, where the tier invariant
/// says something that a corpus file, every one at `std`, does not.
fn programs() -> Vec<Program> {
  let x8 = package(
    "app",
    Tier::Core,
    &[(
      "schema.graphqlx",
      "type Query {\n  x: Int\n}\n\
       directive @doc(text: string!, tags: [string!]) on FIELD_DEFINITION\n\
       directive @g<T = string> on OBJECT\n\
       type Q @g<String> {\n  y: Int @doc(text: \"x\")\n}\n\
       type R @g<[string]> {\n  z: Int\n}\n\
       directive @h(n: i64 @heap) on OBJECT\n\
       directive @b<T> on OBJECT where T: ^::dep::Named\n",
    )],
  )
  .with_dependencies(["dep"]);
  let dep = package(
    "dep",
    Tier::Alloc,
    &[("dep.graphqlx", "interface Named {\n  s: string\n}\n")],
  );
  vec![
    // `analysis_type_arguments.rs`, `an_ok_analysis_under_type_arguments_has_no_unresolved_value`.
    one_file(
      "an_ok_analysis_under_type_arguments_has_no_unresolved_value",
      Tier::Std,
      "type Query @d<Int>(a: 1) @d<String>(a: \"s\") {\n  x(i: In<Int> = { v: 2 }): Int\n  \
       g: G<Int>\n}\ndirective @d<T>(a: T, l: [T] = []) repeatable on OBJECT\n\
       input In<T> {\n  v: T\n  w: [T] = [4]\n  b: Box<T> = {}\n}\n\
       input Box<U> {\n  u: U = 5\n}\ninput Wrap<W> {\n  w: Box<W>\n}\n\
       input H {\n  h: Wrap<Int> = { w: {} }\n  m: Dict<Int> = map!{ 1 => {} }\n}\n\
       scalar Dict<K, V = Box<K>> @map\n\
       type G<T> @d<T>(a: 6) {\n  f(y: T = 7): Int\n}\n",
    ),
    // `analysis_type_arguments.rs`,
    // `an_applications_type_arguments_are_completed_from_its_directives_defaults`.
    one_file(
      "an_applications_type_arguments_are_completed_from_its_directives_defaults",
      Tier::Std,
      "directive @d<A, B = [A], C = Int> repeatable on OBJECT\n\
       directive @e<A = B, B = Int> on OBJECT\n\
       type Query @d<String> @d<String, Int> @d<String, Int, Float> @e {\n  x: Int\n}\n",
    ),
    // `analysis_composites.rs`, `a_set_literal_is_a_set_and_a_map_literal_a_map`.
    one_file(
      "a_set_literal_is_a_set_and_a_map_literal_a_map",
      Tier::Std,
      "type Query {\n  x: Int\n}\nscalar Bag<T> @set\n\
       input H {\n  tags: HashSet<string> = set!{\"a\", \"b\"}\n  \
       m: HashMap<string, Int> = map!{\"a\" => 1, \"b\" => null}\n  \
       bt: BTreeSet<Int> = set!{1}\n  bm: BTreeMap<Int, string> = map!{1 => \"x\"}\n  \
       bag: Bag<Int> = set!{2}\n  empty: HashSet<Int> = set!{}\n  \
       none: HashMap<Int, Int> = map!{}\n}\n",
    ),
    // `analysis_values.rs`, `a_lone_values_count_is_its_types_levels`, which asserts it ok under
    // eleven value steps.
    one_file(
      "a_lone_values_count_is_its_types_levels",
      Tier::Std,
      "type Query {\n  x: Int\n}\ndirective @d(m: [[[Int]]] = [1, 2]) on OBJECT\n\
       type T @d {\n  f: Int\n}\n",
    ),
    // `analysis_host.rs`, `a_scalars_externs_are_its_own_packages_applications`.
    (
      "a_scalars_externs_are_its_own_packages_applications",
      package(
        "app",
        Tier::Std,
        &[
          (
            "a.graphqlx",
            "type Query {\n  x: Int\n}\n\
             scalar MediaId @extern(lang: \"rust\", type: \"crate::MediaId\")\n",
          ),
          (
            "b.graphqlx",
            "extend scalar MediaId @extern(lang: \"typescript\", type: \"string\")\n",
          ),
        ],
      ),
      Vec::new(),
    ),
    // `analysis_implementation.rs`, `two_instantiations_of_one_interface_are_two_types`.
    one_file(
      "two_instantiations_of_one_interface_are_two_types",
      Tier::Std,
      "type Query {\n  x: Int\n}\ninterface Cmp<X> {\n  id: ID\n}\n\
       type T implements Cmp<Int> & Cmp<String> {\n  id: ID\n}\n",
    ),
    // `analysis_implementation.rs`,
    // `substituting_a_member_draws_on_the_implementation_budget_alone`, which asserts it ok under
    // no substitution step and under three implementation steps.
    one_file(
      "substituting_a_member_draws_on_the_implementation_budget_alone",
      Tier::Std,
      "type Query {\n  x: Int\n}\ninterface Node<T> {\n  id: T\n}\n\
       type User implements Node<ID> {\n  id: ID\n}\n",
    ),
    // `analysis_tiers.rs`, `every_definition_is_within_its_packages_tier_in_an_ok_analysis`: its
    // three programs, at `core`, `alloc` and `std`.
    one_file(
      "every_definition_is_within_its_packages_tier_in_an_ok_analysis, at core",
      Tier::Core,
      "type Query {\n  x: Int\n  n: NonZeroU32\n  e: E\n  m: M\n}\nenum E {\n  A\n}\n\
       scalar M @extern(lang: \"rust\", type: \"m\", tier: CORE)\n",
    ),
    one_file(
      "every_definition_is_within_its_packages_tier_in_an_ok_analysis, at alloc",
      Tier::Alloc,
      "type Query {\n  x: Int\n  s: String\n  l: [Int]\n}\n",
    ),
    one_file(
      "every_definition_is_within_its_packages_tier_in_an_ok_analysis, at std",
      Tier::Std,
      "type Query {\n  h: HashMap<i32, i32>\n}\n",
    ),
    // `analysis_tiers.rs`, `what_counts_nowhere_is_not_judged`: `[X8]`'s program, whose directive
    // metadata stands above its package's tier while every definition's tier is within it.
    ("what_counts_nowhere_is_not_judged", x8, vec![dep]),
  ]
}

/// The census over every corpus file, each analyzed as the one file of a package of its own: on
/// each checked analysis, nothing is unresolved, no owner holds two members of one kind and one
/// name, no named type has a count of arguments other than its definition's and no occurrence
/// writes more, every application's arguments are its directive's in order with the required ones
/// present and each coerced to its type, and no definition's tier is above its package's. Refuses
/// a phase that leaves something unresolved in an analysis it does not refuse, a duplicate no rule
/// refused, an arity a fill dropped, an application argument out of order or a required one
/// missing, a tier above its package's, and the opt-in rule panicking on any corpus file.
#[test]
fn w13_holds_on_every_ok_analysis_of_the_corpora() {
  let mut failures: Vec<String> = Vec::new();
  let mut read: Vec<PathBuf> = Vec::new();
  let (mut corpusx, mut corpusx_checked) = (0usize, 0usize);
  let (mut schemas, mut schemas_checked) = (0usize, 0usize);
  for (path, text) in corpus() {
    let analysis = analyzed(&path, &text);
    let name = path.display().to_string();
    let in_corpusx = path
      .parent()
      .and_then(Path::file_name)
      .is_some_and(|directory: &std::ffi::OsStr| directory == "corpusx");
    if in_corpusx {
      corpusx += 1;
    } else {
      schemas += 1;
    }
    if !checked(&analysis) {
      let reported = visit(&analysis, Packages::All, &mut UnboundedInstantiation::new());
      the_rules_own(&name, &reported);
      continue;
    }
    if in_corpusx {
      corpusx_checked += 1;
    } else {
      schemas_checked += 1;
    }
    failures.extend(census(&name, &analysis));
    read.push(path);
  }
  println!(
    "checked {corpusx_checked} of the {corpusx} files of `corpusx` and {schemas_checked} of the \
     {schemas} schema fixtures"
  );
  checked_the_known_ok(&read);
  assert!(
    read
      .iter()
      .any(|path: &PathBuf| path.file_name() == Some(std::ffi::OsStr::new(KNOWN_OK_X))),
    "`{KNOWN_OK_X}` is not read as ok"
  );
  assert!(
    failures.is_empty(),
    "{} breaches of W §13:\n{}",
    failures.len(),
    failures.join("\n")
  );
}

/// The census over programs copied from tests that assert them ok, each asserted ok here first:
/// what no corpus file writes, packages at `core` and `alloc` and a dependency among them, is held
/// too. Refuses what the corpus census refuses, there.
#[test]
fn w13_holds_on_programs_that_assert_ok() {
  let mut failures: Vec<String> = Vec::new();
  for (origin, root, dependencies) in programs() {
    let analysis = analyze(&root, &dependencies, &Limits::new());
    assert!(
      analysis.is_ok() && !analysis.diagnostics().truncated(),
      "{origin}: {:#?}",
      analysis.diagnostics()
    );
    failures.extend(census(origin, &analysis));
  }
  assert!(
    failures.is_empty(),
    "{} breaches of W §13:\n{}",
    failures.len(),
    failures.join("\n")
  );
}
