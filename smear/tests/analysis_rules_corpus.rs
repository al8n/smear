//! The walk the GraphQLx analyzer's rules ride, and the rules themselves, over real programs:
//! every file of the GraphQLx corpus and of the GraphQL schema fixtures, each analyzed as the one
//! file of a package of its own, and programs of several packages written here.
//!
//! The census: for each selection of packages, each hook is called once per entity the walk must
//! reach, and what it must reach is read off the model, never off the walk. Tolerance: no rule
//! panics on a refused program, and none reports within what a phase already refused.
#![cfg(all(feature = "std", feature = "analysis"))]

use std::path::{Path, PathBuf};

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, Contribution, Cx, DefId, Diagnostic, Layout, Limits, MemberId,
    ModuleId, Package, PackageId, PackageKind, Packages, SourceFile, SourceId, Tier, TypeExprId,
    ValueId, Visitor, analyze,
    test_support::{self, RULES},
    visit,
  },
  diagnostic::{Code, Diagnose, Location, Severity},
};

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

/// A package named `name` of `layout` at tier `Std`, holding `files`, `(path, text)`.
fn package(
  name: &str,
  layout: Layout,
  files: &[(&'static str, &'static str)],
) -> Package<&'static str> {
  Package::new(name, layout, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// The code of each diagnostic of `analysis`, in the list's order.
fn codes(analysis: &Analysis) -> Vec<&'static str> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect::<Vec<&'static str>>()
}

/// Every id each hook was handed, in the order it was handed them.
#[derive(Default)]
struct Census {
  begins: u32,
  schemas: u32,
  ends: u32,
  packages: Vec<u32>,
  left_packages: Vec<u32>,
  modules: Vec<u32>,
  left_modules: Vec<u32>,
  defs: Vec<u32>,
  left_defs: Vec<u32>,
  members: Vec<u32>,
  left_members: Vec<u32>,
  type_exprs: Vec<u32>,
  applications: Vec<u32>,
  values: Vec<u32>,
}

impl Visitor for Census {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.begins += 1;
  }

  fn enter_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.packages.push(package.get());
  }

  fn exit_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.left_packages.push(package.get());
  }

  fn enter_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    self.modules.push(module.get());
  }

  fn exit_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    self.left_modules.push(module.get());
  }

  fn enter_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    self.defs.push(def.get());
  }

  fn exit_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    self.left_defs.push(def.get());
  }

  fn enter_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    self.members.push(member.get());
  }

  fn exit_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    self.left_members.push(member.get());
  }

  fn type_expr(&mut self, _cx: &mut Cx<'_>, ty: TypeExprId) {
    self.type_exprs.push(ty.get());
  }

  fn application(&mut self, _cx: &mut Cx<'_>, application: ApplicationId) {
    self.applications.push(application.get());
  }

  fn value(&mut self, _cx: &mut Cx<'_>, value: ValueId) {
    self.values.push(value.get());
  }

  fn schema(&mut self, _cx: &mut Cx<'_>) {
    self.schemas += 1;
  }

  fn end(&mut self, _cx: &mut Cx<'_>) {
    self.ends += 1;
  }
}

/// `seen`, sorted, after asserting that no id of it was seen twice.
fn once(seen: &[u32], kind: &str, what: &str) -> Vec<u32> {
  let mut sorted: Vec<u32> = seen.to_vec();
  sorted.sort_unstable();
  sorted.dedup();
  assert_eq!(
    sorted.len(),
    seen.len(),
    "{what}: every {kind} is reached once"
  );
  sorted
}

/// Whether a walk over `packages` covers what a package of `kind` wrote.
fn covers(packages: Packages, kind: PackageKind) -> bool {
  match packages {
    Packages::Root => kind == PackageKind::Root,
    Packages::Supplied => matches!(kind, PackageKind::Root | PackageKind::Dependency),
    // `Packages::All`: every package's writing.
    _ => true,
  }
}

/// What a walk must reach: each kind's ids, in id order.
struct Reach {
  packages: Vec<u32>,
  modules: Vec<u32>,
  defs: Vec<u32>,
  members: Vec<u32>,
  type_exprs: Vec<u32>,
  applications: Vec<u32>,
  values: Vec<u32>,
}

/// What a walk of `analysis` over `packages` must reach, read off the model.
///
/// A member, a type occurrence, a value and an application are each reached exactly when the walk
/// covers the package that wrote it, which is the package of the source its own location is in;
/// a definition, when the walk covers one of its contributions; and a module and a package, when
/// the walk covers the package or one of the definitions it holds is entered. The rows of the four
/// tables the public model answers no range of are numbered from 0 to the counts the test-support
/// door answers, so over every package this is each table whole.
///
/// It holds because nothing the analyzer writes yet is located outside the item that wrote it. A
/// value a default fills is to be a copy located where the default is written, which may be
/// another package's source than that of the application the copy fills: once coercion makes such
/// copies, a value's own location stops saying who wrote it, and this must read the application
/// that holds the copy instead.
fn must_reach(analysis: &Analysis, packages: Packages) -> Reach {
  let model = analysis.model();
  let sources = analysis.sources();
  let wrote = |at: Location| -> bool {
    let package = sources.source(SourceId::new(at.source())).package();
    covers(packages, model.package(package).kind())
  };
  let covered = |package: PackageId| -> bool { covers(packages, model.package(package).kind()) };
  let (members, type_exprs, values, applications) = test_support::counts(model);

  let entered: Vec<bool> = model
    .defs()
    .into_iter()
    .map(|def: DefId| {
      model
        .def(def)
        .contributions()
        .any(|part: Contribution<'_>| wrote(part.location()))
    })
    .collect::<Vec<bool>>();
  let holds = |module: ModuleId| -> bool {
    model
      .module(module)
      .defs()
      .into_iter()
      .any(|def: DefId| entered[def.get() as usize])
  };

  Reach {
    packages: model
      .packages()
      .into_iter()
      .filter(|package: &PackageId| {
        covered(*package)
          || model
            .modules()
            .into_iter()
            .any(|module: ModuleId| model.module(module).package() == *package && holds(module))
      })
      .map(|package: PackageId| package.get())
      .collect::<Vec<u32>>(),
    modules: model
      .modules()
      .into_iter()
      .filter(|module: &ModuleId| covered(model.module(*module).package()) || holds(*module))
      .map(|module: ModuleId| module.get())
      .collect::<Vec<u32>>(),
    defs: (0..model.defs().len())
      .filter(|index: &u32| entered[*index as usize])
      .collect::<Vec<u32>>(),
    members: (0..members)
      .filter(|index: &u32| wrote(model.member(MemberId::new(*index)).location()))
      .collect::<Vec<u32>>(),
    type_exprs: (0..type_exprs)
      .filter(|index: &u32| wrote(model.type_expr(TypeExprId::new(*index)).location()))
      .collect::<Vec<u32>>(),
    applications: (0..applications)
      .filter(|index: &u32| wrote(model.application(ApplicationId::new(*index)).location()))
      .collect::<Vec<u32>>(),
    values: (0..values)
      .filter(|index: &u32| wrote(model.value(ValueId::new(*index)).location()))
      .collect::<Vec<u32>>(),
  }
}

/// The census of each walk of `analysis`, named `what` when it fails: over the root package, over
/// the supplied packages and over every package, each hook is called once per entity the model
/// says the walk must reach, each id once, and every entity entered is left.
fn census(analysis: &Analysis, what: &str) {
  for packages in [Packages::Root, Packages::Supplied, Packages::All] {
    let mut seen = Census::default();
    let reported = visit(analysis, packages, &mut seen);
    assert!(reported.is_empty(), "{what}: the census reports nothing");
    let expected = must_reach(analysis, packages);

    assert_eq!(
      (seen.begins, seen.schemas, seen.ends),
      (1, 1, 1),
      "{what}, {packages:?}: `begin`, `schema` and `end` are each called once"
    );
    for (entered, left, wanted, kind) in [
      (
        &seen.packages,
        &seen.left_packages,
        &expected.packages,
        "package",
      ),
      (
        &seen.modules,
        &seen.left_modules,
        &expected.modules,
        "module",
      ),
      (&seen.defs, &seen.left_defs, &expected.defs, "definition"),
      (
        &seen.members,
        &seen.left_members,
        &expected.members,
        "member",
      ),
    ] {
      assert_eq!(
        &once(entered, kind, what),
        wanted,
        "{what}, {packages:?}: the {kind}s entered"
      );
      assert_eq!(
        &once(left, kind, what),
        wanted,
        "{what}, {packages:?}: the {kind}s left"
      );
    }
    for (reached, wanted, kind) in [
      (&seen.type_exprs, &expected.type_exprs, "type occurrence"),
      (&seen.applications, &expected.applications, "application"),
      (&seen.values, &expected.values, "value"),
    ] {
      assert_eq!(
        &once(reached, kind, what),
        wanted,
        "{what}, {packages:?}: the {kind}s reached"
      );
    }

    // Over every package, what the walk must reach is every row of every table.
    if packages == Packages::All {
      let model = analysis.model();
      let (members, type_exprs, values, applications) = test_support::counts(model);
      for (wanted, count, kind) in [
        (&expected.packages, model.packages().len(), "package"),
        (&expected.modules, model.modules().len(), "module"),
        (&expected.defs, model.defs().len(), "definition"),
        (&expected.members, members, "member"),
        (&expected.type_exprs, type_exprs, "type occurrence"),
        (&expected.applications, applications, "application"),
        (&expected.values, values, "value"),
      ] {
        assert_eq!(
          wanted,
          &(0..count).collect::<Vec<u32>>(),
          "{what}: every {kind} of the model"
        );
      }
    }
  }
}

/// The census, over every corpus file.
#[test]
fn each_walk_of_every_corpus_file_reaches_what_the_model_says_once() {
  for (path, text) in corpus() {
    let analysis = analyzed(&path, &text);
    census(&analysis, &path.display().to_string());
  }
}

/// A root package in two modules that names a dependency's type, extends it, which no package but
/// its own may, and defines a schema with two roots and an application.
fn a_dependency_and_an_orphan_extension() -> Analysis {
  let root = package(
    "app",
    Layout::Directory,
    &[
      (
        "schema.graphqlx",
        "directive @tag(name: String = \"none\") repeatable on OBJECT | FIELD_DEFINITION | SCHEMA\n\
         interface Node {\n  id: ID\n}\n\
         type Query implements Node @tag(name: \"q\") {\n  id: ID\n  node(id: ID = \"1\"): Node\n  \
         nodes: [Node!]\n  origin: ^::geo::Point\n}\n\
         extend type ^::geo::Point @tag(name: \"orphan\") {\n  label: String @tag\n}\n\
         schema @tag(name: \"s\") {\n  query: Query\n  mutation: inner::Change\n}\n",
      ),
      (
        "inner/change.graphqlx",
        "type Change {\n  done: Boolean\n}\nextend type ~::Query {\n  changes: [Change]\n}\n",
      ),
    ],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[(
      "geo.graphqlx",
      "type Point {\n  x: Float\n  y: Float\n}\nunion Shape = Point\n",
    )],
  );
  analyze(&root, &[geo], &Limits::new())
}

/// A root package that replaces two GraphQL built-ins, a scalar and a directive, and adds a
/// directive to two more, one of them a primitive `core` materialises; and a dependency that
/// names the built-ins the root package replaces and extends.
fn replaced_and_extended_built_ins() -> Analysis {
  let root = package(
    "app",
    Layout::Flat,
    &[(
      "schema.graphqlx",
      "directive @x on SCALAR\n\
       scalar Int @specifiedBy(url: \"https://example.com/int\")\n\
       extend scalar String @x\nextend scalar Float @x\n\
       directive @deprecated(reason: String = \"gone\") on FIELD_DEFINITION\n\
       type Query {\n  old: Int @deprecated\n  size: Float\n  origin: ^::geo::Point\n}\n",
    )],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[(
      "geo.graphqlx",
      "type Point {\n  x: Float\n  count: Int\n  label: String @deprecated\n}\n",
    )],
  );
  analyze(&root, &[geo], &Limits::new())
}

/// A root package and two dependencies: generic definitions with a default, a bound and type
/// arguments, an enum, an input object with defaults, a union of the root's type and a
/// dependency's, and a type that is none; and a dependency that extends the other's type and a
/// built-in, and the schema.
fn three_packages_with_refusals() -> Analysis {
  let root = package(
    "app",
    Layout::Flat,
    &[(
      "schema.graphqlx",
      "interface Ord @marker\n\
       type Page<T = String> where T: Ord {\n  items: [T]\n  next: Page<T>\n}\n\
       directive @typed<V>(value: V) on OBJECT\n\
       type Query @typed<[Int!]>(value: 1) {\n  page: Page<Color>\n  star: ^::astro::Star\n  \
       missing: Nowhere<Int>\n}\n\
       enum Color {\n  RED\n  GREEN\n}\n\
       input Filter {\n  color: Color = RED\n  tags: [String] = [\"a\", \"b\"]\n}\n\
       union Either = Query | ^::geo::Point\n",
    )],
  )
  .with_dependencies(["astro", "geo"]);
  let astro = package(
    "astro",
    Layout::Flat,
    &[("astro.graphqlx", "type Star {\n  name: String\n}\n")],
  );
  let geo = package(
    "geo",
    Layout::Flat,
    &[(
      "geo.graphqlx",
      "type Point {\n  x: Float\n}\nextend type ^::astro::Star {\n  near: Point\n}\n\
       extend scalar String @deprecated\nextend schema {\n  mutation: Point\n}\n",
    )],
  )
  .with_dependencies(["astro"]);
  analyze(&root, &[geo, astro], &Limits::new())
}

/// The census, over programs of several packages: what a selection leaves out is then more than
/// the built-in library.
#[test]
fn each_walk_of_a_program_of_several_packages_reaches_what_the_model_says_once() {
  // Each program is what its name says: the rules' diagnostics, and no syntax error.
  let analysis = a_dependency_and_an_orphan_extension();
  assert_eq!(codes(&analysis), ["smear::graphqlx::orphan-extension"]);
  census(&analysis, "a dependency and an orphan extension");

  let analysis = replaced_and_extended_built_ins();
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  census(&analysis, "replaced and extended built-ins");

  let analysis = three_packages_with_refusals();
  assert_eq!(
    codes(&analysis),
    [
      "smear::schema::undefined-type",
      "smear::graphqlx::orphan-extension",
      "smear::graphqlx::orphan-extension",
      "smear::graphqlx::schema-outside-root",
    ]
  );
  census(&analysis, "three packages with refusals");
}

/// Every code a rule of smear's lists.
fn rule_codes() -> Vec<Code> {
  let mut codes: Vec<Code> = Vec::new();
  for (_, listed) in RULES {
    codes.extend_from_slice(listed);
  }
  codes
}

/// Whether `at` lies within `extent`: in its source, and between its first byte and its last. A
/// location that covers a whole source lies within no extent, and an extent that covers a whole
/// source holds nothing, which is how an error marks what holds it.
fn holds(extent: Location, at: Location) -> bool {
  match (extent.span(), at.span()) {
    (Some(outer), Some(inner)) => {
      extent.source() == at.source() && outer.start() <= inner.start() && inner.end() <= outer.end()
    }
    _ => false,
  }
}

/// Tolerance, for `analysis`, named `what` when it fails: no diagnostic whose code a rule lists
/// has its primary location within an extent, a contribution's or a member's, that also holds the
/// primary location of an error a phase reported. Such a diagnostic would repeat the phase's.
fn tolerant(analysis: &Analysis, what: &str) {
  let rules = rule_codes();
  let diagnostics = analysis.diagnostics();
  let reported: Vec<&Diagnostic> = diagnostics
    .iter()
    .filter(|diagnostic: &&Diagnostic| rules.contains(&diagnostic.code()))
    .collect::<Vec<&Diagnostic>>();
  if reported.is_empty() {
    return;
  }
  let refused: Vec<&Diagnostic> = diagnostics
    .iter()
    .filter(|diagnostic: &&Diagnostic| {
      diagnostic.severity() == Severity::Error && !rules.contains(&diagnostic.code())
    })
    .collect::<Vec<&Diagnostic>>();
  let extents = test_support::extents(analysis.model());
  for rule in reported {
    for extent in &extents {
      if !holds(*extent, rule.primary()) {
        continue;
      }
      for phase in &refused {
        assert!(
          !holds(*extent, phase.primary()),
          "{what}: `{}` at {:?} repeats `{}` at {:?}, which {extent:?} holds too",
          rule.code(),
          rule.primary(),
          phase.code(),
          phase.primary()
        );
      }
    }
  }
}

/// Tolerance, over every corpus file: no rule panics on one, and none repeats a phase.
#[test]
fn no_rule_repeats_a_phase_over_any_corpus_file() {
  for (path, text) in corpus() {
    let analysis = analyzed(&path, &text);
    tolerant(&analysis, &path.display().to_string());
  }
}

/// Tolerance, over refused programs whose rules have something to say beside a phase.
#[test]
fn no_rule_repeats_a_phase_over_refused_programs() {
  // An extension of a dependency's type that holds a type that is none: the phase's diagnostic
  // alone, since the extension that holds it is not also an orphan.
  let root = package(
    "app",
    Layout::Flat,
    &[(
      "schema.graphqlx",
      "type Query {\n  ok: Int\n}\nextend type ^::geo::Point {\n  extra: Missing\n}\n",
    )],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[("geo.graphqlx", "type Point {\n  x: Int\n}\n")],
  );
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert_eq!(codes(&analysis), ["smear::schema::undefined-type"]);
  tolerant(
    &analysis,
    "an orphan extension that holds an unresolved type",
  );

  // A second schema definition, beside a first whose root names nothing: the phase's diagnostic
  // lies within the first, and the rule's at the second.
  let root = package(
    "app",
    Layout::Flat,
    &[(
      "schema.graphqlx",
      "schema {\n  query: Missing\n}\nschema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n",
    )],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert_eq!(
    codes(&analysis),
    [
      "smear::schema::undefined-root-operation-type",
      "smear::schema::duplicate-schema-definition",
    ]
  );
  tolerant(&analysis, "a second schema definition");

  // A dependency's schema definition and extension, beside a root package's schema definition
  // whose root names nothing.
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "schema {\n  query: Missing\n}\n")],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[(
      "geo.graphqlx",
      "schema {\n  query: Point\n}\nextend schema {\n  mutation: Point\n}\n\
       type Point {\n  x: Int\n}\n",
    )],
  );
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert_eq!(
    codes(&analysis),
    [
      "smear::schema::undefined-root-operation-type",
      "smear::graphqlx::schema-outside-root",
      "smear::graphqlx::schema-outside-root",
    ]
  );
  tolerant(&analysis, "a dependency's schema items");
}
