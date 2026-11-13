//! The walk the GraphQLx analyzer's rules ride, and the rules themselves, over real programs:
//! every file of the GraphQLx corpus and of the GraphQL schema fixtures, each analyzed as the one
//! file of a package of its own, and programs of several packages written here.
//!
//! The census: for each selection of packages, each hook is called once per entity the walk must
//! reach, and what it must reach is read off the model, never off the walk. The order: passing the
//! dependencies in another order changes nothing the rules report or mark. Tolerance: no rule
//! panics on a refused program, and none reports within what a phase already refused.
#![cfg(all(feature = "std", feature = "analysis"))]

use std::path::{Path, PathBuf};

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, Contribution, Cx, DefId, Diagnostic, Layout, Limits, MemberId, Model,
    ModuleId, Namespace, ObjectEntry, Package, PackageId, PackageKind, Packages, SourceFile,
    SourceId, Tier, TypeExprId, ValueId, ValueKind, Visitor, analyze,
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

#[test]
fn an_ok_corpus_file_has_no_unresolved_value() {
  for (path, text) in corpus() {
    let analysis = analyzed(&path, &text);
    if !analysis.is_ok() {
      continue;
    }
    let model = analysis.model();
    let (_, _, values, _) = test_support::counts(model);
    for index in 0..values {
      assert!(
        !matches!(
          model.value(ValueId::new(index)).kind(),
          ValueKind::Unresolved
        ),
        "{}: value {index} is unresolved",
        path.display()
      );
    }
  }
}

/// The value `id`, written out: a scalar as its kind and its text, `Int 1`, a list as
/// `List [Int 1]`, and an input object as `Object {key: String "value", answer= Int 42}`, each
/// entry its field's name and its value, a field a default filled marked `=` for its `:`.
fn shown(model: &Model, id: ValueId) -> String {
  match model.value(id).kind() {
    ValueKind::Null => String::from("Null null"),
    ValueKind::Bool(flag) => format!("Boolean {flag}"),
    ValueKind::Int(int) => format!("Int {}", int.text()),
    ValueKind::Float(float) => format!("Float {}", float.text()),
    ValueKind::String(text) => format!("String {text:?}"),
    ValueKind::List(items) => {
      let items: Vec<String> = items
        .iter()
        .map(|item: &ValueId| shown(model, *item))
        .collect();
      format!("List [{}]", items.join(", "))
    }
    ValueKind::Object(entries) => {
      let entries: Vec<String> = entries
        .iter()
        .map(|entry: &ObjectEntry| {
          let mark = if entry.name().is_some() { ":" } else { "=" };
          format!(
            "{}{mark} {}",
            model.member(entry.field()).name(),
            shown(model, entry.value())
          )
        })
        .collect();
      format!("Object {{{}}}", entries.join(", "))
    }
    other => format!("{other:?}"),
  }
}

/// Each object literal the corpus writes as a default is an input object: the fields it writes,
/// found by name, and the fields a default fills, each in its input object's order; a field with
/// no default, a OneOf input object's among them, left out and absent. Each value is found by its
/// owner: the argument of a field of a type of the file.
#[test]
fn the_corpus_input_objects_are_objects() {
  let files = corpus();
  let default = |name: &str, owner: &str, field: &str, argument: &str| -> String {
    let (path, text) = files
      .iter()
      .find(|(path, _): &&(PathBuf, Vec<u8>)| path.file_name() == Some(std::ffi::OsStr::new(name)))
      .unwrap_or_else(|| panic!("no corpus file `{name}`"));
    let analysis = analyzed(path, text);
    let model = analysis.model();
    let def = model
      .lookup(Namespace::Type, &format!("^::app::{owner}"))
      .unwrap_or_else(|| panic!("{name}: no type `{owner}`"));
    let field = model
      .def(def)
      .fields()
      .into_iter()
      .find(|id: &MemberId| model.member(*id).name() == field)
      .unwrap_or_else(|| panic!("{name}: no field `{owner}.{field}`"));
    let argument = model
      .member(field)
      .arguments()
      .into_iter()
      .find(|id: &MemberId| model.member(*id).name() == argument)
      .unwrap_or_else(|| panic!("{name}: no argument `{argument}`"));
    let value = model
      .member(argument)
      .default()
      .unwrap_or_else(|| panic!("{name}: no default"));
    shown(model, value)
  };
  assert_eq!(
    default("introspection_defaults.graphql", "Query", "range", "range"),
    "Object {min: Int 1, max: Int 9, step= Int 1}"
  );
  assert_eq!(
    default("introspection_agreement.graphql", "User", "posts", "filter"),
    "Object {published: Boolean false, ranked: List [List [Int 1, Null null], List []]}"
  );
  assert_eq!(
    default("schema_kitchen_sink.graphql", "Foo", "six", "argument"),
    "Object {key: String \"value\", answer= Int 42, other= Float 1.23e4}"
  );
  assert_eq!(
    default("countries.graphql", "Query", "continents", "filter"),
    "Object {}"
  );
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

/// Whether each of the schema's contributions has errors, in order.
fn schema_errors(analysis: &Analysis) -> Vec<bool> {
  analysis
    .model()
    .schema()
    .contributions()
    .map(|part: Contribution<'_>| part.has_errors())
    .collect::<Vec<bool>>()
}

/// How many definitions the built-in library alone gives a model: its own, and the primitives it
/// writes.
fn library_definitions() -> u32 {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  analyze(&root, &[], &Limits::new()).model().defs().len()
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
/// A member, a type occurrence and an application are each reached exactly when the walk covers
/// the package that wrote it, which is the package of the source its own location is in; a value,
/// when the walk covers the package that wrote its owner, the member whose default holds it or the
/// application whose argument holds it; a definition, when the walk covers one of its
/// contributions; and a module and a package, when the walk covers the package or one of the
/// definitions it holds is entered. The rows of the four tables the public model answers no range
/// of are numbered from 0 to the counts the test-support door answers, so over every package this
/// is each table whole.
///
/// A value is read by its owner because a copy of a default that fills an application's argument
/// is located where the default is written, which may be another package's source than the
/// application's: its own location does not say who wrote it. Each value's owner is read off the
/// model, and every value has exactly one.
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
    values: owners(model, values)
      .into_iter()
      .enumerate()
      .filter(|&(_, owner): &(usize, Location)| wrote(owner))
      .map(|(index, _): (usize, Location)| u32::try_from(index).expect("a value id"))
      .collect::<Vec<u32>>(),
  }
}

/// Where the owner of each of the `count` values of `model` is: the member whose default holds it,
/// or the application whose argument holds it. Each definition's applications, then each of its
/// direct members, right after each its arguments, a member's default and then its applications;
/// then the schema's applications; and under each the values it holds.
///
/// # Panics
///
/// If a value has two owners, or none.
fn owners(model: &Model, count: u32) -> Vec<Location> {
  let mut owner: Vec<Option<Location>> = vec![None; count as usize];
  for def in model.defs() {
    let definition = model.def(def);
    for &application in definition.applications() {
      own_arguments(model, &mut owner, application);
    }
    let direct = [
      definition.fields(),
      definition.input_fields(),
      definition.enum_values(),
      definition.arguments(),
    ];
    for member in direct.into_iter().flatten() {
      own_member(model, &mut owner, member);
      for argument in model.member(member).arguments() {
        own_member(model, &mut owner, argument);
      }
    }
  }
  for &application in model.schema().applications() {
    own_arguments(model, &mut owner, application);
  }
  owner
    .into_iter()
    .enumerate()
    .map(|(index, at): (usize, Option<Location>)| {
      at.unwrap_or_else(|| panic!("value {index} has no owner"))
    })
    .collect()
}

/// Gives `member`'s default, and its applications' arguments, their owners.
fn own_member(model: &Model, owner: &mut [Option<Location>], member: MemberId) {
  let held = model.member(member);
  if let Some(default) = held.default() {
    own(model, owner, default, held.location());
  }
  for &application in held.applications() {
    own_arguments(model, owner, application);
  }
}

/// Gives `application`'s arguments their owner, `application`.
fn own_arguments(model: &Model, owner: &mut [Option<Location>], application: ApplicationId) {
  let applied = model.application(application);
  for argument in applied.arguments() {
    own(model, owner, argument.value(), applied.location());
  }
}

/// Gives `root`, and every value it holds, the owner written at `at`.
fn own(model: &Model, owner: &mut [Option<Location>], root: ValueId, at: Location) {
  let mut stack: Vec<ValueId> = vec![root];
  while let Some(id) = stack.pop() {
    let slot = owner
      .get_mut(id.get() as usize)
      .unwrap_or_else(|| panic!("value {} is past the model's values", id.get()));
    assert!(slot.is_none(), "value {} has two owners", id.get());
    *slot = Some(at);
    match model.value(id).kind() {
      ValueKind::List(items) | ValueKind::Set(items) => stack.extend(items.iter().copied()),
      ValueKind::Map(entries) => {
        for &(key, item) in entries {
          stack.push(key);
          stack.push(item);
        }
      }
      ValueKind::Object(entries) => {
        stack.extend(entries.iter().map(|entry: &ObjectEntry| entry.value()));
      }
      _ => {}
    }
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
  let [root, astro, geo] = three_packages();
  analyze(&root, &[geo, astro], &Limits::new())
}

/// The packages of `three_packages_with_refusals`: the root package, `astro`, and `geo`.
fn three_packages() -> [Package<&'static str>; 3] {
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
  [root, astro, geo]
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
      "smear::graphqlx::unsatisfied-bound",
      "smear::schema::undefined-type",
      "smear::graphqlx::orphan-extension",
      "smear::graphqlx::orphan-extension",
      "smear::graphqlx::schema-outside-root",
    ]
  );
  census(&analysis, "three packages with refusals");
}

/// The order, W's test 7 for the rules: what they report, and what their errors mark, is the same
/// whichever order the dependencies are passed in.
#[test]
fn permuting_the_dependencies_changes_nothing_the_rules_report_or_mark() {
  let [root, astro, geo] = three_packages();
  let forward = analyze(&root, &[geo.clone(), astro.clone()], &Limits::new());
  let backward = analyze(&root, &[astro, geo], &Limits::new());
  // The case is not vacuous: the orphan rule reports a dependency's extension of the other's type
  // and of a built-in, and the schema rule the dependency's schema extension, which its error
  // marks.
  assert_eq!(
    codes(&forward),
    [
      "smear::graphqlx::unsatisfied-bound",
      "smear::schema::undefined-type",
      "smear::graphqlx::orphan-extension",
      "smear::graphqlx::orphan-extension",
      "smear::graphqlx::schema-outside-root",
    ]
  );
  assert_eq!(schema_errors(&forward), [true]);
  assert_eq!(forward.diagnostics(), backward.diagnostics());
  assert_eq!(schema_errors(&forward), schema_errors(&backward));
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
  // Every contribution's extent, each definition's and then the schema's, and then every member's,
  // read as a consumer reads them; the members' ids run from 0 to the count the test-support door
  // answers, for no public range holds every member.
  let model = analysis.model();
  let (members, _, _, _) = test_support::counts(model);
  let mut extents: Vec<Location> = Vec::new();
  for def in model.defs() {
    for part in model.def(def).contributions() {
      extents.push(part.extent());
    }
  }
  for part in model.schema().contributions() {
    extents.push(part.extent());
  }
  for index in 0..members {
    extents.push(model.member(MemberId::new(index)).extent());
  }
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

/// Whether a rule of smear's reported anything on `analysis`.
fn a_rule_fired(analysis: &Analysis) -> bool {
  let rules = rule_codes();
  analysis
    .diagnostics()
    .iter()
    .any(|diagnostic: &Diagnostic| rules.contains(&diagnostic.code()))
}

/// Tolerance, over every corpus file: no rule panics on one, and none repeats a phase.
///
/// No corpus file makes a rule fire: each is the one file of a `Flat` root package, with no
/// dependency beside it; none defines the schema twice, adds more than directives to a built-in or
/// applies `@marker`, `@set` or `@map` anywhere; none declares an enum with a type parameter, or
/// two type parameters of one name; none names a marker, `TypeRef` or `NonZero`, and every
/// interface a scalar, an enum, an input object or a union of the corpus implements names nothing.
/// So over the corpus this pins that no rule panics, and nothing more. It prints on how many files
/// a rule fired, which shows the first file that makes one fire.
#[test]
fn no_rule_repeats_a_phase_over_any_corpus_file() {
  let files = corpus();
  let total = files.len();
  let mut fired: usize = 0;
  for (path, text) in files {
    let analysis = analyzed(&path, &text);
    if a_rule_fired(&analysis) {
      fired += 1;
    }
    tolerant(&analysis, &path.display().to_string());
  }
  println!("a rule fired on {fired} of the {total} corpus files");
}

/// Tolerance, over refused programs on which a rule has something to say beside a phase, or would
/// have within what a phase refused.
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

  // The definition limit leaves room for `Query` and for `i32`, which `Int` writes, and for no
  // definition after them: the primitive a later schema item writes first is refused where it is
  // written, inside that item. A schema rule that reported the item all the same would report
  // within the extent that holds the refusal.
  let limits = Limits::new().with_max_definitions(library_definitions() + 2);

  // A second schema definition of the root package's that holds the refusal: the phase's
  // diagnostic alone.
  let root = package(
    "app",
    Layout::Flat,
    &[(
      "schema.graphqlx",
      "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\nschema {\n  query: u8\n}\n",
    )],
  );
  let analysis = analyze(&root, &[], &limits);
  assert_eq!(codes(&analysis), ["smear::graphqlx::resource-limit"]);
  tolerant(&analysis, "a second schema definition that holds a refusal");

  // A dependency's schema definition that holds the refusal: the phase's diagnostic alone.
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[("geo.graphqlx", "schema {\n  query: u8\n}\n")],
  );
  let analysis = analyze(&root, &[geo], &limits);
  assert_eq!(codes(&analysis), ["smear::graphqlx::resource-limit"]);
  tolerant(
    &analysis,
    "a dependency's schema definition that holds a refusal",
  );
}
