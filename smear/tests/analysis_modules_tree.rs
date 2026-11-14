//! The GraphQLx analyzer's module tree: a module per directory, the `Flat` layout, inline `mod`s,
//! every way a module is declared twice, directory names, the depth limit, and the `ModuleId`
//! order.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Diagnostic, DiagnosticKind, Layout, Limits, Model, Module, ModuleId,
    ModuleKind, Package, PackageId, SourceFile, SourceId, Sym, Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for a schema with no query root. The programs of this file are parts of a
/// program and say nothing of the schema, so each reading below leaves this code out, as a
/// consumer that analyzes a library waives it.
const NO_QUERY_ROOT: &str = "smear::schema::missing-query-root-operation-type";

/// The diagnostics of `analysis`, in the list's order, but for the missing query root.
fn reported(analysis: &Analysis) -> Vec<&Diagnostic> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() != NO_QUERY_ROOT)
    .collect()
}

/// Whether `analysis` reports nothing but that its program has no query root.
fn clean(analysis: &Analysis) -> bool {
  let diagnostics = analysis.diagnostics();
  let waived = diagnostics.len() - reported(analysis).len();
  diagnostics.len() == waived && diagnostics.error_count() == waived
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

/// The id of the file at `path` in the package `package`.
fn file(analysis: &Analysis, package: u32, path: &str) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == path)
    .unwrap_or_else(|| panic!("no file `{path}` in package {package}"))
}

/// The whole of the package `package`'s package source.
fn package_source(analysis: &Analysis, package: u32) -> Location {
  let id = analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .next()
    .expect("every package has a package source");
  Location::entire(id.get())
}

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  let start = text
    .match_indices(needle)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{needle}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

/// Where the `nth` `mod name` of `text`, the file `source`, names its module.
fn mod_name(source: SourceId, text: &str, name: &str, nth: usize) -> Location {
  let written = at(source, text, &format!("mod {name} "), nth);
  let start = written.span().expect("a span").start() + "mod ".len();
  Location::new(source.get(), SimpleSpan::new(start, start + name.len()))
}

/// Each module after the built-in packages' three root modules: its package, its path and its
/// kind, in id order.
fn tree(model: &Model) -> Vec<(u32, Vec<&str>, ModuleKind)> {
  model
    .modules()
    .into_iter()
    .skip(3)
    .map(|id: ModuleId| {
      let module: Module<'_> = model.module(id);
      let path: Vec<&str> = module
        .path()
        .iter()
        .map(|segment: &Sym| model.name(*segment))
        .collect();
      (module.package().get(), path, module.kind())
    })
    .collect()
}

/// The module `path` names, spelled as `Model::module_by_path` takes it.
fn module(model: &Model, path: &str) -> ModuleId {
  model
    .module_by_path(path)
    .unwrap_or_else(|| panic!("no module `{path}`"))
}

/// Each diagnostic's code and primary location, in the list's order, but for the missing query
/// root.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  reported(analysis)
    .into_iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// The code of `kind`.
fn code(kind: DiagnosticKind) -> &'static str {
  kind.code().as_str()
}

/// The spellings `message` quotes, each between a pair of backticks, in order.
fn quoted(message: &str) -> Vec<&str> {
  message.split('`').skip(1).step_by(2).collect()
}

#[test]
fn a_directory_package_has_a_module_per_directory_and_its_files_share_it() {
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("schema.graphqlx", "scalar Root\n"),
      ("media/a.graphqlx", "scalar A\n"),
      ("media/b.graphqlx", "scalar B\n"),
      ("media/vision/c.graphqlx", "scalar C\n"),
      // `deep` holds no file of its own: it is an empty module on the way to `deep::er`.
      ("deep/er/d.graphqlx", "scalar D\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());

  let model = analysis.model();
  assert_eq!(
    tree(model),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["deep"], ModuleKind::Directory),
      (3, vec!["deep", "er"], ModuleKind::Directory),
      (3, vec!["media"], ModuleKind::Directory),
      (3, vec!["media", "vision"], ModuleKind::Directory),
    ]
  );

  let app = model.package(model.root_package()).root_module();
  let deep = module(model, "^::app::deep");
  let er = module(model, "^::app::deep::er");
  let media = module(model, "^::app::media");
  let vision = module(model, "^::app::media::vision");
  assert_eq!(module(model, "^::app"), app);
  assert_eq!(model.module(app).children(), &[deep, media]);
  assert_eq!(model.module(deep).parent(), Some(app));
  assert_eq!(model.module(deep).children(), &[er]);
  assert_eq!(model.module(er).parent(), Some(deep));
  assert_eq!(model.module(media).children(), &[vision]);
  assert_eq!(model.module(vision).parent(), Some(media));
  assert!(model.module(vision).children().is_empty());

  // Only a `mod` gives a module a location, an extent and a description.
  for id in [app, deep, er, media, vision] {
    let module: Module<'_> = model.module(id);
    assert_eq!(module.location(), None);
    assert_eq!(module.description(), None);
    assert_eq!(module.extent(), None);
  }
}

#[test]
fn a_flat_package_has_every_file_in_its_root_module() {
  let root = package(
    "app",
    Layout::Flat,
    &[
      ("schema.graphqlx", "scalar Root\n"),
      ("media/a.graphqlx", "scalar A\n"),
      ("media/vision/c.graphqlx", "scalar C\n"),
      // Not a Name, and no matter: a flat package's paths name no module.
      ("not-a-name/d.graphqlx", "scalar D\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert_eq!(tree(analysis.model()), [(3, vec![], ModuleKind::Root)]);
}

/// A described `mod` holding another, in one file.
const NESTED: &str = "\"The outer module.\"\nmod outer {\n  mod inner {\n    scalar X\n  }\n}\n";
/// A second file's `mod`.
const OTHER: &str = "mod other {\n  scalar Y\n}\n";

#[test]
fn an_inline_mod_declares_a_child_of_the_module_it_is_written_in() {
  for layout in [Layout::Directory, Layout::Flat] {
    let root = package(
      "app",
      layout,
      &[("a.graphqlx", NESTED), ("b.graphqlx", OTHER)],
    );
    let analysis = analyze(&root, &[], &Limits::new());
    assert!(clean(&analysis), "{:#?}", analysis.diagnostics());

    let model = analysis.model();
    assert_eq!(
      tree(model),
      [
        (3, vec![], ModuleKind::Root),
        (3, vec!["other"], ModuleKind::Inline),
        (3, vec!["outer"], ModuleKind::Inline),
        (3, vec!["outer", "inner"], ModuleKind::Inline),
      ]
    );

    let a = file(&analysis, 3, "a.graphqlx");
    let b = file(&analysis, 3, "b.graphqlx");
    let outer = module(model, "^::app::outer");
    let inner = module(model, "^::app::outer::inner");
    let other = module(model, "^::app::other");
    assert_eq!(model.module(inner).parent(), Some(outer));
    assert_eq!(model.module(outer).children(), &[inner]);

    // A module's location is where its `mod` names it, and its extent runs from its description,
    // or its `mod`, to its `}`.
    let outer_end = NESTED.rfind('}').expect("a body") + 1;
    let inner_start = NESTED.find("mod inner").expect("a `mod`");
    let inner_end = NESTED.find('}').expect("a body") + 1;
    assert_eq!(
      model.module(outer).location(),
      Some(mod_name(a, NESTED, "outer", 0))
    );
    assert_eq!(
      model.module(outer).extent(),
      Some(Location::new(a.get(), SimpleSpan::new(0, outer_end)))
    );
    assert_eq!(
      model.module(inner).location(),
      Some(mod_name(a, NESTED, "inner", 0))
    );
    assert_eq!(
      model.module(inner).extent(),
      Some(Location::new(
        a.get(),
        SimpleSpan::new(inner_start, inner_end)
      ))
    );
    assert_eq!(
      model.module(other).location(),
      Some(mod_name(b, OTHER, "other", 0))
    );

    let description = model
      .module(outer)
      .description()
      .expect("the outer `mod` is described");
    assert_eq!(description.text(), "The outer module.");
    assert_eq!(
      description.location(),
      at(a, NESTED, "\"The outer module.\"", 0)
    );
    assert_eq!(model.module(inner).description(), None);
  }
}

/// Asserts that `diagnostic` is a `module-defined-twice` at `at`, whose label points at `first`
/// and says `label`.
fn defined_twice(diagnostic: &Diagnostic, at: Location, first: Location, label: &str) {
  assert_eq!(
    diagnostic.code().as_str(),
    code(DiagnosticKind::ModuleDefinedTwice)
  );
  assert_eq!(diagnostic.primary(), at);
  assert_eq!(diagnostic.labels(), 1);
  let written = diagnostic
    .label(0)
    .expect("a label at the first declaration");
  assert_eq!(written.location(), first);
  assert_eq!(written.text(), label);
  assert!(diagnostic.help().is_some());
}

#[test]
fn two_mods_of_one_path_in_one_file_are_one_module_declared_twice() {
  let text = "mod twice {\n  scalar A\n}\nmod twice {\n  scalar B\n}\n";
  let root = package("app", Layout::Directory, &[("a.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, 3, "a.graphqlx");
  let diagnostics = reported(&analysis);
  assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
  defined_twice(
    diagnostics[0],
    mod_name(a, text, "twice", 1),
    mod_name(a, text, "twice", 0),
    "first declared here",
  );

  // One module, located at its first declaration.
  let model = analysis.model();
  assert_eq!(
    tree(model),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["twice"], ModuleKind::Inline)
    ]
  );
  assert_eq!(
    model.module(module(model, "^::app::twice")).location(),
    Some(mod_name(a, text, "twice", 0))
  );
}

#[test]
fn a_mod_in_two_files_of_one_directory_is_one_module_declared_twice() {
  let first = "mod shared {\n  scalar A\n}\n";
  let second = "mod shared {\n  scalar B\n}\n";
  // Passed in reverse: the first declaration is the first in source order, which is by path.
  let root = package(
    "app",
    Layout::Directory,
    &[("x/b.graphqlx", second), ("x/a.graphqlx", first)],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, 3, "x/a.graphqlx");
  let b = file(&analysis, 3, "x/b.graphqlx");
  let diagnostics = reported(&analysis);
  assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
  defined_twice(
    diagnostics[0],
    mod_name(b, second, "shared", 0),
    mod_name(a, first, "shared", 0),
    "first declared here",
  );
  assert_eq!(
    tree(analysis.model()),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["x"], ModuleKind::Directory),
      (3, vec!["x", "shared"], ModuleKind::Inline),
    ]
  );
}

#[test]
fn a_mod_of_a_directory_module_declares_that_module_twice() {
  let schema = "mod media {\n  scalar M\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("schema.graphqlx", schema),
      ("media/photo.graphqlx", "scalar Photo\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let written = file(&analysis, 3, "schema.graphqlx");
  let diagnostics = reported(&analysis);
  assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
  // The directory has no position of its own: the label is at the package source, and names it.
  defined_twice(
    diagnostics[0],
    mod_name(written, schema, "media", 0),
    package_source(&analysis, 3),
    "the directory `media` is this module",
  );

  // The directory declares the module, which keeps no `mod`'s location.
  let model = analysis.model();
  assert_eq!(
    tree(model),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["media"], ModuleKind::Directory),
    ]
  );
  assert_eq!(
    model.module(module(model, "^::app::media")).location(),
    None
  );
}

#[test]
fn every_later_declaration_of_a_nested_mod_is_reported() {
  let text = "mod a {\n  mod b {}\n}\nmod a {\n  mod b {}\n}\n";
  let root = package("app", Layout::Directory, &[("a.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, 3, "a.graphqlx");
  let diagnostics = reported(&analysis);
  assert_eq!(diagnostics.len(), 2, "{diagnostics:#?}");
  defined_twice(
    diagnostics[0],
    mod_name(a, text, "a", 1),
    mod_name(a, text, "a", 0),
    "first declared here",
  );
  defined_twice(
    diagnostics[1],
    mod_name(a, text, "b", 1),
    mod_name(a, text, "b", 0),
    "first declared here",
  );
  assert_eq!(
    tree(analysis.model()),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["a"], ModuleKind::Inline),
      (3, vec!["a", "b"], ModuleKind::Inline),
    ]
  );
}

#[test]
fn a_directory_whose_name_is_not_a_name_is_reported_once_and_places_no_file() {
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("my-dir/a.graphqlx", "scalar A\n"),
      ("my-dir/b.graphqlx", "type B {\n"),
      ("my-dir/sub/c.graphqlx", "scalar C\n"),
      ("ok/x-y/d.graphqlx", "scalar D\n"),
      ("fine/e.graphqlx", "scalar E\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let whole = package_source(&analysis, 3);
  let broken = file(&analysis, 3, "my-dir/b.graphqlx");

  // The files under the directory are still parsed: the syntax error is reported.
  let reported = found(&analysis);
  assert_eq!(reported.len(), 3, "{:#?}", analysis.diagnostics());
  assert!(
    reported
      .iter()
      .any(|(code, location): &(&'static str, Location)| {
        code.starts_with("smear::syntax::") && location.source() == broken.get()
      }),
    "{reported:?}"
  );

  // Each directory once, at the package source, quoting the package, the directory and its name.
  let mut messages: Vec<String> = Vec::new();
  for diagnostic in analysis.diagnostics() {
    if diagnostic.code().as_str() == code(DiagnosticKind::InvalidDirectoryName) {
      assert_eq!(diagnostic.primary(), whole);
      assert!(diagnostic.help().is_some());
      messages.push(diagnostic.to_string());
    }
  }
  messages.sort();
  assert_eq!(messages.len(), 2, "{messages:?}");
  assert_eq!(quoted(&messages[0]), ["app", "my-dir", "my-dir"]);
  assert_eq!(quoted(&messages[1]), ["app", "ok/x-y", "x-y"]);

  // `ok` is a module on the way to `ok/x-y`, which is none.
  assert_eq!(
    tree(analysis.model()),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["fine"], ModuleKind::Directory),
      (3, vec!["ok"], ModuleKind::Directory),
    ]
  );
}

#[test]
fn a_module_deeper_than_the_limit_is_a_resource_limit() {
  let text = "mod m {\n  mod n {\n    scalar N\n  }\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[("a/x.graphqlx", text), ("a/b/y.graphqlx", "scalar Y\n")],
  );
  let limits = Limits::new().with_max_module_depth(1);
  let analysis = analyze(&root, &[], &limits);
  let x = file(&analysis, 3, "a/x.graphqlx");
  let resource = code(DiagnosticKind::ResourceLimit);
  // The directory `a/b` at the package source, and the `mod` at its name, which is inside `a` and
  // so would sit two deep: nothing inside it is walked, so `n` is not reported again.
  assert_eq!(
    found(&analysis),
    [
      (resource, package_source(&analysis, 3)),
      (resource, mod_name(x, text, "m", 0)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    tree(analysis.model()),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["a"], ModuleKind::Directory),
    ]
  );

  // At the depth the program needs, nothing is refused.
  let analysis = analyze(&root, &[], &Limits::new().with_max_module_depth(3));
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());

  // With no depth, only the root module is.
  let analysis = analyze(&root, &[], &Limits::new().with_max_module_depth(0));
  assert_eq!(found(&analysis).len(), 1, "{:#?}", analysis.diagnostics());
  assert_eq!(tree(analysis.model()), [(3, vec![], ModuleKind::Root)]);
}

/// Every module of `analysis` with what it answers, and every diagnostic: what the module ids
/// number, and what a permutation of the input must not change.
#[allow(clippy::type_complexity)]
fn numbered(
  analysis: &Analysis,
) -> (
  Vec<(
    u32,
    Vec<&str>,
    ModuleKind,
    Option<ModuleId>,
    Option<Location>,
  )>,
  Vec<Diagnostic>,
) {
  let model = analysis.model();
  let modules = model
    .modules()
    .into_iter()
    .map(|id: ModuleId| {
      let module: Module<'_> = model.module(id);
      let path: Vec<&str> = module
        .path()
        .iter()
        .map(|segment: &Sym| model.name(*segment))
        .collect();
      (
        module.package().get(),
        path,
        module.kind(),
        module.parent(),
        module.location(),
      )
    })
    .collect();
  (modules, analysis.diagnostics().iter().cloned().collect())
}

/// W's module order: package, then path, compared segment by segment, each segment bytewise, so a
/// package's root module comes first and a module before its descendants; and no permutation of
/// the files or the dependencies changes an id or a diagnostic.
#[test]
fn module_ids_follow_the_package_then_the_path_segment_by_segment() {
  let app = [
    ("b/1.graphqlx", "scalar B\n"),
    ("a/x/2.graphqlx", "scalar X\n"),
    ("a/3.graphqlx", "mod y {}\nmod y {}\n"),
    ("a0/4.graphqlx", "scalar A0\n"),
    ("Z/5.graphqlx", "scalar Z\n"),
    ("not-a-name/6.graphqlx", "scalar N\n"),
  ];
  let geo = [("m/7.graphqlx", "scalar M\n")];
  let alpha = [("n/8.graphqlx", "scalar N\n")];
  let build = |reversed: bool| -> (Package<&'static str>, Vec<Package<&'static str>>) {
    let order = |files: &[(&'static str, &'static str)]| -> Vec<(&'static str, &'static str)> {
      let mut files = files.to_vec();
      if reversed {
        files.reverse();
      }
      files
    };
    let root =
      package("app", Layout::Directory, &order(&app[..])).with_dependencies(["geo", "alpha"]);
    let mut dependencies = vec![
      package("geo", Layout::Directory, &order(&geo[..])),
      package("alpha", Layout::Directory, &order(&alpha[..])),
    ];
    if reversed {
      dependencies.reverse();
    }
    (root, dependencies)
  };

  let (root, dependencies) = build(false);
  let forward = analyze(&root, &dependencies, &Limits::new());
  let (root, dependencies) = build(true);
  let backward = analyze(&root, &dependencies, &Limits::new());

  // `Z` sorts before `a`, and `a` with every module inside it before `a0`, which a comparison of
  // the joined paths would put first; the root's modules before `alpha`'s, and `alpha`'s before
  // `geo`'s.
  assert_eq!(
    tree(forward.model()),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["Z"], ModuleKind::Directory),
      (3, vec!["a"], ModuleKind::Directory),
      (3, vec!["a", "x"], ModuleKind::Directory),
      (3, vec!["a", "y"], ModuleKind::Inline),
      (3, vec!["a0"], ModuleKind::Directory),
      (3, vec!["b"], ModuleKind::Directory),
      (4, vec![], ModuleKind::Root),
      (4, vec!["n"], ModuleKind::Directory),
      (5, vec![], ModuleKind::Root),
      (5, vec!["m"], ModuleKind::Directory),
    ]
  );
  let codes: Vec<&str> = found(&forward)
    .into_iter()
    .map(|(code, _): (&'static str, Location)| code)
    .collect();
  assert_eq!(
    codes,
    [
      code(DiagnosticKind::InvalidDirectoryName),
      code(DiagnosticKind::ModuleDefinedTwice),
    ]
  );
  assert_eq!(numbered(&forward), numbered(&backward));
}

/// A module's descendants are numbered before its next sibling, whatever order the input gives
/// them in: `a`, `a::b`, then `ab`, where a numbering by depth, or in the order the walk finds
/// them, would put `ab` before `a::b`. In both layouts: from directories, the files given in
/// reverse, and from `mod`s, written in reverse.
#[test]
fn a_modules_descendants_come_before_its_next_sibling() {
  let directories = package(
    "app",
    Layout::Directory,
    &[
      ("ab/1.graphqlx", "scalar AB\n"),
      ("a/b/2.graphqlx", "scalar B\n"),
      ("a/3.graphqlx", "scalar A\n"),
    ],
  );
  let mods = package(
    "app",
    Layout::Flat,
    &[("lib.graphqlx", "mod ab {}\nmod a {\n  mod b {}\n}\n")],
  );
  for (root, kind) in [
    (directories, ModuleKind::Directory),
    (mods, ModuleKind::Inline),
  ] {
    let analysis = analyze(&root, &[], &Limits::new());
    assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
    assert_eq!(
      tree(analysis.model()),
      [
        (3, vec![], ModuleKind::Root),
        (3, vec!["a"], kind),
        (3, vec!["a", "b"], kind),
        (3, vec!["ab"], kind),
      ]
    );
  }
}

/// The modules every analysis of a root package with no dependency holds before the package adds
/// one: the three built-in packages' root modules and the root package's.
const ROOTS: u32 = 4;

/// The names of the definitions `module` holds, in id order.
fn defined(model: &Model, module: ModuleId) -> Vec<&str> {
  model
    .module(module)
    .defs()
    .into_iter()
    .map(|id: DefId| model.def(id).name())
    .collect()
}

/// The first module a supplied package's `mod` would add past the module limit is reported where
/// the `mod` names it, with the limit; nothing inside it is collected, and no module is added after
/// it, unreported; what sits in a module already held is still collected.
#[test]
fn a_mod_past_the_module_limit_is_refused_and_no_module_after_it() {
  let text = "mod a {\n  scalar A\n}\nmod b {\n  mod c {}\n  scalar B\n}\nmod d {}\nscalar C\n";
  let root = package("app", Layout::Flat, &[("lib.graphqlx", text)]);
  let max = ROOTS + 1;
  let analysis = analyze(&root, &[], &Limits::new().with_max_modules(max));
  let lib = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::ResourceLimit),
      mod_name(lib, text, "b", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let diagnostic = reported(&analysis)[0];
  let message = diagnostic.to_string();
  assert_eq!(quoted(&message), ["mod b"], "{message}");
  assert!(
    message.contains(&format!("limit of {max} modules")),
    "{message}"
  );
  assert!(
    diagnostic
      .help()
      .is_some_and(|help: &str| help.contains("`Limits::max_modules`")),
    "{:?}",
    diagnostic.help()
  );

  let model = analysis.model();
  assert_eq!(model.modules().len(), max);
  assert_eq!(
    tree(model),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["a"], ModuleKind::Inline),
    ]
  );
  assert_eq!(defined(model, module(model, "^::app::a")), ["A"]);
  assert_eq!(defined(model, module(model, "^::app")), ["C"]);
}

/// The first module a supplied package's directory would add past the module limit is reported at
/// the package source, quoting the directory, with the limit: the files under it are parsed but
/// placed in no module, and no module is added after it, by a directory or by a `mod`.
#[test]
fn a_directory_past_the_module_limit_is_refused_and_no_module_after_it() {
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("a/1.graphqlx", "scalar A\nmod m {}\n"),
      ("b/2.graphqlx", "scalar B\n"),
      ("c/3.graphqlx", "scalar C\n"),
      ("4.graphqlx", "scalar D\n"),
    ],
  );
  let max = ROOTS + 1;
  let analysis = analyze(&root, &[], &Limits::new().with_max_modules(max));
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::ResourceLimit),
      package_source(&analysis, 3)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let message = reported(&analysis)[0].to_string();
  assert_eq!(quoted(&message), ["app", "b"], "{message}");
  assert!(
    message.contains(&format!("limit of {max} modules")),
    "{message}"
  );

  let model = analysis.model();
  assert_eq!(model.modules().len(), max);
  assert_eq!(
    tree(model),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["a"], ModuleKind::Directory),
    ]
  );
  assert_eq!(defined(model, module(model, "^::app::a")), ["A"]);
  assert_eq!(defined(model, module(model, "^::app")), ["D"]);
}

/// A program whose modules fill the module limit exactly is no error; one fewer refuses the last
/// module the walk adds.
#[test]
fn the_module_limit_met_exactly_is_no_error() {
  let text = "mod a {}\nmod b {\n  mod c {}\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[("lib.graphqlx", text), ("d/1.graphqlx", "scalar D\n")],
  );
  // The four root modules, then `d`, `a`, `b` and `b::c`.
  let analysis = analyze(&root, &[], &Limits::new().with_max_modules(ROOTS + 4));
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert_eq!(analysis.model().modules().len(), ROOTS + 4);

  let analysis = analyze(&root, &[], &Limits::new().with_max_modules(ROOTS + 3));
  let lib = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::ResourceLimit),
      mod_name(lib, text, "c", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(analysis.model().modules().len(), ROOTS + 3);
}

/// Every package's root module counts toward the module limit and is always held, the built-in
/// packages' and a dependency's included, although the dependency is numbered after the root
/// package: here they leave no room for the root package's `mod`, and none for the dependency's
/// directory; and a limit below their count holds them all the same.
#[test]
fn every_root_module_counts_toward_the_module_limit_and_is_always_held() {
  let text = "mod a {}\n";
  let root = package("app", Layout::Flat, &[("lib.graphqlx", text)]).with_dependencies(["geo"]);
  let dependencies = [package(
    "geo",
    Layout::Directory,
    &[("m/1.graphqlx", "scalar M\n")],
  )];
  for max in [ROOTS + 1, 0] {
    let analysis = analyze(&root, &dependencies, &Limits::new().with_max_modules(max));
    let lib = file(&analysis, 3, "lib.graphqlx");
    assert_eq!(
      found(&analysis),
      [(
        code(DiagnosticKind::ResourceLimit),
        mod_name(lib, text, "a", 0)
      )],
      "{max}: {:#?}",
      analysis.diagnostics()
    );
    let model = analysis.model();
    assert_eq!(model.modules().len(), ROOTS + 1, "{max}");
    assert_eq!(
      tree(model),
      [(3, vec![], ModuleKind::Root), (4, vec![], ModuleKind::Root)],
      "{max}"
    );
    // The dependency's file sat in the refused directory `m`, so it declares nothing.
    assert!(defined(model, module(model, "^::geo")).is_empty(), "{max}");
  }
}

/// A `mod` of a module the package already has adds no module, so at the module limit it is not
/// refused: it is a module declared twice, and its items are collected into that module, whether
/// a `mod` or a directory declared the module first.
#[test]
fn a_mod_of_a_module_the_package_has_is_not_refused_at_the_limit() {
  let limits = Limits::new().with_max_modules(ROOTS + 1);
  let twice = code(DiagnosticKind::ModuleDefinedTwice);

  let text = "mod a {\n  scalar A\n}\nmod a {\n  scalar B\n}\n";
  let root = package("app", Layout::Flat, &[("lib.graphqlx", text)]);
  let analysis = analyze(&root, &[], &limits);
  let lib = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [(twice, mod_name(lib, text, "a", 1))],
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(model.modules().len(), ROOTS + 1);
  assert_eq!(defined(model, module(model, "^::app::a")), ["A", "B"]);

  let text = "mod a {\n  scalar B\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[("a/1.graphqlx", "scalar A\n"), ("lib.graphqlx", text)],
  );
  let analysis = analyze(&root, &[], &limits);
  let lib = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [(twice, mod_name(lib, text, "a", 0))],
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(model.modules().len(), ROOTS + 1);
  assert_eq!(defined(model, module(model, "^::app::a")), ["A", "B"]);
}

/// Which modules the module limit refuses, and what it reports, depend on the package set alone:
/// the files and the dependencies given in reverse change no module and no diagnostic. Every
/// directory is a module before any `mod` is walked, so the directories take the room first.
#[test]
fn the_module_limit_refuses_the_same_modules_whatever_the_order_given() {
  let app = [
    ("0.graphqlx", "mod z {}\n"),
    ("a/1.graphqlx", "mod m {}\n"),
    ("b/2.graphqlx", "mod n {}\n"),
    ("c/3.graphqlx", "scalar C\n"),
  ];
  let geo = [("g/1.graphqlx", "scalar G\n")];
  let build = |reversed: bool| -> (Package<&'static str>, Vec<Package<&'static str>>) {
    let order = |files: &[(&'static str, &'static str)]| -> Vec<(&'static str, &'static str)> {
      let mut files = files.to_vec();
      if reversed {
        files.reverse();
      }
      files
    };
    let root = package("app", Layout::Directory, &order(&app[..])).with_dependencies(["geo"]);
    let dependencies = vec![package("geo", Layout::Directory, &order(&geo[..]))];
    (root, dependencies)
  };
  // The five root modules, then `a`, `b` and `c`.
  let limits = Limits::new().with_max_modules(ROOTS + 4);

  let (root, dependencies) = build(false);
  let forward = analyze(&root, &dependencies, &limits);
  let (root, dependencies) = build(true);
  let backward = analyze(&root, &dependencies, &limits);

  let zero = file(&forward, 3, "0.graphqlx");
  assert_eq!(
    found(&forward),
    [(
      code(DiagnosticKind::ResourceLimit),
      mod_name(zero, "mod z {}\n", "z", 0)
    )],
    "{:#?}",
    forward.diagnostics()
  );
  assert_eq!(
    tree(forward.model()),
    [
      (3, vec![], ModuleKind::Root),
      (3, vec!["a"], ModuleKind::Directory),
      (3, vec!["b"], ModuleKind::Directory),
      (3, vec!["c"], ModuleKind::Directory),
      (4, vec![], ModuleKind::Root),
    ]
  );
  assert_eq!(numbered(&forward), numbered(&backward));
}

/// A long module name is not read again for each module under it.
///
/// Every model is checked when it is finished, in a build with debug assertions, and the check
/// compares each module's path with the one before it. Two neighbouring modules share their
/// ancestors' names, so a comparison that read every segment's spelling read a long ancestor's
/// name once for every module under it; comparing the interned names first reads spellings only
/// where two names differ.
///
/// Nothing here can count that work: the check allocates nothing and reports nothing. So the sizes
/// tell the two costs apart. One module named with 32 MiB of `N` holds 60 000 child modules: a file
/// of about 33 MiB, and 60 005 modules with the four root modules, within the default limits' 64
/// MiB of source and 65 536 modules. Read segment by segment, the 60 000 neighbouring pairs read
/// the long name about 2 × 10^12 bytes in all, which is minutes at memory speed; compared by symbol
/// they read only the children's short names, and the test costs about what parsing the file costs.
/// In a build without debug assertions the check does not run, and the test asserts only what the
/// analysis answers.
#[test]
fn a_long_module_name_is_not_read_again_for_each_module_under_it() {
  const LONG: usize = 1 << 25;
  const CHILDREN: u32 = 60_000;
  let mut text = String::with_capacity(LONG + 20 * CHILDREN as usize);
  text.push_str("mod ");
  text.push_str(&"N".repeat(LONG));
  text.push_str(" {\n");
  for index in 0..CHILDREN {
    text.push_str(&format!("  mod m{index} {{}}\n"));
  }
  text.push_str("}\n");
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("lib.graphqlx", text.as_str())]);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());

  let model = analysis.model();
  assert_eq!(model.modules().len(), ROOTS + 1 + CHILDREN);
  let app = model.package(model.root_package()).root_module();
  let long = model.module(app).children()[0];
  let path: Vec<&str> = model
    .module(long)
    .path()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect();
  assert_eq!(path.len(), 1);
  assert_eq!(path[0].len(), LONG);
  assert_eq!(model.module(long).children().len(), CHILDREN as usize);
}
