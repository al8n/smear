//! The GraphQLx analyzer's declarations: one definition per type or directive a program declares,
//! numbered in W's order; duplicates, in one file and across files, in each namespace; a module and
//! a type of one name; reserved names; lookup; and files that declare nothing.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, DefKind, Diagnostic, DiagnosticKind, Layout, Limits, Model, ModuleId,
    ModuleKind, Namespace, Origin, Package, PackageId, SourceFile, SourceId, Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

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

/// Where the `nth`, counting from 0, of the names `name` written right after `before` is in `text`,
/// the file `source`: a name, so one followed by no letter, digit or `_`.
fn named(source: SourceId, text: &str, before: &str, name: &str, nth: usize) -> Location {
  let written = format!("{before}{name}");
  let start = text
    .match_indices(written.as_str())
    .map(|(start, _): (usize, &str)| start + before.len())
    .filter(|start: &usize| {
      text[start + name.len()..]
        .chars()
        .next()
        .is_none_or(|next: char| !(next.is_ascii_alphanumeric() || next == '_'))
    })
    .nth(nth)
    .unwrap_or_else(|| panic!("`{written}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + name.len()))
}

/// The supplied packages' definitions, in id order: their names.
fn declared(model: &Model) -> Vec<&str> {
  model
    .defs()
    .into_iter()
    .filter(|id: &DefId| model.def(*id).origin() == Origin::User)
    .map(|id: DefId| model.def(id).name())
    .collect()
}

/// The names of the definitions `module` holds, in id order.
fn names(model: &Model, module: ModuleId) -> Vec<&str> {
  model
    .module(module)
    .defs()
    .into_iter()
    .map(|id: DefId| model.def(id).name())
    .collect()
}

/// Each diagnostic's code, primary location, and first label's location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location, Option<Location>)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// One definition of every kind, beside a schema definition and an extension, which are no
/// definitions of their own.
const EVERY_KIND: &str = "\"A scalar.\"\nscalar S\ntype O { f: i32 }\ninterface I { f: i32 }\n\
                          union U = O\nenum E { A }\ninput In { f: i32 }\ndirective @d on FIELD\n\
                          schema { query: O }\nextend type O { g: i32 }\n";

#[test]
fn every_kind_of_definition_is_a_definition_of_its_module() {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", EVERY_KIND)]);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = file(&analysis, 3, "schema.graphqlx");
  let app_root = model.package(model.root_package()).root_module();

  let expected = [
    (
      "S",
      DefKind::Scalar,
      named(schema, EVERY_KIND, "scalar ", "S", 0),
    ),
    (
      "O",
      DefKind::Object,
      named(schema, EVERY_KIND, "type ", "O", 0),
    ),
    (
      "I",
      DefKind::Interface,
      named(schema, EVERY_KIND, "interface ", "I", 0),
    ),
    (
      "U",
      DefKind::Union,
      named(schema, EVERY_KIND, "union ", "U", 0),
    ),
    (
      "E",
      DefKind::Enum,
      named(schema, EVERY_KIND, "enum ", "E", 0),
    ),
    (
      "In",
      DefKind::InputObject,
      named(schema, EVERY_KIND, "input ", "In", 0),
    ),
    (
      "d",
      DefKind::Directive,
      named(schema, EVERY_KIND, "directive @", "d", 0),
    ),
  ];
  let held: Vec<DefId> = model.module(app_root).defs().into_iter().collect();
  assert_eq!(held.len(), expected.len());
  for (id, (name, kind, location)) in held.into_iter().zip(expected) {
    let definition = model.def(id);
    assert_eq!(definition.name(), name);
    assert_eq!(definition.kind(), kind);
    assert_eq!(definition.module(), app_root);
    assert_eq!(definition.origin(), Origin::User);
    assert_eq!(definition.location(), location);
    assert!(!definition.has_errors());
    let contributions: Vec<(Location, bool)> = definition
      .contributions()
      .map(|part| (part.location(), part.is_extension()))
      .collect();
    // Its definition, and for `O` the extension too, named where its target path is.
    let mut parts = vec![(location, false)];
    if name == "O" {
      parts.push((named(schema, EVERY_KIND, "extend type ", "O", 0), true));
    }
    assert_eq!(contributions, parts, "{name}");
  }

  let scalar = model.def(
    model
      .module(app_root)
      .defs()
      .into_iter()
      .next()
      .expect("`S`"),
  );
  let description = scalar.description().expect("`S` is described");
  assert_eq!(description.text(), "A scalar.");
  assert_eq!(
    description.location(),
    Location::new(schema.get(), SimpleSpan::new(0, "\"A scalar.\"".len()))
  );
  assert_eq!(
    model
      .def(model.lookup(Namespace::Type, "^::app::O").expect("`O`"))
      .description(),
    None
  );
}

#[test]
fn def_ids_follow_the_package_the_module_path_the_source_and_the_position() {
  // `A` is written further into its file than `B2` is into its, and its file comes first; `B2` is
  // written before `B1`; `Z` is in a file that numbers before `m/c.graphqlx`, but in a module
  // that comes after `m`; `geo`'s definition comes after every one of the root's.
  let app = [
    ("b.graphqlx", "scalar B2\nscalar B1\n"),
    ("m/c.graphqlx", "scalar C\n"),
    (
      "a.graphqlx",
      "\n\n\n\n\n\n\n\n\n\n\n\nscalar A\nmod z {\n  scalar Z\n}\n",
    ),
  ];
  let geo = [("g.graphqlx", "scalar G\n")];
  let alpha = [("x.graphqlx", "scalar Alpha\n")];
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
      package("geo", Layout::Flat, &order(&geo[..])),
      package("alpha", Layout::Flat, &order(&alpha[..])),
    ];
    if reversed {
      dependencies.reverse();
    }
    (root, dependencies)
  };

  let (root, dependencies) = build(false);
  let forward = analyze(&root, &dependencies, &Limits::new());
  assert!(forward.is_ok(), "{:#?}", forward.diagnostics());
  let model = forward.model();
  assert_eq!(declared(model), ["A", "B2", "B1", "C", "Z", "Alpha", "G"]);
  let m = model
    .module_by_path("^::app::m")
    .expect("the directory `m`");
  let z = model.module_by_path("^::app::z").expect("the `mod z`");
  assert_eq!(model.module(z).kind(), ModuleKind::Inline);
  assert_eq!(names(model, m), ["C"]);
  assert_eq!(names(model, z), ["Z"]);
  for (canonical, module) in [("^::app::m::C", m), ("^::app::z::Z", z)] {
    let id = model.lookup(Namespace::Type, canonical).expect(canonical);
    assert_eq!(model.def(id).module(), module);
    assert_eq!(model.canonical_name(id).to_string(), canonical);
  }

  // No permutation of the files or the dependencies changes an id or a diagnostic.
  let (root, dependencies) = build(true);
  let backward = analyze(&root, &dependencies, &Limits::new());
  assert_eq!(summary(&forward), summary(&backward));
}

/// Every definition with what it answers, and every diagnostic: what a permutation of the input
/// must not change.
#[allow(clippy::type_complexity)]
fn summary(
  analysis: &Analysis,
) -> (
  Vec<(DefId, &str, DefKind, ModuleId, Location, Origin)>,
  Vec<Diagnostic>,
) {
  let model = analysis.model();
  let defs = model
    .defs()
    .into_iter()
    .map(|id: DefId| {
      let definition = model.def(id);
      (
        id,
        definition.name(),
        definition.kind(),
        definition.module(),
        definition.location(),
        definition.origin(),
      )
    })
    .collect();
  (defs, analysis.diagnostics().iter().cloned().collect())
}

/// Two definitions of each name in each namespace, and a type and a directive of one name.
const TWICE: &str = "scalar X\nscalar X\ndirective @d on FIELD\ndirective @d on FIELD\n\
                     scalar same\ndirective @same on FIELD\n";

#[test]
fn two_definitions_of_one_name_in_one_namespace_of_one_module_are_reported() {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", TWICE)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  // The later of each pair, with a label at the first; a type and a directive of one name are in
  // two namespaces, and so are no duplicate.
  assert_eq!(
    found(&analysis),
    [
      (
        "smear::schema::duplicate-type-name",
        named(schema, TWICE, "scalar ", "X", 1),
        Some(named(schema, TWICE, "scalar ", "X", 0)),
      ),
      (
        "smear::schema::duplicate-directive-definition",
        named(schema, TWICE, "directive @", "d", 1),
        Some(named(schema, TWICE, "directive @", "d", 0)),
      ),
    ]
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(diagnostic.labels(), 1);
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("first defined here")
    );
    assert!(diagnostic.primary_label().is_some());
    assert!(diagnostic.help().is_some());
  }

  // Both of each pair keep an id; the name binds the first.
  let model = analysis.model();
  let app_root = model.package(model.root_package()).root_module();
  assert_eq!(names(model, app_root), ["X", "X", "d", "d", "same", "same"]);
  let held: Vec<DefId> = model.module(app_root).defs().into_iter().collect();
  assert_eq!(model.lookup(Namespace::Type, "^::app::X"), Some(held[0]));
  assert_eq!(
    model.lookup(Namespace::Directive, "@^::app::d"),
    Some(held[2])
  );
  assert_eq!(model.lookup(Namespace::Type, "^::app::same"), Some(held[4]));
  assert_eq!(
    model.lookup(Namespace::Directive, "@^::app::same"),
    Some(held[5])
  );
  // The duplicate is the definition that has errors.
  assert!(!model.def(held[0]).has_errors());
  assert!(model.def(held[1]).has_errors());
}

#[test]
fn two_files_of_one_directory_share_its_names() {
  let first = "type Y { f: i32 }\n";
  let second = "scalar Y\n";
  let root = package(
    "app",
    Layout::Directory,
    &[("dir/b.graphqlx", second), ("dir/a.graphqlx", first)],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, 3, "dir/a.graphqlx");
  let b = file(&analysis, 3, "dir/b.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      "smear::schema::duplicate-type-name",
      named(b, second, "scalar ", "Y", 0),
      Some(named(a, first, "type ", "Y", 0)),
    )]
  );

  // Two `mod`s of one module, in two files, hold their definitions in that one module.
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("x/a.graphqlx", "mod shared {\n  scalar A\n}\n"),
      ("x/b.graphqlx", "mod shared {\n  scalar B\n}\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let codes: Vec<&str> = found(&analysis)
    .into_iter()
    .map(|(code, _, _): (&'static str, Location, Option<Location>)| code)
    .collect();
  assert_eq!(codes, [DiagnosticKind::ModuleDefinedTwice.code().as_str()]);
  let model = analysis.model();
  let shared = model
    .module_by_path("^::app::x::shared")
    .expect("one module");
  assert_eq!(names(model, shared), ["A", "B"]);
}

#[test]
fn a_type_named_as_a_child_module_of_its_module_collides() {
  let text = "mod media {\n  scalar M\n}\nscalar media\ndirective @media on FIELD\n\
              mod a {\n  mod b {}\n}\nscalar b\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  // The type, labelled at the `mod`; the directive is in another namespace, and `b` is a child of
  // `a`, not of the module that holds the type `b`.
  assert_eq!(
    found(&analysis),
    [(
      DiagnosticKind::ModuleTypeCollision.code().as_str(),
      named(schema, text, "scalar ", "media", 0),
      Some(named(schema, text, "mod ", "media", 0)),
    )]
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(
    diagnostic.label(0).map(|label| label.text()),
    Some("the module is declared here")
  );
  assert!(diagnostic.help().is_some());

  // A directory module, which has no position of its own, is labelled at the package source.
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("media/x.graphqlx", "scalar X\n"),
      ("schema.graphqlx", "scalar media\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      DiagnosticKind::ModuleTypeCollision.code().as_str(),
      named(schema, "scalar media\n", "scalar ", "media", 0),
      Some(package_source(&analysis, 3)),
    )]
  );
}

#[test]
fn a_supplied_name_that_begins_with_two_underscores_is_reserved() {
  let text = "scalar __Mine\ndirective @__mine on FIELD\nscalar fine__\n\
              scalar __Twice\nscalar __Twice\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", "scalar __Geo\n")]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  let geo = file(&analysis, 4, "geo.graphqlx");
  // The second `__Twice` is a duplicate, and only that: the name it would reserve is the first's.
  assert_eq!(
    found(&analysis),
    [
      (
        "smear::schema::reserved-type-name",
        named(schema, text, "scalar ", "__Mine", 0),
        None
      ),
      (
        "smear::schema::reserved-directive-name",
        named(schema, text, "directive @", "__mine", 0),
        None
      ),
      (
        "smear::schema::reserved-type-name",
        named(schema, text, "scalar ", "__Twice", 0),
        None
      ),
      (
        "smear::schema::duplicate-type-name",
        named(schema, text, "scalar ", "__Twice", 1),
        Some(named(schema, text, "scalar ", "__Twice", 0)),
      ),
      (
        "smear::schema::reserved-type-name",
        named(geo, "scalar __Geo\n", "scalar ", "__Geo", 0),
        None
      ),
    ]
  );
}

#[test]
fn a_meta_types_name_in_the_root_module_redefines_a_built_in() {
  // In a `Flat` root, all of whose files are its root module, a type named as one of GraphQL's
  // introspection types redefines it, whatever its kind, and its name is not also reserved; a
  // directive of that name is reserved; and a second type of the name is a duplicate and only that.
  let text = "type Query {\n  ok: Int\n}\ntype __Type {\n  name: String\n}\n\
              enum __TypeKind {\n  A\n}\ndirective @__Type on FIELD\n\
              type __Type {\n  kind: String\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        "smear::schema::redefined-built-in-type",
        named(schema, text, "type ", "__Type", 0),
        None
      ),
      (
        "smear::schema::redefined-built-in-type",
        named(schema, text, "enum ", "__TypeKind", 0),
        None
      ),
      (
        "smear::schema::reserved-directive-name",
        named(schema, text, "directive @", "__Type", 0),
        None
      ),
      (
        "smear::schema::duplicate-type-name",
        named(schema, text, "type ", "__Type", 1),
        Some(named(schema, text, "type ", "__Type", 0)),
      ),
    ]
  );
  // GraphQL's phrase and help, and a message of the analyzer's own.
  let first = analysis.diagnostics().iter().next().expect("a diagnostic");
  assert_eq!(
    first.to_string(),
    "the type `__Type` redefines an introspection type"
  );
  assert_eq!(
    first.primary_label(),
    Some("redefinition of an introspection type")
  );
  assert_eq!(
    first.help(),
    Some(
      "the `__`-prefixed introspection types are injected by every build and cannot be replaced."
    )
  );

  // In a module that is no root module, and in a dependency, the name is reserved and redefines
  // nothing: GraphQL's own document is the root package's root module alone.
  let sub = "type __Type {\n  name: String\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("schema.graphqlx", "type Query {\n  ok: Int\n}\n"),
      ("sub/a.graphqlx", sub),
    ],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", sub)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let sub_file = file(&analysis, 3, "sub/a.graphqlx");
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        "smear::schema::reserved-type-name",
        named(sub_file, sub, "type ", "__Type", 0),
        None
      ),
      (
        "smear::schema::reserved-type-name",
        named(geo_file, sub, "type ", "__Type", 0),
        None
      ),
    ]
  );
}

#[test]
fn a_file_that_holds_only_what_the_lexer_skips_declares_nothing() {
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("empty.graphqlx", ""),
      ("notes.graphqlx", "# Only a comment.\n"),
      (
        "trivia.graphqlx",
        "\u{FEFF} \t\r\n,,\n# a comment, last, with no line end",
      ),
      (
        "docs/README.graphqlx",
        "# What the `docs` directory holds.\n",
      ),
      ("schema.graphqlx", "scalar Real\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert!(analysis.diagnostics().is_empty());
  let model = analysis.model();
  assert_eq!(declared(model), ["Real"]);
  // A directory whose one file only documents it is a module, and an empty one.
  let docs = model.module_by_path("^::app::docs").expect("the directory");
  assert_eq!(model.module(docs).kind(), ModuleKind::Directory);
  assert!(model.module(docs).defs().is_empty());

  // So is the same as bytes; and anything else is parsed, and refused as the parser refuses it.
  let bytes = Package::new("app", Layout::Flat, Tier::Std).with_files([
    SourceFile::new("notes.graphqlx", "# Only a comment.\n".as_bytes()),
    SourceFile::new("junk.graphqlx", "# a comment\n}\n".as_bytes()),
  ]);
  let analysis = analyze(&bytes, &[], &Limits::new());
  let junk = file(&analysis, 3, "junk.graphqlx");
  let codes: Vec<(&str, u32)> = found(&analysis)
    .into_iter()
    .map(|(code, at, _): (&'static str, Location, Option<Location>)| (code, at.source()))
    .collect();
  assert!(!codes.is_empty());
  for (code, source) in codes {
    assert!(code.starts_with("smear::syntax::"), "{code}");
    assert_eq!(source, junk.get());
  }
}

/// The codes the declarations share with GraphQL's rules are `smear-schema`'s own.
#[cfg(feature = "validator")]
#[test]
fn the_shared_codes_are_smear_schemas() {
  use smear::validator::SchemaErrorKind;

  let text = "scalar X\nscalar X\ndirective @d on FIELD\ndirective @d on FIELD\n\
              scalar __Y\ndirective @__y on FIELD\nextend type X {\n  f: Int\n}\n\
              scalar Z @d(nope: 1)\nschema {\n  query: X\n  query: X\n}\nschema {\n  query: X\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let mut codes: Vec<&str> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect();
  codes.sort_unstable();
  let mut expected: Vec<&str> = [
    SchemaErrorKind::DuplicateTypeName,
    SchemaErrorKind::DuplicateDirectiveDefinition,
    SchemaErrorKind::ReservedTypeName,
    SchemaErrorKind::ReservedDirectiveName,
    SchemaErrorKind::ExtensionKindMismatch,
    SchemaErrorKind::UndefinedDirectiveArgument,
    SchemaErrorKind::DuplicateRootOperationType,
    SchemaErrorKind::DuplicateSchemaDefinition,
  ]
  .iter()
  .map(|kind: &SchemaErrorKind| kind.code().as_str())
  .collect();
  expected.sort_unstable();
  assert_eq!(codes, expected);
}

/// How many definitions the built-in library alone gives a model: its own, and `bool`, which it
/// writes.
fn library_definitions() -> u32 {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  analyze(&root, &[], &Limits::new()).model().defs().len()
}

/// The definition limit counts every definition the model would hold, the built-in library's
/// included: the first a supplied package would add past it is reported where it is named, with
/// the limit, and nothing after it is collected.
#[test]
fn the_first_definition_past_the_limit_is_reported_and_nothing_after_it_is_declared() {
  let library = library_definitions();
  let resource = DiagnosticKind::ResourceLimit.code().as_str();
  let text = "scalar A\nscalar B\ntype C {\n  b: B\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  let max = library + 1;
  let analysis = analyze(&root, &[], &Limits::new().with_max_definitions(max));
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis),
    [(resource, named(schema, text, "scalar ", "B", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!analysis.is_ok());
  let message = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic")
    .to_string();
  assert!(message.contains("the type `B`"), "{message}");
  assert!(message.contains(&format!("limit of {max}")), "{message}");
  let model = analysis.model();
  assert_eq!(declared(model), ["A"]);
  assert_eq!(model.defs().len(), max);

  // A limit below the built-in library's own count admits no supplied definition, and the library
  // is declared whole all the same.
  let analysis = analyze(&root, &[], &Limits::new().with_max_definitions(0));
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis),
    [(resource, named(schema, text, "scalar ", "A", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(model.defs().len(), library);
  assert_eq!(
    model.def(model.builtins().deprecated()).name(),
    "deprecated"
  );
}

/// A program whose definitions fill the limit exactly is no error.
#[test]
fn the_definition_limit_met_exactly_is_no_error() {
  let library = library_definitions();
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "scalar A\nscalar B\n")],
  );
  let analysis = analyze(&root, &[], &Limits::new().with_max_definitions(library + 2));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert_eq!(declared(analysis.model()), ["A", "B"]);
  assert_eq!(analysis.model().defs().len(), library + 2);
}

/// A primitive a package writes first is a definition `core` makes, and it counts where it is
/// written: `i32` here takes the last place the limit leaves, so the scalar after it is past the
/// limit; and with one place fewer, `i32` itself is, reported where it is written.
#[test]
fn a_written_primitive_counts_toward_the_definition_limit() {
  let library = library_definitions();
  let resource = DiagnosticKind::ResourceLimit.code().as_str();
  let text = "type Query {\n  a: i32\n}\nscalar Real\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);

  let analysis = analyze(&root, &[], &Limits::new().with_max_definitions(library + 2));
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis),
    [(resource, named(schema, text, "scalar ", "Real", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(declared(model), ["Query"]);
  assert!(model.lookup(Namespace::Type, "^::core::i32").is_some());
  assert_eq!(model.defs().len(), library + 2);

  // `i32`, refused, is no definition, so the field that names it names nothing either.
  let analysis = analyze(&root, &[], &Limits::new().with_max_definitions(library + 1));
  let schema = file(&analysis, 3, "schema.graphqlx");
  let i32_at = named(schema, text, "a: ", "i32", 0);
  assert_eq!(
    found(&analysis),
    [
      (resource, i32_at, None),
      ("smear::schema::undefined-type", i32_at, None)
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let message = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic")
    .to_string();
  assert!(message.contains("the primitive `i32`"), "{message}");
  let model = analysis.model();
  assert_eq!(declared(model), ["Query"]);
  assert_eq!(model.lookup(Namespace::Type, "^::core::i32"), None);
  assert_eq!(model.defs().len(), library + 1);
}

/// A root package's definition that replaces a GraphQL built-in takes the built-in's definition
/// and adds none to the count, unless it makes a primitive no package writes.
#[test]
fn a_replacing_definition_counts_only_the_primitive_it_makes() {
  let library = library_definitions();
  let resource = DiagnosticKind::ResourceLimit.code().as_str();
  let text = "scalar String\ndirective @deprecated(reason: String) on FIELD_DEFINITION\n\
              scalar Int\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);

  // `String` and `@deprecated` take their built-ins' places, and `Int` makes `i32`.
  let analysis = analyze(&root, &[], &Limits::new().with_max_definitions(library + 1));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert_eq!(analysis.model().defs().len(), library + 1);

  let analysis = analyze(&root, &[], &Limits::new().with_max_definitions(library));
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found(&analysis),
    [(resource, named(schema, text, "scalar ", "Int", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(analysis.model().defs().len(), library);
}
