//! The GraphQLx analyzer's own rules, which `analyze` runs over every supplied package once its
//! phases have written the model: a program defines its schema at most once; only the root
//! package defines or extends the schema; and a package extends its own definitions alone, but
//! for the directives the root package adds to a built-in. Each rule says nothing about what a
//! phase already refused, marks what it reports, and quotes a name only as an excerpt.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Contribution, DefId, Diagnostic, DiagnosticKind, Layout, Limits, MemberId, Model,
    Namespace, Package, PackageId, Primitive, Root, RootOperation, SourceFile, SourceId, Tier,
    analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for a second schema definition, which the analyzer shares.
const DUPLICATE: &str = "smear::schema::duplicate-schema-definition";

/// The code for a schema item a package other than the root package writes.
const OUTSIDE: &str = "smear::graphqlx::schema-outside-root";

/// The code for an extension of a definition its package may not extend.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";

/// What the orphan rule tells the author of an extension of another supplied package's definition.
const THIRD_PARTY: &str = "a package extends its own definitions alone, so that a definition is \
                           the same in every program that holds it: define a type or a directive \
                           of this package's own, or make the change in the package that defines \
                           it";

/// What the orphan rule tells a dependency that extends a built-in.
const NOT_THE_ROOT: &str = "only the root package extends a definition of `core`, `alloc` or \
                            `std`, and only to add directives: remove this extension, or write it \
                            in the root package";

/// What the orphan rule tells the root package when it adds more than directives to a built-in.
const MORE_THAN_DIRECTIVES: &str = "an extension of a definition of `core`, `alloc` or `std` adds \
                                    directives and nothing else: remove what this one adds beside \
                                    them";

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

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_one(text: &'static str) -> Analysis {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  analyze(&root, &[], &Limits::new())
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

/// The root package's one file, `schema.graphqlx`.
fn schema_file(analysis: &Analysis) -> SourceId {
  file(analysis, 3, "schema.graphqlx")
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

/// Each diagnostic's message, in the list's order.
fn messages(analysis: &Analysis) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect::<Vec<String>>()
}

/// Whether each of the schema's contributions has errors, in order.
fn schema_errors(model: &Model) -> Vec<bool> {
  model
    .schema()
    .contributions()
    .map(|part: Contribution<'_>| part.has_errors())
    .collect::<Vec<bool>>()
}

/// The type the canonical name `name` names.
fn def(model: &Model, name: &str) -> DefId {
  model
    .lookup(Namespace::Type, name)
    .unwrap_or_else(|| panic!("no type `{name}`"))
}

/// Whether each contribution of `def` is an extension, and whether it has errors, in order.
fn contributions(model: &Model, def: DefId) -> Vec<(bool, bool)> {
  model
    .def(def)
    .contributions()
    .map(|part: Contribution<'_>| (part.is_extension(), part.has_errors()))
    .collect::<Vec<(bool, bool)>>()
}

/// How many definitions the built-in library alone gives a model: its own, and the primitives it
/// writes.
fn library_definitions() -> u32 {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  analyze(&root, &[], &Limits::new()).model().defs().len()
}

#[test]
fn a_second_schema_definition_is_reported_at_the_later_with_a_label_at_the_first() {
  // In one file: each definition after the first is reported, each with a label at the first.
  let text = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n\
              schema {\n  query: Nowhere\n}\nschema {\n  query: Query\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let first = at(schema, text, "schema", 0);
  assert_eq!(
    found(&analysis),
    [
      (DUPLICATE, at(schema, text, "schema", 1), Some(first)),
      (DUPLICATE, at(schema, text, "schema", 2), Some(first)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!analysis.is_ok());
  // GraphQL's own phrase, label and help for its own rule.
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.to_string(),
      "the root package defines the schema twice"
    );
    assert_eq!(
      diagnostic.primary_label(),
      Some("duplicate schema definition")
    );
    assert_eq!(diagnostic.labels(), 1);
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("first defined here")
    );
    assert_eq!(
      diagnostic.help(),
      Some("a document holds at most one `schema` block; put the extra roots in `extend schema`.")
    );
  }
  // The first definition is the schema's, and the type a later one names is not looked for.
  assert!(matches!(
    analysis.model().schema().root(RootOperation::Query),
    Some(Root::Written(_))
  ));

  // In two files, whichever is passed first: the later by source is the one reported.
  let a = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n";
  let b = "schema {\n  query: Query\n}\n";
  for files in [
    [("a.graphqlx", a), ("b.graphqlx", b)],
    [("b.graphqlx", b), ("a.graphqlx", a)],
  ] {
    let root = package("app", Layout::Flat, &files);
    let analysis = analyze(&root, &[], &Limits::new());
    let a_file = file(&analysis, 3, "a.graphqlx");
    let b_file = file(&analysis, 3, "b.graphqlx");
    assert_eq!(
      found(&analysis),
      [(
        DUPLICATE,
        at(b_file, b, "schema", 0),
        Some(at(a_file, a, "schema", 0))
      )],
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn extensions_beside_one_definition_are_no_duplicate() {
  // One definition, with an extension written before it and one after it.
  let text = "extend schema {\n  mutation: Change\n}\nschema {\n  query: Query\n}\n\
              extend schema {\n  subscription: Tick\n}\ntype Query {\n  ok: Int\n}\n\
              type Change {\n  done: Int\n}\ntype Tick {\n  at: Int\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let extensions: Vec<bool> = analysis
    .model()
    .schema()
    .contributions()
    .map(|part: Contribution<'_>| part.is_extension())
    .collect::<Vec<bool>>();
  assert_eq!(extensions, [false, true, true]);
}

#[test]
fn no_schema_definition_is_no_diagnostic() {
  // No schema item at all.
  let analysis = analyze_one("type Query {\n  ok: Int\n}\n");
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(analysis.model().schema().contributions().count(), 0);

  // A schema extension with no schema definition to extend is no rule's of this layer: it gives
  // the schema its root, beside the one presumed.
  let analysis = analyze_one(
    "type Query {\n  ok: Int\n}\ntype Change {\n  done: Int\n}\n\
     extend schema {\n  mutation: Change\n}\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let schema = analysis.model().schema();
  assert!(matches!(
    schema.root(RootOperation::Query),
    Some(Root::Presumed(_))
  ));
  assert!(matches!(
    schema.root(RootOperation::Mutation),
    Some(Root::Written(_))
  ));
}

#[test]
fn a_dependency_that_defines_the_schema_is_reported_once_and_not_as_a_duplicate() {
  let ours = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n";
  let theirs = "schema {\n  query: Point\n}\ntype Point {\n  x: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  // Beside the root package's own definition it is no second definition of the schema: it is
  // reported once, as the dependency's to remove, with no label.
  assert_eq!(
    found(&analysis),
    [(OUTSIDE, at(geo_file, theirs, "schema", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!analysis.is_ok());
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(diagnostic.code(), DiagnosticKind::SchemaOutsideRoot.code());
  assert_eq!(
    diagnostic.to_string(),
    "package `geo` defines the schema, which only the root package defines and extends"
  );
  assert_eq!(
    diagnostic.primary_label(),
    Some("schema outside the root package")
  );
  assert_eq!(diagnostic.labels(), 0);
  assert_eq!(
    diagnostic.help(),
    Some(
      "a dependency is a library, and the schema is the root package's: remove this item, or \
       write it in the root package"
    )
  );

  // With no schema definition of the root package's, two dependencies' definitions are each
  // reported once, and neither as a duplicate of the other.
  let stars = "schema {\n  query: Star\n}\ntype Star {\n  name: String\n}\n";
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["astro", "geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let astro = package("astro", Layout::Flat, &[("astro.graphqlx", stars)]);
  let analysis = analyze(&root, &[geo, astro], &Limits::new());
  let astro_file = file(&analysis, 4, "astro.graphqlx");
  let geo_file = file(&analysis, 5, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (OUTSIDE, at(astro_file, stars, "schema", 0), None),
      (OUTSIDE, at(geo_file, theirs, "schema", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_dependency_that_extends_the_schema_is_reported() {
  let ours = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n";
  let theirs = "type Point {\n  x: Int\n}\nextend schema {\n  mutation: Point\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(OUTSIDE, at(geo_file, theirs, "extend", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    ["package `geo` extends the schema, which only the root package defines and extends"]
  );
  // It gives the schema nothing: the root it writes is not the schema's.
  assert_eq!(
    analysis.model().schema().root(RootOperation::Mutation),
    None
  );
}

#[test]
fn the_root_package_defines_and_extends_the_schema_from_any_of_its_modules() {
  // The definition in one directory module, an extension in another, and one in an inline `mod`.
  let root = package(
    "app",
    Layout::Directory,
    &[
      (
        "types.graphqlx",
        "type Query {\n  ok: Int\n}\ntype Change {\n  done: Int\n}\ntype Tick {\n  at: Int\n}\n",
      ),
      ("defined/s.graphqlx", "schema {\n  query: ~::Query\n}\n"),
      (
        "extended/e.graphqlx",
        "extend schema {\n  mutation: ~::Change\n}\n\
         mod inner {\n  extend schema {\n    subscription: ~::Tick\n  }\n}\n",
      ),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let schema = analysis.model().schema();
  for operation in [
    RootOperation::Query,
    RootOperation::Mutation,
    RootOperation::Subscription,
  ] {
    assert!(
      matches!(schema.root(operation), Some(Root::Written(_))),
      "{operation:?}"
    );
  }
}

#[test]
fn the_root_extending_a_dependencys_type_is_an_orphan() {
  // A dependency's type, and a dependency's directive.
  let ours = "extend type ^::geo::Point {\n  label: String\n}\n\
              extend directive @^::geo::tag @deprecated\n";
  let theirs = "type Point {\n  x: Int\n}\ndirective @tag on OBJECT\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = schema_file(&analysis);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  // Each at the extension's target path, with a label at the definition's name.
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, ours, "^::geo::Point", 0),
        Some(at(geo_file, theirs, "Point", 0))
      ),
      (
        ORPHAN,
        at(schema, ours, "^::geo::tag", 0),
        Some(at(geo_file, theirs, "tag", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!analysis.is_ok());
  assert_eq!(
    messages(&analysis),
    [
      "package `app` extends the type `Point`, which package `geo` defines",
      "package `app` extends the directive `@tag`, which package `geo` defines",
    ]
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(diagnostic.code(), DiagnosticKind::OrphanExtension.code());
    assert_eq!(diagnostic.primary_label(), Some("orphan extension"));
    assert_eq!(diagnostic.labels(), 1);
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("the definition it extends")
    );
    assert_eq!(diagnostic.help(), Some(THIRD_PARTY));
  }
  // The extension is the definition's contribution all the same: the model holds what is written.
  let point = def(analysis.model(), "^::geo::Point");
  assert_eq!(
    contributions(analysis.model(), point),
    [(false, false), (true, true)]
  );
}

#[test]
fn a_dependency_extending_another_packages_type_is_an_orphan() {
  // Another dependency's type, and the root package's, which a dependency reaches when it
  // declares the root package.
  let ours = "type Query {\n  ok: Int\n}\n";
  let stars = "type Star {\n  name: String\n}\n";
  let theirs = "extend type ^::astro::Star {\n  near: Int\n}\n\
                extend type ^::app::Query {\n  fromGeo: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]);
  let astro = package("astro", Layout::Flat, &[("astro.graphqlx", stars)]);
  let geo =
    package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]).with_dependencies(["astro", "app"]);
  let analysis = analyze(&root, &[geo, astro], &Limits::new());
  let schema = schema_file(&analysis);
  let astro_file = file(&analysis, 4, "astro.graphqlx");
  let geo_file = file(&analysis, 5, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(geo_file, theirs, "^::astro::Star", 0),
        Some(at(astro_file, stars, "Star", 0))
      ),
      (
        ORPHAN,
        at(geo_file, theirs, "^::app::Query", 0),
        Some(at(schema, ours, "Query", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "package `geo` extends the type `Star`, which package `astro` defines",
      "package `geo` extends the type `Query`, which package `app` defines",
    ]
  );
}

#[test]
fn a_package_extends_its_own_types_from_any_module() {
  // The root package and a dependency each extend a type of their own: from the module that
  // defines it, from an inline `mod` inside that one, and from a directory module.
  let root = package(
    "app",
    Layout::Directory,
    &[
      (
        "shape.graphqlx",
        "type Shape {\n  own: Int\n}\nextend type Shape {\n  beside: Int\n}\n\
         mod inner {\n  extend type ..::Shape {\n    inMod: Int\n  }\n}\n",
      ),
      ("m/x.graphqlx", "extend type ~::Shape {\n  fromM: Int\n}\n"),
    ],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Directory,
    &[
      ("lib.graphqlx", "type Point {\n  x: Int\n}\n"),
      ("ext/e.graphqlx", "extend type ~::Point {\n  y: Int\n}\n"),
    ],
  );
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(
    contributions(model, def(model, "^::app::Shape")),
    [(false, false), (true, false), (true, false), (true, false)]
  );
  assert_eq!(
    contributions(model, def(model, "^::geo::Point")),
    [(false, false), (true, false)]
  );
}

#[test]
fn the_root_adds_directives_to_a_built_in() {
  // To `alloc`'s `string`, by its GraphQL name; to `i32`, a primitive `core` materialises; to
  // `f64`, which the root package's own `Float` replaces; to `core`'s `NonZero`; and to a
  // directive of `core`'s.
  let text = "directive @x on SCALAR | DIRECTIVE_DEFINITION\n\
              extend scalar String @x\nextend scalar Int @x\n\
              scalar Float\nextend scalar Float @x\n\
              extend scalar NonZero @x\nextend directive @deprecated @x\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  // Each is a contribution of a built-in definition, which the rule read and let be.
  let model = analysis.model();
  let builtins = model.builtins();
  let extended = [
    builtins.string(),
    builtins
      .primitive(Primitive::signed(32))
      .expect("`Int` writes `i32`"),
    builtins
      .primitive(Primitive::F64)
      .expect("the root package replaces `f64`"),
    builtins.non_zero(),
    builtins.deprecated(),
  ];
  for built_in in extended {
    assert_eq!(
      contributions(model, built_in),
      [(false, false), (true, false)],
      "{}",
      model.def(built_in).name()
    );
  }
}

#[test]
fn the_root_adding_more_than_directives_to_a_built_in_is_an_orphan() {
  // An enum value to `Tier`, a field to `__Type`, and an `implements` entry to a scalar: to
  // `alloc`'s `string`, and to `i32`, which has no position for a label to point at.
  let text = "interface Marked @marker\n\
              extend enum Tier {\n  EXTRA\n}\n\
              extend type __Type {\n  extra: Int\n}\n\
              extend scalar String implements Marked\n\
              extend scalar Int implements Marked\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let named = |name: &str| -> Option<Location> { Some(model.def(def(model, name)).location()) };
  assert_eq!(
    found(&analysis),
    [
      (ORPHAN, at(schema, text, "Tier", 0), named("^::core::Tier")),
      (
        ORPHAN,
        at(schema, text, "__Type", 0),
        named("^::core::__Type")
      ),
      (
        ORPHAN,
        at(schema, text, "String", 0),
        named("^::alloc::string")
      ),
      (ORPHAN, at(schema, text, "Int", 1), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each names the definition by its own name, `string` for `String` and `i32` for `Int`, and
  // the two packages.
  let more = |name: &str, built_in: &str| -> String {
    format!(
      "package `app` adds more than directives to the type `{name}` of the built-in package \
       `{built_in}`"
    )
  };
  assert_eq!(
    messages(&analysis),
    [
      more("Tier", "core"),
      more("__Type", "core"),
      more("string", "alloc"),
      more("i32", "core"),
    ]
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(diagnostic.help(), Some(MORE_THAN_DIRECTIVES));
    assert_eq!(diagnostic.primary_label(), Some("orphan extension"));
  }
}

#[test]
fn a_dependency_extending_a_built_in_is_an_orphan() {
  // With a directive alone, which the root package may add and a dependency may not.
  let ours = "type Query {\n  ok: Int\n}\n";
  let theirs = "directive @once on SCALAR\nextend scalar String @once\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  let model = analysis.model();
  let string = model.builtins().string();
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      at(geo_file, theirs, "String", 0),
      Some(model.def(string).location())
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "package `geo` extends the type `string` of the built-in package `alloc`, which only the \
       root package extends"
    ]
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(diagnostic.help(), Some(NOT_THE_ROOT));
}

#[test]
fn a_flat_package_never_meets_the_orphan_rule() {
  // Every file of a `Flat` package is one module of one package, as the documents of a GraphQL
  // schema are one schema: each extension is of a definition of the package's own, whichever
  // file holds it, or adds a directive to a built-in.
  let a = "type Query {\n  ok: Int\n}\ninterface Node {\n  id: ID\n}\nenum Color {\n  RED\n}\n\
           input Filter {\n  f: Int\n}\nunion Either = Query\nscalar Date\n\
           directive @d on SCALAR | DIRECTIVE_DEFINITION\n";
  let b = "extend type Query implements Node {\n  id: ID\n}\n\
           extend interface Node {\n  name: String\n}\nextend enum Color {\n  GREEN\n}\n\
           extend input Filter {\n  g: Int\n}\nextend union Either = Other\n\
           extend scalar Date @d\nextend directive @d @deprecated\nextend scalar String @d\n\
           type Other {\n  ok: Int\n}\n";
  for files in [
    [("a.graphqlx", a), ("b.graphqlx", b)],
    [("b.graphqlx", b), ("a.graphqlx", a)],
  ] {
    let root = package("app", Layout::Flat, &files);
    let analysis = analyze(&root, &[], &Limits::new());
    assert!(
      analysis.diagnostics().is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn a_rule_says_nothing_about_a_contribution_a_phase_refused() {
  let resource = DiagnosticKind::ResourceLimit.code().as_str();
  // The definition limit leaves room for `Query` and for `i32`, which `Int` writes, and for no
  // definition after them: the primitive a later schema item writes first is refused where it is
  // written, inside that item.
  let limits = Limits::new().with_max_definitions(library_definitions() + 2);

  // A second schema definition of the root package's that holds the refusal is not also reported
  // as a duplicate.
  let text = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\nschema {\n  query: u8\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &limits);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(resource, at(schema, text, "u8", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(schema_errors(analysis.model()), [false, true]);
  // With no limit in the way nothing refuses it, and the rule does.
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE,
      at(schema, text, "schema", 1),
      Some(at(schema, text, "schema", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // A dependency's schema definition that holds the refusal is not also reported as a schema item
  // outside the root package.
  let ours = "type Query {\n  ok: Int\n}\n";
  let theirs = "schema {\n  query: u8\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let dependencies = [package("geo", Layout::Flat, &[("geo.graphqlx", theirs)])];
  let analysis = analyze(&root, &dependencies, &limits);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(resource, at(geo_file, theirs, "u8", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(schema_errors(analysis.model()), [true]);
  let analysis = analyze(&root, &dependencies, &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(OUTSIDE, at(geo_file, theirs, "schema", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  // An extension of a dependency's type that names a type that is none: name resolution's
  // diagnostic lies within the extension, so the extension is not also reported as an orphan.
  let theirs = "type Point {\n  x: Int\n}\n";
  let dependencies = [package("geo", Layout::Flat, &[("geo.graphqlx", theirs)])];
  let ours = "extend type ^::geo::Point {\n  extra: Missing\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &dependencies, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      "smear::schema::undefined-type",
      at(schema, ours, "Missing", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let point = def(analysis.model(), "^::geo::Point");
  assert_eq!(
    contributions(analysis.model(), point),
    [(false, false), (true, true)]
  );
  // With a type that is one, nothing refuses the extension, and the rule does.
  let ours = "extend type ^::geo::Point {\n  extra: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &dependencies, &Limits::new());
  let schema = schema_file(&analysis);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      at(schema, ours, "^::geo::Point", 0),
      Some(at(geo_file, theirs, "Point", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_rules_error_marks_its_contribution_and_its_definition_and_fails_the_verdict() {
  // The schema's contributions: the root package's first definition and its second, then a
  // dependency's definition, then that dependency's extension.
  let ours =
    "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\nschema {\n  query: Query\n}\n";
  let theirs = "schema {\n  query: Point\n}\ntype Point {\n  x: Int\n}\n\
                extend schema {\n  mutation: Point\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(!analysis.is_ok());
  assert_eq!(analysis.diagnostics().error_count(), 3);
  let model = analysis.model();
  // Each item a rule refused holds its error. The first definition, which a label alone points
  // at, holds none.
  assert_eq!(schema_errors(model), [false, true, true, true]);
  // And no definition does: a schema item is no definition's contribution.
  assert!(
    model
      .defs()
      .into_iter()
      .all(|def: DefId| !model.def(def).has_errors())
  );

  // An orphan extension holds its error, and through it the definition it extends does. The
  // definition's own contribution holds none, nor does the member the extension writes, which the
  // extension's target path lies outside of, nor any other definition.
  let ours = "type Query {\n  ok: Int\n}\nextend type ^::geo::Point {\n  label: String\n}\n";
  let theirs = "type Point {\n  x: Int\n}\ntype Line {\n  head: Point\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(!analysis.is_ok());
  assert_eq!(analysis.diagnostics().error_count(), 1);
  let model = analysis.model();
  let point = def(model, "^::geo::Point");
  assert_eq!(contributions(model, point), [(false, false), (true, true)]);
  assert!(model.def(point).has_errors());
  let marked_members: Vec<MemberId> = model
    .def(point)
    .fields()
    .into_iter()
    .filter(|member: &MemberId| model.member(*member).has_errors())
    .collect::<Vec<MemberId>>();
  assert!(marked_members.is_empty(), "{marked_members:?}");
  let marked: Vec<&str> = model
    .defs()
    .into_iter()
    .filter(|id: &DefId| model.def(*id).has_errors())
    .map(|id: DefId| model.def(id).name())
    .collect::<Vec<&str>>();
  assert_eq!(marked, ["Point"]);
}

#[test]
fn long_names_are_quoted_as_excerpts() {
  // A dependency whose name is long defines the schema: the message quotes the ends of the name.
  let name = "p".repeat(4096);
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  );
  let dependency: Package<&'static str> = Package::new(name.as_str(), Layout::Flat, Tier::Std)
    .with_files([SourceFile::new(
      "s.graphqlx",
      "schema {\n  query: Query\n}\n",
    )]);
  let analysis = analyze(&root, &[dependency], &Limits::new());
  assert_eq!(
    messages(&analysis),
    [format!(
      "package `{}…{}` defines the schema, which only the root package defines and extends",
      "p".repeat(64),
      "p".repeat(64)
    )]
  );

  // A root package whose name is long extends a dependency's type whose name is long: the message
  // quotes the ends of each, and no path.
  let root_name = "r".repeat(4096);
  let type_name = "E".repeat(4096);
  let ours = format!("extend type ^::geo::{type_name} {{\n  extra: Int\n}}\n");
  let theirs = format!("type {type_name} {{\n  x: Int\n}}\n");
  let root: Package<&str> = Package::new(root_name.as_str(), Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", ours.as_str())])
    .with_dependencies(["geo"]);
  let geo: Package<&str> = Package::new("geo", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("geo.graphqlx", theirs.as_str())]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert_eq!(
    messages(&analysis),
    [format!(
      "package `{}…{}` extends the type `{}…{}`, which package `geo` defines",
      "r".repeat(64),
      "r".repeat(64),
      "E".repeat(64),
      "E".repeat(64)
    )]
  );
}
