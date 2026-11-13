//! The GraphQLx analyzer's own rules, which `analyze` runs over every supplied package once its
//! phases have written the model: a program defines its schema at most once, and only the root
//! package defines or extends the schema. Each rule says nothing about what a phase already
//! refused, marks what it reports, and quotes a name only as an excerpt.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Contribution, DefId, Diagnostic, DiagnosticKind, Layout, Limits, Model, Package,
    PackageId, Root, RootOperation, SourceFile, SourceId, Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for a second schema definition, which the analyzer shares.
const DUPLICATE: &str = "smear::schema::duplicate-schema-definition";

/// The code for a schema item a package other than the root package writes.
const OUTSIDE: &str = "smear::graphqlx::schema-outside-root";

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
}
