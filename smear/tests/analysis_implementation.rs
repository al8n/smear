//! The GraphQLx analyzer's implementation phase: what each definition says it is with its
//! `implements` entries, and a union with its members, read of what its own package writes. No
//! two entries of a definition, nor two members of a union, are one type.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, Layout, Limits, Package, PackageId, SourceFile, SourceId, Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for an `implements` entry of a type an earlier entry has.
const DUPLICATE: &str = "smear::schema::duplicate-implements-interface";
/// GraphQL's code for a union member of a type an earlier member has.
const MEMBER: &str = "smear::schema::duplicate-union-member";
/// The resolver's code for an `implements` entry that names nothing.
const UNDEFINED_ENTRY: &str = "smear::schema::undefined-implements-interface";
/// The orphan rule's code.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";

/// A package named `name`, `Flat`, at tier `Std`, holding `files`, `(path, text)`.
fn package<'a>(name: &'a str, files: &[(&'a str, &'a str)]) -> Package<&'a str> {
  Package::new(name, Layout::Flat, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'a str, &'a str)| SourceFile::new(path, text)),
  )
}

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  analyze(&package("app", &[("schema.graphqlx", text)]), &[], limits)
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

/// Where the `nth` occurrence, counting from 0, of the name `name` written right after `before` is
/// in `text`, the file `source`: a name that a longer name begins with is not one.
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

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// Each diagnostic's code, in the list's order.
fn codes(analysis: &Analysis) -> Vec<&'static str> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect()
}

/// Asserts that `analysis` says nothing.
fn quiet(analysis: &Analysis) {
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The diagnostic with the code `code` at `at`.
fn the<'a>(analysis: &'a Analysis, code: &str, at: Location) -> &'a Diagnostic {
  analysis
    .diagnostics()
    .iter()
    .find(|diagnostic: &&Diagnostic| {
      diagnostic.code().as_str() == code && diagnostic.primary() == at
    })
    .unwrap_or_else(|| panic!("no `{code}` at {at:?}: {:#?}", analysis.diagnostics()))
}

/// Each label of `diagnostic`, where it is and what it says, in order.
fn labels(diagnostic: &Diagnostic) -> Vec<(Location, &str)> {
  (0..diagnostic.labels())
    .filter_map(|index: usize| diagnostic.label(index))
    .map(|label| (label.location(), label.text()))
    .collect()
}

/// GraphQL's own document: the second `I` is refused, labelled at the first, and nothing else is
/// said. Refuses a report at the first.
#[test]
fn an_interface_implemented_twice_is_refused_at_the_later_entry() {
  let text = "type Query { ok: Int } interface I { a: Int } type T implements I & I { a: Int }";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let second = named(schema, text, "& ", "I", 0);
  assert_eq!(found(&analysis), [(DUPLICATE, second)]);
  let report = the(&analysis, DUPLICATE, second);
  assert_eq!(report.to_string(), "`T` implements `I` twice");
  assert_eq!(
    labels(report),
    [(
      named(schema, text, "implements ", "I", 0),
      "first implemented here"
    )]
  );
}

/// Every later entry of one type is reported, each labelled at the first: `I & I & I` is two. An
/// entry the definition's own package writes in an extension, in another file, is the
/// definition's own, and reported there. Refuses a report at the second entry alone.
#[test]
fn every_later_entry_of_one_type_is_a_duplicate() {
  let text = "type Query {\n  x: Int\n}\ninterface I {\n  a: Int\n}\n\
              type T implements I & I & I {\n  a: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let first = named(schema, text, "implements ", "I", 0);
  let later = [
    named(schema, text, "& ", "I", 0),
    named(schema, text, "& ", "I", 1),
  ];
  assert_eq!(
    found(&analysis),
    [(DUPLICATE, later[0]), (DUPLICATE, later[1])]
  );
  for at in later {
    assert_eq!(
      labels(the(&analysis, DUPLICATE, at)),
      [(first, "first implemented here")]
    );
  }

  let a =
    "type Query {\n  x: Int\n}\ninterface I {\n  a: Int\n}\ntype T implements I {\n  a: Int\n}\n";
  let b = "extend type T implements I\n";
  let root = package("app", &[("a.graphqlx", a), ("b.graphqlx", b)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let (a_file, b_file) = (
    file(&analysis, 3, "a.graphqlx"),
    file(&analysis, 3, "b.graphqlx"),
  );
  let extended = named(b_file, b, "implements ", "I", 0);
  assert_eq!(found(&analysis), [(DUPLICATE, extended)]);
  assert_eq!(
    labels(the(&analysis, DUPLICATE, extended)),
    [(
      named(a_file, a, "implements ", "I", 0),
      "first implemented here"
    )]
  );
}

/// Two instantiations of one interface are two types. Refuses duplicates told by definition.
#[test]
fn two_instantiations_of_one_interface_are_two_types() {
  quiet(&analyze_text(
    "type Query {\n  x: Int\n}\ninterface Cmp<X> {\n  id: ID\n}\n\
     type T implements Cmp<Int> & Cmp<String> {\n  id: ID\n}\n",
    &Limits::new(),
  ));
}

/// GraphQL's own document: the second `A` is refused, labelled at the first; and so is a member
/// the union's own extension writes again.
#[test]
fn a_union_member_written_twice_is_refused_at_the_later() {
  let text = "type Query { ok: Int } type A { a: Int } union U = A | A";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let second = named(schema, text, "| ", "A", 0);
  assert_eq!(found(&analysis), [(MEMBER, second)]);
  let report = the(&analysis, MEMBER, second);
  assert_eq!(
    report.to_string(),
    "the union `U` has `A` as a member twice"
  );
  assert_eq!(
    labels(report),
    [(named(schema, text, "= ", "A", 0), "first a member here")]
  );

  let text = "type Query {\n  ok: Int\n}\ntype A {\n  a: Int\n}\nunion U = A\nextend union U = A\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let extended = named(schema, text, "U = ", "A", 1);
  assert_eq!(found(&analysis), [(MEMBER, extended)]);
  assert_eq!(
    labels(the(&analysis, MEMBER, extended)),
    [(named(schema, text, "U = ", "A", 0), "first a member here")]
  );
}

/// An entry that names nothing is the resolver's alone: compared with nothing, `Missing & Missing`
/// is two `undefined-implements-interface` and no duplicate.
#[test]
fn unresolved_entries_are_no_duplicates() {
  let text = "type Query {\n  x: Int\n}\ntype T implements Missing & Missing {\n  a: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        UNDEFINED_ENTRY,
        named(schema, text, "implements ", "Missing", 0)
      ),
      (UNDEFINED_ENTRY, named(schema, text, "& ", "Missing", 0)),
    ]
  );
}

/// What another package writes into a definition is the orphan rule's: the root package's
/// extension that adds `^::dep::I` to the dependency's `P`, which implements `I` already, is
/// `orphan-extension` alone, and no duplicate. Refuses an orphan's entry read as the definition's.
#[test]
fn what_another_package_writes_is_not_read() {
  let theirs = "interface I {\n  a: Int\n}\ntype P implements I {\n  a: Int\n}\n";
  let ours = "type Query {\n  x: Int\n}\nextend type ^::dep::P implements ^::dep::I\n";
  let root = package("app", &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  assert_eq!(codes(&analysis), [ORPHAN]);
}
