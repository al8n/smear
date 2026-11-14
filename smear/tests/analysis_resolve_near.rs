//! The GraphQLx analyzer's near misses: the "did you mean" of a name that does not resolve, its
//! budget, and the names it does not search for.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, DiagnosticKind, Layout, Limits, Package, SourceFile, Tier, analyze,
  },
  diagnostic::Diagnose,
};

/// A package named `name` of the `Directory` layout at tier `Std`, holding `files`,
/// `(path, text)`.
fn directory(name: &str, files: &[(&'static str, &'static str)]) -> Package<&'static str> {
  Package::new(name, Layout::Directory, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// GraphQL's code for a schema with no query root. The programs of this file are parts of a
/// program and say nothing of the schema, so each reading below leaves this code out, as a
/// consumer that analyzes a library waives it.
const NO_QUERY_ROOT: &str = "smear::schema::missing-query-root-operation-type";

/// Each diagnostic's code and help, in the list's order, but for the missing query root.
fn helps(analysis: &Analysis) -> Vec<(&'static str, String)> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() != NO_QUERY_ROOT)
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        String::from(diagnostic.help().unwrap_or("")),
      )
    })
    .collect()
}

/// The code of `kind`.
fn code(kind: DiagnosticKind) -> &'static str {
  kind.code().as_str()
}

/// A root package whose root module misspells a package, a declaration of a module, and a child
/// module, beside the package it means.
fn misspelt() -> (Package<&'static str>, Package<&'static str>) {
  let geography = directory("geography", &[("lib.graphqlx", "scalar Point\n")]);
  let root = directory(
    "app",
    &[
      (
        "schema.graphqlx",
        "use ^::geograpy::Point\nuse ~::lib::Thnig\nuse medai::Image\n",
      ),
      (
        "lib/a.graphqlx",
        "scalar Thing\ndirective @marked on FIELD_DEFINITION\n",
      ),
      ("media/m.graphqlx", "scalar Image\n"),
    ],
  )
  .with_dependencies(["geography"]);
  (root, geography)
}

#[test]
fn a_name_that_does_not_resolve_is_offered_the_nearest_visible_one() {
  let (root, geography) = misspelt();
  let analysis = analyze(&root, &[geography], &Limits::new());
  let found = helps(&analysis);
  assert_eq!(found.len(), 3, "{:#?}", analysis.diagnostics());
  let expected = [
    (
      code(DiagnosticKind::UnknownPackage),
      "did you mean `geography`?",
    ),
    (
      code(DiagnosticKind::UnresolvedPath),
      "did you mean `Thing`?",
    ),
    (
      code(DiagnosticKind::UnresolvedPath),
      "did you mean `media`?",
    ),
  ];
  for ((code, help), (want, suggestion)) in found.iter().zip(expected) {
    assert_eq!(*code, want, "{help}");
    assert!(help.starts_with(suggestion), "{help}");
  }
}

#[test]
fn no_suggestion_is_offered_once_the_budget_is_spent() {
  let (root, geography) = misspelt();
  let limits = Limits::new().with_max_near_miss_comparisons(0);
  let analysis = analyze(&root, &[geography], &limits);
  let found = helps(&analysis);
  assert_eq!(found.len(), 3, "{:#?}", analysis.diagnostics());
  for (_, help) in &found {
    assert!(!help.is_empty());
    assert!(!help.contains("did you mean"), "{help}");
  }
}

#[test]
fn a_long_name_is_not_compared() {
  // Seventy letters, one of them changed: past the length a near miss is searched for.
  let long = "use ~::lib::AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB\n\
              use ~::lib::Shortnaem\n";
  let lib = "scalar AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n\
             scalar Shortname\n";
  let root = directory("app", &[("schema.graphqlx", long), ("lib/a.graphqlx", lib)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let found = helps(&analysis);
  assert_eq!(found.len(), 2, "{:#?}", analysis.diagnostics());
  assert!(!found[0].1.contains("did you mean"), "{}", found[0].1);
  assert!(
    found[1].1.starts_with("did you mean `Shortname`?"),
    "{}",
    found[1].1
  );
}
