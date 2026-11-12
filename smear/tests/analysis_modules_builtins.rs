//! The GraphQLx analyzer's built-in library: `core`, `alloc` and `std`, one embedded file each,
//! parsed on every analysis exactly as a supplied file is.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::analysis::graphqlx::{
  Layout, Limits, Package, PackageId, PackageKind, SourceId, Tier, analyze,
};

/// An analysis of a root package that holds no file is an analysis of the built-in library alone,
/// and it reports nothing: every embedded file parses, and each is answered by `bind`.
#[test]
fn the_built_in_library_analyzes_with_no_diagnostic() {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  let model = analysis.model();
  let texts = analysis.sources().bind(&root, &[]);
  for (index, name) in ["core", "alloc", "std"].into_iter().enumerate() {
    let package = model.package(PackageId::new(
      u32::try_from(index).expect("three built-in packages"),
    ));
    assert_eq!(package.name(), name);
    assert_eq!(package.kind(), PackageKind::BuiltIn);
    let sources: Vec<SourceId> = package.sources().into_iter().collect();
    assert_eq!(sources.len(), 2, "{name}: its package source and one file");
    let file = analysis.sources().source(sources[1]);
    assert_eq!(file.path(), "lib.graphqlx");
    let text = texts.text(sources[1]).expect("smear's own text");
    assert_eq!(text.len(), file.len());
    assert!(!text.is_empty());
  }
}
