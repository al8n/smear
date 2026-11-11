//! The GraphQLx analyzer's diagnostics: the public builder, the contract its diagnostics answer,
//! and the one total order a list of them keeps.
//!
//! Each level of the order has a test of its own, so a level that stops deciding fails by name.
//! Each test collects its two diagnostics in both orders: the order a list keeps is a property of
//! the diagnostics, never of the order they arrived in.
#![cfg(all(feature = "alloc", feature = "analysis"))]

use smear::{
  analysis::graphqlx::{Diagnostic, Diagnostics},
  diagnostic::{Code, Diagnose, DiagnoseExt, Location, Severity},
  lexer::tokora::SimpleSpan,
};

/// A consumer's code: the builder takes any.
const CODE: Code = Code::new("ig::test::finding");

/// The bytes `start..end` of source `source`.
fn at(source: u32, start: usize, end: usize) -> Location {
  Location::new(source, SimpleSpan::new(start, end))
}

/// The list's diagnostics, in its order.
fn listed(list: &Diagnostics) -> Vec<Diagnostic> {
  list.iter().cloned().collect::<Vec<Diagnostic>>()
}

/// Asserts that `first` comes before `second`, whichever of the two is reported first.
fn ordered(first: Diagnostic, second: Diagnostic) {
  assert_ne!(first, second, "the two must differ for the order to decide");
  let forward: Diagnostics = [first.clone(), second.clone()].into_iter().collect();
  let backward: Diagnostics = [second.clone(), first.clone()].into_iter().collect();
  let expected: Vec<Diagnostic> = vec![first, second];
  assert_eq!(listed(&forward), expected, "reported in order");
  assert_eq!(listed(&backward), expected, "reported in reverse");
}

#[test]
fn the_builder_starts_from_an_error_that_says_nothing() {
  let diagnostic = Diagnostic::builder(CODE, Location::entire(0)).build();
  assert_eq!(diagnostic.code(), CODE);
  assert_eq!(diagnostic.severity(), Severity::Error);
  assert_eq!(diagnostic.primary(), Location::entire(0));
  assert_eq!(diagnostic.primary_label(), None);
  assert_eq!(diagnostic.labels(), 0);
  assert_eq!(diagnostic.help(), None);
  assert_eq!(diagnostic.path_segments(), 0);
  assert_eq!(diagnostic.to_string(), "");
}

/// A consumer reports with its own code at a source id of its own, and the diagnostic renders
/// through tokora's contract exactly as smear's do, through erasure included.
#[test]
fn a_consumer_diagnostic_answers_the_contract() {
  let tag = 7;
  let manifest = "wire.toml";
  let diagnostic = Diagnostic::builder(Code::new("ig::wire::duplicate-tag"), at(1_000, 4, 9))
    .severity(Severity::Warning)
    .message(format!("tag {tag} is used twice"))
    .primary_label("used again here")
    .label(at(1_000, 0, 3), "first used here")
    .label(Location::entire(1_001), format!("declared in {manifest}"))
    .help("give each field a tag of its own")
    .build();

  let erased: &dyn Diagnose = &diagnostic;
  assert_eq!(erased.code().as_str(), "ig::wire::duplicate-tag");
  assert_eq!(erased.severity(), Severity::Warning);
  assert_eq!(erased.primary(), at(1_000, 4, 9));
  assert_eq!(erased.primary_label(), Some("used again here"));
  assert_eq!(erased.labels(), 2);
  let labels: Vec<(Location, &str)> = erased
    .labels_iter()
    .map(|label| (label.location(), label.text()))
    .collect();
  assert_eq!(
    labels,
    [
      (at(1_000, 0, 3), "first used here"),
      (Location::entire(1_001), "declared in wire.toml"),
    ]
  );
  assert!(erased.label(2).is_none());
  assert_eq!(erased.path_segments(), 0);
  assert!(erased.path_segment(0).is_none());
  assert_eq!(erased.help(), Some("give each field a tag of its own"));
  assert_eq!(erased.to_string(), "tag 7 is used twice");

  let error: &dyn core::error::Error = &diagnostic;
  assert_eq!(error.to_string(), "tag 7 is used twice");
}

#[test]
fn the_order_is_by_source_id_first() {
  ordered(
    Diagnostic::builder(CODE, at(1, 50, 60)).build(),
    Diagnostic::builder(CODE, at(2, 0, 1)).build(),
  );
}

#[test]
fn a_whole_source_comes_before_every_span_of_it() {
  ordered(
    Diagnostic::builder(CODE, Location::entire(3)).build(),
    Diagnostic::builder(CODE, at(3, 0, 0)).build(),
  );
  // But not before an earlier source's spans.
  ordered(
    Diagnostic::builder(CODE, at(2, 9, 9)).build(),
    Diagnostic::builder(CODE, Location::entire(3)).build(),
  );
}

#[test]
fn then_by_span_start() {
  ordered(
    Diagnostic::builder(CODE, at(3, 2, 20)).build(),
    Diagnostic::builder(CODE, at(3, 5, 9)).build(),
  );
}

#[test]
fn then_by_span_end() {
  ordered(
    Diagnostic::builder(CODE, at(3, 5, 6)).build(),
    Diagnostic::builder(CODE, at(3, 5, 9)).build(),
  );
}

#[test]
fn then_by_code() {
  ordered(
    Diagnostic::builder(Code::new("ig::test::a"), at(3, 5, 9)).build(),
    Diagnostic::builder(Code::new("ig::test::b"), at(3, 5, 9)).build(),
  );
}

#[test]
fn then_by_severity_errors_first() {
  let at_severity = |severity: Severity| -> Diagnostic {
    Diagnostic::builder(CODE, at(3, 5, 9))
      .severity(severity)
      .build()
  };
  ordered(at_severity(Severity::Error), at_severity(Severity::Warning));
  ordered(
    at_severity(Severity::Warning),
    at_severity(Severity::Advice),
  );
}

#[test]
fn then_by_message() {
  ordered(
    Diagnostic::builder(CODE, at(3, 5, 9))
      .message("alpha")
      .build(),
    Diagnostic::builder(CODE, at(3, 5, 9))
      .message("beta")
      .build(),
  );
}

#[test]
fn then_by_primary_label_absent_first() {
  let none = Diagnostic::builder(CODE, at(3, 5, 9)).build();
  let alpha = Diagnostic::builder(CODE, at(3, 5, 9))
    .primary_label("alpha")
    .build();
  let beta = Diagnostic::builder(CODE, at(3, 5, 9))
    .primary_label("beta")
    .build();
  ordered(none, alpha.clone());
  ordered(alpha, beta);
}

#[test]
fn then_by_help_absent_first() {
  let none = Diagnostic::builder(CODE, at(3, 5, 9)).build();
  let alpha = Diagnostic::builder(CODE, at(3, 5, 9)).help("alpha").build();
  let beta = Diagnostic::builder(CODE, at(3, 5, 9)).help("beta").build();
  ordered(none, alpha.clone());
  ordered(alpha, beta);
}

#[test]
fn then_by_labels_label_by_label() {
  let none = Diagnostic::builder(CODE, at(3, 5, 9)).build();
  let one = Diagnostic::builder(CODE, at(3, 5, 9))
    .label(at(3, 0, 1), "x")
    .build();
  let later = Diagnostic::builder(CODE, at(3, 5, 9))
    .label(at(3, 2, 3), "x")
    .build();
  let other_text = Diagnostic::builder(CODE, at(3, 5, 9))
    .label(at(3, 0, 1), "y")
    .build();
  let two = Diagnostic::builder(CODE, at(3, 5, 9))
    .label(at(3, 0, 1), "x")
    .label(at(0, 0, 1), "x")
    .build();
  // A list that begins another comes first.
  ordered(none, one.clone());
  ordered(one.clone(), two);
  // Then each label by its location, and then by its text.
  ordered(one.clone(), later);
  ordered(one, other_text);
}

#[test]
fn only_errors_are_counted() {
  let list: Diagnostics = [
    Diagnostic::builder(CODE, at(0, 0, 1))
      .severity(Severity::Warning)
      .build(),
    Diagnostic::builder(CODE, at(0, 1, 2))
      .severity(Severity::Advice)
      .build(),
  ]
  .into_iter()
  .collect();
  assert_eq!(list.len(), 2);
  assert_eq!(list.error_count(), 0);
  assert!(!list.has_errors());
  assert!(!list.truncated());
}

#[test]
fn merge_keeps_both_lists_whole_in_one_order_and_adds_their_counts() {
  let mut ours: Diagnostics = [
    Diagnostic::builder(CODE, at(5, 0, 1)).build(),
    Diagnostic::builder(CODE, at(1, 0, 1))
      .severity(Severity::Warning)
      .build(),
  ]
  .into_iter()
  .collect();
  let theirs: Diagnostics = [
    Diagnostic::builder(Code::new("ig::test::theirs"), at(3, 0, 1)).build(),
    Diagnostic::builder(Code::new("ig::test::theirs"), at(0, 0, 1)).build(),
  ]
  .into_iter()
  .collect();
  assert_eq!(ours.error_count(), 1);
  assert_eq!(theirs.error_count(), 2);

  ours.merge(theirs);
  assert_eq!(ours.len(), 4);
  assert_eq!(ours.error_count(), 3);
  assert!(ours.has_errors());
  assert!(!ours.truncated());
  let mut sources: Vec<u32> = Vec::new();
  for diagnostic in &ours {
    sources.push(diagnostic.primary().source());
  }
  assert_eq!(sources, [0, 1, 3, 5]);

  let owned: Vec<Diagnostic> = ours.clone().into_iter().collect();
  assert_eq!(owned, listed(&ours));
}
