#![allow(clippy::all, warnings)]

#[test]
fn test_long_fail() {
  let t = trybuild::TestCases::new();
  t.compile_fail("tests/boolean_directive/long_fail.rs");
}

#[test]
fn test_long_success() {
  let t = trybuild::TestCases::new();
  t.pass("tests/boolean_directive/long_success.rs");
}

#[test]
fn test_short_fail() {
  let t = trybuild::TestCases::new();
  t.compile_fail("tests/boolean_directive/short_fail.rs");
}

#[test]
fn test_short_success() {
  let t = trybuild::TestCases::new();
  t.pass("tests/boolean_directive/short_success.rs");
}

#[test]
fn test_alias_fail() {
  let t = trybuild::TestCases::new();
  t.compile_fail("tests/boolean_directive/aliases_fail.rs");
}

#[test]
fn test_alias_success() {
  let t = trybuild::TestCases::new();
  t.pass("tests/boolean_directive/aliases_success.rs");
}

#[test]
fn test_default() {
  #[derive(smear::BooleanDirective)]
  #[smear(short)]
  struct IndexedFalse;

  assert_eq!(IndexedFalseDirective::default(), false);

  #[derive(smear::BooleanDirective)]
  #[smear(short)]
  struct IndexedTrue;

  assert_eq!(IndexedTrueDirective::default(), true);
}

#[test]
fn test_possible_names() {
  use smear::NamedDiagnosticable;

  #[derive(smear::BooleanDirective)]
  #[smear(aliases("a", "ab", "_d", "_1", what, my_alias, good))]
  struct Indexed;

  assert_eq!(
    IndexedDirective::possible_names(),
    &["indexed", "a", "ab", "_d", "_1", "what", "my_alias", "good"]
  );
}
