#[test]
fn test_success() {
  let t = trybuild::TestCases::new();
  t.pass("tests/set/success.rs");
}

#[test]
fn test_failure() {
  let t = trybuild::TestCases::new();
  t.compile_fail("tests/set/failure.rs");
}
