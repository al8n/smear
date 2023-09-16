#[test]
fn test_success() {
  let t = trybuild::TestCases::new();
  t.pass("tests/directive/success.rs");
}

#[test]
fn test_failure() {
  let t = trybuild::TestCases::new();
  t.compile_fail("tests/directive/failure.rs");
}
