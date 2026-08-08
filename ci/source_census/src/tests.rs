//! The selftest, as a `cargo test` target as well as a `--selftest` flag.
//!
//! `ci/miri_scope.py` runs its selftest first inside the job it guards, before anything expensive,
//! and the census does the same — the CI step calls `--selftest` before the census proper. Running
//! the identical cases here as well is what puts them in `cargo test --workspace --all-features`,
//! so a change to the rule that breaks the discrimination fails on a developer's machine and not
//! only in the job.

use crate::selftest;

#[test]
fn the_rule_discriminates() {
  match selftest::run() {
    Ok(cases) => assert!(cases > 0, "the selftest ran no cases"),
    Err(problems) => panic!("{}", problems.join("\n")),
  }
}
