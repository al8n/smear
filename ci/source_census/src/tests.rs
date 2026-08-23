//! The selftests, as `cargo test` targets as well as a `--selftest` flag.
//!
//! `ci/miri_scope.py` runs its selftest first inside the job it guards, before anything expensive,
//! and the census does the same — the CI step calls `--selftest` before the census proper. Running
//! the identical cases here as well is what puts them in `cargo test --workspace --all-features`,
//! so a change to either rule that breaks the discrimination fails on a developer's machine and
//! not only in the job.

use crate::{diagnose, roots, selftest};

#[test]
fn the_rule_discriminates() {
  match selftest::run() {
    Ok(cases) => assert!(cases > 0, "the selftest ran no cases"),
    Err(problems) => panic!("{}", problems.join("\n")),
  }
}

#[test]
fn the_contract_checks_discriminate() {
  match diagnose::selftest::run() {
    Ok(cases) => assert!(cases > 0, "the contract selftest ran no cases"),
    Err(problems) => panic!("{}", problems.join("\n")),
  }
}

#[test]
fn the_caller_scan_reads_what_its_header_claims() {
  match roots::selftest() {
    Ok(cases) => assert!(cases > 0, "the caller-census selftest ran no cases"),
    Err(problems) => panic!("{}", problems.join("\n")),
  }
}

/// The caller census against the real tree, so it fails on a developer's machine and not only in
/// the job. Unlike the two selftests above this one reads the repository, and `repository_root`
/// is what makes it work from either the crate directory or the repository root.
#[test]
fn the_declared_callers_are_the_callers() {
  let repository = roots::repository_root().expect("the repository this crate is a member of");
  let report = roots::detect(&repository);
  assert!(
    report.findings.is_empty(),
    "the root-verdict caller census disagrees with `roots::DECLARED`:\n  {}",
    report.findings.join("\n  ")
  );
}
