//! The selftests, as `cargo test` targets as well as a `--selftest` flag.
//!
//! `ci/miri_scope.py` runs its selftest first inside the job it guards, before anything expensive,
//! and the census does the same — the CI step calls `--selftest` before the census proper. Running
//! the identical cases here as well is what puts them in `cargo test --workspace --all-features`,
//! so a change to either rule that breaks the discrimination fails on a developer's machine and
//! not only in the job.

use std::{
  fs,
  path::{Path, PathBuf},
};

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

/// The gate, and not the reconciliation behind it.
///
/// Every other test here asks whether a function returns a finding. What CI runs is
/// [`roots::verdict`] over [`roots::detect`] of the whole tree, turned into an exit code by `main`
/// — and the two holes this census has had were both invisible to that pair, not to a case. So
/// this plants a caller into a copy of the real tree and reads the verdict: the plain undeclared
/// one the table exists to catch, and the macro-generated alias that walked past the table without
/// leaving a mark.
///
/// The copy is required to be green before and after. A plant against a tree that was already red
/// proves nothing, and a plant whose removal leaves it red proves the wrong thing.
#[test]
fn a_planted_caller_makes_the_verdict_red() {
  let repository = roots::repository_root().expect("the repository this crate is a member of");
  let copy = Scratch::of(&repository);
  let planted = copy.tree().join("planted_by_the_census_test.rs");

  let before = roots::detect(copy.repository());
  assert!(
    roots::verdict(&before),
    "the copy this plant is measured against is red before anything was planted:\n  {}",
    before.findings.join("\n  ")
  );

  for (what, source, names) in [
    (
      "an undeclared caller",
      "fn planted() { crate::lossless::depth::root_turn(inp, stop, entry) }",
      "is not in DECLARED",
    ),
    (
      "a caller reached through a macro-generated alias",
      "macro_rules! bind {\n  ($alias:ident) => { use crate::lossless::depth::root_turn as \
       $alias; };\n}\nbind!(turn);\nfn planted() { turn(inp, stop, entry) }",
      "followed by `$alias`",
    ),
  ] {
    fs::write(&planted, source).expect("the planted file is writable");
    let report = roots::detect(copy.repository());
    assert!(
      !roots::verdict(&report),
      "{what} was planted in the tree and the census still passes — this is the shape it is for"
    );
    assert!(
      report.findings.iter().any(|finding| {
        finding.contains("planted_by_the_census_test.rs") && finding.contains(names)
      }),
      "{what}: the verdict is red, but no finding names the plant:\n  {}",
      report.findings.join("\n  ")
    );
    fs::remove_file(&planted).expect("the planted file is removable");
  }

  let after = roots::detect(copy.repository());
  assert!(
    roots::verdict(&after),
    "the copy stayed red once the plants were removed, so the red above was not theirs:\n  {}",
    after.findings.join("\n  ")
  );
}

/// A copy of the tree the census reads, under a temporary directory that goes away with it.
///
/// Planting into the real tree would be a race with anything else reading it, and a failed
/// assertion would leave the plant behind.
struct Scratch(PathBuf);

impl Scratch {
  /// Copies every `.rs` file under `repository`'s census root, mounted at the same relative path
  /// so that findings — and [`roots::DECLARED`]'s keys — read the same as against the original.
  fn of(repository: &Path) -> Self {
    let at = std::env::temp_dir().join(format!("source-census-plant-{}", std::process::id()));
    let _ = fs::remove_dir_all(&at);
    let scratch = Scratch(at);
    copy_rust(&repository.join(roots::ROOT), &scratch.tree());
    scratch
  }

  /// What [`roots::detect`] is pointed at.
  fn repository(&self) -> &Path {
    &self.0
  }

  /// The copied census root, which is where a plant goes.
  fn tree(&self) -> PathBuf {
    self.0.join(roots::ROOT)
  }
}

impl Drop for Scratch {
  fn drop(&mut self) {
    let _ = fs::remove_dir_all(&self.0);
  }
}

/// Every `.rs` file under `from`, recreated under `to`. The census reads no other extension.
fn copy_rust(from: &Path, to: &Path) {
  fs::create_dir_all(to).unwrap_or_else(|e| panic!("{}: {e}", to.display()));
  let entries = fs::read_dir(from).unwrap_or_else(|e| panic!("{}: {e}", from.display()));
  for entry in entries {
    let path = entry.expect("a readable directory entry").path();
    let name = path
      .file_name()
      .expect("a named directory entry")
      .to_owned();
    if path.is_dir() {
      copy_rust(&path, &to.join(name));
    } else if path.extension().is_some_and(|extension| extension == "rs") {
      fs::copy(&path, to.join(name)).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    }
  }
}
