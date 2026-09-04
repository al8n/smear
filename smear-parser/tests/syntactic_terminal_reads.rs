//! No read in either syntactic tree folds a terminal scanner stop into an absent outcome.
//!
//! # The property, and why it is a source scan
//!
//! tokora 0.10 ships two shapes of every read that can come back empty. The **blind** one folds
//! three outcomes into one `Ok(None)` — a genuine end of input, a fresh resource-limit trip, and an
//! already-latched poison boundary — and each of its docs says so in the same words: *"`Ok(None)`
//! also covers a terminal stop (limit trip / latched poison boundary); when a decline commits the
//! caller to a different parse, use `try_expect_or_stop`"*
//! (`tokora-0.10.0/src/input/input_ref/try_expect.rs:262`), and, for the raw head read, *"It folds
//! a terminal stop into `Ok(None)` … a production that decides on the answer — 'is there a `{`
//! here?' — will read a halt as a grammar fact and keep going"*
//! (`.../input_ref/peek/mod.rs:83`). The **terminal-aware** one reserves `Ok(None)` for the real
//! end of input and raises the marked end-of-input error otherwise.
//!
//! smear issue #177 is that distinction, and rounds 1 and 2 each closed it over the reads they had
//! looked at rather than over the property: round 1 kept the mark tokora had made, round 2 made it
//! at `next` and at the raw peeks — and Codex round 2 then found three more classes behind
//! `try_expect`, `try_expect_map` and an always-declining `try_expect` helper. Three rounds of
//! naming sites is what a census over the *property* replaces.
//!
//! **A run-time cell cannot hold this.** Each of these reads has to be reached with a live budget
//! refusal at exactly its position to be seen at all — that is what the per-door regressions in
//! `graphql/error/tests/terminal.rs` and its GraphQLx twin do, and it is why they took a
//! measurement per door to place. What no set of them can say is *there is no fourth class*. The
//! blind primitives have names; their absence from these two trees is checkable directly, and it is
//! the only form in which the claim covers reads nobody has written yet.
//!
//! # Two disciplines, borrowed from `smear/tests/lossless_isolation.rs`
//!
//! A grep returning zero is not evidence of absence until the pattern has matched something, so
//! every forbidden pattern runs with a **positive control** in a tree where it legitimately occurs
//! — the lossless substrate, whose defence against a stop is the poison boundary and the document
//! root's own verdict rather than the read's return type. And the scan **panics** on a directory
//! that does not exist or holds no `.rs` file, so a mistyped path can never be the thing that made
//! a count zero.

#![cfg(any(feature = "graphql", feature = "graphqlx"))]
#![allow(missing_docs)]

use std::path::{Path, PathBuf};

/// Every `.rs` file under `dir`, relative to this crate's manifest, as `(relative path, text)`.
///
/// Panics on a missing directory or one with no Rust in it: a scan that silently reads nothing
/// reports every absence as satisfied.
fn rust_files(dir: &str) -> Vec<(String, String)> {
  let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(dir);
  assert!(
    root.is_dir(),
    "{dir}: not a directory — a scan over nothing reports every absence as satisfied"
  );

  let mut out = Vec::new();
  let mut stack = vec![root.clone()];
  while let Some(current) = stack.pop() {
    for entry in std::fs::read_dir(&current).unwrap_or_else(|e| panic!("{current:?}: {e}")) {
      let path = entry.unwrap_or_else(|e| panic!("{current:?}: {e}")).path();
      if path.is_dir() {
        stack.push(path);
      } else if path.extension().is_some_and(|e| e == "rs") && !is_test_file(&path) {
        let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path:?}: {e}"));
        out.push((relative(&root, &path, dir), text));
      }
    }
  }
  assert!(!out.is_empty(), "{dir}: no `.rs` file found");
  out.sort();
  out
}

/// Test modules are excluded, and the exclusion is the scan's one real bound.
///
/// The property is about **productions**: a cell may read the input any way it likes, and several
/// do — a driver that pulls one error out of a container writes `into_iter().next()`, which is
/// `Iterator`'s method and not the input's. Including them would make this census fail on 81 lines
/// that are not reads at all. This crate's convention puts unit tests in a sibling `tests.rs` or a
/// `tests/` directory (see `rust-test-module-layout`), so the exclusion is a path rule rather than
/// a `#[cfg(test)]` parse.
fn is_test_file(path: &Path) -> bool {
  path.components().any(|c| {
    let s = c.as_os_str().to_string_lossy();
    s == "tests" || s == "tests.rs" || s.ends_with("_tests.rs")
  })
}

fn relative(root: &Path, path: &Path, dir: &str) -> String {
  let tail = path
    .strip_prefix(root)
    .unwrap_or_else(|_| panic!("{path:?} is not under {root:?}"));
  format!("{dir}/{}", tail.display())
}

/// Does this line hold a call rather than prose?
///
/// A doc comment naming `try_expect` is the whole reason this file exists — the module docs above
/// name every forbidden pattern — so a substring scan that counted them would fail on itself. A
/// line whose first non-space characters open a comment is prose; anything else is code. That is
/// coarser than a token walk and it is the right side of coarse: it can only ever count *more*
/// than it should, and this census asserts a count of zero.
fn is_code(line: &str) -> bool {
  let t = line.trim_start();
  !(t.starts_with("//") || t.starts_with("/*") || t.starts_with('*'))
}

/// Every code occurrence of `pattern` under `dir`, as `(file, line number, line)`.
///
/// A file that never names [`InputRef`] is skipped for the `next` patterns alone, and that is the
/// scan's second bound. `.next()?` is `Iterator`'s method as well as the input's, and
/// `lossless/project.rs` — a walk over a **finished** green tree, with no `InputRef` anywhere in it
/// — writes `self.raw.next()?` over rowan's child iterator. A file holding no `InputRef` cannot
/// hold a read of the input, so the skip cannot hide one; the other six patterns are the input's
/// alone and are counted in every file.
fn occurrences(dir: &str, pattern: &str) -> Vec<(String, usize, String)> {
  let mut hits = Vec::new();
  for (file, text) in rust_files(dir) {
    if pattern.contains("next()") && !text.contains("InputRef") {
      continue;
    }
    for (n, line) in text.lines().enumerate() {
      if is_code(line) && line.contains(pattern) {
        hits.push((file.clone(), n + 1, line.trim().to_string()));
      }
    }
  }
  hits
}

/// The blind reads, each paired with what a syntactic production must use instead.
///
/// Derived from tokora's own API rather than from the defects that were found: **every primitive
/// that ships an `_or_stop` twin, plus every raw head read the crate documents as folding a stop.**
/// The twins are `next_or_stop` (`input_ref/mod.rs:4133`), `try_expect_or_stop` (`:323`),
/// `try_expect_map_or_stop` (`:628`) and `try_expect_take_or_stop` (`:954`) in `try_expect.rs`; the
/// terminal-aware head readers are `peek_map`, `peek_head_map`, `head_satisfies` and `peek_kind`
/// in `peek/mod.rs`.
///
/// `try_expect_and_then` has no `_or_stop` twin and carries the same warning, pointing at
/// `try_expect_or_stop`; it is forbidden here for that reason rather than because a twin exists.
/// The generated `try_expect_<punct>` family forwards to `try_expect` and inherits the warning
/// verbatim, so the bare prefix covers it — `try_expect_or_stop` and `try_expect_map_or_stop` are
/// subtracted below by matching on the open parenthesis.
/// `next` is matched on its receiver, `inp.next()`, and on the fallible form `.next()?` — two
/// patterns for one method, because bare `.next()` is also `Iterator`'s and this crate calls that
/// one legitimately. Both trees bind the input as `inp` at every production, and a read that
/// escapes both spellings would have to be a differently-named receiver on a `Result` that is not
/// `?`-propagated. The other seven names are the input's alone.
const BLIND: &[(&str, &str)] = &[
  ("inp.next()", "next_or_stop"),
  (".next()?", "next_or_stop"),
  (".try_expect(", "try_expect_or_stop"),
  (".try_expect_map(", "try_expect_map_or_stop"),
  (".try_expect_take(", "try_expect_take_or_stop"),
  (
    ".try_expect_and_then(",
    "try_expect_or_stop / peek_head_map",
  ),
  (".peek_one(", "peek_kind / head_satisfies / peek_head_map"),
  (".peek::<", "peek_map / peek_head_map"),
  (".peek_with_emitter::<", "peek_map / peek_head_map"),
];

/// Every tree the property is about.
///
/// **The lossless trees and the shared token combinators are in here, and that is a decision
/// rather than a sweep.** They were out of the first version because their defence against a stop
/// is a different one — a refusal latches tokora's poison boundary, `root_turn` reads the trip
/// witness beside `MaybeTerminal`, the drain refuses to cross, and the door mints its own
/// report — and because smear issue #177's round 1 measured the shipped lossless `Parse`
/// byte-identical across 2 239 door/dialect/document/ceiling configurations. Neither is a reason
/// to leave the read blind. **An exception in a substrate is a clearance every later reader has to
/// re-derive**, and re-deriving it means re-establishing all four of those defences from scratch;
/// the value being right one layer earlier costs nothing and removes the question. None of those
/// defences moved.
///
/// `src/combinator/token` is here for the same reason at one more remove: it is shared by both
/// dialects, so a blind read in it is a blind read in every production that uses the atom.
const SYNTACTIC: &[&str] = &[
  "src/graphql/syntactic",
  "src/graphqlx/syntactic",
  "src/lossless",
  "src/graphql/lossless",
  "src/graphqlx/lossless",
  "src/combinator/token",
];

/// The positive control: the one module that legitimately keeps a blind read, so a zero above is a
/// fact about the watched trees rather than about this scan.
///
/// # Why `extent_start` stays blind, and it is the only one
///
/// `combinator/extent.rs`'s `extent_start` peeks one token to learn where the node about to be
/// parsed *begins*. Three things make it the exception:
///
/// * **It decides nothing.** Its answer is an offset, not a grammar fact, and its `None` arm is a
///   documented anchor — "at end of input there is no next token, and the answer is the current
///   offset". Nothing branches on it; no production takes a different shape because of what it
///   returned.
/// * **The head it looks at has already been read terminal-aware.** Its own module docs record
///   that "every dispatching production in both dialects already peeks its own head before this
///   runs", and since smear issue #177's rounds 2 and 3 those peeks are `peek_head_map` and
///   `peek_kind`. A stop at that head has raised before this function is reached.
/// * **Each of its 39 callers follows it with a committed read**, which is terminal-aware, so a
///   stop reaches the caller from there — with a span this helper would have anchored at the same
///   offset either way. The module already records that no production in either dialect succeeds
///   without consuming a token.
///
/// Against that, making it terminal-aware puts a `From<UnexpectedEot>` bound on a span helper that
/// is generic over `Completeness`, and propagates it through thirty-nine signatures, for a value
/// that routes nothing.
const CONTROL: &str = "src/combinator";

/// A synthetic source holding one occurrence of every forbidden pattern.
///
/// The tree control above can only prove the **one** pattern it still contains. This proves the
/// other seven match text of the shape they claim to watch — which is the half that would rot
/// silently, because a typo in a pattern nobody's tree contains any more reads exactly like a clean
/// tree. It is deliberately not valid Rust: what is under test is the matcher, not a parse.
const FIXTURE: &str = r#"
  fn f(inp: &mut InputRef<'_, '_, L, Ctx, Lang>) {
  let a = inp.next()?;
  let b = something.next()?;
  let c = inp.try_expect(|t| t.data.is_ident())?;
  let d = inp.try_expect_map(|t| Some(t))?;
  let e = inp.try_expect_take(|t| Some(t))?;
  let f = inp.try_expect_and_then(|t| Some(Ok(t)))?;
  let g = inp.peek_one()?;
  let h = inp.peek::<U1>()?;
  let i = inp.peek_with_emitter::<U1>()?;
  }
"#;

#[test]
fn no_syntactic_read_folds_a_terminal_stop_into_an_absent_outcome() {
  let mut findings = Vec::new();
  let mut checked = 0usize;

  for dir in SYNTACTIC {
    for (blind, instead) in BLIND {
      for (file, line, text) in occurrences(dir, blind) {
        findings.push(format!(
          "{file}:{line}: `{blind}` folds a terminal scanner stop into an absent outcome — use \
           `{instead}`\n    {text}"
        ));
      }
      checked += 1;
    }
  }

  assert_eq!(
    checked,
    SYNTACTIC.len() * BLIND.len(),
    "the pattern set collapsed"
  );
  assert!(
    findings.is_empty(),
    "{} blind read(s) in the syntactic trees — smear issue #177:\n{}",
    findings.len(),
    findings.join("\n")
  );
}

#[test]
fn the_forbidden_patterns_match_rust_that_exists() {
  // Half one: every pattern matches the shape it claims to watch. A typo in a pattern that no
  // tree contains any more is invisible to the census above and visible here.
  let missed: Vec<&str> = BLIND
    .iter()
    .map(|(p, _)| *p)
    .filter(|p| !FIXTURE.lines().any(|l| is_code(l) && l.contains(p)))
    .collect();
  assert!(
    missed.is_empty(),
    "these patterns matched nothing in the fixture, so their zero in the census means nothing: \
     {missed:?}"
  );

  // Half two: the scan reads real files and finds a real blind read in the one module that keeps
  // one. If `extent_start` is ever repaired too, this fails and says so rather than going quietly
  // green over a census with no control left.
  let hits = occurrences(CONTROL, ".peek::<");
  assert!(
    hits.iter().any(|(f, _, _)| f.ends_with("extent.rs")),
    "`{CONTROL}/extent.rs` no longer holds the raw head peek this census's tree-level control \
     rests on. Either `extent_start` was repaired too — in which case fold it into the watched set \
     and retire this half — or the scan stopped reading it. Found: {hits:?}"
  );
}

#[test]
fn the_watched_trees_read_through_the_terminal_aware_primitives() {
  // The other half of the same claim: the blind names are absent because the reads MOVED, not
  // because the productions went away or the scan is looking at the wrong directory. A count, not
  // a boolean, so a sweep that deletes half the reads is a failure here rather than an improvement.
  let mut total = 0usize;
  for dir in SYNTACTIC {
    for aware in [
      ".next_or_stop(",
      ".try_expect_or_stop(",
      ".try_expect_map_or_stop(",
      ".peek_head_map(",
      ".head_satisfies(",
      ".peek_kind(",
    ] {
      total += occurrences(dir, aware).len();
    }
  }
  assert!(
    total >= 70,
    "only {total} terminal-aware reads across the watched trees; the census that produced this \
     floor counted 34 `next_or_stop`, 21 `try_expect*_or_stop`, 20 `peek_head_map` and the head \
     readers beside them"
  );
}
