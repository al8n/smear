#![cfg(all(feature = "rowan", feature = "graphqlx"))]

//! Gate 6 — cross-dialect isolation.
//!
//! **The crate's whole two-dialect design rests on this separation and nothing else enforces it.**
//! `graphqlx/lossless/` reaches into `graphql/` nowhere, `graphql/lossless/` reaches into
//! `graphqlx/` nowhere, and `lossless/` — the substrate both stand on — names no dialect type at
//! all. Every one of those is true today by discipline, and discipline is what a hurried refactor
//! spends first.
//!
//! # This is a source scan, and that is not a compromise
//!
//! The property is about **imports**, and no run-time value carries it: a wrapper typed over the
//! wrong dialect either fails to compile or quietly answers for a tree it should never have seen,
//! and neither shows up as a value some assertion can read. So the assertions read the sources.
//!
//! Two disciplines make a source scan worth running. Every "this pattern does not occur" runs with
//! a **positive control** — the same pattern, counted in a tree where it must occur — because a
//! grep returning zero is not evidence of absence until that pattern has matched something. And
//! [`references`] **panics** on a directory that does not exist or holds no `.rs` file, so a
//! mistyped path can never be the thing that made a count zero.
//!
//! # What this gate does *not* assert, and why
//!
//! The design's §7 lists a "kind-space drift" mitigation for gate 6: assert every common kind holds
//! the same `u16` in both spaces. **That is a B2 artifact and it is deliberately absent.** It
//! presupposes §3a's shared node-kind prefix, which B3 does not build: under B3 the two spaces are
//! derived independently, and Task 8's anti-diff control asserts they *diverge* inside the token
//! block — the exact opposite. Writing both would make the suite self-contradictory.
//!
//! Under B3 the drift mitigation is **Task 8's source census** (`lossless_x_kinds.rs`), which is
//! the stronger instrument for this purpose anyway: a discriminant-equality test notices that two
//! spaces disagree with each other, while the source census notices that a space disagrees with
//! *its own grammar*, which is the failure that actually matters.
//!
//! # The one defect this gate found
//!
//! `graphqlx/lossless/ast/mod.rs` opened with an intra-doc link to
//! `crate::graphql::lossless::ast` — prose, not code, and therefore invisible to every other gate.
//! It is a **compile-time reference across the boundary**: an intra-doc link resolves only when the
//! other dialect's feature is on, so
//! `cargo doc --no-default-features --features rowan,graphqlx` failed on it under `-D warnings`.
//! That is why [`FORBIDDEN`]'s patterns are counted over **every line** rather than over code
//! alone.

use std::{
  collections::BTreeSet,
  path::{Path, PathBuf},
};

/// The dialect-generic substrate: the kind-space contract, the trivia atoms, the `Parse` surface,
/// the coverage shims and the typed-wrapper macro.
const SUBSTRATE: &str = "src/parser/lossless";
/// The GraphQL dialect's lossless layer.
const GRAPHQL: &str = "src/parser/graphql/lossless";
/// The GraphQLx dialect's lossless layer.
const GRAPHQLX: &str = "src/parser/graphqlx/lossless";

/// Every `.rs` file under `dir`, recursively, as `(path relative to the crate root, contents)`.
///
/// **Panics if `dir` does not exist or holds no `.rs` file at all.** That is the whole reason this
/// is a function rather than three inline `read_dir`s: a scan over a mistyped path returns zero
/// matches for every pattern, which would make each "does not occur" assertion below pass over
/// nothing. [`the_scanned_directories_are_real`] exercises the panic.
fn rust_files(dir: &str) -> Vec<(String, String)> {
  let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(dir);
  assert!(
    root.is_dir(),
    "{} is not a directory; this gate would otherwise scan nothing and report it as isolation",
    root.display()
  );
  let mut out = Vec::new();
  let mut stack = vec![root.clone()];
  while let Some(at) = stack.pop() {
    for entry in
      std::fs::read_dir(&at).unwrap_or_else(|e| panic!("{} is unreadable: {e}", at.display()))
    {
      let path = entry.expect("a directory entry").path();
      if path.is_dir() {
        stack.push(path);
      } else if path.extension().is_some_and(|ext| ext == "rs") {
        let text = std::fs::read_to_string(&path)
          .unwrap_or_else(|e| panic!("{} is unreadable: {e}", path.display()));
        out.push((relative(&root, &path, dir), text));
      }
    }
  }
  assert!(
    !out.is_empty(),
    "{} holds no .rs file, so every count taken over it is zero for the wrong reason",
    root.display()
  );
  out.sort();
  out
}

/// `<crate>/src/graphqlx/lossless/ast/mod.rs` -> `src/graphqlx/lossless/ast/mod.rs`.
fn relative(root: &Path, path: &Path, dir: &str) -> String {
  let tail = path
    .strip_prefix(root)
    .expect("every scanned path is under its root");
  format!("{dir}/{}", tail.display())
}

/// Every line under `dir` that contains `pattern`, as `(file, line number, line)`.
///
/// **One function for both sides of every check**, and that is deliberate. An earlier draft counted
/// the forbidden side with one helper and the control side with another; a defect in the forbidden
/// side's helper — a scan that matched nothing — would then have left every control green and every
/// absence assertion vacuous. With one function a broken scan reds on the controls immediately.
///
/// Lines, not files and not occurrences: a line is the unit a failure message can quote.
fn references(dir: &str, pattern: &str) -> Vec<(String, usize, String)> {
  let mut out = Vec::new();
  for (path, text) in rust_files(dir) {
    for (n, line) in text.lines().enumerate() {
      if line.contains(pattern) {
        out.push((path.clone(), n + 1, line.trim().to_string()));
      }
    }
  }
  out
}

/// Print a set one entry per line, for a failure message worth reading.
fn listed<T: core::fmt::Debug>(items: impl IntoIterator<Item = T>) -> String {
  items
    .into_iter()
    .map(|item| format!("\n  {item:?}"))
    .collect::<String>()
}

// ---------------------------------------------------------------------------------------------
// the two dialects
// ---------------------------------------------------------------------------------------------

/// A pattern that names one dialect, the tree it must be absent from, and the tree it must occur
/// in.
///
/// Four spellings per direction rather than the design's one, because they fail differently. A
/// `use` reaches for `crate::graphql::…`; a module path written through `super` chains reaches for
/// `graphql::kinds` or `graphql::lossless` without ever spelling `crate`; and a re-export can bring
/// the `rowan::Language` marker itself into scope under its bare name, which is the single type
/// whose leaking would make a wrapper silently answer for the other dialect's tree.
///
/// The third column is what turns each zero into evidence.
const FORBIDDEN: &[(&str, &str, &str)] = &[
  ("crate::parser::graphql::", GRAPHQLX, GRAPHQL),
  ("graphql::kinds", GRAPHQLX, GRAPHQL),
  ("graphql::lossless", GRAPHQLX, GRAPHQL),
  ("GraphQLLang", GRAPHQLX, GRAPHQL),
  ("crate::parser::graphqlx::", GRAPHQL, GRAPHQLX),
  ("graphqlx::kinds", GRAPHQL, GRAPHQLX),
  ("graphqlx::lossless", GRAPHQL, GRAPHQLX),
  ("GraphQLxLang", GRAPHQL, GRAPHQLX),
];

#[test]
fn the_two_lossless_layers_do_not_reference_each_other() {
  // Note the pattern pairs are not substrings of one another: `crate::graphql::` does not occur
  // inside `crate::graphqlx::` (the character after `graphql` is `x`, not `:`), and `GraphQLLang`
  // does not occur inside `GraphQLxLang`. Were they nested, the graphqlx column would count the
  // graphql pattern on every line and no direction would mean anything.
  for (pattern, absent_from, present_in) in FORBIDDEN {
    let found = references(absent_from, pattern);
    assert!(
      found.is_empty(),
      "`{pattern}` occurs in {absent_from}, which is a reference across the dialect boundary — \
       and an intra-doc link counts, because it resolves only when the other dialect's feature is \
       on:{}",
      listed(found)
    );
    // The control. A grep returning zero is not evidence of absence until that pattern has matched
    // something.
    assert!(
      !references(present_in, pattern).is_empty(),
      "`{pattern}` matches nothing in {present_in} either, so the zero above is about the pattern \
       and not about the tree"
    );
  }
  assert_eq!(FORBIDDEN.len(), 8, "four spellings in each direction");
}

/// The second segment of every `crate::…` path a dialect's lossless tree is allowed to name.
///
/// The totality half of [`the_two_lossless_layers_do_not_reference_each_other`]. That test asks
/// whether four named spellings occur; this one enumerates what *does* occur and compares it to a
/// pinned set, so a fifth spelling nobody thought of shows up as an unpinned root rather than as a
/// pattern that was never written.
///
/// Four roots, and each earns its place: the dialect's own subtree, the substrate it stands on,
/// [`ast_node!`](smear::ast_node) — a `#[macro_export]`ed macro whose expansion names `$crate` and
/// which every wrapper file therefore spells as `crate::ast_node` — and the lexer.
///
/// `crate::lexer` is the entry the crate merge (#83) added, and it is a rename rather than a new
/// permission: the dialect trees have always named their own lexer dialect, and they spelled it
/// `smear_lexer::…` when that was a separate crate. An external crate name is invisible to a
/// census that reads `crate::` roots, so the root was never pinned; now it is. What the merge
/// does NOT do is let a dialect reach the *other* dialect's lexer through it —
/// [`FORBIDDEN`]'s `graphql::kinds` / `graphql::lossless` spellings are substring patterns and
/// match `crate::lexer::graphql::lossless::…` exactly as they matched `smear_lexer::graphql::…`.
///
/// `crate::parser::type_system` is #58's entry, and it is on the GraphQL side only because that
/// is the only dialect with a projection so far. The projection's **target** is the AST, and the
/// AST's carriers are shared and dialect-free in exactly the way `crate::parser::lossless` is —
/// this census is about a dialect reaching the *other dialect*, which a shared carrier is not.
/// The narrow reason it is needed at all: three `Described<…>` aliases and six `…Data` extension
/// enums have no spelling under `graphql::ast`, and a projection has to construct all nine. Every
/// other AST type it builds is reached through the dialect's own `ast` module, which is why this
/// is one root and not eight.
const ALLOWED_CRATE_ROOTS: &[(&str, &[&str])] = &[
  (
    GRAPHQL,
    &[
      "crate::ast_node",
      "crate::lexer",
      "crate::parser::graphql",
      "crate::parser::lossless",
      "crate::parser::type_system",
    ],
  ),
  (
    GRAPHQLX,
    &[
      "crate::ast_node",
      "crate::lexer",
      "crate::parser::graphqlx",
      "crate::parser::lossless",
    ],
  ),
];

/// Every `crate::<segment>` prefix that occurs under `dir`.
fn crate_roots(dir: &str) -> BTreeSet<String> {
  let mut out = BTreeSet::new();
  for (_, text) in rust_files(dir) {
    let mut rest = text.as_str();
    while let Some(at) = rest.find("crate::") {
      rest = &rest[at + "crate::".len()..];
      // The parser is one module below the crate root since the crates merged, so `crate::parser::`
      // is the prefix every in-tree path carries and the segment AFTER it is the root this census
      // is about. The hop is stripped rather than assumed: `crate::ast_node` — the
      // `#[macro_export]`ed macro — is still rooted at the crate itself, and folding both spellings
      // to a single `crate::parser` would collapse the census to one entry and pin nothing.
      let prefix = match rest.strip_prefix("parser::") {
        Some(tail) => {
          rest = tail;
          "crate::parser::"
        }
        None => "crate::",
      };
      let end = rest
        .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .unwrap_or(rest.len());
      if end > 0 {
        out.insert(format!("{prefix}{}", &rest[..end]));
      }
    }
  }
  out
}

#[test]
fn a_dialects_lossless_tree_names_only_itself_the_substrate_and_the_shared_macro() {
  for (dir, allowed) in ALLOWED_CRATE_ROOTS {
    let found = crate_roots(dir);
    let pinned: BTreeSet<String> = allowed.iter().map(|s| s.to_string()).collect();
    let unexpected: BTreeSet<_> = found.difference(&pinned).collect();
    let vanished: BTreeSet<_> = pinned.difference(&found).collect();
    assert!(
      unexpected.is_empty(),
      "{dir} names crate items outside its own dialect, the substrate and the shared macro:{}",
      listed(unexpected)
    );
    // Two-sided. A root that stopped occurring is not a violation, but it does mean this census is
    // pinning something that is no longer there, and an unmaintained census is how the next real
    // entry gets waved through.
    assert!(
      vanished.is_empty(),
      "{dir} no longer names these pinned roots, so the census is stale:{}",
      listed(vanished)
    );
  }
}

// ---------------------------------------------------------------------------------------------
// the substrate
// ---------------------------------------------------------------------------------------------

/// Dialect **types** the substrate may not name, each with a tree it must occur in.
///
/// The Lego rule, mechanised. `src/lossless/` may name `L: Lexer`, tokora's capability traits, the
/// `Ctx` bundles, the `Lang` marker and `rowan::Language` — and no concrete dialect token type,
/// kind enum, keyword enum or error type. `crate::lexer` is on the list because the substrate is
/// generic over the lexer: the moment it names the concrete lexer module it has picked a token
/// space, and a token space is a dialect.
const SUBSTRATE_FORBIDDEN: &[(&str, &str)] = &[
  ("crate::parser::graphql", GRAPHQL),
  ("crate::parser::graphqlx", GRAPHQLX),
  ("GraphQLLang", GRAPHQL),
  ("GraphQLxLang", GRAPHQLX),
  ("crate::lexer", GRAPHQLX),
];

#[test]
fn the_substrate_names_no_dialect() {
  for (pattern, present_in) in SUBSTRATE_FORBIDDEN {
    let found = references(SUBSTRATE, pattern);
    assert!(
      found.is_empty(),
      "the substrate names `{pattern}`, so it is no longer dialect-generic:{}",
      listed(found)
    );
    assert!(
      !references(present_in, pattern).is_empty(),
      "`{pattern}` matches nothing in {present_in} either, so the zero above is about the pattern"
    );
  }
  // What the substrate *is* allowed to stand on, so the five zeros above are not simply a file it
  // failed to read.
  assert!(!references(SUBSTRATE, "tokora").is_empty());
  assert!(!references(SUBSTRATE, "rowan").is_empty());
}

/// The one `#[cfg]` the substrate writes about dialects, and how many times.
///
/// Two, both identical, and they are the substrate declining to compile its macros when **no**
/// dialect is on rather than reaching for one — `any(…)` and not `graphql`. A gate that only
/// forbade dialect *types* would let a `#[cfg(feature = "graphql")]` fork appear here, which is
/// exactly how a generic layer starts having a favourite.
const SUBSTRATE_FEATURE_GATE: &str = r#"#[cfg(any(feature = "graphql", feature = "graphqlx"))]"#;
/// How many times [`SUBSTRATE_FEATURE_GATE`] occurs.
const SUBSTRATE_FEATURE_GATES: usize = 2;

#[test]
fn every_dialect_word_in_the_substrate_is_prose_or_the_one_feature_gate() {
  // `the_substrate_names_no_dialect` forbids the dialect *types*. The substrate does still write
  // the words `graphql` and `GraphQLx` — in prose that explains the rule, in a macro's doc example,
  // and in a feature gate — and a rule that forbade the words outright would be a rule the code
  // cannot follow. So each occurrence is classified instead, and anything that is neither a comment
  // nor that one feature gate is a dialect entering the substrate as code.
  let mut gates = 0usize;
  let mut prose = 0usize;
  let mut offenders = Vec::new();
  for (path, text) in rust_files(SUBSTRATE) {
    for (n, line) in text.lines().enumerate() {
      if !line.to_ascii_lowercase().contains("graphql") {
        continue;
      }
      let trimmed = line.trim();
      if trimmed.starts_with("//") {
        prose += 1;
      } else if trimmed == SUBSTRATE_FEATURE_GATE {
        gates += 1;
      } else {
        offenders.push(format!("{path}:{}: {trimmed}", n + 1));
      }
    }
  }
  assert!(
    offenders.is_empty(),
    "the substrate names a dialect somewhere that is neither prose nor its one feature gate:{}",
    listed(offenders)
  );
  assert_eq!(
    gates, SUBSTRATE_FEATURE_GATES,
    "the substrate's dialect-facing cfg count moved; every one of them must read `any(…)`"
  );
  // The control. With no prose the classification above would be satisfied by a substrate that
  // never says the word, which is not the thing being measured.
  assert!(
    prose > 0,
    "no comment in the substrate mentions a dialect, so the classification is about nothing"
  );
}

// ---------------------------------------------------------------------------------------------
// the scan itself
// ---------------------------------------------------------------------------------------------

#[test]
fn the_scanned_directories_are_real() {
  // The failure this closes: `references()` returning 0 for a path that does not exist would make
  // every assertion above pass over nothing at all.
  for (dir, floor) in [(SUBSTRATE, 5), (GRAPHQL, 15), (GRAPHQLX, 20)] {
    let files = rust_files(dir);
    assert!(
      files.len() >= floor,
      "{dir} holds only {} .rs files, fewer than the {floor} this gate was written against — \
       either the layer moved or the scan is looking in the wrong place",
      files.len()
    );
    assert!(
      files.iter().any(|(_, text)| !text.trim().is_empty()),
      "{dir} holds only empty files"
    );
  }

  // And the panic is real, not a comment. Without this the guard inside `rust_files` is itself
  // untested, and an untested guard is the one that turns out to have been `if false`.
  let hook = std::panic::take_hook();
  std::panic::set_hook(Box::new(|_| {}));
  let missing = std::panic::catch_unwind(|| rust_files("src/parser/lossless_that_does_not_exist"));
  std::panic::set_hook(hook);
  assert!(
    missing.is_err(),
    "scanning a directory that does not exist answered instead of panicking, so a mistyped path \
     in this file would read as perfect isolation"
  );
}
