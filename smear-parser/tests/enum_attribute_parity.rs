//! A public enum's attribute set is part of its contract: `#[non_exhaustive]` forces every
//! downstream exhaustive match to grow a wildcard arm, and a derive lost from the list can drop a
//! trait bound a consumer already relied on. Neither shows up in a normal review pass over the
//! variant list, because neither one touches a variant.
//!
//! This branch (`feat/parser-materialised-values`) added `#[non_exhaustive]` to
//! [`ErrorData`](smear_parser::graphql::error::ErrorData) without saying so. The doc paragraph
//! introduced beside it argued the attribute belonged there, and a later round read that
//! justification as evidence the attribute predated the branch — it did not: `ErrorData` was
//! exhaustive at `c3e35b8`, the base this branch was cut from, and Codex found the gap
//!
//! **The merge base moved and these values did not.** #157 merged into `feat/proto`, so the branch
//! now sits on `6e0baf0`; `git diff --name-only c3e35b8 6e0baf0 -- smear-parser` is **empty**, so
//! every attribute string below is what the current merge base says too. Re-read them from the new
//! base only if that diff ever stops being empty — a table whose provenance commit is no longer
//! reachable from the base is a table nobody can re-derive.
//! after the justification had already made it read as settled. Both the attribute and the
//! paragraph are gone now, and this test is the gate the mistake argues for: **the attribute set
//! of every public enum in the files this branch touches must equal what it was at the merge
//! base, or the difference must be named below.**
//!
//! # Scope: the five files this branch's diff touches
//!
//! Not the whole crate. The defect this test guards was a diff-review blind spot on a branch's
//! own changes, and that is what [`FILES`] enumerates — `git diff --name-only c3e35b8 bd5d52b`
//! filtered to the files that can declare an `enum`. A public enum elsewhere in `smear-parser` can
//! still gain `#[non_exhaustive]` unnoticed; widening this list to the whole crate is future work
//! for whoever decides the cost of auditing every other enum's history is worth it, not a silent
//! limitation of this one.
//!
//! # Why a line scan and not `syn`
//!
//! `ci/source_census` reads source as text with `syn` because it has to see inside expressions —
//! a `match` arm buried in a closure, a type nested three levels deep. This census only has to
//! read the attribute lines stacked directly above a `pub enum` line, and every attribute in
//! these five files is one `cargo fmt`-kept line. [`find_public_enums`] asserts that rather than
//! assuming it, so a line that stops being true fails loudly here instead of being silently
//! misread — that is what keeps this test free of a `syn` dev-dependency `smear-parser` has no
//! other reason to carry.
//!
//! # The two tables, and what each failure means
//!
//! [`MERGE_BASE_ATTRS`] is what `git show c3e35b8:<path>` said for every enum that already
//! existed there, read once and copied here verbatim. [`RECORDED_DIFFERENCES`] is every enum
//! whose current attribute set does not match that — because it is new since the merge base, or
//! because it changed and the change was reviewed on its own merits. An enum in neither table, or
//! one whose current attributes differ from `MERGE_BASE_ATTRS` without a matching
//! `RECORDED_DIFFERENCES` entry, fails the test with both attribute sets printed. That is the
//! shape `#[non_exhaustive]` on `ErrorData` would have taken: no `RECORDED_DIFFERENCES` entry,
//! because the round that added it believed it was already there.
//!
//! A table entry matching nothing this scan still finds fails too, the same way
//! `ci/source_census`'s own exemption table does: a stale entry hides the next real one exactly
//! as an unrecorded change does.

use std::collections::BTreeSet;

/// One file this branch's diff touches, embedded at compile time so the census needs no
/// filesystem access beyond what the compiler already resolved.
struct SourceFile {
  /// Path relative to this crate's `Cargo.toml` — the root `git diff --stat` reports from.
  path: &'static str,
  text: &'static str,
}

const FILES: &[SourceFile] = &[
  SourceFile {
    path: "src/graphql/error.rs",
    text: include_str!("../src/graphql/error.rs"),
  },
  SourceFile {
    path: "src/graphql/ast/value.rs",
    text: include_str!("../src/graphql/ast/value.rs"),
  },
  SourceFile {
    path: "src/graphql/ast/materialized.rs",
    text: include_str!("../src/graphql/ast/materialized.rs"),
  },
  SourceFile {
    path: "src/graphql/syntactic/value/mod.rs",
    text: include_str!("../src/graphql/syntactic/value/mod.rs"),
  },
  SourceFile {
    path: "src/graphql/syntactic/value/numbers.rs",
    text: include_str!("../src/graphql/syntactic/value/numbers.rs"),
  },
];

/// A `pub enum`, found by the scan, with the attribute lines stacked directly above it. Doc
/// comments are excluded — this census is about the enum's contract, not its prose.
#[derive(Debug)]
struct FoundEnum {
  file: &'static str,
  name: String,
  attrs: Vec<String>,
}

/// Walks `file.text` line by line and returns every `pub enum` this scan can read, each paired
/// with the attribute lines immediately above it. `pub(crate)` and private enums are skipped —
/// they cannot break a downstream match, which is the property this gate exists to protect.
fn find_public_enums(file: &SourceFile) -> Vec<FoundEnum> {
  let lines: Vec<&str> = file.text.lines().collect();
  let mut found = Vec::new();

  for (i, line) in lines.iter().enumerate() {
    let Some(rest) = line.trim_start().strip_prefix("pub enum ") else {
      continue;
    };
    let name: String = rest
      .chars()
      .take_while(|c| c.is_alphanumeric() || *c == '_')
      .collect();
    assert!(
      !name.is_empty(),
      "{}:{}: `pub enum` with no name this scan can read: {line:?}",
      file.path,
      i + 1
    );

    // Walk upward collecting attribute lines, stopping at the first line that is not one — a
    // doc comment, a blank line, or code from the previous item.
    let mut attrs = Vec::new();
    let mut j = i;
    while j > 0 {
      j -= 1;
      let above = lines[j].trim();
      if above.starts_with("#[") {
        assert!(
          above.ends_with(']'),
          "{}:{}: an attribute line this scan cannot read whole: {above:?} — it spans more \
           than one line, and the census needs widening before it can trust what it read",
          file.path,
          j + 1
        );
        attrs.push(above.to_string());
      } else {
        break;
      }
    }
    attrs.reverse();
    found.push(FoundEnum {
      file: file.path,
      name,
      attrs,
    });
  }

  found
}

/// The attribute set each currently-existing public enum had at `c3e35b8`, this branch's merge
/// base into `feat/proto` — read once from `git show c3e35b8:<path>` and copied here verbatim.
///
/// `(file, enum name, attribute lines in source order)`.
#[rustfmt::skip]
const MERGE_BASE_ATTRS: &[(&str, &str, &[&str])] = &[
  ("src/graphql/error.rs", "VariableValueHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "ObjectFieldValueHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "SchemaExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "UnionTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "InputObjectTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "ObjectTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "InterfaceTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "EnumTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "Expectation", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "Unclosed", &[
    "#[derive(Debug, Copy, Clone, IsVariant)]",
  ]),
  ("src/graphql/error.rs", "ErrorData", &[
    "#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]",
    "#[unwrap(ref, ref_mut)]",
    "#[try_unwrap(ref, ref_mut)]",
  ]),
  ("src/graphql/ast/value.rs", "InputValue", &[
    "#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]",
    "#[unwrap(ref, ref_mut)]",
    "#[try_unwrap(ref, ref_mut)]",
  ]),
  ("src/graphql/ast/value.rs", "ConstInputValue", &[
    "#[derive(Debug, Clone, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]",
    "#[unwrap(ref, ref_mut)]",
    "#[try_unwrap(ref, ref_mut)]",
  ]),
];

/// Every enum whose current attribute set does not equal `MERGE_BASE_ATTRS` — new since the
/// merge base, or changed since it — each with the reason. This is the table `#[non_exhaustive]`
/// on `ErrorData` should have gone in and did not: an unreviewed attribute change is exactly an
/// entry missing from both this table and `MERGE_BASE_ATTRS`.
///
/// `(file, enum name, reason)`.
const RECORDED_DIFFERENCES: &[(&str, &str, &str)] = &[
  (
    "src/graphql/ast/value.rs",
    "ConstInputValue",
    "gained `From` in its derive list (merge base had none). Additive: `InputValue` beside it \
     has derived `From` since it was written, this is the one change the materialisation axis \
     makes to this file, and the doc comment directly above the derive says why — a shared \
     value-tree production converts the leaf it just read, so one body builds both trees.",
  ),
  (
    "src/graphql/ast/materialized.rs",
    "InputValue",
    "new enum: the materialised-number value tree's twin of `ast::InputValue`, gated behind \
     `materialized-numbers`. No merge-base entry exists because the file did not.",
  ),
  (
    "src/graphql/ast/materialized.rs",
    "ConstInputValue",
    "new enum, same reason as `materialized::InputValue` above.",
  ),
];

fn attrs_match(expected: &[&str], actual: &[String]) -> bool {
  expected.len() == actual.len() && expected.iter().zip(actual).all(|(e, a)| *e == a.as_str())
}

/// The gate: every public enum found in [`FILES`] is either unchanged since `MERGE_BASE_ATTRS`,
/// or its difference is named in `RECORDED_DIFFERENCES`. Plant a new `#[non_exhaustive]` (or any
/// other attribute change) on a tracked enum with no matching entry in either table, and this
/// test names it and fails.
#[test]
fn public_enum_attributes_match_the_merge_base_or_are_named() {
  let mut found = Vec::new();
  for file in FILES {
    found.extend(find_public_enums(file));
  }

  // Non-vacuity: a scan that silently stopped finding the surface must not pass by having
  // nothing left to check. 15 = 11 in error.rs + 2 in ast/value.rs + 2 in ast/materialized.rs;
  // update the count only after checking why it moved.
  assert_eq!(
    found.len(),
    15,
    "the scan found {} public enum(s) across {} files, expected 15 — a file gained or lost one, \
     or the line-matching in `find_public_enums` stopped seeing the surface",
    found.len(),
    FILES.len(),
  );

  let mut unexplained = Vec::new();
  let mut matched_baseline: BTreeSet<(&str, &str)> = BTreeSet::new();
  let mut matched_difference: BTreeSet<(&str, &str)> = BTreeSet::new();

  for enum_ in &found {
    let baseline = MERGE_BASE_ATTRS
      .iter()
      .find(|entry| entry.0 == enum_.file && entry.1 == enum_.name);
    let difference = RECORDED_DIFFERENCES
      .iter()
      .find(|entry| entry.0 == enum_.file && entry.1 == enum_.name);

    match baseline {
      Some(entry) if attrs_match(entry.2, &enum_.attrs) => {
        matched_baseline.insert((entry.0, entry.1));
      }
      Some(entry) => {
        matched_baseline.insert((entry.0, entry.1));
        match difference {
          Some(d) => {
            matched_difference.insert((d.0, d.1));
          }
          None => unexplained.push(format!(
            "{}::{} — attribute set changed since the merge base and is not named in \
             RECORDED_DIFFERENCES\n    merge base: {:?}\n    now:        {:?}",
            enum_.file, enum_.name, entry.2, enum_.attrs,
          )),
        }
      }
      None => match difference {
        Some(d) => {
          matched_difference.insert((d.0, d.1));
        }
        None => unexplained.push(format!(
          "{}::{} — no MERGE_BASE_ATTRS entry and not recorded as new in \
           RECORDED_DIFFERENCES\n    now: {:?}",
          enum_.file, enum_.name, enum_.attrs,
        )),
      },
    }
  }

  assert!(
    unexplained.is_empty(),
    "{} public enum(s) whose attribute set is not accounted for:\n{}",
    unexplained.len(),
    unexplained.join("\n"),
  );

  let stale_baseline: Vec<String> = MERGE_BASE_ATTRS
    .iter()
    .filter(|entry| !matched_baseline.contains(&(entry.0, entry.1)))
    .map(|entry| format!("{}::{}", entry.0, entry.1))
    .collect();
  assert!(
    stale_baseline.is_empty(),
    "MERGE_BASE_ATTRS entries matching no public enum this scan still finds (renamed, removed, \
     or the scan stopped finding them): {stale_baseline:?}",
  );

  let stale_difference: Vec<String> = RECORDED_DIFFERENCES
    .iter()
    .filter(|entry| !matched_difference.contains(&(entry.0, entry.1)))
    .map(|entry| format!("{}::{}", entry.0, entry.1))
    .collect();
  assert!(
    stale_difference.is_empty(),
    "RECORDED_DIFFERENCES entries whose enum now matches MERGE_BASE_ATTRS, or no longer exists \
     — delete the line, the difference it named is gone: {stale_difference:?}",
  );
}
