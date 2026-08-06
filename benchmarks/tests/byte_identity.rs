//! The byte-identity gate: recompute all four corpus hashes and fail if any has moved.
//!
//! # Why this file exists
//!
//! Before it, the four hashes lived in a GitHub issue and in a commit message. That is a note, not
//! a baseline: nothing recomputed it, so a change that re-shaped every tree in the corpus would go
//! green through every gate this repository had. `smear-parser`'s own golden-tree suite covers the
//! hand-written fixtures under `tests/golden/`; this covers the three real `apollo-parser`
//! documents, their one-byte-broken controls, 24 hand-broken documents and ~9,300 truncations, and
//! it is the only thing that does.
//!
//! # What a failure here means
//!
//! The tree changed. Not the text — `gate.rs` already checks that every corpus entry round-trips
//! byte-exactly, and a parse can round-trip perfectly while opening entirely different nodes. A
//! moved hash is a claim about **structure**: an indent shift is a re-parented subtree, a changed
//! kind is a production opening the wrong node, a changed range is a node boundary that moved.
//!
//! To see which:
//!
//! ```text
//! cargo run -p smear-apollo-bench --example treedump --release -- <corpus> > /tmp/after.dump
//! git stash && cargo run -p smear-apollo-bench --example treedump --release -- <corpus> > /tmp/before.dump
//! git stash pop && diff /tmp/before.dump /tmp/after.dump
//! ```
//!
//! **Do not edit the constant to make this pass.** The constants live in `src/dump.rs` with a note
//! on what they are pinned to; re-bless one only after the diff above is understood and intended,
//! and record what moved it in the same commit. Editing a recorded hash to match whatever the code
//! now produces turns the only byte-identity gate in this repository into a tautology.

use smear_apollo_bench::dump::{Corpus, hash};

/// The corpora [`cheap`] covers — everything that is not the 45-million-line one.
const CHEAP: &[Corpus] = &[Corpus::Clean, Corpus::Perturbed, Corpus::Malformed];

/// The corpora [`prefixes`] covers.
const EXPENSIVE: &[Corpus] = &[Corpus::Prefixes];

/// Recompute each corpus's hash and collect every mismatch, rather than tripping on the first.
///
/// Collecting matters: if a parser change moves three corpora, the reader wants all three named in
/// one run, because *which* corpora moved is the strongest available clue about what changed. A
/// change that moves `clean` but not `malformed` is a change on the accepting path; one that moves
/// only `malformed` and `prefixes` is a change to recovery.
fn check(corpora: &[Corpus]) {
  assert!(
    !corpora.is_empty(),
    "this gate was handed an empty corpus set, so it would pass without checking anything"
  );

  let mut failures = Vec::new();
  for &corpus in corpora {
    let got = hash(corpus);
    let want = corpus.expected();
    if got != want {
      failures.push(format!(
        "  {:<10} recorded {want}\n  {:<10} computed {got}",
        corpus.name(),
        ""
      ));
    }
  }

  assert!(
    failures.is_empty(),
    "{} of {} corpus dump(s) no longer hash to their recorded value:\n{}\n\n\
     The tree changed. Read this as a claim about structure, not text: a parse can round-trip \
     byte-exactly and still open different nodes. `diff` the before/after dumps to see where \
     (see this file's header), and do NOT edit the constant in src/dump.rs to make this pass.",
    failures.len(),
    corpora.len(),
    failures.join("\n"),
  );
}

/// The three corpora that are cheap enough to hash unconditionally.
#[test]
fn cheap() {
  check(CHEAP);
}

/// The truncation corpus — ~9,300 documents and roughly 45 million lines of dump.
///
/// `#[ignore]` because in an unoptimised build this is minutes rather than seconds, and it would
/// otherwise be paid by every `cargo test --workspace` on every member. It is **not** dropped: CI
/// runs it in release with `--include-ignored`, and [`every_corpus_is_covered`] fails if a corpus
/// ever stops being reachable from one of the two lists above.
///
/// Locally: `cargo test -p smear-apollo-bench --release -- --include-ignored`.
#[test]
#[ignore = "45M lines; run in release, CI covers it via --include-ignored"]
fn prefixes() {
  check(EXPENSIVE);
}

/// Every corpus is covered by exactly one of the two tests above.
///
/// The failure this exists to prevent is the quiet one. Adding a fifth corpus to
/// [`Corpus::ALL`] and forgetting to list it would leave it unchecked while the suite stayed
/// green — the same shape as a cargo target filter that matches nothing and exits 0. This turns
/// that into a red test naming the corpus that has no home.
#[test]
fn every_corpus_is_covered() {
  for corpus in Corpus::ALL {
    let in_cheap = CHEAP.contains(&corpus);
    let in_expensive = EXPENSIVE.contains(&corpus);
    assert!(
      in_cheap ^ in_expensive,
      "corpus `{}` is covered by {} of this file's two test lists; it must be covered by exactly \
       one, or it is either unchecked or checked twice",
      corpus.name(),
      if in_cheap { "both" } else { "neither" },
    );
  }

  assert_eq!(
    CHEAP.len() + EXPENSIVE.len(),
    Corpus::ALL.len(),
    "the two lists name more corpora between them than `Corpus::ALL` has"
  );
}
