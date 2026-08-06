//! Dump every corpus entry's finished CST — every node and every token, in document order, with
//! its depth, its kind and its exact byte span — so that two builds of the parser can be compared
//! as **trees** rather than as text.
//!
//! Every tokora performance branch ships on the claim "the output trees are byte-identical". This
//! is the command that decides it. Its sibling [`gate.rs`](./gate.rs) checks that each side parses
//! without error and that `tree.text()` reproduces the source byte-for-byte; that is necessary and
//! it is *not* sufficient, because text is the tree with its shape projected away. `query Q { f }`
//! is one `OperationDefinition` through the mixed root and five `Error` nodes through the SDL-only
//! root, and the two round-trip to the same bytes. Everything in that gap is this file's to catch.
//!
//! ```text
//! cargo run -p smear-apollo-bench --example treedump --release | shasum -a 256
//! cargo run -p smear-apollo-bench --example treedump --release -- perturbed | shasum -a 256
//! cargo run -p smear-apollo-bench --example treedump --release -- selfcheck
//! cargo run -p smear-apollo-bench --example treedump --release -- malformed | shasum -a 256
//! ```
//!
//! # This file is the human-facing half of the harness
//!
//! The machinery — the row projection, the escape, the renderer, the four corpora and the recorded
//! hashes — lives in [`smear_apollo_bench::dump`], not here. That is so the *other* caller can
//! reach it: `tests/byte_identity.rs` recomputes all four hashes and fails if any has moved. This
//! example and that gate therefore dump the same bytes by construction rather than by two
//! implementations agreeing, which is the only arrangement in which `treedump | shasum` and the
//! gate can never disagree.
//!
//! # Two more modes, because the clean corpus cannot exercise recovery at all
//!
//! The three entries in `CORPUS` are real, hand-picked-*clean* GraphQL documents: every one of
//! them takes **zero** emitter checkpoints, so a clean run exercises no rewind, no recovery-hole
//! wrap, no gap tile and no error-coverage decision — the entire recovery path stays dark. A
//! byte-identity claim resting only on that corpus is not evidence about any of those; it is
//! silent about them. These two modes are what makes the recovery path non-vacuous:
//!
//! * **`malformed`** — 24 hand-broken documents: unclosed braces, unterminated strings and block
//!   strings, junk prefixes and suffixes, input with nothing lexable, empty input, bad defaults,
//!   bad variables. Cheap; safe to run any time.
//!
//! * **`prefixes`** — every cut point of every clean entry: every byte offset for the two small
//!   entries, every 37th byte of `alias` (a prime stride, so the cut points never align with a
//!   repeating structure in the document). Truncating at each offset is the cheapest way to reach
//!   thousands of *independent* recovery sites, each ending mid-production somewhere different.
//!   **This mode is not cheap**: roughly 9,300 documents, on the order of 45 million lines of
//!   dump. It is gated behind the explicit `prefixes` argument exactly like every other mode on
//!   this page, so it is never what the no-argument default, `perturbed`, `selfcheck` or
//!   `malformed` run. Redirect it to a file rather than a terminal.
//!
//!   ```text
//!   cargo run -p smear-apollo-bench --example treedump --release -- prefixes > /tmp/prefixes.dump
//!   shasum -a 256 /tmp/prefixes.dump
//!   ```
//!
//!   Note that `tests/byte_identity.rs` hashes this same corpus in **bounded memory** without
//!   writing 1.4 GB anywhere, so checking it does not require the redirect above.
//!
//! # Comparing two tokora checkouts
//!
//! This crate takes tokora from crates.io, so the committed configuration measures what a real
//! consumer gets. To measure an unpublished tokora instead, point the patch at each checkout in
//! turn with `--config`, never by editing a committed manifest, and give each variant its own
//! `CARGO_TARGET_DIR` so the two builds cannot contaminate each other:
//!
//! ```text
//! for t in /path/to/tokora-a /path/to/tokora-b; do
//!   CARGO_TARGET_DIR=/tmp/td-$(basename $t) \
//!   cargo --config "patch.crates-io.tokora.path=\"$t\"" \
//!     run -p smear-apollo-bench --example treedump --release | shasum -a 256
//! done
//! ```
//!
//! Equal hashes mean the two builds produced the same tree, node for node, token for token, span
//! for span. Unequal hashes mean they did not, and `diff` over the two dumps names where.
//!
//! # The format, and why it discriminates
//!
//! rust-analyzer's `.rast` style, the same one `smear-parser`'s golden-tree gate uses
//! (`smear-parser/tests/lossless_golden.rs`): one line per element, two spaces of indent per level
//! of depth, each line carrying the kind and the absolute byte range, and each **token** line
//! additionally carrying its text, escaped so that one element is always exactly one line. Each
//! column is one axis of structural change, and none of the four is redundant:
//!
//! * **indent** — a re-parented subtree shifts a whole block, so re-parenting is the loudest diff
//!   the format can produce rather than a diff it cannot produce at all. Two trees can share a
//!   flat preorder sequence of `(kind, span)` pairs and still nest differently; the indent is what
//!   separates them.
//! * **kind** — a production that opens the wrong node changes one word per site. This is the
//!   difference the round-trip check is structurally incapable of seeing: `{f(a:true)}` and
//!   `{f(a:null)}` place a `BooleanValue` and a `NullValue` at *identical* spans.
//! * **range** — a node boundary that moved changes numbers even when no line appears or vanishes:
//!   the "`Arguments` opened after the `(` instead of before" shape.
//! * **token text** — tokens are in the tree, so a token attached to the wrong parent is a moved
//!   line rather than an invisible one. Trivia is included deliberately: which node a comment
//!   commits into is a real decision this parser makes.
//!
//! `selfcheck` is the proof that the format does not collapse the first two of those. It is here
//! because a dump that printed the same thing regardless of the tree would satisfy "the hashes
//! match" vacuously and prove nothing. **A harness you cannot make disagree is not a harness.**
//!
//! # Why an example rather than a `[[bin]]`
//!
//! For exactly the reason [`gate.rs`](./gate.rs) gives at length: `cargo build` builds bins by
//! default and does not build examples, so a `[[bin]]` here would be the first executable smear's
//! fifteen-target `cross` job ever had to link, and it would fail on every target whose linker the
//! runner lacks.

use std::{
  io::{BufWriter, Write as _},
  process::ExitCode,
};

use smear_apollo_bench::{
  dump::{Corpus, for_each_chunk, render, rows},
  smear_parse,
};

/// One `selfcheck` case: a pair of sources chosen so that their trees differ along exactly one
/// axis.
struct Probe {
  /// What the case claims to prove.
  claim: &'static str,
  /// The axis that must differ between the two trees.
  differs: Axis,
  /// The left-hand source.
  a: &'static str,
  /// The right-hand source.
  b: &'static str,
}

/// Which column of the dump a [`Probe`] isolates.
#[derive(Clone, Copy)]
enum Axis {
  /// Kinds differ; depths and byte ranges are identical.
  Kind,
  /// Byte ranges differ; depths and kinds are identical.
  Range,
}

/// The anti-vacuity gate: prove the format does not collapse a kind-only or a range-only
/// difference.
///
/// Each probe is checked in two steps, and the first step is the one that matters. Before the
/// dumps are compared at all, the two trees are compared **column by column** to establish that
/// they really do differ along the claimed axis and along no other — same element count, same
/// depth at every position, and identity on the two columns the probe is not about. Only then is
/// `render` asked whether it can tell them apart. A probe whose precondition fails is reported as
/// a failure rather than skipped, because a source pair that stopped exercising the axis it was
/// chosen for would otherwise turn this gate green by testing nothing.
///
/// A note on what "range-only" can mean. In a rowan tree a span *is* the running sum of the token
/// texts before it, so two trees over different sources cannot differ in their ranges while
/// agreeing on every token's text — a genuinely text-identical, range-different pair can only be
/// built synthetically. The honest claim, and the one [`Axis::Range`] checks, is the one that
/// matters for a diff: the kind column and the depth column are identical across the pair, so the
/// difference the dump reports is carried by the range.
fn selfcheck() -> bool {
  const PROBES: &[Probe] = &[
    Probe {
      claim: "kind-only: BooleanValue vs NullValue at identical spans",
      differs: Axis::Kind,
      a: "{f(a:true)}",
      b: "{f(a:null)}",
    },
    Probe {
      claim: "range-only: the same Int value one byte wider",
      differs: Axis::Range,
      a: "{f(a:1)}",
      b: "{f(a:11)}",
    },
  ];

  let mut all_ok = true;
  for probe in PROBES {
    let (pa, pb) = (smear_parse(probe.a), smear_parse(probe.b));
    let ra = rows(&pa.syntax());
    let rb = rows(&pb.syntax());

    // Recorded so the PASS line evidences the point rather than asserting it: when both sides
    // parse clean and round-trip byte-exactly, `gate.rs` has nothing left to report a difference
    // *with*, and the dump is the only thing in the harness that still can.
    let clean = |p: &smear_parser::graphql::lossless::Parse, src: &str| {
      p.diagnostics().is_empty() && p.syntax().text() == src
    };
    let both_clean = clean(&pa, probe.a) && clean(&pb, probe.b);

    let mut problems: Vec<String> = Vec::new();

    if ra.len() != rb.len() {
      problems.push(format!(
        "element counts differ ({} vs {}), so the pair is not an isolated-axis pair",
        ra.len(),
        rb.len()
      ));
    } else {
      let mut same_depth = true;
      let mut same_kind = true;
      let mut same_range = true;
      for (x, y) in ra.iter().zip(rb.iter()) {
        same_depth &= x.depth == y.depth;
        same_kind &= x.kind == y.kind;
        same_range &= (x.start, x.end) == (y.start, y.end);
      }
      if !same_depth {
        problems.push("depths differ, so more than one axis moved".to_string());
      }
      match probe.differs {
        Axis::Kind => {
          if same_kind {
            problems.push("kinds are identical, so this probe exercises nothing".to_string());
          }
          if !same_range {
            problems.push("ranges differ too, so this is not a kind-only pair".to_string());
          }
        }
        Axis::Range => {
          if same_range {
            problems.push("ranges are identical, so this probe exercises nothing".to_string());
          }
          if !same_kind {
            problems.push("kinds differ too, so this is not a range-only pair".to_string());
          }
        }
      }
    }

    let da = render(&ra);
    let db = render(&rb);
    if da == db {
      problems.push(
        "THE DUMP IS IDENTICAL FOR THE TWO TREES — the format collapses this axis".to_string(),
      );
    }

    if problems.is_empty() {
      println!("PASS  {}", probe.claim);
      println!(
        "      {:?} vs {:?}, {} lines each, both sides {}; differing lines:",
        probe.a,
        probe.b,
        da.lines().count(),
        if both_clean {
          "parse clean and round-trip byte-exactly, so gate.rs sees no difference at all"
        } else {
          "NOT both a clean byte-exact parse"
        }
      );
      for (x, y) in da.lines().zip(db.lines()).filter(|(x, y)| x != y) {
        println!("      -{x}");
        println!("      +{y}");
      }
    } else {
      all_ok = false;
      println!("FAIL  {}", probe.claim);
      for problem in problems {
        println!("      {problem}");
      }
    }
    println!();
  }
  all_ok
}

fn main() -> ExitCode {
  let args: Vec<String> = std::env::args().skip(1).collect();

  if args.iter().any(|a| a == "selfcheck") {
    return if selfcheck() {
      ExitCode::SUCCESS
    } else {
      ExitCode::FAILURE
    };
  }

  // `prefixes` is ~45 million lines. Gated behind its explicit argument exactly like every other
  // mode on this page (see the module doc); nothing in this repository's CI passes any argument
  // to this example, so it can only run when a human types `-- prefixes` on purpose.
  let corpus = Corpus::ALL
    .into_iter()
    .find(|c| args.iter().any(|a| a == c.name()))
    .unwrap_or(Corpus::Clean);

  // Streamed through one buffered, locked handle. At ~45,000 lines an unbuffered write per
  // element spends most of its time re-acquiring the stdout lock, and the dump is a single
  // artifact anyway — it is going to be hashed or diffed, never read as it streams.
  let stdout = std::io::stdout();
  let mut out = BufWriter::with_capacity(1 << 20, stdout.lock());
  for_each_chunk(corpus, |chunk| {
    let _ = out.write_all(chunk.as_bytes());
  });
  let _ = out.flush();
  ExitCode::SUCCESS
}
