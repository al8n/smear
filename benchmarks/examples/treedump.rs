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
//! ```
//!
//! # Comparing two tokora checkouts
//!
//! Point the `tokora` patch at each checkout in turn with `--config`, never by editing the
//! committed `[patch.crates-io]` path, and give each variant its own `CARGO_TARGET_DIR` so the two
//! builds cannot contaminate each other:
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

use core::fmt::Write as _;
use std::process::ExitCode;

use rowan::{NodeOrToken, WalkEvent};
use smear_apollo_bench::{CORPUS, Entry, smear_parse};
use smear_parser::graphql::lossless::SyntaxNode;

/// One element of a CST, projected onto the four axes the dump prints.
///
/// Materialised rather than streamed straight to the output so that [`selfcheck`] can compare two
/// trees **column by column** — and therefore state "these two differ only in kind" as a checked
/// fact rather than as a hope about the source pair it chose.
struct Row {
  /// Levels of nesting below the root. The root itself is 0.
  depth: usize,
  /// `{:?}` of the node's or token's `SyntaxKind`.
  kind: String,
  /// Absolute byte offset of the element's first byte.
  start: u32,
  /// Absolute byte offset one past the element's last byte.
  end: u32,
  /// `Some` for a token, carrying its escaped text; `None` for an interior node.
  ///
  /// This is also what tells a node line from a token line on the page: a token always carries a
  /// quoted text and a node never does.
  text: Option<String>,
}

/// Escape a token's text so that one token is always one line, forever.
///
/// Lifted from `smear-parser/tests/lossless_golden.rs`, and for its reason: backslash, quote and
/// the three whitespace forms that would break the layout are spelled out; every other C0/C1
/// control, plus the byte-order mark, becomes `\u{…}`. Anything else — including ordinary
/// multi-byte text — is written through verbatim.
///
/// `char::escape_debug` is the wrong tool here: its notion of "printable" is a Unicode table that
/// moves between toolchain releases, so a dump hashed on one toolchain and compared on another
/// could differ for a reason that has nothing to do with the tree. `char::is_control` is a fixed
/// two-range test and cannot drift.
fn escape(text: &str) -> String {
  let mut out = String::with_capacity(text.len());
  for ch in text.chars() {
    match ch {
      '\\' => out.push_str("\\\\"),
      '"' => out.push_str("\\\""),
      '\n' => out.push_str("\\n"),
      '\r' => out.push_str("\\r"),
      '\t' => out.push_str("\\t"),
      c if c.is_control() || c == '\u{feff}' => {
        let _ = write!(out, "\\u{{{:04x}}}", c as u32);
      }
      c => out.push(c),
    }
  }
  out
}

/// Walk a tree with `preorder_with_tokens` and project every element onto a [`Row`].
///
/// `preorder_with_tokens` rather than `descendants_with_tokens` because only the former reports
/// `Leave`, and without `Leave` there is no depth — which would cost the dump its one defence
/// against two differently-nested trees flattening to the same sequence.
fn rows(root: &SyntaxNode) -> Vec<Row> {
  let mut out = Vec::new();
  let mut depth = 0usize;
  for event in root.preorder_with_tokens() {
    match event {
      WalkEvent::Enter(element) => {
        let range = element.text_range();
        out.push(Row {
          depth,
          kind: match &element {
            NodeOrToken::Node(node) => format!("{:?}", node.kind()),
            NodeOrToken::Token(token) => format!("{:?}", token.kind()),
          },
          start: u32::from(range.start()),
          end: u32::from(range.end()),
          text: match &element {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(escape(token.text())),
          },
        });
        depth += 1;
      }
      WalkEvent::Leave(_) => depth -= 1,
    }
  }
  out
}

/// Render rows in `.rast` style: `<indent><Kind>@<start>..<end>` for a node, plus ` "<text>"` for
/// a token.
fn render(rows: &[Row]) -> String {
  let mut out = String::new();
  for row in rows {
    for _ in 0..row.depth {
      out.push_str("  ");
    }
    let _ = write!(out, "{}@{}..{}", row.kind, row.start, row.end);
    if let Some(text) = &row.text {
      let _ = write!(out, " \"{text}\"");
    }
    out.push('\n');
  }
  out
}

/// The full dump for one source: a header, the tree, a footer, one line per diagnostic, a blank.
///
/// The footer is a summary of facts already implied by the lines above it, restated in one place
/// so that a `diff` of two dumps that disagree opens with *what* disagreed — a node count, a root
/// span, a round-trip verdict — before the reader has to find the first differing element line.
fn dump(header: &str, src: &str) -> String {
  let parse = smear_parse(src);
  let root = parse.syntax();
  let rows = rows(&root);

  let nodes = rows.iter().filter(|r| r.text.is_none()).count();
  let tokens = rows.len() - nodes;
  let round_trips = root.text() == src;

  let mut out = String::new();
  let _ = writeln!(out, "== {header} ==");
  out.push_str(&render(&rows));
  let _ = writeln!(
    out,
    "-- {nodes} nodes, {tokens} tokens, root {}..{}, round-trip {}, {} diagnostics --",
    u32::from(root.text_range().start()),
    u32::from(root.text_range().end()),
    if round_trips {
      "byte-exact"
    } else {
      "MISMATCH"
    },
    parse.diagnostics().len(),
  );
  for (i, d) in parse.diagnostics().iter().enumerate() {
    let span = d.span();
    let _ = writeln!(
      out,
      "   diagnostic {i}: {}..{} {:?} skipped={}",
      span.start,
      span.end,
      d.severity(),
      match d.skipped_tokens() {
        Some(n) => n.to_string(),
        None => "-".to_string(),
      },
    );
  }
  out.push('\n');
  out
}

/// Replace the entry's first `:` with a space, and report which byte moved.
///
/// **Why this perturbation.** The requirement on a control is that it change the *tree* and not
/// merely the page. A colon in GraphQL is a grammatical separator — variable definition, argument,
/// field definition, object field — and turning one into whitespace makes the production that
/// required it fail and recover. What makes it a good control specifically is what it does *not*
/// change: it is a one-byte substitution, so the document's length is identical, every byte other
/// than that one is identical, the root span is unchanged, the token count is unchanged, and the
/// parse still round-trips byte-exactly. So the perturbed dump is not a differently-*formatted*
/// dump and it is not a dump of differently-sized input; the hash moves because the tree moved.
///
/// Measured, on the three entries as they stand: node counts go 33/1082/11017 → 32/1076/12016 and
/// the diagnostic counts go 0/0/0 → 2/7/7004, while **round-trip stays byte-exact on all three**.
/// So `gate.rs`'s round-trip column is blind to this perturbation and only its error column is
/// not — and the pair in [`selfcheck`] is blind to the error column too.
fn perturb(src: &str) -> Option<(String, usize)> {
  // `:` is ASCII, so the byte index `find` returns is a char boundary in both directions.
  let at = src.find(':')?;
  let mut out = String::with_capacity(src.len());
  out.push_str(&src[..at]);
  out.push(' ');
  out.push_str(&src[at + 1..]);
  Some((out, at))
}

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

  let perturbed = args.iter().any(|a| a == "perturbed");

  // Built whole and written once. At ~45,000 lines a `println!` per element spends most of its
  // time re-acquiring the stdout lock, and the dump is a single artifact anyway — it is going to
  // be hashed or diffed, never read as it streams.
  let mut out = String::new();
  for entry in CORPUS {
    out.push_str(&entry_dump(entry, perturbed));
  }
  print!("{out}");
  ExitCode::SUCCESS
}

/// One corpus entry's dump, in whichever of the two modes was asked for.
fn entry_dump(entry: &Entry, perturbed: bool) -> String {
  if perturbed {
    match perturb(entry.source) {
      Some((src, at)) => dump(
        &format!(
          "{} : {} bytes : PERTURBED byte {at} ':' -> ' '",
          entry.name,
          src.len()
        ),
        &src,
      ),
      // Not a case any current entry hits, and not one to paper over: a corpus entry with no
      // colon would silently make the control identical to the dump it is a control for.
      None => format!(
        "== {} : {} bytes : NOT PERTURBED (no ':' in the source) ==\n\n",
        entry.name,
        entry.len()
      ),
    }
  } else {
    dump(
      &format!("{} : {} bytes", entry.name, entry.len()),
      entry.source,
    )
  }
}
