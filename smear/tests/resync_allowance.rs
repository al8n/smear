#![cfg(all(
  feature = "rowan",
  feature = "parser",
  feature = "test-support",
  feature = "std"
))]

//! The recovery scan allowance — smear issue #168.
//!
//! `recover::unexpected` and `recover::resync_to` both scan through `sync_balanced`, whose
//! no-match exit rewinds the whole scan; the caller then advances one token and asks again, so a
//! tail with no restart point is walked once per token in it. `lossless/recover.rs`'s
//! `scan_allowance_exhausted` bounds the waste. This file is that guard's gate.
//!
//! # Why the linearity assertions are on `spent` and not on the clock
//!
//! The defect was first measured on a machine at load average 54, where the same parse varied
//! 1.6× between runs — a wall-clock ratio gate would fail for the reason the box was busy rather
//! than for the reason the parser regressed. `scan_allowance::peak_spent` is
//! `TokenBudgetTally::spent` at the last recovery call: the produce-event count the guard is
//! denominated in, deterministic and machine-independent, and the quantity that actually grows
//! quadratically. Times are printed beside it for a human, and asserted on by nothing.
//!
//! # What the identity test does and does not establish
//!
//! The diagnostic counts it pins were recorded **before** the guard existed, from an unfused
//! build, so they are an oracle rather than a photograph of the current implementation. The full
//! `format!("{:#?}", syntax())` A/B — fuse on against fuse off, byte-identical on all five shapes
//! — was run at design time and cannot be re-run from a tree that has only one of those builds in
//! it. What is checkable here is the pre-guard diagnostic count and the lossless text, and that is
//! what is checked.

use smear::parser::lossless::recover::scan_allowance;
use std::time::Instant;

/// A parse reduced to the three facts every test here reads.
struct Run {
  diagnostics: usize,
  covered: usize,
  peak_spent: usize,
  refusals: usize,
  seconds: f64,
}

fn run(root: fn(&str) -> (usize, usize), src: &str) -> Run {
  scan_allowance::reset();
  let started = Instant::now();
  let (diagnostics, covered) = root(src);
  let seconds = started.elapsed().as_secs_f64();
  Run {
    diagnostics,
    covered,
    peak_spent: scan_allowance::peak_spent(),
    refusals: scan_allowance::refusals(),
    seconds,
  }
}

macro_rules! roots {
  ($($name:ident => $path:path),+ $(,)?) => {
    $(
      fn $name(src: &str) -> (usize, usize) {
        let parse = $path(src);
        (
          parse.diagnostics().len(),
          parse.syntax().text().to_string().len(),
        )
      }
    )+
  };
}

#[cfg(feature = "graphql")]
roots! {
  gql_document => smear::parser::graphql::lossless::parse_document,
  gql_type_system => smear::parser::graphql::lossless::parse_type_system_document,
  gql_executable => smear::parser::graphql::lossless::parse_executable_document,
}

#[cfg(feature = "graphqlx")]
roots! {
  gqlx_document => smear::parser::graphqlx::lossless::parse_document,
  gqlx_type_system => smear::parser::graphqlx::lossless::parse_type_system_document,
  gqlx_executable => smear::parser::graphqlx::lossless::parse_executable_document,
}

/// One census shape at one root, doubled twice.
///
/// The ratio is `peak_spent(2n) / peak_spent(n)`. Linear is 2; the defect measured 4.0 on every
/// one of these. The gate sits at 2.6, which is where #168's report put it: far enough above 2 to
/// absorb the constant terms a small document carries, far enough below 4 that a reintroduced
/// rescan cannot hide under it.
fn assert_linear(root_name: &str, root: fn(&str) -> (usize, usize), unit: &str) {
  const N: usize = 2_000;
  let small = run(root, &unit.repeat(N));
  let large = run(root, &unit.repeat(N * 2));
  assert_eq!(
    small.covered,
    unit.repeat(N).len(),
    "{root_name}/{unit:?}: the lossless guarantee broke, so the number below is not a number for \
     this document"
  );
  assert_eq!(
    large.covered,
    unit.repeat(N * 2).len(),
    "{root_name}/{unit:?}"
  );
  // A ratio over two numbers that are both ~0 is 1.00 and proves nothing. This shape has to
  // actually drive recovery at this root for the gate below to be a gate: the quantity it reads
  // is `spent` at a recovery call, and a root that never calls one reports 0 — or 1, if a single
  // call happened before anything was lexed. The floor is two orders of magnitude under the
  // smallest genuinely-recovering reading (36 047) and two above the vacuous one.
  assert!(
    small.peak_spent > 10_000 && large.peak_spent > 10_000,
    "{root_name}/{unit:?}: peak spend was {} / {}, so recovery barely ran and the ratio below \
     would pass vacuously. Either this shape stopped reaching the helpers at this root, or this \
     root has no resync site and the shape belongs in \
     `the_graphql_executable_root_does_not_resynchronise` instead.",
    small.peak_spent,
    large.peak_spent
  );
  let ratio = large.peak_spent as f64 / small.peak_spent as f64;
  println!(
    "  {root_name:<16} {:<14} spent {:>9} -> {:>9}  x{ratio:.2}   ({:.1}ms -> {:.1}ms, refusals \
     {} -> {})",
    format!("{unit:?}"),
    small.peak_spent,
    large.peak_spent,
    small.seconds * 1e3,
    large.seconds * 1e3,
    small.refusals,
    large.refusals
  );
  assert!(
    ratio < 2.6,
    "{root_name}/{unit:?}: lexer items produced grew x{ratio:.2} for a doubled input \
     ({} -> {}). Recovery is scanning a tail more than a bounded number of times again — see \
     `scan_allowance_exhausted` in smear-parser/src/lossless/recover.rs.",
    small.peak_spent,
    large.peak_spent
  );
}

/// The census from #168: the junk alphabets that defeat each helper's restart predicate.
///
/// `! ` and `@ ( ) ` are `unexpected`'s — punctuation the wide `is_sync_point` set does not name,
/// with `(` the one opener that is not in it. `[ type ] ` is `resync_to`'s alone: `[` **is** a sync
/// point, so `unexpected` stays at a zero-skip while every definition head sits at depth 1.
/// `( type ) ` is both at once. The executable roots take a keyword their own dispatcher accepts.
#[test]
#[cfg(feature = "graphql")]
fn every_graphql_census_shape_stays_linear() {
  println!("\n== GraphQL ==");
  for unit in ["! ", "@ ( ) ", "[ type ] ", "( type ) "] {
    assert_linear("gql/document", gql_document, unit);
    assert_linear("gql/type-system", gql_type_system, unit);
  }
  // Only the two `unexpected`-shaped units. The resync-shaped ones are inert at this root and
  // are pinned as such below.
  for unit in ["! ", "@ ( ) "] {
    assert_linear("gql/executable", gql_executable, unit);
  }
}

/// GraphQL's executable root has no `resync_to_definition` call site, and must not gain one.
///
/// It writes `executable_definition(inp)?` and propagates where the other five document roots
/// catch and resynchronise, so #168's resync-shaped witnesses — a definition head parked at
/// depth 1 — never reach a scan here at all: the `Err` leaves the loop, `drain_unless_terminal`
/// takes the tail in one pass, and the parse reports the two diagnostics it already had.
///
/// That is why the same units put its GraphQLx twin at ×4.1 before the allowance landed and left
/// this root at ×2.0. A change that "makes the two dialects symmetrical" adds a quadratic here;
/// this test is what makes such a change arrive as a failure rather than as a slowdown.
#[test]
#[cfg(feature = "graphql")]
fn the_graphql_executable_root_does_not_resynchronise() {
  println!("\n== the exemption: GraphQL's executable root propagates ==");
  // One unit per head this root accepts, and **only** heads it accepts. `[ type ] ` looks like
  // the same probe and is not: `type` is not an executable definition head, so the dispatcher
  // routes it to `recover::unexpected` and never reaches a failing definition — it measures the
  // other helper, at 23 999 items, and is covered by the linearity test above instead. The
  // property here is that no *resync* runs, so the witness has to be a head that gets far enough
  // to fail.
  for unit in [
    "[ query ] ",
    "[ mutation ] ",
    "[ subscription ] ",
    "[ fragment ] ",
    "( query ) ",
  ] {
    let src = unit.repeat(4_000);
    let got = run(gql_executable, &src);
    assert_eq!(got.covered, src.len(), "{unit:?}");
    assert_eq!(
      got.refusals, 0,
      "{unit:?}: the allowance engaged at a root that is not supposed to scan here"
    );
    assert!(
      got.peak_spent <= 1,
      "{unit:?}: a recovery scan ran with {} items already produced. This root gained a \
       resynchronisation point — see `executable_document` in \
       smear-parser/src/graphql/lossless/executable.rs.",
      got.peak_spent
    );
    println!(
      "  {:<16} bytes={:<7} diagnostics={:<4} peak_spent={} refusals=0",
      format!("{unit:?}"),
      src.len(),
      got.diagnostics,
      got.peak_spent
    );
  }
}

#[test]
#[cfg(feature = "graphqlx")]
fn every_graphqlx_census_shape_stays_linear() {
  println!("\n== GraphQLx — `<`/`>` is the fourth pair, and `<` is not a sync point ==");
  for unit in ["! ", "@ ( ) ", "[ type ] ", "( type ) ", "< type > "] {
    assert_linear("gqlx/document", gqlx_document, unit);
    assert_linear("gqlx/type-system", gqlx_type_system, unit);
  }
  // GraphQLx's executable root *does* resynchronise — `executable.rs:361` — which is the
  // divergence `the_graphql_executable_root_does_not_resynchronise` pins from the other side.
  for unit in ["! ", "@ ( ) ", "[ fragment ] ", "< query > "] {
    assert_linear("gqlx/executable", gqlx_executable, unit);
  }
}

/// The guard changed nothing on the shapes it governs.
///
/// Each count was measured on the unfused parser while #168 was being investigated, so it is an
/// oracle. They are all "one diagnostic per junk token", which is what these documents got before
/// the guard and what they get after it: a scan that was going to fail is refused, and refusing it
/// is a no-op on the output.
#[test]
#[cfg(feature = "graphql")]
fn the_guard_did_not_change_the_answer() {
  const N: usize = 2_000;
  println!("\n== identity against the pre-guard counts ==");
  for (unit, per_unit) in [
    ("! ", 1),
    ("( ) ", 2),
    ("@ ( ) ", 3),
    ("[ type ] ", 2),
    ("( type ) ", 2),
  ] {
    let src = unit.repeat(N);
    let got = run(gql_document, &src);
    assert_eq!(
      got.covered,
      src.len(),
      "{unit:?}: the tree's text is no longer the source"
    );
    assert_eq!(
      got.diagnostics,
      N * per_unit,
      "{unit:?}: {} diagnostics against the {} this document produced before the allowance \
       existed. The guard is meant to refuse scans that were going to fail, which changes no \
       output; a different count means it refused one that was going to succeed.",
      got.diagnostics,
      N * per_unit
    );
    println!(
      "  {:<12} diagnostics={:<6} covered={:<7} refusals={}",
      format!("{unit:?}"),
      got.diagnostics,
      got.covered,
      got.refusals
    );
  }
}

/// An honest document never reaches the guard.
///
/// The corpus is this repository's own fixtures plus the densest token streams that can be built —
/// every byte its own token, which is where the one-item-per-byte ceiling the factor is calibrated
/// against is actually *reached* rather than approached.
#[test]
#[cfg(feature = "graphql")]
fn an_honest_corpus_never_reaches_the_guard() {
  println!("\n== honest corpus ==");
  let mut checked = 0usize;
  for dir in ["tests/fixtures/executables", "tests/fixtures/schemas"] {
    let listing = std::fs::read_dir(dir)
      .unwrap_or_else(|e| panic!("the fixture corpus is this test's whole population: {dir}: {e}"));
    for entry in listing.flatten() {
      let path = entry.path();
      if path.extension().and_then(|s| s.to_str()) != Some("graphql") {
        continue;
      }
      let Ok(src) = std::fs::read_to_string(&path) else {
        continue;
      };
      if src.is_empty() {
        continue;
      }
      let got = run(gql_document, &src);
      assert_eq!(got.covered, src.len(), "{}", path.display());
      assert_eq!(
        got.refusals,
        0,
        "{}: the allowance refused {} scans on a repository fixture. Either the factor is too low \
         or something in the parser started re-lexing.",
        path.display(),
        got.refusals
      );
      checked += 1;
    }
  }
  assert!(
    checked > 100,
    "only {checked} fixtures were read; the corpus this test asserts over has gone missing"
  );

  for (name, src) in [
    ("dense `1 1 1 ...`", "1 ".repeat(20_000)),
    ("dense `$a $a ...`", "$a ".repeat(20_000)),
    ("dense `,,,,` (all trivia)", ",".repeat(20_000)),
    (
      "alias probe declines",
      format!("{{ {} }}", "a ".repeat(20_000)),
    ),
    (
      "alias probe accepts",
      format!("{{ {} }}", "a: b ".repeat(20_000)),
    ),
    (
      "non-null probe accepts",
      format!("type T {{ {} }}", "f: Int! ".repeat(20_000)),
    ),
    (
      "comment-dense alias",
      format!("{{ {} }}", "a #c\n: b ".repeat(10_000)),
    ),
    (
      "one honest failed scan at the end",
      format!("{} type T {{ f: ", "type A { b: Int } ".repeat(2_000)),
    ),
    (
      "a long junk run before a real definition",
      format!("{} type U {{ f: Int }}", "! ".repeat(5_000)),
    ),
  ] {
    let got = run(gql_document, &src);
    assert_eq!(got.covered, src.len(), "{name}");
    assert_eq!(
      got.refusals, 0,
      "{name}: the allowance refused {} scans on a document that is linear without it",
      got.refusals
    );
    println!(
      "  {name:<42} bytes={:<7} peak_spent={:<8} refusals=0",
      src.len(),
      got.peak_spent
    );
  }
  println!("  {checked} fixtures, none reached the guard");
}

/// The one input the guard *does* answer differently, kept as a fact rather than a footnote.
///
/// It has to compose two predicates at two depths to exist at all: a resync-quadratic prefix
/// (`[ type ] ` — every definition head parked at depth 1, `[` keeping `unexpected` at a zero
/// skip) burns the allowance, and then a junk run whose only sync point is an `Int` — a sync point
/// that is **not** a definition start, so it cannot rescue the earlier resyncs — asks for a scan
/// that would legitimately have succeeded.
///
/// Unfused this document takes 6.3 s and reports 12 003 diagnostics; with the allowance it takes
/// ~40 ms and reports more, because the junk run past the burnt allowance becomes one `Error` node
/// per token instead of one hole. That is the direction `lossless/recover.rs` already prices as
/// the cheap one — *stopping early costs at most one extra `Error` node; stopping late costs a
/// subtree* — and the lossless text is unchanged either way.
///
/// The number below is pinned so that a change which *widens* the difference is visible. It is not
/// a number anything should be optimised toward.
#[test]
#[cfg(feature = "graphql")]
fn the_falsifier_is_a_known_difference() {
  const UNFUSED_DIAGNOSTICS: usize = 12_003;
  let src = format!("{}{}1", "[ type ] ".repeat(6_000), "! ".repeat(3_000));
  let got = run(gql_document, &src);
  assert_eq!(
    got.covered,
    src.len(),
    "the lossless text must survive the guard"
  );
  assert!(
    got.refusals > 0,
    "the falsifier stopped reaching the guard, so it no longer witnesses anything"
  );
  let extra = got.diagnostics.saturating_sub(UNFUSED_DIAGNOSTICS);
  println!(
    "\n== falsifier ==\n  bytes={} diagnostics={} (unfused {UNFUSED_DIAGNOSTICS}, delta +{extra}) \
     refusals={} {:.1}ms",
    src.len(),
    got.diagnostics,
    got.refusals,
    got.seconds * 1e3
  );
  assert!(
    got.diagnostics >= UNFUSED_DIAGNOSTICS,
    "the guard cannot report fewer diagnostics than the unfused parser: {} < {UNFUSED_DIAGNOSTICS}",
    got.diagnostics
  );
  assert!(
    extra <= 400,
    "the guard's divergence from the unfused parser widened to +{extra} diagnostics, against the \
     +212 recorded when it landed. Something made the allowance blow earlier or stay blown longer \
     — check whether `scan_allowance_exhausted` is still re-derived per call rather than latched."
  );
}
