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
  peak_committed: usize,
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
    peak_committed: scan_allowance::peak_committed(),
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

/// Token **length** is an axis, and the first census had one value of it.
///
/// Every shape above is built from one-byte atoms, so all of them hold the parse's bytes and its
/// produce-events in near-lockstep — and a guard that divided one by the other looked sound over
/// the whole census. It was not. A GraphQL comment runs to end of line, so one produce-event can
/// carry as many bytes as the document likes: alternating a one-byte junk atom with an `L`-byte
/// comment grew committed bytes ~`L` per trip while the meter grew ~1, and the allowance outran it
/// by a factor of `L`. Measured on the byte-denominated guard, at `L = 1024`, `k = 4000`: **zero**
/// refusals, and the sqrt-scaled variant ran 64 MB in 106 189 ms, growing ×7.1 in time for every
/// ×4 in bytes.
///
/// So the assertion is not "still linear" — it is that **the meter does not move when only token
/// length moves**. At fixed `k` the parse commits the same number of items whatever the padding,
/// so `peak_spent` must be flat across `pad`; under the byte denominator it read 35 981 at
/// `pad = 0` and 8 011 993 at `pad = 1024`. Flatness is the property; linearity follows from it
/// and is checked too.
#[test]
#[cfg(feature = "graphql")]
fn token_length_does_not_reopen_the_guard() {
  /// `k` copies of `atom` each followed by a comment carrying `pad` filler bytes.
  fn padded(atom: &str, k: usize, pad: usize) -> String {
    let comment = format!("#{}\n", "x".repeat(pad));
    let mut src = String::with_capacity(k * (atom.len() + comment.len()));
    for _ in 0..k {
      src.push_str(atom);
      src.push_str(&comment);
    }
    src
  }

  println!("\n== token length as an axis ==");
  for atom in ["! ", "@ ( ) ", "[ type ] "] {
    const K: usize = 1_500;
    let mut readings = Vec::new();
    for pad in [0usize, 16, 256, 2_048] {
      let src = padded(atom, K, pad);
      let got = run(gql_document, &src);
      assert_eq!(got.covered, src.len(), "{atom:?}/pad={pad}");
      assert!(
        got.refusals > 0,
        "{atom:?}/pad={pad}: the guard never engaged on a shape built to be quadratic. This is \
         the byte-denominated defect exactly: {} bytes committed against {} items, so a \
         denominator counting bytes would have paid for every scan.",
        src.len(),
        got.peak_committed
      );
      println!(
        "  {:<12} pad={pad:5} bytes={:9} spent={:8} committed={:7} refusals={:6} {:7.1}ms",
        format!("{atom:?}"),
        src.len(),
        got.peak_spent,
        got.peak_committed,
        got.refusals,
        got.seconds * 1e3
      );
      readings.push((pad, src.len(), got.peak_spent));
    }

    // Flatness. The committed item count at fixed `k` is the same whatever the padding — one
    // comment and one newline per copy, however long the comment is — so the meter must be too.
    let (_, _, base) = readings[0];
    for &(pad, bytes, spent) in &readings {
      let drift = spent as f64 / base as f64;
      assert!(
        (0.5..2.0).contains(&drift),
        "{atom:?}: padding the comments to {pad} bytes moved the meter x{drift:.2} \
         ({base} -> {spent}) while the committed item count did not move. The guard is being fed \
         a length-sensitive denominator again — it must count produce-events, not bytes. \
         (document was {bytes} bytes)"
      );
    }
  }

  // And the shape the byte denominator made worst: padding scaled with the document, which is
  // where the two arms of `min(trips, factor * bytes_per_event)` cross.
  println!("  -- padding scaled with the document --");
  let mut prev: Option<(usize, usize)> = None;
  for k in [500usize, 1_000, 2_000] {
    let src = padded("! ", k, k);
    let got = run(gql_document, &src);
    assert_eq!(got.covered, src.len());
    if let Some((pk, ps)) = prev {
      let ratio = got.peak_spent as f64 / ps as f64;
      assert!(
        ratio < 2.6,
        "padding scaled with the document: the meter grew x{ratio:.2} for a x{} document \
         ({ps} -> {}). This is the axis the one-byte census could not see.",
        k / pk,
        got.peak_spent
      );
    }
    println!(
      "    k={k:5} pad={k:5} bytes={:9} spent={:8} committed={:7} refusals={:6} {:7.1}ms",
      src.len(),
      got.peak_spent,
      got.peak_committed,
      got.refusals,
      got.seconds * 1e3
    );
    prev = Some((k, got.peak_spent));
  }
}

/// Error density is the allowance's second exception, and this is its price.
///
/// tokora charges `spent` for every item the lexer produces, errors included. smear's tally does
/// not: `tt_hook_and_then` increments through `Result::inspect`, which runs on `Ok` alone, and the
/// rules routed through it include `-`, `+`, `.` and `..`, which never succeed. So a malformed
/// document moves the numerator and not the denominator, and the ratio inflates without bound —
/// measured at **514** for one `!` per 256 `-`.
///
/// Almost none of that is reachable. A run of bad lexemes never becomes tokens the parser recovers
/// from, so a document dense in them makes no recovery call at all; and where the ratio does
/// inflate, the scans it refuses were going to fail anyway. The reachable cost needs an error run
/// long enough to blow the allowance *followed by* a junk run whose scan would have succeeded.
///
/// # The rate is a function of the junk, not a constant
///
/// Refusals continue while `k + c > FACTOR * c + floor`, so they stop at
/// `c = (k - floor) / (FACTOR - 1)` committed items. A junk run committing `m` items per refusal
/// therefore takes `(k - floor) / ((FACTOR - 1) * m)` of them — and **`m` is set by the junk**.
///
/// The first version of this test pinned `beyond <= k / 8`, derived from `! `, which commits a
/// bang and a space per refusal (`m = 2`). A dense `!!!!` suffix commits one, doubles the count,
/// and **broke that pin at k = 33 000** — precisely where `k/7 - 585 > k/8`. So the shapes below
/// lead with the dense one, and the assertion is the formula rather than a constant a single
/// witness happens to satisfy.
#[test]
#[cfg(feature = "graphql")]
fn error_density_is_a_bounded_known_cost() {
  const JUNK: usize = 120_000;

  /// `(k - floor) / ((FACTOR - 1) * m)`, the refusal count the mechanism predicts.
  fn predicted(k: usize, m: usize) -> f64 {
    k.saturating_sub(4_096) as f64 / (7.0 * m as f64)
  }

  println!("\n== error density: the second exception, priced by mechanism ==");
  println!(
    "  {:<8} {:>3} {:>8} {:>9} {:>9} {:>10} {:>8}",
    "junk", "m", "k", "refusals", "predicted", "beyond", "drift"
  );
  // `m` is committed items per refusal: dense junk commits one, spaced junk commits two.
  for (junk, m) in [("!", 1usize), ("@", 1), (":", 1), ("! ", 2), ("@ ", 2)] {
    for k in [0usize, 1_000, 4_000] {
      // Below the floor the guard must not engage at all. This is the guarantee that matters for
      // a merely-malformed document, and it is an equality rather than a bound.
      let src = format!("{}{}1", "-".repeat(k), junk.repeat(3_000));
      let got = run(gql_document, &src);
      assert_eq!(got.covered, src.len());
      assert_eq!(
        got.refusals, 0,
        "{junk:?} k={k}: the guard engaged under SCAN_ALLOWANCE_FLOOR"
      );
      assert_eq!(
        got.diagnostics.saturating_sub(k),
        3,
        "{junk:?} k={k}: recovery must be fully intact below the floor"
      );
    }
    for k in [20_000usize, 33_000, 80_000] {
      let src = format!("{}{}1", "-".repeat(k), junk.repeat(JUNK));
      let got = run(gql_document, &src);
      assert_eq!(got.covered, src.len(), "{junk:?} k={k}");
      let want = predicted(k, m);
      let drift = got.refusals as f64 / want;
      println!(
        "  {:<8} {m:>3} {k:>8} {:>9} {:>9.0} {:>10} {:>8.3}",
        format!("{junk:?}"),
        got.refusals,
        want,
        got.diagnostics.saturating_sub(k),
        drift
      );
      assert!(
        (0.9..1.1).contains(&drift),
        "{junk:?} k={k}: {} refusals against the {want:.0} the mechanism predicts (x{drift:.3}). \
         The rate is `(k - floor) / ((FACTOR - 1) * m)` with m = committed items per refusal; a \
         drift here means either a constant changed or the junk stopped committing {m} per \
         refusal. Do not re-derive this from one shape — that is what broke the last pin.",
        got.refusals
      );
      assert!(
        got.refusals < JUNK,
        "{junk:?} k={k}: the whole junk run was shredded rather than a slice of it, so the guard \
         is not re-closing as committed items accrue."
      );
    }
  }
}

/// `m` — committed items per refusal — is at least one, so the guard always re-closes.
///
/// If a refusal could commit nothing the denominator would never move and the guard would latch,
/// which is the difference between a bounded degradation and a permanent one. It cannot:
/// `unexpected`'s no-progress fallback consumes exactly one token through `try_expect`, and a
/// consumed token is one the lexer produced *and* the tally counted, because error lexemes never
/// reach the parser — the scanner absorbs them into diagnostics on the way past.
///
/// The measurement is what says the floor of 1 is reached rather than approached, which is what
/// makes the worst case in `error_density_is_a_bounded_known_cost` the worst case. It reads the
/// **minimum gap between consecutive refusals**, not the average over the run: an average of 1.008
/// is consistent with one zero-commit refusal among six thousand, and one is all it would take to
/// freeze the denominator.
#[test]
#[cfg(feature = "graphql")]
fn every_refusal_commits_at_least_one_item() {
  println!("\n== committed items per refusal ==");
  for junk in ["!", "! ", "@", ":", "=", "|", "&", "!@:=|&", "()", "( )"] {
    let src = format!("{}{}", "-".repeat(20_000), junk.repeat(6_000));
    let got = run(gql_document, &src);
    assert_eq!(got.covered, src.len(), "{junk:?}");
    assert!(
      got.refusals > 0,
      "{junk:?}: no refusal, so this constrains nothing"
    );
    // The MINIMUM gap between two consecutive refusals, not the average over thousands of them.
    // An average cannot fail on a single zero-commit refusal, which is the only thing that would
    // freeze the denominator, so an average asserts almost nothing here.
    let min_gap = scan_allowance::min_commit_between_refusals().unwrap_or_else(|| {
      panic!("{junk:?}: fewer than two refusals, so there is no gap to measure")
    });
    println!(
      "  {:<10} refusals={:6} committed={:7} min gap={min_gap} (avg {:.3})",
      format!("{junk:?}"),
      got.refusals,
      got.peak_committed,
      got.peak_committed as f64 / got.refusals as f64
    );
    assert!(
      min_gap >= 1,
      "{junk:?}: two consecutive refusals with {min_gap} committed items between them. At zero \
       the denominator stops moving and the guard latches instead of clearing — the degradation \
       would be permanent rather than proportional to the error run."
    );
  }
}

/// The guard re-opening cannot be turned back into superlinearity.
///
/// Self-clearing is what keeps the error-density cost proportional, and read from the other side it
/// is an invitation: alternate cheap commits with expensive failed scans and the allowance refills
/// on purpose. It cannot win, and the reason is the denominator rather than any property of the
/// shapes below — scanning is permitted only while `spent <= FACTOR * committed + floor`, and
/// `committed` can never exceed the number of items the document contains.
///
/// # Why this doubles instead of reading `spent / committed`
///
/// Because that ratio is not a work bound. The first version of this test asserted `ratio < 12.0`,
/// from 8.91 measured on four **error-free** constructions plus margin. On an error-dense one — the
/// axis this same file already carries — the identical metric reads **73 -> 93 -> 109 -> 118**
/// across four doublings *while `spent` doubles at x1.99, x1.99, x2.00*. The work is exactly linear
/// and the metric grows 1.6x over the same range, so the gate would have failed with no defect
/// present: the numerator counts error lexemes the denominator does not, and their ratio drifts
/// with error density by construction.
///
/// Doubling the construction and watching the **numerator** is the mechanism. It is the same
/// instrument `assert_linear` uses on the census, and it carries no constant belonging to a shape.
#[test]
#[cfg(feature = "graphql")]
fn the_guard_cannot_be_refilled_into_superlinearity() {
  const BASE: usize = 2_000;
  const NAMES: [&str; 5] = [
    "burn then junk",
    "valid definition alternating with a burn unit",
    "20 valid definitions per junk burst",
    "a large commit, then a resync-quadratic tail",
    "error-dense junk (the axis the ratio form missed)",
  ];

  fn build(which: usize, k: usize) -> String {
    match which {
      0 => format!("{}{}1", "[ type ] ".repeat(k), "! ".repeat(k)),
      1 => "type T { f: Int } [ type ] ".repeat(k),
      2 => format!("{}[ type ] ", "type T { f: Int } ".repeat(20)).repeat(k / 20 + 1),
      3 => format!(
        "{}{}",
        "type T { f: Int } ".repeat(k * 4),
        "[ type ] ".repeat(k)
      ),
      // The error-density axis, which is what broke the ratio form.
      _ => format!(
        "{}{}",
        "! ".repeat(300),
        format!("{}!", "-".repeat(64)).repeat(k)
      ),
    }
  }

  println!("\n== alternating progress and scans, by doubling ==");
  for (which, name) in NAMES.iter().enumerate() {
    let small_src = build(which, BASE);
    let large_src = build(which, BASE * 2);
    let small = run(gql_document, &small_src);
    let large = run(gql_document, &large_src);
    assert_eq!(small.covered, small_src.len(), "{name}");
    assert_eq!(large.covered, large_src.len(), "{name}");
    assert!(
      small.peak_spent > 10_000 && large.peak_spent > 10_000,
      "{name}: peak spend {} / {}, too small for the ratio below to mean anything",
      small.peak_spent,
      large.peak_spent
    );
    let ratio = large.peak_spent as f64 / small.peak_spent as f64;
    println!(
      "  {name:<50} spent {:>9} -> {:>9}  x{ratio:.2}  refusals {} -> {}",
      small.peak_spent, large.peak_spent, small.refusals, large.refusals
    );
    assert!(
      ratio < 2.6,
      "{name}: doubling the construction grew the lexing x{ratio:.2} ({} -> {}). An alternating \
       shape is refilling the allowance faster than it costs, which puts the total back above \
       FACTOR * T.",
      small.peak_spent,
      large.peak_spent
    );
  }
}

/// What the guard can do to the output, bounded in **both** directions.
///
/// A refusal replaces one committed hole with the fallback's single `Error` node, trading the
/// hole's skipped-region note for whatever the fallback emits. Which way that nets is a property of
/// the junk run's *length*, not of the guard:
///
/// - a **long** run becomes many one-token `Error` nodes, each with its own report, so the count
///   goes **up**. The pinned falsifier is this case, at `+123`;
/// - a **one-token** run has its skipped-region note suppressed with no second token for a
///   replacement report to attach to, so the count goes **down**. `"[ type ] "xk` then `"! 1 "xn`
///   is this case, at `-242 / -543 / -843`.
///
/// The first version of this test asserted `fused >= unfused` and passed, because no census row was
/// chopped fine. That is the same blindness the numbers in this file were corrected for twice
/// already — here inside the gate written to bound the previous instance — so the assertion is now
/// the two-sided bound `|delta| <= refusals`, which is what the mechanism actually gives.
///
/// The oracle is **derived per unit**, not read off a neighbouring size: `[ type ] ` reports 2 and
/// `! 1 ` reports 3. `refusal_free_sizes_calibrate_the_oracle` is what keeps that honest.
#[test]
#[cfg(feature = "graphql")]
fn the_guard_changes_diagnostics_by_at_most_one_per_refusal() {
  println!("\n== output preservation, two-sided, over the census ==");
  let mut cases: Vec<(String, usize)> = vec![
    ("! ".repeat(2_000), 2_000),
    ("( ) ".repeat(2_000), 4_000),
    ("@ ( ) ".repeat(2_000), 6_000),
    ("[ type ] ".repeat(2_000), 4_000),
    ("( type ) ".repeat(2_000), 4_000),
    ("type T { f: Int } ".repeat(2_000), 0),
    (format!("{} type U {{ f: Int }}", "! ".repeat(3_000)), 2),
    (
      format!("{}{}1", "[ type ] ".repeat(6_000), "! ".repeat(3_000)),
      12_003,
    ),
  ];
  // The fine-chopped family, where the sign flips.
  for kn in [2_000usize, 4_000, 6_000] {
    cases.push((
      format!("{}{}", "[ type ] ".repeat(kn), "! 1 ".repeat(kn)),
      2 * kn + 3 * kn,
    ));
  }
  let mut saw_negative = false;
  for (src, unfused) in &cases {
    let got = run(gql_document, src);
    assert_eq!(
      got.covered,
      src.len(),
      "the lossless text must survive the guard"
    );
    let delta = got.diagnostics as i64 - *unfused as i64;
    saw_negative |= delta < 0;
    assert!(
      delta.unsigned_abs() as usize <= got.refusals,
      "|delta| = {} over {} refusals. Each refusal trades exactly one hole for one fallback node, \
       so the change cannot exceed the refusal count in either direction; more than that means \
       the guard is altering output somewhere other than the scan it declined.",
      delta.unsigned_abs(),
      got.refusals
    );
    if got.refusals == 0 {
      assert_eq!(
        got.diagnostics, *unfused,
        "the guard refused nothing and still changed the answer"
      );
    }
    println!(
      "  unfused={unfused:<7} fused={:<7} delta={delta:<6} refusals={:<7} bytes={}",
      got.diagnostics,
      got.refusals,
      src.len()
    );
  }
  assert!(
    saw_negative,
    "no case reported FEWER diagnostics than the unfused parser, so the fine-chopped family has \
     stopped exercising the direction this test exists to cover and the bound is only being \
     checked on one side again."
  );
}

/// The oracle above is derived, so this is what makes it an oracle.
///
/// `2k + 3n` has to be exact where the guard does not fire; if it is not, the deltas it produces
/// where the guard does fire are measuring the arithmetic rather than the guard.
#[test]
#[cfg(feature = "graphql")]
fn refusal_free_sizes_calibrate_the_oracle() {
  println!("\n== oracle calibration ==");
  for (k, n) in [(0usize, 50usize), (0, 200), (0, 2_000)] {
    let src = format!("{}{}", "[ type ] ".repeat(k), "! 1 ".repeat(n));
    let got = run(gql_document, &src);
    assert_eq!(got.refusals, 0, "k={k} n={n}: not a refusal-free size");
    assert_eq!(
      got.diagnostics,
      2 * k + 3 * n,
      "k={k} n={n}: the derived per-unit counts (2 for `[ type ] `, 3 for `! 1 `) no longer match \
       an unguarded parse, so every delta derived from them is wrong."
    );
    println!(
      "  k={k:5} n={n:5} diagnostics={} = 2k+3n, refusals=0",
      got.diagnostics
    );
  }
}

/// Encoding is an axis too, and the byte-denominated guard would have failed on it.
///
/// Every other shape in this file is pure ASCII, where one token is one byte and the two
/// denominators are indistinguishable. A comment full of 4-byte characters carries four times the
/// bytes for the same number of events — the same lever as the comment-length axis, reached
/// through the character set instead of through the length.
///
/// The assertion is equality, not a bound: the guard counts events, so changing only the encoding
/// must move **nothing**.
#[test]
#[cfg(feature = "graphql")]
fn the_guard_is_blind_to_encoding() {
  const K: usize = 2_000;
  println!("\n== encoding ==");
  let mut baseline: Option<(usize, usize, usize)> = None;
  // Twelve comment CHARACTERS in each, so the event counts are identical by construction and the
  // byte counts differ 16 / 40 / 52. Making the *bytes* equal instead would vary the character set
  // without varying the lever, and would pass against a byte denominator too.
  for (name, unit) in [
    ("ascii", format!("! #{}\n", "x".repeat(12))),
    ("3-byte chars", format!("! #{}\n", "\u{4E2D}".repeat(12))),
    ("4-byte chars", format!("! #{}\n", "\u{1F600}".repeat(12))),
  ] {
    let src = unit.repeat(K);
    let got = run(gql_document, &src);
    assert_eq!(got.covered, src.len(), "{name}");
    let reading = (got.peak_spent, got.peak_committed, got.refusals);
    println!(
      "  {name:<14} bytes={:8} spent={:8} committed={:8} refusals={}",
      src.len(),
      reading.0,
      reading.1,
      reading.2
    );
    match baseline {
      None => baseline = Some(reading),
      Some(want) => assert_eq!(
        reading, want,
        "{name}: changing only the character set moved the guard's readings. It counts \
         produce-events, so it must be blind to how many bytes each one spans — a reading that \
         moves here is a byte-denominated denominator returning."
      ),
    }
  }
}

/// The `*_with_limits` doors reach the same guard as the default one.
///
/// #181 on this branch was exactly this omission — a census that covered the default doors and
/// left the `*_with_limits` ones out — so the axis is asserted rather than reasoned about. The
/// ceiling those doors carry is the recursion budget, which has nothing to do with the allowance;
/// the point is that "has nothing to do with" is the sentence #181 was written against.
#[test]
#[cfg(feature = "graphql")]
fn the_with_limits_doors_reach_the_same_guard() {
  use smear::lexer::limits::LosslessLimits;

  println!("\n== the `*_with_limits` doors ==");
  let src = "( type ) ".repeat(3_000);
  let want = {
    let got = run(gql_document, &src);
    (
      got.peak_spent,
      got.peak_committed,
      got.refusals,
      got.diagnostics,
    )
  };
  for depth in [8usize, 64, 256, 1_024] {
    scan_allowance::reset();
    let parse = smear::parser::graphql::lossless::parse_document_with_limits(
      &src,
      LosslessLimits::with_max_nesting_depth(depth),
    );
    let got = (
      scan_allowance::peak_spent(),
      scan_allowance::peak_committed(),
      scan_allowance::refusals(),
      parse.diagnostics().len(),
    );
    assert_eq!(
      parse.syntax().text().to_string().len(),
      src.len(),
      "max_nesting_depth={depth}"
    );
    assert_eq!(
      got, want,
      "max_nesting_depth={depth} reached the allowance differently from the default door. The \
       recursion ceiling and the scan allowance are separate budgets; if raising one moves the \
       other, one of them is being read through the wrong cell."
    );
    println!(
      "  max_nesting_depth={depth:5} spent={} committed={} refusals={}",
      got.0, got.1, got.2
    );
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
    // The denominator counts produce-events, so a document that is mostly *one* very long token
    // has very few of them. These are the shapes that would fire a guard whose denominator was
    // too small, the mirror of the byte-denominated defect.
    // Both of these end in a junk atom so that recovery actually runs: without it the document
    // parses clean, `peak_spent` reads 0 and the assertion below passes without testing anything.
    (
      "one 1 MB comment, then junk",
      format!("#{}\n{{ a }} !", "x".repeat(1_000_000)),
    ),
    (
      "one 1 MB block string, then junk",
      format!("{{ f(a: \"\"\"{}\"\"\") }} !", "x".repeat(1_000_000)),
    ),
    (
      "100 KB comments between definitions, then junk",
      format!(
        "{} !",
        format!("#{}\ntype T {{ f: Int }}\n", "x".repeat(100_000)).repeat(10)
      ),
    ),
    (
      "long comments between real definitions",
      "#cccccccccccccccccccccccccccccccccccccc\ntype T { f: Int }\n".repeat(2_000),
    ),
    (
      "a long comment run then a truncated definition",
      format!(
        "{} type T {{ f: ",
        "#cccccccccccccccccccccccccccccccccccccc\n".repeat(2_000)
      ),
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
      "  {name:<46} bytes={:<8} spent={:<8} committed={:<8} refusals=0{}",
      src.len(),
      got.peak_spent,
      got.peak_committed,
      if got.peak_spent == 0 {
        "   (parsed clean — reached no recovery call, so it constrains nothing)"
      } else {
        ""
      }
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
  // Signed, because the direction is a property of the junk run's length rather than of the guard
  // — `the_guard_changes_diagnostics_by_at_most_one_per_refusal` carries the family where the same
  // mechanism nets NEGATIVE. This shape's run is long, so it nets positive.
  let delta = got.diagnostics as i64 - UNFUSED_DIAGNOSTICS as i64;
  println!(
    "\n== falsifier ==\n  bytes={} diagnostics={} (unfused {UNFUSED_DIAGNOSTICS}, delta {delta:+}) \
     refusals={} {:.1}ms",
    src.len(),
    got.diagnostics,
    got.refusals,
    got.seconds * 1e3
  );
  assert!(
    delta.unsigned_abs() as usize <= got.refusals,
    "|delta| = {} over {} refusals, which breaks the one-per-refusal bound the whole census is \
     held to.",
    delta.unsigned_abs(),
    got.refusals
  );
  assert!(
    (0..=400).contains(&delta),
    "the guard's divergence on this shape moved to {delta:+}, against the +123 recorded when the \
     event-denominated guard landed. Something made the allowance blow earlier or stay blown \
     longer — check that `scan_allowance_exhausted` is still re-derived per call rather than \
     latched. (An unqualified `>= unfused` used to stand here and was wrong in general: see the \
     two-sided test.)"
  );
}
