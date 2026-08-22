#![cfg(feature = "parser")]

//! The nesting ceiling, and the `SIGABRT` it exists to stop — smear issue #61.
//!
//! # What was wrong
//!
//! A **valid** GraphQL document of about a kilobyte — `{ ... on Query { ... on Query { … } } }`
//! nested 58 deep — overflowed the native stack of a 2 MiB thread and killed the process. Nothing
//! rejected it and there was no diagnostic, because there was no return. The only ceiling in the
//! path was tokora's *general-purpose* 500 on the lexer's bracket counter, inherited rather than
//! chosen, and 500 sat an order of magnitude above where the stack actually died, so it could not
//! fire first.
//!
//! A `SIGABRT` is not catchable. `catch_unwind` does not see it, a server cannot turn it into a
//! 400, and one request takes every other request on the process with it.
//!
//! # How this suite can hold that without killing the runner
//!
//! It is the same hazard: a regression here is a process abort, and an abort does not fail a test,
//! it kills the harness. Two things keep this file out of that, and the first is not in this file
//! at all:
//!
//! 1. **`MAX_NESTING_DEPTH` cannot be raised past its own derivation, at compile time.** A `const`
//!    assertion beside the constant fails the *build* if it is. That is not where the check
//!    naturally wants to live, and it is there because the obvious placement was measured and
//!    found not to work: a runtime test asserting the same arithmetic was planted against
//!    `MAX_NESTING_DEPTH = 200`, and the harness died with `SIGABRT` in a *different* test in this
//!    file before the arithmetic one ran. libtest gives no ordering, so no test can guard a
//!    constant that makes its siblings lethal.
//! 2. **Given (1), nothing here is deep.** Every boundary assertion sits at the ceiling and one
//!    past it — a few dozen brackets, which no stack notices — and
//!    [`deep_input_returns_rather_than_aborting`] re-checks the boundary as its own first
//!    statement before handing the parser anything deep.
//!
//! # The reproduction proper
//!
//! Deliberately not here. Bisecting a stack overflow needs one parse per *process*, on an
//! explicitly sized thread, with the bisection reading the child's exit status — a shape a test
//! harness cannot host. `MAX_NESTING_DEPTH`'s own documentation carries the resulting table.

use smear::lexer::limits::MAX_NESTING_DEPTH;

/// `{ ... on Query { … } }` nested `depth` deep: `depth + 1` simultaneously open braces.
///
/// The shape from the issue, and the most expensive one measured — an inline fragment spends more
/// native stack per level than a plain field selection, a list value or an input object.
fn inline_fragments(depth: usize) -> String {
  let mut src = String::with_capacity(depth * 16 + 32);
  src.push('{');
  for _ in 0..depth {
    src.push_str(" ... on Query {");
  }
  src.push_str(" __typename ");
  for _ in 0..depth {
    src.push('}');
  }
  src.push('}');
  src
}

/// The greatest `depth` [`inline_fragments`] may use and stay inside the budget.
const DEEPEST_ACCEPTED: usize = MAX_NESTING_DEPTH - 1;

/// The ceiling is [`MAX_NESTING_DEPTH`] open brackets, and it is what an **unconfigured** parse
/// gets.
///
/// "Unconfigured" is the whole point of #61's fix. The number does not arrive through an argument
/// a caller has to remember: it is the `Default` of the lexer's own state type, which is what
/// `Parser::with_parser(…).parse_str(src)` — the form this workspace's README, `smear-compiler`'s
/// crate docs and `smear-schema`'s builder all use — seeds a parse with. A ceiling that only
/// applied when asked for would have left the abort exactly where it was.
#[cfg(feature = "graphql")]
#[test]
fn the_syntactic_door_reports_at_the_ceiling_instead_of_descending() {
  use smear::{
    lexer::tokora::{Parse as _, Parser},
    parser::graphql::{
      GraphQL,
      ast::Document,
      error::GraphqlErrors,
      syntactic::{GraphqlLexer, document},
    },
  };

  fn parse(src: &str) -> bool {
    Parser::with_parser::<GraphqlLexer<'_, str>, Document<&str>, GraphqlErrors<&str>, _, GraphQL>(
      document,
    )
    .parse_str(src)
    .is_ok()
  }

  assert!(
    parse(&inline_fragments(DEEPEST_ACCEPTED)),
    "{MAX_NESTING_DEPTH} open brackets is inside the budget and must parse"
  );
  assert!(
    !parse(&inline_fragments(DEEPEST_ACCEPTED + 1)),
    "{} open brackets must be refused, and refused by returning",
    MAX_NESTING_DEPTH + 1
  );
}

/// The GraphQLx syntactic door answers identically.
///
/// It is a separate lexer with its own SIMD dispatch loop, and it is the *worse* of the two per
/// level — 53 against GraphQL's 57 on a 2 MiB debug thread — so a ceiling proved only on GraphQL
/// would be proved on the wrong dialect.
#[cfg(feature = "graphqlx")]
#[test]
fn the_graphqlx_syntactic_door_reports_at_the_same_ceiling() {
  use smear::{
    lexer::tokora::{Parse as _, Parser},
    parser::graphqlx::{
      GraphQLx,
      ast::Document,
      error::GraphqlxErrors,
      syntactic::{GraphqlxLexer, document},
    },
  };

  fn parse(src: &str) -> bool {
    Parser::with_parser::<GraphqlxLexer<'_, str>, Document<&str>, GraphqlxErrors<&str>, _, GraphQLx>(
      document,
    )
    .parse_str(src)
    .is_ok()
  }

  assert!(parse(&inline_fragments(DEEPEST_ACCEPTED)));
  assert!(!parse(&inline_fragments(DEEPEST_ACCEPTED + 1)));
}

/// The lossless door reports the trip on the diagnostic channel and still covers every byte.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn the_lossless_door_reports_at_the_same_ceiling() {
  use smear::parser::graphql::lossless::parse_document;

  let inside = inline_fragments(DEEPEST_ACCEPTED);
  let parse = parse_document(&inside);
  assert!(!parse.has_errors());
  assert!(
    parse.diagnostics().is_empty(),
    "inside the budget the ceiling must be silent, not merely non-fatal"
  );

  let over = inline_fragments(DEEPEST_ACCEPTED + 1);
  let parse = parse_document(&over);
  assert!(parse.has_errors());
  assert_eq!(
    parse.syntax().text().to_string(),
    over,
    "the lossless guarantee survives the trip"
  );
}

/// A caller who wants a different ceiling gets one, in both directions.
///
/// This is the half of #61 that says the number must be a decision a deployment can revisit: a
/// server on an 8 MiB main thread can afford roughly four times the default, and a worker
/// deliberately spawned smaller can afford less. Both directions are asserted because a knob that
/// only loosens is not a knob.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn the_ceiling_is_configurable_in_both_directions() {
  use smear::{
    lexer::limits::LosslessLimits, parser::graphql::lossless::parse_document_with_limits,
  };

  // Deeper than the default, accepted because the caller raised the ceiling.
  let deeper = inline_fragments(MAX_NESTING_DEPTH * 2);
  assert!(parse_document_with_limits(&deeper, LosslessLimits::default()).has_errors());
  assert!(
    !parse_document_with_limits(
      &deeper,
      LosslessLimits::with_max_nesting_depth(MAX_NESTING_DEPTH * 4)
    )
    .has_errors(),
    "a caller on a larger stack must be able to raise the ceiling"
  );

  // Shallower than the default, refused because the caller lowered it.
  let shallow = inline_fragments(3);
  assert!(!parse_document_with_limits(&shallow, LosslessLimits::default()).has_errors());
  assert!(
    parse_document_with_limits(&shallow, LosslessLimits::with_max_nesting_depth(2)).has_errors(),
    "a caller on a smaller stack must be able to lower the ceiling"
  );
}

/// A raised ceiling is honoured up to [`HARD_MAX`] and **clamped** above it.
///
/// # What this pins that [`the_ceiling_is_configurable_in_both_directions`] does not
///
/// That test asks only whether raising works at all, at `MAX_NESTING_DEPTH * 4`. It cannot see
/// the wall, and until `parse_lossless_with_context` existed there was no wall of smear's to see:
/// a lossless parse ran under tokora's own `PARSE_DEFAULT_DEPTH`, a number this workspace does not
/// choose and upstream moved twice inside one unreleased window (64, then 16, then 32) with no
/// compile error anywhere. The effective ceiling was `min(what you asked for, whatever tokora
/// currently defaults to)`, so "how deep can a caller actually go" had no answer this file could
/// assert.
///
/// It does now: the doors install `min(ceiling, HARD_MAX)` as **the** recursion budget, so the two
/// halves below are the whole contract. Above the wall the refusal's *position* is the evidence —
/// `HARD_MAX * 4` bytes in, not the requested ceiling's — because a count alone cannot tell a
/// clamp from a coincidence.
///
/// # Why the deep cells here are safe, and why that is not a judgement call
///
/// This file's header says nothing in it is deep, for a reason: past the native boundary a test
/// does not go red, it aborts the harness. These cells reach `HARD_MAX + 1`, which is deeper than
/// anything else here — and it is bounded by the same `const` assertion that makes `HARD_MAX`
/// shippable at all. `HARD_MAX * 1.9 <= 671`, the measured lossless boundary, is checked at
/// compile time beside the constant, so a `HARD_MAX` raised past what the bisection supports fails
/// to *build* rather than killing this runner. The depth here cannot outrun the constant, and the
/// constant cannot outrun the measurement.
#[cfg(all(feature = "rowan", feature = "graphql", feature = "graphqlx"))]
#[test]
fn a_raised_ceiling_is_honoured_to_hard_max_and_clamped_above_it() {
  use smear::lexer::limits::{HARD_MAX, LosslessLimits};

  /// One nesting delimiter per level, four bytes per level, so a refusal entering level `n + 1`
  /// reports at byte `n * 4`.
  fn sel(depth: usize) -> String {
    format!("{}{}", "{ f ".repeat(depth), " }".repeat(depth))
  }

  type Door = fn(&str, LosslessLimits) -> (usize, String, Option<core::ops::Range<usize>>);

  fn gql(src: &str, limits: LosslessLimits) -> (usize, String, Option<core::ops::Range<usize>>) {
    let p = smear::parser::graphql::lossless::parse_document_with_limits(src, limits);
    (
      p.diagnostics().len(),
      p.syntax().text().to_string(),
      p.diagnostics().first().map(|d| d.span()),
    )
  }
  fn glx(src: &str, limits: LosslessLimits) -> (usize, String, Option<core::ops::Range<usize>>) {
    let p = smear::parser::graphqlx::lossless::parse_document_with_limits(src, limits);
    (
      p.diagnostics().len(),
      p.syntax().text().to_string(),
      p.diagnostics().first().map(|d| d.span()),
    )
  }

  let mut cells = 0usize;
  for (dialect, door) in [("graphql", gql as Door), ("graphqlx", glx as Door)] {
    // ── HONOURED: a raise below the wall buys exactly the depth it asks for ───────────────────
    //
    // The ceiling is `MAX_NESTING_DEPTH * 4`, which is the raise this crate's own documentation
    // promises an 8 MiB caller. Both halves are asserted because a ceiling that only ever accepts
    // is not a ceiling.
    let raised = MAX_NESTING_DEPTH * 4;
    assert!(
      raised < HARD_MAX,
      "the honoured cells must sit below the wall"
    );

    let inside = sel(raised);
    let (count, text, _) = door(&inside, LosslessLimits::with_max_nesting_depth(raised));
    assert_eq!(
      count, 0,
      "{dialect}: {raised} levels under a ceiling of {raised} must be accepted"
    );
    assert_eq!(text, inside, "{dialect}: the lossless guarantee survives");
    cells += 1;

    let over = sel(raised + 1);
    let (count, text, _) = door(&over, LosslessLimits::with_max_nesting_depth(raised));
    assert!(
      count > 0,
      "{dialect}: {} levels under a ceiling of {raised} must be refused",
      raised + 1
    );
    assert_eq!(text, over, "{dialect}: the lossless guarantee survives");
    cells += 1;

    // ── CLAMPED: a raise above the wall buys the wall, and the span says so ───────────────────
    //
    // `HARD_MAX * 8` is far above the wall AND far above the depths below, so the lexer's own
    // tally — which reads the unclamped number and is the cheaper check — cannot fire first. What
    // refuses here is therefore the parse, at the clamped budget.
    let asked = HARD_MAX * 8;
    let limits = LosslessLimits::with_max_nesting_depth(asked);

    let at_wall = sel(HARD_MAX);
    let (count, text, _) = door(&at_wall, limits);
    assert_eq!(
      count, 0,
      "{dialect}: HARD_MAX ({HARD_MAX}) levels must be accepted when the caller asked for {asked}"
    );
    assert_eq!(text, at_wall, "{dialect}: the lossless guarantee survives");
    cells += 1;

    let past_wall = sel(HARD_MAX + 1);
    let (count, text, first) = door(&past_wall, limits);
    assert_eq!(
      count, 1,
      "{dialect}: one past HARD_MAX must be one refusal, not {count}"
    );
    assert_eq!(
      text, past_wall,
      "{dialect}: the lossless guarantee survives"
    );
    // THE CLAMP ITSELF. Four bytes a level, so a refusal entering level `HARD_MAX + 1` reports at
    // `HARD_MAX * 4`. A door that honoured the request instead would not refuse at all here, and
    // one that refused at some other number is enforcing something that is not HARD_MAX.
    assert_eq!(
      first,
      Some(HARD_MAX * 4..HARD_MAX * 4),
      "{dialect}: the refusal must land at the clamped ceiling, not at the {asked} asked for"
    );
    cells += 1;
  }

  // A loop whose body never ran exits `ok`, so the cell count is asserted rather than assumed.
  assert_eq!(cells, 2 * 4, "the cell set collapsed");
}

/// A document far deeper than any stack could hold **returns**, at both doors.
///
/// This is issue #61 itself: before the fix this input did not return, it aborted the process. It
/// is safe to run here only because of the ordering stated in this module's header and repeated
/// in the body — the boundary is re-checked first, so a regressed ceiling fails a test instead of
/// killing the harness.
#[cfg(feature = "graphql")]
#[test]
fn deep_input_returns_rather_than_aborting() {
  use smear::{
    lexer::tokora::{Parse as _, Parser},
    parser::graphql::{
      GraphQL,
      ast::Document,
      error::GraphqlErrors,
      syntactic::{GraphqlLexer, document},
    },
  };

  fn parse(src: &str) -> bool {
    Parser::with_parser::<GraphqlLexer<'_, str>, Document<&str>, GraphqlErrors<&str>, _, GraphQL>(
      document,
    )
    .parse_str(src)
    .is_ok()
  }

  // THE GUARD, AND IT MUST STAY FIRST. If the ceiling has regressed upward, this fails and the
  // deep parse below is never reached. Reordering these two turns a red test into a dead runner.
  assert!(
    !parse(&inline_fragments(DEEPEST_ACCEPTED + 1)),
    "the ceiling must hold before anything deep is handed to the parser"
  );

  // The issue's own depth, then far past every stack this could run on. Both were `SIGABRT`.
  for depth in [58, 1_000, 100_000] {
    assert!(
      !parse(&inline_fragments(depth)),
      "depth {depth} must return a refusal rather than overflow the native stack"
    );
  }
}

/// Every document this repository contains stays clear of the ceiling.
///
/// The compile-time assertion beside `MAX_NESTING_DEPTH` holds the constant against the *deepest
/// fixture* as a recorded number (11). This holds that recorded number against the fixtures
/// themselves, so a corpus that grows deeper than the constant assumed cannot pass silently — the
/// two checks together are what make "24 costs a real document nothing" a fact rather than a
/// claim, and neither is sufficient alone.
#[cfg(feature = "graphql")]
#[test]
fn no_fixture_in_the_tree_comes_near_the_ceiling() {
  fn bracket_depth(src: &str) -> usize {
    let (mut depth, mut max) = (0usize, 0usize);
    let bytes = src.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
      match bytes[i] {
        // Block string: skip to its terminator so quoted brackets do not count.
        b'"' if bytes[i..].starts_with(b"\"\"\"") => {
          i = bytes[i + 3..]
            .windows(3)
            .position(|w| w == b"\"\"\"")
            .map_or(bytes.len(), |p| i + 3 + p + 3);
          continue;
        }
        b'"' => {
          i += 1;
          while i < bytes.len() && bytes[i] != b'"' {
            i += if bytes[i] == b'\\' { 2 } else { 1 };
          }
        }
        b'#' => {
          while i < bytes.len() && bytes[i] != b'\n' {
            i += 1;
          }
          continue;
        }
        b'{' | b'[' | b'(' => {
          depth += 1;
          max = max.max(depth);
        }
        b'}' | b']' | b')' => depth = depth.saturating_sub(1),
        _ => {}
      }
      i += 1;
    }
    max
  }

  fn walk(dir: &std::path::Path, worst: &mut (usize, std::path::PathBuf), seen: &mut usize) {
    let Ok(entries) = std::fs::read_dir(dir) else {
      return;
    };
    for entry in entries.flatten() {
      let path = entry.path();
      if path.is_dir() {
        walk(&path, worst, seen);
      } else if path
        .extension()
        .and_then(|e| e.to_str())
        .is_some_and(|e| matches!(e, "graphql" | "graphqls" | "gql"))
      {
        let Ok(src) = std::fs::read_to_string(&path) else {
          continue;
        };
        *seen += 1;
        let depth = bracket_depth(&src);
        if depth > worst.0 {
          *worst = (depth, path);
        }
      }
    }
  }

  // Up from `smear/` to the workspace root, so the sweep covers every member's fixtures.
  let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("the workspace root is this package's parent")
    .to_path_buf();
  let mut worst = (0usize, root.clone());
  let mut seen = 0usize;
  walk(&root, &mut worst, &mut seen);

  assert!(
    seen > 100,
    "the sweep found only {seen} GraphQL fixtures, so it is measuring the wrong tree"
  );
  assert!(
    worst.0 * 2 <= MAX_NESTING_DEPTH,
    "{} nests {} brackets deep, which is no longer clear of MAX_NESTING_DEPTH ({}) by 2x. Either \
     the ceiling was re-derived upward, or DEEPEST_DOCUMENT_IN_TREE in smear-lexer's `limits` no \
     longer describes this corpus.",
    worst.1.display(),
    worst.0,
    MAX_NESTING_DEPTH
  );
}

/// `{ ) f { ) f { …` — the shape that walked past the first fix, and the second half of #61.
///
/// # What it does to the lexer's counter
///
/// Every level opens one brace and closes **one bracket the parser never opened**. The lexer's
/// tally is one saturating scalar over every opener and every closer, pair-blind, so the `)`
/// undoes the `{` and the tally oscillates 1, 0, 1, 0 for the whole document — measured maximum
/// **1**, at every depth. Recovery, meanwhile, reports the `)` and consumes it (a closer is a
/// sync point, so the balanced skip crosses nothing and the fallback eats one token), the
/// selection-set loop continues, and the `f {` after it opens **another** selection set.
///
/// So the ceiling could not fire, and at `6f39cb9` this aborted a 2 MiB thread at 702 levels —
/// 3.5 KB of input, from a counter that never exceeded 1. `crate::…` cannot cite it: the
/// bisection lives outside the suite, in `scratchpad/depth-probe`, for the reason this module's
/// header gives.
///
/// # Why the first assertion is the ordering guard
///
/// The same discipline [`deep_input_returns_rather_than_aborting`] uses, and it needs a
/// **discriminating** signal rather than `has_errors`: this input is an error document either
/// way, since every `)` is reported. What separates the two worlds is how deep the parse
/// *recursed*, and the tree records exactly that — one `SelectionSet` node per live frame. The
/// shallow probe below asserts the tree stops nesting at the ceiling; only then is anything deep
/// handed to the parser.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn the_recovery_bypass_returns_rather_than_aborting() {
  use smear::parser::graphql::{kinds::SyntaxKind as K, lossless::parse_document};

  // One `{` per level, and one closer per level that no opener matched.
  fn bypass(levels: usize) -> String {
    let mut src = String::with_capacity(levels * 5 + 1);
    src.push('{');
    for _ in 0..levels {
      src.push_str(" ) f {");
    }
    src
  }

  // THE GUARD, AND IT MUST STAY FIRST — see this function's docs. A few dozen levels cannot
  // overflow anything, so this assertion is reachable even if the budget has regressed.
  let shallow = parse_document(&bypass(MAX_NESTING_DEPTH + 8));
  let deepest = shallow
    .syntax()
    .descendants()
    .filter(|n| n.kind() == K::SelectionSet)
    .map(|n| {
      n.ancestors()
        .filter(|a| a.kind() == K::SelectionSet)
        .count()
    })
    .max()
    .unwrap_or(0);
  assert!(
    deepest <= MAX_NESTING_DEPTH,
    "the parse nested {deepest} selection sets under a ceiling of {MAX_NESTING_DEPTH}: the budget \
     is being counted on something other than the frames that recurse"
  );

  // Past the native boundary this shape had at `6f39cb9`: 702 levels aborted a 2 MiB thread, so
  // 2 000 is 2.8x beyond it. The depth stops there rather than at 100 000 for a cost reason
  // measured on this branch and recorded in the report: `resync_to_definition`'s scan is
  // quadratic in the length of a run it can find no definition head in — pre-existing, and
  // reproduced on a control shape (`! ! ! …`) that this branch does not touch. 2 000 levels cost
  // 0.44 s; 100 000 cost 407 s, which is a gate nobody would keep.
  for levels in [100, 2_000] {
    let parse = parse_document(&bypass(levels));
    assert!(
      parse.has_errors(),
      "{levels} levels of unmatched closers must be reported"
    );
    assert_eq!(
      parse.syntax().text().to_string(),
      bypass(levels),
      "the lossless guarantee survives the refusal"
    );
  }
}

/// GraphQLx answers identically, **and with its own fourth closer**.
///
/// `>` steps the same tally as `)`, `]` and `}` — that dialect delimits generics with `<` and `>`
/// — and it is a sync point in its recovery, so it is consumed and the loop continues exactly as
/// `)` is. Measured at `6f39cb9`: both families abort a 2 MiB thread at 700 levels. A gate on `)`
/// alone would have proved the dialect and missed the pair that is only GraphQLx's.
#[cfg(all(feature = "rowan", feature = "graphqlx"))]
#[test]
fn the_graphqlx_recovery_bypass_returns_with_both_closer_families() {
  use smear::parser::graphqlx::{kinds::SyntaxKind as K, lossless::parse_document};

  fn bypass(levels: usize, closer: char) -> String {
    let mut src = String::with_capacity(levels * 5 + 1);
    src.push('{');
    for _ in 0..levels {
      src.push(' ');
      src.push(closer);
      src.push_str(" f {");
    }
    src
  }

  for closer in [')', '>'] {
    // The ordering guard, per closer family.
    let shallow = parse_document(&bypass(MAX_NESTING_DEPTH + 8, closer));
    let deepest = shallow
      .syntax()
      .descendants()
      .filter(|n| n.kind() == K::SelectionSet)
      .map(|n| {
        n.ancestors()
          .filter(|a| a.kind() == K::SelectionSet)
          .count()
      })
      .max()
      .unwrap_or(0);
    assert!(
      deepest <= MAX_NESTING_DEPTH,
      "`{closer}` nested {deepest} selection sets under a ceiling of {MAX_NESTING_DEPTH}"
    );

    for levels in [100, 2_000] {
      let parse = parse_document(&bypass(levels, closer));
      assert!(parse.has_errors());
      assert_eq!(parse.syntax().text().to_string(), bypass(levels, closer));
    }
  }
}

/// The converse: a document with far **more** delimiters than the ceiling, none of them nested,
/// is still accepted.
///
/// Moving the count from the lexer's tally to the parse's own frames is only safe if it stays a
/// *depth* count. A budget that accumulated instead of releasing — a guard bound to the wrong
/// scope, or one released after the recursion rather than around it — would refuse this document,
/// and every existing gate in this file would stay green, because they are all about depth and
/// this one is about width.
///
/// Both axes are exercised: siblings at the top level, and siblings *inside* a subtree that is
/// itself near the ceiling, which is where a leaked level would show first.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn width_costs_nothing_because_the_budget_is_a_depth() {
  use smear::parser::graphql::lossless::parse_document;

  // `{ a } { b } …` — 8x the ceiling in delimiters, never more than one open at a time.
  let wide: String = (0..MAX_NESTING_DEPTH * 8)
    .map(|i| format!("{{ f{i} }}\n"))
    .collect();
  let parse = parse_document(&wide);
  assert!(
    !parse.has_errors(),
    "{} sequential selection sets must cost one level, not {}",
    MAX_NESTING_DEPTH * 8,
    MAX_NESTING_DEPTH * 8
  );

  // The same **at** the ceiling: every sibling re-enters the deepest three levels in turn.
  //
  // Each `g(x: [1])` spends three of the budget on its own — the selection set it sits in, its
  // argument list, and its list value — so the outer nesting is sized to put the innermost of
  // those exactly at `MAX_NESTING_DEPTH`. A budget that leaked one level per sibling would refuse
  // the second one; a budget that leaked a fraction would refuse a later one.
  let outer = MAX_NESTING_DEPTH - 3;
  let mut deep_and_wide = String::new();
  for _ in 0..outer {
    deep_and_wide.push_str("{ f ");
  }
  for i in 0..MAX_NESTING_DEPTH * 4 {
    deep_and_wide.push_str(&format!(" g{i}(x: [1]) "));
  }
  for _ in 0..outer {
    deep_and_wide.push('}');
  }
  let parse = parse_document(&deep_and_wide);
  assert!(
    !parse.has_errors(),
    "siblings at the deepest accepted level must each be entered and left, not accumulated: {:?}",
    parse.diagnostics()
  );
}

/// The same bypass, aimed at the **value** cycles rather than the selection-set one.
///
/// Codex's finding named `{ ) f {`, which is `selection_set` recursing through `field`. It is not
/// the only cycle a stray closer can drive: `{ f(a: [ ) [ ) [ …` runs `list_value` into
/// `list_value`, and `{ f(a: { ) k: { ) k: …` runs `object_value` into `object_value`, with the
/// lexer's tally pinned at **3** in both cases because the `)` cancels each `[` or `{`.
///
/// Both were measured to abort a 2 MiB thread at 2 000 levels with only `selection_set` guarded —
/// which is why the budget is taken at *every* production that commits a nesting delimiter rather
/// than at the one shape the finding named. Deriving the fix from the exemplar would have left
/// two live cycles behind it.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn the_value_cycles_have_the_same_bypass_and_the_same_bound() {
  use smear::parser::graphql::{kinds::SyntaxKind as K, lossless::parse_document};

  // `{ f(a: ` + one opener and one unmatched `)` per level.
  fn bypass(open: &str, levels: usize) -> String {
    let mut src = String::from("{ f(a: ");
    for _ in 0..levels {
      src.push_str(open);
    }
    src
  }

  for (open, kind) in [("[ ) ", K::ListValue), ("{ ) k: ", K::ObjectValue)] {
    // THE ORDERING GUARD, per cycle, on an input too shallow to overflow anything.
    let shallow = parse_document(&bypass(open, MAX_NESTING_DEPTH + 8));
    let deepest = shallow
      .syntax()
      .descendants()
      .filter(|n| n.kind() == kind)
      .map(|n| n.ancestors().filter(|a| a.kind() == kind).count())
      .max()
      .unwrap_or(0);
    assert!(
      deepest <= MAX_NESTING_DEPTH,
      "{kind:?} nested {deepest} deep under a ceiling of {MAX_NESTING_DEPTH}"
    );

    // Past where this cycle aborted with only `selection_set` guarded.
    let deep = bypass(open, 2_000);
    let parse = parse_document(&deep);
    assert!(parse.has_errors());
    assert_eq!(parse.syntax().text().to_string(), deep);
  }
}

/// A nesting refusal is **one** diagnostic, at every recursive cycle in both dialects — smear
/// issue #169.
///
/// # What this pins that nothing else does
///
/// Before the repair the parse-side refusal reported and *carried on*: the `Err` unwound the nest
/// and landed in a root loop's `if definition(inp).is_err() { resync_to_definition(inp)? }`, which
/// resynchronised and re-read the abandoned tail at the **document** level, where every closer of
/// the nest is an unexpected token with an `Error` of its own. `sel(66)` at a ceiling of 66
/// returned **67** diagnostics — the refusal, then one per remaining significant token — and the
/// count tracked the document: 201 at 200 levels, 402 at 400, 804 at 800.
///
/// Every existing gate in this file stayed green through all of it. They assert `has_errors()`,
/// the tree's text and the tree's *depth*; none of them counts diagnostics, and the amplified
/// parse gets all three right.
///
/// # Why the ceiling is raised rather than left at the default
///
/// The refusal has to come from the **parse's** budget rather than the lexer's tally, because the
/// lexer's trip latches tokora's poison boundary and ends the document on its own — which is
/// exactly the accident that hid this. `parse_document_with_limits` at a ceiling above the door's
/// own clamp puts the parse's ceiling strictly below the lexer's, so the tally cannot fire and the
/// refusal is the parse's. The ceiling here is a multiple of `MAX_NESTING_DEPTH` rather than
/// `HARD_MAX + 1`, and deliberately so: it was written to clear any plausible value of a number
/// upstream owned and kept moving, and it now clears smear's own — which is the same statement
/// with one fewer thing outside this tree's control. `DEPTH` below has to sit **above** the clamp
/// for a refusal to happen at all, so a `HARD_MAX` raised past 300 reds this file rather than
/// quietly making every cell a clean parse.
///
/// # The cycles are derived, not listed
///
/// One shape per recursive cycle in each dialect's productions, enumerated by taking the strongly
/// connected components of the lossless call graph over the productions that descend: GraphQL has
/// three (`selection_set`; `list_value` ↔ `object_value`; `list_type`), GraphQLx has the same
/// three widened (`collection_body` joins the value cycle, `type_generics` and `set_or_map_type`
/// join the type cycle). The remaining descending productions — every member block, both argument
/// lists, the variables definition, GraphQLx's `import_list` and `angle_name_list` — are in no
/// cycle and cannot reach the ceiling on their own.
///
/// # The tail is an axis, because the first version of this test had only one value of it
///
/// Every cell here used to end in a **well-formed** tail, and the property held over all of them
/// while being false one character away. The repair as first written drained the remaining tokens
/// from the refusing frame, and tokora emits a diagnostic for every lexer error a drain crosses —
/// so `1 + n` for `n` invalid tail lexemes, measured at 1, 2, 5, 17 and 65 through
/// `parse_document_with_limits` in both dialects, with allocation proportional to the tail. A
/// suite that only ever measured `n = 0` reported one diagnostic and a clean first span, and both
/// readings were true of the cells it had.
///
/// So the tail carries `n ∈ {0, 1, 4, 64}` invalid lexemes, and the assertion is the same at every
/// value: **one** diagnostic, and the *same* first diagnostic — span and severity byte-identical
/// to the `n = 0` cell, which is what says the tail changed nothing rather than merely changed
/// nothing visible. `~` is the lexeme, because no GraphQL or GraphQLx token starts with it.
///
/// # What this still does not cover
///
/// * **A malformed tail the *lexer's* tally trips on.** These cells raise the ceiling so the
///   *parse* refuses; the lexer trip latches a poison boundary and is a different mechanism with
///   its own posture, covered by `deep_input_returns_rather_than_aborting` and by
///   `lossless::runner::finish_root`'s note rather than here.
/// * **Malformed bytes *before* the refusal.** The refusal's position is a function of the prefix,
///   so a prefix lexer error is an ordinary second diagnostic and not this property.
/// * **A refusal below a production that catches.** No production in either dialect catches except
///   the five document roots; a sixth would need its own cell, and
///   [`descend`](smear::parser::lossless::depth::descend)'s note records that as the residual.
/// * **Partial (`Sans-I/O`) input.** Both doors here are `Complete`.
#[cfg(all(feature = "rowan", feature = "graphqlx", feature = "graphql"))]
#[test]
fn a_refusal_is_one_diagnostic_at_every_cycle() {
  use smear::lexer::limits::LosslessLimits;

  // Above the door's clamp and above every shape's own depth below, so the *parse* refuses at
  // `HARD_MAX` and the lexer's tally — which reads this number unclamped — never fires.
  const CEILING: usize = MAX_NESTING_DEPTH * 64;
  // Deep enough that a per-token tail would be unmistakable, shallow enough to stay under the
  // ceiling. The refusal happens far above this, wherever tokora's default sits.
  const DEPTH: usize = 300;
  // `0` is the cell the first version of this test had; the rest are the axis it was missing.
  const TAILS: &[usize] = &[0, 1, 4, 64];

  fn nest(open: &str, close: &str, depth: usize, before: &str, after: &str) -> String {
    format!(
      "{before}{}{}{after}",
      open.repeat(depth),
      close.repeat(depth)
    )
  }

  // Every recursive cycle, one shape each. `before`/`after` carry whatever context puts the cycle
  // in a reachable position; the repeated pair is the cycle itself.
  let shapes: &[(&str, String)] = &[
    ("selection_set", nest("{ f ", " }", DEPTH, "", "")),
    ("list_value", nest("[", "]", DEPTH, "{ f(a: ", ") }")),
    ("object_value", nest("{k: ", "}", DEPTH, "{ f(a: ", ") }")),
    (
      "list_type",
      nest("[", "]", DEPTH, "query Q($v: ", ") { f }"),
    ),
  ];
  let graphqlx_only: &[(&str, String)] = &[
    (
      "type_generics",
      nest("A<", ">", DEPTH, "query Q($v: ", ") { f }"),
    ),
    (
      "set_or_map_type",
      nest("<", ">", DEPTH, "query Q($v: ", ") { f }"),
    ),
    (
      "collection_body",
      nest("set {", "}", DEPTH, "{ f(a: ", ") }"),
    ),
  ];

  let mut cells = 0usize;
  for (cycle, base) in shapes.iter().chain(graphqlx_only) {
    for dialect in ["graphql", "graphqlx"] {
      if dialect == "graphql" && graphqlx_only.iter().any(|(c, _)| c == cycle) {
        continue;
      }
      // The `n = 0` reading, held across the whole tail axis below.
      let mut clean_first: Option<(core::ops::Range<usize>, _)> = None;
      for &bad in TAILS {
        let src = format!("{base} {}", "~ ".repeat(bad));
        let limits = LosslessLimits::with_max_nesting_depth(CEILING);
        let (diags, text) = if dialect == "graphql" {
          let p = smear::parser::graphql::lossless::parse_document_with_limits(&src, limits);
          (p.diagnostics().to_vec(), p.syntax().text().to_string())
        } else {
          let p = smear::parser::graphqlx::lossless::parse_document_with_limits(&src, limits);
          (p.diagnostics().to_vec(), p.syntax().text().to_string())
        };
        let count = diags.len();
        assert_eq!(
          count,
          1,
          "{dialect} {cycle} tail={bad}: a refusal must be one diagnostic, not {count} over {} \
           bytes — a drain that reads the tail reports every lexer error in it",
          src.len()
        );
        assert_eq!(
          text, src,
          "{dialect} {cycle} tail={bad}: the lossless guarantee survives the refusal"
        );
        let first = (diags[0].span(), diags[0].severity());
        match &clean_first {
          None => clean_first = Some(first),
          Some(expected) => assert_eq!(
            &first, expected,
            "{dialect} {cycle} tail={bad}: the tail moved the refusal's own diagnostic"
          ),
        }
        cells += 1;
      }
    }
  }

  // A filtered run that selects nothing exits `ok`, and so does a loop whose `continue` swallowed
  // every cell. 11 dialect-cycle pairs (4 shared x 2 dialects, plus 3 GraphQLx-only) x 4 tails.
  assert_eq!(cells, 11 * TAILS.len(), "the cell set collapsed");
}

/// A refusal ends the document at **every** document root, not only the mixed one — smear issue
/// #169.
///
/// # Why this is a second test and not another axis on the one above
///
/// [`a_refusal_is_one_diagnostic_at_every_cycle`] drives `parse_document` alone, which is one root
/// per dialect. The repair has **five** catch sites — the mixed root in each dialect, the SDL-only
/// root in each, and GraphQLx's executable-only root (GraphQL's executable root propagates instead
/// of catching, so it never had the defect) — and three of them are unreachable from that entry
/// point. Reverting them was planted with the cycle test in place and it stayed green: a cell set
/// derived over *cycles* is blind to an axis of *roots*, however many cycles it has.
///
/// So the axis here is the entry point, and the shape is whatever reaches the ceiling from it. The
/// cycle coverage stays where it is; this asks only that each root stops.
///
/// # What this still does not cover
///
/// GraphQL's `parse_executable_document` has no catch arm to revert — its loop is
/// `executable_definition(inp)?` — so the cell below proves its `document_entry` drain and nothing
/// about a catch site it does not have. If that loop ever grows one, this test passes unchanged
/// and the amplification is back; the guard against that is
/// [`descend`](smear::parser::lossless::depth::descend)'s note naming the five, not this file.
#[cfg(all(feature = "rowan", feature = "graphqlx", feature = "graphql"))]
#[test]
fn a_refusal_ends_every_document_root() {
  use smear::lexer::limits::LosslessLimits;

  const CEILING: usize = MAX_NESTING_DEPTH * 64;
  const DEPTH: usize = 300;

  // One shape per root, chosen for reachability from it rather than for its cycle: a bare
  // selection set is an anonymous operation and is refused by both the mixed and the
  // executable-only roots; a nested list type inside a field definition is the deepest thing the
  // SDL-only root will accept, and both dialects spell it the same way.
  let executable = format!("{}{}", "{ f ".repeat(DEPTH), " }".repeat(DEPTH));
  let sdl = format!(
    "type T {{ f: {}Int{} }}",
    "[".repeat(DEPTH),
    "]".repeat(DEPTH)
  );

  type Door = fn(&str, LosslessLimits) -> (usize, String, Option<(core::ops::Range<usize>, bool)>);

  fn gql(
    src: &str,
    limits: LosslessLimits,
  ) -> (usize, String, Option<(core::ops::Range<usize>, bool)>) {
    let p = smear::parser::graphql::lossless::parse_document_with_limits(src, limits);
    project(p.diagnostics(), p.syntax().text().to_string())
  }
  fn gql_sdl(
    src: &str,
    limits: LosslessLimits,
  ) -> (usize, String, Option<(core::ops::Range<usize>, bool)>) {
    let p = smear::parser::graphql::lossless::parse_type_system_document_with_limits(src, limits);
    project(p.diagnostics(), p.syntax().text().to_string())
  }
  fn gql_exec(
    src: &str,
    limits: LosslessLimits,
  ) -> (usize, String, Option<(core::ops::Range<usize>, bool)>) {
    let p = smear::parser::graphql::lossless::parse_executable_document_with_limits(src, limits);
    project(p.diagnostics(), p.syntax().text().to_string())
  }
  fn glx(
    src: &str,
    limits: LosslessLimits,
  ) -> (usize, String, Option<(core::ops::Range<usize>, bool)>) {
    let p = smear::parser::graphqlx::lossless::parse_document_with_limits(src, limits);
    project(p.diagnostics(), p.syntax().text().to_string())
  }
  fn glx_sdl(
    src: &str,
    limits: LosslessLimits,
  ) -> (usize, String, Option<(core::ops::Range<usize>, bool)>) {
    let p = smear::parser::graphqlx::lossless::parse_type_system_document_with_limits(src, limits);
    project(p.diagnostics(), p.syntax().text().to_string())
  }
  fn glx_exec(
    src: &str,
    limits: LosslessLimits,
  ) -> (usize, String, Option<(core::ops::Range<usize>, bool)>) {
    let p = smear::parser::graphqlx::lossless::parse_executable_document_with_limits(src, limits);
    project(p.diagnostics(), p.syntax().text().to_string())
  }
  fn project(
    diags: &[smear::parser::graphql::lossless::runner::Diagnostic],
    text: String,
  ) -> (usize, String, Option<(core::ops::Range<usize>, bool)>) {
    let first = diags
      .first()
      .map(|d| (d.span(), d.severity() == tokora::emitter::Severity::Error));
    (diags.len(), text, first)
  }

  let roots: &[(&str, Door, &String)] = &[
    ("graphql mixed", gql, &executable),
    ("graphql sdl-only", gql_sdl, &sdl),
    ("graphql executable-only", gql_exec, &executable),
    ("graphqlx mixed", glx, &executable),
    ("graphqlx sdl-only", glx_sdl, &sdl),
    ("graphqlx executable-only", glx_exec, &executable),
  ];

  let mut cells = 0usize;
  for (root, door, base) in roots {
    let mut clean_first = None;
    for bad in [0usize, 4] {
      let src = format!("{base} {}", "~ ".repeat(bad));
      let (count, text, first) = door(&src, LosslessLimits::with_max_nesting_depth(CEILING));
      assert_eq!(
        count,
        1,
        "{root} tail={bad}: a refusal must be one diagnostic, not {count} over {} bytes",
        src.len()
      );
      assert_eq!(
        text, src,
        "{root} tail={bad}: the lossless guarantee survives"
      );
      match &clean_first {
        None => clean_first = Some(first),
        Some(expected) => assert_eq!(
          &first, expected,
          "{root} tail={bad}: the tail moved the refusal's own diagnostic"
        ),
      }
      cells += 1;
    }
  }
  assert_eq!(cells, roots.len() * 2, "the cell set collapsed");
}

/// A refusal is the error [`descend`](smear::parser::lossless::depth::descend) returns, whichever
/// emission a rejecting emitter refuses and whatever value it substitutes — smear issue #169.
///
/// # Why this needs an emitter no shipped door installs
///
/// The lossless doors pin `tokora::emitter::Verbose`, which records everything and returns `Ok`
/// from every method, so no `Err` can arrive from the emitter and this path is unreachable through
/// `parse_document`. `descend` is nevertheless a **public generic function** over any
/// `ParseContext`, so a consumer with a rejecting emitter reaches it.
///
/// # Two rejection sites, and the second one was found by review rather than by this test
///
/// The first version of this test rejected **`emit_lexer_error`** and accepted `emit_error`, which
/// covers the drain: the drain sat between the emit and the return and was propagated with `?`, so
/// `skip_while`'s fatal exit replaced the refusal (`Refusal` for `{ f }`, `LexerError` for
/// `{ f } ~ ~`). Removing the drain closed that and left the *same defect one call earlier*, on
/// the emission the test accepted: `emit_error(...)?` propagated whatever the emitter returned.
/// Tokora permits a rejecting emitter to return **any same-typed value**, not the payload it was
/// handed, so a host rejecting with an error-budget sentinel got the sentinel — and then, since
/// the sentinel is not the refusal, the entry drain ran over the tail and *its* rejection replaced
/// the sentinel in turn. Measured against that version: `Budget` for `{ f }`, and **`LexerError`
/// for `{ f } ~ ~` via the entry** — a third value, neither the refusal nor the host's.
///
/// So the axis is *which* emission is rejected and *what* it substitutes, and the assertion is the
/// same in every cell: the saved refusal comes back.
///
/// # Which value the refusal IS, and why this cell is where that shows
///
/// `Which::Recursion`, and it used to be `Which::Refusal`. `descend` no longer decides the
/// refusal — it takes the level through
/// [`InputRef::descend`](tokora::InputRef::descend) and hands back what tokora returns — so the
/// value that comes out is built by `From<RecursionLimitReached>`, while the value it *emits* is
/// still built by [`FromNestingLimit`]. Every shipped dialect lands both on the same variant
/// (smear PR #180), so no other test in this file can see the difference; `Which` maps them apart
/// on purpose, which is what makes this the cell that says which path is live.
///
/// The property is unchanged and still discriminating: `Budget` here would mean the rejecting
/// emitter's substituted value displaced the refusal, and `LexerError` would mean the entry drain
/// ran and displaced it. Both were measured before the #169 repair and both are still what a
/// regression looks like.
///
/// # What this still does not cover
///
/// * **A host whose own [`MaybeTerminal`](tokora::error::MaybeTerminal) arm is wrong.** `descend`
///   needs no cooperation — it drops the emit result — and since smear issue #178 the document
///   roots do not either: `root_turn` reads the input's resource-trip witness beside
///   `is_terminal()`, so a caller whose error type answers `false` for its own refusal still ends
///   the document. `each_term_of_a_roots_stop_is_alone_on_a_population` is the cell for that;
///   what is still uncovered here is a wrong arm on a **scanner** stop, which no published witness
///   sees. `Which` below deliberately answers `false` for `LexerError` so that a cell returning it
///   is visibly the drain having run.
/// * **The shipped doors.** `Verbose` cannot reject, so none of this is reachable through
///   `parse_document`; the two tests above are what cover that path.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn a_refusal_is_the_error_returned_even_under_a_rejecting_emitter() {
  use smear::parser::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{FromNestingLimit, RootStop, descend, drain_unless_stopped},
  };
  use tokora::{
    Emitter, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  // A `ParserContext` rather than the `(emitter, cache)` tuple, because the ceiling this cell
  // needs is now a property of the parse rather than an argument to `descend`: the tuple's
  // `ParseContext` impl seeds tokora's default budget and has no door to set one.
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Rejecting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  /// Which error came back — the whole observation.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum Which {
    Refusal,
    /// The host's own "I am at my diagnostic limit" value: a fatal stop that is **not** the
    /// payload it was handed.
    Budget,
    LexerError,
    Unexpected,
    Recursion,
  }

  impl FromNestingLimit for Which {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      Which::Refusal
    }
  }

  impl MaybeTerminal for Which {
    fn is_terminal(&self) -> bool {
      // `LexerError` answers `false` ON PURPOSE. It is the value the drain produces, so leaving it
      // non-terminal keeps a cell that returns it a visible failure rather than one the predicate
      // absorbs.
      matches!(self, Which::Refusal | Which::Budget | Which::Recursion)
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for Which {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      Which::Recursion
    }
  }

  /// What the emitter does with the refusal's own `emit_error`.
  #[derive(Clone, Copy, Debug)]
  enum OnError {
    /// A collecting host: records it and carries on. `Verbose`'s behaviour.
    Accept,
    /// `Fatal`'s behaviour: reject, returning the payload it was handed.
    RejectWithPayload,
    /// The case the review found: reject, returning a value of the host's own choosing.
    RejectWithSentinel,
  }

  struct Rejecting {
    on_error: OnError,
    reject_lexer: bool,
  }

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Rejecting {
    type Error = Which;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      if self.reject_lexer {
        Err(Which::LexerError)
      } else {
        Ok(())
      }
    }

    fn emit_error(&mut self, err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      match self.on_error {
        OnError::Accept => Ok(()),
        OnError::RejectWithPayload => Err(*err.data()),
        OnError::RejectWithSentinel => Err(Which::Budget),
      }
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Err(Which::Unexpected)
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  // A recursion budget of **0**: the first descent is over budget, so the whole of `src` is the
  // tail a drain would cross. That is the shape, minus a 64-level nest that would prove nothing
  // extra. It used to be spelled `descend(inp, 0)`; the ceiling is the parse's own limiter now,
  // which is the same statement one layer down and is what makes the refusal below tokora's own
  // trip rather than a smear pre-check that agreed with it.
  /// The root the entry drain runs, and it classifies **nothing**: no `root_turn` call, so the
  /// slot `drain_unless_stopped` mints for it stays fresh, the ending is `Recoverable`, and
  /// `MaybeTerminal` is the only term left that can stop the tail from being read. That is the
  /// trait half this cell measures — the witness half has
  /// `each_term_of_a_roots_stop_is_alone_on_a_population` — and it is exactly the population
  /// `drain_unless_stopped`'s own note assigns to the trait: a failure that reached the drain by
  /// a path no `root_turn` classified.
  ///
  /// It used to be a hand-written `RootTurn::Recoverable(..)` handed straight to the drain. That
  /// spelling is gone — the variants do not build out of crate — and this is the same cell
  /// through the door that remains.
  fn refuse_without_classifying<'inp>(
    inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), Which> {
    descend::<Lx<'inp>, Ctx<'inp>, GraphQL>(inp).map(|_| ())
  }

  fn run<'inp>(src: &'inp str, via_entry: bool, on_error: OnError, reject_lexer: bool) -> Which {
    tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        if via_entry {
          drain_unless_stopped(inp, refuse_without_classifying)
        } else {
          descend::<Lx<'inp>, Ctx<'inp>, GraphQL>(inp).map(|_| ())
        }
      },
      src,
      ParserContext::of(Rejecting {
        on_error,
        reject_lexer,
      })
      .with_recursion_limiter(RecursionLimiter::with_limitation(0)),
    )
    .expect_err("a budget of 0 refuses the first descent")
  }

  let mut cells = 0usize;
  for on_error in [
    OnError::Accept,
    OnError::RejectWithPayload,
    OnError::RejectWithSentinel,
  ] {
    for reject_lexer in [false, true] {
      // A clean tail and one that does not lex, because only the second makes a drain observable.
      for src in ["{ f }", "{ f } ~ ~", "~ { f }", "~"] {
        for via_entry in [false, true] {
          assert_eq!(
            run(src, via_entry, on_error, reject_lexer),
            Which::Recursion,
            "{src:?} (via_entry={via_entry}, on_error={on_error:?}, \
             reject_lexer={reject_lexer}): the saved refusal was displaced"
          );
          cells += 1;
        }
      }
    }
  }
  assert_eq!(cells, 3 * 2 * 4 * 2, "the cell set collapsed");
}

/// tokora's **own** descent trip lands terminal in both dialects, and not on a string.
///
/// # What this pins, and why it is not covered by anything above
///
/// `lossless_error_impls!` generates two conversions onto a dialect's error container that both
/// mean *the frame budget refused*, and only one of them was repaired by smear issue #169.
/// `FromNestingLimit` — the one [`descend`](smear::parser::lossless::depth::descend) calls —
/// moved to `ErrorData::NestingLimitExceeded`, precisely because a `Cow` discriminator is one
/// reword away from answering `false` forever. `From<RecursionLimitReached>` — the one
/// [`InputRef::descend`](tokora::InputRef::descend) carries as a where-clause — kept
/// `Other("nesting limit exceeded")`, and `Other`'s [`MaybeTerminal`](tokora::error::MaybeTerminal)
/// arm answers **`false`**. A trip arriving through it was therefore classified *recoverable*, and
/// a root loop resynchronised past it: the pre-#169 amplification, on the one carrier #169's
/// repair did not reach.
///
/// # Why it is a value assertion rather than an end-to-end parse
///
/// Nothing shipped reaches this conversion today, and that is measured rather than assumed:
///
/// * every one of smear's 28 descending production call sites goes through
///   [`depth::descend`](smear::parser::lossless::depth::descend), which refuses at
///   `min(ceiling, inp.recursion().limitation())` **before** calling `inp.descend()` — so
///   `live < limitation()` holds at the call, tokora's `check()` fails only at
///   `depth > limitation()`, and the trip cannot fire;
/// * tokora's own internal descents are the two Pratt engines (`input_ref/pratt.rs`,
///   `parser/pratt/expr.rs`), and neither dialect uses Pratt at all.
///
/// So the impl is live for exactly two populations — a consumer driving the **public** generic
/// layer with its own composition, and smear itself the moment `depth::descend` stops pre-checking
/// — and neither has a parse in this tree to observe. A conversion no test can redden is a
/// conversion that drifts, which is the argument the `MaybeTerminal` censuses in
/// `smear-parser/src/*/error/tests/terminal.rs` already make about their own arms.
///
/// # The plant
///
/// Reverting the conversion body to `Other(Cow::Borrowed("nesting limit exceeded"))` reddens all
/// four cells here: the two `is_terminal()` reads, because `Other`'s arm is `false`, and the two
/// `is_nesting_limit_exceeded()` reads, because the value is on the wrong variant.
#[cfg(all(feature = "rowan", feature = "graphql", feature = "graphqlx"))]
#[test]
fn tokoras_own_descent_trip_lands_terminal_in_both_dialects() {
  use smear::parser::{
    graphql::lossless::GraphqlLosslessErrors, graphqlx::lossless::GraphqlxLosslessErrors,
  };
  use tokora::{
    error::{MaybeTerminal, RecursionLimitReached},
    state::recursion_tracker::RecursionLimiter,
  };

  /// tokora's own payload, built the way tokora builds it: a limiter driven past its own
  /// limitation, and the report `check()` hands back. Nothing here invents a value.
  fn trip<Lang: ?Sized>() -> RecursionLimitReached<usize, Lang> {
    let mut limiter = RecursionLimiter::with_limitation(0);
    limiter.increase();
    let exceeded = limiter
      .check()
      .expect_err("depth 1 exceeds a limitation of 0");
    RecursionLimitReached::of(7usize, exceeded)
  }

  let mut cells = 0usize;

  let graphql: GraphqlLosslessErrors<&str> = trip::<smear::parser::graphql::GraphQL>().into();
  assert!(
    graphql.is_terminal(),
    "GraphQL: tokora's own descent trip must end the document — a frame budget is never cleared \
     by more input, so the carrier it arrives on cannot decide the answer"
  );
  cells += 1;

  let graphqlx: GraphqlxLosslessErrors<&str> = trip::<smear::parser::graphqlx::GraphQLx>().into();
  assert!(
    graphqlx.is_terminal(),
    "GraphQLx: tokora's own descent trip must end the document — a frame budget is never cleared \
     by more input, so the carrier it arrives on cannot decide the answer"
  );
  cells += 1;

  // `is_terminal()` alone would also pass on a *different* terminal carrier, and the point of the
  // repair is that both conversions name ONE variant. `IsVariant` is what makes that a
  // compile-checked question rather than a string one — the failure mode the variant replaced.
  assert!(
    graphql[0].data().is_nesting_limit_exceeded(),
    "GraphQL: the backstop must land on the variant `FromNestingLimit` lands on, not on a \
     second carrier that merely happens to answer the same way"
  );
  cells += 1;

  assert!(
    graphqlx[0].data().is_nesting_limit_exceeded(),
    "GraphQLx: the backstop must land on the variant `FromNestingLimit` lands on, not on a \
     second carrier that merely happens to answer the same way"
  );
  cells += 1;

  assert_eq!(cells, 4, "the cell set collapsed");
}

/// The two terms of a root's stop, each pinned on the population the other one misses.
///
/// # Why this cell set exists, and why nothing above it could ask this
///
/// A document root stops on `e.is_terminal() || inp.tripped_during_attempt(since)`, and smear
/// issue #178 is the second half: the first term is a **caller-implemented** answer, so a consumer
/// composing the public generic layer with its own error type could answer `false` for its own
/// refusal and get the pre-#169 amplification back. The witness is the repair, and the plant that
/// proves it is end-to-end — flip a dialect's `NestingLimitExceeded` arm to `false` and the three
/// refusal cells above stay green.
///
/// The **converse** plant is the one no cell above can see. Deleting `e.is_terminal()` from
/// [`root_turn`](smear::parser::lossless::depth::root_turn) left every other test in this file
/// green — measured, with this cell removed — and the reason is the one
/// `smear-parser/src/graphql/error/tests/terminal.rs` already records about the `Lexer` arm: with
/// an **accepting** emitter a lexer state trip latches tokora's poison boundary, the root loop's
/// next peek answers `None`, and the loop exits with no error to classify — so through the shipped
/// doors, which pin `Verbose`, no scanner stop ever reaches a catch arm as an `Err` at all. It
/// reaches one only for a consumer whose emitter **rejects**, which is the caller tokora's rule
/// tells to write a `MaybeTerminal` arm and the caller no in-tree parse is.
///
/// So the cells drive [`root_turn`](smear::parser::lossless::depth::root_turn) directly, which is
/// public and is now the one place the five roots' arm lives.
///
/// # The three cells, and what each one is alone on
///
/// * **Scanner.** A real `smear-lexer` state trip, whose diagnostic the emitter rejects, arriving
///   on the parser's channel as an `Err`. `descend` is never called, so tokora's resource-trip
///   counter cannot have moved and the witness answers `false` by construction — the trait is the
///   only term that can see it. This is the population the withdrawn scanner witness would have
///   covered; it is withdrawn for cause (al8n/tokora#311: a document fully recovered through the
///   documented `set_state` path still reads as truncated), so "beside, not instead of" is not a
///   posture here but the only available answer.
/// * **Refusal.** A real descent trip under a budget of `0`, on an error type whose
///   [`MaybeTerminal`] arm answers **`false`** for it. That is #178's consumer, written out: the
///   witness is the only term that can see it.
/// * **Ordinary.** A plain syntax error, no trip, arm `false`. Neither term fires and the root
///   resynchronises — without it a `root_turn` that answered `EndsTheDocument` unconditionally
///   would pass the other two.
///
/// # The plants
///
/// Deleting `e.is_terminal()` turns the scanner cell from `Ends` into `Recoverable`; deleting
/// `inp.tripped_during_attempt(since)` turns the refusal cell from `Ends` into `Recoverable`; the
/// ordinary cell is `Recoverable` under both, which is what makes the other two readings about the
/// term and not about the function. All three were run.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn each_term_of_a_roots_stop_is_alone_on_a_population() {
  use core::cell::Cell as StdCell;

  use smear::parser::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{
      FromNestingLimit, RootStop, RootTurn, descend, drain_unless_stopped, root_turn,
    },
  };
  use tokora::{
    Emitter, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Rejecting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  /// The consumer's error type — and the whole point is that **only `Scanner` is terminal**.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    /// A scanner stop the emitter rejected onto the parser's channel.
    Scanner,
    /// An ordinary syntax error.
    Ordinary,
    /// A descent refusal, on an arm that answers **`false`** for it. This is the wrong answer a
    /// consumer is free to write, and #178 is the statement that a root must stop anyway.
    Refusal,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      matches!(self, E::Scanner)
    }
  }

  impl FromNestingLimit for E {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      E::Refusal
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for E {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      E::Refusal
    }
  }

  /// Rejects every lexer diagnostic, which is the only shape that puts a scanner stop on the
  /// parser's channel. `emit_error` accepts, so the refusal cell below measures the witness rather
  /// than a rejection.
  struct Rejecting;

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Rejecting {
    type Error = E;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      Err(E::Scanner)
    }

    fn emit_error(&mut self, _err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      Ok(())
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Ok(())
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum Cell {
    Scanner,
    Ordinary,
    Refusal,
  }

  /// [`RootTurn`] flattened to something comparable — it carries a parse's error type and is not
  /// asked to be `Debug` or `PartialEq` for the sake of one test.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum Verdict {
    Parsed,
    Ends(E),
    Recoverable(E),
  }

  // The `'inp` is NAMED, threaded from `src`: elided, it varies independently of the error type
  // and the closure `E0521`s — the same reason the driver macro names it.
  fn drive<'inp>(src: &'inp str, limit: usize, cell: Cell) -> Verdict {
    // The verdict leaves through a `Cell` rather than through the parse's own `Result`, because
    // the two failure arms both return `Err` and the whole question is *which* of them it was.
    let observed: StdCell<Option<Verdict>> = StdCell::new(None);

    let _ = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        // THE SLOT IS THE DRAIN'S, LENT FOR THIS ONE CALL. A cell that wants to read
        // `root_turn`'s verdict has to sit inside a `drain_unless_stopped`, because that is the
        // only frame that mints a `RootStop` — which is the shape the seal forces on every
        // consumer, this test included. Before smear PR #189's round 3 the slot was `RootStop::new()`
        // here, which is the minting door round 2 found.
        drain_unless_stopped(
          inp,
          |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
           stop: &mut RootStop| {
            let turn = root_turn(
              inp,
              stop,
              |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| match cell {
                // A real scan. The lexer's own nesting tally trips on `src`, the emitter rejects
                // the diagnostic, and the rejection is what leaves this entry as an `Err`.
                // Nothing here descends, so tokora's resource-trip counter cannot have moved.
                Cell::Scanner => {
                  inp.skip_while(|_| true)?;
                  Ok(())
                }
                Cell::Ordinary => Err(E::Ordinary),
                // A real descent trip: the budget below is `0`, so the first descent is over it.
                Cell::Refusal => descend(inp).map(|_| ()),
              },
            );
            // The verdict and what the root returns are the same decision, and the root returns
            // its failure the way every shipped root does.
            let (verdict, out) = match turn {
              RootTurn::Parsed { .. } => (Verdict::Parsed, Ok(())),
              RootTurn::EndsTheDocument { error, .. } => (Verdict::Ends(error), Err(error)),
              RootTurn::Recoverable { error, .. } => (Verdict::Recoverable(error), Err(error)),
            };
            observed.set(Some(verdict));
            out
          },
        )
      },
      src,
      ParserContext::of(Rejecting).with_recursion_limiter(RecursionLimiter::with_limitation(limit)),
    );

    observed.get().expect("the root ran")
  }

  // Past `MAX_NESTING_DEPTH`, which is what the lexer's tally is seeded with by default, so the
  // scan below trips a real `smear_lexer::limits` budget rather than a manufactured one.
  let deep = "{".repeat(MAX_NESTING_DEPTH * 2);

  assert_eq!(
    drive(&deep, MAX_NESTING_DEPTH, Cell::Scanner),
    Verdict::Ends(E::Scanner),
    "a scanner stop ends the document, and `is_terminal()` is the only term that can see it — no \
     descent ran, so the trip witness answers `false` by construction"
  );
  assert_eq!(
    drive("{ f }", 0, Cell::Refusal),
    Verdict::Ends(E::Refusal),
    "a descent trip ends the document even though the caller's `MaybeTerminal` arm answers \
     `false` for it — smear issue #178"
  );
  assert_eq!(
    drive("{ f }", MAX_NESTING_DEPTH, Cell::Ordinary),
    Verdict::Recoverable(E::Ordinary),
    "an ordinary syntax error must still resynchronise, or the two readings above are about a \
     function that stops on everything"
  );
}

/// A trip **caught** in one entry does not silence the drain a *later* entry's ordinary failure
/// needs — smear PR #189.
///
/// # The defect this replays
///
/// `drain_unless_stopped` used to run the root itself and read
/// `inp.tripped_during_attempt(since)` with `since` taken **before the whole root**. tokora's
/// resource-trip counter is a monotone session fact, so that reading answers `true` for a root in
/// which *any* entry ever tripped — including one that tripped, was caught, and was recovered
/// from. Pair it with an ordinary failure later in the same root and both conjuncts hold: the
/// drain is skipped, the valid tail is left uncommitted, and every diagnostic that reading it
/// would have produced is never emitted.
///
/// That is the **false-stop** direction. It does not add diagnostics, it removes them, and it
/// truncates a document that was fine — the failure tokora's own note says survives testing and
/// points at nothing. `root` was a caller-supplied closure on a publicly reachable module, so the
/// root below is not a contrivance: it is a consumer that reports a too-deep entry and carries on,
/// which is what `RootTurn::EndsTheDocument` being a *value* rather than a `panic!` invites.
///
/// # Why the repair is structural
///
/// Nothing here needed a new measurement. `root_turn` had already decided, per entry, at the only
/// granularity where "did this failure end the document" means anything — and the arm threw the
/// answer away, after which the drain rebuilt it from a counter whose span is the whole root. The
/// classification is carried now, in `RootStop`, and a drain cannot be reached without one of
/// `RootTurn`'s three arms having been named.
///
/// The drain does read the counter again — smear PR #189 round 4, for the failures no `root_turn`
/// judged — and this test is the cell that says that reading is **scoped**: the slot latches that
/// an entry here already judged the caught trip, so the frame above subtracts it and drains.
/// Deleting the subtraction reddens the first cell below and nothing else.
///
/// # The three cells
///
/// * **Caught, then ordinary.** The defect. Before the repair: `0` tail diagnostics at every tail
///   length, the error `Ordinary` with its tail unread. After: `n`, one per malformed lexeme.
/// * **Ordinary alone.** The control that says the assertion is about the *caught trip* and not
///   about the drain having been disabled outright.
/// * **A refusal that is not caught.** The property the whole branch exists for, asserted from the
///   other side: the last turn ends the document, so the tail is never read and the refusal stays
///   one diagnostic. A repair that simply deleted the drain's stop condition would redden here.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn a_caught_trip_does_not_silence_a_later_failures_drain() {
  use core::cell::Cell as StdCell;

  use smear::parser::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{
      FromNestingLimit, RootStop, RootTurn, descend, drain_unless_stopped, root_turn,
    },
  };
  use tokora::{
    Emitter, InputRef, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Counting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  thread_local! {
    /// One per `emit_lexer_error`, which is what a drain over a tail that does not lex produces.
    /// Thread-local rather than a borrow in the emitter because `ParserContext::of` takes the
    /// emitter by value and the harness runs each `#[test]` on its own thread.
    static TAIL_DIAGNOSTICS: StdCell<usize> = const { StdCell::new(0) };
  }

  /// The consumer's error type, with the arm #178's consumer gets wrong: a refusal answers
  /// **`false`** for `is_terminal`, so the witness is the only term that can classify it and the
  /// cells below measure the carried verdict rather than the trait.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    Refusal,
    Ordinary,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      false
    }
  }

  impl FromNestingLimit for E {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      E::Refusal
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for E {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      E::Refusal
    }
  }

  /// Accepts everything — a collecting host, `Verbose`'s posture — and counts the lexer
  /// diagnostics. Rejecting would stop the drain at the first bad lexeme and make the count
  /// answer a different question.
  struct Counting;

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Counting {
    type Error = E;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      TAIL_DIAGNOSTICS.with(|n| n.set(n.get() + 1));
      Ok(())
    }

    fn emit_error(&mut self, _err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      Ok(())
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Ok(())
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  /// One turn of the root loop below.
  #[derive(Debug, Clone, Copy)]
  enum Entry {
    /// A real descent trip the root **catches** and carries on from. The plausible consumer:
    /// "this definition is too deep, it is already reported, parse the next one".
    CaughtRefusal,
    /// A real descent trip the root propagates, the way every shipped root does.
    Refusal,
    /// An ordinary syntax error, already reported at the point of failure.
    Ordinary,
  }

  /// A document root a consumer could plausibly write: one `root_turn` per entry, matching its
  /// verdict, threading the slot its drain will read.
  fn root<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    stop: &mut RootStop,
    entries: &[Entry],
  ) -> Result<(), E> {
    for entry in entries {
      // The `..` in every pattern is `#[non_exhaustive]` on the variants: out of crate a verdict
      // still matches and still gets exhaustiveness checking, and no longer BUILDS — smear
      // PR #189, round 3. The variants are braced for the same reason: on a TUPLE variant the
      // attribute privates the constructor, and a tuple pattern out of crate resolves through it.
      match *entry {
        Entry::CaughtRefusal => {
          match root_turn(inp, stop, |inp: &mut InputRef<'inp, '_, _, _, _>| {
            descend(inp).map(|_| ())
          }) {
            RootTurn::Parsed { .. } => {}
            // CAUGHT AND CARRIED ON. Nothing in either shipped dialect does this; the public
            // generic layer lets a consumer, and `RootTurn::EndsTheDocument` is a value rather
            // than a stop the type system forces.
            RootTurn::EndsTheDocument { .. } | RootTurn::Recoverable { .. } => {}
          }
        }
        Entry::Refusal => {
          match root_turn(inp, stop, |inp: &mut InputRef<'inp, '_, _, _, _>| {
            descend(inp).map(|_| ())
          }) {
            RootTurn::Parsed { .. } => {}
            RootTurn::EndsTheDocument { error, .. } | RootTurn::Recoverable { error, .. } => {
              return Err(error);
            }
          }
        }
        Entry::Ordinary => {
          // `Err::<(), E>`: the `Parsed` arm below binds nothing now that the variants are
          // braced, so nothing else in this call fixes the entry's `T`.
          match root_turn(inp, stop, |_inp: &mut InputRef<'inp, '_, _, _, _>| {
            Err::<(), E>(E::Ordinary)
          }) {
            RootTurn::Parsed { .. } => {}
            RootTurn::EndsTheDocument { error, .. } | RootTurn::Recoverable { error, .. } => {
              return Err(error);
            }
          }
        }
      }
    }
    Ok(())
  }

  /// The root plus its drain, exactly as an `*_entry` production writes it — and the tail
  /// diagnostics that drain produced.
  ///
  /// A budget of `0` refuses the first descent, so `src` is entirely tail: no entry consumes
  /// anything, and what the drain crosses is the whole document.
  fn drive<'inp>(src: &'inp str, entries: &[Entry]) -> (Result<(), E>, usize) {
    TAIL_DIAGNOSTICS.with(|n| n.set(0));
    let out = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        // Exactly what an `*_entry` production writes: the drain runs the root, mints the slot
        // for it, and spends that slot against what that root returned. The three-step form this
        // used to spell — `RootStop::new()`, run, `stop.ending(out)` — is gone, and with it every
        // way to reach this drain with a verdict about some other root.
        drain_unless_stopped(
          inp,
          |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>, stop: &mut RootStop| {
            root(inp, stop, entries)
          },
        )
      },
      src,
      ParserContext::of(Counting).with_recursion_limiter(RecursionLimiter::with_limitation(0)),
    );
    (out, TAIL_DIAGNOSTICS.with(StdCell::get))
  }

  // `~` does not lex in either dialect, so one per lexeme is what a drain over this tail reports.
  // `n = 0` is carried because it is the cell that would stay green under the defect and says so.
  for n in [0usize, 1, 4, 16] {
    let src = "~ ".repeat(n);

    assert_eq!(
      drive(&src, &[Entry::CaughtRefusal, Entry::Ordinary]),
      (Err(E::Ordinary), n),
      "n={n}: an entry that caught a refusal and carried on must not cost the NEXT entry's \
       ordinary failure its drain — before smear PR #189 this read 0 at every n, with the tail \
       left uncommitted and its diagnostics unemitted"
    );
    assert_eq!(
      drive(&src, &[Entry::Ordinary]),
      (Err(E::Ordinary), n),
      "n={n}: the control — the same ordinary failure with no earlier trip. If this ever \
       disagrees with the cell above, the reading there is about the drain and not about the \
       caught trip"
    );
    assert_eq!(
      drive(&src, &[Entry::Refusal]),
      (Err(E::Refusal), 0),
      "n={n}: a refusal that is NOT caught still ends the document, so nothing reads the tail \
       and the refusal stays one diagnostic — the `1 + n` amplification this branch closes. A \
       repair that deleted the drain's stop condition instead of scoping it reddens here"
    );
  }
}

/// A **nested** drain's stop is not reclassified as recoverable by the drain above it —
/// smear PR #189, round 4.
///
/// # The defect this replays
///
/// A root returns `Result` independently of the slot it is handed, so a downstream root can
/// return a nested `drain_unless_stopped` call and never touch its own slot. On a genuine descent
/// refusal whose caller-defined [`MaybeTerminal`] arm answers `false`, the inner drain classifies
/// the entry correctly, records `EndsTheDocument` in *its* slot and skips *its* drain — and the
/// `Err` it hands back carries none of that. The outer slot is untouched, so the frame above reads
/// the same failure as `Recoverable` and takes the malformed tail: `1 + n` diagnostics for a tail
/// of `n` invalid lexemes, which is the amplification smear issue #178 closes, back on the public
/// generic surface.
///
/// Every operation in that shape is legitimate. Nothing is forged, nothing is copied, no
/// `#[must_use]` value is dropped and no borrow escapes — which is why the round-3 seal, which is
/// about who may *mint* a verdict, does not reach it. What reaches it is the input's own trip
/// witness, read at the frame that is about to drain and scoped to what no `root_turn` in that
/// frame has already judged.
///
/// # The three cells
///
/// * **Nested, uncaught.** The defect. Before the repair: `n` tail diagnostics at every tail
///   length. After: `0` — the refusal is one diagnostic, which is what a stop means.
/// * **A tail with nothing to say.** `n = 0` is carried because it is the cell that stays green
///   under the defect and therefore says the others are about the tail rather than the shape.
/// * **The single-level control.** The same refusal through one drain, which was already right,
///   so a repair that stopped on every failure rather than on a tripped one is visible here as a
///   changed *error* rather than a changed count.
///
/// # Each term of the drain's reading is alone on a population
///
/// `drain_unless_stopped`'s stop condition for an unclassified failure is
/// `!a_classified_entry_saw_a_trip && tripped_during_attempt(since)`, and the two conjuncts are
/// pinned separately rather than argued for. Deleting the **subtraction** leaves a whole-root
/// reading, which is round 1: 17 pass and only
/// `a_caught_trip_does_not_silence_a_later_failures_drain` reddens. Deleting the **whole reading**
/// leaves round 3: 17 pass and only this test reddens. Neither deletion moves any other cell.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it() {
  use core::cell::Cell as StdCell;

  use smear::parser::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{
      FromNestingLimit, RootStop, RootTurn, descend, drain_unless_stopped, root_turn,
    },
  };
  use tokora::{
    Emitter, InputRef, Lexer, ParserContext, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
    state::recursion_tracker::RecursionLimiter,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = ParserContext<'inp, Lx<'inp>, Counting, DefaultCache<'inp, Lx<'inp>>, GraphQL>;

  thread_local! {
    /// One per `emit_lexer_error` — what a drain over a tail that does not lex produces.
    static TAIL_DIAGNOSTICS: StdCell<usize> = const { StdCell::new(0) };
  }

  /// The consumer's error type, with #178's arm: a refusal answers **`false`** for
  /// `is_terminal`, so the trait cannot classify it and the witness is the only term left.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum E {
    Refusal,
  }

  impl MaybeTerminal for E {
    fn is_terminal(&self) -> bool {
      false
    }
  }

  impl FromNestingLimit for E {
    fn nesting_limit_exceeded(_span: SimpleSpan, _attempted: usize, _limit: usize) -> Self {
      E::Refusal
    }
  }

  impl<Lang: ?Sized> From<RecursionLimitReached<usize, Lang>> for E {
    fn from(_: RecursionLimitReached<usize, Lang>) -> Self {
      E::Refusal
    }
  }

  /// Accepts everything and counts the lexer diagnostics, for
  /// `a_caught_trip_does_not_silence_a_later_failures_drain`'s reason: rejecting would stop the
  /// drain at the first bad lexeme and make the count answer a different question.
  struct Counting;

  impl<'inp, L: Lexer<'inp>> Emitter<'inp, L, GraphQL> for Counting {
    type Error = E;

    fn emit_lexer_error(
      &mut self,
      _err: Spanned<<L::Token as Token<'inp>>::Error, L::Span>,
    ) -> Result<(), Self::Error> {
      TAIL_DIAGNOSTICS.with(|n| n.set(n.get() + 1));
      Ok(())
    }

    fn emit_error(&mut self, _err: Spanned<Self::Error, L::Span>) -> Result<(), Self::Error> {
      Ok(())
    }

    fn emit_unexpected_token(
      &mut self,
      _err: UnexpectedTokenOf<'inp, L, GraphQL>,
    ) -> Result<(), Self::Error> {
      Ok(())
    }

    fn rewind(&mut self, _cursor: &tokora::input::Cursor<'inp, '_, L>, _checkpoint: u64) {}
  }

  /// The inner root: one classified entry, and it is a genuine descent refusal. Its own drain is
  /// correctly skipped — the verdict this cell is about exists and is right.
  fn inner<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    stop: &mut RootStop,
  ) -> Result<(), E> {
    match root_turn(inp, stop, |inp: &mut InputRef<'inp, '_, _, _, _>| {
      descend(inp).map(|_| ())
    }) {
      RootTurn::Parsed { .. } => Ok(()),
      RootTurn::EndsTheDocument { error, .. } | RootTurn::Recoverable { error, .. } => Err(error),
    }
  }

  /// The outer root: it returns the nested drain's `Result` and touches its own slot not at all.
  /// Nothing here is a misuse — a `Root` is a `fn(&mut Input, &mut RootStop) -> Result<…>` and
  /// this is one.
  fn outer<'inp>(
    inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>,
    _stop: &mut RootStop,
  ) -> Result<(), E> {
    drain_unless_stopped(inp, inner)
  }

  /// One drain over `root`, and the tail diagnostics that drain produced. A budget of `0` refuses
  /// the first descent, so `src` is entirely tail.
  fn drive<'inp, R>(src: &'inp str, root: R) -> (Result<(), E>, usize)
  where
    R:
      FnOnce(&mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>, &mut RootStop) -> Result<(), E>,
  {
    TAIL_DIAGNOSTICS.with(|n| n.set(0));
    let mut root = Some(root);
    let out = tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        drain_unless_stopped(inp, root.take().expect("the production runs once"))
      },
      src,
      ParserContext::of(Counting).with_recursion_limiter(RecursionLimiter::with_limitation(0)),
    );
    (out, TAIL_DIAGNOSTICS.with(StdCell::get))
  }

  // `~` does not lex in either dialect, so one per lexeme is what a drain over this tail reports.
  for n in [0usize, 1, 4, 16] {
    let src = "~ ".repeat(n);

    assert_eq!(
      drive(&src, outer),
      (Err(E::Refusal), 0),
      "n={n}: the inner drain classified this refusal as ending the document and skipped its own \
       drain; the frame above must not read the same failure as recoverable and take the tail. \
       Before smear PR #189 round 4 this read n at every n — the `1 + n` amplification, through \
       a composition in which every operation is legitimate"
    );
    assert_eq!(
      drive(&src, inner),
      (Err(E::Refusal), 0),
      "n={n}: the single-level control. One drain over the same refusal was already right, so a \
       repair that stopped on every failure rather than on a tripped one shows up here as a \
       changed error rather than a changed count"
    );
  }
}
