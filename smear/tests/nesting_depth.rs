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
/// exactly the accident that hid this. `parse_document_with_limits` at a ceiling above tokora's
/// own parse-side default puts the parse's ceiling strictly below the lexer's, so the tally cannot
/// fire and the refusal is the parse's. `RecursionLimiter::PARSE_DEFAULT_DEPTH` is not public at
/// the pinned version and has already moved upstream, so the ceiling here is a multiple of
/// `MAX_NESTING_DEPTH` large enough to clear any plausible value of it rather than a number read
/// off one.
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

  // Above any parse-side default tokora can plausibly install, and above every shape's own depth
  // below, so the *parse* refuses and the lexer's tally never reaches its own ceiling.
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
/// # What this still does not cover
///
/// * **A host whose own [`MaybeTerminal`](tokora::error::MaybeTerminal) arm is wrong.** `descend`
///   needs no cooperation — it drops the emit result — but the document roots stop on
///   `is_terminal()`, so a caller whose error type answers `false` for its own fatal value has
///   told the emitter *stop* and the recovery machinery *recoverable*. That contradiction is the
///   caller's, it is the residue tokora's own rule documents, and `Which` below deliberately
///   answers `false` for `LexerError` so that a cell returning it is visibly the drain having run.
/// * **The shipped doors.** `Verbose` cannot reject, so none of this is reachable through
///   `parse_document`; the two tests above are what cover that path.
#[cfg(all(feature = "rowan", feature = "graphql"))]
#[test]
fn a_refusal_is_the_error_returned_even_under_a_rejecting_emitter() {
  use smear::parser::{
    graphql::{GraphQL, lossless::GraphqlLosslessLexer},
    lossless::depth::{FromNestingLimit, descend, drain_unless_terminal},
  };
  use tokora::{
    Emitter, Lexer, SimpleSpan, Token,
    cache::DefaultCache,
    error::{MaybeTerminal, RecursionLimitReached},
    prelude::UnexpectedTokenOf,
    span::Spanned,
  };

  type Lx<'inp> = GraphqlLosslessLexer<'inp, str>;
  type Ctx<'inp> = (Rejecting, DefaultCache<'inp, Lx<'inp>>);

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

  // `ceiling = 0`: the first descent is over budget, so the whole of `src` is the tail a drain
  // would cross. That is the shape, minus a 64-level nest that would prove nothing extra.
  fn run<'inp>(src: &'inp str, via_entry: bool, on_error: OnError, reject_lexer: bool) -> Which {
    tokora::parse_with::<Lx<'inp>, str, _, (), Ctx<'inp>, GraphQL>(
      |inp: &mut tokora::InputRef<'inp, '_, Lx<'inp>, Ctx<'inp>, GraphQL>| {
        let out = descend::<Lx<'inp>, Ctx<'inp>, GraphQL>(inp, 0).map(|_| ());
        if via_entry {
          drain_unless_terminal(inp, out)
        } else {
          out
        }
      },
      src,
      (
        Rejecting {
          on_error,
          reject_lexer,
        },
        DefaultCache::<Lx<'inp>>::default(),
      ),
    )
    .expect_err("a ceiling of 0 refuses the first descent")
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
            Which::Refusal,
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
