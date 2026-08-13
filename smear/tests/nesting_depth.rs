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
