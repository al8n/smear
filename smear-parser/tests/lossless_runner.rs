#![cfg(feature = "rowan")]

use smear_parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{parse_str, runner::test_support::open_raw_kind},
};

#[test]
fn an_empty_source_yields_a_root_and_an_empty_document() {
  // **This test asserted `!has_errors()` until Task 8**, when `document` stopped being a stub
  // that could not report anything. `syntactic/`'s `document` is `.at_least(1)` — "nonempty" —
  // and gate 1 compares the two suites' verdicts input by input, so the empty source is
  // reported here too; Task 7 took the same ruling for `ExecutableDocument`. The tree is
  // unchanged, which is what the two surviving assertions pin.
  let p = parse_str("");
  assert_eq!(p.syntax().kind(), K::Root);
  assert_eq!(p.syntax().text().to_string(), "");
  assert!(p.has_errors(), "an empty document must report");
}

#[test]
fn every_byte_reaches_the_tree_including_trivia() {
  // The lossless guarantee, and the reason the sink refuses trivia-skipping lexers.
  let src = "# leading comment\n{ a }\n";
  let p = parse_str(src);
  assert_eq!(
    p.syntax().text().to_string(),
    src,
    "tree.text() must equal the source byte for byte"
  );
}

#[test]
fn the_kind_predicate_the_validator_wraps_rejects_out_of_space_raws() {
  // The validator is data on the profile because rowan's kind_from_raw cannot fail — but
  // `KindValidator::admits` is `pub(crate)` in tokora (`cst/profile.rs:77`), so there is no
  // public door to evaluate a profile's predicate from a test. `CstProfile::validator()` hands
  // back the `KindValidator` and nothing more can be done with it from outside tokora.
  //
  // So assert the predicate this crate hands tokora, at its own door. `K::from_raw` IS the
  // predicate: `profile()` passes `KindValidator::new(|raw| K::from_raw(raw).is_some())`, and a
  // fn pointer that is literally this expression cannot disagree with it.
  assert!(
    K::from_raw(K::Document.raw()).is_some(),
    "an in-space kind must be admitted"
  );
  assert!(
    K::from_raw(u16::MAX).is_none(),
    "the tombstone must not be admitted"
  );
  assert!(
    K::from_raw(K::ALL.len() as u16).is_none(),
    "one past the space must not be admitted"
  );
}

#[test]
fn constructing_the_profile_does_not_panic() {
  // `CstProfile::new` asserts in EVERY build that its own validator admits `error_kind` and
  // `gap_kind` (`cst/profile.rs:140`) — a profile cannot describe a sink that would refuse its
  // own output. Calling it is therefore itself an assertion about K::Error and K::Gap.
  let _ = smear_parser::graphql::lossless::profile::<str>();
}

#[test]
fn the_profile_validator_refuses_an_out_of_space_kind_at_the_emit_door() {
  // The gap this closes, measured: `KindValidator::admits` is `pub(crate)`
  // (`cst/profile.rs:77`), so nothing outside tokora can evaluate a profile's predicate
  // directly. `the_kind_predicate_the_validator_wraps_rejects_out_of_space_raws`, above, is the
  // test that limitation produced — it checks `K::from_raw` on its own and never touches the
  // sink `profile()` actually arms. A prior measurement swapped `runner::profile`'s validator
  // for `KindValidator::new(|_| true)` and found every shipped test blind to it: `--test
  // lossless_runner` stayed at 4 passed and the whole crate at 540 passed, exit 0. This test
  // exists to go red the moment that swap happens again.
  //
  // `K::ALL.len()` is one past the dialect's own kind space — every real kind is
  // `0..K::ALL.len()` — so no production in this crate ever produces it; it stands in here for
  // a kind that, by construction, no production could ever pass.
  let out_of_space_kind = K::ALL.len() as u16;

  // `open_raw_kind` spends `CstEmitter::cst_start_at` — the exact retro-wrap door every
  // `node`/`node_at` production in this suite spends to open its own node — through the
  // crate's real, shipped `runner::profile()`. Under the real validator this panics before
  // `Sink::finish` is ever reached (`tokora/src/cst/sink.rs:1209`); under a validator that
  // admits everything it instead materializes a tree containing the out-of-space kind. The
  // panic hook is silenced only for the duration of the call, so an unexpected panic elsewhere
  // still prints normally.
  let prev_hook = std::panic::take_hook();
  std::panic::set_hook(Box::new(|_| {}));
  let outcome = std::panic::catch_unwind(|| open_raw_kind("", out_of_space_kind));
  std::panic::set_hook(prev_hook);

  // `expect_err`/`unwrap_err` both require `T: Debug` to format the `Ok` arm they didn't take,
  // and `Parse` (this crate's, not tokora's) derives no `Debug` — so the match is spelled out
  // instead.
  let payload = match outcome {
    Err(payload) => payload,
    Ok(_) => panic!(
      "opening a node one past the dialect's kind space must panic at the sink's emit door — if \
       this returned Ok instead, `profile()`'s validator has stopped rejecting out-of-space kinds"
    ),
  };

  let message = payload
    .downcast_ref::<&str>()
    .copied()
    .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
    .expect("the sink's kind-validator assert! panics with a &str or String payload");
  assert!(
    message.contains("outside the dialect's own kind space"),
    "expected the sink's own kind-validator refusal message, got a different panic: {message:?}"
  );
}

/// `depth` nested selection sets, `{ a { a … } } `, four bytes per opening level.
///
/// The generator is spelled out rather than hard-coding a literal because the assertions below
/// name the byte offset of one particular brace, and deriving it from the same `"{ a "` that
/// built the string is the only way the two cannot drift.
fn nested_selection_sets(depth: usize) -> String {
  const OPEN: &str = "{ a ";
  let mut src = String::with_capacity(depth * (OPEN.len() + 2));
  for _ in 0..depth {
    src.push_str(OPEN);
  }
  for _ in 0..depth {
    src.push_str("} ");
  }
  src
}

/// Byte offset of the opening brace at `level` (1-based) in [`nested_selection_sets`].
fn brace_offset(level: usize) -> usize {
  (level - 1) * "{ a ".len()
}

/// The nesting budget is a **report**, not a panic — smear issue #57.
///
/// # The boundary, measured
///
/// **The 501st simultaneously-open bracket**, exactly. Bisected: 500 nested selection sets are
/// accepted with no diagnostic at all and 501 trip, while `{ f(a: [[[…]]]) }` trips at 499 nested
/// lists — its two enclosing brackets count toward the same tally, which is what identifies the
/// budget as one global bracket-depth counter rather than anything per-production.
///
/// The issue reported "roughly 512" from two sampled points, and read the clean parse at 256 as
/// the budget working at lower depths. Neither is what the code does: 256 is not a report, it is
/// silence — nothing is consulted until the 501st bracket.
///
/// # Which of the two candidate defects it was
///
/// Neither, as the issue framed them. The budget is not the parser's: nothing in this crate
/// descends through `InputRef::descend`, so tokora's parser-facing `RecursionLimiter` (64) is
/// never consulted, and no amount of nesting trips it. It is the **lexer's** — every `{`, `[` and
/// `(` steps the `Limiter` carried in the Logos `Extras`, whose inherited ceiling is tokora's
/// general-purpose 500.
///
/// It does trip, and what follows the trip is what broke. A resource-limit trip **latches a
/// poison boundary**: the scanner refuses to rebuild a lexer past that offset, so nothing — not
/// the parse, not `document_entry`'s `skip_while` drain — can ever cover the remaining bytes.
/// `Cst::finish` refuses that stream (`FinishError::UncoveredGap`, naming the exact byte range)
/// and `finish_root` turned the refusal into a panic on a public entry point. So: the budget
/// trips, and the trip was mishandled — but one layer below `finish_root`, which was only the
/// place the mishandling surfaced.
///
/// # GraphQLx
///
/// The same three assertions hold for GraphQLx's `parse_str`, at the identical count, measured on
/// the Phase B branch where that dialect exists. It shares this lexer's `Limiter` and this
/// runner's materialization step, so it inherits the fix rather than needing its own; Phase B's
/// own gates cover it.
#[test]
fn nesting_past_the_lexer_budget_reports_instead_of_panicking() {
  // The last depth inside the budget: unchanged by the fix, and the control that proves the
  // assertions below measure the boundary rather than "deep input reports".
  let inside = nested_selection_sets(500);
  let parse = parse_str(&inside);
  assert!(
    !parse.has_errors(),
    "500 open brackets is inside the budget and must still parse clean"
  );
  assert!(
    parse.diagnostics().is_empty(),
    "500 open brackets must report nothing at all, not merely no error"
  );
  assert_eq!(parse.syntax().text().to_string(), inside);

  // One past it. Every assertion here was unreachable before the fix: the call panicked.
  let over = nested_selection_sets(501);
  let parse = parse_str(&over);
  assert!(
    parse.has_errors(),
    "501 open brackets must be reported, not accepted"
  );
  assert_eq!(
    parse.syntax().text().to_string(),
    over,
    "the lossless guarantee survives the trip: the un-lexable tail tiles as gaps, so the tree \
     still reproduces the source byte for byte"
  );
  let first = parse
    .diagnostics()
    .first()
    .expect("the trip must be on the diagnostic channel");
  assert_eq!(
    first.span(),
    brace_offset(501)..brace_offset(501) + 1,
    "the diagnostic must sit on the brace that exceeded the budget"
  );
  assert_eq!(first.severity(), tokora::emitter::Severity::Error);

  // Far past it, because a fix that merely moved the cliff would pass everything above.
  let far = nested_selection_sets(2_000);
  let parse = parse_str(&far);
  assert!(parse.has_errors());
  assert_eq!(parse.syntax().text().to_string(), far);
}
