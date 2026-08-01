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
