#![cfg(feature = "rowan")]

use smear_parser::graphql::{kinds::SyntaxKind as K, lossless::parse_str};

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
