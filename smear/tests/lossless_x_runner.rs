//! The GraphQLx runner: the profile, its kind validator, and the first parse this dialect can
//! actually perform.
//!
//! # What is real here and what is a stub
//!
//! `parse_document` drives a **drain-everything stub** until Task 14 writes `document_entry`, so no
//! node exists yet and nothing in this file asserts anything about GraphQLx's grammar. What it
//! does assert is the wiring the grammar will sit on, and every one of those facts is real now:
//!
//! - the sink accepts this dialect's lexer at all, which is a *build-time* fact
//!   ([`the_graphqlx_sink_builds_at_all`] explains why running it is the evidence);
//! - every committed token enters the tree under the image [`kind_map`] gives it, checked through
//!   the sink rather than by calling the mapper directly;
//! - the profile's validator refuses a kind outside the space, at the emit door;
//! - a materialization failure names *this* dialect.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear::parser::graphqlx::{
  kinds::{GraphQLxLang, SyntaxKind as X},
  lossless::{parse_document, runner::test_support::open_raw_kind},
};

/// The node-kind pre-order of a parse's *tokens*, in document order.
fn token_kinds(node: &rowan::SyntaxNode<GraphQLxLang>) -> Vec<X> {
  node
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .map(|token| token.kind())
    .collect()
}

/// The GraphQLx sink builds, which is the whole of Task 1's payoff.
///
/// **`cargo check` cannot show this and `cargo test` can.** tokora's `cst::Sink` gates on
/// `L::SURFACES_TRIVIA` in an inline `const` block (`tokora/src/cst/sink.rs:573-582`), so a
/// trivia-skipping lexer is a post-monomorphization `E0080` at build/test/doc time and a green
/// `cargo check`. Task 1 added the declaration to both `token_impl!` arms; this is the first place
/// a GraphQLx `Sink` is monomorphized, so the *existence* of this test's binary is the evidence.
#[test]
fn the_graphqlx_sink_builds_at_all() {
  let parse = parse_document("");
  assert_eq!(parse.syntax().kind(), X::Root);
  assert_eq!(parse.syntax().text().to_string(), "");
}

/// Every byte reaches the tree, trivia included.
///
/// The lossless guarantee, and the reason the sink refuses trivia-skipping lexers. The source
/// exercises the images GraphQL has no counterpart for — `<`, `>`, `::`, `=>`, `*` — because those
/// are the ones a mapper copied from the other dialect could not have.
#[test]
fn every_byte_reaches_the_tree_including_trivia() {
  let src = "# leading\nimport * from \"a\"\ntype T<A> { f: map<::ns::K => V> }\n";
  let parse = parse_document(src);
  assert_eq!(
    parse.syntax().text().to_string(),
    src,
    "tree.text() must equal the source byte for byte"
  );
}

/// The tokens in the tree carry the images the GraphQLx mapper gives them.
///
/// **The mapper checked through the sink, not by calling it.** `tests/lossless_x_kind_map.rs`
/// calls `token_kind` directly; this asserts that the profile `parse_document` actually arms is
/// wired to *that* mapper, in order, over a real parse. A profile pointing at some other mapper —
/// or a pair of arms transposed — reds here as well as there, and a transposition is the mutation
/// that
/// survives the round-trip gate, the validator and the golden printer alike.
///
/// The source is chosen so that no two adjacent tokens share an image: a mapper that answered its
/// neighbour's kind would have to be wrong twice to still produce this sequence.
#[test]
fn the_tree_carries_the_mappers_images_in_document_order() {
  let parse = parse_document("a::b<C>");
  assert_eq!(
    token_kinds(&parse.syntax()),
    vec![
      X::Name,
      X::PathSeparator,
      X::Name,
      X::LAngle,
      X::Name,
      X::RAngle,
    ],
    "the parse's token images are not the ones kind_map gives"
  );

  // The trivia forms the tree keeps apart, in one source. `Space` and `Tab` are distinct images
  // and the BOM is a third; only the line terminators fold.
  let parse = parse_document("\u{FEFF} \t\r\n,# c\n");
  assert_eq!(
    token_kinds(&parse.syntax()),
    vec![
      X::Bom,
      X::Space,
      X::Tab,
      X::Newline,
      X::Comma,
      X::Comment,
      X::Newline,
    ],
    "the trivia images collapsed; only \\n, \\r and \\r\\n may share one"
  );
}

/// The predicate the validator wraps rejects out-of-space raws.
///
/// `KindValidator::admits` is `pub(crate)` in tokora (`cst/profile.rs:77`), so there is no public
/// door to evaluate a profile's predicate from a test. `X::from_raw` *is* the predicate:
/// `profile()` passes `KindValidator::new(|raw| X::from_raw(raw).is_some())`, and a fn pointer that
/// is literally this expression cannot disagree with it.
#[test]
fn the_kind_predicate_the_validator_wraps_rejects_out_of_space_raws() {
  assert!(
    X::from_raw(X::Document.raw()).is_some(),
    "an in-space kind must be admitted"
  );
  assert!(
    X::from_raw(u16::MAX).is_none(),
    "the tombstone must not be admitted"
  );
  assert!(
    X::from_raw(X::ALL.len() as u16).is_none(),
    "one past the space must not be admitted"
  );
}

/// Constructing the profile does not panic.
///
/// `CstProfile::new` asserts in EVERY build that its own validator admits `error_kind` and
/// `gap_kind` (`cst/profile.rs:140`) — a profile cannot describe a sink that would refuse its own
/// output. Calling it is therefore itself an assertion about `X::Error` and `X::Gap`.
#[test]
fn constructing_the_profile_does_not_panic() {
  let _ = smear::parser::graphqlx::lossless::profile::<str>();
}

/// The profile's validator refuses an out-of-space kind at the emit door.
///
/// **The GraphQLx twin of the probe Phase A had to add after measuring the gap.** Swapping
/// `runner::profile`'s validator for `KindValidator::new(|_| true)` left GraphQL's whole 540-test
/// suite green — the permissive validator simply never had anything to refuse, because no
/// production can name a kind outside its own space. Only a caller that hands the sink such a kind
/// can tell the two validators apart, and `open_raw_kind` is that caller: it spends
/// `cst_start_at`, the exact retro-wrap door every `node`/`node_at` production spends, through the
/// crate's real shipped `profile()`.
#[test]
fn the_profile_validator_refuses_an_out_of_space_kind_at_the_emit_door() {
  // One past the dialect's own kind space — every real kind is `0..X::ALL.len()` — so no
  // production in this crate could ever produce it.
  let out_of_space_kind = X::ALL.len() as u16;

  // The panic hook is silenced only for the duration of the call, so an unexpected panic elsewhere
  // still prints normally.
  let prev_hook = std::panic::take_hook();
  std::panic::set_hook(Box::new(|_| {}));
  let outcome = std::panic::catch_unwind(|| open_raw_kind("", out_of_space_kind));
  std::panic::set_hook(prev_hook);

  // `expect_err`/`unwrap_err` both require `T: Debug` to format the `Ok` arm they didn't take, and
  // `Parse` derives no `Debug` — so the match is spelled out instead.
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

/// A materialization failure names **this** dialect's sink.
///
/// `finish_root` is shared by both dialects, so its panic is the one place a reader of a
/// malformed-stream crash learns which suite emitted it — and the `space` argument is threaded on
/// trust unless something reaches the arm. No production can sever the token channel;
/// `structure_without_tokens` wraps a node over a nonempty source and commits nothing on purpose.
/// An unclosed node would not do: `finish_partial` closes one by design.
///
/// The expected string is the whole point: GraphQL's twin of this test asserts "graphql", so a
/// GraphQLx runner that passed the wrong `space` through would go red here and green there.
#[test]
#[should_panic(expected = "the graphqlx lossless event stream was refused")]
fn a_malformed_stream_panics_naming_the_graphqlx_sink() {
  let _ = smear::parser::graphqlx::lossless::runner::test_support::structure_without_tokens("a", 0);
}
