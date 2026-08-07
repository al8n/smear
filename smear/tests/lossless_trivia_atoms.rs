#![cfg(all(feature = "rowan", feature = "graphql"))]

use smear::parser::{
  graphql::lossless::trivia, lexer::graphql::lossless::LosslessTokenKind as Kind,
};

/// Every case pairs a compact input with a trivia-laden one that must behave identically.
/// The pair is the point: an atom that forgot its skip passes the first and fails the second.
#[test]
fn peek_kind_sees_past_leading_trivia() {
  for (compact, padded) in [("{", "  {"), ("{", "# c\n{"), ("{", ",\n\t{")] {
    assert_eq!(
      trivia::test_support::peek_kind_of(compact),
      trivia::test_support::peek_kind_of(padded),
      "peek_kind must agree on {compact:?} and {padded:?}"
    );
    assert!(trivia::test_support::peek_kind_of(padded).is_some());
  }
}

#[test]
fn expect_accepts_a_token_behind_trivia_and_commits_the_trivia() {
  let (ok, text) = trivia::test_support::expect_brace_of("  # hi\n{");
  assert!(ok, "expect must match the brace behind trivia");
  assert_eq!(
    text, "  # hi\n{",
    "the trivia must be committed, not skipped over"
  );
}

#[test]
fn eat_if_declines_without_consuming_trivia_twice() {
  let (ate, text) = trivia::test_support::eat_if_brace_of("  a");
  assert!(!ate, "eat_if must decline when the token does not match");
  assert_eq!(
    text, "  ",
    "a decline still commits the trivia it crossed, exactly once"
  );
}

/// What `is_trivia` covers, pinned by behaviour rather than left to the reader.
///
/// The atoms cross whatever `LosslessToken::is_trivia` admits, so this is the atom set's real
/// contract surface. GraphQL's ignored tokens are the BOM, the four whitespace/line-terminator
/// forms, the comma, and the comment — and the comma is the one an outsider is most likely to
/// mistake for punctuation. Every form is asserted **committed**, not merely crossed: the
/// second half of each pair is the byte-for-byte text the atom left in the tree.
#[test]
fn is_trivia_covers_every_graphql_ignored_token_and_commits_it() {
  for lead in [
    "\u{feff}",      // BOM
    " ",             // Space
    "\t",            // Tab
    "\n",            // Newline
    "\r",            // CarriageReturn
    "\r\n",          // CarriageReturnAndNewline
    ",",             // Comma — insignificant in GraphQL, so trivia, not punctuation
    "# a comment\n", // Comment, and the terminator that closes it
  ] {
    let src = format!("{lead}{{");
    let (ok, text) = trivia::test_support::expect_brace_of(&src);
    assert!(ok, "{lead:?} must be crossed as trivia");
    assert_eq!(text, src, "{lead:?} must be committed, byte for byte");
  }
}

/// And nothing else. A lead the lexer calls significant stops the skip at itself, so the atom
/// never reaches the brace behind it.
#[test]
fn is_trivia_covers_nothing_significant() {
  for lead in ["a", "1", "$", "\"s\"", "...", "@", ":"] {
    let src = format!("{lead}{{");
    assert_ne!(
      trivia::test_support::peek_kind_of(&src),
      Some(Kind::LBrace),
      "{lead:?} is significant and must stop the skip"
    );
    let (ok, _) = trivia::test_support::expect_brace_of(&src);
    assert!(!ok, "{lead:?} is significant and must make expect fail");
  }
}

/// `peek_kind` never answers with a trivia kind.
///
/// The skip runs first, so every answer is a significant token or `None`. That is what lets a
/// production match on the answer without a trivia arm — and it means the atom set is *not* a
/// door through which a production could branch on `\r` versus `\n`, however much the lexer's
/// kind space keeps those apart. The line-terminator spelling survives in the token's text
/// (pinned above, byte for byte), and nowhere else a production can reach.
#[test]
fn peek_kind_never_answers_with_a_trivia_kind() {
  for src in [
    " {",
    "\t{",
    "\n{",
    "\r{",
    "\r\n{",
    ",{",
    "# c\n{",
    "\u{feff}{",
    "  \n",
    ",,,",
    " \t ",
  ] {
    let answer = trivia::test_support::peek_kind_of(src);
    assert!(
      !matches!(
        answer,
        Some(
          Kind::Space
            | Kind::Tab
            | Kind::Newline
            | Kind::CarriageReturn
            | Kind::CarriageReturnAndNewline
            | Kind::Comma
            | Kind::Comment
            | Kind::Bom
        )
      ),
      "peek_kind answered {answer:?} on {src:?} — the skip did not run"
    );
  }
}

/// End of input is `None`, not an error — the atoms' one non-token answer.
#[test]
fn peek_kind_reports_none_at_end_of_input() {
  assert_eq!(trivia::test_support::peek_kind_of(""), None);
  // Trivia-only input is end of input *after* the skip, which is the case a production hits
  // at the tail of a formatted document.
  assert_eq!(trivia::test_support::peek_kind_of("  \n"), None);
}

/// `eat_if` on a match consumes exactly the token and the trivia before it.
#[test]
fn eat_if_accepts_a_token_behind_trivia() {
  let (ate, text) = trivia::test_support::eat_if_brace_of(" ,{ ");
  assert!(ate, "eat_if must accept the brace behind trivia");
  assert_eq!(
    text, " ,{",
    "the trailing space belongs to whatever comes next"
  );
}
