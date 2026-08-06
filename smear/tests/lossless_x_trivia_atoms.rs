//! The GraphQLx trivia atoms — the only peek/expect door its productions will use.
//!
//! # Why the second dialect gets its own copy of this file
//!
//! The atoms are the substrate's, shared verbatim, so it is tempting to call them proven. What is
//! *not* shared is what they are instantiated over: a different lexer, whose own `is_trivia`
//! decides what the skip crosses. GraphQLx's ignored set happens to be the same eight forms as
//! GraphQL's — and "happens to be" is exactly the kind of claim that needs a test rather than a
//! sentence, because GraphQLx widened the significant vocabulary by seven images and could as
//! easily have widened the ignored one.
//!
//! Two atoms are covered here that GraphQL's twin leaves to its productions. `peek_as` is the
//! door every contextual keyword arrives through — `import`, `from`, `as`, `where`, `set`, `map`
//! are ordinary identifier tokens to this lexer — and `try_eat` is the retro-wrap probe. GraphQLx
//! has no productions yet, so an atom whose only witness is a production that does not exist has
//! no witness at all.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear::parser::graphqlx::lossless::trivia::test_support::{
  self as atoms, Keyword, Kind, eat_if_brace_of, expect_brace_of, peek_keyword_of, peek_kind_of,
  peek_second_kind_of, try_eat_bang_of,
};

/// Every case pairs a compact input with a trivia-laden one that must behave identically.
/// The pair is the point: an atom that forgot its skip passes the first and fails the second.
#[test]
fn peek_kind_sees_past_leading_trivia() {
  for (compact, padded) in [("{", "  {"), ("{", "# c\n{"), ("{", ",\n\t{")] {
    assert_eq!(
      peek_kind_of(compact),
      peek_kind_of(padded),
      "peek_kind must agree on {compact:?} and {padded:?}"
    );
    assert!(peek_kind_of(padded).is_some());
  }
}

#[test]
fn expect_accepts_a_token_behind_trivia_and_commits_the_trivia() {
  let (ok, text) = expect_brace_of("  # hi\n{");
  assert!(ok, "expect must match the brace behind trivia");
  assert_eq!(
    text, "  # hi\n{",
    "the trivia must be committed, not skipped over"
  );
}

#[test]
fn eat_if_declines_without_consuming_trivia_twice() {
  let (ate, text) = eat_if_brace_of("  a");
  assert!(!ate, "eat_if must decline when the token does not match");
  assert_eq!(
    text, "  ",
    "a decline still commits the trivia it crossed, exactly once"
  );
}

/// What `is_trivia` covers, pinned by behaviour rather than left to the reader.
///
/// The atoms cross whatever `LosslessToken::is_trivia` admits, so this is the atom set's real
/// contract surface. Every form is asserted **committed**, not merely crossed: the second half of
/// each pair is the byte-for-byte text the atom left in the tree.
///
/// The comma is the one an outsider is most likely to mistake for punctuation, and GraphQLx makes
/// that mistake easier to make than GraphQL does: its generic lists, `where` clauses and import
/// lists all read as comma-separated and are in fact comma-*tolerant*, because the comma never
/// reaches a production at all.
#[test]
fn is_trivia_covers_every_graphqlx_ignored_token_and_commits_it() {
  for lead in [
    "\u{feff}",      // BOM
    " ",             // Space
    "\t",            // Tab
    "\n",            // Newline
    "\r",            // CarriageReturn
    "\r\n",          // CarriageReturnAndNewline
    ",",             // Comma — insignificant in GraphQLx exactly as in GraphQL
    "# a comment\n", // Comment, and the terminator that closes it
  ] {
    let src = format!("{lead}{{");
    let (ok, text) = expect_brace_of(&src);
    assert!(ok, "{lead:?} must be crossed as trivia");
    assert_eq!(text, src, "{lead:?} must be committed, byte for byte");
  }
}

/// And nothing else — including the six images GraphQL has no counterpart for.
///
/// A lead the lexer calls significant stops the skip at itself, so the atom never reaches the
/// brace behind it. The GraphQLx-only leads are the point: `*`, `+`, `-`, `<`, `::` and `=>` are
/// tokens *no* GraphQLx production consumes in most positions, which is exactly the shape a lexer
/// change could quietly reclassify as ignorable without any production noticing.
#[test]
fn is_trivia_covers_nothing_significant() {
  for lead in [
    "a", "1", "$", "\"s\"", "...", "@", ":", // shared with GraphQL
    "*", "+", "-", "<", ">", "::", "=>", // GraphQLx only
  ] {
    let src = format!("{lead}{{");
    assert_ne!(
      peek_kind_of(&src),
      Some(Kind::LBrace),
      "{lead:?} is significant and must stop the skip"
    );
    let (ok, _) = expect_brace_of(&src);
    assert!(!ok, "{lead:?} is significant and must make expect fail");
  }
}

/// `peek_kind` never answers with a trivia kind.
///
/// The skip runs first, so every answer is a significant token or `None`. That is what lets a
/// production match on the answer without a trivia arm — and it means the atom set is *not* a door
/// through which a production could branch on `\r` versus `\n`, however much the lexer's kind space
/// keeps those apart. The line-terminator spelling survives in the token's text, and nowhere else
/// a production can reach.
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
    let answer = peek_kind_of(src);
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
  assert_eq!(peek_kind_of(""), None);
  // Trivia-only input is end of input *after* the skip, which is the case a production hits at
  // the tail of a formatted document.
  assert_eq!(peek_kind_of("  \n"), None);
}

/// `eat_if` on a match consumes exactly the token and the trivia before it.
#[test]
fn eat_if_accepts_a_token_behind_trivia() {
  let (ate, text) = eat_if_brace_of(" ,{ ");
  assert!(ate, "eat_if must accept the brace behind trivia");
  assert_eq!(
    text, " ,{",
    "the trailing space belongs to whatever comes next"
  );
}

/// `peek_as` reads a contextual keyword's **spelling** through the downcast, past trivia.
///
/// The atom GraphQLx leans on hardest. Six of its keywords — `import`, `from`, `as`, `where`,
/// `set`, `map` — are ordinary names in GraphQL and contextual keywords here, so every dispatch
/// that turns on one of them is a downcast rather than a kind comparison. `peek_kind` answers
/// `Identifier` for all of them and can never tell them apart.
#[test]
fn peek_as_reads_the_keyword_spelling_behind_trivia() {
  // The keywords this dialect added. Each must project, and `peek_kind` must be blind to the
  // difference — that second half is what makes the downcast load-bearing rather than redundant.
  for (src, keyword) in [
    ("import", Keyword::Import),
    ("from", Keyword::From),
    ("as", Keyword::As),
    ("where", Keyword::Where),
    ("set", Keyword::Set),
    ("map", Keyword::Map),
  ] {
    assert_eq!(peek_keyword_of(src), Some(keyword), "{src:?} must project");
    let padded = format!("  # c\n\t{src}");
    assert_eq!(
      peek_keyword_of(&padded),
      Some(keyword),
      "{padded:?}: peek_as must skip leading trivia exactly as peek_kind does"
    );
    assert_eq!(
      peek_kind_of(src),
      Some(Kind::Identifier),
      "{src:?} must be an ordinary identifier to the kind-level atom"
    );
  }

  // An identifier that spells nothing contextual, and end of input, both answer `None`. The
  // substrate's `peek_as` flattens the two `Option`s on purpose: no caller branches on which.
  assert_eq!(peek_keyword_of("notakeyword"), None);
  assert_eq!(peek_keyword_of("  "), None);
  assert_eq!(peek_keyword_of("{"), None);
}

/// `try_eat` accepts or declines, and a decline still commits the trivia it crossed.
///
/// The retro-wrap probe: `node_at` spends the caller's mark only on `Accept`, so the accept/decline
/// distinction is the whole mechanism. `!` is the token GraphQLx's type references will spend it
/// on — this dialect folds the non-null marker into the type node it follows rather than wrapping
/// it, so the `!` is a token child found *after* the node is already open.
///
/// The decline case is the one worth asserting: the trivia crossed before the mismatch reaches the
/// tree once, attached to the enclosing node rather than to the node that was never opened.
#[test]
fn try_eat_accepts_or_declines_and_commits_the_trivia_either_way() {
  let (accepted, text) = try_eat_bang_of(" \t!");
  assert!(accepted, "try_eat must accept the bang behind trivia");
  assert_eq!(text, " \t!", "the trivia and the token are both committed");

  let (accepted, text) = try_eat_bang_of(" \ta");
  assert!(!accepted, "try_eat must decline on a mismatch");
  assert_eq!(
    text, " \t",
    "a declined try_eat still commits the trivia it crossed, exactly once"
  );

  let (accepted, text) = try_eat_bang_of("  ");
  assert!(!accepted, "try_eat must decline at end of input");
  assert_eq!(text, "  ", "and still commit the trivia it crossed");
}

/// The module's re-export surface is the one this file uses.
///
/// A guard against the drivers being quietly renamed out from under the productions that will
/// share them: `atoms::peek_kind_of` and the imported `peek_kind_of` must be the same function.
/// The one atom that is GraphQLx's own: the kind of the **second significant** token.
///
/// Three properties, and the third is the one a bounded raw window cannot have:
///
/// - it sees past the trivia *before* the head, like every other atom;
/// - it sees past the trivia *between* the two, which is what a `peek::<U2>()` cannot — that
///   window answers `Space` for `A : B` and the answer would be wrong rather than merely coarse;
/// - **it consumes nothing**, however much trivia it had to cross to answer. The committed text
///   is the witness: it must hold the head's leading trivia and nothing after it.
#[test]
fn peek_second_kind_sees_the_token_after_the_head_and_consumes_nothing() {
  // No trivia between the two.
  assert_eq!(peek_second_kind_of("A:").0, Some(Kind::Colon));
  // Trivia between the two, in every shape the ignored set admits. A fixed two-token raw window
  // answers with the trivia kind here; this atom must not.
  for src in [
    "A :",
    "A\t:",
    "A\n\n:",
    "A # c\n:",
    "A,:",
    "A ,\n # c\n\t,:",
  ] {
    assert_eq!(
      peek_second_kind_of(src).0,
      Some(Kind::Colon),
      "peek_second_kind must cross the trivia between the two tokens: {src:?}"
    );
  }
  // The discriminating pairs the `where` continuation actually asks about.
  assert_eq!(peek_second_kind_of("A::B").0, Some(Kind::PathSeparator));
  assert_eq!(peek_second_kind_of("A<T>").0, Some(Kind::LAngle));
  assert_eq!(
    peek_second_kind_of("type X").0,
    Some(Kind::Identifier),
    "the decline case: a definition keyword followed by its name, not by a colon"
  );

  // Nothing survives the look-ahead. The head's LEADING trivia is committed (every atom does
  // that); the head itself and everything after it are rolled back.
  assert_eq!(peek_second_kind_of("  A:B").1, "  ");
  assert_eq!(peek_second_kind_of("A:B").1, "");
  assert_eq!(peek_kind_of("  A:B"), Some(Kind::Identifier));

  // Ends of input, at both distances.
  assert_eq!(peek_second_kind_of("A").0, None, "no second token");
  assert_eq!(
    peek_second_kind_of("A  # c\n").0,
    None,
    "only trivia after the head"
  );
  assert_eq!(peek_second_kind_of("").0, None, "no head at all");
  assert_eq!(
    peek_second_kind_of("   ").1,
    "   ",
    "and its trivia is still committed"
  );
}

#[test]
fn the_driver_module_exposes_all_six_atoms() {
  let six: [fn(&'static str) -> bool; 6] = [
    |s| atoms::peek_kind_of(s).is_some(),
    |s| atoms::peek_second_kind_of(s).0.is_some(),
    |s| atoms::peek_keyword_of(s).is_some(),
    |s| atoms::expect_brace_of(s).0,
    |s| atoms::eat_if_brace_of(s).0,
    |s| atoms::try_eat_bang_of(s).0,
  ];
  // `{}` matches the two brace drivers and the second-kind one, and not the bang or keyword ones.
  let answers: Vec<bool> = six.iter().map(|driver| driver("{}")).collect();
  assert_eq!(answers, vec![true, true, false, true, true, false]);
}
