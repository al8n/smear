//! The GraphQLx token mapper: which tree kind each lexer token enters the CST as.
//!
//! # Why this file has to be stronger than GraphQL's twin
//!
//! GraphQL's mapper is a **wildcard-free** match, so the compiler is its own completeness check:
//! delete an arm and `smear-parser` stops building. GraphQLx's cannot be. Its
//! `LosslessToken` is `#[non_exhaustive]` (`smear-lexer/src/graphqlx/lossless/mod.rs:90`) where
//! GraphQL's is not, and `#[non_exhaustive]` binds across the **crate** boundary — `smear-parser`
//! is a different crate from `smear-lexer` — so rustc *requires* a wildcard arm here no matter
//! what the mapper's author wants. The plan asked for a wildcard-free match and for "delete an
//! arm ⇒ build failure" as the detector; neither is available, and this file is what stands in
//! their place.
//!
//! Four properties, each closing a different hole the compiler used to close:
//!
//! - [`every_token_maps_to_its_tree_image`] — all thirty-four variants, each asserted by name.
//!   A *wrong* arm is what a panicking wildcard can never see, and it is also the failure mode
//!   that survives every downstream gate: a mis-imaged token still round-trips through `text()`,
//!   still passes the validator, and still prints a plausible golden tree.
//! - [`the_mapper_covers_the_whole_lexer_vocabulary`] — the case list reaches thirty-four distinct
//!   lexer kinds and `BlockString` is still the last of them, so a variant *added* to the lexer is
//!   a failure here rather than a silent widening of the wildcard.
//! - [`the_only_fold_is_the_line_terminator`] — thirty-four tokens reach exactly thirty-two
//!   images, all of them inside the image block.
//! - [`the_mapper_wildcard_arm_panics_rather_than_classifying`] — a source scan, because the shape
//!   of the wildcard is a property of the text and not of any value. A wildcard that answered
//!   `K::Error` or `K::Name` instead of panicking would make an unmapped token indistinguishable
//!   from a mapped one, which is precisely the state `#[non_exhaustive]` forces this file to guard
//!   against.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use std::{collections::BTreeSet, fs, path::PathBuf};

use smear::parser::{
  graphqlx::{kinds::SyntaxKind as X, lossless::kind_map::token_kind},
  lexer::graphqlx::lossless::{LosslessLexer, LosslessToken, LosslessTokenKind as LK},
};
use tokora::Lexer as _;

/// One snippet, the lexer kind it must produce, and the tree image it must map to.
///
/// **Every case is lexed, not constructed.** The narrow reason is that the string literals'
/// payload constructors are `pub(crate)` to `smear-lexer`, so `LitInlineStr` and `LitBlockStr`
/// cannot be built from here at all. The broad one is better: a hand-built token proves only that
/// a variant exists, while a lexed one proves the GraphQLx lexer *emits* it — which is the claim
/// the image block makes about all thirty-four of them.
///
/// Each row names the lexer kind as well as the image, so a snippet that lexes into something
/// other than what its author intended shows up as a failed case rather than being absorbed by a
/// neighbour. `-` and `+` are the rows where that matters most: they are ordinary tokens here and
/// would be part of a numeric literal in a lexer that folded sign into the number.
const CASES: &[(&str, LK, X)] = &[
  // ---- Punctuation the two dialects share ----
  ("@", LK::At, X::At),
  ("$", LK::Dollar, X::Dollar),
  ("(", LK::LParen, X::LParen),
  (")", LK::RParen, X::RParen),
  ("...", LK::Spread, X::Spread),
  (":", LK::Colon, X::Colon),
  ("=", LK::Equal, X::Equal),
  ("[", LK::LBracket, X::LBracket),
  ("]", LK::RBracket, X::RBracket),
  ("{", LK::LBrace, X::LBrace),
  ("}", LK::RBrace, X::RBrace),
  ("|", LK::Pipe, X::Pipe),
  ("!", LK::Bang, X::Bang),
  ("&", LK::Ampersand, X::Ampersand),
  // ---- The seven images GraphQL has no counterpart for ----
  ("*", LK::Asterisk, X::Asterisk),
  ("<", LK::LAngle, X::LAngle),
  (">", LK::RAngle, X::RAngle),
  ("=>", LK::FatArrow, X::FatArrow),
  ("+", LK::Plus, X::Plus),
  ("-", LK::Minus, X::Minus),
  ("::", LK::PathSeparator, X::PathSeparator),
  // ---- Trivia ----
  ("\u{FEFF}", LK::Bom, X::Bom),
  (",", LK::Comma, X::Comma),
  (" ", LK::Space, X::Space),
  ("\t", LK::Tab, X::Tab),
  ("# c", LK::Comment, X::Comment),
  // ---- The 3 -> 1 line-terminator fold ----
  ("\n", LK::Newline, X::Newline),
  ("\r", LK::CarriageReturn, X::Newline),
  ("\r\n", LK::CarriageReturnAndNewline, X::Newline),
  // ---- Renames: the lexer and the kind space spell these differently ----
  ("x", LK::Identifier, X::Name),
  // Radix is not a kind distinction: `LitInt<S>` has four forms and `LitFloat<S>` two, and each
  // family enters the tree as one image. Two rows per family so a mapper that reached inside the
  // payload would have to answer for both.
  ("1", LK::Int, X::Int),
  ("0x1f", LK::Int, X::Int),
  ("1.0", LK::Float, X::Float),
  ("\"s\"", LK::InlineString, X::InlineString),
  ("\"\"\"b\"\"\"", LK::BlockString, X::BlockString),
];

/// Lex `src` and hand back its single token, asserting that it *is* single.
fn lex_one(src: &str) -> LosslessToken<&str> {
  let mut lexer = LosslessLexer::<&str>::new(src);
  let Some(Ok(token)) = lexer.lex() else {
    panic!("`{src}` must lex as a token");
  };
  assert!(
    lexer.lex().is_none(),
    "`{src}` must lex as exactly ONE token, or the case says nothing about the one it names"
  );
  token
}

/// Every lexer token enters the tree as the image this file names for it.
///
/// The mapper's only real failure mode. A *missing* arm falls into the panicking wildcard and is
/// loud; a *wrong* arm is silent everywhere else in the suite — the token still carries its own
/// text, so the round-trip gate holds, the validator admits the kind, and the golden printer
/// prints a tree that reads as plausible. Only an assertion that names the expected image can see
/// it.
#[test]
fn every_token_maps_to_its_tree_image() {
  for (src, lexer_kind, image) in CASES {
    let token = lex_one(src);
    assert_eq!(
      token.kind(),
      *lexer_kind,
      "`{src}` does not lex as {lexer_kind:?}"
    );
    assert_eq!(
      token_kind(&token),
      image.raw(),
      "`{src}` ({lexer_kind:?}) mapped to {:?}, not {image:?}",
      X::from_raw(token_kind(&token))
    );
  }
}

/// The case list reaches every lexer variant, and the lexer still has exactly that many.
///
/// **The positive control for the test above, and the substitute for exhaustiveness.** A list
/// missing a variant leaves every other test in this file passing while saying nothing about it,
/// and a `#[non_exhaustive]` enum gives the compiler no way to notice.
///
/// The second half is the part the compiler cannot do at all. `LosslessTokenKind` is `#[repr(u16)]`
/// with default discriminants, so `BlockString as u16 == 33` says the enum has exactly thirty-four
/// variants and that `BlockString` is the last — a variant appended, inserted or removed moves it.
/// Without this, a thirty-fifth lexer token would simply start falling into the mapper's wildcard.
#[test]
fn the_mapper_covers_the_whole_lexer_vocabulary() {
  let reached: BTreeSet<LK> = CASES.iter().map(|(_, kind, _)| *kind).collect();
  assert_eq!(
    reached.len(),
    34,
    "the case list reaches {} lexer kinds; the GraphQLx lossless token has 34 variants",
    reached.len()
  );

  assert_eq!(
    LK::BlockString as u16,
    33,
    "`LosslessTokenKind` no longer ends at BlockString/33, so the lexer's vocabulary changed and \
     the mapper's wildcard is absorbing whatever was added"
  );
}

/// Thirty-four tokens reach exactly thirty-two images, the collapse is the line terminators, and
/// every image is inside the image block.
///
/// **Not a bijection, and it must not be made one.** The lexer distinguishes `\r`, `\n` and `\r\n`
/// while the space carries a single `Newline`; the tree keeps every token's text verbatim, so what
/// is lost is only the ability to tell the three apart *by kind*.
///
/// A *second* fold is the defect this test exists for, and it is invisible to every other gate:
/// folding `Bom` onto `Space` (which `apollo-parser` does), or `PathSeparator` onto `Colon`, still
/// round-trips, still validates, and still passes the trivia-injection gate.
///
/// The last assertion catches a different and equally quiet mistake. GraphQLx's space carries both
/// `Int` and `IntValue`, both `Float` and `FloatValue`, and both `InlineString` and `StringValue` —
/// so an arm pointing at the *node* rather than the *image* is a one-word slip that `from_raw`
/// admits and the round-trip cannot see. Every image lives below `IMAGE_BLOCK`; no node does.
#[test]
fn the_only_fold_is_the_line_terminator() {
  let terminators = ["\n", "\r", "\r\n"];
  for src in terminators {
    let token = lex_one(src);
    assert_eq!(
      token_kind(&token),
      X::Newline.raw(),
      "{:?} must fold onto the one line-terminator image",
      token.kind()
    );
  }

  let images: BTreeSet<u16> = CASES
    .iter()
    .map(|(src, _, _)| token_kind(&lex_one(src)))
    .collect();
  assert_eq!(
    images.len(),
    32,
    "34 tokens must reach exactly 32 distinct images — the line-terminator fold is the only \
     collapse; reached {}",
    images.len()
  );

  for raw in images {
    assert!(
      (raw as usize) < X::IMAGE_BLOCK,
      "raw {raw} ({:?}) is past the token-image block; the mapper may only produce token images, \
       never a node kind and never a bookkeeping tile",
      X::from_raw(raw)
    );
    assert!(
      X::from_raw(raw).is_some(),
      "raw {raw} is outside the space the profile's validator admits"
    );
  }
}

/// The mapper's forced wildcard arm panics; it does not classify.
///
/// A property of the source, so a source scan — there is no value of type `LosslessToken` that
/// reaches the arm, which is the whole reason the arm is dangerous. The danger is not that the
/// wildcard exists (rustc requires it) but that it could quietly answer: `_ => K::Error` or
/// `_ => K::Name` would give an unmapped token a real, valid image, and nothing downstream could
/// tell the result from a correct parse.
///
/// The named-arm count is the positive control **and** a second completeness pin: a scan that
/// found no arms at all would pass the wildcard assertion vacuously, and thirty-four is the number
/// of variants the mapper must name for its wildcard to be genuinely dead.
#[test]
fn the_mapper_wildcard_arm_panics_rather_than_classifying() {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../smear-parser/src/graphqlx/lossless/kind_map.rs");
  let text =
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));

  let lines: Vec<&str> = text.lines().collect();
  let mut named = 0usize;
  let mut wildcards: Vec<usize> = Vec::new();
  for (index, line) in lines.iter().enumerate() {
    let trimmed = line.trim_start();
    if trimmed.starts_with("LosslessToken::") && trimmed.contains("=>") {
      named += 1;
    }
    // The two spellings rustc accepts for a catch-all: a binding or a hole.
    if trimmed.starts_with("other =>") || trimmed.starts_with("_ =>") {
      wildcards.push(index);
    }
  }

  assert_eq!(
    named, 34,
    "the mapper names {named} `LosslessToken::` arms; it must name all 34, or its wildcard is \
     carrying variants the compiler cannot tell it about"
  );
  assert_eq!(
    wildcards.len(),
    1,
    "expected exactly one catch-all arm, found {} at lines {wildcards:?}",
    wildcards.len()
  );

  // The arm's body runs from the arm line to the line that closes the `match`, which rustfmt puts
  // at two spaces — the function body's own indentation. Bounding the scan there rather than
  // running it to end of file keeps a helper declared below `token_kind` from breaking this test
  // by merely mentioning a kind.
  let close = lines[wildcards[0]..]
    .iter()
    .position(|line| *line == "  }")
    .map(|offset| wildcards[0] + offset)
    .expect("the match this arm belongs to has no closing brace at the expected indentation");
  let arm = lines[wildcards[0]..close].join("\n");

  assert!(
    arm.contains("panic!"),
    "the catch-all arm must panic rather than answer with a kind; it reads:\n{arm}"
  );
  assert!(
    !arm.contains("K::"),
    "the catch-all arm names a kind, so an unmapped token would enter the tree as that kind \
     instead of stopping the parse:\n{arm}"
  );
  // The positive control for the two assertions above: an empty region satisfies the second one
  // vacuously and would satisfy the first only by accident.
  assert!(
    arm.lines().count() >= 2 && close > wildcards[0],
    "the scan found no arm body to check"
  );
}
