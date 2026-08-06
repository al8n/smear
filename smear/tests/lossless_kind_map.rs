#![cfg(feature = "rowan")]

use smear::{
  lexer::graphql::lossless::{LosslessLexer, LosslessToken as T, LosslessTokenKind as LK},
  parser::graphql::{kinds::SyntaxKind as K, lossless::kind_map::token_kind},
};
use tokora::Lexer as _;

/// Lex `src` and hand back its first token.
///
/// **Why two of the 27 cases are not hand-built.** `LosslessToken::LitInlineStr` carries a
/// `LitInlineStr<S>` and `LitBlockStr` a `LitBlockStr<S>`; both wrap a `LitPlainStr<S>` whose
/// only constructor is `pub(crate)` **to `smear-lexer`** (`string_lexer/mod.rs:60`, the
/// `variant_type!` macro). From another crate there is no door — the two `From` impls need a
/// `LitPlainStr` to start from, and `TryFrom<LitInlineStr<&[u8]>>` needs a `LitInlineStr`. So the
/// lexer is the constructor. Each call site asserts the variant it got back, which is what keeps
/// this from silently testing a token other than the one it names.
fn lex_one(src: &'static str) -> T<&'static str> {
  let mut lexer = LosslessLexer::<'static, &'static str>::new(src);
  let Some(Ok(token)) = lexer.lex() else {
    panic!("`{src}` must lex as a single token");
  };
  token
}

/// One constructed token per `LosslessToken` variant, paired with the kind it must enter the
/// tree as. Enumerated by hand on purpose: a new lexer variant must break this list.
fn cases() -> Vec<(T<&'static str>, K)> {
  let inline = lex_one(r#""s""#);
  assert!(
    matches!(inline, T::LitInlineStr(_)),
    "the inline-string case must actually be a LitInlineStr"
  );
  let block = lex_one(r#""""b""""#);
  assert!(
    matches!(block, T::LitBlockStr(_)),
    "the block-string case must actually be a LitBlockStr"
  );

  vec![
    // ---- Punctuation: 1:1 with the kind space ----
    (T::Ampersand, K::Ampersand),
    (T::At, K::At),
    (T::Bang, K::Bang),
    (T::Colon, K::Colon),
    (T::Dollar, K::Dollar),
    (T::Equal, K::Equal),
    (T::LBrace, K::LBrace),
    (T::RBrace, K::RBrace),
    (T::LBracket, K::LBracket),
    (T::RBracket, K::RBracket),
    (T::LParen, K::LParen),
    (T::RParen, K::RParen),
    (T::Pipe, K::Pipe),
    (T::Spread, K::Spread),
    // ---- Trivia ----
    (T::Space, K::Space),
    (T::Tab, K::Tab),
    (T::Comma, K::Comma),
    (T::Comment("# c"), K::Comment),
    (T::Bom("\u{FEFF}"), K::Bom),
    // ---- The 3 -> 1 line-terminator fold: the kind space has one line-terminator image ----
    (T::Newline, K::Newline),
    (T::CarriageReturn, K::Newline),
    (T::CarriageReturnAndNewline, K::Newline),
    // ---- Renames: the lexer and the kind space spell these differently ----
    (T::Identifier("x"), K::Name),
    (T::LitInt("1"), K::Int),
    (T::LitFloat("1.0"), K::Float),
    (inline, K::String),
    (block, K::BlockString),
  ]
}

#[test]
fn every_lexer_token_maps_to_its_tree_kind() {
  for (tok, want) in cases() {
    assert_eq!(
      token_kind(&tok),
      want.raw(),
      "{tok:?} mapped to the wrong kind"
    );
  }
}

#[test]
fn every_mapped_kind_is_one_the_profile_validator_admits() {
  // The mapper and the validator must agree, or a perfectly valid token is refused at the
  // emit door — a `FinishError::InvalidDialectKind` on well-formed input.
  for (tok, _) in cases() {
    let raw = token_kind(&tok);
    assert!(
      K::from_raw(raw).is_some(),
      "{tok:?} mapped to out-of-space raw {raw}"
    );
  }
}

#[test]
fn the_case_list_covers_the_whole_lexer_vocabulary() {
  // Positive control for the two tests above: a list missing a variant makes both of them
  // pass while saying nothing about it. `LosslessToken::kind` is injective — 27 variants onto
  // 27 distinct `LosslessTokenKind`s — so deduping the kinds this list produces counts the
  // variants it reaches.
  //
  // The count pins the *token* enum, not the kind enum: `LosslessTokenKind` carries 28
  // variants, one more than `LosslessToken`, because `Boolean` has no token variant (booleans
  // lex as `Identifier`). Counting kinds would therefore never reach 28 and is not the pin.
  let mut seen: Vec<LK> = cases().iter().map(|(t, _)| t.kind()).collect();
  seen.sort_unstable();
  seen.dedup();
  assert_eq!(
    seen.len(),
    27,
    "the case list must cover every LosslessToken variant"
  );
}

#[test]
fn the_mapping_is_many_to_one_only_at_the_line_terminators() {
  // **The mapping is not a bijection, and must not be made one.** 27 lexer tokens enter a
  // space of 25 token images; the whole difference is that the lexer distinguishes `\r`, `\n`
  // and `\r\n` while `SyntaxKind` carries a single `Newline` image (`kinds.rs:73`).
  //
  // Losslessness is untouched — the tree keeps every token's text verbatim, so `tree.text()`
  // still reproduces the source byte for byte. What is lost is only the ability to tell the
  // three forms apart *by kind*; a consumer that needs the distinction reads the token text,
  // exactly as it would to tell one comment from another.
  //
  // This test exists to stop the mapping from being "repaired" into a bijection by adding
  // `CarriageReturn`/`CarriageReturnAndNewline` kinds, which would break Task 2's
  // `K::ALL.len() == 87` pin, and equally to catch a *new* fold arriving by accident.
  // `S` is unconstrained here — `token_kind` is generic over it and these three variants carry
  // no payload — so the array's element type has to be spelled.
  let terminators: [T<&str>; 3] = [T::Newline, T::CarriageReturn, T::CarriageReturnAndNewline];
  for tok in terminators {
    assert_eq!(
      token_kind(&tok),
      K::Newline.raw(),
      "{tok:?} must fold onto the one line-terminator image"
    );
  }

  let mut raws: Vec<u16> = cases().iter().map(|(t, _)| token_kind(t)).collect();
  raws.sort_unstable();
  raws.dedup();
  assert_eq!(
    raws.len(),
    25,
    "27 tokens must reach exactly 25 distinct kinds — the line-terminator fold is the only \
     collapse"
  );

  // And those 25 are the *token images*, not node kinds: the images are the leading block of
  // the space and `Bom` is the last of them, so a mapper arm pointing at a node kind — or at
  // `Error`/`Gap`/`Root` — lands past it and fails here. `from_raw` alone cannot see this: it
  // admits all 87 kinds.
  for raw in raws {
    assert!(
      raw <= K::Bom.raw(),
      "raw {raw} is past the token-image block; the mapper may only produce token images"
    );
  }
}
