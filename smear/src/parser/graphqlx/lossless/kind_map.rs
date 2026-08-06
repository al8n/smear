//! The token mapper: which [`SyntaxKind`](crate::graphqlx::kinds::SyntaxKind) each lexer token
//! enters the tree as.
//!
//! [`CstProfile::new`](tokora::cst::CstProfile::new)'s first argument is `fn(&T) -> u16`, and the
//! sink calls it for every *committed* token. It is the only place the lexer's vocabulary and the
//! tree's kind space are put in correspondence, so a wrong arm here produces a tree that
//! round-trips perfectly and is wrong about every node it feeds.
//!
//! # The mapping is many-to-one, deliberately, and exactly once
//!
//! 34 `LosslessToken` variants enter a space of 32 token images. Six differences are pure
//! spelling — `Identifier`/`Name`, `LitInt`/`Int`, `LitFloat`/`Float`, `LitInlineStr`/
//! `InlineString`, `LitBlockStr`/`BlockString` — and the rest is one genuine fold: the lexer
//! distinguishes `\r`, `\n` and `\r\n`, while the kind space carries a single `Newline` image.
//!
//! **That fold costs nothing a CST consumer can observe through kinds alone that it cannot
//! recover from text.** The tree keeps each token's bytes verbatim, so `tree.text()` still
//! reproduces the source exactly; only the *kind* is coarser. A consumer that needs to tell CRLF
//! from LF reads the token's text, exactly as it would to tell one comment from another.
//!
//! `Space` and `Tab` are **not** folded, and neither is `Bom`. They are three different
//! characters a formatter branches on, not three spellings of one — and folding the BOM into
//! whitespace, which `apollo-parser` does (`is_whitespace_assimilated`, `lexer/mod.rs:602`), would
//! be a *second* many-to-one that `tests/lossless_x_kind_map.rs` exists to refuse.
//!
//! # The radix is not a kind distinction
//!
//! GraphQLx's numeric tokens carry payloads GraphQL's do not: `LitInt(LitInt<S>)` where `LitInt<S>
//! = Decimal | Hex | Binary | Octal` (`smear-lexer/src/graphqlx/mod.rs:55`), and
//! `LitFloat(LitFloat<S>)` where `LitFloat<S> = Decimal | Hex` (`:134`). The kind space derived in
//! Task 8 has a single [`Int`](crate::graphqlx::kinds::SyntaxKind::Int) image and a single
//! [`Float`](crate::graphqlx::kinds::SyntaxKind::Float), so **all four radices map to one image
//! and all two do**, and the radix stays readable from the token's text exactly as the
//! line-terminator spelling does. That is not a fold in the sense above: the *lexer kind* is
//! already one `Int`, so nothing is being collapsed at this boundary at all.
//!
//! # The wildcard arm is forced, and what stands in for the compiler
//!
//! GraphQL's `kind_map` is wildcard-free, and that is the property the plan asked for here too. It
//! cannot be had: `smear_lexer::graphqlx::lossless::LosslessToken` is `#[non_exhaustive]`
//! (`smear-lexer/src/graphqlx/lossless/mod.rs:90`) and GraphQL's is not, and `#[non_exhaustive]`
//! binds across the **crate** boundary — `smear-parser` is a different crate from `smear-lexer`,
//! so rustc requires a wildcard arm no matter what this file wants. Task 8's
//! `the_image_block_matches_the_graphqlx_lexer` recorded the same limitation for its own
//! `image_of`.
//!
//! Three things stand in for the exhaustiveness check the compiler cannot give:
//!
//! 1. **The wildcard panics.** It does not fall back to a filler kind, so a token this mapper does
//!    not name stops the parse where it is met rather than entering the tree as something else.
//!    A *deleted* arm therefore fails loudly on the first token of that shape.
//! 2. **`tests/lossless_x_kind_map.rs` walks all thirty-four lexed tokens** and asserts the image
//!    of each by name, so a *wrong* arm — the failure mode a panic cannot see — is red there.
//! 3. **That file also pins the lexer enum's cardinality** through its discriminants, so a variant
//!    added to `LosslessTokenKind` is a test failure rather than a silent widening of the
//!    wildcard.

use smear_lexer::graphqlx::lossless::LosslessToken;

use crate::graphqlx::kinds::SyntaxKind as K;

/// The kind a committed lexer token enters the CST as.
///
/// A plain non-capturing `fn`, because it is handed to
/// [`CstProfile::new`](tokora::cst::CstProfile::new) as a `fn` pointer.
///
/// Arms are in the kind space's declaration order, so reading this match top to bottom walks the
/// kind space's token-image block — and the one place three arms share a right-hand side is the
/// line-terminator fold this module's docs describe.
///
/// # Panics
///
/// On a `LosslessToken` variant this mapper does not name. Unreachable for the lexer as it
/// stands — the case list in `tests/lossless_x_kind_map.rs` covers all thirty-four variants — and
/// reachable only if `smear-lexer` grows a thirty-fifth, which is exactly the event the
/// `#[non_exhaustive]` wildcard would otherwise absorb in silence.
#[inline]
pub fn token_kind<S>(token: &LosslessToken<S>) -> u16 {
  match token {
    // ---- Token images, non-trivia ----
    LosslessToken::Asterisk => K::Asterisk,
    LosslessToken::At => K::At,
    LosslessToken::Dollar => K::Dollar,
    LosslessToken::FatArrow => K::FatArrow,
    LosslessToken::LAngle => K::LAngle,
    LosslessToken::RAngle => K::RAngle,
    LosslessToken::LParen => K::LParen,
    LosslessToken::RParen => K::RParen,
    LosslessToken::Spread => K::Spread,
    LosslessToken::Colon => K::Colon,
    LosslessToken::Equal => K::Equal,
    LosslessToken::LBracket => K::LBracket,
    LosslessToken::RBracket => K::RBracket,
    LosslessToken::LBrace => K::LBrace,
    LosslessToken::RBrace => K::RBrace,
    LosslessToken::Pipe => K::Pipe,
    LosslessToken::Bang => K::Bang,
    LosslessToken::Ampersand => K::Ampersand,
    LosslessToken::Plus => K::Plus,
    LosslessToken::Minus => K::Minus,
    LosslessToken::PathSeparator => K::PathSeparator,
    LosslessToken::Identifier(_) => K::Name,
    LosslessToken::LitFloat(_) => K::Float,
    LosslessToken::LitInt(_) => K::Int,
    LosslessToken::LitInlineStr(_) => K::InlineString,
    LosslessToken::LitBlockStr(_) => K::BlockString,

    // ---- Trivia token images ----
    LosslessToken::Bom(_) => K::Bom,
    LosslessToken::Comma => K::Comma,
    LosslessToken::Space => K::Space,
    LosslessToken::Tab => K::Tab,
    // The 3 -> 1 fold, and the only one. `K::CarriageReturn` and `K::CarriageReturnAndNewline` do
    // not exist and must not be added: the three forms stay distinguishable through the token's
    // text, and adding them would break the space's 113-kind pin.
    LosslessToken::Newline => K::Newline,
    LosslessToken::CarriageReturn => K::Newline,
    LosslessToken::CarriageReturnAndNewline => K::Newline,
    LosslessToken::Comment(_) => K::Comment,

    // Forced by `#[non_exhaustive]`; see the module docs. `token.kind()` rather than the token
    // itself because `S` carries no `Debug` bound and the kind is the whole of what a reader needs
    // to find the missing arm.
    other => panic!(
      "the graphqlx lexer emits {:?} and this mapper names no image for it; add an arm rather \
       than letting the wildcard classify it",
      other.kind()
    ),
  }
  .raw()
}
