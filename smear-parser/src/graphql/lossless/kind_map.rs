//! The token mapper: which [`SyntaxKind`](crate::graphql::kinds::SyntaxKind) each lexer token
//! enters the tree as.
//!
//! [`CstProfile::new`](tokora::cst::CstProfile::new)'s first argument is `fn(&T) -> u16`, and
//! the sink calls it for every *committed* token. It is the only place the lexer's vocabulary
//! and the tree's kind space are put in correspondence, so a wrong arm here produces a tree
//! that round-trips perfectly and is wrong about every node it feeds.
//!
//! # The mapping is many-to-one, deliberately
//!
//! 27 `LosslessToken` variants enter a space of 25 token images. Five differences are pure
//! spelling — `Identifier`/`Name`, `LitInt`/`Int`, `LitFloat`/`Float`, `LitInlineStr`/`String`,
//! `LitBlockStr`/`BlockString` — and the rest is one genuine fold: the lexer distinguishes
//! `\r`, `\n` and `\r\n`, while the kind space carries a single `Newline` image.
//!
//! **That fold costs nothing a CST consumer can observe through kinds alone that it cannot
//! recover from text.** The tree keeps each token's bytes verbatim, so `tree.text()` still
//! reproduces the source exactly; only the *kind* is coarser. A consumer that needs to tell
//! CRLF from LF reads the token's text, exactly as it would to tell one comment from another.
//!
//! A coarser image space than the lexer's is the norm for a lossless GraphQL CST rather than a
//! design smell: `apollo-parser` folds harder still, carrying one `TokenKind::Whitespace`
//! (`// \r | \n |   | \t`, `lexer/token_kind.rs:18`) onto one `SyntaxKind::WHITESPACE` for
//! every horizontal *and* vertical whitespace form. This suite deliberately keeps `Space`,
//! `Tab` and `Newline` apart — a formatter reasoning about indentation and blank lines wants
//! those three distinguished by kind, and only the *spelling* of a line terminator is
//! genuinely a text-level detail.
//!
//! **Do not "repair" the fold by adding `CarriageReturn`/`CarriageReturnAndNewline` kinds.**
//! That breaks [`SyntaxKind::ALL`](crate::graphql::kinds::SyntaxKind::ALL)'s 87-kind pin and
//! buys nothing the text does not already give.
//!
//! # No wildcard arm
//!
//! The match is exhaustive with no `_` arm, so a new lexer token is a compile error here rather
//! than a token silently classified as filler. The mapper has no fallible form — `fn(&T) -> u16`
//! must answer for every token — so the compiler is the only place that check can live.

use smear_lexer::graphql::lossless::LosslessToken;

use crate::graphql::kinds::SyntaxKind as K;

/// The kind a committed lexer token enters the CST as.
///
/// A plain non-capturing `fn`, because it is handed to
/// [`CstProfile::new`](tokora::cst::CstProfile::new) as a `fn` pointer.
///
/// Arms are in the kind space's declaration order, so reading this match top to bottom walks
/// the kind space's token-image block — and the one place three arms share a right-hand side is
/// the line-terminator fold this module's docs describe.
#[inline]
pub fn token_kind<S>(token: &LosslessToken<S>) -> u16 {
  match token {
    // ---- Token images ----
    LosslessToken::Identifier(_) => K::Name,
    LosslessToken::LitInt(_) => K::Int,
    LosslessToken::LitFloat(_) => K::Float,
    LosslessToken::LitInlineStr(_) => K::String,
    LosslessToken::LitBlockStr(_) => K::BlockString,
    LosslessToken::Dollar => K::Dollar,
    LosslessToken::LParen => K::LParen,
    LosslessToken::RParen => K::RParen,
    LosslessToken::Spread => K::Spread,
    LosslessToken::Colon => K::Colon,
    LosslessToken::Equal => K::Equal,
    LosslessToken::At => K::At,
    LosslessToken::LBracket => K::LBracket,
    LosslessToken::RBracket => K::RBracket,
    LosslessToken::LBrace => K::LBrace,
    LosslessToken::RBrace => K::RBrace,
    LosslessToken::Pipe => K::Pipe,
    LosslessToken::Bang => K::Bang,
    LosslessToken::Ampersand => K::Ampersand,

    // ---- Trivia token images ----
    LosslessToken::Space => K::Space,
    LosslessToken::Tab => K::Tab,
    // The 3 -> 1 fold. `K::CarriageReturn` and `K::CarriageReturnAndNewline` do not exist and
    // must not be added; the three forms stay distinguishable through the token's text.
    LosslessToken::Newline => K::Newline,
    LosslessToken::CarriageReturn => K::Newline,
    LosslessToken::CarriageReturnAndNewline => K::Newline,
    LosslessToken::Comma => K::Comma,
    LosslessToken::Comment(_) => K::Comment,
    LosslessToken::Bom(_) => K::Bom,
  }
  .raw()
}
