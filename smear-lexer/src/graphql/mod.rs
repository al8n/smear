/// Errors for standard GraphQL lexers
pub mod error;

/// Syntactic tokens for GraphQL - fast lexing that skips trivia.
///
/// This module provides [`SyntacticToken`](syntactic::SyntacticToken), which is optimized for
/// high-performance parsing by automatically filtering out whitespace, comments, and commas.
///
/// **Use this for**: GraphQL servers, query execution, schema compilation, and any
/// performance-critical parsing where you don't need to preserve formatting.
///
/// **Key benefits**:
/// - Minimal memory footprint
/// - Maximum parsing speed
/// - Zero-copy token references
///
/// See [`SyntacticToken`](syntactic::SyntacticToken) for detailed documentation.
pub mod syntactic;

/// Lossless tokens for GraphQL - complete source preservation.
///
/// This module provides [`LosslessToken`](lossless::LosslessToken), which preserves all source
/// information including whitespace, comments, and formatting. Essential for developer tools
/// that need to maintain or manipulate source code without losing information.
///
/// **Use this for**: Code formatters, linters, IDEs, syntax highlighters, documentation tools,
/// and any application that needs perfect source reconstruction.
///
/// **Key benefits**:
/// - Complete source fidelity
/// - Access to all comments and formatting
/// - Build Concrete Syntax Trees (CST)
///
/// See [`LosslessToken`](lossless::LosslessToken) for detailed documentation.
pub mod lossless;

/// GraphQL contextual keywords recognized from identifier spellings.
pub mod keyword;

pub use keyword::ContextualKeyword;

pub(crate) mod handlers;

use tokora::{SimpleSpan, state::recursion_tracker::RecursionLimitExceeded, utils::Lexeme};

#[cfg(test)]
mod tests;

/// One whole token, re-scanned by the lexer that produced it.
///
/// The private half of [`identifier`], [`int_literal`] and [`float_literal`], which carry the
/// contract. The slice must scan to **exactly one** token whose span is the whole slice: leading or
/// trailing trivia fails it and a second token fails it. The scan is
/// [`SyntacticLexer`](syntactic::SyntacticLexer) — the shipped one, not a second grammar — so there
/// is nothing left for the two layers to disagree about.
///
/// # Why these exist: a payload with no door is a payload with a second custodian
///
/// The CST → AST projection reads a name or a number out of the source by the range of a token the
/// *tree* labelled `Name`, `Int` or `Float`, and a tree it did not build itself can label anything.
/// A string literal has always been re-cooked through [`LitStr`](crate::LitStr)'s `TryFrom<&str>`,
/// which is the string sub-lexer; these three had no door at all, so `Name("1")` and
/// `IntValue("abc")` were values the projection would answer and the parser could never produce.
/// al8n/smear#218. GraphQLx's `identifier`, `LitInt::try_from` and `LitFloat::try_from` are the
/// same doors over that dialect's scanner.
///
/// # `&str` only
///
/// The consumer hands back an AST borrowing the caller's `&'src str`, so the slice it re-cooks is
/// always text and the answer is that same slice. A byte door here would be surface with no caller.
fn scan_one_token<'de: 'a, 'a>(
  value: &'de str,
) -> Result<syntactic::SyntacticToken<&'a str>, error::LexerErrors<char, RecursionLimitExceeded>> {
  use tokora::Lexer as _;

  let mut lexer = syntactic::SyntacticLexer::<'de, str>::new(value);
  let Some(first) = lexer.lex() else {
    return Err(
      error::LexerError::const_new(
        SimpleSpan::new(0, value.len()),
        error::LexerErrorData::UnexpectedEndOfInput,
      )
      .into(),
    );
  };
  let token = first?;
  let span = lexer.span();
  if span.start() != 0 || span.end() != value.len() || lexer.lex().is_some() {
    return Err(not_one_token(value.len()));
  }
  Ok(token)
}

/// The slice is not the one token the caller asked for, and the scanner had no complaint of its
/// own to make.
fn not_one_token(len: usize) -> error::LexerErrors<char, RecursionLimitExceeded> {
  error::LexerError::const_new(
    SimpleSpan::new(0, len),
    error::LexerErrorData::UnexpectedLexeme(Lexeme::from_range_const(SimpleSpan::new(0, len))),
  )
  .into()
}

/// The scanner's own reading of `value` as one whole identifier.
///
/// # Whole, and the scanner's
///
/// The slice must scan to **exactly one** token, that token must be an
/// [`Identifier`](syntactic::SyntacticToken::Identifier), and its span must be the whole slice:
/// leading or trailing trivia fails it, a second token fails it, and a token of any other kind
/// fails it. The answer is the slice itself.
///
/// A keyword passes: this dialect's keywords are contextual, so `query` and `on` are identifiers
/// to the scanner and it is [`ContextualKeyword`] that tells them apart afterwards.
///
/// The lexer's own error passes through unchanged; the failures the lexer has no opinion about are
/// one `UnexpectedLexeme` over the slice.
#[inline]
pub fn identifier(value: &str) -> Result<&str, error::LexerErrors<char, RecursionLimitExceeded>> {
  match scan_one_token(value)? {
    syntactic::SyntacticToken::Identifier(name) => Ok(name),
    _ => Err(not_one_token(value.len())),
  }
}

/// The scanner's own reading of `value` as one whole integer literal, answered as the slice.
///
/// [`identifier`]'s twin for an `Int`: the same "whole, and the scanner's" contract. This dialect's
/// AST keeps an integer's **text** rather than a classified literal, so the door validates the
/// spelling and hands the slice back; a slice the scanner reads as a **float** fails here rather
/// than being accepted, because the two AST carriers are different productions.
#[inline]
pub fn int_literal(value: &str) -> Result<&str, error::LexerErrors<char, RecursionLimitExceeded>> {
  match scan_one_token(value)? {
    syntactic::SyntacticToken::LitInt(text) => Ok(text),
    _ => Err(not_one_token(value.len())),
  }
}

/// The scanner's own reading of `value` as one whole float literal, answered as the slice.
///
/// [`int_literal`]'s twin, and an integer slice fails here rather than being widened.
#[inline]
pub fn float_literal(
  value: &str,
) -> Result<&str, error::LexerErrors<char, RecursionLimitExceeded>> {
  match scan_one_token(value)? {
    syntactic::SyntacticToken::LitFloat(text) => Ok(text),
    _ => Err(not_one_token(value.len())),
  }
}
