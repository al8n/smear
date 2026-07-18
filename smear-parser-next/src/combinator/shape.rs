//! Shape atoms: the higher-order glue that peeks the input and wraps sub-parsers.
//!
//! [`peek_kind`] is the dispatch primitive a sum-type composite reaches for: it
//! reports the next token's kind without consuming it, so a composite peeks once
//! and matches into committed arms — jump-table style — instead of chaining
//! declining attempts. [`opt`] adapts a declining `try_`-parser into one that
//! yields `Option`, and [`spanned`] runs a sub-parser and pairs its output with
//! the span it covered. [`try_description`] is the SDL "optional leading
//! description" atom: an optional leading string literal, expressed as
//! [`try_string`] lifted through [`opt`].
//!
//! [`separated1`] and [`list_of`] are the collecting-shape atoms, each returning a
//! parser that gathers repeated `item`s into a `Vec`. [`separated1`] parses
//! one-or-more items separated by a `Sep` punctuator, permitting an optional
//! leading separator but rejecting a trailing one, and requires at least one item;
//! its trailing-separator and too-few cases are emitter paths, so they abort under
//! a fail-fast emitter and record-then-continue under a collecting one. [`list_of`]
//! parses zero-or-more items with no separator, stopping when the caller's `until`
//! predicate accepts the next token (which it leaves in place) or at end of input.
//!
//! [`padded`] and [`padded_skipping`] bridge a trivia-bearing token stream — one
//! whose lexer surfaces whitespace and comments as ordinary tokens rather than
//! dropping them. [`padded_skipping`] delegates to tokora's padding combinator,
//! consuming and discarding the trivia on either side of the sub-parser, for a
//! consumer that only needs it tolerated. [`padded`] instead *captures* it: it
//! collects the leading trivia tokens (those [`Token::is_trivia`] accepts), runs
//! the sub-parser, collects the trailing trivia likewise, and returns a [`Padded`]
//! whose span covers the whole run — so nothing the sub-parser was wrapped in is
//! lost. Over a stream that never surfaces trivia both collections come back
//! empty.
//!
//! The higher-order atoms take their sub-parser through a `for<'c> FnMut(&mut
//! InputRef<…>)` bound, the closure-parameter shape that keeps inference honest at
//! the call site, so one atom composes over every lexer, source, and emitter the
//! substrate admits.

use std::vec::Vec;

use tokora::{InputRef, Lexer, ParseInput, Token, error::UnexpectedEot, span::Spanned};

use super::{ErrorOf, LiteralValueToken, ParseCtx, StringOf, try_string};

pub use tokora::parser::{
  ListOf, OptOf, PeekedKind, Separated1Of, list_of, opt, peek_kind, separated1,
};

/// The result the parser [`spanned`] builds yields: the sub-parser's output paired
/// with the span it covered, or the propagated error.
pub type SpannedOf<'inp, L, Ctx, Lang, O> =
  Result<(O, <L as Lexer<'inp>>::Span), ErrorOf<'inp, L, Ctx, Lang>>;

/// The result [`try_description`] returns: the optional leading string payload and
/// its span, or the propagated error.
pub type DescriptionAttempt<'inp, L, Ctx, Lang = ()> =
  Result<Option<(StringOf<'inp, L>, <L as Lexer<'inp>>::Span)>, ErrorOf<'inp, L, Ctx, Lang>>;

/// Runs `p` and pairs its output with the span covering the tokens it consumed.
///
/// Brackets the sub-parser with the cursor before and the span after, the
/// span-capture idiom that reports exactly what `p` advanced over.
#[inline]
pub fn spanned<'inp, L, Ctx, Lang, P, O>(
  mut p: P,
) -> impl for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> SpannedOf<'inp, L, Ctx, Lang, O>
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  P: ParseInput<'inp, L, O, Ctx, Lang>,
{
  move |inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let cursor = inp.cursor().clone();
    let out = p.parse_input(inp)?;
    let span = inp.span_since(&cursor);
    Ok((out, span))
  }
}

/// Declines to `None` (no tokens consumed) unless the next token is a string
/// literal — the SDL optional leading description — whose payload (tagged
/// [`StringLiteral::Inline`](super::StringLiteral::Inline) or
/// [`StringLiteral::Block`](super::StringLiteral::Block)) and span it then returns
/// as `Some`.
#[inline]
pub fn try_description<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> DescriptionAttempt<'inp, L, Ctx, Lang>
where
  L: Lexer<'inp>,
  L::Token: LiteralValueToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  opt(try_string)(inp)
}

/// A sub-parser's output together with the trivia tokens captured on each side.
///
/// [`padded`] builds this: `leading` holds the trivia consumed before the value
/// and `trailing` the trivia consumed after it, each kept as the raw [`Spanned`]
/// token the lexer yielded, and `span` covers the whole run — leading trivia,
/// value, and trailing trivia. Empty collections mean the stream surfaced no
/// trivia at that edge. [`Deref`](core::ops::Deref) targets the value, so a
/// `Padded` stands in for its value where only that is needed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Padded<T, Tok, Span> {
  span: Span,
  leading: Vec<Spanned<Tok, Span>>,
  value: T,
  trailing: Vec<Spanned<Tok, Span>>,
}

impl<T, Tok, Span> core::ops::Deref for Padded<T, Tok, Span> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.value()
  }
}

impl<T, Tok, Span> Padded<T, Tok, Span> {
  /// Returns the span covering the leading trivia, the value, and the trailing
  /// trivia.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the trivia tokens captured before the value.
  #[inline]
  pub const fn leading(&self) -> &[Spanned<Tok, Span>] {
    self.leading.as_slice()
  }

  /// Returns a reference to the value the sub-parser produced.
  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }

  /// Returns the trivia tokens captured after the value.
  #[inline]
  pub const fn trailing(&self) -> &[Spanned<Tok, Span>] {
    self.trailing.as_slice()
  }
}

/// The result the parser [`padded`] builds yields: the value wrapped in a
/// [`Padded`] carrying the trivia captured on each side, or the propagated error.
pub type PaddedOf<'inp, L, Ctx, Lang, O> = Result<
  Padded<O, <L as Lexer<'inp>>::Token, <L as Lexer<'inp>>::Span>,
  ErrorOf<'inp, L, Ctx, Lang>,
>;

/// Captures the trivia on each side of `p`: collects the leading trivia tokens,
/// runs `p`, collects the trailing trivia tokens, and returns a [`Padded`] whose
/// span covers the whole run.
///
/// A token counts as trivia when [`Token::is_trivia`] accepts it — whitespace,
/// comments, and the like on a lossless stream. Each side loops
/// [`try_expect`](InputRef::try_expect) over that predicate, consuming trivia
/// while it is next and leaving the first non-trivia token in place for `p` (and,
/// after it, the next atom). Over a stream that never surfaces trivia both
/// collections come back empty and the span is exactly `p`'s; `p`'s error, if
/// any, propagates.
#[inline]
pub fn padded<'inp, L, Ctx, Lang, P, O>(
  mut p: P,
) -> impl for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> PaddedOf<'inp, L, Ctx, Lang, O>
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  P: ParseInput<'inp, L, O, Ctx, Lang>,
{
  move |inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let cursor = inp.cursor().clone();
    let mut leading = Vec::new();
    while let Some(tok) = inp.try_expect(|t| t.data.is_trivia())? {
      leading.push(tok);
    }
    let value = p.parse_input(inp)?;
    let mut trailing = Vec::new();
    while let Some(tok) = inp.try_expect(|t| t.data.is_trivia())? {
      trailing.push(tok);
    }
    Ok(Padded {
      span: inp.span_since(&cursor),
      leading,
      value,
      trailing,
    })
  }
}

/// The result the parser [`padded_skipping`] builds yields: the sub-parser's
/// output with the surrounding trivia discarded, or the propagated error.
pub type PaddedSkippingOf<'inp, L, Ctx, Lang, O> = Result<O, ErrorOf<'inp, L, Ctx, Lang>>;

/// Consumes and discards the trivia on each side of `p`, returning only `p`'s
/// output.
///
/// A thin delegate to tokora's [padding combinator](ParseInput::padded): it skips
/// the leading trivia (those [`Token::is_trivia`] accepts), runs `p`, and skips
/// the trailing trivia, retaining none of it. For a consumer that only needs
/// trivia tolerated rather than kept; the capturing counterpart is [`padded`].
#[inline]
pub fn padded_skipping<'inp, L, Ctx, Lang, P, O>(
  p: P,
) -> impl for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> PaddedSkippingOf<'inp, L, Ctx, Lang, O>
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  P: for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> Result<O, ErrorOf<'inp, L, Ctx, Lang>>,
{
  let mut inner = p.padded();
  move |inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| inner.parse_input(inp)
}

#[cfg(test)]
mod tests;
