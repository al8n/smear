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
//! The higher-order atoms take their sub-parser through a `for<'c> FnMut(&mut
//! InputRef<…>)` bound, the closure-parameter shape that keeps inference honest at
//! the call site, so one atom composes over every lexer, source, and emitter the
//! substrate admits.

use tokora::{InputRef, Lexer, Token, error::UnexpectedEot, try_parse_input::ParseAttempt};

use super::{ErrorOf, LiteralValueToken, ParseCtx, StringOf, try_string};

/// The result [`peek_kind`] returns: the next token's kind, `None` at end of
/// input, or the propagated error.
pub type PeekedKind<'inp, L, Ctx, Lang = ()> =
  Result<Option<<<L as Lexer<'inp>>::Token as Token<'inp>>::Kind>, ErrorOf<'inp, L, Ctx, Lang>>;

/// The result the parser [`opt`] builds yields: `Some` on accept, `None` on
/// decline, or the propagated error.
pub type OptOf<'inp, L, Ctx, Lang, O> = Result<Option<O>, ErrorOf<'inp, L, Ctx, Lang>>;

/// The result the parser [`spanned`] builds yields: the sub-parser's output paired
/// with the span it covered, or the propagated error.
pub type SpannedOf<'inp, L, Ctx, Lang, O> =
  Result<(O, <L as Lexer<'inp>>::Span), ErrorOf<'inp, L, Ctx, Lang>>;

/// The result [`try_description`] returns: the optional leading string payload and
/// its span, or the propagated error.
pub type DescriptionAttempt<'inp, L, Ctx, Lang = ()> =
  Result<Option<(StringOf<'inp, L>, <L as Lexer<'inp>>::Span)>, ErrorOf<'inp, L, Ctx, Lang>>;

/// Reports the kind of the next token without consuming it, or `None` at end of
/// input.
///
/// The dispatch primitive for sum-type composites: a composite peeks one kind and
/// matches it into a committed arm rather than trying each declining atom in turn.
/// Peeking leaves the token in place, so a subsequent committed atom still parses
/// it. Any lexer error surfacing as the next token is read is propagated.
#[inline]
pub fn peek_kind<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> PeekedKind<'inp, L, Ctx, Lang>
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  let mut kind = None;
  inp.try_expect(|spanned| {
    kind = Some(<L::Token as Token<'inp>>::kind(spanned.data));
    false
  })?;
  Ok(kind)
}

/// Adapts a declining `try_`-parser into one that yields `Option`: an accepted
/// attempt becomes `Some`, a decline becomes `None`.
#[inline]
pub fn opt<'inp, L, Ctx, Lang, P, O>(
  mut p: P,
) -> impl for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> OptOf<'inp, L, Ctx, Lang, O>
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  P: for<'c> FnMut(
    &mut InputRef<'inp, 'c, L, Ctx, Lang>,
  ) -> Result<ParseAttempt<O>, ErrorOf<'inp, L, Ctx, Lang>>,
{
  move |inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| p(inp).map(Option::from)
}

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
  P: for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> Result<O, ErrorOf<'inp, L, Ctx, Lang>>,
{
  move |inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let cursor = inp.cursor().clone();
    let out = p(inp)?;
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

#[cfg(test)]
mod tests;
