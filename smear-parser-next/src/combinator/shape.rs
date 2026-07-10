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
//! [`braces`], [`parens`], and [`brackets`] are the delimited-shape atoms: each
//! commits its opener, runs the sub-parser between the delimiters, commits its
//! closer, and returns the three as a span-carrying [`Delimited`] covering the
//! whole construct. A missing closer is a hard error — the closer atom's
//! unexpected-token or end-of-input error propagates rather than fabricating a
//! delimiter — so an unterminated group fails under a fail-fast and a collecting
//! emitter alike.
//!
//! The higher-order atoms take their sub-parser through a `for<'c> FnMut(&mut
//! InputRef<…>)` bound, the closure-parameter shape that keeps inference honest at
//! the call site, so one atom composes over every lexer, source, and emitter the
//! substrate admits.

use tokora::{
  InputRef, Lexer, ParseInput, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  punct::{CloseBrace, CloseBracket, CloseParen, OpenBrace, OpenBracket, OpenParen},
  token::PunctuatorToken,
  utils::Delimited,
};

use super::{
  ErrorOf, LiteralValueToken, ParseCtx, StringOf, lbrace, lbracket, lparen, rbrace, rbracket,
  rparen, try_string,
};

pub use tokora::parser::{OptOf, PeekedKind, opt, peek_kind};

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

/// The result [`braces`] returns: `inner`'s output wrapped in a brace-delimited
/// [`Delimited`] spanning the whole `{ … }`, or the propagated error.
pub type BracesOf<'inp, L, Ctx, Lang, T> = Result<
  Delimited<
    OpenBrace<<L as Lexer<'inp>>::Span, (), Lang>,
    CloseBrace<<L as Lexer<'inp>>::Span, (), Lang>,
    T,
    <L as Lexer<'inp>>::Span,
  >,
  ErrorOf<'inp, L, Ctx, Lang>,
>;

/// The result [`parens`] returns: `inner`'s output wrapped in a paren-delimited
/// [`Delimited`] spanning the whole `( … )`, or the propagated error.
pub type ParensOf<'inp, L, Ctx, Lang, T> = Result<
  Delimited<
    OpenParen<<L as Lexer<'inp>>::Span, (), Lang>,
    CloseParen<<L as Lexer<'inp>>::Span, (), Lang>,
    T,
    <L as Lexer<'inp>>::Span,
  >,
  ErrorOf<'inp, L, Ctx, Lang>,
>;

/// The result [`brackets`] returns: `inner`'s output wrapped in a
/// bracket-delimited [`Delimited`] spanning the whole `[ … ]`, or the propagated
/// error.
pub type BracketsOf<'inp, L, Ctx, Lang, T> = Result<
  Delimited<
    OpenBracket<<L as Lexer<'inp>>::Span, (), Lang>,
    CloseBracket<<L as Lexer<'inp>>::Span, (), Lang>,
    T,
    <L as Lexer<'inp>>::Span,
  >,
  ErrorOf<'inp, L, Ctx, Lang>,
>;

/// Commits the `{` opener, runs `inner`, commits the `}` closer, and returns the
/// three as a [`Delimited`] whose span covers the whole `{ … }`.
///
/// `inner` runs between the committed delimiters and its output becomes the
/// [`Delimited`] data. A missing closer is not recovered: the closer atom's error
/// — an unexpected token or end of input — propagates, so an unterminated `{ …`
/// fails rather than fabricating a brace.
#[inline]
pub fn braces<'inp, L, Ctx, Lang, P, T>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mut inner: P,
) -> BracesOf<'inp, L, Ctx, Lang, T>
where
  L: Lexer<'inp>,
  L::Token: PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  P: for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let open = lbrace(inp)?;
  let data = inner(inp)?;
  let close = rbrace(inp)?;
  Ok(Delimited::new(open, close, data, inp.span_since(&cursor)))
}

/// Commits the `(` opener, runs `inner`, commits the `)` closer, and returns the
/// three as a [`Delimited`] whose span covers the whole `( … )`.
///
/// `inner` runs between the committed delimiters and its output becomes the
/// [`Delimited`] data. A missing closer is not recovered: the closer atom's error
/// — an unexpected token or end of input — propagates, so an unterminated `( …`
/// fails rather than fabricating a paren.
#[inline]
pub fn parens<'inp, L, Ctx, Lang, P, T>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mut inner: P,
) -> ParensOf<'inp, L, Ctx, Lang, T>
where
  L: Lexer<'inp>,
  L::Token: PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  P: for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let open = lparen(inp)?;
  let data = inner(inp)?;
  let close = rparen(inp)?;
  Ok(Delimited::new(open, close, data, inp.span_since(&cursor)))
}

/// Commits the `[` opener, runs `inner`, commits the `]` closer, and returns the
/// three as a [`Delimited`] whose span covers the whole `[ … ]`.
///
/// `inner` runs between the committed delimiters and its output becomes the
/// [`Delimited`] data. A missing closer is not recovered: the closer atom's error
/// — an unexpected token or end of input — propagates, so an unterminated `[ …`
/// fails rather than fabricating a bracket.
#[inline]
pub fn brackets<'inp, L, Ctx, Lang, P, T>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mut inner: P,
) -> BracketsOf<'inp, L, Ctx, Lang, T>
where
  L: Lexer<'inp>,
  L::Token: PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  P: for<'c> FnMut(&mut InputRef<'inp, 'c, L, Ctx, Lang>) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let open = lbracket(inp)?;
  let data = inner(inp)?;
  let close = rbracket(inp)?;
  Ok(Delimited::new(open, close, data, inp.span_since(&cursor)))
}

#[cfg(test)]
mod tests;
