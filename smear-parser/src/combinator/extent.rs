//! Token-extent spans: a composite node's span is the extent of the tokens it contains.
//!
//! # The two positions a parser can be at, and why only one of them is a span endpoint
//!
//! tokora's [`InputRef`] tracks two offsets, and they are not the same number:
//!
//! - [`cursor()`](InputRef::cursor) — the **lookahead** position. It is the start of the first
//!   cached token when a peek has filled the cache, and the end of the last committed token when
//!   it has not. tokora documents it as "not a progress metric": a peek or a declined scan moves
//!   it across skipped bytes without committing anything.
//! - [`span().end()`](InputRef::span) — **committed progress**: where the last consumed token
//!   actually ended.
//!
//! tokora's [`spanned()`](tokora::ParseInput::spanned) and
//! [`span_since`](InputRef::span_since) build a span out of `cursor()` at both ends, which is
//! right for a lexer that has no trivia to skip and wrong for one that does. A production that
//! peeks past its own last token to decide whether an optional tail follows closes its span at
//! **the next token's start**, so the span swallows the whitespace in between; a production
//! entered with a cold cache opens its span at **the previous token's end**, so it swallows the
//! whitespace in front. On compact input the two rules coincide, which is why a corpus of tightly
//! written source cannot tell them apart.
//!
//! This module is the other rule. A node's span runs from the start of its **first** token to the
//! end of its **last committed** token, and no ignorable byte on either side belongs to it.
//!
//! # The three entry points
//!
//! [`extent_start`] opens a node, [`extent_end`] closes one, and [`extent_since`] pairs them.
//! [`TokenSpannedExt::token_spanned`] is the combinator form and is the drop-in replacement for
//! `.spanned()` — the same `Spanned<O, L::Span>` output, measured the other way.
//!
//! # The peek in `extent_start`
//!
//! Opening a node needs the first token's *start*, and the only way to learn it is to look at the
//! token. So [`extent_start`] peeks one token, which is what makes `cursor()` report a token start
//! rather than the previous token's end. The peek is not extra lexing: a peeked token lands in the
//! cache and the production that follows consumes it from there, and every dispatching production
//! in both dialects already peeks its own head before this runs. What it does add is an *earlier*
//! observation point for a lexer error on that first token — the error a production entered on
//! unlexable bytes would have raised a moment later anyway.
//!
//! # The empty node
//!
//! A parser that succeeds without consuming a token leaves `extent_end` behind `extent_start`.
//! [`extent_since`] answers with an empty span at the start rather than an inverted one — which is
//! also what [`SimpleSpan::new`](tokora::SimpleSpan) would panic on. No production in either
//! dialect is in that state today; the clamp is there so that a future one degrades to a point
//! rather than to an abort.

use core::marker::PhantomData;

use tokora::{
  Complete, Completeness, InputRef, Lexer, ParseContext, ParseInput,
  cache::PeekedTokenExt,
  span::{Span, Spanned},
  utils::typenum::U1,
};

use super::{ErrorOf, ParseCtx};

/// The offset the node about to be parsed **begins** at: the start of the next token.
///
/// At end of input there is no next token, and the answer is the current offset — an anchor for
/// the empty span a production that parses nothing there would carry.
///
/// See the module docs for why this peeks and what the peek does and does not cost.
#[inline]
pub fn extent_start<'inp, L, Ctx, Lang, Cmpl>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang, Cmpl>,
) -> Result<L::Offset, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Cmpl: Completeness,
{
  let head = {
    let mut peeked = inp.peek::<U1>()?;
    peeked.pop_front().map(|token| token.span().start())
  };
  Ok(match head {
    Some(start) => start,
    None => inp.offset().clone(),
  })
}

/// The offset the node just parsed **ends** at: the end of the last committed token.
///
/// This is tokora's committed-progress reading, deliberately not
/// [`cursor()`](InputRef::cursor) — see the module docs.
#[inline]
pub fn extent_end<'inp, L, Ctx, Lang, Cmpl>(
  inp: &InputRef<'inp, '_, L, Ctx, Lang, Cmpl>,
) -> L::Offset
where
  L: Lexer<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
  Lang: ?Sized,
  Cmpl: Completeness,
{
  inp.span().end()
}

/// The token extent of everything parsed since `start`, as a span.
///
/// `start` comes from [`extent_start`] and the end from [`extent_end`]. A parser that consumed
/// nothing yields the empty span at `start`; see the module docs.
#[inline]
pub fn extent_since<'inp, L, Ctx, Lang, Cmpl>(
  inp: &InputRef<'inp, '_, L, Ctx, Lang, Cmpl>,
  start: L::Offset,
) -> L::Span
where
  L: Lexer<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
  Lang: ?Sized,
  Cmpl: Completeness,
{
  let end = extent_end(inp);
  let end = if end < start { start.clone() } else { end };
  L::Span::new(start, end)
}

/// A parser wrapped so its output carries the **token extent** of what it consumed.
///
/// Built by [`TokenSpannedExt::token_spanned`]; the `.spanned()` shape with the other span rule.
pub struct TokenSpanned<P, Cmpl = Complete> {
  parser: P,
  _cmpl: PhantomData<Cmpl>,
}

impl<P, Cmpl> TokenSpanned<P, Cmpl> {
  /// Wraps `parser`.
  #[inline(always)]
  pub const fn new(parser: P) -> Self {
    Self {
      parser,
      _cmpl: PhantomData,
    }
  }
}

impl<'inp, L, O, Ctx, P, Lang, Cmpl> ParseInput<'inp, L, Spanned<O, L::Span>, Ctx, Lang, Cmpl>
  for TokenSpanned<P, Cmpl>
where
  P: ParseInput<'inp, L, O, Ctx, Lang, Cmpl>,
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Cmpl: Completeness,
{
  #[inline(always)]
  fn parse_input(
    &mut self,
    inp: &mut InputRef<'inp, '_, L, Ctx, Lang, Cmpl>,
  ) -> Result<Spanned<O, L::Span>, ErrorOf<'inp, L, Ctx, Lang>> {
    let start = extent_start(inp)?;
    let data = self.parser.parse_input(inp)?;
    Ok(Spanned::new(extent_since(inp, start), data))
  }
}

/// The `.token_spanned()` method, on every parser.
///
/// Deliberately a separate trait rather than a wider `ParseInput`: tokora owns that trait, and a
/// dialect's span rule is not tokora's to choose.
pub trait TokenSpannedExt<'inp, L, O, Ctx, Lang, Cmpl>:
  ParseInput<'inp, L, O, Ctx, Lang, Cmpl> + Sized
where
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Cmpl: Completeness,
{
  /// Wraps this parser's output in a [`Spanned`] carrying the **token extent** it consumed.
  ///
  /// The replacement for tokora's [`spanned()`](tokora::ParseInput::spanned) in dialect
  /// productions: same output type, span measured from the first token's start to the last
  /// committed token's end instead of from lookahead position to lookahead position.
  #[inline(always)]
  fn token_spanned(self) -> TokenSpanned<Self, Cmpl> {
    TokenSpanned::new(self)
  }
}

impl<'inp, L, O, Ctx, P, Lang, Cmpl> TokenSpannedExt<'inp, L, O, Ctx, Lang, Cmpl> for P
where
  P: ParseInput<'inp, L, O, Ctx, Lang, Cmpl>,
  L: Lexer<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Cmpl: Completeness,
{
}
