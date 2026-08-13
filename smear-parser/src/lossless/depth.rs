//! The **parser-frame** nesting bound: one level per recursive production entry, released by an
//! RAII guard on every exit path.
//!
//! # Why this exists beside the lexer's bracket tally
//!
//! The lexer already carries a nesting counter, and #61's first fix put a measured ceiling on it.
//! That counter is a **proxy**: it tallies every `{`, `[`, `(` — and, in GraphQLx, `<` and `>` —
//! pair-blind, one saturating scalar, decrementing on *any* closer regardless of which opener the
//! parser is actually inside. The parse's own recursion is a different quantity, and recovery is
//! where the two come apart:
//!
//! - a selection set's loop meets a closer no opener matched;
//! - [`recover::unexpected`](super::recover::unexpected) reports it, `sync_balanced` finds it is
//!   itself a sync point and skips nothing, so the fallback consumes it into an `Error` node;
//! - the loop continues, and the next `f {` opens **another** selection set.
//!
//! Over `{ ) f { ) f { …` the tally therefore oscillates between 1 and 0 for the whole document
//! while every level leaves a `field` frame and a `selection_set` frame live. Measured on this
//! tree at `6f39cb9`: the tally's maximum is **1** at every depth, the ceiling never fires, and a
//! 3.5 KB document aborts a 2 MiB thread with `fatal runtime error: stack overflow` at 702 levels
//! (GraphQLx: 700, with `)` and with its angle closer `>` alike). That is the failure #61 is
//! about, reached past the fix for it.
//!
//! # What is counted here, and why it is every delimiter rather than the shape that was reported
//!
//! One level per **nesting delimiter the parse actually descends through** — the same population
//! the lexer tallies, counted by the side that recurses. A production reaches this exactly where
//! it commits an opener it will parse the interior of: `selection_set`, `arguments`,
//! `variable_definitions`, `list_value`, `object_value`, `list_type`, every `{ … }` member block,
//! and GraphQLx's generic `< … >`. So `MAX_NESTING_DEPTH` keeps the meaning it was derived with —
//! "simultaneously open brackets" — and the derivation behind the number is untouched. What
//! changed is that a closer no opener matched can no longer discount a level, because a level is
//! now released by leaving the frame rather than by seeing a byte.
//!
//! **The selection-set shape is not the only cycle, and that is measured rather than assumed.**
//! `{ f(a: [ ) [ ) [ …` runs `list_value` into `list_value` and `{ f(a: { ) k: { ) k: …` runs
//! `object_value` into `object_value`, both with the tally pinned at 3, and both abort a 2 MiB
//! thread at 2 000 levels when only `selection_set` holds a level. A fix derived from the
//! reported exemplar would have left them live; `nesting_depth.rs` pins all three.
//!
//! # The cell, and why it is tokora's rather than the lexer state's
//!
//! [`InputRef::descend`](tokora::InputRef::descend)'s, whose [`Descent`] guard releases the level
//! on return, on `?` and on an unwind alike. It is deliberately **not** in
//! [`Lexer::State`], where the lexer's tally lives, and the difference is
//! not stylistic:
//!
//! - a [`Checkpoint`](tokora::input::Checkpoint) carries `L::State`, so a speculative branch's
//!   rollback would restore a depth whose frames are still live;
//! - a **cached** token carries the state that lexed it, and committing it installs that state —
//!   so a token read during lookahead at one depth would reset the cell when it is committed at
//!   another.
//!
//! Both are correct for a lexer tally, which is a function of the token prefix and nothing else,
//! and both are wrong for a control-stack fact. tokora's cell is outside the checkpoint set for
//! exactly that reason.
//!
//! # The ceiling arrives as a `usize`, not as a type
//!
//! This module may not name `smear-lexer` — the substrate is dialect-generic and
//! `lossless_isolation.rs` enforces it — so the caller reads
//! `inp.state().max_nesting_depth()` and passes the number. Each dialect's `lossless/mod.rs` has a
//! one-line `descend` wrapper that does exactly that, so a production still writes one call.
//!
//! # The upstream ceiling above smear's
//!
//! tokora's own parse-side budget defaults to depth **64** and is set by
//! `InputContext::new`, which [`parse_lossless`](tokora::cst::parse_lossless) calls without a
//! hook: neither `parse_document_with_limits` nor any other smear entry point can raise it. The
//! effective bound is therefore `min(the caller's ceiling, 64)`, and [`descend`] reads tokora's
//! `limitation()` so that the trip is **reported by smear** rather than surfacing as an
//! unemitted `RecursionLimitReached` on the discarded `Result`. At the shipped
//! `MAX_NESTING_DEPTH` of 24 the two are 2.7x apart and only smear's fires; a caller who raises
//! past 64 gets a clean, positioned diagnostic at 64 rather than the depth they asked for. That
//! is a limitation of the door, recorded here because it is invisible at the call site.

use tokora::{
  InputRef, Lexer, ParseContext, SimpleSpan, error::RecursionLimitReached, input::Descent,
  span::Spanned,
};

use crate::combinator::ErrorOf;

/// A dialect error container that can name a refused descent.
///
/// One method rather than a `From` impl, because the payload is three plain numbers and a span
/// rather than a type: a `From<…>` would need a shared error struct in the substrate, and the
/// substrate may not put a type into a dialect's error enum.
pub trait FromNestingLimit {
  /// The parse tried to enter level `attempted` under a ceiling of `limit`.
  ///
  /// `span` is empty and sits at the parse's committed end — the position the descent was refused
  /// at. A refused frame has consumed nothing of its own yet, so there is no lexeme to point at.
  fn nesting_limit_exceeded(span: SimpleSpan, attempted: usize, limit: usize) -> Self;
}

/// Enters one level of parser recursion, or reports and refuses.
///
/// **Bind the guard for the whole frame.** It is `#[must_use]` and dropping it early releases the
/// level before the recursion it was taken for — tokora's `Descent` docs measure four spellings
/// that do exactly that and only one of which warns. The shape every call site here writes is:
///
/// ```text
/// let mut frame = super::descend::<Src, Ctx>(inp)?;
/// let inp = &mut *frame;
/// ```
///
/// # The trip is emitted *and* returned
///
/// Emitted because the lossless door discards the parser's `Result` — `parse_document` keeps the
/// tree and the diagnostics — so a trip that only rode the `Result` would leave a consumer with a
/// truncated tree, no diagnostic and no way to ask. Returned because the frame must not run: the
/// error unwinds every live production, and the document entry's drain then commits the tail, so
/// the tree still covers every byte.
///
/// The check runs **before** the descent, so tokora's own budget can never trip unreported: this
/// refuses at `min(ceiling, tokora's limitation)`, which is at or below where
/// [`InputRef::descend`](tokora::InputRef::descend) would.
#[inline]
pub fn descend<'r, 'inp, 'closure, L, Ctx, Lang>(
  inp: &'r mut InputRef<'inp, 'closure, L, Ctx, Lang>,
  ceiling: usize,
) -> Result<Descent<'r, 'inp, 'closure, L, Ctx, Lang>, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<RecursionLimitReached<usize, Lang>> + FromNestingLimit,
{
  let live = inp.recursion().depth();
  // `min`, because the caller's ceiling is not the only one: see the module header's note on
  // tokora's own 64. Reading it here is what keeps every refusal reported.
  let limit = ceiling.min(inp.recursion().limitation());
  if live >= limit {
    let end = inp.span().end();
    let span = SimpleSpan::new(end, end);
    inp.emit_error(Spanned::new(
      span,
      ErrorOf::<'inp, L, Ctx, Lang>::nesting_limit_exceeded(span, live + 1, limit),
    ))?;
    return Err(ErrorOf::<'inp, L, Ctx, Lang>::nesting_limit_exceeded(
      span,
      live + 1,
      limit,
    ));
  }

  // Cannot trip: `live < limit <= inp.recursion().limitation()`.
  inp.descend()
}
