//! The recovery primitives, generic over the dialect: report an error, make progress, let the
//! sink attribute what was skipped.
//!
//! # What is here and what stayed behind, and why the line is where it is
//!
//! Task 0's classifier put a dialect's `recover.rs` at 266 shared / 8 hooked / 247 forked, and
//! named the forked mass as **tables, not logic** — the head-set constants, the sync predicate,
//! the definition-start predicate, the delimiter classifier. Task 6b measured each remaining
//! function against a stated threshold (a function lifts if it needs **≤ 2** `fn`-pointer
//! parameters) and all eight cleared it, so the *logic* is here and the *tables* are in each
//! dialect's own `recover.rs`, which passes them in.
//!
//! The `'static` blocker Task 0b found does not reach this module, and the distinction is exact:
//! `UnexpectedToken::expected_one_of` demands a `&'static [Kind]` and such a slice cannot be
//! **built** in generic code. [`report_unexpected`] does not build one — it **receives** one, and
//! receiving a `&'static [<L::Token as Token<'inp>>::Kind]` is ordinary.
//!
//! # Built on tokora's sync family, not on hand-rolled skip loops
//!
//! The members behave differently and the differences decide which one each helper uses:
//!
//! - `sync_balanced(classifier, pred)` — **two** args: `classifier` names which kinds open and
//!   close pairs (`Balance`), `pred` is the depth-0 sync predicate. On a **successful** sync that
//!   skipped ≥1 token it reports the region once through `emit_skipped_region`, and the sink wraps
//!   those tokens in the profile's `error_kind` **automatically** — no explicit `node` call.
//! - `sync_balanced` **makes no progress in two cases**, and both are reachable here. A no-match
//!   run to end of input "commits nothing and returns `Ok(None)`, leaving no trace"; and a
//!   *successful* sync whose predicate matches the very first token returns
//!   `Some(Hole { skipped: 0 })` — the sync point was already at hand — consuming nothing and
//!   emitting nothing. See [`unexpected`] for why that pair is a termination hazard rather than a
//!   curiosity.
//!
//! # Every helper called from inside a loop must consume at least one token
//!
//! A helper that returns `Ok` without consuming turns its caller's `while` into an infinite loop.
//! [`unexpected`] is in that class and guarantees progress explicitly; [`unclosed`] and
//! [`report_unexpected`] are not, because their call sites `return` out of the loop rather than
//! falling through it. The distinction is not "does it consume" but "does the call site
//! continue".
//!
//! # `sync_balanced` needs no `L::State: Clone` clause here
//!
//! It is stated on tokora's own impl block (`input/input_ref/sync_balanced.rs:153-157`), which
//! makes writing it again look obligatory. It is not: `Lexer::State: State` and
//! `trait State: Debug + Clone`, so `L: Lexer<'inp>` implies it. Task 6's mutation proof measured
//! that — the clause deletes green — so it is absent rather than carried.
//!
//! **Do not write `node(error_kind, |_| Ok(()))`.** It is a no-op: an empty, zero-width `Error`
//! node that consumes nothing, which is the rule above violated in the one place it looks like
//! recovery.

use tokora::{
  InputRef, Lexer, ParseContext, SimpleSpan, Token,
  emitter::{CstEmitter, FromUnclosed},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  input::Balance,
  lexer::FromLogos,
  span::Spanned,
  utils::DowncastRef,
};

use crate::combinator::ErrorOf;

/// The span of the single-byte delimiter an `expect` has just committed, given the input's
/// committed extent.
///
/// `expect` reports only whether it matched, so the opener's own span has to be recovered from
/// the input: its end is the delimiter's end, and every delimiter either suite opens (`[`, `{`,
/// `(`, and GraphQLx's `<`) is exactly one byte. That span is what an unclosed-delimiter
/// diagnostic points at — the opener that was never closed, not the end of input where the
/// absence was noticed.
#[inline]
pub fn opener_span(end: usize) -> SimpleSpan {
  SimpleSpan::new(end.saturating_sub(1), end)
}

/// [`kind_of`](crate::lossless::trivia::kind_of)'s twin for the spelling: `DowncastRef`, reached
/// without letting method resolution pick the wrong `Self`.
///
/// `sync_balanced` hands its predicate a `Spanned<&Token, &Span>`, so the same `&&Token` receiver
/// that costs `kind_of` its own helper applies here. The projection is owned and `Copy`, so
/// nothing borrowed escapes.
///
/// The projection is a type parameter for the reason
/// [`peek_as`](crate::lossless::trivia::peek_as) records: a dialect's contextual-keyword enum is
/// a concrete dialect type and this module may not name one.
#[inline]
pub fn keyword_of<Kw, T: DowncastRef<Kw>>(token: &T) -> Option<Kw> {
  token.downcast_ref()
}

/// A balanced pair ran to end of input before its closer arrived.
///
/// `err` carries the **opener's** span, which is where the diagnostic points — the closer that
/// never came has no position of its own. It is built by the caller rather than here because the
/// delimiter marker (`UnclosedBracket`, `UnclosedBrace`, `UnclosedParen`, …) is the one part of
/// this that differs per pair, and `from_unclosed` is generic over exactly that marker. One
/// function therefore covers every pair a dialect opens, including one this crate has never seen.
///
/// **This helper opens no node and consumes nothing, and that is correct rather than a
/// shortcoming.** Its callers reach it when the atom set reported end of input, so there is no
/// token left to skip, nothing to attribute to an `Error` node, and nothing for `sync_balanced`
/// to settle: at end of input it would commit nothing, wrap nothing and emit no hole diagnostic.
/// Calling it here would be dead code that *reads* as the mechanism.
///
/// **Loop safety does not depend on consuming here.** Every caller's `while` is guarded by an
/// end-of-input test and this helper's result is `return`ed out of that loop rather than
/// continuing it, so there is no iteration to starve. The missing closer's absence is recorded in
/// the diagnostic; the source bytes are already accounted for by the tokens committed before end
/// of input.
pub fn unclosed<'inp, L, Ctx, Lang, Delimiter>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  err: Unclosed<Delimiter, SimpleSpan, Lang>,
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: FromUnclosed<'inp, L, Lang>,
{
  let open = err.span();
  let err = <ErrorOf<'inp, L, Ctx, Lang> as FromUnclosed<'inp, L, Lang>>::from_unclosed(err);
  inp.emit_error(Spanned::new(open, err))?;
  Ok(())
}

/// Nothing that could start one of `expected` is here. **Report, and consume nothing.**
///
/// # When to reach for this rather than [`unexpected`]
///
/// Whenever the offending token is worth more to the *next* production than it would be inside an
/// `Error` node. Three shapes are in that class, and each one is a bug if it consumes:
///
/// - **A required keyword is absent**, as with a type condition's `on`. The name that *is* there
///   is still the condition's type, so eating it would trade one diagnostic for a lost subtree.
/// - **A delimited shape is empty** where the grammar says `+` — `{}`, `()`. The report points at
///   the closer, which the enclosing loop is about to eat as its own.
/// - **A prefix has no tail**, as with a `...` at the end of a selection set. The `}` after it
///   belongs to the enclosing set; [`unexpected`] would swallow it (a closer *is* a sync point)
///   and the set would then run to end of input looking for a closer it had already consumed.
///
/// **Consuming nothing makes this unsafe inside a loop that continues.** Every caller must either
/// `return` afterwards or have already made progress on its own — the same distinction
/// [`unclosed`] documents, and the reason [`unexpected`] exists at all.
pub fn report_unexpected<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  expected: &'static [<L::Token as Token<'inp>>::Kind],
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp> + Clone,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, SimpleSpan, Lang>>,
{
  // `Clone::clone(t.data)`, not `t.data.clone()`: `Spanned<&Token, &Span>`'s `data` field is
  // already a reference, so the method form resolves to `<&Token as Clone>::clone` and hands back
  // another borrow — which then infers `UnexpectedToken`'s `T` as `&Token` and fails the `From`
  // bound a long way from here.
  match inp.peek_head_map(|t| Spanned::new(*t.span, Clone::clone(t.data)))? {
    Some(found) => {
      let span = found.span;
      let err =
        UnexpectedToken::<_, _, _, Lang>::expected_one_of(span, expected).with_found(found.data);
      inp.emit_error(Spanned::new(span, err.into()))?;
    }
    None => {
      let end = inp.span().end();
      let span = SimpleSpan::new(end, end);
      let err = UnexpectedEot::<usize, Lang>::eot_of(end);
      inp.emit_error(Spanned::new(span, err.into()))?;
    }
  }
  Ok(())
}

/// Nothing that could start one of `expected` is here, and there is still input.
///
/// Reports once through [`report_unexpected`], then makes progress — in that order, because the
/// diagnostic names the token that is about to be skipped.
///
/// # Why this cannot be `sync_balanced` alone
///
/// The obvious implementation is a bare `inp.sync_balanced(…)`, on the reasoning that a balanced
/// skip both makes progress and reports itself. It does neither reliably, and the two gaps are
/// exactly the inputs a recovery path meets:
///
/// - **A stray closer is itself a sync point.** At depth zero `pred` is consulted *first*, so over
///   `[1 ) 2]` the scan matches the `)` the caller is standing on, returns
///   `Some(Hole { skipped: 0 })` and consumes nothing. The caller's `while` then re-reads that
///   same `)` — forever.
/// - **Garbage running to end of input never matches.** Over `[1 ! ! !` there is no sync point, so
///   the scan rewinds wholesale and returns `Ok(None)`. Same spin.
///
/// So the skip is *attempted first* — it is the good outcome: one hole diagnostic for a whole
/// garbage run, nesting-aware, wrapped by the sink itself — and a fallback consumes exactly one
/// token into an `Error` node when the skip made no progress. Progress is then unconditional
/// whenever input remains, which is the enclosing loop's whole safety argument.
///
/// The fallback node is opened **only once the token is in hand**, by hand rather than through the
/// `node` combinator, so a caller that reaches this at genuine end of input gets no node at all
/// rather than an empty zero-width `Error` one.
///
/// # The two `fn` pointers
///
/// `delimiters` is the dialect's pair classifier and `is_sync_point` its depth-zero restart
/// predicate. Both are tables rather than logic, which is why they are arguments and why this
/// function is here: Task 6b's threshold admits a helper needing at most two of them.
pub fn unexpected<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  expected: &'static [<L::Token as Token<'inp>>::Kind],
  delimiters: fn(&<L::Token as Token<'inp>>::Kind) -> Balance<u8>,
  is_sync_point: fn(&L::Token) -> bool,
  error_kind: u16,
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp> + Clone,
  Ctx: ParseContext<'inp, L, Lang>,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, SimpleSpan, Lang>>,
{
  report_unexpected(inp, expected)?;

  let hole = inp.sync_balanced(delimiters, |t| is_sync_point(t.data))?;
  if hole.is_some_and(|h| h.skipped() > 0) {
    // The sink wrapped the skipped region in the profile's `error_kind` on its own.
    return Ok(());
  }

  let mark = inp.cst_mark();
  if inp.try_expect(|_| true)?.is_some() {
    inp.cst_start_at(mark, error_kind);
    inp.cst_finish(error_kind);
  }
  Ok(())
}

/// A production returned `Err`: skip its wreckage and stop **before** the next restart point.
///
/// # This helper reports nothing, and that is the point
///
/// Its callers are the document loops, and they reach it *because* a production failed — which
/// means `expect` already emitted at the position the failure happened. A second diagnostic here
/// would point at whatever the resync happens to start on, which is not where anything went
/// wrong.
///
/// # The fallback is **not** [`unexpected`]'s, and copying it there was a defect
///
/// [`unexpected`] consumes one token whenever its balanced skip made no progress, because its
/// caller has already ruled the token at hand junk. Here the opposite is true: a scan that stops
/// having skipped **zero** tokens has stopped *on a restart point* — the exact token this helper
/// went looking for — and eating it costs the whole definition. `type T { a scalar S }` loses its
/// `ScalarTypeDefinition` that way.
///
/// So the outcomes are told apart rather than lumped together:
///
/// - **`Some(hole)`** — a restart point is at hand, whether the scan crossed anything to reach it
///   or not. Whatever it crossed is already wrapped in `error_kind` by the sink; return, and the
///   caller's next turn parses the head.
/// - **`None`** — the scan ran to end of input without finding one, rewound, and committed
///   nothing. Everything left is junk, so consuming one token into an `Error` node can eat no
///   definition, and it is the branch that keeps this helper self-sufficient.
///
/// # Termination does not rest on that fallback
///
/// A document loop's dispatcher consumes at least one token before it can fail — a description
/// commits its string, every keyword arm commits its keyword, and the fall-through is
/// [`unexpected`], which guarantees progress on its own — so a resync that consumes nothing cannot
/// starve the loop. Deleting the `None` branch is therefore **not** observable as a hang; it is
/// kept because a helper whose safety depends on a caller's internals is one refactor away from
/// being wrong, and because a token nobody attributes is a token that reaches the tree as a loose
/// child of the root.
///
/// The leading peek is not decoration either. `sync_balanced` counts *every* token into its hole,
/// trivia included, so trivia the failed production left uncrossed would otherwise land inside the
/// `Error` node instead of beside it.
pub fn resync_to<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  delimiters: fn(&<L::Token as Token<'inp>>::Kind) -> Balance<u8>,
  is_restart_point: fn(&L::Token) -> bool,
  error_kind: u16,
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>,
{
  if crate::lossless::trivia::peek_kind(inp)?.is_none() {
    return Ok(());
  }

  if inp
    .sync_balanced(delimiters, |t| is_restart_point(t.data))?
    .is_some()
  {
    return Ok(());
  }

  let mark = inp.cst_mark();
  if inp.try_expect(|_| true)?.is_some() {
    inp.cst_start_at(mark, error_kind);
    inp.cst_finish(error_kind);
  }
  Ok(())
}
