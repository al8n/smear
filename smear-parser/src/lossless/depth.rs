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
//! `lossless_isolation.rs` enforces it — so the number crosses that line as a plain `usize`, once
//! per parse, through [`lossless_context`]. Each dialect's `lossless/mod.rs` still has a
//! `descend` wrapper so a production writes one call, but the wrapper carries no number any more.
//!
//! # There is one ceiling now, and it is the parse's own limiter
//!
//! [`descend`] used to take a `ceiling` argument and refuse at `min(ceiling, limitation())`, and
//! that second number was tokora's `RecursionLimiter::PARSE_DEFAULT_DEPTH` — installed by
//! `InputContext::new`, which [`parse_lossless`](tokora::cst::parse_lossless) called with no hook
//! for a caller to raise. So the effective bound was `min(what the caller asked for, whatever
//! upstream currently defaults to)`, a number this tree does not choose and upstream moved three
//! times inside one unreleased window (64, then 16, then 32) with nothing here failing to compile
//! over any of it.
//!
//! [`cst::parse_lossless_with_context`](tokora::cst::parse_lossless_with_context) is the hook that
//! removes it. Each door installs `min(the caller's ceiling, smear's own stack-safety maximum)` as
//! **the** limiter, so what [`InputRef::descend`](tokora::InputRef::descend) checks against IS the
//! ceiling and there is no second number to reconcile. What [`descend`] still does is **emit**:
//! a trip is returned and never emitted, and the lossless doors discard the `Result`, so a refusal
//! that only rode the `Result` would leave a consumer with a truncated tree and no diagnostic.
//!
//! Two things follow, and only the second was ever true by construction:
//!
//! - **The lexer's tally and this budget read one number and are not one mechanism.** The lexer's
//!   trip latches tokora's poison boundary and ends the document; this one reports and returns.
//!   For a well-formed over-deep document at equal ceilings the lexer runs ahead and trips first,
//!   which is why one refusal used to look like one diagnostic — an accident of two numbers being
//!   equal, not an enforced property. The clamp above breaks the equality from the other side: a
//!   caller who raises past the maximum gets a lexer tally at the number they asked for and a
//!   parse budget at the maximum, so the parse refuses first and the tally never fires.
//! - **Equal numbers would not have been enough anyway.** The tally is pair-blind, so a closer no
//!   opener matched discounts a level the parse still holds. Measured on this tree at the
//!   *shipped* ceiling of 24, with both numbers equal: `{` then 60 repetitions of `) f {` then 61
//!   `}` — 483 bytes — leaves the tally at 1 for the whole document while the parse reaches 24,
//!   and produced **115** diagnostics against 61 for the same input under a ceiling it does not
//!   reach. So the parser side has to end the document itself. [`descend`] does, and its
//!   `The refusal ends the document` note is where that lives.

use tokora::{
  InputRef, Lexer, ParseContext, SimpleSpan,
  error::{MaybeTerminal, RecursionLimitReached},
  input::{Descent, InputContext},
  span::Spanned,
  state::recursion_tracker::RecursionLimiter,
};

use crate::combinator::ErrorOf;

/// The [`InputContext`] a lossless door drives its parse under: the caller's emitter and cache,
/// and `ceiling` as **the** recursion budget.
///
/// # Why this is a function and not one line at each door
///
/// [`InputContext::new`] seeds
/// [`RecursionLimiter::PARSE_DEFAULT_DEPTH`](tokora::state::recursion_tracker::RecursionLimiter::PARSE_DEFAULT_DEPTH),
/// and that seed is a number smear does not choose and upstream has already moved it twice — 64
/// at the version this workspace shipped against, then 16, then 32, neither move announced by a
/// compile error. A door that builds its own context and forgets `with_recursion_limiter` is
/// therefore not a door with no budget; it is a door running under **whatever tokora currently
/// defaults to**, with the caller's ceiling silently discarded and nothing on any channel saying
/// so. tokora's own `sink_context` carries the same warning about the same constructor.
///
/// So there is one place that turns a ceiling into a context, and every door goes through it. The
/// argument is a plain `usize` because this module is the dialect-generic substrate and may not
/// name `smear-lexer`; clamping the caller's request against the stack-safety maximum happens on
/// the other side of that line, in `LosslessLimits::parse_ceiling`.
///
/// # This is the whole ceiling, and it was not before
///
/// [`descend`] used to take a `ceiling` argument and refuse at `min(ceiling, limitation())`,
/// because a lossless parse could not install a limiter at all and tokora's default could sit
/// below the caller's request. `cst::parse_lossless_with_context` is the hook that removes the
/// second number: the budget installed here IS what
/// [`InputRef::descend`](tokora::InputRef::descend) checks against, so a refusal is the
/// substrate's own trip rather than smear arithmetic that happens to agree with it.
#[inline]
pub fn lossless_context<E, C>(inner: E, cache: C, ceiling: usize) -> InputContext<E, C> {
  InputContext::new(inner, cache).with_recursion_limiter(RecursionLimiter::with_limitation(ceiling))
}

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
  ///
  /// **The value this builds must answer [`MaybeTerminal::is_terminal`] with `true`**, and that is
  /// a requirement rather than an expectation: it is what stops a document root from
  /// resynchronising past the refusal. The bound is not on this trait — a `From`-style constructor
  /// cannot state a property of what it returns — so it is carried where the readers are, in the
  /// `lossless_production!` bundle's `Error: MaybeTerminal` clause, and asserted where it matters
  /// in `nesting_depth.rs`. tokora's own rule says the same thing from the other side: a frame
  /// budget is never cleared by more input, so `false` here is the arm its table calls spent
  /// silently.
  fn nesting_limit_exceeded(span: SimpleSpan, attempted: usize, limit: usize) -> Self;
}

/// Drains whatever a document production left uncommitted — unless the parse ended on a terminal
/// stop.
///
/// Every dialect's `document_entry` calls this instead of writing `inp.skip_while(|_| true)?`
/// itself, and the exception is the whole reason it is a function.
///
/// # The drain is still not optional, and on the terminal path it is worse than optional
///
/// A production that returns `Err` has committed a prefix and left the rest, so the drain is what
/// keeps the tail in the tree as committed tokens rather than as an opaque gap run. That is right
/// for a syntax error and wrong for a stop, because a stop has already decided the parse ends:
/// reading the tail then costs a full lex of it and, on a tail that does not lex cleanly, **one
/// lexer diagnostic per malformed lexeme** — tokora emits every lexer error it crosses, and
/// `skip_while` crosses all of them. Measured through `parse_document_with_limits` at a ceiling
/// above the door's own clamp, so the *parse* refuses: 300 nested selection sets with a
/// tail of `n` invalid lexemes returned `1 + n` diagnostics (1, 2, 5, 17, 65 for n = 0, 1, 4, 16,
/// 64), in both dialects. The refusal is one diagnostic only if nothing reads the tail.
///
/// So on a terminal stop the tail is left to
/// [`finish_partial`](tokora::cst::Cst::finish_partial), which tiles it as a gap run — **exactly
/// what the lexer's own trip already does**, since a latched poison boundary makes the tail
/// unreachable to any drain. `lossless::runner::finish_root`'s `The trip ends the document,
/// deliberately` note carries that posture and the price of it: the tree still covers every byte,
/// and the tail's tokens are opaque.
///
/// # Terminality, not the nesting refusal — and the first version asked the narrower question
///
/// This tested [`FromNestingLimit`] for one variant. That is narrower than the situation in two
/// ways that both bite. An **emitter rejection** is a stop by tokora's own contract — `Err` from
/// [`Emitter::emit_error`](tokora::Emitter::emit_error) means *processing stops immediately* — and
/// the rejecting emitter may return any same-typed value it likes rather than the payload it was
/// handed, so a refusal whose emission was rejected arrived here as something the narrow predicate
/// answered `false` for; the drain then ran over the tail and its own rejection displaced that
/// value in turn. Measured: `{ f } ~ ~` under an emitter that rejects `emit_error` with a budget
/// sentinel returned `LexerError` — a third value, neither the refusal nor the sentinel. And a
/// **scanner trip the emitter rejected** is the case tokora documents as reaching a caller with
/// nothing marking it, on a carrier the narrow predicate was never going to recognise.
///
/// [`MaybeTerminal`] is the notion tokora already has for exactly this, so it is what both this
/// and the five document roots read. A predicate written against one defect is how the narrow one
/// got here.
///
/// # Why the drain's error may still displace a non-terminal outcome
///
/// It always could, and that is unchanged: `skip_while` returns the emitter's fatal rejection, and
/// for a *lexer* error that `Err` **is** the delivery — the input layer advances its dedup
/// watermark before calling, so the diagnostic is offered exactly once and dropping the `Err`
/// would drop it. That is the case [`descend`] is careful to distinguish from its own.
#[inline]
pub fn drain_unless_terminal<'inp, L, Ctx, Lang, T>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  out: Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: MaybeTerminal,
{
  if out.as_ref().err().is_some_and(MaybeTerminal::is_terminal) {
    return out;
  }
  inp.skip_while(|_| true)?;
  out
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
/// truncated tree, no diagnostic and no way to ask. Returned because the frame must not run, and
/// because the returned value is the *only* thing that reaches the loops which have to tell a
/// resource refusal from a syntax error.
///
/// # Why the descent is probed
///
/// The refusal is [`InputRef::descend`](tokora::InputRef::descend)'s, not this function's: the
/// budget the door installed is the only ceiling, tokora's trip arm decides against it, and the
/// trip is counted on the input's own resource-trip cell before any code here runs. What this
/// function adds is the **emission**, because tokora returns a trip and never emits one and the
/// lossless doors discard the `Result`.
///
/// Getting both from one call is what the borrow checker refuses. `inp.descend()`'s `Ok` holds a
/// [`Descent`] borrowing `inp` for this function's own output lifetime, and NLL keeps that borrow
/// live across every arm of a `match` on it — so the refusal arm cannot reach `inp.emit_error`.
/// Mapping the guard away (`inp.descend().map(|_| ())`) leaves a `Result<(), _>` that borrows
/// nothing, which releases `inp` for the emission, and the level is then taken for real on the
/// accepting path.
///
/// So an accepted descent raises and releases the level once before raising it for good. That
/// costs an increment, a comparison of two `usize`s and a decrement — tokora documents its own
/// raise as exactly that and its `Descent` destructor as "a load, a subtract and a store" — and
/// it buys a refusal whose decision, whose arithmetic and whose terminality latch are all the
/// substrate's. A refused descent is probed once and not re-taken, so the trip is counted exactly
/// once per refusal.
///
/// # The refusal ends the document — smear issue #169
///
/// A refusal is **one** diagnostic, not one per remaining token, and what makes it one is that
/// nothing reads the rest of the document afterwards.
///
/// Left to itself the `Err` unwinds the whole nest and lands in a root loop's `if
/// definition(inp).is_err() { resync_to_definition(inp)? }` — which is correct for a syntax error
/// and wrong for a resource refusal. The loop resynchronises and carries on at the *document*
/// level, where every closer of the abandoned nest is now an unexpected token with an `Error` of
/// its own. Measured: 66 nested selection sets under a ceiling of 66 returned **67** diagnostics —
/// the refusal, then one per remaining significant token — and the count grew with the document
/// (804 at 800 levels). Every recursive cycle in both dialects behaved the same way, `list_value`,
/// `object_value`, `list_type`, GraphQLx's `type_generics`, `set_or_map_type` and
/// `collection_body` included.
///
/// **It is the same posture the lexer's trip already has**: end the document.
/// `lossless::runner::finish_root`'s `The trip ends the document, deliberately` note carries the
/// reasoning — a depth trip is a resource refusal, not a syntax error, and a ceiling re-armed
/// after every trip bounds one region and not the parse.
///
/// ## Where the terminal state lives, and why it is the error value
///
/// **In the `Err` this returns**, read through [`MaybeTerminal::is_terminal`] by the five root
/// loops that catch and by [`drain_unless_terminal`]. Two other homes were available and neither
/// works:
///
/// - **A cell.** al8n/tokora#285 is why not: the two homes a budget can have in that crate have
///   opposite durability. `L::State` — where smear's lexer tally lives — is a
///   [`Checkpoint`](tokora::input::Checkpoint) field, so a speculative rollback refunds it and
///   `set_state`/`state_mut` drop it outright; so is `poison_boundary`, which is why the lexer's
///   own latch is a lineage memo rather than a durable fact. The durable cells (`recursion`, and
///   the token budget #285 added) live on the `Input` with no public mutator, so smear cannot
///   write one. A latch in the refundable cell is the category error that issue names.
/// - **Spending the input**, by draining here to end of stream so every loop above re-derives
///   "stop" from its next peek. That is what this function did when the repair first landed, and
///   it is wrong for a reason no cheap reading of tokora shows: **there is no quiet drain.**
///   `scan_with` — the driver behind `next`, `try_expect*`, `skip_while` and the whole `sync`
///   family — emits every lexer error it crosses, and the peek fill is the only other lexing
///   driver and does the same. So a drain over a tail that does not lex cleanly reports one
///   diagnostic per malformed lexeme: `1 + n` for `n` invalid tail lexemes, measured at 1, 2, 5,
///   17 and 65, which is the amplification back in a smaller font and with the allocation still
///   proportional to the tail. Worse, `skip_while`'s `Err` — the emitter's fatal rejection of one
///   of those lexer errors — displaced the saved refusal through the `?` that propagated it: an
///   emitter accepting the refusal and rejecting a later lexer error got the **lexer error** back
///   from a function whose contract names the refusal.
///
/// The error value has neither problem. It is not a cell, so nothing refunds it; it is produced by
/// the frame that refused and consumed by the frames that must stop, which is exactly the distance
/// it has to travel; and it costs no pass over the tail at all, because the tail is never read.
/// [`finish_partial`](tokora::cst::Cst::finish_partial) tiles it, as it already tiles the lexer
/// trip's.
///
/// The one thing a cell would have bought and this does not is a record for a production that
/// **catches** the refusal and carries on. No production in either dialect does — the five catch
/// sites are the document roots, and all five now stop — but a dialect added later could, and its
/// loop would re-descend and refuse again rather than reading a latch. That is the residual, and
/// it is bounded by the same predicate: a catch site that ignores
/// [`is_terminal`](MaybeTerminal::is_terminal) is the only way to reach it.
///
/// ## The emitted `Err` is dropped, and this is the one call where that is right
///
/// [`Emitter::emit_error`](tokora::Emitter::emit_error) may reject, and a rejecting emitter is
/// entitled to return **any** value of the error type rather than the payload it was handed — an
/// error-budget sentinel, say. Propagating that with `?` returned a non-refusal out of a function
/// whose contract names the refusal, and then, because the value was neither the refusal nor
/// marked, the entry drain read the tail and *its* rejection displaced the sentinel too:
/// measured at `LexerError` for `{ f } ~ ~`, a third value. That is smear issue #169's own repair
/// opening the next instance of itself one call earlier.
///
/// So the result is consumed and the saved refusal is returned unconditionally. **The asymmetry
/// with [`emit_lexer_error`](tokora::Emitter::emit_lexer_error) is what makes that sound rather
/// than merely convenient**: there, the input layer advances its dedup watermark *before* calling,
/// so the `Err` **is** the diagnostic's only delivery and dropping it drops the report. Here
/// nothing was deduped and the refusal is still in hand — it is what this returns — so what the
/// drop costs is the emitter's *choice of value*, not the report. The stop it was signalling is
/// honoured either way, because the value returned is terminal and every document root stops on
/// it.
///
/// What it does cost is real and worth naming: a host that rejects in order to say *"I am at my
/// diagnostic limit"* cannot distinguish that from *"this document is too deep"* at this one call
/// site. The alternative — propagate, and rely on the caller's
/// [`MaybeTerminal`] arm to keep the loops from resynchronising — makes correctness depend on an
/// arm the caller might not write, and tokora's own rule already records that a wrong `false` arm
/// is spent silently. An unconditional guarantee here and a terminality check at the readers is
/// the pair that needs neither.
#[inline]
pub fn descend<'r, 'inp, 'closure, L, Ctx, Lang>(
  inp: &'r mut InputRef<'inp, 'closure, L, Ctx, Lang>,
) -> Result<Descent<'r, 'inp, 'closure, L, Ctx, Lang>, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<RecursionLimitReached<usize, Lang>> + FromNestingLimit,
{
  // THE LEVEL IS TAKEN TWICE ON THE ACCEPTING PATH, AND THAT IS FORCED BY THE BORROW CHECKER.
  // See this function's `Why the descent is probed` note. `.map(|_| ())` is what releases the
  // borrow: the probe's `Ok` holds a `Descent` that borrows `inp` for this function's OUTPUT
  // lifetime, and NLL keeps that borrow live across both arms of any `match` on it, so the
  // refusal arm could not reach `inp.emit_error`. Mapping the guard away leaves a `Result<(), _>`
  // that borrows nothing, and the borrow ends at the scrutinee.
  if let Err(refusal) = inp.descend().map(|_| ()) {
    // The depth is already back down — tokora's trip arm decrements before it latches — so these
    // two reads describe the frame that was refused rather than a live one.
    let end = inp.span().end();
    let span = SimpleSpan::new(end, end);
    let attempted = inp.recursion().depth() + 1;
    let limit = inp.recursion().limitation();
    // DROPPED, NOT `?`. A rejecting emitter may return any same-typed value here rather than the
    // payload, and propagating it returned a non-refusal from a function whose contract names the
    // refusal. See this function's `The emitted `Err` is dropped` note for why that is sound at
    // THIS call and would not be at `emit_lexer_error`.
    let _ = inp.emit_error(Spanned::new(
      span,
      ErrorOf::<'inp, L, Ctx, Lang>::nesting_limit_exceeded(span, attempted, limit),
    ));
    // NOTHING BETWEEN THE EMIT AND THE RETURN, AND THAT IS THE REPAIR FOR #169. See this
    // function's `The refusal ends the document` note: the tail is not read, so it cannot add a
    // diagnostic and cannot displace this one. The loops above stop on the value itself.
    return Err(refusal);
  }

  // Cannot trip: the probe above raised and released the same level against the same cell, and
  // nothing between the two calls can change the depth.
  inp.descend()
}
